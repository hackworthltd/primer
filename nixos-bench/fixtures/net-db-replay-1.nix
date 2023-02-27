# Using nixos-test framework, we spin up 3 VMs
# (a database server, a primer server and a client).
# The client generates some somewhat realistic traffic, and
# we measure the total database size and network traffic (per machine).
{ hostPkgs
, ...
}:
let
  primerPort = 8081;
  dbPort = 5432;
  fixture = ../testfiles/net-db-replay-1;
in
{
  nodes = {
    db = { pkgs, config, ... }: {
      services.postgresql = {
        enable = true;
        package = pkgs.postgresql;
        enableTCPIP = true;
        authentication = "host primer postgres primer trust";
        port = dbPort;
        ensureDatabases = [ "primer" ];
        ensureUsers = [
          {
            name = "primer";
            ensurePermissions = {
              "DATABASE primer" = "ALL PRIVILEGES";
            };
          }
        ];
      };
      networking.firewall.allowedTCPPorts = [ dbPort ];

      # This is essential, or else Sqitch will fail.
      time.timeZone = "UTC";
      environment.systemPackages = with pkgs; [ primer-sqitch ];
      systemd.services.sqitch = {
        wants = [ "postgresql.service" ];
        wantedBy = [ "multi-user.target" ];
        after = [ "postgresql.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          User = "primer";
        };
        script = "${pkgs.primer-sqitch}/bin/primer-sqitch deploy --verify db:postgres:primer";
      };
      users.users.primer =
        {
          name = "primer";
          group = "nobody";
          description = "Primer PostgreSQL user";
          isSystemUser = true;
        };
    };
    primer = { pkgs, config, nodes, ... }: {
      networking.firewall.allowedTCPPorts = [ primerPort ];
      systemd.services.primer = {
        wantedBy = [ "multi-user.target" ];
        environment = {
          DATABASE_URL = "postgres://postgres@db:${toString nodes.db.services.postgresql.port}/primer";
        };
        script = "${pkgs.primer-service}/bin/primer-service serve vUNKNOWN --port ${toString primerPort}";
      };
    };
    client = { pkgs, lib, config, ... }:
      let
        traffic = pkgs.writeScriptBin "traffic" ''
          ${pkgs.primer-replay}/bin/primer-replay --base-url primer:${toString primerPort} ${fixture}
        '';
      in
      {
        environment.systemPackages = [ traffic ];
      };
  };

  testScript = { nodes, ... }:
    ''
      import csv
      import json
      import time

      results = []

      def netstat(m,n):
          return [
              {"name": f"netstat/{m.name}/{i}/{d}/iter:{n}",
               "unit":'bytes',
               "value":int(m.succeed(f"cat /sys/class/net/{i}/statistics/{d}_bytes"))}
              for i in ["eth0","eth1"] for d in ["tx","rx"]]

      def stats(n):
          ret = netstat(client,n) + netstat(primer,n) + netstat(db,n);
          ret.append({"name": f"database/du/iter:{n}",
                      "unit":'bytes',
                      "value":int(db.succeed("du -s --bytes /var/lib/postgresql/ | cut -f1"))})
          sizes = db.succeed("sudo -u primer psql --csv -c 'select datname, pg_database_size(datname) from pg_database;'")
          ret += [{"name":f"database/{x['datname']}/pg_database_size/iter:{n}",
                   "unit":'bytes',
                   "value" : int(x['pg_database_size'])}
                  for x in csv.DictReader(sizes.split())]
          ret.append({"name":f"database/total-pg_database_size/iter:{n}",
                      "unit":'bytes',
                      "value" : sum(map(lambda x:int(x['pg_database_size']),csv.DictReader(sizes.split())))})
          return ret


      start_all()
      db.wait_for_unit("sqitch")
      primer.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")

      results += stats("start")

      client.succeed("traffic")

      results += stats("1")

      n=20
      for i in range(n):
        log.info(f"running traffic: iter {i}")
        client.succeed("traffic")

      results += stats(f"{1+n}")

      # Defense against broken benchmark -- primer will log an error if
      # it cannot connect to the DB
      time.sleep(30) # wait for logs to be written etc
      primer.fail("journalctl -b -u primer | grep -q ERROR")

      # Defence against broken benchmark -- should have the correct number
      # of session uuids
      sessions = 0
      with open("${fixture}",'r') as f:
        for l in f:
          if l.startswith("expected result hash: "):
            sessions+=1
          else:
            break
      db.succeed(f"sudo -u primer psql -tA primer -c 'select count(uuid) from sessions;' | grep -q '^{(1+n)*sessions}$'")

      with open(driver.out_dir / "results.json",'w') as f:
        json.dump(results,f)
    '';
}
