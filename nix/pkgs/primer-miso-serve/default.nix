{ writeShellApplication
, http-server
, primer-miso-dist
}:

writeShellApplication {
  name = "primer-miso-serve";

  text = ''
    ${http-server}/bin/http-server ${primer-miso-dist} -c-1 "$@"
  '';
}
