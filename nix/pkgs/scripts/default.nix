{ stdenv
, lib
, version
, makeWrapper
, writeShellApplication
, sqitch
, coreutils
, sqitchDir
, sqlite
}:

let
  # Run any sqitch command using the Primer schema.
  primer-sqitch = stdenv.mkDerivation {
    pname = "primer-sqitch";
    version = "1.0";
    nativeBuildInputs = [ sqitch makeWrapper ];
    src = sqitchDir;

    buildPhase = ''
    '';

    installPhase =
      let
        sqitchConfDir = "$out/libexec/sqitch";
      in
      ''
          mkdir -p $out/libexec/sqitch
        sqitch bundle --all --dir $out/libexec/sqitch

        # Rewrite top_dir's so they're absolute paths.
        substituteInPlace $out/libexec/sqitch/sqitch.conf \
          --replace "top_dir = " "top_dir = $out/libexec/sqitch/"

        mkdir -p $out/bin
        makeWrapper "${sqitch}/bin/sqitch" "$out/bin/primer-sqitch" \
          --prefix PATH : "${lib.makeBinPath [sqlite]}" \
          --set SQITCH_CONFIG "$out/libexec/sqitch/sqitch.conf"
      '';
  };
in
{
  inherit primer-sqitch;
}
