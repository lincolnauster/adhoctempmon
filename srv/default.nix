{ mkDerivation, base, blaze-html, directory, lib, scotty, stm, text
, time, unix, xhtml
}:
mkDerivation {
  pname = "srv";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blaze-html directory scotty stm text time unix xhtml
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
