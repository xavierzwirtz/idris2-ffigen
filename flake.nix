{
  description = "tree-sitter grammer for idris2";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  # inputs.idris2-pkgs.url = "github:claymager/idris2-pkgs";
  inputs.idris2 = {
    url = "github:idris-lang/Idris2";
  };
  inputs.json = {
    url = "github:stefan-hoeck/idris2-json";
    flake = false;
  };
  inputs.elab-util = {
    url = "github:stefan-hoeck/idris2-elab-util";
    flake = false;
  };

  inputs.idris2-pkgs = {
    url = "github:lizard-business/idris2-pkgs/idris0.6.0";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.idris2.follows = "idris2";
    inputs.json.follows = "json";
    inputs.elab-util.follows = "elab-util";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , idris2-pkgs
    , idris2
    , json
    , elab-util
    }:
    let
      overlay = final: prev:
        rec {
          idris2-ffigen =
            final.callPackage
              ({ pkgs }: pkgs.idris2-pkgs._builders.buildIdris {
                name = "idris2-ffigen";
                src = self;
                idrisLibraries = [ pkgs.idris2-pkgs.json ];
              })
              { };
          idris2-ffigen-make-package = final.callPackage
            ({ pkgs }: { authors
                       , packageName
                       , header
                       , libName
                       , clib
                       , rootModuleName
                       , structs
                       , functions
                       , ccArgs ? null
                       }:
              let
                json =
                  pkgs.runCommand "generate ${header}.json" { } ''
                    ${pkgs.clang_12}/bin/clang -shared \
                      ${if ccArgs == null then "" else ccArgs} \
                      -x c -Xclang -ast-dump=json \
                      ${header} > $out
                  '';
                args = builtins.toJSON {
                  inherit libName;
                  moduleName = "${rootModuleName}.Bindings";
                  structsToParse = structs;
                  functionsToParse = functions;
                };
                generated = pkgs.runCommand "generate-${packageName}-source" { } ''
                  mkdir -p $out
                  cd $out
                  ${idris2-ffigen}/bin/idris2-ffigen ${json} '${args}'
                  (echo -e "#include <stdlib.h>\n#include <${header}>"; cat c.c) > c2.c
                  rm c.c
                  mv c2.c c.c
                '';
                ipkg = pkgs.writeTextFile {
                  name = "${packageName}.ipkg";
                  text = ''
                    package ${packageName}
                    authors    = "${authors}"
                    version    = 0.1.0
                    sourcedir  = "src"
                    depends    = contrib
                               , base         >= 0.6.0

                    modules = ${rootModuleName}
                            , ${rootModuleName}.Bindings
                  '';
                };
                entryModule = pkgs.writeTextFile {
                  name = "${rootModuleName}.idr";
                  text = ''
                    module ${rootModuleName}
                    import public ${rootModuleName}.Bindings
                  '';
                };

                idris2Src = pkgs.runCommand "generate-${packageName}-idris" { } ''
                  mkdir -p $out/src/${rootModuleName}
                  cp -L ${entryModule} $out/src/${rootModuleName}.idr
                  cp -L ${generated}/idris.idr $out/src/${rootModuleName}/Bindings.idr
                  cp -L ${ipkg} $out/${packageName}.ipkg
                '';
              in
              {
                inherit json;
                idris2Lib = pkgs.idris2-pkgs._builders.buildIdris {
                  name = packageName;
                  src = idris2Src;
                };
                clib =
                  let clibName = "libidris2-ffigen-${libName}"; in
                  pkgs.stdenv.mkDerivation {
                  name = clibName;
                  unpackPhase = "true";
                  installPhase = "true";
                  buildInputs = with pkgs; [ clib ];
                  buildPhase = ''
                    mkdir -p $out/lib
                    cc -c -fPIC -o ${clibName}.o \
                      ${if ccArgs == null then "" else ccArgs} \
                      ${generated}/c.c
                    cc -shared -fPIC -Wl,-soname,${clibName}.so.1 \
                      -o ${clibName}.so.0.0.1 ${clibName}.o -lc \
                      -l${pkgs.lib.removePrefix "lib" libName}
                    cp ${clibName}.so.0.0.1 ${clibName}.o $out/lib
                    ln -s $out/lib/${clibName}.so.0.0.1 $out/lib/${clibName}.so
                  '';
                  keepDebugInfo = true;
                  dontStrip = true;
                };

                inherit idris2Src generated;
              }

            )
            { };

        };
      mkPkgs = system:
        import nixpkgs {
          inherit system;
          overlays = [
            idris2-pkgs.overlay
            overlay
          ];
        };
      mainExports = flake-utils.lib.eachDefaultSystem
        (system:
          let pkgs = mkPkgs system; in
          rec {
            # inherit overlay;
            packages =
              {
                inherit (pkgs) idris2-ffigen;
                default = packages.idris2-ffigen;
              };
            devShell = pkgs.mkShell {
              buildInputs = [
                pkgs.idris2
              ];
              IDRIS2_PACKAGE_PATH = "${pkgs.idris2}/${pkgs.idris2.name}:${pkgs.idris2-pkgs.json.asLib}/idris2-${pkgs.idris2.version}";
            };
          }
        );
    in
    {
      inherit overlay;
    } //
    mainExports;
}
