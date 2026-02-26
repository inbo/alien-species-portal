{
  inputs = {
    utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    rUtils.url = "git+https://scm.openanalytics.eu/git/oa-r-utils-nix.git";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    rUtils
  }: utils.lib.eachDefaultSystem (
    system:
    let

      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (final: prev: {
            rPackages = prev.rPackages.override {
              overrides = {
                INBOtheme = prev.rPackages.buildRPackage {
                  name = "INBOtheme";
                  src = prev.fetchFromGitHub {
                    owner = "inbo";
                    repo = "INBOtheme";
                    rev = "v0.5.8"; # release 0.5.8
                    hash = "sha256-64WRSqp1pTHj7uLM0pmBa7f+XoZHPSkrU00lSwBpud4=";
                  };
                  propagatedBuildInputs = with prev.rPackages; [ assertthat colorspace conflicted ggplot2 scales showtext sysfonts ];
                };

                trias = prev.rPackages.buildRPackage {
                  name = "trias";
                  src = prev.fetchFromGitHub {
                    owner = "trias-project";
                    repo = "trias";
                    rev = "f7fe57be4c92eebaba628d7a7b2a1042061f40cf";
                    hash = "sha256-AkKksw9MIPnGq/OQdYytxVdoscmID78uN1OaPUIQ8eU=";
                  };
                  propagatedBuildInputs = with prev.rPackages; [ 
                    assertthat assertable dplyr egg forcats gratia leaflet plotly purrr readr reshape2 rgbif 
                    rnaturalearth sf stringr svDialogs tidyr tidyselect
                  ];
                };

                aws_s3 = prev.rPackages.buildRPackage {
                  name = "aws_s3";
                  src = prev.fetchFromGitHub {
                    owner = "cloudyr";
                    repo = "aws.s3";
                    rev = "0.3.22";
                    hash = "sha256-Swo2397bDUfcX0h5RKdC+pK78Yx72wVUmFeOWsh3HEA=";
                  };
                  propagatedBuildInputs = with prev.rPackages; [ curl httr xml2 base64enc digest aws_signature ];
                };

              };
            };
          })
        ];
      };

      rpackages = with pkgs.rPackages; [ 
        arrow
        aws_ec2metadata
        aws_s3
        aws_signature
        config
        devtools
        DT
        ggspatial
        INBOtheme
        jsonlite
        leaflegend
        leaflet
        leaflet_extras
        leaflet_extras2
        plotly
        reshape2
        rgbif
        rlang
        shinycssloaders
        shiny_i18n
        shinyjs
        shinyscreenshot
        sf
        terra
        tidyverse
        trias
        webshot2 
      ]; # need tidyverse for package doc help
    
    in {
      devShells = {
        default = rUtils.lib.mkRShell {
          pkgs = pkgs;
          packages = [ 
            rpackages 
            pkgs.vscodium
            pkgs.awscli2
          ];
        };
      };
    }

  );
}