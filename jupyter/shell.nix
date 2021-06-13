{ sources ? import ./nix/sources.nix }:     # import the sources
let

  # jupyter = import sources.jupyterWith {} ;
  # # pkgs = import sources.nixpkgs {} ;

  jupyterLibPath = /home/razor/work/jupyterWith ;
  jupyter = import jupyterLibPath {};


  iPythonWithPackages = jupyter.kernels.iPythonWith {
      name = "python";
      packages = p: with p; [
            numpy
            scipy
            pandas
            matplotlib
            seaborn
            umap-learn
            scikitlearn
            ];
      };

  jupyterlabWithKernels = jupyter.jupyterlabWith {
      kernels = [ iPythonWithPackages ];
  };


in jupyterlabWithKernels.env
