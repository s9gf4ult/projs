{ sources ? import ./nix/sources.nix }:     # import the sources
let
  pkgs = import sources.nixpkgs {} ;
  my-python = python-packages: with python-packages;
   [ pandas matplotlib seaborn ipywidgets widgetsnbextension ] ;
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    jupyter
    (python3.withPackages my-python)
    ruby
  ] ;

  shellHook = ''
    # Tells pip to put packages into $PIP_PREFIX instead of the usual locations.
    # See https://pip.pypa.io/en/stable/user_guide/#environment-variables.
    export PIP_PREFIX=$(pwd)/_build/pip_packages
    export PYTHONPATH="$PIP_PREFIX/${pkgs.python3.sitePackages}:$PYTHONPATH"
    export PATH="$PIP_PREFIX/bin:$PATH"
    unset SOURCE_DATE_EPOCH
  '';
}
