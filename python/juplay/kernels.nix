{pkgs, ...}: {
  kernel.python.minimal = {
    enable = true;
  };
  kernel.rust.minimal.enable = true;
}
