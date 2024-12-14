{ pkgs, lib, config, inputs, ... }:

{
  packages = [
    pkgs.bun
    pkgs.jdk21
  ];

  processes = {
    frontend.exec = "./mill -w ballsort.frontend_vite.compile";
    vite.exec = "cd ballsort/frontend_vite && bun install && bun run dev";
  };
}
