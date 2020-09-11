{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    alsaLib pkgconfig gtk3
    (poppler_0_61.override { introspectionSupport = true; })
    harfbuzzFull gobject-introspection pkgconfig gtk3
  ];
}
