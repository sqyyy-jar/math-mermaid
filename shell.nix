{pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
    buildInputs = with pkgs; [
        dotty
        coursier
        sbt
        metals
    ];
}
