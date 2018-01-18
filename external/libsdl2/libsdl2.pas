{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit libsdl2;

interface

uses
  SDL2_gfx, sdl2_image, sdl2_ttf, SDL2, SDL2_mixer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('libsdl2', @Register);
end.
