{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit libsdl2;

interface

uses
  sdl2, sdl2_mixer, sdl2_image, sdl2_ttf, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('libsdl2', @Register);
end.
