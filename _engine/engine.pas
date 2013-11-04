{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit engine;

interface

uses
  Math3D, shaders, wavefront, Model, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('engine', @Register);
end.
