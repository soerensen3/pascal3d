{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit newtonphysics;

interface

uses
  Newton, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('newtonphysics', @Register);
end.
