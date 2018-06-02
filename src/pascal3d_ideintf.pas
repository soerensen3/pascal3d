{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascal3d_ideintf;

{$warn 5023 off : no warning about unused units}
interface

uses
  p3d.ideintf, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascal3d_ideintf', @Register);
end.
