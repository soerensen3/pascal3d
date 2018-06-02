{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascal3d_ide;

{$warn 5023 off : no warning about unused units}
interface

uses
  p3d.ide, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascal3d_ide', @Register);
end.
