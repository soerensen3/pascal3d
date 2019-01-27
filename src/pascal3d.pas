{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pascal3d;

{$warn 5023 off : no warning about unused units}
interface

uses
  p3d.core, p3d.events, p3d.utils, p3d.base, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('pascal3d', @Register);
end.
