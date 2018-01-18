{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit p3d_lclintf;

interface

uses
  P3DEditorMainWindow, p3dlclceditorscene, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('P3DEditorMainWindow', @P3DEditorMainWindow.Register);
end;

initialization
  RegisterPackage('p3d_lclintf', @Register);
end.
