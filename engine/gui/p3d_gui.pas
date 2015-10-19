{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit p3d_gui;

interface

uses
  p3dgui, p3dgui_buttons, p3dgui_stdctrls, p3dgui_shadernodes, 
  p3dgui_sceneviewer, p3dgui_menus, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('p3d_gui', @Register);
end.
