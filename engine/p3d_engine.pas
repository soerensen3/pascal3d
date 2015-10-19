{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit p3d_engine;

interface

uses
  p3dinput, p3dshaders, p3dwavefront, p3dmodel, p3dshadernodes, 
  p3dframebuffer, p3dcamera, p3dscene, p3dwindow, p3dtexture, p3dfilewatch, 
  p3dskybox, p3dgeometry, p3dobjects, p3dbmpfont, p3dbmpfontfile, p3dbuffers, 
  p3dviewport, p3dfileutil, p3dcanvas, p3dSDLApplication, p3dNodes, 
  p3dgenerics, p3dtext, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('p3d_engine', @Register);
end.
