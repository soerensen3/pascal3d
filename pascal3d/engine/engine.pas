{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit engine;

interface

uses
  Math3D, shaders, wavefront, Model, shadernodes, framebuffer, lighting, 
  camera, RPhysics, scene, window_sdl, texture_sdl, filewatch, skybox, 
  geometry, charactercontroller, loadmodelfile, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('engine', @Register);
end.
