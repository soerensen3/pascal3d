{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit engine;

interface

uses
  input, shaders, wavefront, Model, shadernodes, framebuffer, lighting, 
  camera, RPhysics, scene, window_sdl, texture_sdl, filewatch, skybox, 
  geometry, charactercontroller, loadmodelfile, RObjects, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('engine', @Register);
end.