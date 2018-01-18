//TP3DFile.pas
//-------------------------------------------------

//TP3DFile_Datablock.inc
//-------------------------------------------------
TP3DDataBlock = class (TP3DObject )
  File: TP3DFileWatch

//TP3DFile_Actor.inc
//-------------------------------------------------
TP3DActor = class( TP3DDataBlock )
  Position: TVec3;
  Rotation: TVec3;
  Data: TP3DDataBlock;
  procedure LoadFromDom(...);
  ...

//TP3DFile_Scene.inc
//-------------------------------------------------
TP3DScene = class ( TP3DDataBlock )
  Objects[]: TP3DActor;
  ...

//TP3DFile_Mesh.inc
//-------------------------------------------------
TP3DMesh = class( TP3DDataBlock )
  Children[]: TP3DActor;
  ...

//TP3DFile_Light.inc
//-------------------------------------------------
TP3DLight = class( TP3DDataBlock )
  ...

//TP3DFile_Camera.inc
//-------------------------------------------------
TP3DCamera = class( TP3DDataBlock )
  ...

//TP3DFile_Material.inc
//-------------------------------------------------
TP3DMaterial = class( TP3DDataBlock )
  ...

TP3DTexture = class( TP3DDataBlock )
  ...

TP3DShader = class ( TP3DDataBlock )
  ...

//TP3DFile_Data.inc
TP3DData = class 
  Objects[]: TP3DActor;
  Materials[]: TP3DMaterial;
  Scenes[]: TP3DScenes;
  Textures[]: TP3DTexture;
  Shaders[]: TP3DShader;
  Cameras[]: TP3DCameras;
  Lights[]: TP3DLights;
  
  function LoadDataBlockFromDOM(...): TP3DDataBlock;
  
