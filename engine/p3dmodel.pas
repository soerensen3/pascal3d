unit p3dmodel;

{$mode objfpc}{$H+}

{$DEFINE VERBOSE}
{$DEFINE BUFFERS}

interface

uses
  Classes, SysUtils, dglOpenGL, Math, p3dMath, p3dshaders, p3dshadernodes,
  p3dgeometry, LCLIntf, p3dfileutil, p3dobjects, p3dbuffers, DOM, XMLRead, p3dgenerics,
  SDL2, SDL2_image;

type
  TP3DScene = class;
  TP3DData = class;
  TP3DActor = class;
  TP3DActorList = class;

 {$DEFINE INTERFACE}

 {$INCLUDE p3dmodel_common.inc}
 {$INCLUDE p3dmodel_datablock.inc}
 {$INCLUDE p3dmodel_camera.inc}
 {$INCLUDE p3dmodel_scene.inc}
 {$INCLUDE p3dmodel_actor.inc}
 {$INCLUDE p3dmodel_texture.inc}
 {$INCLUDE p3dmodel_mesh.inc}
 {$INCLUDE p3dmodel_lighting.inc}
 {$INCLUDE p3dmodel_resource.inc}

 {$UNDEF INTERFACE}


implementation


{$DEFINE IMPLEMENTATION}

{$INCLUDE p3dmodel_common.inc}
{$INCLUDE p3dmodel_datablock.inc}
{$INCLUDE p3dmodel_camera.inc}
{$INCLUDE p3dmodel_scene.inc}
{$INCLUDE p3dmodel_actor.inc}
{$INCLUDE p3dmodel_texture.inc}
{$INCLUDE p3dmodel_mesh.inc}
{$INCLUDE p3dmodel_lighting.inc}
{$INCLUDE p3dmodel_resource.inc}

{$UNDEF IMPLEMENTATION}


end.

