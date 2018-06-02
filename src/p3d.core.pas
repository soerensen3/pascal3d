{ # License

  Copyright (c) 2017 Johannes Rosleff Sörensen

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the
  "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish,
  distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to
  the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
  ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH
  THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


}


unit p3d.core;
{$mode objfpc}{$H+}
{$interfaces CORBA}
{$modeswitch nestedprocvars}

{.$DEFINE DEBUG_DATABLOCKS}
{.$DEFINE VERBOSE}


// IMPLEMENT Scene-Master
// IMPLEMENT Library-Master

{ SceneMaster
    Handlers
      AddObject
      RemoveObject

  LibraryMaster
      Handlers
        AddObject
        RemoveObject
        AddMesh
        RemoveMesh
        AddTexture
        RemoveTexture
        AddScene
        RemoveScene
        AddCamera
        RemoveCamera
        AddLight
        RemoveLight

}

interface

uses
  Classes,
  SysUtils,
  DOM,
  XMLRead,
  jsonparser,
  fpjson,
  typinfo,
  strutils,
  fgl,
  SDL2,
  sdl2_ttf,
  sdl2_image,
  LazFileUtils,
  dglOpenGL,
  LazUTF8Classes,
  LazMethodList,

  Math,
  p3d.math,
  p3d.utils,
  p3d.events;

{$DEFINE INTERFACE}
  {$INCLUDE p3d.core_lib.inc}
{$UNDEF INTERFACE}

var
  P3DViewports: TP3DViewportStack = nil;
  P3DShaderActive: TP3DShader = nil;
  //P3DShaderNodeLib: TP3DShaderNodeLibrary = nil;
  P3DData: TP3DData = nil;
  P3DFontManager: TP3DFontManager = nil;
  P3DFontManagerBmp: TP3DFontManagerBmp = nil;
  P3DCanvasMaterialDefault: TP3DMaterial = nil;
  P3DCanvasMaterialText: TP3DMaterial = nil;
  P3DCanvasMaterialTexture: TP3DMaterial = nil;
  P3DCanvasMaterialManMipTexture: TP3DMaterial = nil;
  P3DDataBlockCache: TP3DDataBlockCache = nil;
  P3DAttributes: TP3DAttributeList = nil;
  P3DCoreContainers: TP3DJSONRootContainerList;

procedure P3DCoreInit;
procedure P3DCoreFinish;


implementation

{$DEFINE IMPLEMENTATION}
  {$INCLUDE p3d.core_lib.inc}
{$UNDEF IMPLEMENTATION}

procedure InitGL;
begin
  InitOpenGL();
  ReadExtensions;

  // Some OpenGL configurations
  glClearColor( 0.0, 0.5, 1.0, 1.0 );
  glClearDepth( 1.0 );
  //glEnable( GL_DEPTH_TEST ); cle( nil );
  glDepthFunc( GL_LESS ); cle( nil );
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST ); cle( nil );
  glEnable( GL_BLEND ); cle( nil );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA ); cle( nil );
end;

procedure P3DCoreInit;
begin
  InitGL;

  if ( not Assigned( P3DCoreContainers )) then
    P3DCoreContainers:= TP3DJSONRootContainerList.Create( 'P3DCoreContainers' );
  if ( not Assigned( P3DAttributes )) then
    P3DAttributes:= TP3DAttributeList.Create;
  if ( not Assigned( P3DDataBlockCache )) then
    P3DDataBlockCache:= TP3DDataBlockCache.Create;
  if ( not Assigned( P3DViewports )) then
    P3DViewports:= TP3DViewportStack.Create;
  if ( not Assigned( P3DData )) then
    P3DData:= TP3DData.Create;
  if ( not Assigned( P3DClassFactory )) then
    P3DClassFactory:= TP3DClassFactory.Create;
  P3DClassFactory.AddArray([ TP3DStreamable,
                               TP3DNamedStreamable,
                                 TP3DDataBlock,
                                   TP3DObject,
                                   TP3DAction,
                                   TP3DArmature,
                                   TP3DCamera,
                                   TP3DTileGrid,
                                   TP3DLight,
                                   TP3DMaterial,
                                     TP3DMaterialBase,
                                     TP3DMaterialShader,
                                     TP3DMaterialNode,
                                   TP3DMesh,
                                   TP3DScene,
                                   TP3DTexture,
                                   TP3DFontBmp,
                               TP3DJoint,
                                 TP3DRestJoint,
                                 TP3DPoseJoint,
                               TP3DFontLetter,
                               TP3DObjectModifier,
                                 TP3DObjectModifierArmature,
                               TP3DMeshModifier,
                                 TP3DMeshModifierTerrain,
                               TP3DNode,
                               TP3DNodeSocket,
                                 TP3DShaderNodeSocket,
                             TP3DNodeClone,
                               TP3DMaterialMap,
                                 TP3DMaterialMapBase,
                             TP3DNodeSocketClone,
                               TP3DShaderNodeSocketClone,
                             //TP3DMeshMaterialGroup,
                             TP3DShader,
                             TP3DMaterialModifier,
                               TP3DMaterialModifierArmature,
                               TP3DMaterialModifierTerrain,
                               TP3DMaterialModifierText,
                               TP3DMaterialModifierArray,
                               TP3DMaterialModifierCombine,
                               TP3DMaterialModifierTransform,
                               TP3DMaterialModifierInit,
                               TP3DMaterialModifierLightning,
                               TP3DMaterialModifierMaps,
                               TP3DMaterialModifierTexTransform,
                             TP3DTransform ]);
  //if ( not Assigned( P3DShaderNodeLib )) then
  //  P3DShaderNodeLib:= TP3DShaderNodeLibrary.Create();

  if ( TTF_Init() <> 0 ) then
    raise Exception.Create( 'Cannot initialize sdl2_text!' );
  if ( not Assigned( P3DFontManager )) then
    P3DFontManager:= TP3DFontManager.Create;
  if ( not Assigned( P3DFontManagerBmp )) then
    P3DFontManagerBmp:= TP3DFontManagerBmp.Create();
  (*{$DEFINE INITIALIZATION}
  {$INCLUDE pascal3d.core_lib.inc}
  {$UNDEF INITIALIZATION}*)
end;

procedure P3DCoreFinish;
begin
  (*{$DEFINE FINALIZATION}
  {$INCLUDE pascal3d.core_lib.inc}
  {$UNDEF FINALIZATION}*)
  if ( Assigned( P3DCoreContainers )) then
    FreeAndNil( P3DCoreContainers );
  if ( Assigned( P3DFontManager )) then
    FreeAndNil( P3DFontManager );
  if ( Assigned( P3DFontManagerBmp )) then
    FreeAndNil( P3DFontManagerBmp );
  if ( Assigned( P3DViewports )) then
    FreeAndNil( P3DViewports );
  if ( Assigned( P3DData )) then
    begin
      P3DData.Free;
      P3DData:= nil;
    end;
  if ( Assigned( P3DClassFactory )) then
    FreeAndNil( P3DClassFactory );
  if ( Assigned( P3DDataBlockCache )) then
    FreeAndNil( P3DDataBlockCache );
  if ( Assigned( P3DAttributes )) then
    FreeAndNil( P3DAttributes );
end;

initialization



finalization
  P3DCoreFinish;

end.

