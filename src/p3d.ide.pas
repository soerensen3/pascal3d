unit p3d.ide;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Contnrs,
  math,
  clipbrd,
  strutils,
  SDL2,
  LazFileUtils,
  dglOpenGL,
  p3d.math,
  p3d.utils,
  p3d.events,
  p3d.core,
  p3d.ui,
  p3d.ideintf,

  typinfo;

{$DEFINE INTERFACE}
  {$INCLUDE p3d.ide_lib.inc}
{$UNDEF INTERFACE}

procedure P3DIDEInit;
procedure P3DIDEFinish;

implementation

procedure cle( Sender: TObject; const AddMsg: String = ''  ); inline;
begin
  P3DCheckLastError( Sender, AddMsg );
end;

{$DEFINE IMPLEMENTATION}
  {$INCLUDE p3d.ide_lib.inc}
{$UNDEF IMPLEMENTATION}

procedure P3DIDEInit;
begin
  TP3DEditorLibrary.Create( P3DMainIntf.Editors );
  TP3DEditorScene.Create( P3DMainIntf.Editors );
  TP3DEditorMaterial.Create( P3DMainIntf.Editors );
  TP3DEditorTexture.Create( P3DMainIntf.Editors );
  TP3DEditorMesh.Create( P3DMainIntf.Editors );
  P3DClassFactory.AddArray([ TP3DNodeControl, TP3DObjectInspector, TP3DSceneViewer, TP3DShaderNodeEditor ]);
end;

procedure P3DIDEFinish;
begin

end;

end.

