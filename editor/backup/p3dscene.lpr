program p3dscene;

{$mode objfpc}{$H+}

uses
  //heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils,
  dglOpenGL,
  SDL2,
  p3devents,
  p3dutils,
  p3dgraphics,
  p3dgui,
  p3dscenegui,
  p3dMath;

{$INCLUDE initscene.inc}

{$INCLUDE renderscene.inc}

{$INCLUDE inputscene.inc}

procedure DumpSearchPaths;
var
  Path: String;
begin
  WriteLn( 'Basedir: ' + P3DSearchPaths.BaseDir );
  for Path in P3DSearchPaths do
    WriteLn( 'Path: ' + Path );
end;

begin
  DeleteFile( 'heap.trc' );
  SetHeapTraceOutput( 'heap.trc' );
  P3DEventsInit;
  P3DUtilsInit;
  P3DLog.FileName:= 'p3dscene.html';
  P3DGraphicsInit;
  P3DApplication.MainWindow:= TP3DWindow.Create;
  //P3DApplication.OnInit:= @Init;
  //P3DApplication.OnDeinit:= @DeInit;

  P3DApplication.MainWindow.OnRender:= @Render;
  P3DApplication.OnMouseButton:= @OnMouseButton;
  P3DApplication.OnMouseMotion:= @OnMouseMotion;
  P3DApplication.OnMouseWheel:= @OnMouseWheel;
  P3DApplication.OnKey:= @OnKey;
  P3DApplication.OnInput:= @OnInput;

  P3DApplication.Initialize;

  P3DLoadConfig( 'settings_default.xml' );
  P3DShaderNodeLib.LoadLibraryPath( P3DSearchPaths.BaseDir + 'shaders/nodes/core/', '*.pmd' );
  //Init( P3DApplication );
  P3DGUIInit;
  InitScene;

  P3DApplication.Run;
  //DumpSearchPaths;

  P3DSceneGUIFinish;
  P3DGUIFinish;
  P3DApplication.MainWindow.Free;
  P3DGraphicssFinish;
  P3DUtilsFinish;
  P3DEventsFinish;
end.
