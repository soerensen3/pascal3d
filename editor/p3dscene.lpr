program p3dscene;

{$mode objfpc}{$H+}

uses
  heaptrc,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils,
  dglOpenGL,
  SDL2,
  p3dMath,
  pascal3d.events,
  pascal3d.utils,
  pascal3d.core,
  p3dgui,
  p3dscenegui;

procedure DumpSearchPaths;
var
  Path: String;
begin
  WriteLn( 'Basedir: ' + P3DSearchPaths.BaseDir );
  for Path in P3DSearchPaths.Paths do
    WriteLn( 'Path: ' + Path );
end;

begin
  DeleteFile( 'heap.trc' );
  SetHeapTraceOutput( 'heap.trc' );
  P3DEventsInit;
  P3DUtilsInit;
  P3DCoreInit;
  P3DApplication:= TP3DSceneApplication.Create;
  P3DApplication.LoadConfig( 'settings_default.xml' );

  P3DGUIInit;
  P3DApplication.Initialize;

  P3DApplication.Run;
  //DumpSearchPaths;

  P3DGUIFinish;
  P3DCoreFinish;
  P3DUtilsFinish;
  P3DEventsFinish;
end.

