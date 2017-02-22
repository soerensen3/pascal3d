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
  p3devents,
  p3dutils,
  p3dgraphics,
  p3dgui,
  p3dscenegui,
  p3dMath;

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
  P3DGraphicsInit;
  P3DApplication:= TP3DSceneApplication.Create;
  P3DApplication.LoadConfig( 'settings_default.xml' );

  P3DGUIInit;
  P3DApplication.Initialize;

  P3DApplication.Run;
  //DumpSearchPaths;

  P3DGUIFinish;
  P3DGraphicsFinish;
  P3DUtilsFinish;
  P3DEventsFinish;
end.

