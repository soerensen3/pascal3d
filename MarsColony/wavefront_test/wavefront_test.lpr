program wavefront_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils,
  Interfaces,
  wavefront, pl_opengl
  { you can add units after this };

var
  mdl: TModel;
  i: Integer;
  j: Integer;

begin
  SetCurrentDir( '/home/soerensen/Dokumente/MarsColony/model' );
  mdl:= LoadModelFromFile( 'wall.obj' );
//  for i:= 0 to high( mdl.Vertices ) do
//    WriteLn( 'x: ', mdl.Vertices[ i ].x, ' y: ', mdl.Vertices[ i ].y, ' z: ', mdl.Vertices[ i ].z );
  for i:= 0 to high( mdl.Faces ) do
    begin
      for j:= 0 to high( mdl.Faces[ i ].v ) do
        Write( mdl.Faces[ i ].v[ j ], ' ' );
      Write( #13#10 );
    end;

  mdl.Free;
end.

