unit p3dutils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil;


{$DEFINE INTERFACE}
{$INCLUDE p3dgenerics.inc}
{$INCLUDE p3dfileutil.inc}
{$INCLUDE p3dnodes.inc}
{$UNDEF INTERFACE}

var
  P3DFileWatch: TP3DFileWatchList;


procedure P3DUtilsInit;
procedure P3DUtilsFinish;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgenerics.inc}
{$INCLUDE p3dfileutil.inc}
{$INCLUDE p3dnodes.inc}
{$UNDEF IMPLEMENTATION}

procedure P3DUtilsInit;
begin
  DecimalSeparator:= '.';
  if ( not Assigned( P3DFileWatch )) then
    P3DFileWatch:= TP3DFileWatchList.Create;
end;

procedure P3DUtilsFinish;
begin
  if ( Assigned( P3DFileWatch )) then
    FreeAndNil( P3DFileWatch );
end;

finalization
  P3DUtilsFinish;


end.

