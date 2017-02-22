unit p3dutils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  strutils,
  LazFileUtils,
  LazUTF8,
  Math,
  XMLRead,
  DOM,
  typinfo,
  p3dMath;


{$DEFINE INTERFACE}
{$INCLUDE p3dgenerics.inc}
{$INCLUDE p3dinterfacedpersistent.inc}
{$INCLUDE p3dfileutil.inc}
{$INCLUDE p3dpropaccess.inc}
{$INCLUDE p3dxmlutils.inc}
{$INCLUDE p3dnodes.inc}
{$INCLUDE p3dsimpletypes.inc}
{$UNDEF INTERFACE}

var
  P3DFileWatch: TP3DFileWatchList;
  P3DSearchPaths: TP3DSearchPathContainer;


procedure P3DUtilsInit;
procedure P3DUtilsFinish;

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgenerics.inc}
{$INCLUDE p3dinterfacepersistent.inc}
{$INCLUDE p3dfileutil.inc}
{$INCLUDE p3dpropaccess.inc}
{$INCLUDE p3dxmlutils.inc}
{$INCLUDE p3dnodes.inc}
{$INCLUDE p3dsimpletypes.inc}
{$UNDEF IMPLEMENTATION}

procedure P3DUtilsInit;
begin
  DecimalSeparator:= '.';
  if ( not Assigned( P3DFileWatch )) then
    P3DFileWatch:= TP3DFileWatchList.Create;
  if ( not Assigned( P3DSearchPaths )) then
    P3DSearchPaths:= TP3DSearchPathContainer.Create;
end;

procedure P3DUtilsFinish;
begin
  if ( Assigned( P3DFileWatch )) then
    FreeAndNil( P3DFileWatch );
  if ( Assigned( P3DSearchPaths )) then
    FreeAndNil( P3DSearchPaths );
end;


finalization
  P3DUtilsFinish;


end.

