unit pascal3d.utils;

{$mode objfpc}{$H+}
{$interfaces CORBA}

interface

uses
  //Generics.Collections,
  Classes,
  SysUtils,
  strutils,
  FileUtil,
  LazFileUtils,
  LazUTF8,
  Math,
  XMLRead,
  DOM,
  fpjson,
  jsonparser,
  jsonscanner,
  typinfo,
  fgl,
  SDL2,
  LazUTF8Classes,
  p3d.math;


{$DEFINE INTERFACE}
  {$INCLUDE pascal3d.utils_lib.inc}
{$UNDEF INTERFACE}


procedure P3DUtilsInit;
procedure P3DUtilsFinish;

var
  P3DClassFactory: TP3DClassFactory = nil;
  P3DSearchPaths: TP3DSearchPathContainer = nil;
  P3DConfig: TP3DConfig = nil;
  P3DUtilsContainers: TP3DJSONRootContainerList = nil;


implementation

uses pascal3d.events;

{ gP3DStreamableList }
{
function gP3DStreamableList.Add: TP3DPropertyAccess;
begin
  Result:=inherited Add;
  P3DLog.LogInfo( Self, 'gP3DStreamableList.Add: TP3DPropertyAccess' );
end;
}
function gP3DStreamableList.Add(Item: T): Integer;
begin
  Result:=inherited Add(Item);
  P3DLog.LogInfo( Self, 'gP3DStreamableList.Add(Item: T): Integer;' );
end;

procedure gP3DStreamableList.Remove(AItem: T);
begin
  inherited Remove(AItem);
end;

procedure gP3DStreamableList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure gP3DStreamableList.Insert(Index: Integer; Item: T);
begin
  inherited Insert(Index, Item);
  P3DLog.LogInfo( Self, 'gP3DStreamableList.Insert(Item: T): Integer;' );
end;


{$DEFINE IMPLEMENTATION}
  {$INCLUDE pascal3d.utils_lib.inc}
{$UNDEF IMPLEMENTATION}


procedure P3DUtilsInit;
begin
  DecimalSeparator:= '.';

  if ( not Assigned( P3DUtilsContainers )) then
    P3DUtilsContainers:= TP3DJSONRootContainerList.Create( 'P3DUtilsContainers' );

  if ( not Assigned( P3DClassFactory )) then
    P3DClassFactory:= TP3DClassFactory.Create;

  if ( not Assigned( P3DConfig )) then
    P3DConfig:= TP3DConfig.Create;

  if ( not Assigned( P3DFilePointers )) then
    P3DFilePointers:= TP3DFilePointerList.Create;
  if ( not Assigned( P3DSearchPaths )) then
    P3DSearchPaths:= TP3DSearchPathContainer.Create( P3DUtilsContainers );

  {$DEFINE INITIALIZATION}
    {$INCLUDE pascal3d.utils_lib.inc}
  {$UNDEF INITIALIZATION}
end;

procedure P3DUtilsFinish;
begin
  if ( Assigned( P3DFilePointers )) then
    FreeAndNil( P3DFilePointers );
  if ( Assigned( P3DSearchPaths )) then
    FreeAndNil( P3DSearchPaths );
  if ( Assigned( P3DClassFactory )) then
    FreeAndNil( P3DClassFactory );
  if ( Assigned( P3DConfig )) then
    FreeAndNil( P3DConfig );
  if ( Assigned( P3DUtilsContainers )) then
    FreeAndNil( P3DUtilsContainers );
  {$DEFINE FINALIZATION}
    {$INCLUDE pascal3d.utils_lib.inc}
  {$UNDEF FINALIZATION}
end;


finalization
  P3DUtilsFinish;


end.

