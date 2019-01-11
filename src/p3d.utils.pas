unit p3d.utils;

{$mode objfpc}{$H+}
{$interfaces CORBA}

interface

uses
  Classes,
  SysUtils,
  strutils,
  FileUtil,
  LazFileUtils,
  LazUTF8,
  Math,
  fpjson,
  jsonparser,
  //jsonscanner,
  typinfo,
  //SDL2,
  //LazUTF8Classes,
  contnrs,
  p3d.math;


{$DEFINE INTERFACE}
  {$INCLUDE p3d.utils_lib.inc}
{$UNDEF INTERFACE}


procedure P3DUtilsInit;
procedure P3DUtilsFinish;

var
  P3DClassFactory: TP3DClassFactory = nil;
  P3DSearchPaths: TP3DSearchPathContainer = nil;
  P3DConfig: TP3DConfig = nil;
  P3DUtilsContainers: TP3DJSONRootContainerList = nil;


implementation



{$DEFINE IMPLEMENTATION}
  {$INCLUDE p3d.utils_lib.inc}
{$UNDEF IMPLEMENTATION}


procedure P3DUtilsInit;
begin
  DecimalSeparator:= '.';

  if ( not Assigned( P3DClassFactory )) then
    P3DClassFactory:= TP3DClassFactory.Create;

  P3DClassFactory.AddArray([ TP3DStreamable, TP3DNamedStreamable ]);

  if ( not Assigned( P3DConfig )) then
    P3DConfig:= TP3DConfig.Create;

  if ( not Assigned( P3DFilePointers )) then
    begin
      P3DFilePointers:= TP3DFilePointerList.Create;
      P3DFilePointers.OwnsObjects:= False;
    end;
  if ( not Assigned( P3DSearchPaths )) then
    P3DSearchPaths:= TP3DSearchPathContainer.Create;

  {$DEFINE INITIALIZATION}
    {$INCLUDE p3d.utils_lib.inc}
  {$UNDEF INITIALIZATION}
end;

procedure P3DUtilsFinish;
begin
  if ( Assigned( P3DFilePointers )) then
    FreeAndNil( P3DFilePointers );
  if ( Assigned( P3DConfig )) then
    FreeAndNil( P3DConfig );
  if ( Assigned( P3DSearchPaths )) then
    FreeAndNil( P3DSearchPaths );
  if ( Assigned( P3DUtilsContainers )) then
    FreeAndNil( P3DUtilsContainers );
  if ( Assigned( P3DClassFactory )) then
    FreeAndNil( P3DClassFactory );
  {$DEFINE FINALIZATION}
    {$INCLUDE p3d.utils_lib.inc}
  {$UNDEF FINALIZATION}
end;


finalization
  P3DUtilsFinish;


end.

