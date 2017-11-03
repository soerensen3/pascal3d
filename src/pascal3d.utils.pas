unit pascal3d.utils;

{$mode objfpc}{$H+}

interface

uses
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
  p3dMath;


{$DEFINE INTERFACE}
  {$INCLUDE pascal3d.utils_lib.inc}
{$UNDEF INTERFACE}


procedure P3DUtilsInit;
procedure P3DUtilsFinish;

var
  P3DClassFactory: TP3DClassFactory = nil;
  P3DConfig: TP3DConfig = nil;

implementation

{ TP3DPropertyAccessFilePointerList }

function TP3DPropertyAccessFilePointerList.GetArrayAsProp(ArrayIndex: Integer): TP3DPropertyAccess;
begin
  Result:= inherited GetArrayAsProp(ArrayIndex);
  if ( Result is TP3DPropertyAccessFilePointer ) then
    TP3DPropertyAccessFilePointer( Result ).PathMode:= PathMode;
end;

{ gP3DPropertyAccessInterfacedPersistent_NoCreate }

procedure gP3DPropertyAccessInterfacedPersistent_NoCreate.ValueCreateNew(ClTp: TP3DInterfacedPersistentType; AContext: TP3DJSONContext);
begin
  if ( Assigned( Value )) then
    Value.LoadFromJSONContext( AContext );
end;



{$DEFINE IMPLEMENTATION}
  {$INCLUDE pascal3d.utils_lib.inc}
{$UNDEF IMPLEMENTATION}


procedure P3DUtilsInit;
begin
  DecimalSeparator:= '.';
  if ( not Assigned( P3DClassFactory )) then
    P3DClassFactory:= TP3DClassFactory.Create;

  if ( not Assigned( P3DConfig )) then
    P3DConfig:= TP3DConfig.Create;

  {$DEFINE INITIALIZATION}
    {$INCLUDE pascal3d.utils_lib.inc}
  {$UNDEF INITIALIZATION}
end;

procedure P3DUtilsFinish;
begin
  if ( Assigned( P3DClassFactory )) then
    FreeAndNil( P3DClassFactory );
  if ( Assigned( P3DConfig )) then
    FreeAndNil( P3DConfig );
  {$DEFINE FINALIZATION}
    {$INCLUDE pascal3d.utils_lib.inc}
  {$UNDEF FINALIZATION}
end;


finalization
  P3DUtilsFinish;


end.

