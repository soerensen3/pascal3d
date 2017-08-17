unit pascal3d.utils;

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
  fpjson,
  jsonparser,
  jsonscanner,
  typinfo,
  fgl,
  SDL2,
  p3dMath;


{$DEFINE INTERFACE}
  {$INCLUDE pascal3d.utils_lib.inc}
{$UNDEF INTERFACE}


procedure P3DUtilsInit;
procedure P3DUtilsFinish;

var
  P3DClassFactory: TP3DClassFactory = nil;

implementation


{$DEFINE IMPLEMENTATION}
  {$INCLUDE pascal3d.utils_lib.inc}
{$UNDEF IMPLEMENTATION}


procedure P3DUtilsInit;
begin
  DecimalSeparator:= '.';
  if ( not Assigned( P3DClassFactory )) then
    P3DClassFactory:= TP3DClassFactory.Create;

  {$DEFINE INITIALIZATION}
    {$INCLUDE pascal3d.utils_lib.inc}
  {$UNDEF INITIALIZATION}
end;

procedure P3DUtilsFinish;
begin
  {$DEFINE FINALIZATION}
    {$INCLUDE pascal3d.utils_lib.inc}
  {$UNDEF FINALIZATION}
end;


finalization
  P3DUtilsFinish;


end.

