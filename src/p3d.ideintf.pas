unit p3d.ideintf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LazMethodList,
  math, fpjson,
  p3d.core,
  p3d.utils;


{$DEFINE INTERFACE}
  {$INCLUDE p3d.ideintf_lib.inc}
{$UNDEF INTERFACE}

procedure P3DIDEIntfInit;
procedure P3DIDEIntfFinish;

var
  P3DMainIntf: TP3DMainIntf = nil;

implementation

procedure P3DIDEIntfInit;
begin
  if ( not Assigned( P3DMainIntf )) then
    P3DMainIntf:= TP3DMainIntf.Create;
end;

procedure P3DIDEIntfFinish;
begin
  P3DMainIntf.Free;
  P3DMainIntf:= nil;
end;


{$DEFINE IMPLEMENTATION}
  {$INCLUDE p3d.ideintf_lib.inc}
{$UNDEF IMPLEMENTATION}

end.

