unit p3d.ideintf;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LazMethodList,
  math, fpjson,
  fgl,
  p3d.core,
  p3d.utils,
  p3d.ui;


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

{ TP3DPopupDictionary }

function TP3DPopupDictionary.FindClassPopup(AClass: TClass; var APopup: TP3DPopupMenu): Boolean;
var
  C: TClass;
begin
  C:= AClass;
  APopup:= nil;
  while Assigned( C ) and not TryGetData( C.ClassName, APopup ) do
    C:= C.ClassParent;
  Result:= Assigned( APopup );
end;




{$DEFINE IMPLEMENTATION}
  {$INCLUDE p3d.ideintf_lib.inc}
{$UNDEF IMPLEMENTATION}

end.

