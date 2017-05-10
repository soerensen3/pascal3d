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
{$INCLUDE p3dpropaccess.inc}
{$INCLUDE p3dinterfacedpersistent.inc}
{$INCLUDE p3dfileutil.inc}
{$INCLUDE p3dstrutils.inc}
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


{ gP3DListPropertyAccessObject }

function gP3DListPropertyAccessObject.GetCount: Integer;
begin
  if ( Assigned( Value )) then
    Result:= Value.Count
  else
    Result:= 0;
end;

function gP3DListPropertyAccessObject.GetValues(idx: Integer): TListData;
begin
  if ( Assigned( Value )) then
    Result:= Value[ idx ]
  else
    Result:= nil;
end;

procedure gP3DListPropertyAccessObject.SetCount(AValue: Integer);
begin
  if ( Assigned( Value )) then
    Value.Count:= AValue;
end;

procedure gP3DListPropertyAccessObject.SetValues(idx: Integer; AValue: TListData);
begin
  if ( Assigned( Value )) then
    Value[ idx ]:= AValue;
end;

{ gP3DTransformPersistent }

procedure gP3DTransformPersistent.Create;
begin
  inherited Create;
  {
  Properties.Add([ TP3DPropertyAccessVec3.CreateGetSet( 'Position', @ClassParent.GetPosition, @SetPosition, smAttribute ),
                   TP3DPropertyAccessVec3.CreateGetSet( 'Rotation', @GetRotation, @SetRotation ),
                   TP3DPropertyAccessRTTI.Create( Self, 'RotationOrder', smAttribute ),
                   TP3DPropertyAccessQuat.CreateGetSet( 'Quaternion', @GetQuaternion, @SetQuaternion, smAttribute ),
                   TP3DPropertyAccessVec3.CreateGetSet( 'Direction', @GetDirection, nil ),
                   TP3DPropertyAccessVec3.CreateGetSet( 'Scale', @GetScale, @SetScale, smAttribute )],
                   'Transform' );}
end;

{ gP3DInterfacedPersistentList }

procedure gP3DInterfacedPersistentList.SaveToDOM(AParent: TDOMElement);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure gP3DInterfacedPersistentList.LoadFromDOM(ADOMNode: TDOMElement);
begin

end;



{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgenerics.inc}
{$INCLUDE p3dinterfacedpersistent.inc}
{$INCLUDE p3dstrutils.inc}
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

