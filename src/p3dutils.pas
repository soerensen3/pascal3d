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
{$INCLUDE p3dfileutil.inc}
{$INCLUDE p3dxmlutils.inc}
{$INCLUDE p3dnodes.inc}
{$INCLUDE p3dsimpletypes.inc}
{$INCLUDE p3dpropaccess.inc}
{$UNDEF INTERFACE}

var
  P3DFileWatch: TP3DFileWatchList;
  P3DSearchPaths: TP3DSearchPathContainer;


procedure P3DUtilsInit;
procedure P3DUtilsFinish;

function P3DStrToBoolDef( S: String; Default: Boolean ): Boolean;
function P3DTextToHTML(Txt: string): string; //Modified some Code of CodeHelp.pas by Mattias Gaertner

implementation

{ TP3DPropertyAccessFileWatch }

function TP3DPropertyAccessFileWatch.GetAsString: String;
begin
  if ( Assigned( Value )) then
    if ( AbsolutePaths ) then
      Result:= Value.FileName
    else
      Result:= ExtractRelativepath( AppendPathDelim( GetCurrentDir()), Value.FileName );
end;

procedure TP3DPropertyAccessFileWatch.SetAsString(AValue: String);
begin
  if ( Assigned( Value )) then
    Value.FileName:= AValue;
end;

function TP3DPropertyAccessFileWatch.GetDefaultValue: TP3DFileWatch;
begin
  Result:= nil;
end;

{ TP3DInterfacedPersistent }

constructor TP3DInterfacedPersistent.Create;
begin
  inherited Create;
  Properties:= TP3DPropertyAccessList.Create( Self );
end;

destructor TP3DInterfacedPersistent.Destroy;
begin
  Properties.Clear( True );
  Properties.Free;
  inherited Destroy;
end;

function TP3DInterfacedPersistent.SaveToDOM( AParent: TDOMElement ): TDOMElement;
var
  i: Integer;
begin
  Result:= AParent.OwnerDocument.CreateElement( DOMNodeName );
  AParent.AppendChild( Result );

  for i:= 0 to Properties.Count - 1 do
    Properties[ i ].SaveToDOM( Result );
end;

procedure TP3DInterfacedPersistent.LoadFromDOMNew(ADOMNode: TDOMElement);
var
  i, propI: Integer;
begin
  for i:= 0 to ADOMNode.Attributes.Length - 1 do
    begin
      //WriteLn( ADOMNode.Attributes[ i ].NodeName, ' = ', ADOMNode.Attributes[ i ].NodeValue );
      propI:= Properties.FindByName( ADOMNode.Attributes[ i ].NodeName );
      if ( propI > -1 ) then
        Properties[ propI ].AsString:= ADOMNode.Attributes[ i ].NodeValue;
    end;
end;


{ TP3DNodeList }

function TP3DNodeList.Find( Name: String ): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = Name ) then
      begin
        Result:= i;
        break;
      end;
end;

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgenerics.inc}
{$INCLUDE p3dfileutil.inc}
{$INCLUDE p3dxmlutils.inc}
{$INCLUDE p3dnodes.inc}
{$INCLUDE p3dsimpletypes.inc}
{$INCLUDE p3dpropaccess.inc}
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

function P3DStrToBoolDef(S: String; Default: Boolean): Boolean;
begin
  case ( S ) of
    'yes': Result:= True;
    'no': Result:= False;
    else
      Result:= Default;
  end;
end;

function P3DTextToHTML(Txt: string): string; //Modified some Code of CodeHelp.pas by Mattias Gaertner
var
  p: Integer;
begin
  Result:=Txt;
  p:=length(Result);
  while p>0 do
  begin
    case Result[p] of
    '<': Result:=copy(Result,1,p-1)+'&lt;'+copy(Result,p+1,length(Result));
    '>': Result:=copy(Result,1,p-1)+'&gt;'+copy(Result,p+1,length(Result));
    '&': Result:=copy(Result,1,p-1)+'&amp;'+copy(Result,p+1,length(Result));
    {#10,#13:
      begin
        if (p>1) and (Result[p-1] in [#10,#13]) and (Result[p-1]<>Result[p]) then
          dec(p);
        Result:=copy(Result,1,p-1)+'<br /> '+copy(Result,p,length(Result));
      end;}
    end;
    dec(p);
  end;
end;

finalization
  P3DUtilsFinish;


end.

