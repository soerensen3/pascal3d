unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  sysutils, strutils, p3dNodes;

function ParseString( S: String; off: Integer; out off_new: Integer ): String;
function ExtractText(const Str: string; const Delim1, Delim2: char): string;
procedure ExtractNameValuePair( S: String; Delim: Char; off: Integer; out off_new: Integer; out Name: String; out Value: String );
procedure MdParseNode( Node: TP3DNode; S: String; i: Integer );

implementation

function ParseString( S: String; off: Integer; out off_new: Integer ): String;
begin
  Result:= '';
  off_new:= Pos( '"', S );
  if ( off_new > 0 ) then
    Result:= MidStr( S, off, off_new );
end;

function ExtractText(const Str: string; const Delim1, Delim2: char): string;
var
  pos1, pos2: integer;
begin
  result := '';
  pos1 := Pos(Delim1, Str);
  pos2 := Pos(Delim2, Str);
  if (pos1 > 0) and (pos2 > pos1) then
    result := Copy(Str, pos1 + 1, pos2 - pos1 - 1);
end;

procedure ExtractNameValuePair( S: String; Delim: Char; off: Integer; out off_new: Integer; out Name: String; out Value: String );
var
  tmp: Integer;
begin
  tmp:= PosEx( Delim, S, off );
  Name:= Trim( Copy( S, off, tmp - 1 - off ));
  off_new:= PosEx( #13, S, tmp );
  if ( off_new = 0 ) then
    off_new:= Length( S );
  Value:= Trim( MidStr( S, tmp + 1, off_new ));
end;

procedure MdParseNode( Node: TP3DNode; S: String; i: Integer );
begin
  while ( i < Length( S )) do
end;

end.

