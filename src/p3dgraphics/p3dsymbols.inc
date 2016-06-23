//p3dsymbols.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}
{
function P3DSymbolIndex( Symbol: Char ): Integer;
function P3DCreateSymbols( FontName: String; Size: Integer ): TP3DText;

var
  P3DSymbols: TP3DText;
}

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{
function P3DCreateSymbols( FontName: String; Size: Integer ): TP3DText;
var
  Text: String;
  c: Char;
begin
  Text:= '';
  for c:= P3DSymbolArrowUp to P3DSymbolLibrary do
    Text:= Text + c;
  Result:= p3dTextSimple( Text, P3DFontManager[ FontName, Size ]);
end;

function P3DSymbolIndex(Symbol: Char): Integer;
begin
  Result:= -1;
  if ( Assigned( P3DSymbols )) then
    Result:= Pos( Symbol, P3DSymbols.Text ) - 1;
end;
}

{$ENDIF}

