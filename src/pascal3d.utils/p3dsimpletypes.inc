{$IFDEF INTERFACE}
type
  TBVec2 = array[ 0..1 ] of Boolean;
  TBVec2Array = array [ 0..( MAXINT shr 8 )] of TBVec2;
  TBVec3 = array[ 0..2 ] of Boolean;
  TBVec3Array = array [ 0..( MAXINT shr 8 )] of TBVec3;
  TBVec4 = array[ 0..3 ] of Boolean;
  TBVec4Array = array [ 0..( MAXINT shr 8 )] of TBVec4;


function P3DStrToBoolDef( S: String; Default: Boolean ): Boolean;
function P3DTextToHTML(Txt: string): string; //Modified some Code of CodeHelp.pas by Mattias Gaertner
{$ENDIF}

{$IFDEF IMPLEMENTATION}
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
{$ENDIF}
