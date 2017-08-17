//p3dstrutils.inc

{$IFDEF INTERFACE}

function Split( S: String; Delim: Char ): TStringList;
function Enclosed( var S: String; sStart: Char; sEnd: Char; const TrimStr: Boolean = True ): Boolean;
function FloatToStr( AValue: Float; const Precision: Integer = 2 ): String;



{$ENDIF}

{$IFDEF IMPLEMENTATION}

function Split( S: String; Delim: Char ): TStringList;
begin
  Result:= TStringList.Create;
  Result.Delimiter:= Delim;
  Result.DelimitedText:= S;
end;

function Enclosed( var S: String; sStart: Char; sEnd: Char; const TrimStr: Boolean = True ): Boolean;
begin
  if ( TrimStr ) then
    S:= Trim( S );
  Result:= ( Length( S ) > 1 ) and ( S[ 1 ] = sStart ) and ( S[ Length( S )] = sEnd );
  if ( Result ) then
    S:= Copy( S, 2, Length( S ) - 2 );
end;

function FloatToStr(AValue: Float; const Precision: Integer): String;
var
  d: String;
begin
  d:= '%.' + IntToStr( Precision ) + 'f';
  Result:= Format( d, [ AValue ]);
end;

// Credit goes to theo at lazarusforum.de
// http://www.lazarusforum.de/viewtopic.php?f=9&t=3321
Function UnicodeEscapeToUTF8(S:String): String;
Var len, i, count: integer;
  temp: String;
Begin
  len := length(S);
  SetLength(Result,len);
  i := 1;
  count := 1;
  Repeat
    If S[i]='\' Then
      If (i<len) And (S[i+1]='u') Then
        Begin
          If i<len-4 Then
            Begin
              temp := Copy(S,i+2,4);
              temp := UTF8Encode(WideChar(StrToInt('$'+temp)));
              Insert(temp,Result,Count);
              inc(Count,length(Temp));
              inc(i,6);
            End;
        End;
    Result[count] := S[i];
    inc(count);
    inc(i);
  Until i>len;
  SetLength(Result,count-1);
End;

{$ENDIF}
