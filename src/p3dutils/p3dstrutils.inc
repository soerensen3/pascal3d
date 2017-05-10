//p3dstrutils.inc

{$IFDEF INTERFACE}

function Split( S: String; Delim: Char ): TStringList;
function Enclosed( var S: String; sStart: Char; sEnd: Char; const TrimStr: Boolean = True ): Boolean;


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


{$ENDIF}
