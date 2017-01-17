{$IFDEF INTERFACE}
type

  { gP3DClassFactory }

  generic gP3DClassFactory < TClassItem > = class ( specialize gP3DCustomList < TClassItem >)
    function FindByName( AName: String ): TClassItem;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ gP3DClassFactory }

function gP3DClassFactory.FindByName(AName: String): TClassItem;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].ClassName = AName ) then
      begin
        Result:= Items[ i ];
        exit;
      end;
end;


{$ENDIF}
