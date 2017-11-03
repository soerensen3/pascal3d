{$IFDEF INTERFACE}
type
  { gP3DClassFactory

    gP3DClassFactory is a template for a dictionary class that stores names of classes and returns the class pointer.
    This is required for streaming object properties with the JSON Loader. The specialized type is considered the base class of the dictionary.
  }

  generic gP3DClassFactory < TClassItem > = class ( specialize gP3DCustomList < TClassItem >)
    function FindByName( AName: String ): TClassItem; // Find a class by it's name. Nil is returned if the class was not found.
  end;

  { TP3DClassFactory

    A class factory with TP3DInterfacedPersistent as the base class.
  }

  TP3DClassFactory = class ( specialize gP3DClassFactory < TP3DInterfacedPersistentType >)
    function Add(Item: TCustomItem): Integer; override;  // Add's a new class to the dictionary and returns it's index.
                                                         // If the class was already added the found index is returned instead.
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

{ TP3DClassFactory }

function TP3DClassFactory.Add(Item: TCustomItem): Integer;
begin
  Result:= Find( Item );
  if ( Result = -1 ) then
    Result:= inherited Add(Item);
end;


{$ENDIF}

{$IFDEF INITIALIZATION}
  //if ( not Assigned( P3DClassFactory )) then
  //  P3DClassFactory:= TP3DClassFactory.Create;
{$ENDIF}

{$IFDEF FINALIZATION}
  //if ( Assigned( P3DClassFactory )) then
  //  FreeAndNil( P3DClassFactory );
{$ENDIF}


