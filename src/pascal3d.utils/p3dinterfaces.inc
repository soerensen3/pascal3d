{ p3dinterfaces.inc }

{$IFDEF INTERFACE}

    TP3DInterfacedObject = class;

    { IP3DBaseInterface }

    IP3DBaseInterface = interface ['{ADA5FD76-2F9B-4062-BAF3-945C235D4EAB}']
      function GetInstance: TObject;
      function GetInterface( const iidstr : shortstring; out obj ): boolean;
    end;


    { TP3DInterfacedObject }

    {$M+}
    TP3DInterfacedObject = class ( TObject, IP3DBaseInterface )
      function GetInstance: TObject;
    end;
    {$M-}

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DInterfacedObject }

function TP3DInterfacedObject.GetInstance: TObject;
begin
  Result:= Self;
end;


{$ENDIF}

