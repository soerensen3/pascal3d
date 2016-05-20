//p3dgenerics.inc
//part of p3dutils.pas

{$IFDEF INTERFACE}

  type
    generic IP3DEnumerator < TCustomItem: TObject >  = interface
      ['{B873CDC1-491F-4F62-A1F8-CED1C13426C4}']
      function GetCurrent: TCustomItem;
      function MoveNext: Boolean;
      procedure Reset;
      property Current: TCustomItem read GetCurrent;
    end;
    generic IP3DEnumerable < TCustomItem: TObject >  = interface
      ['{A8D955A0-E3A9-4BF5-8CDF-8A21AB3E2A06}']
      function GetEnumerator: specialize IP3DEnumerator < TCustomItem >;
    end;

     { gP3DCustomListEnumerator }
     generic gP3DCustomListEnumerator < TCustomItem: TObject > = class ( TInterfacedPersistent, specialize IP3DEnumerator < TCustomItem >)
       private type
         TMoveNext = function ( var Index: Integer ): TCustomItem of object;

       private
         FCurrent: TCustomItem;
         FCurrentIdx: Integer;
         FMoveNext: TMoveNext;
         function GetCurrent(): TCustomItem;
         procedure Reset();

       public
         constructor Create( AMoveNext: TMoveNext );
         function MoveNext: Boolean;
         property Current: TCustomItem read GetCurrent;
         property CurrentIdx: Integer read FCurrentIdx;
     end;

    { gP3DCustomObjectList }

    generic gP3DCustomObjectList <TCustomItem: TObject> = class ( TInterfacedPersistent, specialize IP3DEnumerable < TCustomItem > )
      type
        TP3DCustomObjectListEnumerator = specialize gP3DCustomListEnumerator < TCustomItem >;

      private
        FItems: TList;
        FOnChange: TNotifyEvent;
        function GetCount: Integer;
        procedure SetCount( AValue: Integer );
        function GetItems( Index: Integer ): TCustomItem;
        function MoveNext( var Index: Integer ): TCustomItem;

      public
        constructor Create;
        destructor Destroy; override;

        function Add( Item: TCustomItem ): Integer; virtual;
        procedure Delete( Index: Integer ); virtual;
        function IndexOf( Item: TCustomItem ): Integer; virtual;
        procedure Clear( const FreeObjects: Boolean = False ); virtual;
        function GetEnumerator: specialize IP3DEnumerator < TCustomItem >;
        //IP3DEnumerator.GetEnumerator = GetEnumerator;
        procedure Insert( Index: Integer; Item: TCustomItem );

        property Items[ Index: Integer ]: TCustomItem read GetItems; default;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;

      published
        property Count: Integer read GetCount write SetCount;
    end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ gP3DCustomObjectList }
function gP3DCustomObjectList.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure gP3DCustomObjectList.SetCount( AValue: Integer );
begin
  FItems.Count:= AValue;
end;

function gP3DCustomObjectList.GetItems( Index: Integer ): TCustomItem;
begin
  if (( Index < Count ) and ( Index > -1 )) then
    Result:= TCustomItem( FItems[ Index ])
  else
    Result:= nil;
end;

constructor gP3DCustomObjectList.Create;
begin
  inherited;
  FItems:= TList.Create;
end;

destructor gP3DCustomObjectList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function gP3DCustomObjectList.Add( Item: TCustomItem ): Integer;
begin
  Result:= IndexOf( Item );
  if ( Result = -1 ) then
    begin
      Result:= FItems.Add( Item );
      if ( Assigned( OnChange )) then
        OnChange( Self );
    end;
end;

procedure gP3DCustomObjectList.Insert( Index: Integer; Item: TCustomItem );
begin
  FItems.Insert( Index, Item );
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

procedure gP3DCustomObjectList.Delete( Index: Integer );
begin
  FItems.Delete( Index );
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

function gP3DCustomObjectList.IndexOf( Item: TCustomItem ): Integer;
begin
  Result:= Fitems.IndexOf( Item );
end;

procedure gP3DCustomObjectList.Clear( const FreeObjects: Boolean );
var
  i: Integer;
begin
  if ( FreeObjects ) then
    for i:= 0 to Self.Count - 1 do
      Items[ i ].Free;

  FItems.Clear;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

function gP3DCustomObjectList.GetEnumerator: specialize IP3DEnumerator < TCustomItem >;
begin
  Result:= TP3DCustomObjectListEnumerator.Create( @MoveNext );
end;

function gP3DCustomObjectList.MoveNext( var Index: Integer ): TCustomItem;
begin
  Inc( Index );
  if ( Index < Count ) then
    Result:= Items[ Index ]
  else
    Result:= nil;
end;


{ TCustomListEnumerator }

function gP3DCustomListEnumerator.GetCurrent: TCustomItem;
begin
  Result:= FCurrent;
end;

procedure gP3DCustomListEnumerator.Reset;
begin
  FCurrent:= nil;
  FCurrentIdx:= -1;
end;

constructor gP3DCustomListEnumerator.Create( AMoveNext: TMoveNext );
begin
  inherited Create;
  FCurrentIdx:= -1;
  FCurrent:= nil;
  FMoveNext:= AMoveNext;
end;

function gP3DCustomListEnumerator.MoveNext: Boolean;
begin
  FCurrent:= FMoveNext( FCurrentIdx );
  Result:= FCurrent <> nil; //Assigned( FCurrent );
end;


{$ENDIF}

