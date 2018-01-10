//p3dgenerics.inc
//part of p3dutils.pas

{$IFDEF INTERFACE}

  type
    generic IP3DEnumeratorClass < TCustomItem >  = interface
      ['{B873CDC1-491F-4F62-A1F8-CED1C13426C4}']
      function GetCurrent: TCustomItem;
      function MoveNext: Boolean;
      procedure Reset;
      property Current: TCustomItem read GetCurrent;
    end;

    generic IP3DEnumerableClass < TCustomItem >  = interface
      ['{A8D955A0-E3A9-4BF5-8CDF-8A21AB3E2A06}']
      function GetEnumerator: specialize IP3DEnumeratorClass < TCustomItem >;
    end;

     { gP3DCustomListEnumeratorClass }
    generic gP3DCustomListEnumeratorClass < TCustomItem > =
      class ( TInterfacedPersistent, specialize IP3DEnumeratorClass < TCustomItem >)
      private type
        TMoveNext = function ( var Index: Integer ): TCustomItem of object;
        TEnumClass = specialize gP3DCustomListEnumeratorClass < TCustomItem >;

      private
        FCurrent: TCustomItem;
        FCurrentIdx: Integer;
        FMoveNext: TMoveNext;
        FStartIndex: Integer;

        function GetCurrent(): TCustomItem;
        procedure Reset();

      public
        constructor Create( AStartIndex: Integer; AMoveNext: TMoveNext );

        function MoveNext: Boolean;
        function GetEnumerator: TEnumClass;

        property Current: TCustomItem read GetCurrent;
        property CurrentIdx: Integer read FCurrentIdx;
        property StartIndex: Integer read FStartIndex;
    end;

    TP3DCustomListOnChangeAction = ( actAdd, actDelete, actClear );

    { gP3DCustomObjectList }

    generic gP3DCustomObjectList <TCustomItem> =
      class ( TPersistent{, specialize IP3DEnumerableClass < TCustomItem >} )
      type
        TP3DCustomObjectListEnumeratorClass = specialize gP3DCustomListEnumeratorClass < TCustomItem >;
        TP3DCustomListOnChangeEvent = procedure ( ASender: {specialize gP3DCustomDatablockList < TCustomItem >}TPersistent; AItem: TCustomItem; Action: TP3DCustomListOnChangeAction ) of object;

      protected
        FOwnsObjects: Boolean;
        FItems: TList;
        FLastUpdate: Integer;
        FOnChange: TP3DCustomListOnChangeEvent;

        procedure DoUpdate; virtual;
        function GetCount: Integer;
        procedure SetCount( AValue: Integer );
        function GetItem( Index: Integer ): TCustomItem;
        function MoveNext( var Index: Integer ): TCustomItem;
        function MoveBack( var Index: Integer ): TCustomItem;
        procedure SetItem( Index: Integer ; AValue: TCustomItem); virtual;

      public
        constructor Create;
        destructor Destroy; override;

        function Add( Item: TCustomItem ): Integer; virtual;
        procedure Delete( Index: Integer ); virtual;
        procedure Remove( Item: TCustomItem ); virtual;
        function IndexOf( Item: TCustomItem ): Integer; virtual;
        procedure Clear( const FreeObjects: Boolean = False ); virtual;
        function GetEnumerator: specialize gP3DCustomListEnumeratorClass < TCustomItem >;
        function Reversed(): specialize gP3DCustomListEnumeratorClass < TCustomItem >;
        //IP3DEnumerator.GetEnumerator = GetEnumerator;
        procedure Insert( Index: Integer; Item: TCustomItem );

        procedure LoadFromStream( S: TStream ); //gP3DListPropertyAccessObject needs this to compile
        procedure SaveToStream( S: TStream );

        property Items[ Index: Integer ]: TCustomItem read GetItem write SetItem; default;
        property OnChange: TP3DCustomListOnChangeEvent read FOnChange write FOnChange;
        property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;

      published
        property Count: Integer read GetCount write SetCount;
    end;

    generic IP3DEnumerator < TCustomItem > = interface
      ['{117ADC07-FB5D-4F8A-B1E2-B16912EFED68}']
      function GetCurrent: TCustomItem;
      function MoveNext: Boolean;
      procedure Reset;
      property Current: TCustomItem read GetCurrent;
    end;

    generic IP3DEnumerable < TCustomItem >  = interface
      ['{68F68805-65E7-4C7C-8662-9D4C54FE34D3}']
      function GetEnumerator: specialize IP3DEnumerator < TCustomItem >;
    end;

    { gP3DCustomListEnumerator }
    generic gP3DCustomListEnumerator < TCustomItem >
      = class ( TInterfacedPersistent, specialize IP3DEnumerator < TCustomItem >)
      private type
        TMoveNext = function ( var Index: Integer; var Value: TCustomItem ): Boolean of object;

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

    generic gP3DCustomList <TCustomItem>
      = class ( TInterfacedPersistent, specialize IP3DEnumerable < TCustomItem > )
      private type
        TItemArray = array [ 0..( MAXINT shr 8 )] of TCustomItem;
        pItemArray = ^TItemArray;
        TP3DCustomListEnumerator = specialize gP3DCustomListEnumerator < TCustomItem >;
        TItemList = specialize gP3DCustomList <TCustomItem>;

      private
        FItems: pItemArray;
        FCount: Integer;
        FCapacity: Integer;
        FGrowth: Integer;
        FSizeLimit: Integer;

        function GetItem( Index: Integer ): TCustomItem;
        procedure SetCapacity( const Value: Integer );
        procedure SetCount( AValue: Integer );
        procedure SetGrowth( const Value: Integer );
        procedure SetItem( Index: Integer; const Value: TCustomItem );
        procedure Grow;
        procedure Shrink;
        function MoveNext( var Index: Integer; var Value: TCustomItem ): Boolean;

      protected
        function GetItemSize: Integer;

      public
        constructor Create;
        destructor Destroy; override;

        function Find( Item: TCustomItem ): Integer;
        function Add( Item: TCustomItem ): Integer; virtual;
        function AddArray(Items: array of TCustomItem): Integer; virtual;
        procedure Delete( Index: Integer ); overload;
        procedure Clear;
        function PtrTo( Index: Integer ): Pointer;
        function Ptr: Pointer;
        procedure Remove( Item: TCustomItem ); virtual;
        procedure AssignTo( Dest: TItemList ); reintroduce;
        function GetEnumerator(): specialize IP3DEnumerator < TCustomItem >;

        property Items[ Index: Integer ]: TCustomItem read GetItem write SetItem; default;
        property Count: Integer read FCount write SetCount;
        property Capacity: Integer read FCapacity write SetCapacity;
        property Growth: Integer read FGrowth write SetGrowth;
        property SizeLimit: Integer read FSizeLimit;
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

function gP3DCustomObjectList.GetItem(Index: Integer): TCustomItem;
begin
  if ( Index < 0 ) then
    Index:= Count + Index;
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
  Clear();
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
        OnChange( Self, Item, actAdd );
      DoUpdate;
    end;
end;

procedure gP3DCustomObjectList.Insert( Index: Integer; Item: TCustomItem );
begin
  FItems.Insert( Index, Item );
  if ( Assigned( OnChange )) then
    OnChange( Self, Item, actAdd );
  DoUpdate;
end;

procedure gP3DCustomObjectList.LoadFromStream(S: TStream);
begin

end;

procedure gP3DCustomObjectList.SaveToStream(S: TStream);
begin

end;

procedure gP3DCustomObjectList.Delete( Index: Integer );
var
  Item: TCustomItem;
begin
  Item:= Items[ Index ];
  if ( Assigned( OnChange )) then
    OnChange( Self, Items[ Index ], actDelete );
  FItems.Delete( Index );
  if ( OwnsObjects ) then
    Item.Free;

  DoUpdate;
end;

procedure gP3DCustomObjectList.Remove(Item: TCustomItem);
begin
  if ( Assigned( OnChange )) then
    OnChange( Self, Item, actDelete );
  FItems.Remove( Item );
  DoUpdate;
  if ( OwnsObjects ) then
    Item.Free;
end;

function gP3DCustomObjectList.IndexOf( Item: TCustomItem ): Integer;
begin
  Result:= FItems.IndexOf( Item );
end;

procedure gP3DCustomObjectList.Clear( const FreeObjects: Boolean );
var
  i: Integer;
begin
  if ( Assigned( OnChange )) then
    OnChange( Self, nil, actClear );
  if ( FreeObjects ) then
    for i:= Self.Count - 1 downto 0 do //From last to first in case item is deleted from list in destructor
      begin
        if ( Assigned( OnChange )) then
          OnChange( Self, Items[ i ], actClear );
        Items[ i ].Free;
      end;

  FItems.Clear;
  DoUpdate;
end;

function gP3DCustomObjectList.GetEnumerator: TP3DCustomObjectListEnumeratorClass;
begin
  Result:= TP3DCustomObjectListEnumeratorClass.Create( -1, @MoveNext );
end;

function gP3DCustomObjectList.Reversed(): TP3DCustomObjectListEnumeratorClass;
begin
  Result:= TP3DCustomObjectListEnumeratorClass.Create( Count, @MoveBack );
end;

function gP3DCustomObjectList.MoveNext( var Index: Integer ): TCustomItem;
begin
  Inc( Index );
  if ( Index < Count ) then
    Result:= Items[ Index ]
  else
    Result:= nil;
end;

function gP3DCustomObjectList.MoveBack(var Index: Integer): TCustomItem;
begin
  Dec( Index );
  if ( Index > -1 ) then
    Result:= Items[ Index ]
  else
    Result:= nil;
end;

procedure gP3DCustomObjectList.SetItem( Index: Integer ; AValue: TCustomItem);
var
  Item: TCustomItem;
begin
  Item:= Items[ Index ];
  if ( Item = AValue ) then
    exit;

  if ( Assigned( FOnChange )) then
    OnChange( Self, Item, actDelete );

  FItems[ Index ]:= AValue;

  if ( OwnsObjects ) then
    Item.Free;

  if ( Assigned( FOnChange )) then
    OnChange( Self, AValue, actAdd );

  DoUpdate;
end;


procedure gP3DCustomObjectList.DoUpdate;
begin
  FLastUpdate:= SDL_GetTicks;
end;

{ gP3DCustomListEnumeratorClass }

function gP3DCustomListEnumeratorClass.GetCurrent: TCustomItem;
begin
  Result:= FCurrent;
end;

procedure gP3DCustomListEnumeratorClass.Reset;
begin
  FCurrent:= nil;
  FCurrentIdx:= StartIndex;
end;

constructor gP3DCustomListEnumeratorClass.Create(AStartIndex: Integer;
  AMoveNext: TMoveNext);
begin
  inherited Create;
  FStartIndex:= AStartIndex;
  FCurrentIdx:= AStartIndex;
  FCurrent:= nil;
  FMoveNext:= AMoveNext;
end;

function gP3DCustomListEnumeratorClass.MoveNext: Boolean;
begin
  FCurrent:= FMoveNext( FCurrentIdx );
  Result:= FCurrent <> nil; //Assigned( FCurrent );
end;

function gP3DCustomListEnumeratorClass.GetEnumerator: TEnumClass;
begin
  Result:= Self;
end;

{ gP3DCustomList }

function gP3DCustomList.Add( Item: TCustomItem ): Integer;
begin
  if ( FCount = FCapacity ) then
    Grow;

  FItems^[ FCount ]:= Item;
  Result:= FCount;

  Inc( FCount );
end;

function gP3DCustomList.AddArray( Items: array of TCustomItem ): Integer;
var
  i: Integer;
begin
  Result:= Count;
  for i:= Low( Items ) to High( Items ) do
    Add( Items[ I ]);
end;

procedure gP3DCustomList.AssignTo(Dest: TItemList);
begin
  Dest.Count:= Count;
  Move( FItems^, Dest.FItems^, SizeOf( TCustomItem ) * Count );
end;

function gP3DCustomList.GetEnumerator: specialize IP3DEnumerator < TCustomItem >;
begin
  Result:= TP3DCustomListEnumerator.Create( @MoveNext );
end;

procedure gP3DCustomList.Clear;
begin
  FreeMem( FItems );
  FCount:= 0;
  FCapacity:= 0;
  FItems:= nil;
end;

constructor gP3DCustomList.Create;
begin
  inherited;
  FItems:= nil;
  FCount:= 0;
  FCapacity:= 0;
  FGrowth:= 256;
  FSizeLimit:= SizeOf( TItemArray ) div SizeOf( TCustomItem );
end;

procedure gP3DCustomList.Delete( Index: Integer );
var
  i: Integer;
begin
  for i:= Index to Count - 2 do
    FItems[ i ]:= FItems[ i + 1 ];
  FCount:= FCount - 1;
end;

destructor gP3DCustomList.Destroy;
begin
  FreeMem( FItems);
  inherited;
end;

function gP3DCustomList.Find(Item: TCustomItem): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( FItems^[ i ] = Item ) then
      begin
        Result:= i;
        break;
      end;
end;

function gP3DCustomList.GetItem( Index: Integer ): TCustomItem;
begin
  if (( Index >= 0 ) and ( Index < FCount )) then
    Result:= FItems^[ Index ];
end;

procedure gP3DCustomList.Grow;
begin
  FCapacity:= FCapacity + FGrowth;
  ReallocMem( FItems, FCapacity * SizeOf( TCustomItem ));
end;

function gP3DCustomList.PtrTo(Index: Integer): Pointer;
begin
  if ( Count > Index ) then
    Result:= @FItems[ Index ]
  else
    Result:= nil;
end;

procedure gP3DCustomList.SetCount(AValue: Integer);
begin
  FCount:= AValue;
  while ( FCapacity < FCount ) do
    Grow;
  while ( FCapacity > FCount + FGrowth ) do
    Shrink;
end;

procedure gP3DCustomList.SetCapacity( const Value: Integer );
begin
  FCapacity:= Value;
  if ( FCapacity < FCount ) then
    FCapacity:= FCount;
  ReallocMem( FItems, FCapacity * SizeOf( TCustomItem ));
end;

procedure gP3DCustomList.SetGrowth(const Value: Integer);
begin
  FGrowth:= Math.Max( 16, Value ); // Minimum Value 16
end;

procedure gP3DCustomList.SetItem( Index: Integer; const Value: TCustomItem );
begin
  FItems^[ Index ]:= Value;
end;

procedure gP3DCustomList.Remove(Item: TCustomItem);
var
  Idx: Integer;
begin
  Idx:= Find( Item );
  if ( Idx > -1 ) then
    Delete( Idx );
end;

procedure gP3DCustomList.Shrink;
begin
  FCapacity:= Math.Max( 0, FCapacity - FGrowth );
  ReallocMem( FItems, FCapacity * SizeOf( TCustomItem ));
end;

function gP3DCustomList.Ptr: Pointer;
begin
  Result:= FItems;
end;

function gP3DCustomList.GetItemSize: Integer;
begin
  Result:= SizeOf( TCustomItem );
end;

function gP3DCustomList.MoveNext( var Index: Integer; var Value: TCustomItem ): Boolean;
begin
  Inc( Index );
  Result:= Index < Count;
  if ( Result ) then
    Value:= Items[ Index ];
end;

{ gP3DCustomListEnumerator }

function gP3DCustomListEnumerator.GetCurrent: TCustomItem;
begin
  Result:= FCurrent;
end;

procedure gP3DCustomListEnumerator.Reset;
begin
  FCurrentIdx:= -1;
end;

constructor gP3DCustomListEnumerator.Create( AMoveNext: TMoveNext );
begin
  inherited Create;
  FCurrentIdx:= -1;
  FMoveNext:= AMoveNext;
end;

function gP3DCustomListEnumerator.MoveNext: Boolean;
begin
  Result:= FMoveNext( FCurrentIdx, FCurrent );
end;



{$ENDIF}

