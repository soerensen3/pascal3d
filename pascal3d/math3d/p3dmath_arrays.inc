
{$IFDEF INTERFACE}
type
    TItemArray = array [ 0..( MAXINT shr 8 )] of TItem;
    pItemArray = ^TItemArray;

    { TItemList }

    TListEnumerator = class;
    TItemList = class
      private
        FItems: pItemArray;
        FCount: Integer;
        FCapacity: Integer;
        FGrowth: Integer;
        FSizeLimit: Integer;

        function GetItem( Index: Integer ): TItem;
        procedure SetCapacity( const Value: Integer );
        procedure SetCount( AValue: Integer );
        procedure SetGrowth( const Value: Integer );
        procedure SetItem( Index: Integer; const Value: TItem );
        procedure Grow;
        procedure Shrink;

      protected
        function GetItemSize: Integer;

      public
        constructor Create;
        destructor Destroy; override;

        function Add( Item: TItem ): Integer; overload;
        function Add( Items: array of TItem ): Integer; overload;
        procedure Delete( Index: Integer ); overload;
        procedure Clear;
        function PtrTo( Index: Integer ): Pointer;
        function Ptr: Pointer;
        procedure AssignTo( Dest: TItemList );
        function GetEnumerator(): TListEnumerator;

        property Items[ Index: Integer ]: TItem read GetItem write SetItem; default;
        property Count: Integer read FCount write SetCount;
        property Capacity: Integer read FCapacity write SetCapacity;
        property Growth: Integer read FGrowth write SetGrowth;
        property SizeLimit: Integer read FSizeLimit;
    end;

    { TListEnumerator }

    TListEnumerator = class
    private
      FCurrent: TItem;
      FCurrentIdx: Integer;
      FItemList: TItemList;

    public
      constructor Create( ItemList: TItemList );
      function MoveNext: Boolean;
      property Current: TItem read FCurrent;
      property CurrentIdx: Integer read FCurrentIdx;
    end;
{$ENDIF}


{$IFDEF IMPLEMENTATION}

function TItemList.Add( Item: TItem ): Integer;
begin
  if ( FCount = FCapacity ) then
    Grow;

  FItems^[ FCount ]:= Item;
  Result:= FCount;

  Inc( FCount );
end;

function TItemList.Add( Items: array of TItem ): Integer;
var
  i: Integer;
begin
  Result:= Count;
  for i:= Low( Items ) to High( Items ) do
    Add( Items[ I ]);
end;

procedure TItemList.AssignTo( Dest: TItemList );
begin
  Dest.Count:= Count;
  Move( FItems^, Dest.FItems^, SizeOf( TItem ) * Count );
end;

function TItemList.GetEnumerator: TListEnumerator;
begin
  Result:= TListEnumerator.Create( Self );
end;

procedure TItemList.Clear;
begin
  FreeMem( FItems );
  FCount:= 0;
  FCapacity:= 0;
  FItems:= nil;
end;

constructor TItemList.Create;
begin
  inherited;
  FItems:= nil;
  FCount:= 0;
  FCapacity:= 0;
  FGrowth:= 256;
  FSizeLimit:= SizeOf( TItemArray ) div SizeOf( TItem );
end;

procedure TItemList.Delete( Index: Integer );
var
  i: Integer;
begin
  for i:= Index to Count - 2 do
    FItems[ i ]:= FItems[ i + 1 ];
  FCount:= FCount - 1;
end;

destructor TItemList.Destroy;
begin
  FreeMem( FItems);
  inherited;
end;

function TItemList.GetItem( Index: Integer ): TItem;
begin
  if (( Index >= 0 ) and ( Index < FCount )) then
    Result:= FItems^[ Index ];
end;

procedure TItemList.Grow;
begin
  FCapacity:= FCapacity + FGrowth;
  ReallocMem( FItems, FCapacity * SizeOf( TItem ));
end;

function TItemList.PtrTo(Index: Integer): Pointer;
begin
  if ( Count > Index ) then
    Result:= @FItems[ Index ]
  else
    Result:= nil;
end;

procedure TItemList.SetCount(AValue: Integer);
begin
  FCount:= AValue;
  while ( FCapacity < FCount ) do
    Grow;
  while ( FCapacity > FCount + FGrowth ) do
    Shrink;
end;

procedure TItemList.SetCapacity( const Value: Integer );
begin
  FCapacity:= Value;
  if ( FCapacity < FCount ) then
    FCapacity:= FCount;
  ReallocMem( FItems, FCapacity * SizeOf( TItem ));
end;

procedure TItemList.SetGrowth(const Value: Integer);
begin
  FGrowth:= Math.Max( 16, Value ); // Minimum Value 16
end;

procedure TItemList.SetItem( Index: Integer; const Value: TItem );
begin
  FItems^[ Index ]:= Value;
end;

procedure TItemList.Shrink;
begin
  FCapacity:= Math.Max( 0, FCapacity - FGrowth );
  ReallocMem( FItems, FCapacity * SizeOf( TItem ));
end;

function TItemList.Ptr: Pointer;
begin
  Result:= FItems;
end;

function TItemList.GetItemSize: Integer;
begin
  Result:= SizeOf( TItem );
end;

{ TListEnumerator }

constructor TListEnumerator.Create( ItemList: TItemList );
begin
  inherited Create;
  FCurrentIdx:= -1;
  FillByte( FCurrent, SizeOf( FCurrent ), 0 );
  FItemList:= ItemList;
end;

function TListEnumerator.MoveNext: Boolean;
begin
  Inc( FCurrentIdx );
  Result:= FCurrentIdx < FItemList.Count;
  if ( Result ) then
    FCurrent:= FItemList[ FCurrentIdx ];
end;

{$ENDIF IMPLEMENTATION}
