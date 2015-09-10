{$IFDEF INTERFACE}

  { TCustomList }

  generic TCustomList <TCustomItem> = class ( TPersistent )
    private
      { TCustomListEnumerator }
      type TCustomListEnumerator = class
        private type
          TMoveNext = function ( var Index: Integer ): TCustomItem of object;

        private
          FCurrent: TCustomItem;
          FCurrentIdx: Integer;
          FCustomList: TCustomList;
          FMoveNext: TMoveNext;

        public
          constructor Create( AMoveNext: TMoveNext );
          function MoveNext: Boolean;
          property Current: TCustomItem read FCurrent;
          property CurrentIdx: Integer read FCurrentIdx;
      end;

    private
      FItems: TList;
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
      procedure Clear({$IFDEF OBJECTLIST} const FreeObjects: Boolean = False {$ENDIF}); virtual;
      function GetEnumerator(): TCustomListEnumerator;
      procedure Insert( Index: Integer; Item: TCustomItem );

      property Items[ Index: Integer ]: TCustomItem read GetItems; default;
      property Count: Integer read GetCount write SetCount;
  end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TCustomList }
function TCustomList.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure TCustomList.SetCount( AValue: Integer );
begin
  FItems.Count:= AValue;
end;

function TCustomList.GetItems( Index: Integer ): TCustomItem;
begin
  Result:= TCustomItem( FItems[ Index ]);
end;

constructor TCustomList.Create;
begin
  inherited;
  FItems:= TList.Create;
end;

destructor TCustomList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TCustomList.Add( Item: TCustomItem ): Integer;
begin
  Result:= FItems.Add( Item );
end;

procedure TCustomList.Insert( Index: Integer; Item: TCustomItem );
begin
  FItems.Insert( Index, Item );
end;

procedure TCustomList.Delete( Index: Integer );
begin
  FItems.Delete( Index );
end;

function TCustomList.IndexOf( Item: TCustomItem ): Integer;
begin
  Result:= Fitems.IndexOf( Item );
end;

procedure TCustomList.Clear( {$IFDEF OBJECTLIST} const FreeObjects: Boolean {$ENDIF} );
{$IFDEF OBJECTLIST}
var
  Itm: TCustomItem;
{$ENDIF}
begin
  {$IFDEF OBJECTLIST}
  if ( FreeObjects ) then
    for Itm in Self do
      Itm.Free;
  {$ENDIF}
  FItems.Clear;
end;

function TCustomList.GetEnumerator(): TCustomListEnumerator;
begin
  Result:= TCustomListEnumerator.Create( @MoveNext );
end;

function TCustomList.MoveNext( var Index: Integer ): TCustomItem;
begin
  Inc( Index );
  if ( Index < Count ) then
    Result:= Items[ Index ]
  else
    Result:= nil;
end;


{ TCustomListEnumerator }

constructor TCustomList.TCustomListEnumerator.Create(
  AMoveNext: TMoveNext);
begin
  inherited Create;
  FCurrentIdx:= -1;
  FCurrent:= nil;
  FMoveNext:= AMoveNext;
end;

function TCustomList.TCustomListEnumerator.MoveNext: Boolean;
begin
  FCurrent:= FMoveNext( FCurrentIdx );
  Result:= Assigned( FCurrent );
end;


{$UNDEF IMPLEMENTATION}
{$ENDIF}
