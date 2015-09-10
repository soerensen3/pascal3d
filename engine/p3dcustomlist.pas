{$IFDEF INTERFACE}
  TCustomListEnumerator = class;

  TCustomList = class ( TPersistent )
    private
      FItems: TList;
      function GetCount: Integer;
      procedure SetCount( AValue: Integer );
      function GetItems( Index: Integer ): TCustomItem;

    public
      constructor Create;
      destructor Destroy; override;

      function Add( Item: TCustomItem ): Integer; virtual;
      procedure Delete( Index: Integer ); virtual;
      function IndexOf( Item: TCustomItem ): Integer; virtual;
      procedure Clear(); virtual;
      procedure Empty(); virtual;
      function GetEnumerator(): TCustomListEnumerator;
      procedure Insert( Index: Integer; Item: TCustomItem );

      property Items[ Index: Integer ]: TCustomItem read GetItems; default;
      property Count: Integer read GetCount write SetCount;
  end;

  { TCustomListEnumerator }

  TCustomListEnumerator = class
    private
      FCurrent: TCustomItem;
      FCurrentIdx: Integer;
      FCustomList: TCustomList;

    public
      constructor Create( CustomList: TCustomList );
      function MoveNext: Boolean;
      property Current: TCustomItem read FCurrent;
      property CurrentIdx: Integer read FCurrentIdx;
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

procedure TCustomList.Clear;
{$IFDEF OBJECTLIST}
var
  Itm: TCustomItem;
{$ENDIF}
begin
  {$IFDEF OBJECTLIST}
  for Itm in Self do
    Itm.Free;
  {$ENDIF}
  FItems.Clear;
end;

procedure TCustomList.Empty;
begin
  FItems.Clear;
end;

function TCustomList.GetEnumerator(): TCustomListEnumerator;
begin
  Result:= TCustomListEnumerator.Create( Self );
end;


{ TCustomListEnumerator }

constructor TCustomListEnumerator.Create( CustomList: TCustomList );
begin
  inherited Create;
  FCurrentIdx:= -1;
  FCurrent:= nil;
  FCustomList:= CustomList;
end;

function TCustomListEnumerator.MoveNext: Boolean;
begin
  Inc( FCurrentIdx );
  Result:= FCurrentIdx < FCustomList.Count;
  if ( Result ) then
    FCurrent:= FCustomList[ FCurrentIdx ];
end;


{$UNDEF IMPLEMENTATION}
{$ENDIF}
