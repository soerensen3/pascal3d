{$IFDEF INTERFACE}
  TCustomList = class (TPersistent)
    private
      FItems: TList;
      function GetCount: Integer;
      function GetItems( Index: Integer ): TCustomItem;

    public
      constructor Create;
      destructor Destroy; override;

      function Add( Item: TCustomItem ): Integer; virtual;
      procedure Delete( Index: Integer ); virtual;
      function IndexOf( Item: TCustomItem ): Integer; virtual;
      procedure Clear; virtual;

      property Items[ Index: Integer ]: TCustomItem read GetItems; default;
      property Count: Integer read GetCount;
  end;
{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TCustomList }
function TCustomList.GetCount: Integer;
begin
  Result:= FItems.Count;
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

procedure TCustomList.Delete( Index: Integer );
begin
  FItems.Delete( Index );
end;

function TCustomList.IndexOf( Item: TCustomItem ): Integer;
begin
  Result:= Fitems.IndexOf( Item );
end;

procedure TCustomList.Clear;
begin
  FItems.Clear;
end;

{$UNDEF IMPLEMENTATION}
{$ENDIF}