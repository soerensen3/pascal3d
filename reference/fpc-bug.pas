unit p3dgenerics;

{$mode objfpc}{$H+}

interface
  uses
    Classes, sysutils;

  type

    { TCustomListEnumerator }

    generic TCustomListEnumerator < TCustomItem > = class
      private type
        TGetCurrent = function( const Index: Integer ): TCustomItem of object;
        TMoveNext = function( var Index: Integer ): Boolean of object;

      private
        FCurrentIdx: Integer;
        FGetCurrent: TGetCurrent;
        FMoveNext: TMoveNext;
        function GetCurrent: TCustomItem;

      public
        procedure Init( const StartIdx: Integer; const AMoveNext: TMoveNext; const AGetCurrent: TGetCurrent);

        function MoveNext: Boolean;
        property Current: TCustomItem read GetCurrent;
        property CurrentIdx: Integer read FCurrentIdx;
    end;

    generic TCustomList <TCustomItem> = class ( TPersistent )
      private type
        TListEnumerator = specialize TCustomListEnumerator < TCustomItem >;

      private
        FItems: TList;

        function MoveNext( var Index: Integer ): Boolean;
        function GetCount: Integer;
        procedure SetCount( AValue: Integer );
        function GetItems( Index: Integer ): TCustomItem;
        function GetEnumerator(): TListEnumerator;

      public
        constructor Create;
        destructor Destroy; override;

        function Add( Item: TCustomItem ): Integer; virtual;
        procedure Delete( Index: Integer ); virtual;
        function IndexOf( Item: TCustomItem ): Integer; virtual;
        procedure Clear(); virtual;

        procedure Insert( Index: Integer; Item: TCustomItem );

        property Items[ Index: Integer ]: TCustomItem read GetItems; default;
        property Count: Integer read GetCount write SetCount;
    end;

    { TCustomObjectList }

    generic TCustomObjectList <TCustomItem> = class ( TPersistent )
      private type
        TListEnumerator = specialize TCustomListEnumerator < TCustomItem >;

      private
        FItems: TList;

        function MoveNext( var Index: Integer ): Boolean;
        function GetCount: Integer;
        procedure SetCount( AValue: Integer );
        function GetItems( Index: Integer ): TCustomItem;
        function GetEnumerator: TListEnumerator;

      public
        constructor Create;
        destructor Destroy; override;

        function Add( Item: TCustomItem ): Integer; virtual;
        procedure Delete( Index: Integer ); virtual;
        function IndexOf( Item: TCustomItem ): Integer; virtual;
        procedure Clear( const FreeContent: Boolean = False ); virtual;

        procedure Insert( Index: Integer; Item: TCustomItem );

        property Items[ Index: Integer ]: TCustomItem read GetItems; default;
        property Count: Integer read GetCount write SetCount;
    end;

implementation

{ TCustomObjectList }

function TCustomObjectList.MoveNext(var Index: Integer): Boolean;
begin
  Result:= Index + 1 < Count;
  if ( Result ) then
    Inc(Index);
end;

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

constructor TCustomObjectList.Create;
begin
  inherited;
  FItems:= TList.Create;
end;

destructor TCustomObjectList.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TCustomObjectList.Add( Item: TCustomItem ): Integer;
begin
  Result:= FItems.Add( Item );
end;

procedure TCustomObjectList.Insert( Index: Integer; Item: TCustomItem );
begin
  FItems.Insert( Index, Item );
end;

procedure TCustomObjectList.Delete( Index: Integer );
begin
  FItems.Delete( Index );
end;

function TCustomObjectList.IndexOf( Item: TCustomItem ): Integer;
begin
  Result:= Fitems.IndexOf( Item );
end;

function TCustomObjectList.GetEnumerator(): TListEnumerator;
begin
  Result:= TListEnumerator.Init( -1, @GetItems);
end;


procedure TCustomObjectList.Clear(const FreeContent: Boolean);
var
  Itm: TCustomItem;
begin
  for Itm in Self do
    Itm.Free;
  inherited Clear;
end;


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


function TCustomList.MoveNext(var Index: Integer): Boolean;
begin
  Result:= Index + 1 < Count;
  if ( Result ) then
    Inc(Index);
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
begin
  FItems.Clear;
end;

procedure TCustomList.Empty;
begin
  FItems.Clear;
end;

function TCustomList.GetEnumerator(): TListEnumerator;
begin
  Result:= TListEnumerator.Init( -1, @GetItems);
end;


{ TCustomListEnumerator }

function TCustomListEnumerator.GetCurrent: TCustomItem;
begin
  Result:= FGetCurrent( CurrentIdx );
end;

function TCustomListEnumerator.MoveNext: Boolean;
begin
  Result:= FMoveNext( FCurrentIdx );
end;

procedure TCustomListEnumerator.Init( const StartIdx: Integer;
  const AMoveNext: TMoveNext; const AGetCurrent: TGetCurrent );
begin
  Assert( Assigned( AMoveNext ));
  Assert( Assigned( AGetCurrent ));
  FCurrentIdx:= StartIdx;
  FMoveNext:= AMoveNext;
  FGetCurrent:= AGetCurrent;
end;

end.

