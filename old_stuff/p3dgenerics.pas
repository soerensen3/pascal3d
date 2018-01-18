unit p3dgenerics;

{$mode objfpc}{$H+}

interface
  uses
    Classes, sysutils;

  type
    { TCustomObjectList }

    generic TCustomObjectList <TCustomItem> = class ( TPersistent )
      private type
        { TCustomListEnumerator }

         TCustomListEnumerator = class
           private type
             TGetCurrent = function( Index: Integer ): TCustomItem of object;
             TMoveNext = function( var Index: Integer ): Boolean of object;

           private
             FCurrentIdx: Integer;
             FGetCurrent: TGetCurrent;
             FMoveNext: TMoveNext;
             function GetCurrent: TCustomItem;

           public
             constructor Create( const StartIdx: Integer; const AMoveNext: TMoveNext; const AGetCurrent: TGetCurrent);

             function MoveNext: Boolean;
             property Current: TCustomItem read GetCurrent;
             property CurrentIdx: Integer read FCurrentIdx;
         end;

      private
        FItems: TList;

        function MoveNext( var Index: Integer ): Boolean;
        function GetCount: Integer;
        procedure SetCount( AValue: Integer );
        function GetItems( Index: Integer ): TCustomItem;

      public
        constructor Create;
        destructor Destroy; override;

        function GetEnumerator(): TCustomListEnumerator;
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

function TCustomObjectList.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure TCustomObjectList.SetCount( AValue: Integer );
begin
  FItems.Count:= AValue;
end;

function TCustomObjectList.GetItems( Index: Integer ): TCustomItem;
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

function TCustomObjectList.GetEnumerator(): TCustomListEnumerator;
begin
  Result:= TCustomListEnumerator.Create( -1, @MoveNext, @GetItems );
end;


procedure TCustomObjectList.Clear(const FreeContent: Boolean);
var
  Itm: TCustomItem;
begin
  for Itm in Self do
    Itm.Free;
end;

{ TCustomListEnumerator }

function TCustomObjectList.TCustomListEnumerator.GetCurrent: TCustomItem;
begin
  Result:= FGetCurrent( CurrentIdx );
end;

function TCustomObjectList.TCustomListEnumerator.MoveNext: Boolean;
begin
  Result:= FMoveNext( FCurrentIdx );
end;

constructor TCustomObjectList.TCustomListEnumerator.Create(
  const StartIdx: Integer; const AMoveNext: TMoveNext;
  const AGetCurrent: TGetCurrent);
begin
  Assert( Assigned( AMoveNext ));
  Assert( Assigned( AGetCurrent ));
  FCurrentIdx:= StartIdx;
  FMoveNext:= AMoveNext;
  FGetCurrent:= AGetCurrent;
end;

end.

