unit p3dgenerics;

{$mode objfpc}{$H+}

interface
  uses
    Classes, sysutils;

  type
     { gP3DCustomListEnumerator }
     generic gP3DCustomListEnumerator < TCustomItem > = class
       private type
         TMoveNext = function ( var Index: Integer ): TCustomItem of object;

       private
         FCurrent: TCustomItem;
         FCurrentIdx: Integer;
         FMoveNext: TMoveNext;

       public
         constructor Create( AMoveNext: TMoveNext );
         function MoveNext: Boolean;
         property Current: TCustomItem read FCurrent;
         property CurrentIdx: Integer read FCurrentIdx;
     end;

    { gP3DCustomObjectList }

    generic gP3DCustomObjectList <TCustomItem> = class ( TPersistent )
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
        function GetEnumerator(): TP3DCustomObjectListEnumerator;
        procedure Insert( Index: Integer; Item: TCustomItem );

        property Items[ Index: Integer ]: TCustomItem read GetItems; default;
        property Count: Integer read GetCount write SetCount;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

implementation

{ TCustomList }
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
  Result:= TCustomItem( FItems[ Index ]);
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

function gP3DCustomObjectList.GetEnumerator(): TP3DCustomObjectListEnumerator;
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

constructor gP3DCustomListEnumerator.Create(
  AMoveNext: TMoveNext);
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


end.

