//p3dgenerics.inc
//part of p3dutils.pas

{$IFDEF INTERFACE}

  type
    TP3DListOnChangeAction = ( actAdd, actDelete, actExtract, actClear );

    generic gP3DListEnumerator < T > = class
      private type
        TMoveNext = function ( var AIndex: Integer; out AItem: T ): Boolean of object;
      private
        FCurrent: T;
        FCurrentIdx: Integer;
        FMoveNext: TMoveNext;

      public
        constructor Create( AStartIndex: Integer; AMoveNext: TMoveNext );
        function MoveNext: Boolean;
        property Current: T read FCurrent;
        property CurrentIdx: Integer read FCurrentIdx;
    end;

    { gP3DCustomList }

    generic gP3DCustomList < T > = class( TObject )
      public type
        TEnumerator = specialize gP3DListEnumerator < T >;
        TListNotifyEvent = procedure ( ASender: TObject; AItem: T; Action: TP3DListOnChangeAction ) of object;

      protected
        FItems: TList;
        FOnChange: TListNotifyEvent;
        FLastUpdate: Integer;

        function GetCount: Integer; virtual;
        procedure SetCount(AValue: Integer); virtual;
        function GetItems( Index: Integer ): T;
        procedure SetItems( Index: Integer ; AValue: T);

        function MoveNext( var AIndex: Integer; out AItem: T ): Boolean;
        procedure Notify( ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction ); virtual;
        procedure DoUpdate;

      public
        constructor Create;
        destructor Destroy; override;

        function GetEnumerator: TEnumerator;

        function Add( Item: T ): Integer; virtual;
        procedure Remove( AItem: T ); virtual;
        procedure Delete( Index: Integer ); virtual;
        procedure Clear; virtual;
        procedure Insert( Index: Integer; Item: T ); virtual;

        function IndexOf(AItem: Pointer): Integer; virtual;

        property Items[ Index: Integer ]: T read GetItems write SetItems; default;
        property OnChange: TListNotifyEvent read FOnChange write FOnChange;
        property Count: Integer read GetCount write SetCount;
        property LastUpdate: Integer read FLastUpdate;
    end;

    { gP3DCustomObjectList }
    generic gP3DCustomObjectList < T > = class( TObject )
      public type
        TEnumerator = specialize gP3DListEnumerator < T >;
        TListNotifyEvent = procedure ( ASender: TObject; AItem: T; Action: TP3DListOnChangeAction ) of object;

      protected
        FItems: TObjectList;
        FOnChange: TListNotifyEvent;
        FLastUpdate: Integer;

        function GetOwnsObjects: Boolean;
        procedure SetOwnsObjects(AValue: Boolean);
        function GetCount: Integer; virtual;
        procedure SetCount(AValue: Integer); virtual;
        function GetItems( Index: Integer ): T;
        procedure SetItems( Index: Integer ; AValue: T);

        function MoveNext( var AIndex: Integer; out AItem: T ): Boolean;
        procedure Notify( ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction ); virtual;
        procedure DoUpdate;

      public
        constructor Create;
        destructor Destroy; override;

        function GetEnumerator: TEnumerator;

        function Add( Item: T ): Integer; virtual;
        procedure Remove( AItem: T ); virtual;
        function Extract( AItem: T ): T; virtual;
        procedure Delete( Index: Integer ); virtual;
        procedure Clear; virtual;
        procedure Insert( Index: Integer; Item: T ); virtual;

        function IndexOf( AItem: T ): Integer; virtual;

        property Items[ Index: Integer ]: T read GetItems write SetItems; default;
        property OnChange: TListNotifyEvent read FOnChange write FOnChange;
        property Count: Integer read GetCount write SetCount;
        property LastUpdate: Integer read FLastUpdate;
        property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
    end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

constructor gP3DListEnumerator.Create( AStartIndex: Integer; AMoveNext: TMoveNext );
begin
  inherited Create;
  FillByte( FCurrent, SizeOf( FCurrent ), 0 );
  FCurrentIdx:= AStartIndex;
  FMoveNext:= AMoveNext;
end;

function gP3DListEnumerator.MoveNext: Boolean;
begin
  Result:= FMoveNext( FCurrentIdx, FCurrent );
end;


{ gP3DCustomList }

function gP3DCustomList.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure gP3DCustomList.SetCount(AValue: Integer);
begin
  FItems.Count:= AValue;
end;

function gP3DCustomList.GetItems(Index: Integer): T;
begin
  Result:= T( FItems[ Index ]);
end;

procedure gP3DCustomList.SetItems(Index: Integer; AValue: T);
var
  Item: T;
begin
  Item:= Items[ Index ];
  Notify( Self, Item, actDelete );
  FItems[ Index ]:= AValue;
  Notify( Self, AValue, actAdd );
end;

function gP3DCustomList.MoveNext(var AIndex: Integer; out AItem: T): Boolean;
begin
  Inc( AIndex );
  Result:= AIndex < Count;
  if ( Result ) then
    AItem:= Items[ AIndex ]
  else
    AItem:= default( T );
end;

procedure gP3DCustomList.Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction);
begin
  if ( Assigned( OnChange )) then
    OnChange( ASender, AItem, AAction );
  DoUpdate;
end;

procedure gP3DCustomList.DoUpdate;
begin
  FLastUpdate:= GetTickCount64;
end;

constructor gP3DCustomList.Create;
begin
  inherited Create;
  FItems:= TList.Create;
end;

destructor gP3DCustomList.Destroy;
begin
  FreeAndNil( FItems );
  inherited Destroy;
end;

function gP3DCustomList.GetEnumerator: TEnumerator;
begin
  Result:= TEnumerator.Create( -1, @MoveNext );
end;

function gP3DCustomList.Add(Item: T): Integer;
begin
  Result:= FItems.Add( Item );
  Notify( Self, Item, actAdd );
end;

procedure gP3DCustomList.Remove(AItem: T);
begin
  Notify( Self, AItem, actDelete );
  FItems.Remove( AItem );
end;

procedure gP3DCustomList.Delete(Index: Integer);
begin
  Notify( Self, Items[ Index ], actDelete );
  FItems.Delete( Index );
end;

procedure gP3DCustomList.Clear;
begin
  Notify( Self, Default( T ), actClear );
  FItems.Clear;
end;

procedure gP3DCustomList.Insert(Index: Integer; Item: T);
begin
  FItems.Insert( Index, Item );
  Notify( Self, Item, actAdd );
end;

function gP3DCustomList.IndexOf(AItem: Pointer): Integer;
begin
  if ( Assigned( AItem )) then
    Result:= FItems.IndexOf( T( AItem^ ))
  else
    Result:= FItems.IndexOf( nil );
end;

{ gP3DCustomObjectList }

function gP3DCustomObjectList.GetOwnsObjects: Boolean;
begin
  Result:= FItems.OwnsObjects;
end;

procedure gP3DCustomObjectList.SetOwnsObjects(AValue: Boolean);
begin
  FItems.OwnsObjects:= AValue;
end;

function gP3DCustomObjectList.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure gP3DCustomObjectList.SetCount(AValue: Integer);
begin
  FItems.Count:= AValue;
end;

function gP3DCustomObjectList.GetItems(Index: Integer): T;
begin
  Result:= T( FItems[ Index ]);
end;

procedure gP3DCustomObjectList.SetItems(Index: Integer; AValue: T);
var
  Item: T;
begin
  Item:= Items[ Index ];
  Notify( Self, Item, actDelete );
  FItems[ Index ]:= TObject( AValue );
  Notify( Self, AValue, actAdd );
end;

function gP3DCustomObjectList.MoveNext(var AIndex: Integer; out AItem: T): Boolean;
begin
  Inc( AIndex );
  Result:= AIndex < Count;
  if ( Result ) then
    AItem:= Items[ AIndex ]
  else
    AItem:= default( T );
end;

procedure gP3DCustomObjectList.Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction);
begin
  if ( Assigned( OnChange )) then
    OnChange( ASender, AItem, AAction );
  DoUpdate;
end;

procedure gP3DCustomObjectList.DoUpdate;
begin
  FLastUpdate:= GetTickCount64;
end;

constructor gP3DCustomObjectList.Create;
begin
  inherited Create;
  FItems:= TObjectList.Create;
end;

destructor gP3DCustomObjectList.Destroy;
begin
  FreeAndNil( FItems );
  inherited Destroy;
end;

function gP3DCustomObjectList.GetEnumerator: TEnumerator;
begin
  Result:= TEnumerator.Create( -1, @MoveNext );
end;

function gP3DCustomObjectList.Add(Item: T): Integer;
begin
  Result:= FItems.Add( TObject( Item ));
  Notify( Self, Item, actAdd );
end;

procedure gP3DCustomObjectList.Remove(AItem: T);
begin
  Notify( Self, AItem, actDelete );
  FItems.Remove( TObject( AItem ));
end;

function gP3DCustomObjectList.Extract(AItem: T): T;
begin
  Notify( Self, AItem, actExtract );
  Result:= T( FItems.Extract( AItem ));
end;

procedure gP3DCustomObjectList.Delete(Index: Integer);
begin
  Notify( Self, Items[ Index ], actDelete );
  FItems.Delete( Index );
end;

procedure gP3DCustomObjectList.Clear;
begin
  Notify( Self, Default( T ), actClear );
  FItems.Clear;
end;

procedure gP3DCustomObjectList.Insert(Index: Integer; Item: T);
begin
  FItems.Insert( Index, TObject( Item ));
  Notify( Self, Item, actAdd );
end;

function gP3DCustomObjectList.IndexOf(AItem: T): Integer;
begin
  Result:= FItems.IndexOf( AItem );
end;


{$ENDIF}

