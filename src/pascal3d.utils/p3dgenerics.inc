//p3dgenerics.inc
//part of p3dutils.pas

{$IFDEF INTERFACE}

  type
    TP3DListOnChangeAction = ( actAdd, actDelete, actClear );
    { gP3DCustomList }

    generic gP3DCustomList < T > = class( TObject )
      public type
        TList = specialize TFPGList < T >;
        TEnumerator = specialize TFPGListEnumerator < T >;
        TListNotifyEvent = procedure ( ASender: TObject; AItem: T; Action: TP3DListOnChangeAction ) of object;

      protected
        FItems: TList;
        FOnChange: TListNotifyEvent;
        FLastUpdate: Integer;

        function GetCount: Integer; virtual;
        procedure SetCount(AValue: Integer); virtual;
        function GetItems( Index: Integer ): T;
        procedure SetItems( Index: Integer ; AValue: T);

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

    generic gP3DCustomObjectList < T > = class ( specialize gP3DCustomList < T > )
      protected
        FOwnsObjects: Boolean;

        procedure Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction); override;
        procedure FreeObjects( AFrom, ATo : Integer );

      public
        constructor Create;

        property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

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
  Result:= FItems[ Index ];
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
  Result:= FItems.GetEnumerator;
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

procedure gP3DCustomObjectList.Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction);
begin
  inherited Notify(ASender, AItem, AAction);
  if ( not Assigned( AItem )) then
    exit;
  if (( AAction = actDelete ) and OwnsObjects ) then
    FreeObjects( IndexOf( AItem ), IndexOf( AItem ))
  else if (( AAction = actClear ) and OwnsObjects ) then
    FreeObjects( 0, Count - 1 );
end;

procedure gP3DCustomObjectList.FreeObjects(AFrom, ATo: Integer);
var
  i: Integer;
begin
  for i:= ATo downto AFrom do
    Items[ i ].Free;
end;

constructor gP3DCustomObjectList.Create;
begin
  inherited Create;
  FOwnsObjects:= True;
end;


{$ENDIF}

