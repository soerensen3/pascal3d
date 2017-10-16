//p3dobject.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

  type
    TP3DNamedInterfacedPersistent = class;

    { gP3DNamedInterfacedPersistentList }

    {generic gP3DNamedInterfacedPersistentList < TListItem > = class( specialize gP3DCustomObjectList < TP3DNamedInterfacedPersistent >, IP3DNamedInterfacedPersistentList )
      private
        FOwner: TP3DInterfacedPersistent;

        function GetOwner: TP3DInterfacedPersistent;

      public
        constructor Create( AOwner: TP3DInterfacedPersistent );
        function FindUniqueName( BaseStr: String ): String;
        function FindByName( AName: String ): Integer;
        procedure Clear(const FreeObjects: Boolean = True ); override;
        function DumpObjectList: String;

      published
        property Owner: TP3DInterfacedPersistent read GetOwner;
    end;

    TP3DNamedInterfacedPersistentList = specialize gP3DNamedInterfacedPersistentList < TP3DNamedInterfacedPersistent >;
    }

    { TP3DNamedInterfacedPersistentList }

    TP3DNamedInterfacedPersistentList = class( specialize gP3DCustomObjectList < TP3DNamedInterfacedPersistent >)
      private
        FOwner: TP3DInterfacedPersistent;

        function FindInsertPos(AName: String): Integer;

      public
        constructor Create( AOwner: TP3DInterfacedPersistent );

        function FindUniqueName( BaseStr: String ): String;
        function FindByName( AName: String ): Integer;
        procedure Clear(const FreeObjects: Boolean = False ); override;
        function DumpObjectList: String;
	function Add( Item: TP3DNamedInterfacedPersistent ): Integer; override;
	procedure UpdateItemKey( Index: Integer );

      published
        property Owner: TP3DInterfacedPersistent read FOwner;
    end;

    generic gP3DNamedInterfacedPersistentList < TListItem > = class( TP3DNamedInterfacedPersistentList )
      private type
        TP3DNamedInterfacedPersistentListOnChangeEvent = procedure ( Sender: TP3DNamedInterfacedPersistentList; Item: TListItem; Action: TP3DCustomListOnChangeAction ) of object;
        TP3DNamedInterfacedPersistentListEnumeratorClass = specialize gP3DCustomListEnumeratorClass < TListItem >;

      private
        function GetNamedItems( Index: Integer ): TListItem;
        function GetOnChange: TP3DNamedInterfacedPersistentListOnChangeEvent;
        function GetValue( Name: String ): TListItem;
        procedure SetNamedItems( Index: Integer ; AValue: TListItem);
        procedure SetOnChanged(AValue: TP3DNamedInterfacedPersistentListOnChangeEvent);
        function MoveNext( var Index: Integer ): TListItem;
        function MoveBack(var Index: Integer): TListItem;

      public
        function Add( Item: TListItem ): Integer; reintroduce;
	procedure Remove( Item: TListItem ); reintroduce;
        function GetEnumerator: TP3DNamedInterfacedPersistentListEnumeratorClass;

	property Values[ Name: String ]: TListItem read GetValue;
        property NamedItems[ Index: Integer ]: TListItem read GetNamedItems write SetNamedItems; default;
        property OnChange: TP3DNamedInterfacedPersistentListOnChangeEvent read GetOnChange write SetOnChanged;
    end;
    { IP3DObject }

    IP3DNamedObject = interface
      ['{713A3737-F83B-4AE6-8C04-E3A88FE26FC5}']
      function GetName: TComponentName;
      procedure SetName( NewName: TComponentName);

      property Name: TComponentName read GetName write SetName;
    end;

    { TP3DNamedInterfacedPersistent }

    TP3DNamedInterfacedPersistent = class( TP3DInterfacedPersistent, IP3DNamedObject )
      private
        FName: TComponentName;
        FParentList: TP3DNamedInterfacedPersistentList;

        function GetName: TComponentName;
        procedure SetName( NewName: TComponentName ); virtual;

      public
        constructor Create( const AParentList: TP3DNamedInterfacedPersistentList = nil );
        constructor CreateFromJSONContext(AContext: TP3DJSONContext); override;

        destructor Destroy; override;

        property ParentList: TP3DNamedInterfacedPersistentList read FParentList;

      published
        property Name: TComponentName read FName write SetName stored False;
    end;

    { gP3DListPropertyAccessInterfacedPersistent }

    generic gP3DListPropertyAccessInterfacedPersistent < TListProp, TListData, TPropType > = class ( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
      public
        procedure SetArrayCount(AValue: Integer); override;
        procedure ValueCreateNew( AContext: TP3DJSONContext ); virtual;
        procedure LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext); override;
    end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DObjectList }

constructor TP3DNamedInterfacedPersistentList.Create(AOwner: TP3DInterfacedPersistent);
begin
  inherited Create();
  FOwner:= AOwner;
end;

function TP3DNamedInterfacedPersistentList.FindUniqueName( BaseStr: String ): String;
var
  I: Integer;
begin
  I:= 0;
  repeat
    Inc( I );
    Result:= BaseStr + IntToStr( I );
  until ( FindByName( Result ) = -1 );
end;

function TP3DNamedInterfacedPersistentList.FindByName(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = AName ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DNamedInterfacedPersistentList.FindInsertPos(AName: String): Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 0 to Count - 1 do
    begin //WriteLn( Items[ i ].Name, '<', AName, ' = ', Items[ i ].Name < AName );
    if ( Items[ i ].Name < AName ) then
      Result:= i + 1
    else
      break;end;
end;

procedure TP3DNamedInterfacedPersistentList.Clear(const FreeObjects: Boolean);
var
  i: Integer;
begin
  if ( FreeObjects ) then
    for i:= Count - 1 downto 0 do
      Items[ i ].Free;
  Count:= 0;
end;

function TP3DNamedInterfacedPersistentList.DumpObjectList: String;
var
  i: Integer;
begin
  Result:= '';

  for i:= 0 to Count - 1 do
    if ( i > 0 ) then
      Result+= ', ' + Items[ i ].Name
    else
      Result+= Items[ i ].Name;
end;

function TP3DNamedInterfacedPersistentList.Add(Item: TP3DNamedInterfacedPersistent): Integer;
begin
  if ( not Assigned( Item )) then
    exit( -1 );
  Result:= FindByName( Item.Name );
  if ( Result = -1 ) then
    begin
      Result:= FindInsertPos( Item.Name );
      //WriteLn( 'Item.Name = ', Item.Name, ' at ', Result );
      FItems.Insert( Result, Item );
      if ( Assigned( OnChange )) then
        OnChange( Self, Item, actAdd );
      DoUpdate;
    end
  else if ( Item <> Items[ Result ]) then
    raise Exception.Create( 'Cannot add to NamedInterfacePersistentList. An item with the same name already exists: ' + Item.Name );
end;

procedure TP3DNamedInterfacedPersistentList.UpdateItemKey(Index: Integer);
var
  Item: TCustomItem;
  idx: Integer;
begin
  Item:= Items[ Index ];
  idx:= FindInsertPos( Item.Name );
  if ( idx <> Index ) then
    begin
      FItems.Delete( Index );
      FItems.Insert( idx, Item );
    end;
end;


function gP3DNamedInterfacedPersistentList.GetNamedItems( Index: Integer ): TListItem;
begin
  Result:= TListItem( FItems[ Index ]);
end;

function gP3DNamedInterfacedPersistentList.GetOnChange: TP3DNamedInterfacedPersistentListOnChangeEvent;
begin
  Result:= TP3DNamedInterfacedPersistentListOnChangeEvent( FOnChange );
end;

function gP3DNamedInterfacedPersistentList.GetValue( Name: String ): TListItem;
var
  n: Integer;
begin
  n:= FindByName( Name );
  if ( n > 0 ) then
    Result:= TListItem( FItems[ n ])
  else
    Result:= nil;
end;

procedure gP3DNamedInterfacedPersistentList.SetNamedItems( Index: Integer ; AValue: TListItem);
var
  OldVal: TListItem;
begin
  OldVal:= TListItem( FItems[ Index ]);
  if ( OldVal = AValue ) then
    exit;
  if ( Assigned( OnChange ) and Assigned( OldVal )) then
    OnChange( Self, OldVal, actDelete );
  FItems[ Index ]:= AValue;

  if ( Assigned( OnChange ) and Assigned( AValue )) then
    OnChange( Self, AValue, actAdd );
end;

procedure gP3DNamedInterfacedPersistentList.SetOnChanged(AValue: TP3DNamedInterfacedPersistentListOnChangeEvent);
begin
  FOnChange:= TP3DCustomListOnChangeEvent( AValue );
end;

function gP3DNamedInterfacedPersistentList.MoveNext( var Index: Integer ): TListItem;
begin
  Inc( Index );
  if ( Index < Count ) then
    Result:= NamedItems[ Index ]
  else
    Result:= nil;
end;

function gP3DNamedInterfacedPersistentList.MoveBack(var Index: Integer): TListItem;
begin
  Dec( Index );
  if ( Index > -1 ) then
    Result:= NamedItems[ Index ]
  else
    Result:= nil;
end;

function gP3DNamedInterfacedPersistentList.Add(Item: TListItem): Integer;
begin
  Result:= inherited Add( Item );
end;

procedure gP3DNamedInterfacedPersistentList.Remove(Item: TListItem);
begin
  FItems.Remove( Item );
end;

function gP3DNamedInterfacedPersistentList.GetEnumerator: TP3DNamedInterfacedPersistentListEnumeratorClass;
begin
  Result:= TP3DNamedInterfacedPersistentListEnumeratorClass.Create( -1, @MoveNext );
end;

{ TP3DObject }

function TP3DNamedInterfacedPersistent.GetName: TComponentName;
begin
  Result:= FName;
end;

procedure TP3DNamedInterfacedPersistent.SetName(NewName: TComponentName);
begin
  if ( Name = NewName ) then
    exit;
  if ( ParentList.FindByName( NewName ) < 0 ) then
    FName:= NewName
  else
    raise Exception.CreateFmt( ' Error in TP3DObject.SetName(''%s''). An object with that name already exists!', [ NewName ]);
end;

constructor TP3DNamedInterfacedPersistent.Create(const AParentList: TP3DNamedInterfacedPersistentList);
var
  BaseName: String;
begin
  inherited Create;
  if ( not Assigned( AParentList )) then
    raise Exception.Create( 'Error while creating NamedInterfacedPersistent: ParentList is not assigned!' )
    //FParentList:= P3DData
  else
    FParentList:= AParentList;

  if ( ClassName[ 1 ] = 'T' ) then
    BaseName:= Copy( ClassName, 2, Length( ClassName ) - 1 )
  else
    BaseName:= ClassName;

  FName:= ParentList.FindUniqueName( BaseName );
  ParentList.Add( Self );

  Properties.Add( TP3DPropertyAccessString.CreateFieldSet( 'Name', @FName, @SetName, smAttribute ));
end;

constructor TP3DNamedInterfacedPersistent.CreateFromJSONContext(AContext: TP3DJSONContext);
begin
  if ( AContext.Owner is TP3DNamedInterfacedPersistentList ) then
    Create( AContext.Owner as TP3DNamedInterfacedPersistentList )
  else
    raise Exception.Create( 'Cannot create TP3DNamedInterfacedPersistent from JSON context. Owner is empty or no TP3DNamedInterfacedPersistentList.' );
  LoadFromJSONContext( AContext );
end;

destructor TP3DNamedInterfacedPersistent.Destroy;
begin
  ParentList.Remove( Self );
  inherited Destroy;
end;

{$ENDIF}
