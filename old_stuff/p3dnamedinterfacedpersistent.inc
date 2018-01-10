//p3dobject.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

  type
    TP3DNamedInterfacedPersistent = class;

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

    { IP3DNamedObject }

    IP3DNamedObject = interface ( IP3DBaseInterface )
      ['{713A3737-F83B-4AE6-8C04-E3A88FE26FC5}']
      function GetName: TComponentName;
      procedure SetName( NewName: TComponentName);

      property Name: TComponentName read GetName write SetName;
    end;

    { IP3DDataPath }

    IP3DDataPath = interface ( IP3DBaseInterface )
      ['{51D5A346-4073-4399-8289-1CF26BA88424}']
      function GetDataPath: String;
    end;

    { TP3DNamedInterfacedPersistent }

    TP3DNamedInterfacedPersistent = class( TP3DInterfacedPersistent, IP3DNamedObject )
      private
        FName: TComponentName;
        FParentList: TP3DNamedInterfacedPersistentList;

        function GetName: TComponentName;
        procedure SetName( NewName: TComponentName ); virtual;
        procedure SetParentList(AValue: TP3DNamedInterfacedPersistentList);

      public
        constructor Create( const AParentList: TP3DNamedInterfacedPersistentList = nil );
        constructor CreateFromJSONContext(AContext: TP3DJSONContext); override;

        destructor Destroy; override;

        property ParentList: TP3DNamedInterfacedPersistentList read FParentList write SetParentList;

      published
        property Name: TComponentName read FName write SetName stored False;
    end;

    { gP3DPropertyAccessNamedInterfacedPersistent }

    generic gP3DPropertyAccessNamedInterfacedPersistent < TItem: TP3DNamedInterfacedPersistent > = class ( specialize gP3DPropertyAccessInterfacedPersistent < TItem >)
      procedure SetAsString( AValue: String ); override;
      function GetAsString: String; override;
      function Serialize: String; override;
      function SerializeFull: String;
      procedure CreateValueFromJSONContext(AClass: TP3DInterfacedPersistentType; AContext: TP3DJSONContext); override;
    end;

    { gP3DListPropertyAccessNamedInterfacedPersistent }

    generic gP3DListPropertyAccessNamedInterfacedPersistent < TListProp, TListData, TPropType > = class( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
      public
        procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
        procedure LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext); override;
      end;

    { gP3DListPropertyAccessNamedInterfacedPersistentContainer }

    generic gP3DListPropertyAccessNamedInterfacedPersistentContainer < TListProp, TListData, TPropType > = class( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
      public
        procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
        procedure LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext); override;
        function Serialize: String; override;
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
  if ( Assigned( ParentList )) then
    begin
      if ( ParentList.FindByName( NewName ) < 0 ) then
        FName:= NewName
      else
        raise Exception.CreateFmt( ' Error in TP3DObject.SetName(''%s''). An object with that name already exists!', [ NewName ]);
    end
  else
    FName:= NewName;
end;

procedure TP3DNamedInterfacedPersistent.SetParentList(AValue: TP3DNamedInterfacedPersistentList);
begin
  if FParentList=AValue then Exit;

  if ( Assigned( ParentList )) then
    ParentList.Remove( Self );
  if ( Assigned( AValue )) then
    AValue.Add( Self );
  FParentList:=AValue;
end;

constructor TP3DNamedInterfacedPersistent.Create(const AParentList: TP3DNamedInterfacedPersistentList);
var
  BaseName: String;
begin
  inherited Create;
  {if ( not Assigned( AParentList )) then
    raise Exception.Create( 'Error while creating NamedInterfacedPersistent: ParentList is not assigned!' )
    //FParentList:= P3DData
  else
    FParentList:= AParentList;}

  if ( ClassName[ 1 ] = 'T' ) then
    BaseName:= Copy( ClassName, 2, Length( ClassName ) - 1 )
  else
    BaseName:= ClassName;

  if ( Assigned( AParentList )) then
    begin
      FName:= AParentList.FindUniqueName( BaseName );
      ParentList:= AParentList;
    end;

  Properties.Add( TP3DPropertyAccessString.CreateFieldSet( 'Name', @FName, @SetName, smAttribute ));
end;

constructor TP3DNamedInterfacedPersistent.CreateFromJSONContext(AContext: TP3DJSONContext);
begin
  Create( nil );
  LoadFromJSONContext( AContext );
end;

destructor TP3DNamedInterfacedPersistent.Destroy;
begin
  ParentList:= nil;
  inherited Destroy;
end;

{ gP3DListPropertyAccessNamedInterfacedPersistent }

procedure gP3DListPropertyAccessNamedInterfacedPersistent.LoadFromJSONContext(AContext: TP3DJSONContext);
var
  i: Integer;
begin
  if ( AContext.Data.JSONType = jtArray ) then
    begin
      Value.Count:= AContext.Data.Count;
      for i:= 0 to AContext.Data.Count - 1 do
        LoadItemFromJSONContext( i, P3DJSONContext( AContext.JSONLoader, AContext.Data.Items[ i ], Value, AContext.Parent ))
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtArray ), JSONTypeName( AContext.Data.JSONType )]);
end;

procedure gP3DListPropertyAccessNamedInterfacedPersistent.LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext);
var
  _Prop: TP3DPropertyAccess;
  Prop: TPropType;
  Val: TListData;
  ItemName: String;
  SPath: TJSONStringType;
  Path: TJSONData;
begin
  if ( AContext.Data.JSONType = jtString ) then
    begin
      SPath:= AContext.Data.AsString;
      _Prop:= GetArrayAsProp( AIndex );
      if ( Assigned( _Prop )) then
        _Prop.AsString:= SPath;
      {if (( SPath = '' ) or ( SPath = 'None' )) then
        begin
          Value:= nil;
          exit;
        end;
      WriteLn( 'Load ' + TItem.ClassName + ' from path: ', SPath );

      _Prop:= AContext.JSONLoader.Root.Properties.GetPropByPath( SPath ); // No need to check if already loaded, this is done in appendfrompath
      if ( _Prop is IP3DPropertyAccessInterfacedPersistent ) then
        begin
          Val:= ( _Prop as IP3DPropertyAccessInterfacedPersistent ).GetObjectValue;
          if ( Val is TItem ) then
            Value:= TItem( Val )
          else
            raise Exception.CreateFmt( 'Cannot set datablock by string. The assigned value "%s" is of the wrong type.', [ Prop.Name ]);
        end
      else
        begin
          Path:= AContext.JSONLoader.FindPath( SPath );
          if ( Assigned( Path )) then
            LoadItemFromJSONContext( AIndex, P3DJSONContext( AContext.JSONLoader, Path, AContext.Owner, AContext.Parent ))
          else
            raise Exception.CreateFmt( 'Cannot load %s from path "%s" because the path is invalid!', [ TItem.ClassName, AContext.Data.AsString ]);
        end;}
    end
{  else if ( AContext.Data.JSONType = jtObject ) then
    begin
      ItemName:= TJSONObject( AContext.Data ).Strings[ 'Name' ];
      if ( Value.FindByName( ItemName ) > -1 ) then
        exit;
      Prop:= TPropType.Create( 'ItemName', smDoNotStore );
      Prop.Field:= @Val;
      Prop.LoadFromJSONContext( AContext );
      Prop.Free;
    end}
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtObject ), JSONTypeName( AContext.Data.JSONType )]);
end;

{ gP3DListPropertyAccessNamedInterfacedPersistentContainer }

procedure gP3DListPropertyAccessNamedInterfacedPersistentContainer.LoadFromJSONContext(AContext: TP3DJSONContext);
var
  i: Integer;
  S: TJSONStringType;
begin
  if ( AContext.Data.JSONType = jtArray ) then
    begin
      for i:= 0 to AContext.Data.Count - 1 do
        LoadItemFromJSONContext( i, P3DJSONContext( AContext.JSONLoader, AContext.Data.Items[ i ], AContext.Owner, AContext.Data ));
    end
  else if ( AContext.Data.JSONType = jtString ) then
    begin
      S:= AContext.Data.AsString;
      if ( S[ 1 ] = '@' ) then
        AContext.JSONLoader.BinaryFile.Stream.Position:= StrToInt( Copy( S, 2, Length( S ) - 1 ))
      else
        exit;
      LoadFromStream( AContext.JSONLoader.BinaryFile.Stream )
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtArray ), JSONTypeName( AContext.Data.JSONType )]);
end;

procedure gP3DListPropertyAccessNamedInterfacedPersistentContainer.LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext);
var
  Prop: TPropType;
  Val: TListData;
  ItemName: String;
begin
  if ( AContext.Data.JSONType = jtObject ) then
    begin
      ItemName:= TJSONObject( AContext.Data ).Strings[ 'Name' ];
      if ( Value.FindByName( ItemName ) > -1 ) then
        exit;
      Prop:= TPropType.Create( ItemName, smDoNotStore );
      Prop.Context:= Value;
      Prop.Field:= @Val;
      Prop.LoadFromJSONContext( AContext );
      Prop.Free;
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtObject ), JSONTypeName( AContext.Data.JSONType )]);
end;

function gP3DListPropertyAccessNamedInterfacedPersistentContainer.Serialize: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to ArrayCount - 1 do
    Result+= ', ' + TPropType( GetArrayAsProp( i )).SerializeFull;
  Result:= '[' + Copy( Result, 3, Length( Result ) - 2 ) + ']';
end;

{ gP3DPropertyAccessNamedInterfacedPersistent }

procedure gP3DPropertyAccessNamedInterfacedPersistent.SetAsString(AValue: String);
var
  n: Integer;
  Owner: TP3DLibrary;
  Path: TP3DXMLPath;
  FN: String;
  Prop: TP3DPropertyAccess;
  Val: TP3DInterfacedPersistent;
begin
  {if (( Context is TP3DDataBlock ) and ( AValue > '' )) then
    if (( AValue = '<None>' ) or ( AValue = '<None:' + TItem.ClassName + '>' )) then
      Value:= nil
    else
      begin
        Owner:= Context.Owner.GetOwnerFromPath( AValue, True );
        if ( not Assigned( Owner )) then
          raise Exception.Create( 'Could not find resource: ' + AValue );
        Enclosed( AValue, '"', '"' );
        Path:= P3DXMLExtractRelativePath( AValue, FN );
        Prop:= Owner.Properties.GetPropByPath( Path{, TItem} ); // No need to check if already loaded, this is done in appendfrompath
        if ( Prop is IP3DPropertyAccessInterfacedPersistent ) then
          begin
            Val:= ( Prop as IP3DPropertyAccessInterfacedPersistent ).GetObjectValue;
            if ( Val is TItem ) then
              Value:= TItem( Val )
            else
              raise Exception.CreateFmt( 'Cannot set datablock by string. The assigned value "%s" is of the wrong type.', [ Prop.Name ]);
          end
        else
          raise Exception.CreateFmt( 'Cannot set datablock by string. The assigned value "%s" is of the wrong type.', [ Prop.Name ]);
      end;}
end;

function gP3DPropertyAccessNamedInterfacedPersistent.GetAsString: String;
begin
  Result:=inherited GetAsString;
end;

function gP3DPropertyAccessNamedInterfacedPersistent.Serialize: String;
begin
  if ( Context is IP3DDataPath ) then
    Result:= ( Context as IP3DDataPath ).GetDataPath + '.' + Name
  else
    Result:= inherited Serialize;
end;

function gP3DPropertyAccessNamedInterfacedPersistent.SerializeFull: String;
begin
  Result:= inherited Serialize;
end;

procedure gP3DPropertyAccessNamedInterfacedPersistent.CreateValueFromJSONContext(AClass: TP3DInterfacedPersistentType; AContext: TP3DJSONContext);
begin
  Value:= TItem.CreateFromJSONContext( AContext );
  if ( Context is TP3DNamedInterfacedPersistentList ) then
    Value.ParentList:= TP3DNamedInterfacedPersistentList( Context );
end;

{$ENDIF}
