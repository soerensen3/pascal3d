//p3ddatablock.inc

{$IFDEF INTERFACE}

type
  TP3DRenderListFlag = ( rlfMaterials, rlfIndexColors, rlfWireFrame, rlfDepthTest, rlfLighting, rlfRenderEdges, rlfObjects, rlfMeshes, rlfScenes, rlfTileGrids, rlfJoints );
  TP3DRenderListFlags = set of TP3DRenderListFlag;

 const
   P3DRenderListFlagsDefault = [ rlfMaterials, rlfDepthTest, rlfLighting, rlfMeshes, rlfScenes, rlfTileGrids ];

type
  TP3DRenderList = class;
  //TP3DAction = class;

  { TP3DDataBlock

    introduces the following functionalities
    - It can be loaded from a link to a json path inside a json file.
    - It has a timestamp of the json file to check if it was already loaded
    - It offers a reload function to reload the block from a json or binary file.
    - It introduces reference counting (Users) if another datablock's property points to it.
    - Every datablock has a library as an owner.
    }
  TP3DDataBlock = class;
  TP3DDataBlockList = specialize gP3DNamedInterfacedPersistentList <TP3DDataBlock>;


  TP3DDataBlock = class abstract( TP3DNamedInterfacedPersistent )
    private
      //FAction: TP3DAction;
      FOwner: TP3DLibrary;
      FTimeStamp: Integer;
      FUsers: TP3DDataBlockList;

      function GetDataPath: String; virtual; abstract;
//      procedure SetAction(AValue: TP3DAction);
      procedure SetName(NewName: TComponentName); override;

    //protected
    public
      class function GetContainer( ALib: TP3DLibrary ): TP3DNamedInterfacedPersistentList; virtual; abstract;

    public
      constructor Create( const AData: TP3DLibrary = nil ); virtual;
      constructor CreateFromJSONContext( AContext: TP3DJSONContext ); override;

      destructor Destroy; override;
      procedure FastDestroy; virtual;

      procedure LoadFromFile( FileName: String );
      procedure LoadFromBinary( FileName: String ); virtual;
      procedure Reload;
      procedure RemoveFromFields( Block: TP3DDataBlock ); virtual;
      procedure RemoveUser( User: TP3DDataBlock );
      procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
      procedure AddUser( User: TP3DDataBlock );
      procedure ClearFields; virtual;
      procedure ClearUsers;

      property Owner: TP3DLibrary read FOwner;
      property TimeStamp: Integer read FTimeStamp;
//      property Action: TP3DAction read FAction write SetAction;

    published
      property DataPath: String read GetDataPath;
      property Users: TP3DDataBlockList read FUsers;
  end;



  TP3DDataBlockType = class of TP3DDataBlock;


  { gP3DCustomDatablockList }

  {generic gP3DCustomDatablockList <TCustomItem: TP3DDataBlock> = class ( TPersistent )
    type
      TP3DCustomObjectListEnumerator = specialize gP3DCustomListEnumeratorClass < TCustomItem >;
      TP3DCustomListOnChangeEvent = procedure ( Sender: {specialize gP3DCustomDatablockList < TCustomItem >}TPersistent; Item: TCustomItem; Action: TP3DCustomListOnChangeAction ) of object;

    private
      FFreeChildrenOnDestroy: Boolean;
      FItems: TList;
      FLastUpdate: Integer;
      FOnChange: TP3DCustomListOnChangeEvent;

      function GetCount: Integer;
      function GetValues( Index: String ): TCustomItem;
      procedure SetCount( AValue: Integer );
      function GetItems( Index: Integer ): TCustomItem;
      function MoveNext( var Index: Integer ): TCustomItem;
      function MoveBack( var Index: Integer ): TCustomItem;
      procedure SetItems( Index: Integer ; AValue: TCustomItem);
      procedure DoUpdate;
      //procedure Insert( Index: Integer; Item: TCustomItem );
      function FindInsertPos( AName: String ): Integer;

    public
      constructor Create;
      destructor Destroy; override;

      function Add( Item: TCustomItem ): Integer; virtual;
      procedure Delete( Index: Integer ); virtual;
      procedure Remove( Item: TCustomItem ); virtual;
      function IndexOf( Item: TCustomItem ): Integer; virtual;
      procedure Clear( const FreeObjects: Boolean = False ); virtual;
      function GetEnumerator(): TP3DCustomObjectListEnumerator;
      function Reversed(): TP3DCustomObjectListEnumerator;
      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
      function GetByName( AName: String ): TCustomItem;
      function UpdateItemIndex( Index: Integer ): Integer;
      procedure SaveToDOM(AParent: TDOMElement);
      procedure LoadFromStream( S: TStream ); //gP3DListPropertyAccessObject needs this to compile
      procedure SaveToStream( S: TStream );

      //TODO: Implement load from DOM

      property Items[ Index: Integer ]: TCustomItem read GetItems write SetItems; default;
      property Values[ Index: String ]: TCustomItem read GetValues;

    published
      property Count: Integer read GetCount write SetCount;
      property OnChange: TP3DCustomListOnChangeEvent read FOnChange write FOnChange;
      property FreeChildrenOnDestroy: Boolean read FFreeChildrenOnDestroy write FFreeChildrenOnDestroy;
      property LastUpdate: Integer read FLastUpdate;
  end;}

  { gP3DDatablockContainer }

  generic gP3DDatablockContainer < TListItem: TP3DDataBlock > = class ( specialize gP3DNamedInterfacedPersistentList < TListItem >)
    private
      function FindInsertPos(AName: String): Integer;

    public
      procedure Clear(const FreeObjects: Boolean=True); override;
      function UpdateItemIndex( Index: Integer ): Integer;
  end;

  { gP3DListPropertyAccessDictionary }

  { gP3DListPropertyAccessContainer }

  generic gP3DListPropertyAccessContainer < TListProp, TListData, TPropType > = class( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
     procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
     procedure LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext); override;
     function Serialize: String; override;
  end;

{  TP3DCustomDataBlockList = specialize gP3DNamedInterfacedPersistentList <TP3DDataBlock>;

  { TP3DDataBlockList }

  TP3DDataBlockList = class ( TP3DCustomDataBlockList )
    public
      function DumpUsers: String;

  end;}

  { gP3DPropertyAccessDataBlock }

  generic gP3DPropertyAccessDataBlock < TItem: TP3DDataBlock > = class( specialize gP3DPropertyAccessInterfacedPersistent < TItem >)
    protected
      function CheckSameContext: Boolean;
      function GetDefaultValue: TItem; override;

    public
      function GetChildPropByPath( APath: String ): TP3DPropertyAccess; override;
      procedure ValueCreateNew( ClTp: TP3DInterfacedPersistentType; AContext: TP3DJSONContext ); override;
      procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
      procedure CreateValueFromJSONContext( AClass: TP3DInterfacedPersistentType; AContext: TP3DJSONContext); override;
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      function Serialize: String; override;
      function SerializeFull: String;
  end;

  TP3DPropertyAccessDataBlock = specialize gP3DPropertyAccessDataBlock < TP3DDataBlock >;

  { TP3DPropertyAccessDataBlockList }

  { gP3DPropertyAccessDataBlockList }

  generic gP3DPropertyAccessDataBlockList < TListProp, TListData, TPropType > = class( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
    public
      procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
    end;

  TP3DPropertyAccessDataBlockList = specialize gP3DPropertyAccessDataBlockList < TP3DDataBlockList, TP3DDataBlock, TP3DPropertyAccessDataBlock >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}


{ TP3DPropertyAccessDataBlock }

function gP3DPropertyAccessDataBlock.GetAsString: String;
begin
  if ( Assigned( Value )) then
    Result:= '"' + Value.DataPath + '"'
  else
    Result:= '"None"';
  {if ( not Assigned( Value )) then
    begin
      Result:= '<None:' + TItem.ClassName + '>';
      exit;
    end;
  Result:= Value.DOMPath; //Value.Owner.Name + '.' + Value.Name;
  if ( Result = '' ) then
    begin
      WriteLn( 'Error: DOMPath is empty for Value ' + Value.Owner.Name + '.' + Value.Name );
      exit;
    end;
  if ( Result[ 1 ] <> '/' ) then
    Result:= '/' + Result;
  if ( not (( Context is TP3DDataBlock ) and ( TP3DDataBlock( Context ).Owner = Value.Owner ))) then
    Result:= Value.Owner.FileWatch.FileName + '$' + Result;}
  {if ( Assigned( Value )) then
    begin
      Prop:= Value.Properties.GetPropByName( 'Name' );
      if ( Assigned( Prop )) then
        Result:= '{ "' + Prop.AsString + ':' + Value.ClassName + '" : {' + Value.Properties.GetAsString() + '}}'
      else
        Result:= '{ "' + Value.ClassName + '" : {' + Value.Properties.ToStr() + '}}';
    end
  else
    Result:= Prop.AsString + ':' + Value.ClassName + ' = <None>';}
end;

procedure gP3DPropertyAccessDataBlock.SetAsString(AValue: String);
var
  n: Integer;
  Owner: TP3DLibrary;
  Path: TP3DXMLPath;
  FN: String;
  Prop: TP3DPropertyAccess;
  Val: TP3DInterfacedPersistent;
begin
  if (( Context is TP3DDataBlock ) and ( AValue > '' )) then
    if (( AValue = '<None>' ) or ( AValue = '<None:' + TItem.ClassName + '>' )) then
      Value:= nil
    else
      begin
        WriteLn( 'Warning: gP3DPropertyAccessDataBlock.SetAsString() broken' );
        Owner:= TP3DDataBlock( Context ).Owner.GetOwnerFromPath( AValue, True );
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
      end;
end;

function gP3DPropertyAccessDataBlock.Serialize: String;
begin
  if ( Assigned( Value )) then
    Result:= '"' + StringToJSONString( Value.DataPath ) + '"'
  else
    Result:= '""';
end;

function gP3DPropertyAccessDataBlock.SerializeFull: String;
begin
  Result:= inherited Serialize;
end;

function gP3DPropertyAccessDataBlock.CheckSameContext: Boolean;
begin
  Result:= Assigned( Value ) and ( Context is TP3DDataBlock ) and ( TP3DDataBlock( Context ).Owner = Value.Owner );
end;

function gP3DPropertyAccessDataBlock.GetDefaultValue: TItem;
begin
  Result:= nil;
end;

function gP3DPropertyAccessDataBlock.GetChildPropByPath( APath: String ): TP3DPropertyAccess;
begin
  if ( Assigned( Value )) then
    Result:= Value.Properties.GetPropByPath( APath );
end;

procedure gP3DPropertyAccessDataBlock.ValueCreateNew(ClTp: TP3DInterfacedPersistentType; AContext: TP3DJSONContext);
begin
  //if (( AContext.Data.JSONType is jtObject ) and ( AContext.Owner )) ;
  if ( AContext.Owner is TP3DLibrary ) then
    if ( ClTp.InheritsFrom( TItem )) then
      Value:= TItem( ClTp ).Create( TP3DLibrary( AContext.Owner ));
end;

procedure gP3DPropertyAccessDataBlock.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  inherited LoadFromJSONContext(AContext);
end;

procedure gP3DPropertyAccessDataBlock.CreateValueFromJSONContext(AClass: TP3DInterfacedPersistentType; AContext: TP3DJSONContext);
var
  BlockName: TJSONData;
  cnt: TP3DNamedInterfacedPersistentList;
  n: Integer;
begin
  BlockName:= AContext.Data.FindPath( 'Name' );
  if ( not ( Assigned( BlockName ) and ( AContext.Owner is TP3DLibrary ))) then
    exit;
  cnt:= TP3DDataBlockType( AClass ).GetContainer( TP3DLibrary( AContext.Owner ));
  n:= cnt.FindByName( BlockName.AsString );
  if ( n > -1 ) then
    Value:= TItem( cnt[ n ])
  else
    inherited CreateValueFromJSONContext( AClass, AContext );
end;

procedure StringSplit( S: String; Delim: Char; var S1, S2: String );
begin
  S1:= ExtractWord( 1, S, [ Delim ]);
  S2:= Copy( S, Length( S1 ) + Length( Delim ) + 1, Length( S ));
end;


{ TP3DDataBlock }

procedure TP3DDataBlock.SetName(NewName: TComponentName);
var
  idx: Integer;
begin
  inherited SetName(NewName);
  idx:= P3DDataBlockCache.FindBlock( Self );
  if ( idx > -1 ) then
    P3DDataBlockCache[ idx ].Name:= Name;
end;

constructor TP3DDataBlock.Create(const AData: TP3DLibrary);
begin
  if ( AData = nil ) then
    FOwner:= P3DData
  else
    FOwner:= AData;
  inherited Create( GetContainer( Owner ));
  //FileWatch:= TP3DFilePointer.Create( '<None>', Self );
  //DOMPath:= '';
  //AABB:= P3DInvalidAABB;
  FUsers:= TP3DDataBlockList.Create( Self );
  P3DDataBlockCache.Add( Self );
  //Properties.Add( TP3DPropertyAccessAction.CreateFieldSet( 'Action', @FAction, @SetAction, smAttribute ));
  Properties.Add( TP3DPropertyAccessLibrary.CreateFieldSet( 'Owner', @FOwner, nil, smDoNotStore ));
end;

constructor TP3DDataBlock.CreateFromJSONContext(AContext: TP3DJSONContext);
begin
  if ( AContext.Owner is TP3DLibrary ) then
    Create( TP3DLibrary( AContext.Owner ))
  else
    Create( P3DData );
  LoadFromJSONContext( AContext );
  WriteLn( 'CreateFromJSONContext: ', Name );
end;

destructor TP3DDataBlock.Destroy;
begin
  ClearUsers;
  Owner.RemoveFromFields( Self );
  ClearFields;

  FastDestroy;
end;

procedure TP3DDataBlock.FastDestroy;
begin
  FUsers.Free;
  {$IFDEF DEBUG_DATABLOCKS}
  if ( Assigned( Owner.FileWatch ) and ( Owner.FileWatch.FileName > '' )) then
    WriteLn( '<' + ExtractFileNameOnly( Owner.FileWatch.FileName ) + '.' + Name + ':' + ClassName + '>.FastDestroy' )
  else
    WriteLn( '<main.' + Name + ':' + ClassName + '>.FastDestroy' );
  {$ENDIF}
  inherited Destroy;
end;


procedure TP3DDataBlock.LoadFromFile(FileName: String);
var
  Path, FN, dir: String;
  Loader: TP3DJSONLoader;
  json: TJSONData;
begin
  StringSplit( FileName, '?', FN, Path );

  if ( lowercase( ExtractFileExt( FN )) = '.p3d' ) then
    begin
      FName:= P3DSearchPaths.FindFileEx( FN, GetCurrentDir );
      if ( FName = '' ) then
        raise Exception.Create( Format( 'Cannot load datablock from file! The specified file "%s" was not found!', [ FileName ]));
      try
        dir:= GetCurrentDir;
        SetCurrentDir( ExtractFilePath( FName ));
        Loader:= TP3DJSONLoader.Create( FName );
        Loader.Root:= Self.Owner;
        json:= Loader.FindPath( Path );

        if ( not Assigned( json )) then
          raise Exception.Create( Format( 'Cannot load datablock from file! The specified node "%s" was not found in the file "%s"!', [ Path, FName ]));
        Self.LoadFromJSONContext( P3DJSONContext( Loader, json, Self.Owner, nil ));
      finally
        SetCurrentDir( dir );
        Loader.Free;
      end;
      {ReadXMLFile( xml, FName );
      dir:= GetCurrentDir;
      try
        xml.DocumentElement.SetAttribute( 'filename', FName );
        SetCurrentDir( ExtractFilePath( FName ));

        Root:= P3DXMLFindPath( xml.DocumentElement, XMLPath );
        if ( not Assigned( Root )) then
          raise Exception.Create( Format( 'Cannot load datablock from file! The specified node "%s" was not found in the file "%s"!', [ XMLPath, FName ]));
        LoadFromDOMNew( Root );
      finally
        SetCurrentDir( dir );
        xml.Free;
      end;}
    end
  else
    LoadFromBinary( FN );
end;

procedure TP3DDataBlock.RemoveUser( User: TP3DDataBlock );
var
  idx: Integer;
begin
  {$IFDEF DEBUG_DATABLOCKS}
  if ( Assigned( Parent.FileWatch ) and ( Parent.FileWatch.FileName > '' )) then
    WriteLn( '<' + ExtractFileNameOnly( Parent.FileWatch.FileName ) + '.' + Name + ':' + ClassName + '>.RemoveUser( ' + User.Name + ' )' )
  else
    WriteLn( '<main.' + Name + ':' + ClassName + '>.RemoveUser( ' + User.Name + ' )' );
  WriteLn( Name + '.Users before remove: ' + Users.DumpUsers );
  {$ENDIF}
  if ( P3DDataBlockCache.IsValid( Self )) then
    begin
      Users.Remove( User );
    end
  else
    begin
      idx:= P3DDataBlockCache.FindBlock( Self );
      if ( idx > -1 ) then
        WriteLn( User.Name + ': Orphaned user with cached name ' + P3DDataBlockCache[ idx ].Name )
      else
        WriteLn( User.Name + ': Orphaned user! ' );
    end;
end;

procedure TP3DDataBlock.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  if ( TimeStamp < AContext.JSONLoader.LastFileAge ) then
    begin
      inherited LoadFromJSONContext(AContext);
      FTimeStamp:= AContext.JSONLoader.LastFileAge;
    end
  else
    WriteLn( 'Datablock was already loaded: ', Name );
end;

procedure TP3DDataBlock.AddUser( User: TP3DDataBlock );
begin
  if ( User = Self ) then
    raise Exception.Create( 'TP3DDataBlock.AddUser( ' + User.Name + ' ): Users loop detected!' );

  {$IFDEF DEBUG_DATABLOCKS}

  if ( not ( User is TP3DObject )) then
    begin
      if ( Assigned( Owner.FileWatch ) and ( Owner.FileWatch.FileName > '' )) then
        WriteLn( '<' + ExtractFileNameOnly( Owner.FileWatch.FileName ) + '.' + Name + ':' + ClassName + '>.AddUser( ' + User.Name + ' )' )
      else
        WriteLn( '<main.' + Name + ':' + ClassName + '>.AddUser( ' + User.Name + ' )' );
    end;
  {$ENDIF}
  Users.Add( User );
end;

procedure TP3DDataBlock.ClearFields;
begin
  //nothing to do in base class
end;

procedure TP3DDataBlock.ClearUsers;
var
  i: Integer;
begin
  for i:= Users.Count - 1 downto 0 do
    if ( P3DDataBlockCache.IsValid( Users.NamedItems[ i ])) then
      Users.NamedItems[ i ].RemoveFromFields( Self );
  Users.Clear;
end;

procedure TP3DDataBlock.LoadFromBinary(FileName: String);
begin

end;

procedure TP3DDataBlock.Reload;
var
  Node: TDOMElement;
  Dir: String;
begin
  WriteLn( 'Warning: TP3DDataBlock.Reload broken' );
  {Owner.OpenDOMFile;
  Node:= Owner.FindDOMToPath( DataPath );
  Dir:= GetCurrentDir;
  SetCurrentDir( ExtractFilePath( Owner.FileWatch.FileName ));
  if ( Assigned( Node )) then
    LoadFromDOM( Node )
  else
    WriteLn( 'Warning, cannot reload. Not found in DOM file!' );
  Owner.CloseDOMFile;
  SetCurrentDir( Dir );}
end;


procedure TP3DDataBlock.RemoveFromFields(Block: TP3DDataBlock);
begin
  {$IFDEF DEBUG_DATABLOCKS}
  if ( Assigned( Owner.FileWatch ) and ( Owner.FileWatch.FileName > '' )) then
    WriteLn(  '<' + ExtractFileNameOnly( Owner.FileWatch.FileName ) + '.' + Name + ':' + ClassName + '>.RemoveFromFields( ' + Block.Name + ' )' )
  else
    WriteLn( '<main.' + Name + ':' + ClassName + '>.RemoveFromFields( ' + Block.Name + ' )' );
  {$ENDIF}
  {if ( Block = Action ) then
    Action:= nil;}
end;


{ gP3DDatablockContainer }

procedure gP3DDatablockContainer.Clear(const FreeObjects: Boolean);
begin
  inherited Clear( FreeObjects );
end;

function gP3DDatablockContainer.FindInsertPos(AName: String): Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 0 to Count - 1 do
    begin //WriteLn( Items[ i ].Name, '<', AName, ' = ', Items[ i ].Name < AName );
      if ( Items[ i ].Name < AName ) then
        Result:= i + 1
      else
        break;
    end;
end;

function gP3DDatablockContainer.UpdateItemIndex(Index: Integer): Integer;
var
  Item: TCustomItem;
begin
  Item:= Items[ Index ];
  Result:= FindInsertPos( Item.Name );
  if ( Result <> Index ) then
    begin
      FItems.Delete( Index );
      FItems.Insert( Result, Item );
    end;
end;

{ gP3DListPropertyAccessContainer }

procedure gP3DListPropertyAccessContainer.LoadFromJSONContext(AContext: TP3DJSONContext);
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

procedure gP3DListPropertyAccessContainer.LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext);
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
      Prop:= TPropType.Create( 'ItemName', smDoNotStore );
      Prop.Field:= @Val;
      Prop.LoadFromJSONContext( AContext );
      Prop.Free;
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtObject ), JSONTypeName( AContext.Data.JSONType )]);
end;

function gP3DListPropertyAccessContainer.Serialize: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to ArrayCount - 1 do
    Result+= ', ' + TPropType( GetArrayAsProp( i )).SerializeFull;
  Result:= '[' + Copy( Result, 3, Length( Result ) - 2 ) + ']';
end;


{ TP3DPropertyAccessDataBlockList }


procedure gP3DPropertyAccessDataBlockList.LoadFromJSONContext(AContext: TP3DJSONContext);
var
  i: Integer;
  FakeProp: TP3DPropertyAccess;
  Item: TItem;
begin
  if ( AContext.Data.JSONType = jtArray ) then
    begin
      Value.Count:= AContext.Data.Count;
      for i:= 0 to AContext.Data.Count - 1 do
        begin
          FakeProp:= //TPropType.CreateField( 'FakePropName', @Item, smDoNotStore );
          GetArrayAsProp( i );
          FakeProp.LoadFromJSONContext( P3DJSONContext( AContext.JSONLoader, AContext.Data.Items[ i ], AContext.Owner, AContext.Parent ));
        end;
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtArray ), JSONTypeName( AContext.Data.JSONType )]);
end;


{$ENDIF}
