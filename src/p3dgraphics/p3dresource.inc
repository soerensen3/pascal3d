//p3dresource.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}
type
  { TP3DResourceManager }

  TP3DDataList = class;
  TP3DData = class ( TPersistent )
    private
      FActions: TP3DArmatureActionList;
      FArmatures: TP3DArmatureList;
      FCameras: TP3DCameraList;
      FDataBlocks: TP3DObjectList;
      FDOMFile: TXMLDocument;
      FFileWatch: TP3DFileWatch;
      FIsDesignerPackage: Boolean;
      FLibraries: TP3DDataList;
      FLights: TP3DLightList;
      FMaterials: TP3DMaterialList;
      FMeshes: TP3DMeshList;
      FObjects: TP3DActorList;
      FScenes: TP3DSceneList;
      FShaders: TP3DShaderList;
      FTextures: TP3DTextureList;
      function GetName: String;

    public
      constructor Create;
      constructor CreateFromDOM( ADOMNode: TDOMElement );
      constructor CreateFromFile( AFileName: String );
      destructor Destroy; override;

      procedure OpenDOMFile;
      procedure CloseDOMFile;
      function FindDOMToPath( DOMPath: String ): TDOMElement;

      function AppendFromDom( ADOMNode: TDOMElement; const AParent: TP3DActor = nil ): Integer;
      function AppendFromDom( ADOMNode: TDOMElement; AName: String;
        AType: TP3DDataBlockType; const AParent: TP3DActor = nil ): Integer;
      function AppendAllFromDom( ADOMNode: TDOMElement; AType: TP3DDataBlockType;
        const AParent: TP3DActor = nil ): Integer;
      function AppendFile( AFileName: String; const AParent: TP3DActor = nil ): String;
      function CreateNew( Tp: TP3DDataBlockType; const CreateObject: Boolean = True ): Integer;
      procedure RemoveFromFields( Block: TP3DDataBlock ); virtual;
      procedure ClearFields;
      function IsValid( Obj: TP3DObject ): Boolean;

      property DataBlocks: TP3DObjectList read FDataBlocks write FDataBlocks;

    published
      property Cameras: TP3DCameraList read FCameras write FCameras;
      property Libraries: TP3DDataList read FLibraries write FLibraries;
      property Lights: TP3DLightList read FLights write FLights;
      property Materials: TP3DMaterialList read FMaterials write FMaterials;
      property Meshes: TP3DMeshList read FMeshes write FMeshes;
      property Objects: TP3DActorList read FObjects write FObjects;
      property Scenes: TP3DSceneList read FScenes write FScenes;
      property Shaders: TP3DShaderList read FShaders write FShaders;
      property Textures: TP3DTextureList read FTextures write FTextures;
      property Armatures: TP3DArmatureList read FArmatures write FArmatures;
      property Actions: TP3DArmatureActionList read FActions write FActions;
      property FileWatch: TP3DFileWatch read FFileWatch write FFileWatch;
      property DOMFile: TXMLDocument read FDOMFile write FDOMFile;
      property IsDesignerPackage: Boolean read FIsDesignerPackage write FIsDesignerPackage;
      property Name: String read GetName;
  end;

  TP3DCustomDataList = specialize gP3DCustomObjectList < TP3DData >;

  { TP3DDataList }

  TP3DDataList = class ( TP3DCustomDataList )
    function FindByFileName( AFileName: String ): Integer;
  end;

  { TDataBlockCacheEntry }

  { TP3DDataBlockCacheEntry }

  TP3DDataBlockCacheEntry = class ( TPersistent )
    private
      FBlockType: TP3DDataBlockType;
      FDatablock: TP3DDataBlock;
      FName: String;
      FStackTrace: String;

    published
      property Name: String read FName write FName;
      property BlockType: TP3DDataBlockType read FBlockType write FBlockType;
      property Datablock: TP3DDataBlock read FDatablock write FDatablock;
      property StackTrace: String read FStackTrace write FStackTrace;
  end;

  TP3DDatablockCacheEntryList = specialize gP3DCustomObjectList < TP3DDatablockCacheEntry >;

  { TP3DDataBlockCache }

  TP3DDataBlockCache = class ( TP3DDatablockCacheEntryList )
    public
      function FindBlock( Block: TP3DDataBlock ): Integer;
      function Add( Block: TP3DDataBlock ): Integer;
      destructor Destroy; override;
  end;

  function OpenLibrary( FileName: String ): Integer; //TODO: Move to libraries


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DData }

function TP3DData.GetName: String;
begin
  Result:= ExtractFileName( FileWatch.FileName );
end;

constructor TP3DData.Create;
begin
  inherited;

  DataBlocks:= TP3DObjectList.Create( Self );

  Cameras:= TP3DCameraList.Create;
  Lights:= TP3DLightList.Create;
  Materials:= TP3DMaterialList.Create;
  Meshes:= TP3DMeshList.Create;
  Objects:= TP3DActorList.Create( Self );
  Libraries:= TP3DDataList.Create;
  Scenes:= TP3DSceneList.Create;
  Shaders:= TP3DShaderList.Create;
  Textures:= TP3DTextureList.Create;
  Armatures:= TP3DArmatureList.Create;
  Actions:= TP3DArmatureActionList.Create;
  FileWatch:= TP3DFileWatch.Create( '', Self );
end;

constructor TP3DData.CreateFromDOM(ADOMNode: TDOMElement);
begin
  Create;
  AppendFromDom( ADOMNode );
end;

constructor TP3DData.CreateFromFile(AFileName: String);
begin
  Create;
  FileWatch.FileName:= AppendFile( AFileName );
end;

destructor TP3DData.Destroy;
begin
  {$IFDEF DEBUG_DATABLOCKS}
  if ( Assigned( FileWatch ) and ( FileWatch.FileName > '' )) then
    WriteLn( '<' + ExtractFileNameOnly( FileWatch.FileName ) + '>.Destroy' )
  else
    WriteLn('<main>.Destroy' );

  if ( Assigned( FileWatch ) and ( FileWatch.FileName > '' )) then
    WriteLn( '<' + ExtractFileNameOnly( FileWatch.FileName ) + '>.Objects: ' + DataBlocks.DumpObjectList )
  else
    WriteLn('<main>.Objects: ' + DataBlocks.DumpObjectList );
  {$ENDIF}

  ClearFields;
  FreeAndNil( FLibraries );
  FreeAndNil( FCameras );
  FreeAndNil( FLights );
  FreeAndNil( FMaterials );
  FreeAndNil( FMeshes );
  FreeAndNil( FObjects );
  FreeAndNil( FScenes );
  FreeAndNil( FShaders );
  FreeAndNil( FTextures );
  FreeAndNil( FArmatures );
  FreeAndNil( FActions );
  FreeAndNil( FFileWatch );

  FreeAndNil( FDataBlocks );

  inherited Destroy;
end;

procedure TP3DData.OpenDOMFile;
begin
  ReadXMLFile( FDOMFile, FileWatch.FileName );
end;

procedure TP3DData.CloseDOMFile;
begin
  FreeAndNil( FDOMFile );
end;

function TP3DData.FindDOMToPath(DOMPath: String): TDOMElement;
begin
  Result:= FindDOMByIdentifier( DOMPath, DOMFile.DocumentElement );
end;

function TP3DData.AppendFromDom(ADOMNode: TDOMElement; const AParent: TP3DActor): Integer;
var
  Parent: TP3DActorList;

  function LoadData(): Integer;
    function CreateObject: Integer;
    var
      tp: TP3DDataBlockType;
    begin
      Result:= -1;

      case ADOMNode.NodeName of
        'mesh':
          tp:= TP3DMesh;
        'material':
          case ADOMNode.GetAttribute( 'type' ) of
            'shader': tp:= TP3DMaterialShader;
            else
              tp:= TP3DMaterialBase;
          end;
        'light', 'lamp':
          tp:= TP3DLight;
        'camera':
          tp:= TP3DCamera;
        'scene':
          tp:= TP3DScene;
        'armature':
          tp:= TP3DArmature;
        'action':
          tp:= TP3DArmatureAction;
      end;
      Result:= CreateNew( tp, False );
    end;

  var
    AName: DOMString;
  begin
    AName:= ADOMNode.GetAttribute( 'name' );
    Result:= DataBlocks.FindByName( AName );

    if ( Result = -1 ) then
      begin
        Result:= CreateObject;
        TP3DDataBlock( DataBlocks[ Result ]).LoadFromDOM( ADOMNode );
      end;
    {$IFDEF VERBOSE}
    WriteLn( Format( 'Created datablock of type "%s" and name "%s" with index %d', [ ADOMNode.NodeName, AName, Result ]));
    {$ENDIF}
  end;

begin
  if ( not Assigned( ADOMNode )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node which is not assigned!' );

  if ( Assigned( AParent )) then
    Parent:= AParent.Children
  else
    Parent:= Objects;

  case ADOMNode.NodeName of
    'p3dfile':
      AppendAllFromDom( ADOMNode, TP3DScene, AParent );
    'object':
      Result:= Parent.Add( TP3DActor.CreateFromDOM( ADOMNode, Self ));
    'mesh', 'material',
      'camera', 'light',
      'lamp', 'scene',
      'armature', 'action': Result:= LoadData();
    'texture':
        Result:= DataBlocks.IndexOf( TP3DTexture.CreateFromDOM( ADOMNode, Self ))
    else
      P3DLog.LogInfo( Self, 'Warning: Cannot append from dom. Unsupported node type "' + ADOMNode.NodeName + '". Skipping.' );
  end;
end;

function TP3DData.AppendFromDom(ADOMNode: TDOMElement;
  AName: String; AType: TP3DDataBlockType; const AParent: TP3DActor): Integer;
var
  Element: TDOMElement;
begin
  Element:= TDOMElement( ADOMNode.FirstChild );
  Result:= DataBlocks.FindByName( AName );
  if ( Result > -1 ) then
    if ( not ( DataBlocks[ Result ] is AType )) then
      raise Exception.Create( Format( 'Error while appending from DOM: A class with name %s was already loaded before but is of the wrong type. Expected "%s", Found "%s"', [ AName, AType.ClassName, FindClassForDOM( Element ).ClassName ]))
    else
      exit;
  while ( Assigned( Element )) do
    begin
      if ( Element.GetAttribute( 'name' ) = AName ) then
        if ( FindClassForDOM( Element ).InheritsFrom( AType )) then
          break
        else
          raise Exception.Create( Format( 'Error while appending from DOM: A class with name %s was found but is of the wrong type. Expected "%s", Found "%s"', [ AName, AType.ClassName, FindClassForDOM( Element ).ClassName ]));
      Element:= TDOMElement( Element.NextSibling );
    end;
  if ( not Assigned( Element )) then
    raise Exception.Create( Format( 'Error while appending from DOM: A class with name %s was not found!', [ AName ]));
  Result:= AppendFromDom( Element, AParent );
end;

function TP3DData.AppendAllFromDom(ADOMNode: TDOMElement;
  AType: TP3DDataBlockType; const AParent: TP3DActor): Integer;
var
  filter: String;
  lst: TDOMNodeList;
  i: Integer;
begin
  filter:= FindDOMForClass( AType );
  lst:= ADOMNode.GetElementsByTagName( filter );
  Result:= 0;
  for i:= 0 to lst.Count - 1 do
    begin
      AppendFromDom( TDOMElement( lst[ i ]), AParent );
      Inc( Result );
    end;
end;

function TP3DData.AppendFile( AFileName: String; const AParent: TP3DActor ): String;
var
  dir, FName: String;
begin
  Result:= '';
  FName:= P3DSearchPaths.Models.FindFileEx( AFileName, GetCurrentDir );
  if ( FName = '' ) then
    raise Exception.Create( Format( 'Cannot load library! The specified file "%s" was not found!', [ AFileName ]));
  FName:= P3DSearchPaths.Models.FindFile( AFileName );
  if ( not FileExists( FName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );

  FileWatch.FileName:= AFileName;
  OpenDOMFile;
  dir:= GetCurrentDir;
  try
    DOMFile.DocumentElement.SetAttribute( 'filename', AFileName );
    SetCurrentDir( ExtractFilePath( FName ));
    AppendFromDom( DOMFile.DocumentElement, AParent );
  finally
    SetCurrentDir( dir );
    CloseDOMFile;
  end;
  Result:= FName;
end;

function TP3DData.CreateNew(Tp: TP3DDataBlockType; const CreateObject: Boolean ): Integer;
var
  data: TP3DDataBlock;
  obj: TP3DActor;
begin
  data:= Tp.Create( Self );

  if ( CreateObject ) then
    begin
      obj:= TP3DActor.Create( Self );
      obj.data:= data;
      Result:= Objects.IndexOf( obj );
    end
  else
    Result:= DataBlocks.IndexOf( data );
end;

procedure TP3DData.RemoveFromFields( Block: TP3DDataBlock );
begin
  {$IFDEF DEBUG_DATABLOCKS}
  if ( Assigned( FileWatch ) and ( FileWatch.FileName > '' )) then
    WriteLn( '<' + ExtractFileNameOnly( FileWatch.FileName ) + ':TP3DData>.RemoveFromFields( ' + Block.Name + ' )' )
  else
    WriteLn('<main:TP3DData>.RemoveFromFields( ' + Block.Name + ' )' );
  {$ENDIF}

  {DataBlocks.Remove( Block );
  if ( Block is TP3DCamera and Assigned( Cameras )) then
    Cameras.Remove( TP3DCamera( Block ))
  else if ( Block is TP3DLight and Assigned( Lights )) then
    Lights.Remove( TP3DLight( Block ))
  else if ( Block is TP3DMaterial and Assigned( Materials )) then
    Materials.Remove( TP3DMaterial( Block ))
  else if ( Block is TP3DMesh and Assigned( Meshes )) then
    Meshes.Remove( TP3DMesh( Block ))
  else if ( Block is TP3DActor and Assigned( Objects )) then
    Objects.Remove( TP3DActor( Block ))
  else if ( Block is TP3DScene and Assigned( Scenes )) then
    Scenes.Remove( TP3DScene( Block ))
  else if ( Block is TP3DShader and Assigned( Shaders )) then
    Shaders.Remove( TP3DShader( Block ))
  else if ( Block is TP3DTexture and Assigned( Textures )) then
    Textures.Remove( TP3DTexture( Block ))
  else if ( Block is TP3DArmatureAction and Assigned( Actions )) then
    Actions.Remove( TP3DArmatureAction( Block ))
  else if ( Block is TP3DArmature and Assigned( Armatures )) then
    Armatures.Remove( TP3DArmature( Block ));}
end;

procedure TP3DData.ClearFields;
var
  User: TP3DDataBlock;
  Block: TP3DObject;
  i, j, idx: Integer;
begin
  for i:= Libraries.Count - 1 downto 0 do
    begin
      Libraries[ i ].Free;
      Libraries.Delete( i );
    end;

  for i:= Datablocks.Count - 1 downto 0 do
    begin
      Block:= DataBlocks[ i ];
      {if ( Block is TP3DDataBlock ) then
        begin
          for j:= TP3DDataBlock( TP3DDataBlock( Block )).Users.Count - 1 downto 0 do
            begin
              User:= TP3DDataBlock( TP3DDataBlock( Block )).Users[ j ];
              if ( User.Owner <> Self ) then
                User.RemoveFromFields( TP3DDataBlock( Block ));
            end;
          TP3DDataBlock( Block ).FastDestroy; //Destroy the object without checking fields
        end
      else }if ( IsValid( Block )) then
        Block.Free
      else
        begin
          idx:= P3DDataBlockCache.FindBlock( TP3DDataBlock( Block ));
          if ( idx > -1 ) then
            WriteLn( 'Orphaned datablock no. ', i, ' with cached name: ', P3DDataBlockCache[ idx ].Name )
          else
            WriteLn( 'Orphaned datablock no. ', i, ' with no cached name' );
        end;
    end;
  Datablocks.Clear( False );
  Cameras.Clear();
  Lights.Clear();
  Materials.Clear();
  Meshes.Clear();
  Objects.Clear();
  Scenes.Clear();
  Shaders.Clear();
  Textures.Clear();
  Actions.Clear();
  Armatures.Clear();
end;

function TP3DData.IsValid(Obj: TP3DObject): Boolean;
var
  Lib: TP3DData;
begin
  if ( not Assigned( DataBlocks )) then
    begin
      Result:= False;
      exit;
    end;
  Result:= DataBlocks.IndexOf( Obj ) >= 0;
  if ( not Result ) then
    for Lib in Libraries do
      begin
        Result:= Lib.IsValid( Obj );
        if ( Result ) then
          break;
      end;
end;

function OpenLibrary( FileName: String ): Integer;
var
  F: TP3DData;
  FName: String;
begin
  FName:= P3DSearchPaths.Models.FindFileEx( FileName, GetCurrentDir );
  if ( FName = '' ) then
    raise Exception.Create( Format( 'Cannot load library! The specified file "%s" was not found!', [ FileName ]));
  Result:= P3DData.Libraries.FindByFileName( FName );
  if ( Result = -1 ) then
    begin
      F:= TP3DData.CreateFromFile( FName );
      Result:= P3DData.Libraries.Add( F );
    end;
end;

{ TP3DDataList }

function TP3DDataList.FindByFileName(AFileName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( P3DMatchFileName( Items[ i ].FileWatch.FileName, AFileName )) then
      begin
        Result:= i;
        break;
      end;
end;

{ TP3DDataBlockCache }

function TP3DDataBlockCache.FindBlock(Block: TP3DDataBlock): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Self[ i ].Datablock = Block ) then
      begin
        Result:= i;
        Break;
      end;
end;
function DumpCallStack: String;
var
  I: Longint;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
const
  MaxDepth = 20;
begin
  Result := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    while bp > prevbp do begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;
       Result := Result + BackTraceStrFunc(CallerAddress) + LineEnding;
       Inc(I);
       if (I >= MaxDepth) or (CallerFrame = nil) then
         Break;
       prevbp := bp;
       bp := CallerFrame;
     end;
   except
     { prevent endless dump if an exception occured }
   end;
end;

function TP3DDataBlockCache.Add(Block: TP3DDataBlock): Integer;
var
  entry: TP3DDataBlockCacheEntry;
begin
  entry:= TP3DDataBlockCacheEntry.Create;
  entry.Datablock:= Block;
  entry.Name:= Block.Name;
  entry.BlockType:= TP3DDataBlockType( Block.ClassType );
  //entry.StackTrace:= DumpCallStack;

  inherited Add( entry );
end;

destructor TP3DDataBlockCache.Destroy;
begin
  Clear( True );
  inherited Destroy;
end;

{ TP3DDataBlockList }

function TP3DDataBlockList.DumpUsers: String;
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


{$ENDIF}

