//p3dresource.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

const
  //Version 0.1
  P3DFileVerMaj = 0;
  P3DFileVerMin = 1;

type
  { TP3DResourceManager }

  TP3DLibraryList = class;

  { TP3DLibrary }

  TP3DLibrary = class ( TPersistent )
    private
      FActions: TP3DArmatureActionList;
      FArmatures: TP3DArmatureList;
      FCameras: TP3DCameraList;
      FDataBlocks: TP3DObjectList;
      FDOMFile: TXMLDocument;
      FFileWatch: TP3DFileWatch;
      FIsDesignerPackage: Boolean;
      FLights: TP3DLightList;
      FMaterials: TP3DMaterialList;
      FMeshes: TP3DMeshList;
      FObjects: TP3DActorList;
      FScenes: TP3DSceneList;
      FShaders: TP3DShaderList;
      FTextures: TP3DTextureList;

      function GetDOMFile: TXMLDocument;
      function GetName: String;
//      function LoadDataBlockByPathNoCheck( LocalPath: String ): TP3DDataBlock;

    public
      constructor Create;
      constructor CreateFromDOM( ADOMNode: TDOMElement );
      constructor CreateFromFile( AFileName: String );
      destructor Destroy; override;

      procedure OpenDOMFile;
      procedure CreateDOMFile;
      procedure CloseDOMFile;
      procedure SaveDOMFile( const FName: String = '' );
      function FindDOMToPath( DOMPath: String ): TDOMElement;
      function GetOwnerFromPath( DOMPath: TP3DXMLPath; const ForceLoad: Boolean = False ): TP3DLibrary;
//      function FindDataBlockByPath( DOMPath: String ): TP3DDataBlock;
//      function LoadDataBlockByPath( DOMPath: String ): TP3DDataBlock;
//      function LoadDatablockByDOM(ADOMNode: TDOMElement): TP3DDataBlock;

      function AppendFromDOM( ADOMNode: TDOMElement; AParent: TP3DDataBlock ): Integer;
      function AppendFromDOM( ADOMNode: TDOMElement; AType: TP3DDataBlockType; const AParent: TP3DDataBlock = nil ): Integer;
      function AppendFromDOM( ADOMNode: TDOMElement ): Integer;
      function AppendFromPath( APath: TP3DXMLPath ): Integer;
      function AppendFromPath( APath: TP3DXMLPath; AParent: TP3DDataBlock ): Integer;
      function AppendFromPath( APath: TP3DXMLPath; AType: TP3DDataBlockType; const AParent: TP3DDataBlock = nil ): Integer;

      function AppendAllFromDom( ADOMNode: TDOMElement; AType: TP3DDataBlockType ): Integer;
      function AppendFile( AFileName: String ): String;
      function CreateNew( Tp: TP3DDataBlockType; const CreateObject: Boolean = True ): Integer;
      procedure SaveToDOM( const ADOMNode: TDOMElement = nil );
      procedure RemoveFromFields( Block: TP3DDataBlock ); virtual;
      procedure ClearFields; virtual;
      function IsValid( Obj: TP3DObject ): Boolean; virtual;
      function CheckLastChange: Integer; virtual;

      property DataBlocks: TP3DObjectList read FDataBlocks write FDataBlocks;

    published
      property Cameras: TP3DCameraList read FCameras write FCameras;
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
      property DOMFile: TXMLDocument read GetDOMFile write FDOMFile;
      property IsDesignerPackage: Boolean read FIsDesignerPackage write FIsDesignerPackage;
      property Name: String read GetName;
  end;

  TP3DData = class ( TP3DLibrary )
    private
      FLibraries: TP3DLibraryList;

    public
      constructor Create;
      destructor Destroy; override;

      procedure ClearFields; override;
      function IsValid(Obj: TP3DObject): Boolean; override;
      function OpenLibrary( FileName: String ): Integer;
      function CheckLastChange: Integer; override;

    published
      property Libraries: TP3DLibraryList read FLibraries write FLibraries;
  end;

  { TP3DLibraryList }

  TP3DLibraryList = class ( specialize gP3DCustomObjectList < TP3DLibrary >)
    private
      FLastUpdate: Integer;
      procedure DoChange; override;

    public
      function FindByFileName( AFileName: String ): Integer;
      property LastUpdate: Integer read FLastUpdate;
  end;

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
      function Add( Block: TP3DDataBlock ): Integer; reintroduce;
      destructor Destroy; override;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DData }

function TP3DLibrary.GetName: String;
begin
  Result:= ExtractFileName( FileWatch.FileName );
end;

function TP3DLibrary.GetDOMFile: TXMLDocument;
begin
  if ( not Assigned( FDOMFile )) then
    OpenDOMFile;
  Result:= FDOMFile;
end;

constructor TP3DLibrary.Create;
begin
  inherited;

  DataBlocks:= TP3DObjectList.Create( Self );

  Cameras:= TP3DCameraList.Create;
  Lights:= TP3DLightList.Create;
  Materials:= TP3DMaterialList.Create;
  Meshes:= TP3DMeshList.Create;
  Objects:= TP3DActorList.Create( Self );
  Scenes:= TP3DSceneList.Create;
  Shaders:= TP3DShaderList.Create;
  Textures:= TP3DTextureList.Create;
  Armatures:= TP3DArmatureList.Create;
  Actions:= TP3DArmatureActionList.Create;
  FileWatch:= TP3DFileWatch.Create( '', Self );
end;

constructor TP3DLibrary.CreateFromDOM(ADOMNode: TDOMElement);
begin
  Create;
  AppendFromDom( ADOMNode );
end;

constructor TP3DLibrary.CreateFromFile(AFileName: String);
begin
  Create;
  FileWatch.FileName:= AppendFile( AFileName );
end;

destructor TP3DLibrary.Destroy;
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
  CloseDOMFile;

  ClearFields;
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

procedure TP3DLibrary.OpenDOMFile;
begin
  CloseDOMFile;
  ReadXMLFile( FDOMFile, FileWatch.FileName );
end;

procedure TP3DLibrary.CreateDOMFile;
begin
  CloseDOMFile;
  FDOMFile:= TXMLDocument.Create;
end;

procedure TP3DLibrary.CloseDOMFile;
begin
  FreeAndNil( FDOMFile );
end;

procedure TP3DLibrary.SaveDOMFile(const FName: String);
var
  FileName: String;
begin
  if ( FName > '' ) then
    FileName:= FName
  else
    FileName:= FileWatch.FileName;
  P3DXMLWrite( DOMFile.DocumentElement, FileName );
end;

function TP3DLibrary.FindDOMToPath(DOMPath: String): TDOMElement;
begin
  Result:= P3DXMLFindPath( DOMFile.DocumentElement, DOMPath );
end;

function TP3DLibrary.GetOwnerFromPath(DOMPath: TP3DXMLPath; const ForceLoad: Boolean): TP3DLibrary;
var
  FN: String;
begin
  P3DXMLExtractRelativePath( DOMPath, FN );
  if (( FN = '' ) or ( CompareFilenames( FN, FileWatch.FileName ) = 0 )) then
    Result:= Self
  else
    if ( ForceLoad ) then
      Result:= P3DData.Libraries[ P3DData.OpenLibrary( FN )]
    else
      Result:= P3DData.Libraries[ P3DData.Libraries.FindByFileName( FN )];
end;

{
function TP3DLibrary.FindDataBlockByPath(DOMPath: String): TP3DDataBlock;
var
  RestPath, FN: String;
  LibN, i: Integer;
begin
  Result:= nil;
  RestPath:= P3DXMLExtractRelativePath( DOMPath, FN );

  if ( FN > '' ) then
    begin
      LibN:= P3DData.Libraries.FindByFileName( FN );
      if ( LibN > -1 ) then
        Result:= P3DData.Libraries[ LibN ].FindDatablockByDOM( RestPath );
    end
  else
    for i:= 0 to DataBlocks.Count - 1 do
      if (( DataBlocks[ i ] is TP3DDataBlock ) and ( TP3DDataBlock( DataBlocks[ i ]).DOMPath = RestPath )) then
        begin
          Result:= TP3DDataBlock( DataBlocks[ i ]);
          break;
        end;
end;


function TP3DLibrary.LoadDataBlockByPath(DOMPath: String): TP3DDataBlock;
var
  LibN: Integer;
  FN: String;
  RestPath: TP3DXMLPath;

begin
  RestPath:= P3DXMLExtractRelativePath( DOMPath, FN );

  if (( FN > '' ) and ( CompareFilenames( FN, FileWatch.FileName ) <> 0 )) then
    begin
      LibN:= P3DData.Libraries.FindByFileName( FN );
      if ( LibN < 0 ) then
        LibN:= P3DData.OpenLibrary( FN );
      if ( LibN > -1 ) then
        Result:= P3DData.Libraries[ LibN ].FindDatablockByDOM( RestPath )
    end
  else
    begin
      Result:= FindDataBlockByDOM( RestPath );
      if ( not Assigned( Result )) then
        Result:= LoadDataBlockByPathNoCheck( );
      if ( not Assigned( Result )) then
        raise Exception.Create( 'The specified datablock was not found: ' + FileWatch.FileName + '$' + RestPath );
    end;
  {if relative dom
    datablock FindDataBlockByDOM
    if datablock
      return datablock
    else
      return error
  else
    if Libraries.FindByFileName
      lib = P3DData.OpenLibrary
    return lib.LoadDataBlockByDOM}
end;

function TP3DLibrary.LoadDatablockByDOM(ADOMNode: TDOMElement): TP3DDataBlock;
  function CheckParentRecurse( ADOMNode: TDOMElement; var DB: TP3DDataBlock ): TP3DDataBlock;
  var
    AParentDOM: TDOMNode;
  begin
    AParentDOM:= ADOMNode.ParentNode;
    if ( Assigned( DB )) then
      if (
    if (( not Assigned( AParentDOM )) or ( AParentDOM.NodeName = 'p3dfile' )) then
      begin

      end;
  end;
var
  BlockName: String;
  Names: TStringList;
  ParentNd: TDOMElement;
  Parent: TP3DDataBlock;
  n, i: Integer;

  //Check if actor or not.
begin
  Result:= nil;

  if ( ADOMNode.NodeName = TP3DActor.DOMNodeName ) then
    begin
      Names:= TStringList.Create;
      ParentNd:= TDOMElement( ADOMNode.ParentNode );

      BlockName:= ADOMNode.AttribStrings[ 'Name' ];

      try
        while Assigned( ParentNd ) do
          begin
            if ( ParentNd.NodeName <> 'p3dfile' ) then
              Names.Add( ParentNd.AttribStrings[ 'Name' ]);
            ParentNd:= TDOMElement( ParentNd.ParentNode );
          end;

        Parent:= nil;
        i:= Names.Count - 1;

        repeat
           if ( Assigned( Parent )) then
             begin

             end
           else
             begin
               n:= Scenes.FindByName( Names[ i ]); // First node needs to be a scene
               if ( n > -1 ) then
                 Parent:= Objects[ n ]
               else
                 //LOAD
                 //if not loaded quit (exception already raised)


             end;

        until ( i < 0 ) or ( not Assigned( Parent ));

      finally
        Names.Free;
      end;
    end
  else
    Result:= Datablocks[ AppendFromDom( ADOMNode )];
end;
}

function TP3DLibrary.AppendFromDOM(ADOMNode: TDOMElement; AParent: TP3DDataBlock): Integer;
begin
  if ( ADOMNode.NodeName = 'p3dfile' ) then
    AppendAllFromDom( ADOMNode, TP3DScene )
  else
    begin
      if ( ADOMNode.NodeName = TP3DActor.DOMNodeName ) then
        begin
          if ( AParent is TP3DScene ) then
            Result:= TP3DScene( AParent ).Objects.Add( TP3DActor.CreateFromDOM( ADOMNode, Self ))
          else if ( AParent is TP3DActor ) then
            Result:= TP3DActor( AParent ).Children.Add( TP3DActor.CreateFromDOM( ADOMNode, Self ))
          else
            raise Exception.Create( 'TP3DLibrary.AppendFromDOM - Error: An object must have a parent.' );
        end
      else
        AppendFromDom( ADOMNode );
    end;
end;

//DEBUG THIS
function TP3DLibrary.AppendFromDOM( ADOMNode: TDOMElement; AType: TP3DDataBlockType; const AParent: TP3DDataBlock ): Integer;
var
  ObjName: WideString;
begin
  ObjName:= ADOMNode.AttribStrings[ 'Name' ];
  if ( not FindClassForDOM( ADOMNode.NodeName ).InheritsFrom( AType )) then
    raise Exception.Create( Format( 'Error while appending from DOM: The DOM with name %s is of the wrong type. Expected "%s", Found "%s"', [ ObjName, AType.ClassName, FindClassForDOM( ADOMNode ).ClassName ]));
  Result:= DataBlocks.FindByName( ObjName );
  if ( Result > -1 ) then
    begin
      if ( not ( DataBlocks[ Result ] is AType )) then
        raise Exception.Create( Format( 'Error while appending from DOM: A class with name %s was already loaded before but is of the wrong type. Expected "%s", Found "%s"', [ ObjName, AType.ClassName, FindClassForDOM( ADOMNode ).ClassName ]))
    end
  else
    begin
      if ( Assigned( AParent )) then
        Result:= AppendFromDOM( ADOMNode, AParent )
      else
        Result:= AppendFromDOM( ADOMNode );
    end;
end;

function TP3DLibrary.AppendFromDOM(ADOMNode: TDOMElement): Integer;
var
  ObjName: String;

  function LoadData(): Integer;
    function CreateObject: Integer;
    var
      tp: TP3DDataBlockType;
    begin
      Result:= -1;

      case ADOMNode.NodeName of
        'object': raise Exception.Create( 'TP3DLibrary.AppendFromDOM - Error: An object must have a parent.' );
        'mesh':
          tp:= TP3DMesh;
        'material':
          case ADOMNode.GetAttribute( 'type' ) of
            'shader': tp:= TP3DMaterialShader;
            else
              tp:= TP3DMaterialBase;
          end;
        'materialshader':
          tp:= TP3DMaterialShader;
        'materialbase':
          tp:= TP3DMaterialBase;
        'light', 'lamp':
          tp:= TP3DLight;
        'camera':
          tp:= TP3DCamera;
        'scene':
          tp:= TP3DScene;
        'armature':
          tp:= TP3DArmature;
        'action', 'armatureaction':
          tp:= TP3DArmatureAction;
        'texture':
          tp:= TP3DTexture;
        'tilegrid':
          tp:= TP3DTileGrid;
        else
          P3DLog.LogInfo( Self, 'Warning: Cannot append from dom. Unsupported node type "' + ObjName + '". Skipping.' );
      end;
      Result:= CreateNew( tp, False );
    end;

  var
    versionNd: TDOMElement;
    version: Extended;
  begin
    versionNd:= TDOMElement( ADOMNode.OwnerDocument.DocumentElement );
    if ( Assigned( versionNd )) then
      version:= StrToFloatDef( versionNd.GetAttribute( 'version' ), 0.0 );

    Result:= CreateObject;

    if ( version >= 0.1 ) then
      TP3DDataBlock( DataBlocks[ Result ]).LoadFromDOMNew( ADOMNode )
    else
      TP3DDataBlock( DataBlocks[ Result ]).LoadFromDOM( ADOMNode );
    {.$IFDEF VERBOSE}
    //WriteLn( Format( 'Created datablock of type "%s" and name "%s" with index %d', [ ADOMNode.NodeName, AName, Result ]));
    {.$ENDIF}
  end;

{
'mesh', 'material',
  'camera', 'light',
  'lamp', 'scene',
  'armature', 'action': Result:= LoadData();
'texture':
    Result:= DataBlocks.IndexOf( TP3DTexture.CreateFromDOM( ADOMNode, Self ))


else
  P3DLog.LogInfo( Self, 'Warning: Cannot append from dom. Unsupported node type "' + ADOMNode.NodeName + '". Skipping.' );
}
begin
  ObjName:= ADOMNode.AttribStrings[ 'Name' ];
  if ( ObjName = '' ) then //temporary
    ObjName:= ADOMNode.AttribStrings[ 'name' ];

  if ( ObjName > '' ) then
    begin
      Result:= DataBlocks.FindByName( ObjName );
      if ( Result = -1 ) then
        Result:= LoadData();
    end
  else
    Result:= -1;
end;

function TP3DLibrary.AppendFromPath(APath: TP3DXMLPath): Integer;
var
  Nd: TDOMElement;
  Owner: TP3DLibrary;
begin
  if ( not Assigned( DOMFile )) then
    begin
      Result:= -1;
      exit;
    end;

  Nd:= P3DXMLFindPath( DOMFile.DocumentElement, APath );
  Owner:= GetOwnerFromPath( APath );
  if ( Assigned( Nd )) then
    Result:= Owner.AppendFromDOM( Nd )
  else
    Result:= -1;
end;

function TP3DLibrary.AppendFromPath(APath: TP3DXMLPath; AParent: TP3DDataBlock): Integer;
var
  Nd: TDOMElement;
  Owner: TP3DLibrary;
begin
  if ( not Assigned( DOMFile )) then
    begin
      Result:= -1;
      exit;
    end;

  Nd:= P3DXMLFindPath( DOMFile.DocumentElement, APath );
  Owner:= GetOwnerFromPath( APath );
  if ( Assigned( Nd )) then
    Result:= Owner.AppendFromDOM( Nd, AParent )
  else
    Result:= -1;
end;

function TP3DLibrary.AppendFromPath(APath: TP3DXMLPath; AType: TP3DDataBlockType; const AParent: TP3DDataBlock): Integer;
var
  Nd: TDOMElement;
  Owner: TP3DLibrary;
begin
  if ( not Assigned( DOMFile )) then
    begin
      Result:= -1;
      exit;
    end;

  Nd:= P3DXMLFindPath( DOMFile.DocumentElement, APath );
  Owner:= GetOwnerFromPath( APath );
  if ( Assigned( Nd )) then
    Result:= Owner.AppendFromDOM( Nd, AType, AParent )
  else
    Result:= -1;
end;

function TP3DLibrary.AppendAllFromDom(ADOMNode: TDOMElement; AType: TP3DDataBlockType): Integer;
var
  filter: String;
  lst: TDOMNodeList;
  i: Integer;
begin
  filter:= FindDOMForClass( AType );
  lst:= ADOMNode.GetElementsByTagName( filter );
  try
    Result:= 0;
    for i:= 0 to lst.Count - 1 do
      begin
        if ( AppendFromDom( TDOMElement( lst[ i ])) > -1 ) then
          Inc( Result );
      end;

  finally
    lst.Free;
  end;
end;

function TP3DLibrary.AppendFile( AFileName: String ): String;
var
  dir, FName: String;
begin
  Result:= '';
  P3DLog.LogInfo( Self, Format( 'Loading file "%s"', [ AFileName ]));
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
    AppendAllFromDom( DOMFile.DocumentElement, TP3DScene );
  finally
    SetCurrentDir( dir );
  end;
  Result:= FName;
end;

function TP3DLibrary.CreateNew(Tp: TP3DDataBlockType; const CreateObject: Boolean ): Integer;
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

procedure TP3DLibrary.SaveToDOM(const ADOMNode: TDOMElement);
var
  Parent: TDOMElement;
begin
  //IF ExportScenesOnly
  if ( Assigned( ADOMNode )) then
    Parent:= ADOMNode
  else
    begin
      Parent:= DOMFile.CreateElement( 'p3dfile' );
      Parent.AttribStrings[ 'version' ]:= IntToStr( P3DFileVerMaj ) + '.' + IntToStr( P3DFileVerMin );
      DOMFile.AppendChild( Parent );
    end;

  Scenes.SaveToDOM( Parent );
end;

procedure TP3DLibrary.RemoveFromFields( Block: TP3DDataBlock );
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

procedure TP3DLibrary.ClearFields;
var
  User: TP3DDataBlock;
  Block: TP3DObject;
  i, j, idx: Integer;
begin
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

function TP3DLibrary.IsValid(Obj: TP3DObject): Boolean;
var
  Lib: TP3DData;
begin
  if ( not Assigned( DataBlocks )) then
    begin
      Result:= False;
      exit;
    end;
  Result:= DataBlocks.IndexOf( Obj ) >= 0;
end;

function TP3DLibrary.CheckLastChange: Integer;
begin
  Result:= DataBlocks.LastUpdate;
end;

{ TP3DDataList }

procedure TP3DLibraryList.DoChange;
begin
  inherited DoChange;
  FLastUpdate:= SDL_GetTicks;
end;

function TP3DLibraryList.FindByFileName(AFileName: String): Integer;
var
  i: Integer;
  FName: String;
begin
  Result:= -1;
  FName:= ExpandFileNameUTF8( AFileName );
  for i:= 0 to Count - 1 do
    if ( P3DMatchFileName( Items[ i ].FileWatch.FileName, FName )) then
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

{ TP3DData }

constructor TP3DData.Create;
begin
  inherited Create;
  Libraries:= TP3DLibraryList.Create;
end;

destructor TP3DData.Destroy;
begin
  inherited Destroy;
  FreeAndNil( FLibraries );
end;

procedure TP3DData.ClearFields;
var
  i: Integer;
begin
  for i:= Libraries.Count - 1 downto 0 do
    begin
      Libraries[ i ].Free;
      Libraries.Delete( i );
    end;

  inherited ClearFields;
end;

function TP3DData.IsValid(Obj: TP3DObject): Boolean;
var
  Lib: TP3DLibrary;
begin
  Result:= inherited IsValid(Obj);
  if ( not Result ) then
    for Lib in Libraries do
      begin
        Result:= Lib.IsValid( Obj );
        if ( Result ) then
          break;
      end;
end;

function TP3DData.OpenLibrary( FileName: String ): Integer;
var
  F: TP3DLibrary;
  FName: String;
begin
  FName:= P3DSearchPaths.Models.FindFileEx( FileName, GetCurrentDir );
  if ( FName = '' ) then
    raise Exception.Create( Format( 'Cannot load library! The specified file "%s" was not found!', [ FileName ]));
  Result:= Libraries.FindByFileName( FName );
  if ( Result = -1 ) then
    begin
      F:= TP3DLibrary.CreateFromFile( FName );
      Result:= Libraries.Add( F );
    end;
end;

function TP3DData.CheckLastChange: Integer;
var
  i: Integer;
begin
  Result:= inherited CheckLastChange;
  Result:= max( Result, Libraries.LastUpdate );
  for i:= 0 to Libraries.Count - 1  do;
    Result:= max( Result, Libraries[ i ].CheckLastChange );
end;

{$ENDIF}

