//p3dresource.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

const
  //Version 0.1
  P3DFileVerMaj = 0;
  P3DFileVerMin = 1;

type
  TP3DLibraryList = class;


  TP3DActionContainer   = specialize gP3DDatablockContainer < TP3DAction >;
  TP3DArmatureContainer = specialize gP3DDatablockContainer < TP3DArmature >;
  TP3DCameraContainer   = specialize gP3DDatablockContainer < TP3DCamera >;
  TP3DFontContainer     = specialize gP3DDatablockContainer < TP3DFontBmp >;
  TP3DGridContainer     = specialize gP3DDatablockContainer < TP3DTileGrid >;
  TP3DJointContainer    = specialize gP3DDatablockContainer < TP3DJoint >;
  TP3DLightContainer    = specialize gP3DDatablockContainer < TP3DLight >;
  TP3DMaterialContainer = specialize gP3DDatablockContainer < TP3DMaterial >;
  TP3DMeshContainer     = specialize gP3DDatablockContainer < TP3DMesh >;
  TP3DObjectContainer   = specialize gP3DDatablockContainer < TP3DObject >;
  TP3DSceneContainer    = specialize gP3DDatablockContainer < TP3DScene >;
  TP3DShaderContainer   = specialize gP3DDatablockContainer < TP3DShader >;
  TP3DTextureContainer  = specialize gP3DDatablockContainer < TP3DTexture >;

  TP3DPropertyAccessActionContainer   = specialize gP3DListPropertyAccessContainer < TP3DActionContainer, TP3DAction, TP3DPropertyAccessAction >;
  TP3DPropertyAccessArmatureContainer = specialize gP3DListPropertyAccessContainer < TP3DArmatureContainer, TP3DArmature, TP3DPropertyAccessArmature >;
  TP3DPropertyAccessCameraContainer   = specialize gP3DListPropertyAccessContainer < TP3DCameraContainer, TP3DCamera, TP3DPropertyAccessCamera >;
  TP3DPropertyAccessFontContainer     = specialize gP3DListPropertyAccessContainer < TP3DFontContainer, TP3DFontBmp, TP3DPropertyAccessFontBmp >;
  TP3DPropertyAccessGridContainer     = specialize gP3DListPropertyAccessContainer < TP3DGridContainer, TP3DTileGrid, TP3DPropertyAccessTileGrid >;
  TP3DPropertyAccessJointContainer    = specialize gP3DListPropertyAccessContainer < TP3DJointContainer, TP3DJoint, TP3DPropertyAccessJoint >;
  TP3DPropertyAccessLightContainer    = specialize gP3DListPropertyAccessContainer < TP3DLightContainer, TP3DLight, TP3DPropertyAccessLight >;
  TP3DPropertyAccessMaterialContainer = specialize gP3DListPropertyAccessContainer < TP3DMaterialContainer, TP3DMaterial, TP3DPropertyAccessMaterial >;
  TP3DPropertyAccessMeshContainer     = specialize gP3DListPropertyAccessContainer < TP3DMeshContainer, TP3DMesh, TP3DPropertyAccessMesh >;
  TP3DPropertyAccessObjectContainer   = specialize gP3DListPropertyAccessContainer < TP3DObjectContainer, TP3DObject, TP3DPropertyAccessObject >;
  TP3DPropertyAccessSceneContainer    = specialize gP3DListPropertyAccessContainer < TP3DSceneContainer, TP3DScene, TP3DPropertyAccessScene >;
  TP3DPropertyAccessShaderContainer   = specialize gP3DListPropertyAccessContainer < TP3DShaderContainer, TP3DShader, TP3DPropertyAccessShader >;
  TP3DPropertyAccessTextureContainer  = specialize gP3DListPropertyAccessContainer < TP3DTextureContainer, TP3DTexture, TP3DPropertyAccessTexture >;

  { TP3DLibrary }

  TP3DLibrary = class ( TP3DInterfacedPersistent )
    private
      FActions: TP3DActionContainer;
      FArmatures: TP3DArmatureContainer;
      FCameras: TP3DCameraContainer;
      FFonts: TP3DFontContainer;
      FGlobalTime: Single;
      FGrids: TP3DGridContainer;
      FJoints: TP3DJointContainer;
      FLights: TP3DLightContainer;
      FMaterials: TP3DMaterialContainer;
      FMeshes: TP3DMeshContainer;
      FObjects: TP3DObjectContainer;
      FScenes: TP3DSceneContainer;
      FShaders: TP3DShaderContainer;
      FTextures: TP3DTextureContainer;

      FFileWatch: TP3DFilePointer;
      FDOMFile: TXMLDocument;
      FIsDesignerPackage: Boolean;
      FBinFile: TFileStream;
      FLastUpdate: Integer;

      function GetBinFile: TFileStream;
      function GetDOMFile: TXMLDocument;
      function GetName: String; virtual;
      procedure PerformActions(GlobalTime: Float);
      procedure SetGlobalTime(AValue: Single);
//      function LoadDataBlockByPathNoCheck( LocalPath: String ): TP3DDataBlock;

    protected
      procedure OpenDOMFile;
      procedure CreateDOMFile;
      procedure CloseDOMFile;
      procedure SaveDOMFile( const FName: String = '' );
      procedure CreateBinFile( const AFileName: String = '' );
      procedure CloseBinFile;

    public
      constructor Create;
      constructor CreateFromDOM( ADOMNode: TDOMElement );
      constructor CreateFromFile( AFileName: String );
      destructor Destroy; override;

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
      procedure SaveToFile( const AFileName: String = '' );
      procedure SaveToFile( AParentClass: TP3DDataBlockType; const AFileName: String = '' );
      procedure RemoveFromFields( Block: TP3DDataBlock ); virtual;
      procedure ClearFields; virtual;
      //function IsValid( Obj: TP3DDataBlock ): Boolean; virtual;
      function CheckLastChange: Integer; virtual;
      class function DOMNodeName: String; override;

    published
      property Cameras: TP3DCameraContainer read FCameras write FCameras;
      property Grids: TP3DGridContainer read FGrids write FGrids;
      property Joints: TP3DJointContainer read FJoints write FJoints;
      property Lights: TP3DLightContainer read FLights write FLights;
      property Materials: TP3DMaterialContainer read FMaterials write FMaterials;
      property Meshes: TP3DMeshContainer read FMeshes write FMeshes;
      property Objects: TP3DObjectContainer read FObjects write FObjects;
      property Scenes: TP3DSceneContainer read FScenes write FScenes;
      property Shaders: TP3DShaderContainer read FShaders write FShaders;
      property Textures: TP3DTextureContainer read FTextures write FTextures;
      property Armatures: TP3DArmatureContainer read FArmatures write FArmatures;
      property Actions: TP3DActionContainer read FActions write FActions;
      property Fonts: TP3DFontContainer read FFonts write FFonts;

      property GlobalTime: Single read FGlobalTime write SetGlobalTime;

      property FileWatch: TP3DFilePointer read FFileWatch write FFileWatch;
      property DOMFile: TXMLDocument read GetDOMFile write FDOMFile;
      property BinFile: TFileStream read GetBinFile write FBinFile;
      property IsDesignerPackage: Boolean read FIsDesignerPackage write FIsDesignerPackage;
      property Name: String read GetName;
  end;

  TP3DData = class ( TP3DLibrary )
    private
      FLibraries: TP3DLibraryList;
      function GetName: String; override;

    public
      constructor Create;
      destructor Destroy; override;
//      procedure PerformActions(GlobalTime: Float); override;

      procedure ClearFields; override;
      //function IsValid(Obj: TP3DDataBlock ): Boolean; override;
      function OpenLibrary( FileName: String ): Integer;
      function CheckLastChange: Integer; override;

    published
      property Libraries: TP3DLibraryList read FLibraries write FLibraries;
  end;

  { TP3DLibraryList }

  TP3DLibraryList = class ( specialize gP3DCustomObjectList < TP3DLibrary >)
    public
      procedure LoadFromStream( S: TStream ); //gP3DListPropertyAccessObject needs this to compile
      procedure SaveToStream( S: TStream );

      function FindByFileName( AFileName: String ): Integer;
      property LastUpdate: Integer read FLastUpdate;
  end;

  TP3DPropertyAccessLibrary = specialize gP3DPropertyAccessInterfacedPersistent < TP3DLibrary >;
  TP3DPropertyAccessLibraryList = specialize gP3DListPropertyAccessObject < TP3DLibraryList, TP3DLibrary, TP3DPropertyAccessLibrary >;


  { TP3DDataBlockCacheEntry }

  TP3DDataBlockCacheEntry = class ( TPersistent )
    private
      FBlockType: TP3DDataBlockType;
      FDatablock: TP3DDataBlock;
      FName: String;
      FOwner: TP3DLibrary;
      FStackTrace: String;

    published
      property Name: String read FName write FName;
      property BlockType: TP3DDataBlockType read FBlockType write FBlockType;
      property Owner: TP3DLibrary read FOwner write FOwner;
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
      function IsValid( Obj: TP3DDataBlock ): Boolean;
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

function TP3DLibrary.GetBinFile: TFileStream;
begin
  if ( not Assigned( FBinFile )) then
    CreateBinFile;
  Result:= FBinFile;
end;

constructor TP3DLibrary.Create;
var
  fp: TP3DPropertyAccessFilePointer;
begin
  inherited;

  Cameras:= TP3DCameraContainer.Create( Self );
  Grids:= TP3DGridContainer.Create( Self );
  Joints:= TP3DJointContainer.Create( Self );
  Lights:= TP3DLightContainer.Create( Self );
  Materials:= TP3DMaterialContainer.Create( Self );
  Meshes:= TP3DMeshContainer.Create( Self );
  Objects:= TP3DObjectContainer.Create( Self );
  Scenes:= TP3DSceneContainer.Create( Self );
  Shaders:= TP3DShaderContainer.Create( Self );
  Textures:= TP3DTextureContainer.Create( Self );
  Armatures:= TP3DArmatureContainer.Create( Self );
  Actions:= TP3DActionContainer.Create( Self );
  Fonts:= TP3DFontContainer.Create( Self );
  FileWatch:= TP3DFilePointer.Create( '', Self );

  fp:= TP3DPropertyAccessFilePointer.CreateField( 'FileWatch', @FFileWatch, smAttribute );
  fp.PathMode:= pmRelativeToBaseBath;

  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Name', smDoNotStore ));
  Properties.Add( fp );

  Properties.Add( TP3DPropertyAccessCameraContainer.CreateField( 'Cameras', @FCameras, smAttribute ));
  Properties.Add( TP3DPropertyAccessJointContainer.CreateField( 'Joints', @FJoints, smAttribute ));
  Properties.Add( TP3DPropertyAccessLightContainer.CreateField( 'Lights', @FLights, smAttribute ));
  Properties.Add( TP3DPropertyAccessMaterialContainer.CreateField( 'Materials', @FMaterials, smAttribute ));
  Properties.Add( TP3DPropertyAccessMeshContainer.CreateField( 'Meshes', @FMeshes, smAttribute ));
  Properties.Add( TP3DPropertyAccessObjectContainer.CreateField( 'Objects', @FObjects, smAttribute ));
  Properties.Add( TP3DPropertyAccessSceneContainer.CreateField( 'Scenes', @FScenes, smAttribute ));
  Properties.Add( TP3DPropertyAccessShaderContainer.CreateField( 'Shaders', @FShaders, smAttribute ));
  Properties.Add( TP3DPropertyAccessTextureContainer.CreateField( 'Textures', @FTextures, smAttribute ));
  Properties.Add( TP3DPropertyAccessArmatureContainer.CreateField( 'Armatures', @FArmatures, smAttribute ));
  Properties.Add( TP3DPropertyAccessActionContainer.CreateField( 'Actions', @FActions, smAttribute ));
  Properties.Add( TP3DPropertyAccessFontContainer.CreateField( 'Fonts', @FFonts, smAttribute ));
  if ( not ( Self is TP3DData )) then
    P3DData.Libraries.Add( Self );
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
  FreeAndNil( FGrids );
  FreeAndNil( FJoints );
  FreeAndNil( FLights );
  FreeAndNil( FMaterials );
  FreeAndNil( FMeshes );
  FreeAndNil( FObjects );
  FreeAndNil( FScenes );
  FreeAndNil( FShaders );
  FreeAndNil( FTextures );
  FreeAndNil( FArmatures );
  FreeAndNil( FActions );
  FreeAndNil( FFonts );
  FreeAndNil( FFileWatch );

  if ( Self <> P3DData ) then
    P3DData.Libraries.Remove( Self );

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
  DOMFile.AppendChild( DOMFile.CreateElement( 'p3dscene' ));
  DOMFile.DocumentElement.AttribStrings[ 'version' ]:= IntToStr( P3DFileVerMaj ) + '.' + IntToStr( P3DFileVerMin );
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

procedure TP3DLibrary.CreateBinFile(const AFileName: String);
var
  FName: String;
begin
  if ( AFileName > '' ) then
    FName:= AFileName
  else
    FName:= ChangeFileExt( FileWatch.FileName, '.p3dbin' );

  if ( Assigned( FBinFile )) then
    FreeAndNil( FBinFile );

  BinFile:= TFileStream.Create( FName, fmCreate );
end;

procedure TP3DLibrary.CloseBinFile;
var
  FName: String;
  Del: Boolean;
begin
  if ( Assigned( FBinFile )) then
    begin
      FName:= FBinFile.FileName;
      Del:= FBinFile.Size = 0;
      FreeAndNil( FBinFile );
      if ( Del ) then
        DeleteFileUTF8( FName );
    end;
  FreeAndNil( FBinFile );
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


{function TP3DLibrary.FindDataBlockByPath(DOMPath: String): TP3DDataBlock;
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

  if ( ADOMNode.NodeName = TP3DObject.DOMNodeName ) then
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
      if ( ADOMNode.NodeName = TP3DObject.DOMNodeName ) then
        begin
          if ( AParent is TP3DScene ) then
            Result:= TP3DScene( AParent ).Objects.Add( TP3DObject.CreateFromDOM( ADOMNode, Self ))
          else if ( AParent is TP3DObject ) then
            Result:= TP3DObject( AParent ).Children.Add( TP3DObject.CreateFromDOM( ADOMNode, Self ))
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
  tp: TP3DInterfacedPersistentType;
begin
  ObjName:= ADOMNode.AttribStrings[ 'Name' ];
  tp:= P3DClassFactory.FindByDOMNodeName( ADOMNode.NodeName );
  if ( tp.InheritsFrom( AType )) then
    raise Exception.Create( Format( 'Error while appending from DOM: The DOM with name %s is of the wrong type. Expected "%s", Found "%s"', [ ObjName, AType.ClassName, tp.ClassName ]));
  Result:= AType.GetContainer( Self ).FindByName( ObjName );
  if ( Result > -1 ) then
    begin
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
  tp: TP3DInterfacedPersistentType;

  function LoadData(): Integer;
    function CreateObject: Integer;
    begin
      Result:= -1;

      if ( ADOMNode.NodeName = 'object' ) then
        raise Exception.Create( 'TP3DLibrary.AppendFromDOM - Error: An object must have a parent.' )
      else
        tp:= P3DClassFactory.FindByDOMNodeName( ADOMNode.NodeName );
      if (( not Assigned( tp )) or ( not ( tp.InheritsFrom( TP3DDataBlock )))) then
        P3DLog.LogInfo( Self, 'Warning: Cannot append from dom. Unsupported node type "' + ADOMNode.NodeName + '". Skipping.' )
      else
        Result:= CreateNew( TP3DDataBlockType( tp ), False );
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
      TP3DDataBlockType( tp ).GetContainer( Self )[ Result ].LoadFromDOMNew( ADOMNode )
//    else
//      TP3DDataBlockType( tp ).GetContainer( Self ).LoadFromDOM( ADOMNode );
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
      tp:= P3DClassFactory.FindByDOMNodeName( ADOMNode.NodeName );
      if ( tp.InheritsFrom( TP3DDataBlock )) then
        Result:= TP3DDataBlockType( tp ).GetContainer( Self ).FindByName( ObjName )
      else
        Result:= -1;
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
  filter:= AType.DOMNodeName;
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
  Loader: TP3DJSONLoader;
begin
  Result:= '';
  P3DLog.LogInfo( Self, Format( 'Loading file "%s"', [ AFileName ]));
  FName:= P3DSearchPaths.FindFileEx( AFileName, GetCurrentDir );
  if ( FName = '' ) then
    raise Exception.Create( Format( 'Cannot load library! The specified file "%s" was not found!', [ AFileName ]));
  FName:= P3DSearchPaths.FindFile( AFileName );
  if ( not FileExists( FName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );

  FileWatch.FileName:= AFileName;

  Loader:= TP3DJSONLoader.Create( AFileName );
  Loader.Root:= Self;
  LoadFromJSONContext( P3DJSONContext( Loader, Loader.JSON, Self, nil ));
  Loader.Free;
  {OpenDOMFile;
  dir:= GetCurrentDir;
  try
    DOMFile.DocumentElement.SetAttribute( 'filename', AFileName );
    SetCurrentDir( ExtractFilePath( FName ));
    AppendAllFromDom( DOMFile.DocumentElement, TP3DScene );
  finally
    SetCurrentDir( dir );
  end;}

  Result:= FName;
end;

function TP3DLibrary.CreateNew(Tp: TP3DDataBlockType; const CreateObject: Boolean ): Integer;
var
  data: TP3DDataBlock;
  obj: TP3DObject;
begin
  data:= Tp.Create( Self );

  if ( CreateObject ) then
    begin
      obj:= TP3DObject.Create( Self );
      obj.data:= data;
      Result:= Objects.IndexOf( obj );
    end
  else
    Result:= Tp.GetContainer( Self ).IndexOf( data );
end;

procedure TP3DLibrary.SaveToDOM(const ADOMNode: TDOMElement);
var
  Parent: TDOMElement;
begin
  if ( Assigned( ADOMNode )) then
    Parent:= ADOMNode
  else
    Parent:= DOMFile.DocumentElement;

  //Scenes.SaveToDOM( Parent );
end;

procedure TP3DLibrary.SaveToFile(const AFileName: String);
var
  json: TJSONData;
  S: String;
  F: TStringListUTF8;
  FileName: String;
begin
  if ( AFileName > '' ) then
    FileName:= AFileName
  else
    FileName:= FileWatch.FileName;
  S:= Serialize;
  WriteLn( S );
  json:= GetJSON( S );
  S:= json.FormatJSON();
  DeleteFile( FileName );
  {F:= TFileStream.Create( FileName, fmCreate );
  json.DumpJSON( F );
  F.Free;}
  {AssignFile( F, FileName );
  Rewrite( F );
  Write( F, S );
  CloseFile( F );}
  F:= TStringListUTF8.Create;
  F.Text:= S;
  F.SaveToFile( FileName );
  F.Free;
  json.Free;
  //SaveToFile( TP3DScene, AFileName );
end;

procedure TP3DLibrary.SaveToFile( AParentClass: TP3DDataBlockType; const AFileName: String );
var
  FileName: String;
begin
  if ( AFileName > '' ) then
    FileName:= AFileName
  else
    FileName:= FileWatch.FileName;

  CreateBinFile( ChangeFileExt( FileName, '.p3dbin' ));

  if ( Assigned( FDOMFile )) then
    FreeAndNil( FDOMFile );
  CreateDOMFile;

  {if ( AParentClass = TP3DObject ) then Objects.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DArmature ) then Armatures.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DAction ) then Actions.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DCamera ) then Cameras.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DFontBmp ) then Fonts.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DJoint ) then Joints.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DLight ) then Lights.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DMaterial ) then Materials.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DMesh ) then Meshes.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DScene ) then Scenes.SaveToDOM( DOMFile.DocumentElement )
  else if ( AParentClass = TP3DShader ) then Shaders.SaveToDOM( DOMFile.DocumentElement );}

  SaveDOMFile( FileName );
  CloseBinFile;
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
  else if ( Block is TP3DObject and Assigned( Objects )) then
    Objects.Remove( TP3DObject( Block ))
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
  Block: TP3DNamedInterfacedPersistent;
  i, j, idx: Integer;

begin
{  for i:= Datablocks.Count - 1 downto 0 do
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
    end;}
  Cameras.Clear();
  Joints.Clear();
  Lights.Clear();
  Materials.Clear();
  Meshes.Clear();
  Objects.Clear();
  Scenes.Clear();
  Shaders.Clear();
  Textures.Clear();
  Actions.Clear();
  Armatures.Clear();
  Fonts.Clear();
end;

{
function TP3DLibrary.IsValid(Obj: TP3DDataBlock): Boolean;
var
  Lib: TP3DData;
  n: Integer;
  item: TP3DDataBlockCacheEntry;
begin
  Result:= False;
{  if ( not Assigned( DataBlocks )) then
    begin
      Result:= False;
      exit;
    end;}
  n:= P3DDataBlockCache.FindBlock( Obj );
  if ( n > -1 ) then
    begin
      item:= P3DDataBlockCache[ n ];
      if ( item.Owner = Self ) then
        Result:= item.BlockType.GetContainer( Self ).IndexOf( Obj ) >= 0;
    end
end;
}

function TP3DLibrary.CheckLastChange: Integer;
begin
  Result:= FLastUpdate;
end;

class function TP3DLibrary.DOMNodeName: String;
begin
  Result:= 'p3dfile';
end;

procedure TP3DLibrary.PerformActions(GlobalTime: Float);
begin
  //Actions.PerformActions( GlobalTime );
end;

procedure TP3DLibrary.SetGlobalTime(AValue: Single);
var
  Action: TP3DAction;
  User: TP3DDataBlock;
begin
  if FGlobalTime=AValue then Exit;
  FGlobalTime:=AValue;
  for Action in Actions do
    for User in Action.Users do ;
      Action.Perform( GlobalTime, User );
end;

{ TP3DDataList }

procedure TP3DLibraryList.LoadFromStream(S: TStream);
begin

end;

procedure TP3DLibraryList.SaveToStream(S: TStream);
begin

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
  n: Integer;
begin
  entry:= TP3DDataBlockCacheEntry.Create;
  entry.Datablock:= Block;
  entry.Name:= Block.Name;
  entry.BlockType:= TP3DDataBlockType( Block.ClassType );
  entry.Owner:= Block.Owner;
  //entry.StackTrace:= DumpCallStack;

  n:= FindBlock( Block );
  if ( n > -1 ) then
    begin
      Items[ n ].Free;
      Items[ n ]:= entry;
    end
  else
    inherited Add( entry );
end;

destructor TP3DDataBlockCache.Destroy;
begin
  Clear( True );
  inherited Destroy;
end;

function TP3DDataBlockCache.IsValid(Obj: TP3DDataBlock): Boolean;
var
  n: Integer;
  item: TP3DDataBlockCacheEntry;
  Lib: TP3DLibrary;
begin
  n:= FindBlock( Obj );
  if ( n > -1 ) then
    begin
      item:= Items[ n ];
      if ( not Assigned( item.Owner )) then
        exit( False );
      {if ( Obj.Owner = P3DData ) then
        Lib:= P3DData
      else
        Lib:= P3DData.Libraries[ P3DData.Libraries.IndexOf( item.Owner )];}
      Lib:= item.Owner;
      if ( Assigned( Lib )) then
        begin
          //WriteLn( item.BlockType.GetContainer( Lib ).DumpObjectList );
          Result:= item.BlockType.GetContainer( Lib ).IndexOf( item.Datablock ) > -1
        end
      else
        exit( False );
    end
  else
    exit( False );
end;

{ TP3DData }

function TP3DData.GetName: String;
begin
  Result:= 'P3DData';
end;

constructor TP3DData.Create;
begin
  inherited Create;
  Libraries:= TP3DLibraryList.Create;
  Properties.Add( TP3DPropertyAccessLibraryList.CreateFieldSet( 'Libraries', @FLibraries, Nil, smDoNotStore ));
end;

destructor TP3DData.Destroy;
begin
  inherited Destroy;
  FreeAndNil( FLibraries );
end;
{
procedure TP3DData.PerformActions(GlobalTime: Float);
var
  lib: TP3DLibrary;
begin
  for lib in Libraries do
    lib.PerformActions( GlobalTime );
  inherited PerformActions(GlobalTime);
end;
}

procedure TP3DData.ClearFields;
var
  i: Integer;
begin
  for i:= Libraries.Count - 1 downto 0 do
    begin
      Libraries[ i ].Free;
      //Libraries.Delete( i );
    end;

  inherited ClearFields;
end;

{function TP3DData.IsValid(Obj: TP3DDataBlock ): Boolean;
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
end;}

function TP3DData.OpenLibrary( FileName: String ): Integer;
var
  F: TP3DLibrary;
  FName: String;
begin
  FName:= P3DSearchPaths.FindFileEx( FileName, GetCurrentDir );
  if ( FName = '' ) then
    raise Exception.Create( Format( 'Cannot load library! The specified file "%s" was not found!', [ FileName ]));
  Result:= Libraries.FindByFileName( FName );
  if ( Result = -1 ) then
    begin
      F:= TP3DLibrary.CreateFromFile( FName );
      Result:= Libraries.Count - 1;
      //Result:= Libraries.Add( F );
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

{$IFDEF INITIALIZATION}
  if ( not Assigned( P3DDataBlockCache )) then
    P3DDataBlockCache:= TP3DDataBlockCache.Create;
  if ( not Assigned( P3DData )) then
    P3DData:= TP3DData.Create;
  if ( not Assigned( P3DClassFactory )) then
    begin
      P3DClassFactory:= TP3DClassFactory.Create;
      P3DClassFactory.AddArray([ TP3DAction, TP3DObject,
                                 TP3DArmature, TP3DJoint,
                                 TP3DCamera, TP3DTileGrid, TP3DLight,
                                 TP3DMaterialBase, TP3DMaterialShader,
                                 TP3DMesh, TP3DScene, TP3DTexture ]);
    end;
{$ENDIF}

{$IFDEF FINALIZATION}
  if ( Assigned( P3DDataBlockCache )) then
    FreeAndNil( P3DDataBlockCache );
  if ( Assigned( P3DClassFactory )) then
    FreeAndNil( P3DClassFactory );
  if ( Assigned( P3DData )) then
    begin
      P3DData.Free;
      P3DData:= nil;
    end;
{$ENDIF}
