//p3dresource.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}
type
  { TP3DResourceManager }

  TP3DDataList = class;
  TP3DData = class ( TPersistent )
    private
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
      procedure NotifyDeleteOfData( Sender: TP3DDataBlock ); virtual;

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
      property FileWatch: TP3DFileWatch read FFileWatch write FFileWatch;
      property DOMFile: TXMLDocument read FDOMFile write FDOMFile;
      property IsDesignerPackage: Boolean read FIsDesignerPackage write FIsDesignerPackage;
  end;

  TP3DCustomDataList = specialize gP3DCustomObjectList < TP3DData >;

  { TP3DDataList }

  TP3DDataList = class ( TP3DCustomDataList )
    function FindByFileName( AFileName: String ): Integer;
  end;

  function OpenLibrary( FileName: String ): Integer; //TODO: Move to libraries

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DData }

constructor TP3DData.Create;
begin
  inherited;

  DataBlocks:= TP3DObjectList.Create;

  Cameras:= TP3DCameraList.Create;
  Lights:= TP3DLightList.Create;
  Materials:= TP3DMaterialList.Create;
  Meshes:= TP3DMeshList.Create;
  Objects:= TP3DActorList.Create;
  Libraries:= TP3DDataList.Create;
  Scenes:= TP3DSceneList.Create;
  Shaders:= TP3DShaderList.Create;
  Textures:= TP3DTextureList.Create;
  Armatures:= TP3DArmatureList.Create;
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
  Cameras.Free;
  Lights.Free;
  Materials.Free;
  Meshes.Free;
  Objects.Free;
  Libraries.Clear( True );
  Libraries.Free;
  Scenes.Free;
  Shaders.Free;
  Textures.Free;
  Armatures.Free;
  FileWatch.Free;

  DataBlocks.Free;

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
      DB: TP3DDataBlock;
    begin
      Result:= -1;

      case ADOMNode.NodeName of
        'mesh':
          begin
            DB:= TP3DMesh.Create( Self );
            Meshes.Add( TP3DMesh( DB ));
          end;
        'material':
          begin
            case ADOMNode.GetAttribute( 'type' ) of
              'shader': DB:= TP3DMaterialShader.Create( Self );
              else
                DB:= TP3DMaterialBase.Create( Self );
            end;
            Materials.Add( TP3DMaterialShader( DB ));
          end;
        'light', 'lamp':
          begin
            DB:= TP3DLight.Create( Self );
            Lights.Add( TP3DLight( DB ));
          end;
        'camera':
          begin
            DB:= TP3DCamera.Create( Self );
            Cameras.Add( TP3DCamera( DB ));
          end;
        'scene':
          begin
            DB:= TP3DScene.Create( Self );
            Scenes.Add( TP3DScene( DB ));
          end;
        'armature':
          begin
            DB:= TP3DArmature.Create( Self );
            Armatures.Add( TP3DArmature( DB ));
          end;
      end;
      Result:= DataBlocks.IndexOf( DB );
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
    WriteLn( 'Index of Datablock "', AName, '": ', Result );
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
      'armature': Result:= LoadData();
    'texture':
        Result:= DataBlocks.IndexOf( Textures[ Textures.Add( TP3DTexture.CreateFromDOM( ADOMNode, Self ))])
    else
      P3DLog.LogInfo( Self, 'Warning: Cannot append from dom. Unsupported node type "' + ADOMNode.NodeName + '". Skipping.' );
  end;
end;

function TP3DData.AppendFromDom(ADOMNode: TDOMElement;
  AName: String; AType: TP3DDataBlockType; const AParent: TP3DActor): Integer;
var
  Element: TDOMElement;
begin
  Result:= DataBlocks.FindByName( AName );
  if ( Result > -1 ) then
    if ( not ( DataBlocks[ Result ] is AType )) then
      raise Exception.Create( Format( 'Error while appending from DOM: A class with name %s was already loaded before but is of the wrong type. Expected "%s", Found "%s"', [ AName, AType.ClassName, FindClassForDOM( Element ).ClassName ]))
    else
      exit;
  Element:= TDOMElement( ADOMNode.FirstChild );
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

procedure TP3DData.NotifyDeleteOfData(Sender: TP3DDataBlock);
begin
  if ( Sender is TP3DCamera ) then
    Cameras.Remove( TP3DCamera( Sender ))
  else if ( Sender is TP3DLight ) then
    Lights.Remove( TP3DLight( Sender ))
  else if ( Sender is TP3DMaterialShader ) then
    Materials.Remove( TP3DMaterialShader( Sender ))
  else if ( Sender is TP3DMesh ) then
    Meshes.Remove( TP3DMesh( Sender ))
  else if ( Sender is TP3DActor ) then
    Objects.Remove( TP3DActor( Sender ))
  else if ( Sender is TP3DScene ) then
    Scenes.Remove( TP3DScene( Sender ))
  else if ( Sender is TP3DShader ) then
    Shaders.Remove( TP3DShader( Sender ))
  else if ( Sender is TP3DTexture ) then
    Textures.Remove( TP3DTexture( Sender ))
  else if ( Sender is TP3DArmature ) then
    Armatures.Remove( TP3DArmature( Sender ));
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

{$ENDIF}

