//p3dresource.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}
type
  { TP3DResourceManager }

  TP3DDataList = class;
  TP3DData = class ( TPersistent )
    private
      FCameras: TP3DCameraList;
      FDataBlocks: TP3DObjectList;
      FFileWatch: TP3DFileWatch;
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
      constructor CreateFromDom( ADOMNode: TDOMElement );
      constructor CreateFromFile( AFileName: String );
      destructor Destroy; override;

      procedure AppendFromDom( ADOMNode: TDOMElement;
        const AParent: TP3DActor = nil );
      function AppendFile( AFileName: String; const AParent: TP3DActor = nil ): TP3DData;
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
      property FileWatch: TP3DFileWatch read FFileWatch write FFileWatch;
  end;

  TP3DCustomDataList = specialize gP3DCustomObjectList < TP3DData >;

  { TP3DDataList }

  TP3DDataList = class ( TP3DCustomDataList )
    function FindByFileName( AFileName: String ): Integer;
  end; //Workaround because you cannot forward declare generics

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
  FileWatch:= TP3DFileWatch.Create( '', Self );
end;

constructor TP3DData.CreateFromDom(ADOMNode: TDOMElement);
begin
  Create;
  AppendFromDom( ADOMNode );
end;

constructor TP3DData.CreateFromFile(AFileName: String);
begin
  Create;
  AppendFile( AFileName );
  FileWatch.FileName:= AFileName;
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
  FileWatch.Free;

  DataBlocks.Free;

  inherited Destroy;
end;

procedure TP3DData.AppendFromDom(ADOMNode: TDOMElement; const AParent: TP3DActor
  );
var
  Element: TDOMElement;
  Parent: TP3DActorList;
  lst: TDOMNodeList;
  i: Integer;

  procedure LoadData();
    function CreateObject: Integer;
    var
      DB: TP3DDataBlock;
    begin
      Result:= -1;
      case Element.NodeName of
        'mesh': begin DB:= TP3DMesh.Create( Self ); Meshes.Add( TP3DMesh( DB )); end;
        'material': begin DB:= TP3DMaterial.Create( Self ); Materials.Add( TP3DMaterial( DB )); end;
        'light': begin DB:= TP3DLight.Create( Self ); Lights.Add( TP3DLight( DB )); end;
        'camera': begin DB:= TP3DCamera.Create( Self ); Cameras.Add( TP3DCamera( DB )); end;
      end;
      Result:= DataBlocks.IndexOf( DB );
    end;

  var
    AName: DOMString;
    Idx: Integer;
  begin
    AName:= Element.GetAttribute( 'name' );
    Idx:= DataBlocks.FindByName( AName );

    if ( Idx = -1 ) then
      Idx:= CreateObject;
    if ( TP3DDataBlock( DataBlocks[ Idx ]).FileWatch.FileName = '<empty>' ) then
      TP3DDataBlock( DataBlocks[ Idx ]).LoadFromDOM( Element );
    {$IFDEF VERBOSE}
    WriteLn( 'IDX of Datablock "', AName, '": ', Idx );
    WriteLn( Format( 'Created datablock of type "%s" and name "%s" with index %d', [ Element.NodeName, AName, Idx ]));
    {$ENDIF}
  end;

begin
  if ( not Assigned( ADOMNode )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Not Assigned!' );

  if ( not ( ADOMNode.NodeName = 'p3dscene' )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Scenes must begin with p3dscene!' );

  Element:= TDOMElement( ADOMNode.FirstChild );
  if ( Assigned( AParent )) then
    Parent:= AParent.Children
  else
    Parent:= Objects;

  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'object': ; //Postpone the loading of objects
        'mesh',
        'material',
        'camera',
        'light': LoadData()
      else
        P3DLog.LogInfo( Self, Format( 'Warning unsupported type "%s". Skipping.', [ Element.NodeName ]));
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  lst:= ADOMNode.GetElementsByTagName( 'object' );
  for i:= 0 to lst.Count - 1 do
    Parent.Add( TP3DActor.CreateFromDOM( TDOMElement( lst[ i ]), Self ));
end;

function TP3DData.AppendFile(AFileName: String; const AParent: TP3DActor
  ): TP3DData;
var
  xml: TXMLDocument;
  dir: String;
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  ReadXMLFile( xml, AFileName );
  dir:= GetCurrentDir;
  try
    SetCurrentDir( ExtractFilePath( AFileName ));
    AppendFromDom( xml.DocumentElement, AParent );
  finally
    SetCurrentDir( dir );
    xml.Free;
  end;
end;

procedure TP3DData.NotifyDeleteOfData(Sender: TP3DDataBlock);
begin

end;

function OpenLibrary(FileName: String): Integer;
var
  F: TP3DData;
begin
  Result:= P3DData.Libraries.FindByFileName( FileName );
  if ( Result = -1 ) then
    begin
      F:= TP3DData.CreateFromFile( FileName );
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
    if ( Items[ i ].FileWatch.FileName = ExpandFileName( AFileName )) then
      begin
        Result:= i;
        break;
      end;
end;

{$ENDIF}

