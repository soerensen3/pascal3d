//p3dresource.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

const
  //Version 0.1
  P3DFileVerMaj = 0;
  P3DFileVerMin = 1;

type
  {
  TP3DActionContainer   = specialize gP3DDatablockContainer < TP3DAction >;
  TP3DArmatureContainer = specialize gP3DDatablockContainer < TP3DArmature >;
  TP3DCameraContainer   = specialize gP3DDatablockContainer < TP3DCamera >;
  TP3DFontContainer     = specialize gP3DDatablockContainer < TP3DFontBmp >;
  TP3DGridContainer     = specialize gP3DDatablockContainer < TP3DTileGrid >;
  //TP3DJointContainer    = specialize gP3DDatablockContainer < TP3DJoint >;
  TP3DLightContainer    = specialize gP3DDatablockContainer < TP3DLight >;
  TP3DMaterialContainer = specialize gP3DDatablockContainer < TP3DMaterial >;
  TP3DMeshContainer     = specialize gP3DDatablockContainer < TP3DMesh >;
  TP3DObjectContainer   = specialize gP3DDatablockContainer < TP3DObject >;
  TP3DSceneContainer    = specialize gP3DDatablockContainer < TP3DScene >;
  TP3DShaderContainer   = specialize gP3DDatablockContainer < TP3DShader >;
  TP3DTextureContainer  = specialize gP3DDatablockContainer < TP3DTexture >;

  TP3DPropertyAccessActionContainer   = specialize gP3DListPropertyAccessDataBlockContainer < TP3DActionContainer, TP3DAction, TP3DPropertyAccessAction >;
  TP3DPropertyAccessArmatureContainer = specialize gP3DListPropertyAccessDataBlockContainer < TP3DArmatureContainer, TP3DArmature, TP3DPropertyAccessArmature >;
  TP3DPropertyAccessCameraContainer   = specialize gP3DListPropertyAccessDataBlockContainer < TP3DCameraContainer, TP3DCamera, TP3DPropertyAccessCamera >;
  TP3DPropertyAccessFontContainer     = specialize gP3DListPropertyAccessDataBlockContainer < TP3DFontContainer, TP3DFontBmp, TP3DPropertyAccessFontBmp >;
  TP3DPropertyAccessGridContainer     = specialize gP3DListPropertyAccessDataBlockContainer < TP3DGridContainer, TP3DTileGrid, TP3DPropertyAccessTileGrid >;
  //TP3DPropertyAccessJointContainer    = specialize gP3DListPropertyAccessDataBlockContainer < TP3DJointContainer, TP3DJoint, TP3DPropertyAccessJoint >;
  TP3DPropertyAccessLightContainer    = specialize gP3DListPropertyAccessDataBlockContainer < TP3DLightContainer, TP3DLight, TP3DPropertyAccessLight >;
  TP3DPropertyAccessMaterialContainer = specialize gP3DListPropertyAccessDataBlockContainer < TP3DMaterialContainer, TP3DMaterial, TP3DPropertyAccessMaterial >;
  TP3DPropertyAccessMeshContainer     = specialize gP3DListPropertyAccessDataBlockContainer < TP3DMeshContainer, TP3DMesh, TP3DPropertyAccessMesh >;
  TP3DPropertyAccessObjectContainer   = specialize gP3DListPropertyAccessDataBlockContainer < TP3DObjectContainer, TP3DObject, TP3DPropertyAccessObject >;
  TP3DPropertyAccessSceneContainer    = specialize gP3DListPropertyAccessDataBlockContainer < TP3DSceneContainer, TP3DScene, TP3DPropertyAccessScene >;
  TP3DPropertyAccessShaderContainer   = specialize gP3DListPropertyAccessDataBlockContainer < TP3DShaderContainer, TP3DShader, TP3DPropertyAccessShader >;
  TP3DPropertyAccessTextureContainer  = specialize gP3DListPropertyAccessDataBlockContainer < TP3DTextureContainer, TP3DTexture, TP3DPropertyAccessTexture >;
  }

  { TP3DLibrary }

  TP3DLibraryHandler = (
    lhAddAction,
    lhRemoveAction,
    lhChangeAction,
    lhAddArmature,
    lhRemoveArmature,
    lhChangeArmature,
    lhAddCamera,
    lhRemoveCamera,
    lhChangeCamera,
    lhAddEffect,
    lhRemoveEffect,
    lhChangeEffect,
    lhAddFont,
    lhRemoveFont,
    lhChangeFont,
    lhAddGrid,
    lhRemoveGrid,
    lhChangeGrid,
    lhAddLight,
    lhRemoveLight,
    lhChangeLight,
    lhAddMaterial,
    lhRemoveMaterial,
    lhChangeMaterial,
    lhAddMesh,
    lhRemoveMesh,
    lhChangeMesh,
    lhAddObject,
    lhRemoveObject,
    lhChangeObject,
    lhAddScene,
    lhRemoveScene,
    lhChangeScene,
    lhAddTexture,
    lhRemoveTexture,
    lhChangeTexture,
    lhAddDatablock,
    lhRemoveDatablock,
    lhChangeDatablock,
    lhAddLibrary,
    lhRemoveLibrary,
    lhChangeLibrary
  );


  TP3DDatablockHandler = procedure ( ASender: TP3DDataBlock ) of object;
  TP3DStreamableHandler = procedure ( ASender: TP3DDataBlock ) of object;

  TP3DLibrary = class ( TP3DJSONRootNamed, IP3DJSONStream, IP3DJSONStreamBinary )
    //LibHandlers --->
    protected
      FHandlers: array[ TP3DLibraryHandler ] of TMethodList;

      procedure AddHandler( HandlerType: TP3DLibraryHandler;
                            const AMethod: TMethod; AsLast: Boolean = False );
      procedure RemoveHandler( HandlerType: TP3DLibraryHandler;
                               const AMethod: TMethod );

    public
      procedure AddDatablockHandler( HandlerType: TP3DLibraryHandler;
                            const AMethod: TP3DDatablockHandler; AsLast: Boolean = False );
      procedure RemoveDatablockHandler( HandlerType: TP3DLibraryHandler;
                               const AMethod: TP3DDatablockHandler );

    protected
      procedure DoCallDatablockHandlers( HandlerType: TP3DLibraryHandler; Block: TP3DDataBlock );

      procedure AddAction( ASender: TP3DDatablock );
      procedure RemoveAction( ASender: TP3DDatablock );
      procedure ChangeAction( ASender: TP3DDatablock );
      procedure AddArmature( ASender: TP3DDatablock );
      procedure RemoveArmature( ASender: TP3DDatablock );
      procedure ChangeArmature( ASender: TP3DDatablock );
      procedure AddCamera( ASender: TP3DDatablock );
      procedure RemoveCamera( ASender: TP3DDatablock );
      procedure ChangeCamera( ASender: TP3DDatablock );
      procedure AddEffect( ASender: TP3DDatablock );
      procedure RemoveEffect( ASender: TP3DDatablock );
      procedure ChangeEffect( ASender: TP3DDatablock );
      procedure AddFont( ASender: TP3DDatablock );
      procedure RemoveFont( ASender: TP3DDatablock );
      procedure ChangeFont( ASender: TP3DDatablock );
      procedure AddGrid( ASender: TP3DDatablock );
      procedure RemoveGrid( ASender: TP3DDatablock );
      procedure ChangeGrid( ASender: TP3DDatablock );
      procedure AddLight( ASender: TP3DDatablock );
      procedure RemoveLight( ASender: TP3DDatablock );
      procedure ChangeLight( ASender: TP3DDatablock );
      procedure AddMaterial( ASender: TP3DDatablock );
      procedure RemoveMaterial( ASender: TP3DDatablock );
      procedure ChangeMaterial( ASender: TP3DDatablock );
      procedure AddMesh( ASender: TP3DDatablock );
      procedure RemoveMesh( ASender: TP3DDatablock );
      procedure ChangeMesh( ASender: TP3DDatablock );
      procedure AddObject( ASender: TP3DDatablock );
      procedure RemoveObject( ASender: TP3DDatablock );
      procedure ChangeObject( ASender: TP3DDatablock );
      procedure AddScene( ASender: TP3DDatablock );
      procedure RemoveScene( ASender: TP3DDatablock );
      procedure ChangeScene( ASender: TP3DDatablock );
      procedure AddTexture( ASender: TP3DDatablock );
      procedure RemoveTexture( ASender: TP3DDatablock );
      procedure ChangeTexture( ASender: TP3DDatablock );

      procedure ActionsChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure ArmaturesChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure CamerasChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure EffectsChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure FontsChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure GridsChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure LightsChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure MaterialsChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure MeshesChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure ObjectsChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure ScenesChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );
      procedure TexturesChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );

    // <--- LibHandlers
    protected
      FActions: TP3DActionContainerList;
      FArmatures: TP3DArmatureContainerList;
      FCameras: TP3DCameraContainerList;
      FEffects: TP3DEffectContainerList;
      FFonts: TP3DFontBmpContainerList;
      FGrids: TP3DTileGridContainerList;
      FLights: TP3DLightContainerList;
      FMaterials: TP3DMaterialContainerList;
      FMeshes: TP3DMeshContainerList;
      FObjects: TP3DObjectContainerList;
      FScenes: TP3DSceneContainerList;
      FTextures: TP3DTextureContainerList;

      FGlobalTime: Single;
      FIsDesignerPackage: Boolean;
      FBinFile: TFileStream;

      function GetBinFile: TFileStream;
      procedure PerformActions(GlobalTime: Float);
      procedure SetGlobalTime(AValue: Single);
      procedure FileNameChange( Sender: TP3DFilePointer; ANewValue: String );

    protected
      procedure SetName(AValue: String); override;
      procedure CreateBinFile( const AFileName: String = '' );
      procedure OpenBinFile( const AFileName: String = '' );
      procedure CloseBinFile;
      function GetBinaryStream: TStream;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor CreateFromFile( AFileName: String );
      destructor Destroy; override;

      //function GetOwnerFromPath( DOMPath: TP3DXMLPath; const ForceLoad: Boolean = False ): TP3DLibrary;

      function AppendFile( AFileName: String ): String;
      function CreateNew( Tp: TP3DDataBlockType ): TP3DDataBlock;
      function CreateNewWithObj( Tp: TP3DDataBlockType ): TP3DObject;
      procedure SaveToFile( const AFileName: String = '' );
      //procedure SaveToFile( AParentClass: TP3DDataBlockType; const AFileName: String = '' );
      procedure RemoveFromFields( Block: TP3DDataBlock ); virtual;
      procedure ClearFields; virtual;
      //function IsValid( Obj: TP3DDataBlock ): Boolean; virtual;
      function CheckLastChange: Integer; virtual;

    published
      property Actions: TP3DActionContainerList read FActions write FActions;
      property Armatures: TP3DArmatureContainerList read FArmatures write FArmatures;
      property Cameras: TP3DCameraContainerList read FCameras write FCameras;
      property Effects: TP3DEffectContainerList read FEffects write FEffects;
      property Fonts: TP3DFontBmpContainerList read FFonts write FFonts;
      property Grids: TP3DTileGridContainerList read FGrids write FGrids;
      property Lights: TP3DLightContainerList read FLights write FLights;
      property Materials: TP3DMaterialContainerList read FMaterials write FMaterials;
      property Meshes: TP3DMeshContainerList read FMeshes write FMeshes;
      property Objects: TP3DObjectContainerList read FObjects write FObjects;
      property Scenes: TP3DSceneContainerList read FScenes write FScenes;
      property Textures: TP3DTextureContainerList read FTextures write FTextures;

      property GlobalTime: Single read FGlobalTime write SetGlobalTime;

      property BinFile: TFileStream read GetBinFile write FBinFile;
      property IsDesignerPackage: Boolean read FIsDesignerPackage write FIsDesignerPackage;
      property Name: String read GetName;
  end;

  {TP3DLibraryContainer = specialize gP3DStreamableContainer < TP3DLibrary >;
  TP3DLibraryPointer = class( specialize gP3DStreamablePointer < TP3DLibrary >);
  //TP3DLibraryContainerList = specialize gP3DStreamableContainerList < TP3DLibrary, TP3DLibraryContainer >;
  TP3DLibraryPointerList = specialize gP3DStreamableList < TP3DLibrary, TP3DLibraryPointer >;
  }
  { TP3DLibraryList }

  TP3DLibraryContainerList = class ( specialize gP3DNamedStreamableContainerList < TP3DLibrary >)
    private
      FLastUpdate: Integer;

    public
      function FindByFileName( AFileName: String ): Integer;
      property LastUpdate: Integer read FLastUpdate;
  end;


  TP3DData = class ( TP3DLibrary )
    //LibHandlers --->
    protected
      procedure AddLibrary( ASender: TP3DDatablock );
      procedure RemoveLibrary( ASender: TP3DDatablock );
      procedure ChangeLibrary( ASender: TP3DDatablock );

      procedure LibrarysChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );

    // <--- LibHandlers
    protected
      FLibraries: TP3DLibraryContainerList;
      FP3DShaderNodeLib: TP3DStreamableContainer;
      function GetName: String; override;
      function GetP3DShaderNodeLib: TP3DShaderNodeLibrary;

    public
      constructor Create; reintroduce;
//      procedure PerformActions(GlobalTime: Float); override;

      procedure ClearFields; override;
      //function IsValid(Obj: TP3DDataBlock ): Boolean; override;
      function OpenLibrary(AFileName: String): Integer;
      function CheckLastChange: Integer; override;

    published
      property Libraries: TP3DLibraryContainerList read FLibraries write FLibraries;
      property P3DShaderNodeLib: TP3DShaderNodeLibrary read GetP3DShaderNodeLib;
  end;

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

{ TP3DLibrary }

procedure TP3DLibrary.AddHandler(HandlerType: TP3DLibraryHandler; const AMethod: TMethod; AsLast: Boolean);
begin
  if ( FHandlers[ HandlerType ] = nil ) then
    FHandlers[ HandlerType ]:= TMethodList.Create;
  FHandlers[ HandlerType ].Add( AMethod, AsLast );
end;

procedure TP3DLibrary.RemoveHandler(HandlerType: TP3DLibraryHandler; const AMethod: TMethod);
begin
  FHandlers[ HandlerType ].Remove( AMethod );
end;

procedure TP3DLibrary.AddDatablockHandler(HandlerType: TP3DLibraryHandler; const AMethod: TP3DDatablockHandler;
  AsLast: Boolean);
begin
  if ( FHandlers[ HandlerType ] = nil ) then
    FHandlers[ HandlerType ]:= TMethodList.Create;
  FHandlers[ HandlerType ].Add( TMethod( AMethod ), AsLast );
end;

procedure TP3DLibrary.RemoveDatablockHandler(HandlerType: TP3DLibraryHandler; const AMethod: TP3DDatablockHandler);
begin
  FHandlers[ HandlerType ].Remove( TMethod( AMethod ));
end;

procedure TP3DLibrary.DoCallDatablockHandlers(HandlerType: TP3DLibraryHandler; Block: TP3DDataBlock);
var
  i: LongInt;
  m: TMethodList;
begin
  m:= FHandlers[ HandlerType ];
  if ( not Assigned( m )) then
    exit;
  i:= m.Count;
  while m.NextDownIndex( i ) do
    TP3DDatablockHandler( m[ i ])( Block );
end;

procedure TP3DLibrary.AddAction(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddAction, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveAction(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveAction, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeAction(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeAction, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddArmature(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddArmature, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveArmature(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveArmature, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeArmature(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeArmature, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddCamera(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddCamera, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveCamera(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveCamera, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeCamera(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeCamera, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddEffect(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddEffect, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveEffect(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveEffect, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeEffect(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeEffect, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddFont(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddFont, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveFont(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveFont, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeFont(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeFont, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddGrid(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddGrid, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveGrid(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveGrid, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeGrid(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeGrid, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddLight(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddLight, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveLight(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveLight, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeLight(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeLight, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddMaterial(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddMaterial, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveMaterial(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveMaterial, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeMaterial(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeMaterial, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddMesh(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddMesh, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveMesh(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveMesh, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeMesh(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeMesh, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddObject(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddObject, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveObject(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveObject, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeObject(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeObject, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddScene(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddScene, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveScene(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveScene, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeScene(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeScene, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.AddTexture(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddTexture, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DLibrary.RemoveTexture(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveTexture, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DLibrary.ChangeTexture(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeTexture, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DLibrary.ActionsChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddAction( TP3DDataBlock( AItem ));
    else RemoveAction( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.ArmaturesChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddArmature( TP3DDataBlock( AItem ));
    else RemoveArmature( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.CamerasChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddCamera( TP3DDataBlock( AItem ));
    else RemoveCamera( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.EffectsChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddEffect( TP3DDataBlock( AItem ));
    else RemoveEffect( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.FontsChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddFont( TP3DDataBlock( AItem ));
    else RemoveFont( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.GridsChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddGrid( TP3DDataBlock( AItem ));
    else RemoveGrid( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.LightsChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddLight( TP3DDataBlock( AItem ));
    else RemoveLight( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.MaterialsChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddMaterial( TP3DDataBlock( AItem ));
    else RemoveMaterial( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.MeshesChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddMesh( TP3DDataBlock( AItem ));
    else RemoveMesh( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.ObjectsChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddObject( TP3DDataBlock( AItem ));
    else RemoveObject( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.ScenesChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddScene( TP3DDataBlock( AItem ));
    else RemoveScene( TP3DDataBlock( AItem ));
  end;
end;

procedure TP3DLibrary.TexturesChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddTexture( TP3DDataBlock( AItem ));
    else RemoveTexture( TP3DDataBlock( AItem ));
  end;
end;

function TP3DLibrary.GetBinFile: TFileStream;
begin
  if ( not Assigned( FBinFile )) then
    CreateBinFile;
  Result:= FBinFile;
end;

constructor TP3DLibrary.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  JSONLoader.OnFileNameChange:= @FileNameChange;
  Root:= Self;

  FCameras:= TP3DCameraContainerList.Create( 'Cameras', smText );
  FCameras.OnChange:= @CamerasChange;
  FGrids:= TP3DTileGridContainerList.Create( 'Grids', smText );
  FGrids.OnChange:= @GridsChange;
  FLights:= TP3DLightContainerList.Create( 'Lights', smText );
  FLights.OnChange:= @LightsChange;
  FMaterials:= TP3DMaterialContainerList.Create( 'Materials', smText );
  FMaterials.OnChange:= @MaterialsChange;
  FMeshes:= TP3DMeshContainerList.Create( 'Meshes', smText );
  FMeshes.OnChange:= @MeshesChange;
  FObjects:= TP3DObjectContainerList.Create( 'Objects', smText );
  FObjects.OnChange:= @ObjectsChange;
  FScenes:= TP3DSceneContainerList.Create( 'Scenes', smText );
  FScenes.OnChange:= @ScenesChange;
  FTextures:= TP3DTextureContainerList.Create( 'Textures', smText );
  FTextures.OnChange:= @TexturesChange;
  FArmatures:= TP3DArmatureContainerList.Create( 'Armatures', smText );
  FArmatures.OnChange:= @ArmaturesChange;
  FActions:= TP3DActionContainerList.Create( 'Actions', smText );
  FActions.OnChange:= @ActionsChange;
  FFonts:= TP3DFontBmpContainerList.Create( 'Fonts', smText );
  FFonts.OnChange:= @FontsChange;
  FEffects:= TP3DEffectContainerList.Create( 'Effects', smText );
  FEffects.OnChange:= @EffectsChange;

  //Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Name', smDoNotStore ));

  Properties.Add([ FCameras, FGrids, FLights, FMaterials, FMeshes, FObjects, FScenes, {FShaders, }FTextures, FArmatures, FActions, FFonts, FEffects ]);

  if ( not Assigned( Owner ) and not ( Self is TP3DData )) then
    P3DData.Libraries.Add( Self );
end;

constructor TP3DLibrary.CreateFromFile(AFileName: String);
begin
  if ( Assigned( P3DData )) then
    Create( P3DData.Libraries )
  else
    raise Exception.Create( 'P3DCore not initialized!' );
  AppendFile( AFileName );
end;

destructor TP3DLibrary.Destroy;
var
  h: TP3DLibraryHandler;
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
  CloseBinFile;
  inherited Destroy;
  for h:= Low( FHandlers ) to High( FHandlers ) do
    FreeAndNil( FHandlers[ h ]);
end;

procedure TP3DLibrary.CreateBinFile(const AFileName: String);
var
  FileName: String;
begin
  if ( AFileName > '' ) then
    FileName:= AFileName
  else
    FileName:= ChangeFileExt( JSONLoader.FileName, '.p3dbin' );

  if ( Assigned( FBinFile )) then
    FreeAndNil( FBinFile );

  BinFile:= TFileStream.Create( FileName, fmCreate );
end;

procedure TP3DLibrary.OpenBinFile(const AFileName: String);
var
  FileName: String;
begin
  if ( AFileName > '' ) then
    FileName:= AFileName
  else
    FileName:= ChangeFileExt( JSONLoader.FileName, '.p3dbin' );

  if ( Assigned( FBinFile )) then
    FreeAndNil( FBinFile );

  BinFile:= TFileStream.Create( FileName, fmOpenRead );
end;

procedure TP3DLibrary.CloseBinFile;
var
  FileName: String;
  Del: Boolean;
begin
  if ( Assigned( FBinFile )) then
    begin
      FileName:= FBinFile.FileName;
      Del:= FBinFile.Size = 0;
      FreeAndNil( FBinFile );
      if ( Del ) then
        DeleteFileUTF8( FileName );
    end;
  FreeAndNil( FBinFile );
end;

function TP3DLibrary.GetBinaryStream: TStream;
begin
  Result:= GetBinFile;
end;

{
function TP3DLibrary.GetOwnerFromPath(DOMPath: TP3DXMLPath; const ForceLoad: Boolean): TP3DLibrary;
var
  FN: String;
begin
  P3DXMLExtractRelativePath( DOMPath, FN );
  if (( FN = '' ) or ( CompareFilenames( FN, JSONLoader.FileName ) = 0 )) then
    Result:= Self
  else
    if ( ForceLoad ) then
      Result:= P3DData.Libraries[ P3DData.OpenLibrary( FN )]
    else
      Result:= P3DData.Libraries[ P3DData.Libraries.FindByFileName( FN )];
end;
}

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

function TP3DLibrary.AppendFile( AFileName: String ): String;
var
  FileName: String;
begin
  Result:= '';
  P3DLog.LogInfo( Self, Format( 'Loading file "%s"', [ AFileName ]));
  FileName:= P3DSearchPaths.FindFileEx( AFileName, GetCurrentDir );
  if ( FileName = '' ) then
    raise Exception.Create( Format( 'Cannot load library! The specified file "%s" was not found!', [ AFileName ]));
  FName:= P3DSearchPaths.FindFile( AFileName );
  if ( not FileExists( FileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );

  JSONLoader.FileName:= AFileName;

  try

  JSONLoader.ReadFile;

  if ( FileExistsUTF8( ChangeFileExt( JSONLoader.FileName, '.p3dbin' ))) then
    OpenBinFile();

  JSONLoader.ReadJSON;

  except
    on E: Exception do
      P3DLog.LogException( Self, E.Message );
  end;

  Result:= FileName;
end;

function TP3DLibrary.CreateNew(Tp: TP3DDataBlockType): TP3DDataBlock;
begin
  Result:= Tp.CreateBlock( Self );
end;

function TP3DLibrary.CreateNewWithObj(Tp: TP3DDataBlockType): TP3DObject;
begin
  Result:= TP3DObject.CreateBlock( Self );
  Result.Data:= CreateNew( Tp );
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
    FileName:= JSONLoader.FileName;
  CreateBinFile( ChangeFileExt( FileName, '.p3dbin' ));
  JSONLoader.WriteFile( FileName );
end;
{
procedure TP3DLibrary.SaveToFile( AParentClass: TP3DDataBlockType; const AFileName: String );
var
  FileName: String;
begin
  if ( AFileName > '' ) then
    FileName:= AFileName
  else
    FileName:= JSONLoader.FileName;

  CreateBinFile( ChangeFileExt( FileName, '.p3dbin' ));

  CloseBinFile;
end;
}
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
begin
  Cameras.Clear();
  Lights.Clear();
  Materials.Clear();
  Meshes.Clear();
  Objects.Clear();
  Scenes.Clear();
  //Shaders.Clear();
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

procedure TP3DLibrary.PerformActions(GlobalTime: Float);
begin
  //Actions.PerformActions( GlobalTime );
end;

procedure TP3DLibrary.SetGlobalTime(AValue: Single);
var
  Action: TP3DAction;
  User: IP3DStreamableUser;
begin
  if FGlobalTime=AValue then Exit;
  FGlobalTime:=AValue;
  for Action in Actions do
    for User in Action.Users do ;
      if ( User.GetUser is TP3DDataBlock ) then
        Action.Perform( GlobalTime, TP3DDataBlock( User.GetUser ));
end;

procedure TP3DLibrary.FileNameChange(Sender: TP3DFilePointer; ANewValue: String);
begin
  FName:= ExtractFileName( ANewValue )
end;

procedure TP3DLibrary.SetName(AValue: String);
begin

end;

{ TP3DDataList }

function TP3DLibraryContainerList.FindByFileName(AFileName: String): Integer;
var
  i: Integer;
  _Name: String;
begin
  Result:= -1;
  _Name:= ExpandFileNameUTF8( AFileName );
  for i:= 0 to Count - 1 do
    if ( P3DMatchFileName( AsValues[ i ].JSONLoader.FileName, _Name )) then
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
  entry.Owner:= Block.OwnerLib;
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
  Clear;
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
          //TODO: Fix this line Result:= item.BlockType.GetContainer( Lib ).IndexOf( item.Datablock ) > -1
        end
      else
        exit( False );
    end
  else
    exit( False );
end;

{ TP3DData }

function TP3DData.GetP3DShaderNodeLib: TP3DShaderNodeLibrary;
begin
  Result:= TP3DShaderNodeLibrary( FP3DShaderNodeLib.AsValue );
end;

procedure TP3DData.AddLibrary(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhAddLibrary, ASender );
  DoCallDatablockHandlers( lhAddDatablock, ASender );
end;

procedure TP3DData.RemoveLibrary(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhRemoveLibrary, ASender );
  DoCallDatablockHandlers( lhRemoveDatablock, ASender );
end;

procedure TP3DData.ChangeLibrary(ASender: TP3DDatablock);
begin
  DoCallDatablockHandlers( lhChangeLibrary, ASender );
  DoCallDatablockHandlers( lhChangeDatablock, ASender );
end;

procedure TP3DData.LibrarysChange(ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  case Action of
    actAdd: AddLibrary( TP3DDataBlock( AItem ));
    else RemoveLibrary( TP3DDataBlock( AItem ));
  end;
end;

function TP3DData.GetName: String;
begin
  Result:= 'P3DData';
end;

constructor TP3DData.Create;
begin
  inherited Create();
  Libraries:= TP3DLibraryContainerList.Create( 'Libraries' );
  Libraries.OnChange:= @LibrarysChange;
  FP3DShaderNodeLib:= TP3DStreamableContainer.Create( 'P3DShaderNodeLib', TP3DShaderNodeLibrary );
  Properties.Add([ Libraries, FP3DShaderNodeLib ]);
  TP3DShaderNodeLibrary.Create( FP3DShaderNodeLib );
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
  {
  for i:= Libraries.Count - 1 downto 0 do
    begin
      Libraries[ i ].Free;
      //Libraries.Delete( i );
    end;}

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

function TP3DData.OpenLibrary( AFileName: String ): Integer;
var
  FileName: String;
begin
  FileName:= P3DSearchPaths.FindFileEx( AFileName, GetCurrentDir );
  if ( FileName = '' ) then
    raise Exception.Create( Format( 'Cannot load library! The specified file "%s" was not found!', [ AFileName ]));
  Result:= Libraries.FindByFileName( FileName );
  if ( Result = -1 ) then
    begin
      TP3DLibrary.CreateFromFile( FileName );
      Result:= Libraries.Count - 1;
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
