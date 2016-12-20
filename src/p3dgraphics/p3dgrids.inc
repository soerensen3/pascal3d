
{$IFDEF INTERFACE}


TP3DTileGrid = class;
TP3DRenderListModifierGrid = class;

{ TP3DGridScene }

TP3DGridScene = class ( TP3DInterfacedPersistent )
  private
    FIndex: Integer;
    FInstances: TP3DFloatBufferGL;
    FModifier: TP3DRenderListModifierGrid;
    FScene: TP3DScene;
    FTileGrid: TP3DTileGrid;

  public
    constructor Create( ATileGrid: TP3DTileGrid );
    destructor Destroy; override;

  published
    property TileGrid: TP3DTileGrid read FTileGrid;
    property Scene: TP3DScene read FScene write FScene;
    property Instances: TP3DFloatBufferGL read FInstances;
    property Modifier: TP3DRenderListModifierGrid read FModifier;
end;

{ TP3DGridSceneList }

TP3DGridSceneList = class ( specialize gP3DCustomObjectList < TP3DGridScene >)
  function FindByScene( AScene: TP3DScene ): Integer;
  function FindBySceneName( AName: String ): Integer;
end;

{ TP3DTileGrid }

TP3DTileGrid = class ( TP3DDatablock )
  private
    FGridWorldUnit: Single;
    FHeight: Integer;
    FScenes: TP3DGridSceneList;
    FWidth: Integer;
    FGridData: TP3DGridSceneList;
    function GetSceneAt( x, y: Integer ): TP3DGridScene;
    procedure SetHeight( AValue: Integer );
    procedure SetSceneAt( x, y: Integer; AValue: TP3DGridScene );
    procedure SetWidth( AValue: Integer );

  protected
    procedure UpdateSize;
    procedure Inititalize;

  public
    procedure UpdateArrays;

    constructor Create( const AData: TP3DData=nil ); override;
    destructor Destroy; override;

    procedure PassToShader(Scene: TP3DGridScene; baseInstanceCount: Integer);

    property SceneAt[ x, y: Integer ]: TP3DGridScene read GetSceneAt write SetSceneAt;
    function OnGridDataSet( Sender: TPersistent; ItemIndex: Integer; AValue: TP3DGridScene ): Boolean;
    function AddScene( AScene: TP3DScene ): Integer;

  published
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Scenes: TP3DGridSceneList read FScenes write FScenes;
    property GridWorldUnit: Single read FGridWorldUnit write FGridWorldUnit;
    property GridData: TP3DGridSceneList read FGridData write FGridData;
end;

{ TP3DRenderListModiferGrid }

{ TP3DRenderListModifierGrid }

TP3DRenderListModifierGrid = class ( TP3DRenderListModifier )
  private
    FScene: TP3DGridScene;

  public
    constructor Create( AGridScene: TP3DGridScene );

    procedure PassToShader( Obj: TP3DRenderObject ); override;
    procedure AddObject( var Obj: TP3DRenderObject ); override;

    property Scene: TP3DGridScene read FScene write FScene;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DGridScene }

constructor TP3DGridScene.Create(ATileGrid: TP3DTileGrid );
begin
  inherited Create;
  FTileGrid:= ATileGrid;
  FModifier:= TP3DRenderListModifierGrid.Create( Self );
  FInstances:= TP3DFloatBufferGL.Create( True );
end;

destructor TP3DGridScene.Destroy;
begin
  Modifier.Free;
  Instances.Free;
  inherited Destroy;
end;


{ TP3DTileGrid }

function TP3DTileGrid.GetSceneAt(x, y: Integer): TP3DGridScene;
begin
  Result:= GridData[ x + y * Width ];
end;

procedure TP3DTileGrid.SetHeight( AValue: Integer );
begin
  if ( FHeight = AValue ) then
    Exit;
  FHeight:= AValue;
  UpdateSize;
end;

procedure TP3DTileGrid.SetSceneAt(x, y: Integer; AValue: TP3DGridScene);
begin
  GridData[ x + y * Width ]:= AValue;
end;

procedure TP3DTileGrid.SetWidth( AValue: Integer );
begin
  if ( FWidth = AValue ) then
    Exit;

  FWidth:= AValue;
  UpdateSize;
end;

procedure TP3DTileGrid.UpdateSize;
begin
  GridData.Count:= Width * Height;
  Inititalize;
end;

procedure TP3DTileGrid.Inititalize;
var
  i: Integer;
begin
  for i:= 0 to GridData.Count - 1 do
    GridData[ i ]:= nil;
end;

procedure TP3DTileGrid.UpdateArrays;
var
  i: Integer;
  Scene: TP3DGridScene;
begin
  for i:= 0 to Scenes.Count - 1 do
    Scenes[ i ].Instances.Clear;

  for i:= 0 to Width * Height - 1 do
    begin
      Scene:= GridData[ i ];
      if ( Assigned( Scene )) then
        Scene.Instances.Add( i );
    end;

  for i:= 0 to Scenes.Count - 1 do
    Scenes[ i ].Instances.PushData;
end;

constructor TP3DTileGrid.Create( const AData: TP3DData );
begin
  inherited Create( AData );
  Scenes:= TP3DGridSceneList.Create;
  GridData:= TP3DGridSceneList.Create;
  GridData.OnSet:= @OnGridDataSet;
  GridWorldUnit:= 1;
  FWidth:= 32;
  Height:= 32;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Width' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Height' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'GridWorldUnit' ));
  Properties.Add( TP3DPropertyAccessDataBlockList.CreateField( 'Scenes', @FScenes ));
end;

destructor TP3DTileGrid.Destroy;
begin
  FreeAndNil( FScenes );
  FreeAndNil( FGridData );
  inherited Destroy;
end;

procedure TP3DTileGrid.PassToShader( Scene: TP3DGridScene; baseInstanceCount: Integer);
var
  decl, instanceID: TP3DShaderDecl;
begin
  if ( Assigned( Scene.Scene )) then
    begin
      decl:= P3DShaderActive.Uniforms.Names[ 'instanceGridWidth' ];
      if ( Assigned( decl )) then
        decl.AsInt:= Width;

      decl:= P3DShaderActive.Uniforms.Names[ 'instanceGridWorldSize' ];
      if ( Assigned( decl )) then
        decl.AsFloat:= GridWorldUnit;

      decl:= P3DShaderActive.Uniforms.Names[ 'world_instance_repeat' ];
      if ( Assigned( decl )) then
        decl.AsFloat:= baseInstanceCount;

      if ( P3DShaderActive.Attributes.FindByName( 'instanceID', instanceID )) then
        begin
          Scene.Instances.Divisor:= baseInstanceCount;
          Scene.Instances.SetAttribArray( instanceID.Addr );
        end;
    end;
  {for i:= 0 to Meshes.Count - 1 do
    for j:= 0 to Width * Height - 1 do
      if (( GridData[ j ] = i ) and ( Assigned( Meshes[ i ]))) then
        begin
          p:= vec2( j mod Width, j div Width ) * GridWorldUnit;
          Meshes[ i ].Render( world * mat4translate( vec4( p, 0, 1 )), Scene, RenderObject );
        end;}
end;

function TP3DTileGrid.OnGridDataSet(Sender: TPersistent; ItemIndex: Integer; AValue: TP3DGridScene): Boolean;
var
  OldValue: TP3DGridScene;
begin
  Result:= False;

  OldValue:= GridData[ ItemIndex ];
  if ( OldValue = AValue ) then
    exit;

  if ( Assigned( OldValue )) then
    begin
      OldValue.Instances.Remove( ItemIndex );
      OldValue.Instances.PushData;
    end;

  if ( Assigned( AValue )) then
    begin
      AValue.Instances.Add( ItemIndex );
      AValue.Instances.PushData;
    end;

  Result:= True;
end;

function TP3DTileGrid.AddScene(AScene: TP3DScene): Integer;
var
  Scene: TP3DGridScene;
begin
  Result:= Scenes.FindByScene( AScene );
  if ( Result < 0 ) then
    begin
      Scene:= TP3DGridScene.Create( Self );
      Scene.Scene:= AScene;
      Result:= Scenes.Add( Scene );
    end;
end;

{ TP3DRenderListModifierGrid }

constructor TP3DRenderListModifierGrid.Create(AGridScene: TP3DGridScene);
begin
  inherited Create;
  Scene:= AGridScene;
end;

procedure TP3DRenderListModifierGrid.PassToShader(Obj: TP3DRenderObject);
begin
  if ( Assigned( Scene.TileGrid )) then
    Scene.TileGrid.PassToShader( Scene, Obj.Positions.Count );
end;

procedure TP3DRenderListModifierGrid.AddObject(var Obj: TP3DRenderObject);
begin
  Obj.InstanceCount:= Obj.InstanceCount * Scene.Instances.Count;
end;


{$ENDIF}
