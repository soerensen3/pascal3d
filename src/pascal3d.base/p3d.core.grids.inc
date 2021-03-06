
{$IFDEF INTERFACE}


TP3DGridScene = class;
TP3DRenderListModifierGrid = class;

{ TP3DGridSceneList }

TP3DGridSceneList = class ( specialize gP3DCustomObjectList < TP3DGridScene >)
  function FindByScene( AScene: TP3DScene ): Integer;
  function FindBySceneName( AName: String ): Integer;
end;

{ TP3DTileGrid }

TP3DTileGrid = class ( TP3DDataBlock, IP3DCollidable, IP3DRenderable )
  protected
    FGridWorldUnit: Single;
    FHeight: Integer;
    FScenes: TP3DGridSceneList;
    FWidth: Integer;
    FGridData: TP3DGridSceneList;
    FIsUpdating: Boolean;
    function GetSceneAt( x, y: Integer ): TP3DGridScene;
    procedure SetHeight( AValue: Integer );
    procedure SetSceneAt( x, y: Integer; AValue: TP3DGridScene );
    procedure SetWidth( AValue: Integer );

  protected
    procedure UpdateSize;
    procedure Inititalize;
    procedure UpdateGridData;
    procedure BeginUpdate;
    procedure EndUpdate;

  public
    procedure UpdateArrays;

    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
    destructor Destroy; override;

    class function GetContainer( ALib: TP3DLibrary): TP3DPropertyAccess; override;

    procedure PassToShader(Scene: TP3DGridScene; baseInstanceCount: Integer);

    // IP3DRenderable
    procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
    procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );


    property SceneAt[ x, y: Integer ]: TP3DGridScene read GetSceneAt write SetSceneAt;
    procedure OnGridDataChange( Sender: TObject; Item: TP3DGridScene; Action: TP3DListOnChangeAction );
    function AddScene( AScene: TP3DScene ): Integer;

  published
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Scenes: TP3DGridSceneList read FScenes write FScenes;
    property GridWorldUnit: Single read FGridWorldUnit write FGridWorldUnit;
    property GridData: TP3DGridSceneList read FGridData write FGridData;

    //IP3DCollidable -->
  private
    FAABB: TP3DAABB;
    FSphere: TP3DSphere;

    function GetAABB(): TP3DAABB;
    procedure SetAABB( AValue: TP3DAABB );
    function GetSphere(): TP3DSphere;
    procedure SetSphere( AValue: TP3DSphere );

  public
    function CalcAABB(): TP3DAABB;
    function CalcSphere: TP3DSphere;
    function CastRay( Orig, Dir: TVec3; out HitPos: TVec3 ): Boolean;
    function CastRay( Orig, Dir: TVec3; AObject: TP3DObject; out HitPos: TVec3 ): Boolean;

    function CollideObject( SelfMat: TMat4; ActMat: TMat4; AObject: TP3DObject; out Intersect: TVec3; out N: TVec3 ): Boolean;
    function CollideSphere( Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3 ): Boolean;
    function CollideAABB( Mat: TMat4; AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3 ): Boolean;
    function CollidePoint( Mat: TMat4; APoint: TVec3 ): Boolean;

    property AABB: TP3DAABB read GetAABB write SetAABB;
    property Sphere: TP3DSphere read GetSphere write SetSphere;
    //<-- IP3DCollidable
end;

{TP3DTileGridContainer = specialize gP3DStreamableContainer < TP3DTileGrid >;
TP3DTileGridPointer = specialize gP3DStreamablePointer < TP3DTileGrid >;
TP3DTileGridPointerList = specialize gP3DNamedStreamableList < TP3DTileGrid, TP3DTileGridPointer >;
}
TP3DTileGridContainerList = specialize gP3DNamedStreamableContainerList < TP3DTileGrid >;
{ TP3DGridScene }

TP3DGridScene = class ( TP3DStreamable )
  private
    FObjectList: TP3DRenderList;
    FInstances: TP3DFloatBufferGL;
    FModifier: TP3DRenderListModifierGrid;
    FScene: TP3DStreamablePointer;
    FTileGrid: TP3DStreamablePointer;

    function GetScene: TP3DScene;
    function GetTileGrid: TP3DTileGrid;
    procedure SetScene(AValue: TP3DScene);
    procedure SetTileGrid(AValue: TP3DTileGrid);

  public
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
    destructor Destroy; override;

  published
    property TileGrid: TP3DTileGrid read GetTileGrid write SetTileGrid;
    property Scene: TP3DScene read GetScene write SetScene;
    property Instances: TP3DFloatBufferGL read FInstances;
    property Modifier: TP3DRenderListModifierGrid read FModifier;
    property ObjectList: TP3DRenderList read FObjectList write FObjectList;
end;

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

procedure TP3DGridScene.SetScene(AValue: TP3DScene);
begin
  {TODO:BROKEN:if FScene=AValue then Exit;
  FScene:= AValue;
  FreeAndNil( FObjectList );
  if ( Assigned( Scene )) then
    Scene.MakeRenderList( nil, Mat4Identity, FObjectList, [ rlfObjects ]);}
end;

function TP3DGridScene.GetTileGrid: TP3DTileGrid;
begin
  Result:= TP3DTileGrid( FTileGrid.AsValue );
end;

function TP3DGridScene.GetScene: TP3DScene;
begin
  Result:= TP3DScene( FScene.AsValue );
end;

procedure TP3DGridScene.SetTileGrid(AValue: TP3DTileGrid);
begin
  FTileGrid.AsValue:= AValue;
end;

constructor TP3DGridScene.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FTileGrid:= TP3DStreamablePointer.Create( 'TileGrid', TP3DTileGrid );
  FModifier:= TP3DRenderListModifierGrid.Create( Self );
  FInstances:= TP3DFloatBufferGL.Create;
  FScene:= TP3DStreamablePointer.Create( 'Scene', TP3DScene, smText );
  Properties.Add([ FTileGrid, FScene ]);
end;

destructor TP3DGridScene.Destroy;
begin
  Modifier.Free;
  Instances.Free;
  FreeAndNil( FObjectList );
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

class function TP3DTileGrid.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Grids' );
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

procedure TP3DTileGrid.UpdateGridData;
var
  i, j: Integer;
begin
  Inititalize;
  BeginUpdate;
  for i:= 0 to Scenes.Count - 1 do
    for j:= 0 to Scenes[ i ].Instances.Count - 1 do
      GridData[ floor( Scenes[ i ].Instances[ j ])]:= Scenes[ i ];
  EndUpdate;
end;

procedure TP3DTileGrid.BeginUpdate;
begin
  FIsUpdating:= True;
end;

procedure TP3DTileGrid.EndUpdate;
begin
  FIsUpdating:= False;
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

constructor TP3DTileGrid.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Scenes:= TP3DGridSceneList.Create;
  GridData:= TP3DGridSceneList.Create;
  GridData.OnChange:= @OnGridDataChange;
  GridWorldUnit:= 1;
  FWidth:= 32;
  Height:= 32;
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Width', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Height', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'GridWorldUnit', smText ));
  //Properties.Add( TP3DPropAccessSceneList.CreateField( 'Scenes', @FScenes, smNode ));
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
        decl.AsFloat:= baseInstanceCount * Scene.Instances.Count;

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

procedure TP3DTileGrid.Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
begin

end;

procedure TP3DTileGrid.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
var
  i: Integer;
  Modifier: TP3DRenderListModifier;
begin
  if ( rlfObjects in AFlags ) then
    exit;

  for i:= 0 to Scenes.Count - 1 do
    if ( Assigned( Scenes[ i ].Scene ) and Assigned( Scenes[ i ].Modifier )) then
      begin
        Modifier:= Scenes[ i ].Modifier;

        AList.ModifierStack.Add( Modifier );
        Scenes[ i ].Scene.MakeRenderList( ARootScene, AWorld, AList, AFlags );
        AList.ModifierStack.Remove( Modifier );
      end;
end;

function TP3DTileGrid.CollideSphere(Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3): Boolean;
var
  m: TMat4;
  gridpos: TVec3;
  transsp: TP3DSphere;
  sz: Single;
  x: Integer;
  y: Integer;
  B: TP3DRenderObject;
  sc: TP3DGridScene;
begin
  Result:= False;

  mat4inverse( Mat, m );

  transsp:= P3DTransformSphere( m, ASphere );
  gridpos:= transsp.Position / GridWorldUnit;

  sz:= transsp.Radius / GridWorldUnit;
  for x:= clamp( floor( gridpos.X - sz ), 0, Width - 1 ) to clamp( ceil( gridpos.X + sz ), 0, Width - 1 ) do
    for y:= clamp( floor( gridpos.Y - sz ), 0, Height - 1 ) to clamp( ceil( gridpos.Y + sz ), 0, Height - 1 ) do
      begin
        sc:= SceneAt[ x, y ];
        if ( Assigned( sc ) and Assigned( sc.ObjectList )) then
          if ( sc.ObjectList.CheckCollision( P3DSphere( transsp.Position - vec3( x, y, 0 ) * GridWorldUnit, transsp.Radius ), B, Intersect, N )) then
            begin
              Intersect += vec3( x, y, 0 ) * GridWorldUnit;
              Result:= True;
              Break;
            end;
      end;
end;

function TP3DTileGrid.CollideAABB(Mat: TMat4; AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3): Boolean;
begin

end;

function TP3DTileGrid.CollidePoint(Mat: TMat4; APoint: TVec3): Boolean;
begin

end;

procedure TP3DTileGrid.OnGridDataChange( Sender: TObject; Item: TP3DGridScene; Action: TP3DListOnChangeAction );
begin
  if ( FIsUpdating ) then
    exit;

  case Action of
    actDelete:
      if ( Assigned( Item )) then
        begin
          Item.Instances.Remove( Scenes.IndexOf( Item ));
          Item.Instances.PushData;
        end;
    actAdd:
      if ( Assigned( Item )) then
        begin
          Item.Instances.Add( Scenes.IndexOf( Item ));
          Item.Instances.PushData;
        end;
  end;
end;

function TP3DTileGrid.AddScene(AScene: TP3DScene): Integer;
var
  Scene: TP3DGridScene;
begin
  Result:= Scenes.FindByScene( AScene );
  if ( Result < 0 ) then
    begin
      Scene:= TP3DGridScene.Create;
      Scene.Scene:= AScene;
      Result:= Scenes.Add( Scene );
    end;
end;

function TP3DTileGrid.GetAABB: TP3DAABB;
begin
  Result:= FAABB;
end;

procedure TP3DTileGrid.SetAABB(AValue: TP3DAABB);
begin
  FAABB:= AValue;
end;

function TP3DTileGrid.GetSphere: TP3DSphere;
begin
  Result:= FSphere;
end;

procedure TP3DTileGrid.SetSphere(AValue: TP3DSphere);
begin
  FSphere:= AValue;
end;

function TP3DTileGrid.CalcAABB: TP3DAABB;
begin

end;

function TP3DTileGrid.CalcSphere: TP3DSphere;
begin

end;

function TP3DTileGrid.CastRay(Orig, Dir: TVec3; out HitPos: TVec3): Boolean;
begin

end;

function TP3DTileGrid.CastRay(Orig, Dir: TVec3; AObject: TP3DObject; out HitPos: TVec3): Boolean;
begin

end;

function TP3DTileGrid.CollideObject(SelfMat: TMat4; ActMat: TMat4; AObject: TP3DObject; out Intersect: TVec3; out N: TVec3): Boolean;
begin

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
  if ( Assigned( Scene )) then
    Obj.InstanceCount:= Scene.Instances.Count * Obj.Positions.Count;
end;

{ TP3DGridSceneList }

function TP3DGridSceneList.FindByScene(AScene: TP3DScene): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count -1 do
    if ( Items[ i ].Scene = AScene ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DGridSceneList.FindBySceneName(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count -1 do
    if ( Items[ i ].Scene.Name = AName ) then
      begin
        Result:= i;
        break;
      end;
end;

{$ENDIF}
