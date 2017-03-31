
{$IFDEF INTERFACE}


TP3DTileGrid = class;
TP3DRenderListModifierGrid = class;

{ TP3DGridScene }

TP3DGridScene = class ( TP3DInterfacedPersistent )
  private
    FActorList: TP3DRenderList;
    FInstances: TP3DFloatBufferGL;
    FModifier: TP3DRenderListModifierGrid;
    FScene: TP3DScene;
    FTileGrid: TP3DTileGrid;

    procedure SetScene(AValue: TP3DScene);

  public
    constructor Create( ATileGrid: TP3DTileGrid );
    destructor Destroy; override;

    function SaveToDOM( AParent: TDOMElement ): TDOMElement; override;
    class function DOMNodeName: String; override;

  published
    property TileGrid: TP3DTileGrid read FTileGrid;
    property Scene: TP3DScene read FScene write SetScene;
    property Instances: TP3DFloatBufferGL read FInstances;
    property Modifier: TP3DRenderListModifierGrid read FModifier;
    property ActorList: TP3DRenderList read FActorList write FActorList;
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
    class function DOMNodeName: String; override;

    constructor Create( const AData: TP3DLibrary=nil ); override;
    destructor Destroy; override;
    function SaveToDOM(AParent: TDOMElement): TDOMElement; override;
    procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;

    procedure PassToShader(Scene: TP3DGridScene; baseInstanceCount: Integer);
    procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags ); override;
    function CollideSphere(Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3): Boolean; override;

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
  if FScene=AValue then Exit;
  FScene:=AValue;
  FreeAndNil( FActorList );
  if ( Assigned( Scene )) then
    Scene.MakeRenderList( nil, Mat4Identity, FActorList, [ rlfActors ]);
end;

constructor TP3DGridScene.Create(ATileGrid: TP3DTileGrid );
begin
  inherited Create;
  FTileGrid:= ATileGrid;
  FModifier:= TP3DRenderListModifierGrid.Create( Self );
  FInstances:= TP3DFloatBufferGL.Create( True );
  Properties.Context:= ATileGrid;
  Properties.Add( TP3DPropertyAccessScene.CreateFieldSet( 'Scene', @FScene, @SetScene, smAttribute ));
end;

destructor TP3DGridScene.Destroy;
begin
  Modifier.Free;
  Instances.Free;
  FreeAndNil( FActorList );
  inherited Destroy;
end;

function TP3DGridScene.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM(AParent);

end;

class function TP3DGridScene.DOMNodeName: String;
begin
  Result:= 'gridscene';
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

class function TP3DTileGrid.DOMNodeName: String;
begin
  Result:= 'tilegrid';
end;

constructor TP3DTileGrid.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  Scenes:= TP3DGridSceneList.Create;
  GridData:= TP3DGridSceneList.Create;
  GridData.OnSet:= @OnGridDataSet;
  GridWorldUnit:= 1;
  FWidth:= 32;
  Height:= 32;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Width', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Height', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'GridWorldUnit', smAttribute ));
  //Properties.Add( TP3DPropertyAccessSceneList.CreateField( 'Scenes', @FScenes, smNode ));
end;

destructor TP3DTileGrid.Destroy;
begin
  FreeAndNil( FScenes );
  FreeAndNil( FGridData );
  inherited Destroy;
end;

function TP3DTileGrid.SaveToDOM(AParent: TDOMElement): TDOMElement;
var
  Prop: TDOMElement;
  i: Integer;
  fn: String;
  S: TFileStream;
begin
  Result:= inherited SaveToDOM(AParent);

  Prop:= AParent.OwnerDocument.CreateElement( 'property' );
  Prop.SetAttribute( 'Name', 'Scenes' );
  Result.AppendChild( Prop );

  if ( Scenes.Count > 0 ) then
    begin
      //fn:= ExtractFileNameOnly( Owner.Name ) + '.' + Name + '.p3dbin';
      //S:= TFileStream.Create( fn, fmCreate );
      fn:= Owner.BinFile.FileName;
      S:= Owner.BinFile;
      for i:= 0 to Scenes.Count - 1 do
        if ( Assigned( Scenes[ i ]) and ( Scenes[ i ].Instances.Count > 0 )) then
          begin
            Scenes[ i ].SaveToDOM( Prop ).AttribStrings[ 'Instances' ]:= fn + ':' + IntToStr( S.Position );
            Scenes[ i ].Instances.SaveToStream( S );
          end;
      //S.Free;
    end;
end;

procedure TP3DTileGrid.LoadFromDOMNew(ADOMNode: TDOMElement);

var
  S: TFileStream;

  function SetupStream( ADOMNode: TDOMElement; AName: String ): Boolean;
  var
    P: Integer;
    SPos, FN: String;
  begin
    Result:= False;
    SPos:= ADOMNode.AttribStrings[ AName ];
    if ( SPos = '' ) then
      exit;
    FN:= Copy2SymbDel( SPos, ':' );
    P:= StrToInt( SPos );
    if ( Assigned( S )) then
      if ( CompareFilenames( S.FileName, FN ) <> 0 ) then
        S.Free;
    if ( not Assigned( S )) then
      S:= TFileStream.Create( FN, fmOpenRead );
    S.Position:= P;
    Result:= True;
  end;

var
  PropLst: TDOMNodeList;
  gridscene: TDOMElement;
  n, i: Integer;
  AName: DOMString;
begin
  inherited LoadFromDOMNew(ADOMNode);
  PropLst:= ADOMNode.GetElementsByTagName( 'property' );

  Scenes.Clear( True );
  for i:= 0 to PropLst.Count - 1 do
    if ( TDOMElement( PropLst[ i ]).GetAttribute( 'Name' ) =  'Scenes' ) then
      begin
        S:= nil;
        gridscene:= TDOMElement( PropLst[ i ].FirstChild );
        while Assigned( gridscene ) do
         begin
           AName:= TDOMElement( gridscene ).NodeName;
           if ( AName = 'gridscene' ) and ( SetupStream( gridscene, 'Instances' )) then
              begin
                n:= Scenes.Add( TP3DGridScene.Create( Self ));
                Scenes[ n ].LoadFromDOMNew( gridscene );
                Scenes[ n ].Instances.LoadFromStream( S );
                Scenes[ n ].Instances.PushData();
              end
           else if ( AName = 'empty' ) then
             AddScene( nil );
           gridscene:= TDOMElement( gridscene.NextSibling );
         end;
      S.Free;
    end;
  UpdateGridData;
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

procedure TP3DTileGrid.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
var
  i: Integer;
  Modifier: TP3DRenderListModifier;
begin
  if ( rlfActors in AFlags ) then
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
  Result:= inherited CollideSphere( Mat, ASphere, Intersect, N );

  m:= -Mat;

  transsp:= P3DTransformSphere( m, ASphere );
  gridpos:= transsp.Position / GridWorldUnit;

  sz:= transsp.Radius / GridWorldUnit;
  for x:= clamp( floor( gridpos.X - sz ), 0, Width - 1 ) to clamp( ceil( gridpos.X + sz ), 0, Width - 1 ) do
    for y:= clamp( floor( gridpos.Y - sz ), 0, Height - 1 ) to clamp( ceil( gridpos.Y + sz ), 0, Height - 1 ) do
      begin
        sc:= SceneAt[ x, y ];
        if ( Assigned( sc ) and Assigned( sc.ActorList )) then
          if ( sc.ActorList.CheckCollision( P3DSphere( transsp.Position - vec3( x, y, 0 ) * GridWorldUnit, transsp.Radius ), B, Intersect, N )) then
            begin
              Intersect += vec3( x, y, 0 ) * GridWorldUnit;
              Result:= True;
              Break;
            end;
      end;

  P3DApplication.MainWindow.Title:= gridpos.ToString();
end;

function TP3DTileGrid.OnGridDataSet(Sender: TPersistent; ItemIndex: Integer; AValue: TP3DGridScene): Boolean;
var
  OldValue: TP3DGridScene;
begin
  if ( FIsUpdating ) then
    begin
      Result:= True;
      exit;
    end;
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
  if ( Assigned( Scene )) then
    Obj.InstanceCount:= Scene.Instances.Count * Obj.Positions.Count;
end;


{$ENDIF}
