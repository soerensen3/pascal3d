//p3dscene.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
type

  { TP3DRenderQueryObject }

  TP3DRenderQueryObject = class
    private
      FActor: TP3DActor;
      FMatrix: TMat4;
      FScene: TP3DScene;

    public
      property Actor: TP3DActor read FActor write FActor;
      property Matrix: TMat4 read FMatrix write FMatrix;
      property Scene: TP3DScene read FScene write FScene;
  end;

  { TP3DRenderObject }

  TP3DRenderObject = class
    private
      FData: TP3DDataBlock;
      FMaterial: TP3DMaterial;
      FMatrices: TMat4List;

    public
      constructor Create;
      destructor Destroy; override;

      property Data: TP3DDataBlock read FData write FData;
      property Matrices: TMat4List read FMatrices write FMatrices;
      property Material: TP3DMaterial read FMaterial write FMaterial;
  end;

  TP3DCustomRenderList = specialize gP3DCustomObjectList < TP3DRenderObject >;

  { TP3DRenderList }

  TP3DRenderListScoreFunc = function ( Item: TP3DRenderObject ): Integer;

  TP3DRenderList = class ( TP3DCustomRenderList )
    private
      FLights: TP3DLightList;
      FTransparencyStart: Integer;

    public
      constructor Create;
      destructor Destroy; override;

      function Add( AData: TP3DDataBlock; world: TMat4; AScene: TP3DScene ): Integer;
      function PickFromCamera( X, Y: Integer; Camera: TP3DActor ): TP3DActor;
      function Find( AData: TP3DDataBlock; Material: TP3DMaterial ): Integer;
      procedure Render( Camera: TP3DActor );
      procedure Sort( Func: TP3DRenderListScoreFunc; lo, hi: Integer );
      procedure Optimize;

      property Lights: TP3DLightList read FLights write FLights;
  end;

  { TP3DScene }

  TP3DSceneRenderCallback = procedure ( world: TMat4; AActor: TP3DActor ) of object;
  TP3DScene = class( TP3DDataBlock )
    private
      FCam: TP3DActor;
      FIsDesignTime: Boolean;
      FInheritedDesignTime: Boolean;
      FLights: TP3DLightList;
      FObjects: TP3DActorList;
      FActiveCam: TP3DActor;
      FOnRenderCallback: TP3DSceneRenderCallback;
      FRootScene: TP3DScene;

      procedure SetCam( AValue: TP3DActor );
      procedure RenderCallback( world: TMat4; AActor: TP3DActor ); virtual;
      procedure OnObjChange( Sender: TPersistent; Item: TP3DActor; Action: TP3DCustomListOnChangeAction );

    protected
      property ActiveCam: TP3DActor read FActiveCam write FActiveCam;
      property RootScene: TP3DScene read FRootScene write FRootScene;
      property Lights: TP3DLightList read FLights write FLights;

    public
      constructor Create( const AData: TP3DData = nil ); override;
      constructor Create( AFileName: String; const AWatchFileForChanges: Boolean = False; const AData: TP3DData = nil );
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      procedure FastDestroy; override;

      procedure ClearFields; override;
      procedure UpdateLights;

      procedure Clear;

//      function Debug: String;

      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor); override;
      procedure Render(const ARootScene: TP3DScene=nil); reintroduce;
      procedure MakeRenderList( ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList );
      procedure RenderFromCamera( ACam: TP3DActor );
      procedure RenderFromCamera( ACam: TP3DActor; world: TMat4 );
      procedure AppendFile( AFileName: String );
      procedure AppendFromData( AData: TP3DData );
      procedure PassToShader; virtual;
      procedure RemoveFromFields( Block: TP3DDataBlock ); override;
      //function Pick( X,Y: Integer; const ARootScene: TP3DScene=nil ): TP3DActor;

      property Objects: TP3DActorList read FObjects;
      property Cam: TP3DActor read FCam write SetCam;
      property IsDesignTime: Boolean read FIsDesignTime write FIsDesignTime;
      property OnRenderCallback: TP3DSceneRenderCallback read FOnRenderCallback write FOnRenderCallback;
  end;
  TP3DSceneList = specialize gP3DCustomDatablockList < TP3DScene >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

function P3DRenderListScoreTransp( Item: TP3DRenderObject ): Integer;
begin
  Result:= Integer( Item.Material.HasTransparency );
end;

{ TP3DRenderList }

constructor TP3DRenderList.Create;
begin
  inherited Create;
  Lights:= TP3DLightList.Create;
end;

destructor TP3DRenderList.Destroy;
begin
  Clear( True );
  FreeAndNil( FLights );
  inherited Destroy;
end;

function TP3DRenderList.Add( AData: TP3DDataBlock; world: TMat4; AScene: TP3DScene): Integer;
  function MakeRenderObject( AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial ): Integer;
  var
    Obj: TP3DRenderObject;
  begin
    Result:= Find( AData, AMaterial );
    if ( Result > -1 ) then
      Self[ Result ].Matrices.Add( world )
    else
      begin
        Obj:= TP3DRenderObject.Create;
        Obj.Data:= AData;
        Obj.Matrices.Add( world );
        Obj.Material:= AMaterial;
        Result:= inherited Add( Obj );
      end;
  end;

var
  i, j: Integer;
  mat: TP3DMaterial;

begin
  if ( Assigned( AData )) then
    begin
      if ( AData is TP3DScene ) then
        TP3DScene( AData ).MakeRenderList( AScene, world, Self )
      else if ( AData is TP3DMesh ) then
        for i:= 0 to TP3DMesh( AData ).MaterialGroups.Count - 1 do
          begin
            mat:= TP3DMesh( AData ).MaterialGroups[ i ].Material;
            Result:= MakeRenderObject( AData, world, mat );
          end
      else if ( AData is TP3DTileGrid ) then
        with ( TP3DTileGrid( AData )) do
          for i:= 0 to Width * Height - 1 do
            Add( Meshes[ GridData[ i ]], world * mat4translate( vec4( vec2( i mod Width, i div Width ) * GridWorldUnit, 0, 1 )), AScene )
      else if ( AData is TP3DLight ) then
        Lights.Add( AData as TP3DLight );
    end
  else
    Result:= -1;
end;

function TP3DRenderList.PickFromCamera(X, Y: Integer; Camera: TP3DActor ): TP3DActor;
begin
  {glScissor( X, Y, 1, 1 );
  glEnable( GL_SCISSOR_TEST );
  if ( not Assigned( PickMaterial )) then
    begin
      PickMaterial.Create();
      PickMaterial.Unlit:= True;
    end;
  PickMaterial.Diff:= vec3( 0 );
  PickMaterial.PassToShader();

  glDisable( GL_SCISSOR_TEST );}
end;

function TP3DRenderList.Find(AData: TP3DDataBlock; Material: TP3DMaterial): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if (( Self[ i ].Data = AData ) and ( Self[ i ].Material = Material )) then
      begin
        Result:= i;
        break;
      end;
end;

procedure TP3DRenderList.Render( Camera: TP3DActor );

  function Mat4ListToTexture( m: TMat4List ): TP3DTexture;
    function nextPowerOfTwo( Value: Integer ): Integer;
    begin
      Dec( Value );
      value:= value or value shr 1;
      value:= value or value shr 2;
      value:= value or value shr 4;
      value:= value or value shr 8;
      value:= value or value shr 16;
      Inc( Value );
      Result:= Value;
    end;

  var
    size, i, oldc: Integer;
    m2: TMat4List;
  begin
    size:= Max( 4, nextPowerOfTwo( m.Count * 4 )); //Max( 4, ceil( sqrt( power( 2, ceil( log2( m.Count * 4 ))))));

    oldc:= m.Count;
    m.Count:= ( size * size ) div 4;
    //m2:= TMat4List.Create;
    //m.AssignTo( m2 );
    //for i:= m2.Count to ( size*size ) div 4 do
    //  m2.Add( mat4( 0 ));
    Result:= TP3DTexture.CreateFromBuffer( size, size, 4, dtFloat, m.Ptr );
    m.Count:= oldc;
    Result.MipMap:= False;
    Result.Filtering:= tfNearest;
    Result.FilteringMipMap:= tfNearest;
    //m2.Free;
  end;

var
  cam: TP3DCamera;
  mat: TP3DMaterial;
  i: Integer;
  idx: LongInt;
  Tex: TP3DTexture;
begin
  if ( not ( Camera.Data is TP3DCamera )) then
    exit;
  cam:= TP3DCamera( Camera.Data );
  mat:= nil;

  glDepthMask( True );
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Data is TP3DMesh ) then
      begin
        if ( i = FTransparencyStart ) then
          glDepthMask( False );
        if ( mat <> Items[ i ].Material ) then
          begin
            mat:= Items[ i ].Material;
            mat.PassToShader();
            cam.PassToShader();
            Lights.PassToActiveShader( cam.View, [ liLightParams, liPosition ]);
          end;
        with( TP3DMesh( Items[ i ].Data )) do
          begin
            idx:= MaterialGroups.Find( mat );

            Tex:= Mat4ListToTexture( Items[ i ].Matrices );
            mat.PassToShader(); //Needs to be done for now because creating a texture will overwrite the first texture used
            if ( idx > -1 ) then
              RenderMaterialGroupInstanced( Tex, Items[ i ].Matrices.Count, idx );
            Tex.Free;
          end;
      end;
  glDepthMask( True );
end;

procedure TP3DRenderList.Sort(Func: TP3DRenderListScoreFunc; lo, hi: Integer);
  procedure swap( a, b: Integer );
  var
    Element: TP3DRenderObject;
  begin
    Element:= Self[ a ];
    FItems[ a ]:= Self[ b ];
    FItems[ b ]:= Element;
  end;

  function partition( lo, hi: Integer ): Integer;
  var
    pivot, i, j: Integer;
  begin
    pivot:= Func( Self[ hi ]);
    i:= lo;        // place for swapping
    for j := lo to hi - 1 do
      if ( Func( Self[ j ]) <= pivot ) then
        begin
          swap( i, j );
          Inc( i );
        end;
    swap( i, hi );
    Result:= i;
  end;

  procedure quicksort( lo, hi: Integer );
  var
    p: Integer;
  begin
    if lo < hi then
      begin
        p:= partition( lo, hi );
        quicksort( lo, p - 1 );
        quicksort( p + 1, hi );
      end;
  end;

begin
  quicksort( lo, hi );
end;

procedure TP3DRenderList.Optimize;
var
  i: Integer;
begin
  Sort( @P3DRenderListScoreTransp, 0, Count - 1 );

  FTransparencyStart:= 0;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Material.HasTransparency ) then
      begin
        FTransparencyStart:= i;
        break;
      end;
end;

{ TP3DScene }

procedure TP3DScene.SetCam(AValue: TP3DActor);
begin
  if FCam=AValue then Exit;

  if ( Assigned( FCam )) then
    FCam.RemoveUser( Self );

  FCam:= AValue;

  if ( Assigned( FCam )) then
    FCam.AddUser( Self );
end;

procedure TP3DScene.RenderCallback(world: TMat4; AActor: TP3DActor);
begin
  if ( Assigned( OnRenderCallback )) then
    OnRenderCallback( world, AActor );
end;

procedure TP3DScene.OnObjChange( Sender: TPersistent; Item: TP3DActor; Action: TP3DCustomListOnChangeAction );
begin
  if ( Action = actAdd ) then
    begin
      Item.AddUser( Self );
      Item.FScene:= Self;
    end
  else
    begin
      Item.RemoveUser( Self );
      Item.FScene:= nil;
    end;
end;

constructor TP3DScene.Create( const AData: TP3DData );
begin
  inherited Create( AData );
  FObjects:= TP3DActorList.Create( Self );
  FObjects.OnChange:= @OnObjChange;
  Lights:= TP3DLightList.Create;
  Owner.Scenes.Add( Self );
end;

constructor TP3DScene.Create( AFileName: String;
  const AWatchFileForChanges: Boolean; const AData: TP3DData );
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  Create( AData );
  AppendFile( AFileName );
end;

procedure TP3DScene.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  Child: TP3DActor;
  CamName: DOMString;
begin
  inherited LoadFromDOM( ADOMNode );
  Name:= ADOMNode.GetAttribute( 'name' );

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'object': begin Child:= TP3DActor.CreateFromDOM( Element, Owner ); Objects.Add( Child ); end;
      else
        raise Exception.Create( 'Unknown tag inside scene element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  CamName:= ADOMNode.GetAttribute( 'camera' );
  if ( CamName > '' ) then
    Cam:= Objects[ Objects.FindByName( CamName )];
end;

{
constructor TP3DScene.CreateFromDOM(ADOMNode: TDOMElement; const AData: TP3DData );
begin
  if ( not Assigned( ADOMNode )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Not Assigned!' );

  if ( not ( ADOMNode.NodeName = 'p3dscene' )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Scenes must begin with p3dscene!' );

  Create( AData );

  AppendFromDom( ADOMNode );
end;}

procedure TP3DScene.FastDestroy;
begin
  Objects.Free;
  Lights.Free;
  Owner.Scenes.Remove( Self );
  inherited FastDestroy;
end;

procedure TP3DScene.ClearFields;
begin
  Cam:= nil;
  Lights.Clear();
  Objects.Clear();
  ActiveCam:= nil;
  RootScene:= nil;
  inherited ClearFields;
end;

procedure TP3DScene.UpdateLights;
var
  Obj: TP3DActor;
  light: TP3DLight;
begin
  Lights.Clear();
  for Obj in Objects do
    if ( Obj.Data is TP3DLight ) then
      Lights.Add( TP3DLight( Obj.Data ))
    else if ( Obj.Data is TP3DScene ) then
      begin
        TP3DScene( Obj.Data ).UpdateLights;
        for light in TP3DScene( Obj.Data ).Lights do
          Lights.Add( light );
      end;
  {WriteLn( Name + ' light list:' );
  for light in Lights do
    WriteLn( ExtractFileName( light.Parent.FileWatch.FileName ) + '.' + light.Name );
  WriteLn( '-------------------' );}
end;


procedure TP3DScene.Clear;
begin
  Objects.Clear( False );
end;

procedure TP3DScene.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DActor);
begin
  inherited Render(world, Scene, RenderObject);
  if ( Assigned( Scene )) then
    if ( Assigned( Scene.RootScene )) then
      RootScene:= Scene.RootScene
    else
      begin
        RootScene:= Scene;
        RootScene.UpdateLights;
      end;
  if ( Scene.FInheritedDesignTime or IsDesignTime ) then
    FInheritedDesignTime:= True;
  if ( Assigned( Cam )) then
    begin
      ActiveCam:= Cam;
      if ( Assigned( Cam.Data )) then
        TP3DCamera( Cam.Data ).UpdateMatrices( Cam );
      Objects.Render( world, Self );
    end
  else
    RenderFromCamera( Scene.ActiveCam, world );
end;

procedure TP3DScene.Render( const ARootScene: TP3DScene = nil );
begin
  ActiveCam:= Cam;
  RootScene:= ARootScene;
  if ( Assigned( Cam.Data )) then
    TP3DCamera( Cam.Data ).UpdateMatrices( Cam );
  UpdateLights;
  Objects.Render( Mat4Identity, Self );
end;

procedure TP3DScene.MakeRenderList( ARootScene: TP3DScene; world: TMat4;
  var ARenderList: TP3DRenderList);
begin
  if ( not Assigned( ARenderList )) then
    ARenderList:= TP3DRenderList.Create;
  if ( not Assigned( ARootScene )) then
    ARootScene:= Self;

  Objects.MakeRenderList( ARootScene, world, ARenderList );
end;

procedure TP3DScene.RenderFromCamera(ACam: TP3DActor);
begin
  RenderFromCamera( ACam, Mat4Identity );
end;

procedure TP3DScene.RenderFromCamera(ACam: TP3DActor; world: TMat4);
begin
  ActiveCam:= ACam;
  if ( Assigned( ACam.Data )) then
    TP3DCamera( ACam.Data ).UpdateMatrices( ACam );
  Objects.Render( world, Self );
end;

procedure TP3DScene.AppendFile( AFileName: String );
var
  n: Integer;
begin
  n:= OpenLibrary( AFileName );
  AppendFromData( P3DData.Libraries[ n ]);
end;

procedure TP3DScene.AppendFromData(AData: TP3DData);
var
  Scene: TP3DScene;
  Obj: TP3DActor;
begin
  for Scene in AData.Scenes do
    begin
      Obj:= TP3DActor.Create();
      Obj.Data:= Scene;
      Objects.Add( Obj );
      //Obj.Users.Add( Self );
    end;
end;

procedure TP3DScene.PassToShader;
var
  LightList: TP3DLightList;
  decl: TP3DShaderDecl;
begin
  if ( Assigned( ActiveCam )) then
    begin
      if ( Assigned( RootScene )) then
        LightList:= RootScene.Lights
      else
        LightList:= Lights;

      if ( ActiveCam.Data is TP3DCamera ) then
        begin
          //TP3DCamera( ActiveCam.Data ).UpdateMatrices( ActiveCam );
          TP3DCamera( ActiveCam.Data ).PassToShader;
          LightList.PassToActiveShader( TP3DCamera( ActiveCam.Data ).View, [ liPosition, liLightParams ]);
        end;
    end;
end;

procedure TP3DScene.RemoveFromFields( Block: TP3DDataBlock );
begin
  if ( Block = Cam ) then
    Cam:= nil;
  if ( Block is TP3DActor ) then
    Objects.Remove( TP3DActor( Block ));
  inherited RemoveFromFields( Block );
end;

{
function TP3DScene.Pick(X, Y: Integer; const ARootScene: TP3DScene): TP3DActor;
begin
  if ( not Assigned( ARootScene )) then
    begin
      glScissor( X, Y, 1, 1 );
      glEnable( GL_SCISSOR_TEST );
      if ( not Assigned( PickMaterial )) then
        PickMaterial.Create();
      PickMaterial.Diff:= vec3( 0 );
      PickMaterial.PassToShader();
    end;

  RenderCallbackOnly();

  if ( not Assigned( ARootScene )) then
    begin
      glDisable( GL_SCISSOR_TEST );
    end;
end;
}

{$ENDIF}
