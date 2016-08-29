//p3dscene.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
type

  { TP3DRenderObject }

  TP3DRenderObject = object
    private
      FActor: TP3DActor;
      FMatrix: TMat4;
      FScene: TP3DScene;

    public
      property Actor: TP3DActor read FActor write FActor;
      property Scene: TP3DScene read FScene write FScene;
      property Matrix: TMat4 read FMatrix write FMatrix;
  end;

  TP3DCustomRenderList = specialize gP3DCustomList < TP3DRenderObject >;

  { TP3DRenderList }

  TP3DRenderList = class ( TP3DCustomRenderList )
    private
      FLights: TP3DLightList;

    public
      constructor Create;
      destructor Destroy; override;

      function Add( AActor: TP3DActor; world: TMat4; AScene: TP3DScene ): Integer;
      function PickFromCamera( X, Y: Integer; Camera: TP3DActor ): TP3DActor;
      function Find( AActor: TP3DActor ): Integer;
      procedure Render;

      property Lights: TP3DLightList read FLights write FLights;
  end;

  { TP3DScene }

  TP3DSceneRenderCallback = procedure ( world: TMat4; AActor: TP3DActor ) of object;
  TP3DScene = class( TP3DDataBlock )
    private
      FCam: TP3DActor;
      FIsDesignTime: Boolean;
      FLights: TP3DLightList;
      FObjects: TP3DActorList;
      FActiveCam: TP3DActor;
      FDesignTime: Boolean;
      FOnRenderCallback: TP3DSceneRenderCallback;
      FRootScene: TP3DScene;

      procedure SetCam(AValue: TP3DActor);
      procedure RenderCallback( world: TMat4; AActor: TP3DActor ); virtual;
      procedure OnObjChange(Sender: TPersistent; Item: TP3DActor; Action: TP3DCustomListOnChangeAction);

    protected
      property ActiveCam: TP3DActor read FActiveCam write FActiveCam;
      property RootScene: TP3DScene read FRootScene write FRootScene;
      property Lights: TP3DLightList read FLights write FLights;

    public
      constructor Create( const AData: TP3DData = nil );
      constructor Create( AFileName: String; const AWatchFileForChanges: Boolean = False; const AData: TP3DData = nil );
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil );
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      destructor Destroy; override;
      procedure UpdateLights;

      procedure Clear;

//      function Debug: String;

      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor); override;
      procedure Render(const ARootScene: TP3DScene=nil); reintroduce;
      procedure MakeRenderList(ARootScene: TP3DScene; world: TMat4;
        var ARenderList: TP3DRenderList);
      procedure RenderFromCamera( ACam: TP3DActor );
      procedure RenderFromCamera( ACam: TP3DActor; world: TMat4 );
      procedure AppendFile( AFileName: String );
      procedure AppendFromData( AData: TP3DData );
      procedure PassToShader( world: TMat4 ); virtual;
      procedure NotifyDeleteOfData(Sender: TP3DDataBlock); override;
      //function Pick( X,Y: Integer; const ARootScene: TP3DScene=nil ): TP3DActor;

      property Objects: TP3DActorList read FObjects;
      property Cam: TP3DActor read FCam write SetCam;
      property IsDesignTime: Boolean read FIsDesignTime write FIsDesignTime;
      property OnRenderCallback: TP3DSceneRenderCallback read FOnRenderCallback write FOnRenderCallback;
  end;
  TP3DSceneList = specialize gP3DCustomDatablockList < TP3DScene >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DRenderList }

constructor TP3DRenderList.Create;
begin
  inherited Create;
  Lights:= TP3DLightList.Create;
end;

destructor TP3DRenderList.Destroy;
begin
  FreeAndNil( FLights );
  inherited Destroy;
end;

function TP3DRenderList.Add(AActor: TP3DActor; world: TMat4; AScene: TP3DScene): Integer;
var
  Obj: TP3DRenderObject;
begin
  Obj.Actor:= AActor;
  Obj.Matrix:= world;
  Obj.Scene:= AScene;
  if ( AActor.Data is TP3DLight ) then
    Lights.Add( AActor.Data as TP3DLight );
  Result:= inherited Add( Obj );
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

function TP3DRenderList.Find(AActor: TP3DActor): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Self[ i ].Actor = AActor ) then
      begin
        Result:= i;
        break;
      end;
end;

procedure TP3DRenderList.Render;
begin
  //Lights.PassToActiveShader( );
end;

{ TP3DScene }

procedure TP3DScene.SetCam(AValue: TP3DActor);
begin
  if FCam=AValue then Exit;

  if ( Assigned( FCam )) then
    FCam.Users.Delete( FCam.Users.IndexOf( Self ));

  FCam:= AValue;

  if ( Assigned( FCam )) then
    FCam.Users.Add( Self );
end;

procedure TP3DScene.RenderCallback(world: TMat4; AActor: TP3DActor);
begin
  if ( Assigned( OnRenderCallback )) then
    OnRenderCallback( world, AActor );
end;

procedure TP3DScene.OnObjChange(Sender: TPersistent; Item: TP3DActor; Action: TP3DCustomListOnChangeAction);
begin
  if ( Action = actAdd ) then
    Item.Users.Add( Self )
  else if ( Action in [ actDelete, actClear ]) then
    Item.Users.Remove( Self );
end;

constructor TP3DScene.Create(const AData: TP3DData);
begin
  inherited Create( AData );
  FObjects:= TP3DActorList.Create;
  FObjects.OnChange:= @OnObjChange;
  Lights:= TP3DLightList.Create;
  Parent.Scenes.Add( Self );
end;

constructor TP3DScene.Create(AFileName: String;
  const AWatchFileForChanges: Boolean; const AData: TP3DData);
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  inherited Create( AData );
  AppendFile( AFileName );
end;

constructor TP3DScene.CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData );
begin
  inherited;
  LoadFromDOM( ADOMNode );
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
        'object': begin Child:= TP3DActor.CreateFromDOM( Element, Parent ); Objects.Add( Child ); end;
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

destructor TP3DScene.Destroy;
begin
  Clear;
  Objects.Free;
  Lights.Clear();
  Lights.Free;
  inherited Destroy;
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
var
  i: Integer;
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
  if ( Scene.FDesignTime or IsDesignTime ) then
    FDesignTime:= True;
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

procedure TP3DScene.PassToShader(world: TMat4);
var
  LightList: TP3DLightList;
  decl: TP3DShaderDecl;
begin
  if ( Assigned( ActiveCam )) then
    begin
      //LightList:= TP3DLightList.Create;
      {for Obj in Objects do
        if ( Obj.Data is TP3DLight ) then
          LightList.Add( TP3DLight( Obj.Data ));}
      if ( Assigned( RootScene )) then
        LightList:= RootScene.Lights
      else
        LightList:= Lights;

      if ( ActiveCam.Data is TP3DCamera ) then
        begin
          //TP3DCamera( ActiveCam.Data ).UpdateMatrices( ActiveCam );
          TP3DCamera( ActiveCam.Data ).PassToShader( world );
          LightList.PassToActiveShader( 0, Self, world, TP3DCamera( ActiveCam.Data ).View, [ liPosition, liLightParams ]);
        end;
      if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
        decl.AsMat4:= world;
    end;
end;

procedure TP3DScene.NotifyDeleteOfData(Sender: TP3DDataBlock);
begin
  inherited NotifyDeleteOfData( Sender );
  if ( Sender = Cam ) then
    Cam:= nil;
  if ( Sender is TP3DActor ) then
    Objects.Remove( TP3DActor( Sender ));
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
