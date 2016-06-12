//TODO: CHECK IF BOUNDING BOX IS CALCULATED CORRECTLY

var
  mainscene: TP3DScene;
  skyboxscene: TP3DScene;
  maincamera: TP3DActor;
  skycamera: TP3DActor;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../armature_test/media';
  pathfonts: String = '../../../fonts';

  //shadersimple: TShader;

  CatchMouse: Boolean = False;
  CamYawPitchRoll: TVec3;
  CamPos: TVec3;
  FPS: Single;
  FramesCounter: Integer;
  FPSLastPoll: Integer;



const
  FPSUpdate: Integer = 2000;

  //Mdl: TP3DModelScene;


procedure ReloadShader;
var
  Shader: TP3DShader;
  Lib: TP3DData;
begin
  for Shader in P3DData.Shaders do
    Shader.Reload;
  for Lib in P3DData.Libraries do
    for Shader in Lib.Shaders do
      Shader.Reload;
end;

function LoadShader: Boolean;
begin
  Result:= False;
  SetCurrentDir( pathshaders );

  {shadersimple:= CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' ));

  InitGeom( CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )),
            CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )));
  //mainscene.Shader:= shadersimple; }
  P3DShaderNodeLib.LoadLibraryPath( 'nodes/core/', '*.pmd' );

  //LOAD SHADERS
end;

procedure UnloadShader;
begin
  P3DShaderNodeLib.Nodes.Clear( True );
  //FREE SHADERS
end;

function LoadModels: Boolean;
var
  Obj, i, n: Integer;
  BBox: TP3DBoundingBox;
  lib: TP3DData;

begin
  Result:= False;
  SetCurrentDir( pathmodels );

  //mainscene.AppendFile( 'lights.p3d' );

  //mainscene.AppendFile( 'makehuman.p3d' );
  //mainscene.AppendFile( 'rocks.p3d' );
  //mainscene.AppendFile( 'terrain_simple.p3d' );
  //mainscene.AppendFile( 'grid_projterrain_high.p3d' );
  //mainscene.AppendFile( 'grid_projterrain_med.p3d' );
  mainscene.AppendFile( 'grid_projterrain_low.p3d' );

  skyboxscene.AppendFile( 'skybox.p3d' );
  skyboxscene.AppendFile( 'sun.p3d' );

  {Obj:= skyboxscene.Objects.FindByName( 'Plane' );
  if ( Obj >= 0 ) then
    with ( skyboxscene.Objects[ Obj ] as TP3DActor ) do
      begin
        //TP3DMesh( mainscene.Objects[ Obj ].Data ).MaterialGroups[ 0 ].Material.Unlit:= True;
        Position:= Direction * 0.7;
        Scale:= vec3( 0.1 );
        Quaternion:= quat( 0.573576, 0.000000, 0.000000, 0.819152 );
//      Matrix:= mat4scale( vec4(( BBox.Max - BBox.Min ) / 2, 1 )) * mat4translate( vec4( BBox.Center, 1 ));
      end;}

  BBox:= mainscene.Objects.CalcBoundingBox();
  //BBox:= TP3DRenderableObject( mainscene.Objects[ 3 ]).CalcBoundingBox();

  //mainscene.AppendFile( 'bbox.p3d' );

  //mainscene.AppendFile( 'grid.p3d' );
  //n:= OpenLibrary( 'bulb.p3d' );
  //mainscene.AppendFile( 'bulb.p3d', nil, True );

  Obj:= mainscene.Objects.FindByName( 'grid' );
  if ( Obj >= 0 ) then
    with ( mainscene.Objects[ Obj ] as TP3DActor ) do
      begin
        //TP3DMesh( mainscene.Objects[ Obj ].Data ).MaterialGroups[ 0 ].Material.Unlit:= True;
        Position:= vec3( 0, 0, -1 );
        Scale:= vec3( 10 );
//      Matrix:= mat4scale( vec4(( BBox.Max - BBox.Min ) / 2, 1 )) * mat4translate( vec4( BBox.Center, 1 ));
      end;

  //TArmature( Mdl.Children[ 1 ]).CurrentAction:= 0;
  WriteLn( mainscene.Objects.OutputDebugInfo );
  //for i:= 0 to mainscene.Children.Count - 1 do
  //  WriteLn( mainscene.Children[ i ].ClassName + ': ' + mainscene.Children[ i ].Name );
  {for i:= 0 to mainscene.Objects.Count - 1 do
    if ( TP3DActor( mainscene.Objects[ i ]).Data is TP3DLight ) then
      begin
        //WriteLn( mainscene.Lights[ i ].Direction.ToString());

        //TP3DRenderableObject( mainscene.Objects[ i ]).Position:= -mainscene.Lights[ i ].Position;
        //mainscene.Lights[ i ].LinearAttenuation:= 0;
        mainscene.AppendFromData( P3DData.Libraries[ n ]);
      end;}
  //LOAD MODELS

  {for mat in P3DData.Materials do
    mat.BuildShader;

  for lib in P3DData.Libraries do
    for mat in lib.Materials do
      begin
        mat.Unlit:= True;
        //mat.BuildShader;
      end;}
end;

procedure UnloadModels;
begin
  mainscene.Clear;
  //FREE MODELS
end;

procedure LoadTextures;
begin
  //LOAD TEXTURES
end;


procedure InitPhysics;
begin
  //INIT PHYSICS IF NEEDED
end;

procedure DeInitPhysics;
begin
  //FREE PHYSICS
end;

procedure Init( Sender: TP3DApplication );
var
  SceneViewer: TP3DTreeViewActors;
begin
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  Sender.MainWindow.Width:= 800;
  Sender.MainWindow.Height:= 600;
  Sender.MainWindow.FullScreen:= False;

  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  mainscene:= TP3DScene.Create();
  maincamera:= TP3DActor.Create();
  maincamera.Position:= vec3( 0, 0, -10 );
  maincamera.Data:= TP3DCamera.Create();
  maincamera.RotationOrder:= roZYX;
  TP3DCamera( maincamera.Data ).Far:= 2000;

  skyboxscene:= TP3DScene.Create();
  skycamera:= TP3DActor.Create();
  skycamera.Position:= vec3( 0, 0, 0 );
  skycamera.Data:= TP3DCamera.Create();
  skycamera.RotationOrder:= roZYX;
  TP3DCamera( skycamera.Data ).Far:= 2000;
  skyboxscene.Cam:= skycamera;

  LoadShader;
  LoadModels;
  InitPhysics;
  LoadTextures;

  mainscene.Cam:= maincamera;

  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));
  glEnable( GL_DEPTH_TEST );
  glCullFace( GL_NONE );

  P3DGUIInit;
  //P3DGUIManager.Window:= P3DApplication.MainWindow;
  {SceneViewer:= TP3DTreeViewActors.Create();
  SceneViewer.Width:= 200;
  //SceneViewer.Height:= 600;
  SceneViewer.ShowSymbols:= True;
  SceneViewer.Update;
  SceneViewer.Children.ExpandAll( True );
  SceneViewer.Align:= alLeft;
  SceneViewer.Visible:= False;}
end;

procedure DeInit( Sender: TP3DApplication );
begin
  DeInitPhysics;
  UnloadShader;
  UnloadModels;
  maincamera.Free;
  mainscene.Free;
end;

