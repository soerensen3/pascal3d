//TODO: CHECK IF BOUNDING BOX IS CALCULATED CORRECTLY

var
  mainscene: TP3DModelScene;
  maincamera: TP3DCamera;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../armature_test/media';
  pathfonts: String = '../../../fonts';

  shadersimple: TShader;

  CatchMouse: Boolean = False;

  //Mdl: TP3DModelScene;


function LoadShader: Boolean;
var
  mat: TP3DMaterial;
begin
  Result:= False;
  SetCurrentDir( pathshaders );

  shadersimple:= CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' ));

  InitGeom( CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )),
            CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )));
  mainscene.Shader:= shadersimple;
  P3DShaderLib.LoadLibraryPath( 'nodes/core/', '*.pmd' );

  for mat in P3DData.Materials do
    mat.BuildShader;
  //LOAD SHADERS
end;

procedure UnloadShader;
begin
  LineShader2D.Free;
  LineShader3D.Free;
  shadersimple.Free;
  P3DShaderLib.Nodes.Clear( True );
  //FREE SHADERS
end;

function LoadModels: Boolean;
var
  Obj, i: Integer;
  BBox: TP3DBoundingBox;
begin
  Result:= False;
  SetCurrentDir( pathmodels );

  //mainscene.AppendFile( 'lights.p3d' );

  //mainscene.AppendFile( 'makehuman.p3d' );
  mainscene.AppendFile( 'rocks.p3d' );

  BBox:= mainscene.Objects.CalcBoundingBox();
  //BBox:= TP3DRenderableObject( mainscene.Objects[ 3 ]).CalcBoundingBox();

  //mainscene.AppendFile( 'bbox.p3d' );

  mainscene.AppendFile( 'grid.p3d' );
  //mainscene.AppendFile( 'bulb.p3d', nil, True );

  Obj:= mainscene.Objects.FindByName( 'BBox' );
  if ( Obj >= 0 ) then
    with ( mainscene.Objects[ Obj ] as TP3DActor ) do
      Matrix:= mat4scale( vec4(( BBox.Max - BBox.Min ) / 2, 1 )) * mat4translate( vec4( BBox.Center, 1 ));

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
        mainscene.AppendFile( 'bulb.p3d', TP3DActor( mainscene.Objects[ i ]), True );
      end;}
  //LOAD MODELS
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
begin
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  Sender.MainWindow.Width:= 800;
  Sender.MainWindow.Height:= 600;

  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  mainscene:= TP3DModelScene.Create;
  maincamera:= TP3DCamera.Create;

  LoadShader;
  LoadModels;
  InitPhysics;
  LoadTextures;

  mainscene.DrawObjects:= @DrawObjects;
  mainscene.Cam:= maincamera;

  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));
  glEnable( GL_DEPTH_TEST );
  glCullFace( GL_BACK );
end;

procedure DeInit( Sender: TP3DApplication );
begin
  DeInitPhysics;
  UnloadShader;
  UnloadModels;
  maincamera.Free;
  mainscene.Free;
end;

