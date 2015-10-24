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
  P3DShaderLib.LoadLibraryPath( 'nodes/core/', '*.xml' );
  for mat in mainscene.Materials do
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
  i: Integer;
begin
  Result:= False;
  SetCurrentDir( pathmodels );

  mainscene.AppendFile( 'rocks.p3d' ); //'island.blend.p3d' );
  //TArmature( Mdl.Children[ 1 ]).CurrentAction:= 0;
  WriteLn( mainscene.Objects.OutputDebugInfo );
  //for i:= 0 to mainscene.Children.Count - 1 do
  //  WriteLn( mainscene.Children[ i ].ClassName + ': ' + mainscene.Children[ i ].Name );
  {for i:= 0 to mainscene.Lights.Count - 1 do
    begin
      WriteLn( mainscene.Lights[ i ].Direction.ToString());
      //mainscene.Lights[ i ].Position:= -mainscene.Lights[ i ].Direction * 10;
      mainscene.Lights[ i ].LinearAttenuation:= 0;
      mainscene.AppendFile( 'bulb.p3d', mainscene.Lights[ i ]);
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

procedure Init( Sender: TSDLWindow );
begin
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  Sender.Width:= 800;
  Sender.Height:= 600;

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

procedure DeInit( Sender: TSDLWindow );
begin
  mainscene.Free;
  maincamera.Free;

  DeInitPhysics;

  UnloadShader;
  UnloadModels;
end;

