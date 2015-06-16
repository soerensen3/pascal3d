var
  mainscene: tScene;
  maincamera: tCamera;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../armature_test/media';
  pathfonts: String = '../../../fonts';

  shadersimple: TShader;

  CatchMouse: Boolean = False;

  Mdl: TModelFile;


function LoadShader: Boolean;
begin
  Result:= False;
  SetCurrentDir( pathshaders );

  shadersimple:= CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' ));

  InitGeom( CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )),
            CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )));
  mainscene.Shader:= shadersimple;
  //LOAD SHADERS
end;

procedure UnloadShader;
begin
  LineShader2D.Free;
  LineShader3D.Free;
  shadersimple.Free;
  //FREE SHADERS
end;

function LoadModels: Boolean;
var
  i: Integer;
begin
  Result:= False;
  SetCurrentDir( pathmodels );

  Mdl:= LoadModelFileFromFile( 'character_bones_walk.model' );
  TArmature( Mdl.Children[ 1 ]).CurrentAction:= 0;
  for i:= 0 to Mdl.Children.Count - 1 do
    WriteLn( 'Object: ' + Mdl.Children[ i ].Name );
  //LOAD MODELS
end;

procedure UnloadModels;
begin
  Mdl.Free;
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

  mainscene:= tScene.Create;
  maincamera:= tCamera.Create;

  LoadModels;
  InitPhysics;
  LoadTextures;
  LoadShader;

  mainscene.DrawObjects:= @DrawObjects;
  mainscene.Cam:= maincamera;

  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));
end;

procedure DeInit( Sender: TSDLWindow );
begin
  mainscene.Free;
  maincamera.Free;

  DeInitPhysics;

  UnloadShader;
  UnloadModels;
end;
