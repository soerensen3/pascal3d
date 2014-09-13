var
  mainscene: tScene;
  maincamera: tCamera;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../line_test/media';
  pathfonts: String = '../../../fonts';

  CatchMouse: Boolean = False;

  points: array of TVec2;


function LoadShader: Boolean;
begin
  Result:= False;
  SetCurrentDir( pathshaders );

  InitGeom( CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )),
            CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )));
//  InitGeom( CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' )));
  Setup2D( 800, 600 );
  //LOAD SHADERS
end;

procedure UnloadShader;
begin
  LineShader2D.Free;
  LineShader3D.Free;
  //FREE SHADERS
end;

function LoadModels: Boolean;
var
  i: Integer;
begin
  Result:= False;
  SetCurrentDir( pathmodels );

  //LOAD MODELS
end;

procedure UnloadModels;
begin
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

