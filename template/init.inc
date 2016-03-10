{$IFDEF INTERFACE}
var
  mainscene: TP3DModelScene;
  maincamera: TP3DCamera;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../template/media';
  pathfonts: String = '../../../fonts';

  CatchMouse: Boolean = False;

function LoadShader: Boolean;
procedure UnloadShader;
function LoadModels: Boolean;
procedure UnloadModels;
procedure LoadTextures;
procedure InitPhysics;
procedure InitOpenGL;
procedure DeInitPhysics;
procedure Init( Sender: TP3DApplication );
procedure DeInit( Sender: TP3DApplication );
{$ENDIF}
{$IFDEF IMPLEMENTATION}

function LoadShader: Boolean;
begin
  Result:= False;
  SetCurrentDir( pathshaders );

  //LOAD SHADERS
end;

procedure UnloadShader;
begin
  //FREE SHADERS
end;

function LoadModels: Boolean;
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
  //INIT PHYSICS
end;

procedure InitOpenGL;
begin
  P3DLog.LogInfo( nil, 'InitGL');
  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);    // Set the Background colour
end;

procedure DeInitPhysics;
begin
  //FREE PHYSICS
end;

procedure Init(Sender: TP3DApplication);
begin
  //SETUP PATHS
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  //SET WINDOW RESOLUTION
  Sender.MainWindow.Width:= 800;
  Sender.MainWindow.Height:= 600;
  Sender.MainWindow.Title:= '__PROJNAME__';

  //CRETATE THE SCENE AND A CAMERA
  mainscene:= TP3DModelScene.Create;
  maincamera:= TP3DCamera.Create;

  LoadModels;
  InitPhysics;
  LoadTextures;
  LoadShader;

  //SET SCENE RENDER PROCEDURE
  mainscene.DrawObjects:= @DrawObjects;
  mainscene.Cam:= maincamera;

  //CATCH THE MOUSE IF NEEDED
  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));

  //SETUP SOME OPENGL INITIALIZATIONS
  InitOpenGL;
end;

procedure DeInit(Sender: TP3DApplication);
begin
  mainscene.Free;
  maincamera.Free;

  DeInitPhysics;

  UnloadShader;
  UnloadModels;
end;
{$ENDIF}
