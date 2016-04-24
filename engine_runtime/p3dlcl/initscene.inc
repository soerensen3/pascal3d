var
  mainscene: TP3DScene;
  maincamera: TP3DActor;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../p3dlcl/media';
  pathfonts: String = '../../../fonts';

  dejavu: TP3DFont;
  txt: TP3DText;

  CatchMouse: Boolean = False;

function LoadShader: Boolean;
begin
  Result:= False;
  SetCurrentDir( pathshaders );
  P3DShaderNodeLib.LoadLibraryPath( 'nodes/core/', '*.pmd' );
  //LOAD SHADERS
end;

procedure UnloadShader;
begin
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

procedure LoadFonts;
begin
  SetCurrentDir( pathfonts );
  P3DFontManager.DefaultFontsPath:= pathfonts;
end;

procedure UnloadFonts;
begin
  P3DFontManager.Clear();
  //dejavu.Free;
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

procedure Init( Sender: TP3DWindow );
begin
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  Sender.Width:= 800;
  Sender.Height:= 600;

  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  mainscene:= TP3DScene.Create;
  maincamera:= TP3DActor.Create;
  maincamera.Data:= TP3DCamera.Create();

  LoadModels;
  InitPhysics;
  LoadTextures;
  LoadShader;

  LoadFonts;

  P3DGUIInit;
  P3DGUIManager.Window:= P3DApplication.MainWindow;
  //mainscene.DrawObjects:= @DrawObjects;
  //mainscene.Cam:= maincamera;

  //SimpleControl:= TP3DGraphicControl.Create( ParentList, GUIManager );
  //SimpleControl.Width:= 400;
  //SimpleControl.Height:= 300;
  with ( TP3DEdit.Create( P3DData.DataBlocks, P3DGUIManager )) do
    begin
      Left:= 20;
      Top:= 100;
      Width:= 200;
      Height:= 50;
      Font.Size:= 32;
      Font.Name:= 'DejaVuSans';
      Caption:= 'Wurst';
    end;
  //Setup2D( mainwnd.Width, mainwnd.Height );

  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));
  //p3dviewports.Push( 0, 0, mainwnd.Width, mainwnd.Height );

  //txt:= p3dTextSimple( 'Wurst', dejavu, 64 );
end;

procedure DeInit( Sender: TP3DApplication );
begin
  mainscene.Free;
  maincamera.Free;
  //ParentList.Free;

  DeInitPhysics;

  UnloadShader;
  UnloadModels;
  UnloadFonts;
  txt.Free;
end;

