var
  mainscene: tScene;
  maincamera: tCamera;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../p3dlcl/media';
  pathfonts: String = '../../../fonts';

  dejavu: TP3DFont;
  txt: TP3DText;

  CatchMouse: Boolean = False;

  font_shader: TShader;
  ParentList: TObjectList;
  SimpleControl: TP3DGraphicControl;

function LoadShader: Boolean;
begin
  Result:= False;
  SetCurrentDir( pathshaders );

  InitGeom( CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )),
            CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )));
  mainscene.Shader:= LineShader2D;
  font_shader:= CreateVertexAndFragmentShader( LoadShaderToText( 'font.vert' ), LoadShaderToText( 'font.frag' ));
  P3DFontManager.Shader:= font_shader;
  //LOAD SHADERS
end;

procedure UnloadShader;
begin
  font_shader.Free;
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
  dejavu:= LoadP3DFont( 'Deja Vu Sans.p3dfont' );
  dejavu.SizeFactor:= 10;
  P3DFontManager.Add( dejavu );
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

  LoadFonts;

  mainscene.DrawObjects:= @DrawObjects;
  mainscene.Cam:= maincamera;

  ParentList:= TObjectList.Create;
  //SimpleControl:= TP3DGraphicControl.Create( ParentList, GUIManager );
  //SimpleControl.Width:= 400;
  //SimpleControl.Height:= 300;
  with ( TP3DEdit.Create( ParentList, GUIManager )) do
    begin
      Left:= 20;
      Top:= 100;
      Width:= 200;
      Font.Size:= 32;
      Font.Name:= 'Deja Vu Sans';;
      Caption:= 'Wurst';
    end;
  //Setup2D( mainwnd.Width, mainwnd.Height );

  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));
  p3dviewports.Push( 0, 0, mainwnd.Width, mainwnd.Height );

  //txt:= p3dTextSimple( 'Wurst', dejavu, 64 );
end;

procedure DeInit( Sender: TSDLWindow );
begin
  mainscene.Free;
  maincamera.Free;
  ParentList.Free;

  DeInitPhysics;

  UnloadShader;
  UnloadModels;
  UnloadFonts;
  txt.Free;
end;

