var
  mainscene: tScene;
  maincamera: tCamera;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../template/media';
  pathfonts: String = '../../../fonts';

  CatchMouse: Boolean = False;

  font_shader: TShader;
  ParentList: TObjectList;
  dejavu: TP3DFont;

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
var
  grpbx: TP3DGroupBox;
begin
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  Sender.Width:= 800;
  Sender.Height:= 600;
  //Sender.FullScreen:= True;

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
  Setup2D( 0, 0, mainwnd.Width, mainwnd.Height );

  p3dviewports.Push( 0, 0, mainwnd.Width, mainwnd.Height );


  grpbx:= TP3DGroupBox.Create( ParentList, GUIManager );
  with ( grpbx ) do
    begin
      Left:= 10;
      Top:= 10;
      Height:= 100;
      Width:= 300;
      Color:= vec4( vec3( 1 ), 0.5 );
      with ( TP3DEdit.Create( ParentList, GUIManager )) do
      begin
        Parent:= grpbx;
        Align:= alTop;
      end;
    end;

  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));
end;


procedure DeInit( Sender: TSDLWindow );
begin
  mainscene.Free;
  maincamera.Free;

  DeInitPhysics;

  UnloadShader;
  UnloadModels;
  UnloadFonts;
end;

