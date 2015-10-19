type

  { TEventsHandler }

  TEventsHandler = object
    procedure OptionsBtnClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
    procedure NodeDeleteClick( Sender: TObject );
    procedure CompileItmClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
    procedure AddItmClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
    procedure DrawScene( Sender: TObject );
  end;

var
  mainscene: tScene;
  maincamera: tCamera;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../shadernodes/media';
  pathfonts: String = '../../../fonts';

  CatchMouse: Boolean = False;

  font_shader: TShader;
  ParentList: TP3DObjectList;
  dejavu: TP3DFont;

  //canvas: TP3DCanvas2D;

  //shadernodeoutline: TP3DShaderNodeOutline;
  optionsBtn: TP3DButton;
  events: TEventsHandler;

  guiscene: TP3DShaderPreview;
  TestPopupMenu: p3dgui_menus.TP3DPopupMenu;
  mdlCube: TP3DScene;

  NodeTree: TP3DShaderNodeTree;
  tex1,tex2,tex3,tex4,tex5: TP3DTexture;
  //guisno: TP3DShaderNodeOutlineFragment;

function LoadShader: Boolean;
var
  tmp: TMat4;
begin
  Result:= False;
  SetCurrentDir( pathshaders );

  InitGeom( CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )),
            CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )));
  mainscene.Shader:= LineShader2D;
  font_shader:= CreateVertexAndFragmentShader( LoadShaderToText( 'font.vert' ), LoadShaderToText( 'font.frag' ));
  TriangleScene.Shader:= CreateVertexAndFragmentShader( LoadShaderToText( 'default3d.vert' ), LoadShaderToText( 'default3d.frag' ));
  P3DFontManager.Shader:= font_shader;
  P3DShaderLib.LoadLibraryPath( 'nodes/core/', '*.xml' );
  //shadernodeoutline:= TP3DShaderNodeOutline.CreateFromFile( 'nodes/shader_test2.xml' );

  //LOAD SHADERS
end;

procedure UnloadShader;
begin
  P3DShaderLib.Nodes.Clear( True );
  font_shader.Free;
  //shadernodeoutline.Free;
  //FREE SHADERS
end;

procedure LoadFonts;
begin
  //SetCurrentDir( pathfonts );
  //dejavu:= LoadP3DFont( 'Deja Vu Sans.p3dfont' );
  //dejavu.SizeFactor:= 10;
  //P3DFontManager.Add( dejavu );
  P3DFontManager.DefaultFontsPath:= pathfonts;
end;

procedure UnloadFonts;
begin
  P3DFontManager.Clear();
end;

function LoadModels: Boolean;
var
  i: Integer;
  Mdl: TP3DMesh;
begin
  Result:= False;
  SetCurrentDir( pathmodels );
  mdlCube:= {LoadModelFileFromFile( 'landscape.model' ); //}TP3DScene.Create( ExpandFileName( pathmodels + '/cube.model' ));
  {mdlCube:= TModelFile.Create();
  Mdl:= TP3DMesh.Create( mdlCube.Children );

  Mdl.Positions.Add([ vec3( -1.0, -1.0, 0.0 ),
                      vec3(  1.0, -1.0, 0.0 ),
                      vec3(  0.0,  1.0, 0.0 )]);
  SetLength( Mdl.Faces, 1 );
  SetLength( Mdl.Faces[ 0 ].verts, 3 );
  Mdl.Faces[ 0 ].verts[ 0 ].v:= 0;
  Mdl.Faces[ 0 ].verts[ 1 ].v:= 1;
  Mdl.Faces[ 0 ].verts[ 2 ].v:= 2;
  Mdl.Material:= TMaterial.Create;
  Mdl.Material.Diff:= vec3( 1 );
  mdlCube.Children.Add( Mdl );
  Mdl.Calc_Tangent_Binormal;
  Mdl.UnpackBuffers;
  WriteLn( mdlCube.Debug );}

  //LOAD MODELS
end;

procedure UnloadModels;
begin
  mdlCube.Free;
  //FREE MODELS
end;

procedure LoadTextures;
begin
  //LOAD TEXTURES
  SetCurrentDir( pathmodels );
  tex1:= TP3DTexture.Create( 'rock.jpg' );
  tex2:= TP3DTexture.Create( 'rock_displace.jpg' );
  tex3:= TP3DTexture.Create( 'rock_normal.jpg' );
  tex4:= TP3DTexture.Create( 'bark.JPG' );
end;

procedure UnloadTextures;
begin
  tex1.Free;
  tex2.Free;
  tex3.Free;
  tex4.Free;
end;

procedure InitPhysics;
begin
  //INIT PHYSICS IF NEEDED
end;

procedure DeInitPhysics;
begin
  //FREE PHYSICS
end;
{
procedure LoadOutlineFragment( snof: TP3DShaderNodeOutlineFragment );
var
  guisno: TP3DSNodeControl;
  inp: TP3DShaderNodeInput;
  frag: TP3DShaderNodeOutlineFragment;
begin
  guisno:= TP3DNodeControl.Create( ParentList, GUIManager );
  for inp in snof.Inputs do
    for frag in inp.Fragments do
      LoadOutlineFragment( frag );
  guisno.Node:= snof;
end;

procedure LoadOutline( sno: TP3DShaderNodeOutline );
var
  frag: TP3DShaderNodeOutlineFragment;
begin
  for frag in sno.Fragments do
    LoadOutlineFragment( frag );
end;
}

procedure NodeTest;
var
  Node: TP3DNode;
  i: Integer;
begin
  NodeTree:= TP3DShaderNodeTree.Create;

  TestPopupMenu:= TP3DPopupMenu.Create( ParentList, GUIManager );
  TestPopupMenu.Left:= 600;
  TestPopupMenu.Width:= 200;
  TestPopupMenu.Height:= 200;
  i:= TestPopupMenu.Items.Add( 'Update shader' );
  TestPopupMenu.Items[ i ].OnMouseDown:= @events.CompileItmClick;
  for Node in P3DShaderLib.Nodes do
    begin
      i:= TestPopupMenu.Items.Add( 'Add ' + Node.Name + '...' );
      TestPopupMenu.Items[ i ].OnMouseDown:= @events.AddItmClick;
      TestPopupMenu.Items[ i ].UserData:= Node;
    end;
end;

procedure Init( Sender: TSDLWindow );
var
  grpbx: TP3DGroupBox;
  Obj: TP3DObject;

begin
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  Sender.Width:= 1024;
  Sender.Height:= 768;
  //Sender.FullScreen:= True;

  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  glEnable( GL_LINE_SMOOTH );

  mainscene:= tScene.Create;
  maincamera:= tCamera.Create;

  mainscene.DrawObjects:= @DrawObjects;
  mainscene.Cam:= maincamera;

  Setup2D( 0, 0, Sender.Width, Sender.Height );

  p3dviewports.Push( 0, 0, Sender.Width, Sender.Height );

  ParentList:= TP3DObjectList.Create;

  LoadModels;
  InitPhysics;
  LoadTextures;
  LoadShader;
  LoadFonts;

  {
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
    end; }
  OptionsBtn:= TP3DButton.Create( ParentList, GUIManager );
  OptionsBtn.Width:= 50;
  optionsBtn.Font.Name:= 'Pascal3D-Symbols';
  OptionsBtn.Caption:= 'L';
  OptionsBtn.OnMouseClick:= @events.OptionsBtnClick;
  LineShader2D.Enable;

  NodeTest;
  //canvas:= TP3DCanvas2D.Create( Nil );

  //guisno:= TP3DGUIShaderOutlineFragment.Create( ParentList, GUIManager );
  //guisno.OutlineFragment:= shadernodeoutline.Fragments[ 0 ];

  guiscene:= TP3DShaderPreview.Create( ParentList, GUIManager );
  //guiscene.Width:= 300;
  //guiscene.Height:= 300;

  //LoadOutline( shadernodeoutline );

  guiscene.ModelCube:= mdlCube;
  guiscene.Left:= 50; guiscene.Top:= 70;
  //guiscene.Shader:= shadernodeoutline;
  guiscene.Canvas.Pen.Style:= psStippled3;
  guiscene.SceneViewer.Scene.Shader:= TriangleScene.Shader;
  guiscene.ShaderTree:= NodeTree;
  guiscene.OnDrawScene:= @events.DrawScene;

  //guiscene.Color:= vec4( 1, 0.5, 0.5, 1 );

  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));
  ResizeWnd( mainwnd );

  SDL_ShowCursor( 0 );
  GUIManager.ShowCursor:= True;
end;


procedure DeInit( Sender: TSDLWindow );
begin
  mainscene.Free;
  maincamera.Free;

  //canvas.Free;

  DeInitPhysics;

  UnloadShader;
  UnloadModels;
  UnloadFonts;
  UnloadTextures;
  NodeTree.Free;
end;

