type

  { TEventsHandler }

  TEventsHandler = object
    procedure CompileBtnClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
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

  shadernodeoutline: TP3DShaderNodeOutline;
  compileBtn: TP3DButton;
  events: TEventsHandler;

  //guiscene: TP3DGUIShaderPreview;
  mdlCube: TP3DScene;

  NodeTree: TP3DNodeTree;
  //guisno: TP3DGUIShaderOutlineFragment;

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
  shadernodeoutline:= TP3DShaderNodeOutline.CreateFromFile( 'nodes/shader_test2.xml' );

  //LOAD SHADERS
end;

procedure UnloadShader;
begin
  font_shader.Free;
  shadernodeoutline.Free;
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
  guisno: TP3DNodeControl;
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
  procedure CreateNodeCtrl( ANode: TP3DNode );
  var
    guisno: TP3DNodeControl;
  begin
    guisno:= TP3DNodeControl.Create( ParentList, GUIManager );
    guisno.Node:= ANode;
  end;
var
  socket: TP3DNodeSocket;
  Node: TP3DNode;
begin
  NodeTree:= TP3DNodeTree.Create;
  NodeTree.X:= 400;
  NodeTree.Y:= 400;
  NodeTree.Name:= 'VertexShader';
  socket:= TP3DNodeSocket.Create( NodeTree );
  socket.Name:= 'Position';
  NodeTree.Inputs.Add( socket );
  socket:= TP3DNodeSocket.Create( NodeTree );
  socket.Name:= 'Color';
  NodeTree.Inputs.Add( socket );
  CreateNodeCtrl( NodeTree );

  Node:= TP3DNode.Create;
  Node.X:= 400;
  Node.Y:= 600;
  Node.Name:= 'FragmentShader';
  socket:= TP3DNodeSocket.Create( Node );
  socket.Name:= 'Position';
  Node.Inputs.Add( socket );
  socket:= TP3DNodeSocket.Create( Node );
  socket.Name:= 'Color';
  Node.Inputs.Add( socket );
  NodeTree.Nodes.Add( Node );
  CreateNodeCtrl( Node );

  Node:= TP3DNode.Create;
  Node.X:= 400;
  Node.Y:= 600;
  Node.Name:= 'Material';
  socket:= TP3DNodeSocket.Create( Node );
  socket.Name:= 'Color';
  Node.Inputs.Add( socket );
  socket:= TP3DNodeSocket.Create( Node );
  socket.Name:= 'Normal';
  Node.Inputs.Add( socket );
  socket:= TP3DNodeSocket.Create( Node );
  socket.Name:= 'Color';
  Node.Outputs.Add( socket );
  socket:= TP3DNodeSocket.Create( Node );
  socket.Name:= 'Normal';
  Node.Outputs.Add( socket );

  NodeTree.Nodes.Add( Node );
  CreateNodeCtrl( Node );
end;

procedure Init( Sender: TSDLWindow );
var
  grpbx: TP3DGroupBox;

begin
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  //Sender.Width:= 800;
  //Sender.Height:= 600;
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
  compileBtn:= TP3DButton.Create( ParentList, GUIManager );
  compileBtn.Width:= 200;
  compileBtn.Caption:= 'Compile';
  compileBtn.OnMouseClick:= @events.CompileBtnClick;
  LineShader2D.Enable;

  NodeTest;
  //canvas:= TP3DCanvas2D.Create( Nil );

  //guisno:= TP3DGUIShaderOutlineFragment.Create( ParentList, GUIManager );
  //guisno.OutlineFragment:= shadernodeoutline.Fragments[ 0 ];

  {guiscene:= TP3DGUIShaderPreview.Create( ParentList, GUIManager );
  guiscene.Width:= 300;
  guiscene.Height:= 300;}

  //LoadOutline( shadernodeoutline );

  {guiscene.ModelCube:= mdlCube;
  guiscene.Left:= 50; guiscene.Top:= 70;
  guiscene.Shader:= shadernodeoutline;
  //guiscene.Scene.Shader:= TriangleScene.Shader;
  guiscene.Color:= vec4( 1, 0.5, 0.5, 1 );
  }
  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));
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
  NodeTree.Free;
end;

