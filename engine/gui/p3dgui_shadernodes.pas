unit p3dgui_shadernodes;

interface
  uses
    p3dgui,
    p3dobjects,
    p3dMath,
    p3dtext,
    p3dinput,
    p3dcanvas,
    p3dgui_stdctrls,
    p3dgui_buttons,
    p3dgui_menus,
    p3dshaders,
    p3dNodes,
    p3dmodel,
    p3dgui_sceneviewer,
    p3dbuffers,
    p3dgenerics,
    p3dshadernodes,
    dglOpenGL,
    Classes,
    math,
    Types;

type
  {$DEFINE INTERFACE}
  {$INCLUDE p3dgui_shadernodes_controls.inc}
  {$UNDEF INTERFACE}



  { TP3DNodeControl }

  TP3DNodeControl = class ( TP3DGroupBox )
    private
      FNode: TP3DNode;
      FOnDelete: TNotifyEvent;
      FSockets: TP3DNodeSocketControlSimpleList;
      DeleteBtn: TP3DButton;

    protected
      procedure SetNode(AValue: TP3DNode);
      procedure Update;
      procedure OnChange( Sender: TObject );
      procedure MouseMove( X, Y: Integer ); override;
      procedure MouseDownEvnt( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
      procedure BtnDragDrop( Sender: TP3DGraphicControl; Source: TP3DGraphicControl; X, Y: Integer );
      procedure BtnMouseDown( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
      procedure Render( BaseColor: TVec4; ScrollAcc: TVec2 ); override;
      procedure BtnDeleteClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );

    public
      procedure Draw; override;

      constructor Create( AOwner: TP3DObjectList; AManager: TP3DGUIManager;
        const AParent: TP3DGraphicControl=nil );
      destructor Destroy; override;

      property Sockets: TP3DNodeSocketControlSimpleList read FSockets;
      property Node: TP3DNode read FNode write SetNode;
      property OnDelete: TNotifyEvent read FOnDelete write FOnDelete;
  end;

  { TP3DShaderPreview }

  TP3DShaderPreview = class ( TP3DGroupBox )
    private
      FOnDrawScene: TNotifyEvent;
      FSceneViewer: TP3DGUISceneViewer;
      FShaderTree: TP3DShaderNodeTree;
      Fworld: TMat4;
      FModelCube: TP3DScene;
      FXRot: Single;
      FYRot: Single;
      FZoom: Single;
      //vshader: TP3DShaderNodeSocket;
      //fshader: TP3DShaderNodeSocket;

      procedure SetModelCube( AValue: TP3DScene );
      procedure DrawObjects(AScene: TP3DScene);
      procedure PreviewMouseMove( Sender: TP3DGraphicControl; X, Y: Integer );

      property XRot: Single read FXRot write FXRot;
      property YRot: Single read FYRot write FYRot;
      property Zoom: Single read FZoom write FZoom;
      property world: TMat4 read Fworld write Fworld;

    protected
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure MouseMove( X, Y: Integer ); override;

    public
      constructor Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
        const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

      procedure UpdateShader;

      procedure Render(BaseColor: TVec4; ScrollAcc: TVec2); override;

      property ModelCube: TP3DScene read FModelCube write SetModelCube;
      property SceneViewer: TP3DGUISceneViewer read FSceneViewer write FSceneViewer;
      property ShaderTree: TP3DShaderNodeTree read FShaderTree write FShaderTree;
      property OnDrawScene: TNotifyEvent read FOnDrawScene write FOnDrawScene;
  end;


implementation

{ TP3DNodeSocketVectorEdit }

procedure TP3DNodeSocketVectorEdit.SetSocket( AValue: TP3DShaderNodeSocketVector );
begin
  if FSocket=AValue then Exit;
  FSocket:=AValue;
  Caption:= AValue.Name;
end;

function TP3DNodeSocketVectorEdit.GetValue(Sender: TP3DEventValueEdit): Single;
var
  i: Integer;
begin
  for i:= 0 to high( FValueEdits ) do
    if ( FValueEdits[ i ] = Sender ) then
      break;
  Result:= Socket.Value[ i ];
end;

procedure TP3DNodeSocketVectorEdit.SetValue(Sender: TP3DEventValueEdit;
  AValue: Single);
var
  i: Integer;
begin
  for i:= 0 to high( FValueEdits ) do
    if ( FValueEdits[ i ] = Sender ) then
      break;
  Socket.Value[ i ]:= AValue;
end;

function TP3DNodeSocketVectorEdit.GetValueName(Sender: TP3DEventValueEdit): String;
const
  E: array [ 0..3 ] of String = ( 'X', 'Y', 'Z', 'W' );
var
  i: Integer;
begin
  for i:= 0 to high( FValueEdits ) do
    if ( FValueEdits[ i ] = Sender ) then
      break;
  Result:= E[ i ];
end;

constructor TP3DNodeSocketVectorEdit.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; ASocket: TP3DShaderNodeSocketVector;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AOwner, AManager, AParent );
  FDropDown:= TP3DCustomPopupMenu.Create( ParentList, Manager, nil );
  FDropDown.Visible:= False;
  SetLength( FValueEdits, 4 );
  Socket:= ASocket;
  Update;
end;

destructor TP3DNodeSocketVectorEdit.Destroy;
begin
  if ( ParentList.IndexOf( FDropDown ) > -1 ) then //Prevent error on freeing of already destroyed Control when destroying ParentList
    FDropDown.Free;
  inherited Destroy;
end;

procedure TP3DNodeSocketVectorEdit.Update;
var
  i: Integer;
begin
  FDropDown.Height:= 1024;
  for i:= 0 to Length( FValueEdits ) - 1 do
    begin
      if ( Assigned( FValueEdits[ i ])) then
        FValueEdits[ i ].Free;
      if ( Socket = nil ) then
        continue;
      FValueEdits[ i ]:= TP3DEventValueEdit.Create( ParentList, Manager, FDropDown );
      FValueEdits[ i ].Align:= alTop;
      FValueEdits[ i ].SetValueEvent:= @SetValue;
      FValueEdits[ i ].GetValueEvent:= @GetValue;
      FValueEdits[ i ].GetValueNameEvent:= @GetValueName;
    end;
  if ( Socket <> nil ) then
    FDropDown.Height:= FValueEdits[ i ].Top + FValueEdits[ i ].Height;
end;

procedure TP3DNodeSocketVectorEdit.MouseClick(mb1, mb2, mb3: Boolean; X,
  Y: Integer);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
  FDropDown.Width:= Canvas.Width;
  FDropDown.PopUp( Canvas.Left, Canvas.Top + Canvas.Height );
end;

{ TP3DNodeSocketControlVector }

procedure TP3DNodeSocketControlVector.SetCanEdit(AValue: Boolean);
begin
  if FCanEdit=AValue then Exit;
  FCanEdit:=AValue;
  if ( CanEdit ) then
    begin
      Lbl.Visible:= False;
      VecEdit.Visible:= True;
      //VecEdit.ValueName:= Socket.Name;
    end
  else
    begin
      Lbl.Visible:= True;
      VecEdit.Visible:= False;
    end
end;

procedure TP3DNodeSocketControlVector.Draw;
begin
  inherited Draw;
  CanEdit:= not Assigned( Socket.Connected );
end;

constructor TP3DNodeSocketControlVector.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; ASocket: TP3DNodeSocket;
  ADirection: TP3DNodeSocketDirection; const AParent: TP3DGraphicControl);
begin
  inherited;
  FVecEdit:= TP3DNodeSocketVectorEdit.Create( ParentList, Manager, Socket as TP3DShaderNodeSocketVector, Self );
  CanEdit:= False;
  FVecEdit.Align:= alClient;
end;

{ TP3DNodeSocketButton }

constructor TP3DNodeSocketButton.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; const AParent: TP3DGraphicControl);
begin
  inherited;
  BoundsLeft:= 10;
  BoundsRight:= 10;
end;

procedure TP3DNodeSocketButton.Draw;
var
  Preset: TP3DButtonPreset;
  c: TVec2;
begin
  if ( gcisMouseBtn1Down in InputState ) then
    Preset:= PresetDown
  else if ( gcisMouseOver in InputState ) then
    Preset:= PresetHover
  else
    Preset:= PresetNormal;

  c:= vec2( ClientRect.Left + ClientRect.Right / 2, ClientRect.Top + ClientRect.Bottom / 2 );
  Canvas.RenderCircle( c, ClientRect.Right / 2, 16, Preset.Color );
  Canvas.RenderLineCircle( c, ClientRect.Right / 2, 16, Preset.OutlineColor );
end;

function TP3DNodeSocketButton.MouseRay(X, Y: Integer): Boolean;
var
  c: TVec2;
begin
  if ( inherited MouseRay( X, Y )) then
    begin
      c:= vec2( ClientRect.Left + ClientRect.Right / 2, ClientRect.Top + ClientRect.Bottom / 2 );
      Result:= ( c - vec2( x - Canvas.Left, y - Canvas.Top )).GetDist() <= ClientRect.Right / 2; //r = Width / 2
    end
  else
    Result:= False;
end;

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgui_shadernodes_controls.inc}
{$UNDEF IMPLEMENTATION}

{ TP3DShaderPreview }

procedure TP3DShaderPreview.SetModelCube(AValue: TP3DScene);
begin
  if FModelCube=AValue then Exit;
  FModelCube:=AValue;
end;

procedure TP3DShaderPreview.DrawObjects(AScene: TP3DScene);
begin
  glClear( GL_DEPTH_BUFFER_BIT);
  glEnable( GL_DEPTH_TEST );
  if ( Assigned( OnDrawScene )) then
    OnDrawScene( Self );
  if ( Assigned( AScene.Shader )) then
    begin
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex0' ), 0 );
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex1' ), 1 );
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex2' ), 2 );
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex3' ), 3 );
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex4' ), 4 );
    end;
  if ( Assigned( ModelCube )) then
    ModelCube.Render;//( world, AScene );
  glDisable( GL_DEPTH_TEST );
end;

procedure TP3DShaderPreview.PreviewMouseMove(Sender: TP3DGraphicControl; X,
  Y: Integer);
begin
  if ( gcisMouseBtn1Down in Sender.InputState ) then
    begin
      if ( InputManager.Keyboard.Keys[ P3DK_LCTRL ]) then
        FZoom:= Max( 0.05, FZoom - InputManager.Mouse.DY / 50 )
      else
        begin
          FXRot+= InputManager.Mouse.DY / 2;
          FYRot+= InputManager.Mouse.DX / 2;
        end;
      world:= mat4rotate( vec3_Axis_PX, deg2rad * FXRot ) * mat4rotate( vec3_Axis_PY, deg2rad * FYRot ) * mat4scale( vec4( Zoom )) * mat4translate( vec4( 0, 0, -1, 1 ));
    end;
end;

procedure TP3DShaderPreview.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if ( mb1 and ( gcisMouseOver in InputState )) then
    BringToFront;
end;

procedure TP3DShaderPreview.MouseMove(X, Y: Integer);
begin
  inherited MouseMove( X, Y );
  if ( gcisMouseBtn1Down in InputState ) then
    begin
      Left:= Left + InputManager.Mouse.DX;
      Top:= Top + InputManager.Mouse.DY;
    end;
end;

constructor TP3DShaderPreview.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; const AParent: TP3DGraphicControl);
begin
  inherited;
  Width:= 200;
  Height:= 200;
  SceneViewer:= TP3DGUISceneViewer.Create( ParentList, Manager, Self );
  SceneViewer.Scene:= TP3DScene.Create;
  SceneViewer.Scene.Cam:= TP3DCamera.Create;
  FZoom:= 1;
  SceneViewer.Scene.Cam.Position:= vec3( 0, 0, 4 );
  SceneViewer.Scene.DrawObjectsObj:= @DrawObjects;
  SceneViewer.OnMouseMove:= @PreviewMouseMove;
  SceneViewer.Caption:= 'Preview';
  SceneViewer.Align:= alClient;
  SceneViewer.Color:= vec4( $86 / 255, $A0 / 255, $CA / 255, 1.0 );
  Color:= vec4( $86 / 255, $A0 / 255, $CA / 255, 1.0 );
end;

destructor TP3DShaderPreview.Destroy;
begin
  //Scene.Cam.Free;
  //Scene.Free;
  inherited Destroy;
end;

procedure TP3DShaderPreview.UpdateShader;
var
  ShaderCompiled: TP3DShaderCompiled;
  v_shader: TP3DShaderBuffer;
  f_shader: TP3DShaderBuffer;
begin
  if ( Assigned( SceneViewer.Scene ) and ( Assigned( ShaderTree ))) then
    with ( SceneViewer.Scene ) do
      begin
        Shader.Free;

        ShaderCompiled:= ShaderTree.Compile;

        v_shader:= ShaderCompiled.FindBuffer( 'vshader' );
        f_shader:= ShaderCompiled.FindBuffer( 'fshader' );

        Shader:= CreateVertexAndFragmentShader( v_shader.Code, f_shader.Code );

        ShaderCompiled.Free;
      end;
end;

{procedure TP3DShaderPreview.Update;
var
  v_shader: TP3DShaderBuffer;
  f_shader: TP3DShaderBuffer;
begin
  {if ( not Assigned( Shader )) then
    Exit;
  ShaderCompiled.Free;
  ShaderObject.Free;

  ShaderCompiled:= Shader.Compile();

  v_shader:= ShaderCompiled.FindBuffer( 'vshader' );
  f_shader:= ShaderCompiled.FindBuffer( 'fshader' );

  ShaderObject:= CreateVertexAndFragmentShader( v_shader.Code, f_shader.Code );
  if ( Assigned( Scene )) then
    Scene.Shader:= ShaderObject;}
end;}

procedure TP3DShaderPreview.Render(BaseColor: TVec4; ScrollAcc: TVec2);
var
  p1: TVec2;
begin
  p1:= vec2( Canvas.Left + 2, Canvas.Top + 2 );
  Manager.ScreenCanvas.Lock;
  Manager.ScreenCanvas.RenderRectShadow( p1, p1 + vec2( Canvas.Width, Canvas.Height ), 5, vec4( 0, 0, 0, 0.1 ));
  Manager.ScreenCanvas.Unlock();

  inherited Render(BaseColor, ScrollAcc);
end;


{ TP3DGUIShaderOutlineFragment }

procedure TP3DNodeControl.SetNode(AValue: TP3DNode);
begin
  if FNode=AValue then Exit;
  FNode:=AValue;
  if ( Assigned( FNode )) then
    FNode.OnChange:= @OnChange;
  //FNode.OnUpdateChild:= @UpdatePC;
  Update;
end;

procedure TP3DNodeControl.Draw;
begin

  inherited Draw;
end;

procedure TP3DNodeControl.Update;
var
  i: Integer;
  socket: TP3DNodeSocketControlSimple;
begin
  Sockets.Clear( True );
  if ( not Assigned( Node )) then
    exit;
  //Controls.Clear( True );
  Caption:= Node.Name;

  Left:= Round( Node.X );
  Top:= Round( Node.Y );
  Width:= 200;
  Height:= 200;

  for i:= 0 to Node.Outputs.Count - 1 do
    begin
      if ( Node.Outputs[ i ].SocketType = 'shader' ) then
        continue;
      socket:= TP3DNodeSocketControlSimple.Create( ParentList, Manager, Node.Outputs[ i ], nsdOutput, Self );
      socket.Align:= alTop;
      Sockets.Add( socket );
    end;
  for i:= 0 to Node.Inputs.Count - 1 do
    begin
      case Node.Inputs[ i ].SocketType of
        'vec4': socket:= TP3DNodeSocketControlVector.Create( ParentList, Manager, Node.Inputs[ i ], nsdInput, Self );
        else
          socket:= TP3DNodeSocketControlSimple.Create( ParentList, Manager, Node.Inputs[ i ], nsdInput, Self );
      end;
      socket.Align:= alTop;
      Sockets.Add( socket );
    end;

  if ( Sockets.Count > 0 ) then
    with ( Sockets[ Sockets.Count - 1 ]) do
      Self.Height:= Top + Height + 40
  else
    Height:= 40;
end;

procedure TP3DNodeControl.OnChange(Sender: TObject);
begin
  Update;
end;

procedure TP3DNodeControl.MouseMove(X, Y: Integer);
begin
  inherited MouseMove( X, Y );
  if (( gcisMouseBtn1Down in InputState ) and ( Assigned( Node ))) then
    begin
      Node.X:= Left + InputManager.Mouse.DX;
      Node.Y:= Top + InputManager.Mouse.DY;
    end;
end;

procedure TP3DNodeControl.MouseDownEvnt(
  Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if ( mb1 and ( gcisMouseOver in InputState )) then
    BringToFront;
end;

procedure TP3DNodeControl.BtnDragDrop(Sender: TP3DGraphicControl;
  Source: TP3DGraphicControl; X, Y: Integer);
begin

end;

procedure TP3DNodeControl.BtnMouseDown(Sender: TP3DGraphicControl;
  mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if (( mb1 ) and ( gcisMouseOver in Sender.InputState )) then
    BeginDrag();
end;

procedure TP3DNodeControl.Render(BaseColor: TVec4; ScrollAcc: TVec2);
var
  p1: TVec2;
begin
  p1:= vec2( Canvas.Left + 2, Canvas.Top + 2 );
  Manager.ScreenCanvas.Lock;
  Manager.ScreenCanvas.RenderRectShadow( p1, p1 + vec2( Canvas.Width, Canvas.Height ), 5, vec4( 0, 0, 0, 0.1 ));
  Manager.ScreenCanvas.Unlock();
  inherited Render(BaseColor, ScrollAcc);
end;

procedure TP3DNodeControl.BtnDeleteClick(Sender: TP3DGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);
begin
  if ( Assigned( OnDelete )) then
    OnDelete( Self );
end;

constructor TP3DNodeControl.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; const AParent: TP3DGraphicControl);
begin
  inherited;
  FOnMouseDown:= @MouseDownEvnt;
  FSockets:= TP3DNodeSocketControlSimpleList.Create;
  Color:= vec4( $EB / 255, $75 / 255, $6B / 255, 1.0 );
  BorderColor:= vec4( 0, 0, 0, 0.2 );
  DeleteBtn:= TP3DButton.Create( ParentList, Manager, Self );
  DeleteBtn.PresetNormal.Color:= vec4( 0, 0, 0, 0 );
  DeleteBtn.PresetHover.Color:= vec4( 0, 0, 0, 0 );
  DeleteBtn.PresetDown.Color:= vec4( 0, 0, 0, 0 );

  DeleteBtn.Top:= -BoundsTop;
  DeleteBtn.Left:= -BoundsLeft;
  DeleteBtn.Width:= 20;
  DeleteBtn.Height:= 20;
  DeleteBtn.Font.Name:= 'Pascal3D-Symbols';
  DeleteBtn.Caption:= 'M';
  DeleteBtn.OnMouseClick:= @BtnDeleteClick;
end;

destructor TP3DNodeControl.Destroy;
begin
  FSockets.Free;
  inherited Destroy;
end;

end.

