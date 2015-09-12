unit p3dgui_shadernodes;

interface
  uses
    p3dgui,
    p3dobjects,
    p3dMath,
    p3dbmpfont,
    p3dgui_focuscontrol,
    p3dinput,
    p3dcanvas,
    p3dgui_stdctrls,
    p3dgui_buttons,
    p3dshaders,
    p3dNodes,
    p3dmodel,
    p3dscene,
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
      FIsMoving: Boolean;
      FSockets: TP3DNodeSocketControlSimpleList;

      procedure SetNode(AValue: TP3DNode);
      procedure Update;
      procedure OnChange( Sender: TObject );
      procedure MouseMove( X, Y: Integer ); override;
      procedure MouseDownEvnt( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
      procedure BtnDragDrop( Sender: TP3DGraphicControl; Source: TP3DGraphicControl; X, Y: Integer );
      procedure BtnMouseDown( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
      procedure Render( BaseColor: TVec4; ScrollAcc: TVec2 ); override;

    public
      procedure Draw; override;

      constructor Create( AOwner: TP3DObjectList; AManager: TP3DGUIManager;
        const AParent: TP3DGraphicControl=nil );
      destructor Destroy; override;

      property Sockets: TP3DNodeSocketControlSimpleList read FSockets;
      property Node: TP3DNode read FNode write SetNode;
  end;

  { TP3DGUIShaderPreview }

  TP3DGUIShaderPreview = class ( TP3DGUISceneViewer )
    private
      Fworld: TMat4;
      FShaderCompiled: TP3DShaderCompiled;
      FShaderObject: TShader;
      FModelCube: TP3DScene;
      FShader: TP3DShaderNodeOutline;
      FMoving: Boolean;
      FXRot: Single;
      FYRot: Single;
      FZoom: Single;

      procedure SetModelCube( AValue: TP3DScene );
      procedure SetShader( AValue: TP3DShaderNodeOutline );
      procedure DrawObjects( AScene: tScene );
      procedure MouseMove( X, Y: Integer ); override;
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;

      property ShaderCompiled: TP3DShaderCompiled read FShaderCompiled write FShaderCompiled;
      property ShaderObject: TShader read FShaderObject write FShaderObject;
      property Scene;
      property XRot: Single read FXRot write FXRot;
      property YRot: Single read FYRot write FYRot;
      property Zoom: Single read FZoom write FZoom;
      property world: TMat4 read Fworld write Fworld;

    public
      constructor Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
        const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;
      procedure Update;

      procedure Render(BaseColor: TVec4; ScrollAcc: TVec2); override;

      property ModelCube: TP3DScene read FModelCube write SetModelCube;
      property Shader: TP3DShaderNodeOutline read FShader write SetShader;
  end;


implementation

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

{ TP3DGUIShaderPreview }

procedure TP3DGUIShaderPreview.SetModelCube(AValue: TP3DScene);
begin
  if FModelCube=AValue then Exit;
  FModelCube:=AValue;
  Update;
end;

procedure TP3DGUIShaderPreview.SetShader(AValue: TP3DShaderNodeOutline);
begin
  //if FShader=AValue then Exit;
  FShader:=AValue;
  Update;
end;

procedure TP3DGUIShaderPreview.DrawObjects(AScene: tScene);
var
  Mesh: TP3DMesh;
begin
  glClear( GL_DEPTH_BUFFER_BIT);
  glEnable( GL_DEPTH_TEST );
  if ( Assigned( ModelCube )) then
    ModelCube.Render( world );
  glDisable( GL_DEPTH_TEST );
end;

procedure TP3DGUIShaderPreview.MouseMove(X, Y: Integer);
begin
  inherited MouseMove(X, Y);
  if ( FMoving and InputManager.Keyboard.Keys[ P3DK_LCTRL ] AND InputManager.Mouse.Buttons[ 0 ]) then
    begin
      FZoom:= Max( 0.05, FZoom - InputManager.Mouse.DY / 50 );
    end
  else if ( InputManager.Mouse.Buttons[ 0 ] and FMoving ) then
    begin
      FXRot+= InputManager.Mouse.DY / 2;
      FYRot+= InputManager.Mouse.DX / 2;
    end
  else
    FMoving:= False;
  world:= mat4rotate( vec3_Axis_PX, deg2rad * FXRot ) * mat4rotate( vec3_Axis_PY, deg2rad * FYRot ) * mat4scale( vec4( Zoom )) * mat4translate( vec4( 0, 0, -1, 1 ));
end;

procedure TP3DGUIShaderPreview.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if (( gcisMouseOver in InputState ) and ( mb1 )) then
    FMoving:= True;
end;


constructor TP3DGUIShaderPreview.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; const AParent: TP3DGraphicControl);
begin
  inherited;
  Scene:= tScene.Create;
  Scene.Cam:= tCamera.Create;
  FZoom:= 1;
  //Scene.Cam.Position:= vec3( 0, 0, 4 );
  Scene.DrawObjectsObj:= @DrawObjects;
  Color:= vec4( $86 / 255, $A0 / 255, $CA / 255, 1.0 );
end;

destructor TP3DGUIShaderPreview.Destroy;
begin
  Scene.Cam.Free;
  Scene.Free;
  inherited Destroy;
end;

procedure TP3DGUIShaderPreview.Update;
var
  v_shader: TP3DShaderBuffer;
  f_shader: TP3DShaderBuffer;
begin
  if ( not Assigned( Shader )) then
    Exit;
  ShaderCompiled.Free;
  ShaderObject.Free;

  ShaderCompiled:= Shader.Compile();

  v_shader:= ShaderCompiled.FindBuffer( 'vshader' );
  f_shader:= ShaderCompiled.FindBuffer( 'fshader' );

  ShaderObject:= CreateVertexAndFragmentShader( v_shader.Code, f_shader.Code );
  if ( Assigned( Scene )) then
    Scene.Shader:= ShaderObject;
end;

procedure TP3DGUIShaderPreview.Render(BaseColor: TVec4; ScrollAcc: TVec2);
var
  p1: TVec2;
begin
  p1:= vec2( Canvas.Left + 2, Canvas.Top + 2 );
  Manager.ScreenCanvas.Lock;
  Manager.ScreenCanvas.RenderRectShadow( p1, p1 + vec2( Canvas.Width, Canvas.Height ), 5, vec4( 0, 0, 0, 0.1 ));
  Manager.ScreenCanvas.Unlock();

  inherited Render(BaseColor, ScrollAcc);
  {if ( FMoving ) then
    begin
      Canvas.Lock;
      Canvas.RenderRect( vec2( 10 ), vec2( 50 ), vec4( vec3( 0 ), 1 ));
      Canvas.Unlock();
    end;}
end;

{ TP3DGUIShaderOutlineConnector }

{var
  n: Integer;

const
  Tolerance = 2;

  function EqualTolerance( Val: Integer; EqualTo: Integer ): Boolean;
  begin
    Result:= ( Val + Tolerance >= EqualTo ) and ( Val - Tolerance <= EqualTo );
  end;

  function InSection( Val: Integer; B1, B2: Integer ): Boolean;
  begin
    Result:= InRange( Val, Min( B1 - Tolerance, B2 - Tolerance ), Max( B1 + Tolerance, B2 + Tolerance ));
  end;

begin
  Result:= inherited MouseRay(X, Y);
  if ( not Result ) then
    exit;

  X:= X - Canvas.Left;
  Y:= Y - Canvas.Top;
  n:= Round( FPStart.X + FPEnd.X ) div 2;
  if ( EqualTolerance( X, n )) then
    Result:= InSection( Y, round( FPStart.Y ), round( FPEnd.Y ))
  else if ( InSection( X, n, round( FPStart.X ))) then
    Result:= EqualTolerance( Y, round( FPStart.Y ))
  else if ( InSection( X, n, round( FPEnd.X ))) then
    Result:= EqualTolerance( Y, round( FPEnd.Y ));
end;}

{ TP3DGUIShaderOutlineFragment }

procedure TP3DNodeControl.SetNode(AValue: TP3DNode);
begin
  if FNode=AValue then Exit;
  FNode:=AValue;
  FNode.OnChange:= @OnChange;
  //FNode.OnUpdateChild:= @UpdatePC;
  Update;
end;

procedure TP3DNodeControl.Draw;
var
  p1: TVec2;
begin

  inherited Draw;
end;

procedure TP3DNodeControl.Update;
var
  i: Integer;
  lbl: TP3DLabel;
  socket: TP3DNodeSocketControlSimple;
  j: Integer;
  btn: TP3DButton;
begin
  Sockets.Clear( False );
  Controls.Clear( True );
  Caption:= Node.Name;

  Left:= Round( Node.X );
  Top:= Round( Node.Y );
  Width:= 200;
  Height:= 200;

  for i:= 0 to Node.Outputs.Count - 1 do
    begin
      socket:= TP3DNodeSocketControlSimple.Create( ParentList, Manager, Node.Outputs[ i ], nsdOutput, Self );
      socket.Align:= alTop;
      Sockets.Add( socket );
    end;
  for i:= 0 to Node.Inputs.Count - 1 do
    begin
      socket:= TP3DNodeSocketControlSimple.Create( ParentList, Manager, Node.Inputs[ i ], nsdInput, Self );
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
  if ( gcisMouseBtn1Down in InputState ) then
    begin
      Node.X:= Left + InputManager.Mouse.DX;
      Node.Y:= Top + InputManager.Mouse.DY;
    end
  else
    FIsMoving:= False;
end;

procedure TP3DNodeControl.MouseDownEvnt(
  Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if ( mb1 and ( gcisMouseOver in InputState )) then
    begin
      FIsMoving:= True;
      BringToFront;
    end;
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

constructor TP3DNodeControl.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; const AParent: TP3DGraphicControl);
begin
  inherited;
  FOnMouseDown:= @MouseDownEvnt;
  FSockets:= TP3DNodeSocketControlSimpleList.Create;
  Color:= vec4( $EB / 255, $75 / 255, $6B / 255, 1.0 );
  BorderColor:= vec4( 0, 0, 0, 0.2 );
end;

destructor TP3DNodeControl.Destroy;
begin
  FSockets.Free;
  inherited Destroy;
end;

end.

