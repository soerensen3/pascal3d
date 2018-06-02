//p3dgui_buttons.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

type
  { TP3DNodeControl }

  TP3DNodeControl = class ( TP3DForm )
    protected
      FNode: TP3DStreamablePointer;
      FSockets: TP3DNodeSocketControlPointerList;
      FIsUpdating: Boolean;
      FUpdateSockets: Boolean;

      function GetNode: TP3DNodeClone;
      procedure SetNeedsUpdate(AValue: Boolean); override;
      procedure OnNodeChange(AValue: TP3DStreamable);
      procedure AfterNodeChange(AValue: TP3DStreamable);
      procedure SetNode(AValue: TP3DNodeClone);
      procedure Update; override;
      procedure SetColor(AValue: TP3DColorPaletteMaterialDesign); override;

      procedure OnChange( Sender: TObject );
      procedure BtnDragDrop( Sender: TP3DGraphicControl; Source: TP3DGraphicControl; X, Y: Integer );
      procedure BtnMouseDown( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
      function AddSocket( Socket: TP3DNodeSocketClone ): TP3DNodeSocketControl;
      procedure RemoveSocket( Socket: TP3DNodeSocketClone );
      function GetSocket( Socket: TP3DNodeSocketClone ): TP3DNodeSocketControl;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Logic(); override;

      property Sockets: TP3DNodeSocketControlPointerList read FSockets;
      property Node: TP3DNodeClone read GetNode write SetNode;
  end;

  { TP3DShaderPreview }

  {TP3DShaderPreview = class ( TP3DGroupBox )
    private
      FOnDrawScene: TNotifyEvent;
      FSceneViewer: TP3DStreamablePointer;
      FShaderTree: TP3DStreamablePointer;
      Fworld: TMat4;
      FModelCube: TP3DStreamablePointer;
      FXRot: Single;
      FYRot: Single;
      FZoom: Single;
      //vshader: TP3DShaderNodeSocket;
      //fshader: TP3DShaderNodeSocket;

      function GetModelCube: TP3DScene;
      function GetSceneViewer: TP3DSceneViewer;
      function GetShaderTree: TP3DShaderNodeTree;
      procedure SetModelCube( AValue: TP3DScene );
      procedure DrawObjects(AScene: TP3DScene);
      procedure PreviewMouseMove( Sender: TP3DGraphicControl; X, Y: Integer );
      procedure SetSceneViewer(AValue: TP3DSceneViewer);
      procedure SetShaderTree(AValue: TP3DShaderNodeTree);

      property XRot: Single read FXRot write FXRot;
      property YRot: Single read FYRot write FYRot;
      property Zoom: Single read FZoom write FZoom;
      property world: TMat4 read Fworld write Fworld;

    protected
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure MouseMove( X, Y: Integer ); override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure UpdateShader;

      procedure Draw(); override;

      property ModelCube: TP3DScene read GetModelCube write SetModelCube;
      property SceneViewer: TP3DSceneViewer read GetSceneViewer write SetSceneViewer;
      property ShaderTree: TP3DShaderNodeTree read GetShaderTree write SetShaderTree;
      property OnDrawScene: TNotifyEvent read FOnDrawScene write FOnDrawScene;
  end;}

  { TP3DShaderNodeEditor }

  //TODO: filo event handler

  TP3DNodeEventType = ( netNode, netSocket );
  PP3DNodeEvent = ^TP3DNodeEvent;
  TP3DNodeEvent = record
    Next: PP3DNodeEvent;
    Obj: TP3DStreamable;
    Method: TNotifyEvent;
  end;


  TP3DShaderNodeEditor = class ( TP3DGraphicControl )

    { TP3DNodeEventHandler }
    private type

      { TP3DNodeEventHandler }

      TP3DNodeEventHandler = object
        private
          FFirst: PP3DNodeEvent;
          FLast: PP3DNodeEvent;

        public
          property First: PP3DNodeEvent read FFirst write FFirst;
          property Last: PP3DNodeEvent read FLast write FLast;

          procedure PushEvent(AMethod: TNotifyEvent; Obj: TP3DStreamable);
          procedure PushNodeEvent( AMethod: TP3DNodeMasterNodeHandler; ANode: TP3DNodeClone );
          procedure PushSocketEvent( AMethod: TP3DNodeMasterSocketHandler; ASocket: TP3DNodeSocketClone );
          function HandleFirstEvent: Boolean;
      end;
    private
      FEventHandler: TP3DNodeEventHandler;
      FMaterial: TP3DStreamablePointer;
      FNodeEditor: TP3DStreamablePointer;
      FNodeList: TP3DStreamablePointer;
      //FToolbar: TP3DStreamablePointer;

      function GetMaterial: TP3DMaterialNode;
      function GetNodeEditor: TP3DFrame;
      //function GetToolbar: TP3DToolbar;
      procedure OnUniformsChange(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
      procedure SetMaterial(AValue: TP3DMaterialNode);
      procedure SetNodeEditor(AValue: TP3DFrame);
      procedure OnNodesChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );
      function AddControl( ANode: TP3DNodeClone; AParent: TP3DGraphicControl ): TP3DNodeControl;
      procedure OnNodeClose( ASender: TP3DGraphicControl );
      function GetNodeList: TP3DTreeViewObjects;
      procedure SetNodeList(AValue: TP3DTreeViewObjects);
      procedure NodeEditorDragDrop(Sender, Source: TP3DGraphicControl; X, Y: Float; var Accept: Boolean);
      procedure MouseWheel(deltax: Integer; deltay: Integer); override;
      procedure NodeEditorMouseMove( Sender: TP3DGraphicControl; X, Y: Float);
      procedure NodeEditorMouseWheel( Sender: TP3DGraphicControl; X, Y: Float);
      procedure OnIntfSetSelection( Sender: TObject );
      //procedure SetToolbar(AValue: TP3DToolbar);

      procedure AddNode(Node: TP3DNodeClone);
      function AddAnyNode(Node: TP3DNodeClone): TP3DNodeControl;
      function AddTexNode( Node: TP3DNodeClone ): TP3DNodeControl;
      function AddUniformNode( Node: TP3DNodeClone ): TP3DNodeControl;
      function AddRootNode( Node: TP3DNodeClone ): TP3DNodeControl;
      function AddGeometryNode( Node: TP3DNodeClone ): TP3DNodeControl;
      procedure SetSocketConnected( Socket: TP3DNodeSocketClone );

      procedure NodeClearSockets( Node: TP3DNodeClone );

      procedure RemoveNode( Node: TP3DNodeClone );
      procedure AddSocket( Socket: TP3DNodeSocketClone );
      procedure RemoveSocket( Socket: TP3DNodeSocketClone );

      procedure AddNodeEvent( Node: TP3DNodeClone );
      procedure RemoveNodeEvent( Node: TP3DNodeClone );
      procedure AddSocketEvent( Socket: TP3DNodeSocketClone );
      procedure RemoveSocketEvent( Socket: TP3DNodeSocketClone );
      procedure SetSocketConnectedEvent( Socket: TP3DNodeSocketClone );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      function FindControl( AOwner: TP3DGraphicControl; ANode: TP3DNodeClone ): Integer;
      function GetControl( ANode: TP3DNodeClone ): TP3DNodeControl;
      procedure AlignConnected(ANodeCtrl: TP3DNodeControl; ARecurse: Boolean; AMargin: TVec2);

      procedure Logic(); override;
      procedure Draw; override;
      procedure Update; override;

      property Material: TP3DMaterialNode read GetMaterial write SetMaterial;
      property NodeEditor: TP3DFrame read GetNodeEditor write SetNodeEditor;
      property NodeList: TP3DTreeViewObjects read GetNodeList write SetNodeList;
      //property Toolbar: TP3DToolbar read GetToolbar write SetToolbar;
      property EventHandler: TP3DNodeEventHandler read FEventHandler;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DShaderPreview }
{
procedure TP3DShaderPreview.SetModelCube(AValue: TP3DScene);
begin
  FModelCube.AsValue:= AValue;
end;

function TP3DShaderPreview.GetModelCube: TP3DScene;
begin
  Result:= TP3DScene( FModelCube.AsValue );
end;

function TP3DShaderPreview.GetSceneViewer: TP3DSceneViewer;
begin
  Result:= TP3DSceneViewer( FSceneViewer.AsValue );
end;

function TP3DShaderPreview.GetShaderTree: TP3DShaderNodeTree;
begin
  Result:= TP3DShaderNodeTree( FShaderTree.AsValue );
end;

procedure TP3DShaderPreview.DrawObjects(AScene: TP3DScene);
begin
  glClear( GL_DEPTH_BUFFER_BIT); cle( Self );
  glEnable( GL_DEPTH_TEST ); cle( Self );
  if ( Assigned( OnDrawScene )) then
    OnDrawScene( Self );
  {if ( Assigned( AScene.Shader )) then
    begin
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex0' ), 0 );
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex1' ), 1 );
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex2' ), 2 );
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex3' ), 3 );
      glUniform1i( AScene.Shader.Uniforms.AddrByName( 'tex4' ), 4 );
    end;}
  if ( Assigned( ModelCube )) then
    ModelCube.Render;//( world, AScene );
  glDisable( GL_DEPTH_TEST ); cle( Self );
end;

procedure TP3DShaderPreview.PreviewMouseMove(Sender: TP3DGraphicControl; X,
  Y: Integer);
begin
  if ( gcisMouseBtn1Down in Sender.InputState ) then
    begin
      if ( P3DInput.Keyboard.Keys[ P3DK_LCTRL ]) then
        FZoom:= Max( 0.05, FZoom - P3DInput.Mouse.DY / 50 )
      else
        begin
          FXRot+= P3DInput.Mouse.DY / 2;
          FYRot+= P3DInput.Mouse.DX / 2;
        end;
      world:= mat4rotate( vec3_Axis_PX, deg2rad * FXRot ) * mat4rotate( vec3_Axis_PY, deg2rad * FYRot ) * mat4scale( vec4( Zoom )) * mat4translate( vec4( 0, 0, -1, 1 ));
    end;
end;

procedure TP3DShaderPreview.SetSceneViewer(AValue: TP3DSceneViewer);
begin
  FSceneViewer.AsValue:= AValue;
end;

procedure TP3DShaderPreview.SetShaderTree(AValue: TP3DShaderNodeTree);
begin
  FShaderTree.AsValue:= AValue;
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
      Left:= Left + P3DInput.Mouse.DX;
      Top:= Top + P3DInput.Mouse.DY;
    end;
end;

constructor TP3DShaderPreview.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Width:= 200;
  Height:= 200;
  FSceneViewer:= TP3DStreamablePointer.Create( 'SceneViewer', TP3DSceneViewer );
  Properties.Add( FSceneViewer );
  SceneViewer:= TP3DSceneViewer.Create( Self.Controls );
  SceneViewer.Scene:= TP3DScene.Create;
  //SceneViewer.Scene.Cam:= TP3DCamera.Create;
  FZoom:= 1;
  //SceneViewer.Scene.Cam.Transform.Position:= vec3( 0, 0, 4 );
//  SceneViewer.Scene.DrawObjectsObj:= @DrawObjects;
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
  {if ( Assigned( SceneViewer.Scene ) and ( Assigned( ShaderTree ))) then
    with ( SceneViewer.Scene ) do
      begin
        Shader.Free;

        ShaderCompiled:= ShaderTree.Compile;

        v_shader:= ShaderCompiled.FindBuffer( 'vshader' );
        f_shader:= ShaderCompiled.FindBuffer( 'fshader' );

        Shader:= CreateVertexAndFragmentShader( v_shader.Code, f_shader.Code );

        ShaderCompiled.Free;
      end;}
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

procedure TP3DShaderPreview.Draw();
var
  p1: TVec2;
begin
  p1:= vec2( Canvas.Left + 2, Canvas.Top + 2 );
  P3DUI.ScreenCanvas.Lock;
  P3DUI.ScreenCanvas.RenderRectShadow( p1, p1 + vec2( Canvas.Width, Canvas.Height ), 5, vec4( 0, 0, 0, 0.1 ));
  P3DUI.ScreenCanvas.Unlock();

  inherited Draw;
end;
}

{ TP3DGUIShaderOutlineFragment }

function TP3DNodeControl.GetNode: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FNode.AsValue );
end;

procedure TP3DNodeControl.SetNeedsUpdate(AValue: Boolean);
begin
  inherited SetNeedsUpdate(AValue);
  if ( FIsUpdating ) then
    exit;
  if ( AValue and Assigned( FNode ) and Assigned( Node )) then
    begin
      Node.X:= Left;
      Node.Y:= Top;
    end;
end;

procedure TP3DNodeControl.OnNodeChange(AValue: TP3DStreamable);
begin
  if ( Assigned( Node )) then
    Node.OnChange:= nil;
end;

procedure TP3DNodeControl.AfterNodeChange(AValue: TP3DStreamable);
begin
  if ( not Assigned( Node )) then
  //  Free
  else
    Node.OnChange:= @OnChange;
  Update;
end;

procedure TP3DNodeControl.SetNode(AValue: TP3DNodeClone);
begin
  if FNode.AsValue=AValue then Exit;
  OnNodeChange( AValue );

  FNode.AsValue:=AValue;

  AfterNodeChange( AValue );
end;

procedure TP3DNodeControl.Update;
var
  i: Integer;
  socket: TP3DNodeSocketControl;
  ctrl: TP3DNodeSocketPropEdit;
begin
  FIsUpdating:= True;
  Controls.Clear;
  Sockets.Clear;
  if ( not Assigned( Node )) then
    exit;
  if ( not Assigned( Node.CloneOf )) then
    exit;
  Caption:= Node.CloneOf.Name;

  TopLeft:= vec2( Node.X, Node.Y );
  WidthHeight:= vec2( 120 );
  PageModeY:= pmExpand;

  for i:= 0 to Node.Outputs.Count - 1 do
    begin
      if ( not Node.Outputs[ i ].CloneOf.Visible ) then begin
        //WriteLn( Node.CloneOf.Name + '.Outputs[ "', Node.Outputs[ i ].CloneOf.Name, '" ]' );
        continue;
      end;
      AddSocket( Node.Outputs[ i ]);
    end;
  for i:= 0 to Node.Inputs.Count - 1 do
    begin
      if ( not Node.Inputs[ i ].CloneOf.Visible ) then begin
        //WriteLn( Node.CloneOf.Name + '.Inputs[ "', Node.Inputs[ i ].CloneOf.Name, '" ]' );
        continue;
      end;
      AddSocket( Node.Inputs[ i ]);
    end;
  FIsUpdating:= False;
  FUpdateSockets:= False;
  inherited Update;
end;

procedure TP3DNodeControl.SetColor(AValue: TP3DColorPaletteMaterialDesign);
var
  S: TP3DNodeSocketControl;
begin
  inherited SetColor(AValue);
  if ( Assigned( Sockets )) then
    for S in Sockets do
      begin
        S.PresetNormal.Color:= Color;
        S.PresetDown.Color:= Color;
        S.PresetHover.Color:= Color;
      end;
end;

procedure TP3DNodeControl.OnChange(Sender: TObject);
begin
  FUpdateSockets:= True;
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

function TP3DNodeControl.AddSocket(Socket: TP3DNodeSocketClone): TP3DNodeSocketControl;
begin
  if ( not Socket.CloneOf.Visible ) then
    exit( nil );

  Result:= GetSocket( Socket);
  if ( Assigned( Result )) then
    exit;
  Result:= TP3DNodeSocketControl.Create( Controls );
  Result.Socket:= Socket;

  Result.PresetNormal.Color:= Color;
  Result.Align:= alTop;
  Sockets.Add( Result );
end;

procedure TP3DNodeControl.RemoveSocket(Socket: TP3DNodeSocketClone);
var
  Ctrl: TP3DNodeSocketControl;
begin
  Ctrl:= GetSocket( Socket );
  Ctrl.Free;
end;

function TP3DNodeControl.GetSocket(Socket: TP3DNodeSocketClone): TP3DNodeSocketControl;
var
  S: TP3DNodeSocketControl;
begin
  Result:= nil;
  for S in Sockets do
    if ( S.Socket = Socket ) then
      begin
        Result:= S;
        break;
      end;
end;

constructor TP3DNodeControl.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FSockets:= TP3DNodeSocketControlPointerList.Create( 'Sockets' );
  Properties.Add( FSockets );
  FNode:= TP3DStreamablePointer.Create( 'Node', TP3DNodeClone );
  //FNode.OnChange:= @OnNodeChange;
  //FNode.AfterChange:= @AfterNodeChange;
  Properties.Add( FNode );
end;

destructor TP3DNodeControl.Destroy;
begin
  if ( Assigned( Node )) then
    Node.OnChange:= nil;
  //FNode.OnChange:= nil;
  //FNode.AfterChange:= nil;
  inherited Destroy;
end;

procedure TP3DNodeControl.Logic();
begin
  inherited Logic();
  if ( FUpdateSockets ) then
    Update;
end;

{ TP3DShaderNodeEditor }

function TP3DShaderNodeEditor.GetNodeList: TP3DTreeViewObjects;
begin
  Result:= TP3DTreeViewObjects( FNodeList.AsValue );
end;

procedure TP3DShaderNodeEditor.SetNodeList(AValue: TP3DTreeViewObjects);
begin
  FNodeList.AsValue:= AValue;
end;

procedure TP3DShaderNodeEditor.NodeEditorDragDrop(Sender, Source: TP3DGraphicControl; X, Y: Float; var Accept: Boolean);
var
  Src: TP3DTreeNode;
  n: TP3DNodeClone;
  Nd: String;
begin
  if ( Source is TP3DTreeViewObjects ) then
    begin
      Src:= TP3DTreeViewObjects( Source ).DragItem;
      if ( Assigned( Src ) and ( TObject( Src.Data ) is TP3DNode )) then
        begin
          Nd:= TP3DNode( Src.Data ).Name;
          if (( Nd = 'geometry') or ( Nd = 'output' )) then
            exit;
          if ( Copy( Nd, 1, 8 ) = 'uniform_' ) then
            begin
              {n:= TP3DNodeControl( GetControl( }n:= TP3DNode( Src.Data ).Clone( Material.Uniforms );//));
              //n.Color:= Yellow500;
            end
          else
            {n:= GetControl( }n:= TP3DNode( Src.Data ).Clone( Material.NodeTree );{);}

          if ( Assigned( n )) then
            begin
              //n.Update;
              //n.TopLeft:= vec2( x, y );
              n.X:= x;
              n.Y:= y;
            end;
        end;
    end;
end;

procedure TP3DShaderNodeEditor.MouseWheel(deltax: Integer; deltay: Integer);
begin
  inherited MouseWheel(deltax, deltay);
  NodeEditor.Scroll:= NodeEditor.Scroll + vec2( deltax, 0 );
end;

procedure TP3DShaderNodeEditor.NodeEditorMouseMove(Sender: TP3DGraphicControl; X, Y: Float);
begin
  if ( P3DInput.Mouse.Buttons[ 1 ] and NodeEditor.MouseRay( P3DUI.Viewer.Mouse.X, P3DUI.Viewer.Mouse.Y )) then
    TP3DFrame( NodeEditor ).Position:= TP3DFrame( NodeEditor ).Position - 2 * vec2( P3DInput.Mouse.DX, P3DInput.Mouse.DY );
end;

procedure TP3DShaderNodeEditor.NodeEditorMouseWheel(Sender: TP3DGraphicControl; X, Y: Float);
begin
  if ( NodeEditor.MouseRay( P3DUI.Viewer.Mouse.X, P3DUI.Viewer.Mouse.Y )) then
    TP3DFrame( NodeEditor ).Zoom:= clamp( TP3DFrame( NodeEditor ).Zoom + y * 0.2, 0.2, 5 );
end;

procedure TP3DShaderNodeEditor.OnIntfSetSelection(Sender: TObject);
begin
  if ( P3DMainIntf.ActiveMaterial is TP3DMaterialNode ) then
    Material:= TP3DMaterialNode( P3DMainIntf.ActiveMaterial );
end;

{procedure TP3DShaderNodeEditor.SetToolbar(AValue: TP3DToolbar);
begin
  FToolbar.AsValue:= AValue;
end;}

procedure TP3DShaderNodeEditor.AddNode(Node: TP3DNodeClone);
var
  Nd: String;
begin
  Nd:= Node.CloneOf.Name;
  if ( Nd [1..Length( 'geometry' )] = 'geometry' ) then
    AddGeometryNode( Node )
  else if ( Nd [1..Length( 'output' )] = 'output' ) then
    AddRootNode( Node )
  else if ( Nd [1..Length( 'uniform_' )] = 'uniform_' ) then
    AddUniformNode( Node )
  else if ( Nd [1..Length( 'texture' )] = 'texture' ) then
    AddTexNode( Node )
  else
    AddAnyNode( Node );
end;

function TP3DShaderNodeEditor.AddAnyNode(Node: TP3DNodeClone): TP3DNodeControl;
begin
  //WriteLn( 'Add Node: ', Node.CloneOf.Name );
  Result:= GetControl( Node );
  if ( not Assigned( Result )) then
    Result:= AddControl( Node, NodeEditor );
  try
    Material.BuildShader;
  finally
  end;
end;

function TP3DShaderNodeEditor.AddTexNode(Node: TP3DNodeClone): TP3DNodeControl;
var
  n: LongInt;
begin
  Result:= AddAnyNode( Node );
  //Result.CanClose:= False;
  with ( TP3DPropertyEditorTexture.Create( Result.Controls )) do
    begin
      Align:= alTop;
      SendToBack;
      n:= Self.Material.Maps.Find( Node );
      //n:= StrToInt( Node.FindInput( 'Map' ).ValueProp.AsString );
      if ( InRange( n, 0, Self.Material.Maps.Count - 1 )) then
        Prop:= Self.Material.Maps[ n ].Properties.GetPropByName( 'Map' );
    end;
  Result.Color:= Blue;
  Inc( n );
end;

function TP3DShaderNodeEditor.AddUniformNode(Node: TP3DNodeClone): TP3DNodeControl;
begin
  Result:= AddAnyNode( Node );
  //Result.CanClose:= False;
  Result.Color:= Yellow;
end;

function TP3DShaderNodeEditor.AddRootNode(Node: TP3DNodeClone): TP3DNodeControl;
begin
  Result:= AddAnyNode( Node );
  Result.CanClose:= False;
  Result.Color:= Red;
end;

function TP3DShaderNodeEditor.AddGeometryNode(Node: TP3DNodeClone): TP3DNodeControl;
begin
  Result:= AddAnyNode( Node );
  Result.CanClose:= False;
  Result.Color:= Green;
end;

procedure TP3DShaderNodeEditor.SetSocketConnected(Socket: TP3DNodeSocketClone);
var
  Ctrl: TP3DNodeControl;
  Sock: TP3DNodeSocketControl;
begin
  //WriteLn( 'Set socket connected: ', Socket.CloneOf.Name );
  Ctrl:= GetControl( Socket.Node );
  if ( not Assigned( Ctrl )) then
    Ctrl:= AddControl( Socket.Node, NodeEditor );
  Sock:= Ctrl.GetSocket( Socket );
  if ( Assigned( Sock ) and Assigned( Sock.PropEd )) then
    Sock.PropEd.Visible:= not Assigned( Socket.Connected );
end;

procedure TP3DShaderNodeEditor.NodeClearSockets(Node: TP3DNodeClone);
var
  Ctrl: TP3DNodeControl;
begin
  Ctrl:= GetControl( Node );
  if ( Assigned( Ctrl )) then
    Ctrl.FUpdateSockets:= True;
end;

procedure TP3DShaderNodeEditor.RemoveNode(Node: TP3DNodeClone);
var
  Ctrl: TP3DNodeControl;
begin
  //WriteLn( 'Remove Node: ' );
  //WriteLn( 'Remove Node: ', Node.CloneOf.Name );

  //Node pointer is already set to nil at this stage
  //Ctrl:= GetControl( Node );
  Ctrl:= GetControl( nil );
  Ctrl.Free;
end;

procedure TP3DShaderNodeEditor.AddSocket(Socket: TP3DNodeSocketClone);
var
  Ctrl: TP3DNodeControl;
begin
  //WriteLn( 'Add Socket: ', Socket.CloneOf.Name );
  Ctrl:= GetControl( Socket.Node );
  if ( not Assigned( Ctrl )) then
    Ctrl:= AddControl( Socket.Node, NodeEditor );
  Ctrl.AddSocket( Socket );
end;

procedure TP3DShaderNodeEditor.RemoveSocket(Socket: TP3DNodeSocketClone);
var
  Ctrl: TP3DNodeControl;
begin
  //WriteLn( 'Remove Socket:' );
  {if ( Assigned( Socket )) then begin
    Ctrl:= GetControl( Socket.Node );
    Ctrl.RemoveSocket( Socket );
  end;}
end;

procedure TP3DShaderNodeEditor.AddNodeEvent(Node: TP3DNodeClone);
begin
  EventHandler.PushNodeEvent( @AddNode, Node );
end;

procedure TP3DShaderNodeEditor.RemoveNodeEvent(Node: TP3DNodeClone);
begin
  EventHandler.PushNodeEvent( @RemoveNode, Node );
end;

procedure TP3DShaderNodeEditor.AddSocketEvent(Socket: TP3DNodeSocketClone);
begin
  EventHandler.PushSocketEvent( @AddSocket, Socket );
end;

procedure TP3DShaderNodeEditor.RemoveSocketEvent(Socket: TP3DNodeSocketClone);
begin
  EventHandler.PushSocketEvent( @RemoveSocket, Socket );
end;

procedure TP3DShaderNodeEditor.SetSocketConnectedEvent(Socket: TP3DNodeSocketClone);
begin
  EventHandler.PushSocketEvent( @SetSocketConnected, Socket );
end;

function TP3DShaderNodeEditor.GetNodeEditor: TP3DFrame;
begin
  Result:= TP3DFrame( FNodeEditor.AsValue );
end;

{function TP3DShaderNodeEditor.GetToolbar: TP3DToolbar;
begin
  Result:= TP3DToolbar( FToolbar.AsValue );
end;}

function TP3DShaderNodeEditor.GetMaterial: TP3DMaterialNode;
begin
  Result:= TP3DMaterialNode( FMaterial.AsValue );
end;

procedure TP3DShaderNodeEditor.SetMaterial(AValue: TP3DMaterialNode);
begin
  if ( Assigned( Material )) then
    begin
      Material.NodeMaster.RemoveNodeAddHandler( @AddNodeEvent );
      Material.NodeMaster.RemoveNodeRemoveHandler( @RemoveNodeEvent );
      Material.NodeMaster.RemoveSocketAddHandler( @AddSocketEvent );
      Material.NodeMaster.RemoveSocketRemoveHandler( @RemoveSocketEvent );
      Material.NodeMaster.RemoveSocketSetConnectedHandler( @SetSocketConnectedEvent );
      Material.NodeMaster.RemoveNodeSocketsClearHandler( @NodeClearSockets );
      //Material.NodeTree.OnChange:= nil;
      //Material.Uniforms.OnChange:= nil;
    end;
  FMaterial.AsValue:= AValue;
  Update;
  if ( Assigned( Material )) then
    begin
      Material.NodeMaster.AddNodeAddHandler( @AddNodeEvent );
      Material.NodeMaster.AddNodeRemoveHandler( @RemoveNodeEvent );
      Material.NodeMaster.AddSocketAddHandler( @AddSocketEvent );
      Material.NodeMaster.AddSocketRemoveHandler( @RemoveSocketEvent );
      Material.NodeMaster.AddSocketSetConnectedHandler( @SetSocketConnectedEvent );
      Material.NodeMaster.AddNodeSocketsClearHandler( @NodeClearSockets );

      //Material.NodeTree.OnChange:= @OnNodesChange;
      //Material.Uniforms.OnChange:= @OnUniformsChange;
    end;
end;

procedure TP3DShaderNodeEditor.SetNodeEditor(AValue: TP3DFrame);
begin
  FNodeEditor.AsValue:= AValue;
end;

procedure TP3DShaderNodeEditor.OnNodesChange(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  if ( AAction = actAdd ) then
    AddControl( TP3DNodeClone( AItem ), NodeEditor )
  else if ( AAction = actDelete ) then
    //GetControl( nil ).Free // Value was already set to nil here
  else
    NodeEditor.Controls.Clear;
end;

procedure TP3DShaderNodeEditor.OnUniformsChange(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
var
  Item: TP3DNodeControl;
begin
  if ( AAction = actAdd ) then
    begin
      Item:= AddControl( TP3DNodeClone( AItem ), NodeEditor );
      Item.Color:= Yellow;
      Item.Update;
    end
  else if ( AAction = actDelete ) then
    GetControl( TP3DNodeClone( AItem )).Free
  {else
    BuiltInLeft.Controls.Clear;}
end;

function TP3DShaderNodeEditor.FindControl(AOwner: TP3DGraphicControl; ANode: TP3DNodeClone): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to NodeEditor.Controls.Count - 1 do
    if (( NodeEditor.Controls[ i ] is TP3DNodeControl ) and ( TP3DNodeControl( NodeEditor.Controls[ i ]).Node = ANode )) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DShaderNodeEditor.GetControl(ANode: TP3DNodeClone): TP3DNodeControl;
var
  n: Integer;
begin
  n:= FindControl( NodeEditor, ANode );
  if ( n > -1 ) then
    Result:= TP3DNodeControl( NodeEditor.Controls[ n ])
  else begin
    n:= FindControl( NodeEditor{BuiltInLeft}, ANode );
    if ( n > -1 ) then
      Result:= TP3DNodeControl( NodeEditor{BuiltInLeft}.Controls[ n ])
    else
      Result:= nil;{
    else begin
      n:= FindControl( NodeEditor{BuiltInRight}, ANode );
      if ( n > -1 ) then
        Result:= TP3DNodeControl( NodeEditor{BuiltInRight}.Controls[ n ])
      else
        Result:= nil;
    end;}
  end;
end;

procedure TP3DShaderNodeEditor.AlignConnected(ANodeCtrl: TP3DNodeControl; ARecurse: Boolean; AMargin: TVec2 );
var
  LineTop: Float = 0;

  procedure Align( Node: TP3DNodeClone );
  var
    Ctrl: TP3DNodeControl;
  begin
    Ctrl:= GetControl( Node );
    Ctrl.Left:= ANodeCtrl.Left - Ctrl.Width - AMargin.x;
    Ctrl.Top:= LineTop;
    LineTop:= LineTop + Ctrl.Height + AMargin.y;
    if ( ARecurse ) then
      AlignConnected( Ctrl, ARecurse, AMargin );
  end;

var
  Socket: TP3DNodeSocketClone;
begin
  for Socket in ANodeCtrl.Node.Inputs do
    if ( Assigned( Socket.Connected )) then
      Align( Socket.Connected.Node );
end;

procedure TP3DShaderNodeEditor.Logic();
begin
  //if ( Material.LastUpdate > LastUpdate ) then
  //  NeedsUpdate:= True;

  while EventHandler.HandleFirstEvent do;

  inherited Logic();
end;

function TP3DShaderNodeEditor.AddControl(ANode: TP3DNodeClone; AParent: TP3DGraphicControl): TP3DNodeControl;
begin
  Result:= TP3DNodeControl.Create( AParent.Controls );
  Result.Node:= ANode;
  Result.OnClose:= @OnNodeClose;
end;

procedure TP3DShaderNodeEditor.OnNodeClose(ASender: TP3DGraphicControl);
begin
  TP3DNodeControl( ASender ).Node.Free;
end;

constructor TP3DShaderNodeEditor.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);

  {FToolbar:= TP3DStreamablePointer.Create( 'Toolbar', TP3DToolbar );
  Properties.Add( FToolbar );
  Toolbar:= TP3DToolbar.Create( Controls );
  with ( Toolbar ) do
    begin
      Align:= alBottom;
      Height:= 24;
      Font.Name:= P3DSymbolFontName;
      Buttons.Text:= P3DSymbolPlus + P3DSymbolMaterial + LineEnding +
                     P3DSymbolMaterial + P3DSymbolArrowRight + P3DSymbolMesh + LineEnding +
                     P3DSymbolOptions  + P3DSymbolArrowRight + P3DSymbolShader;
    end;}

  FNodeEditor:= TP3DStreamablePointer.Create( 'NodeEditor', TP3DFrame );
  Properties.Add( FNodeEditor );
  NodeEditor:= TP3DFrame.Create( Controls );//TP3DGraphicControl.Create( Controls );
  NodeEditor.Align:= alClient;
  NodeEditor.OnDragDrop:= @NodeEditorDragDrop;
  NodeEditor.OnMouseMove:= @NodeEditorMouseMove;
  NodeEditor.OnMouseWheel:= @NodeEditorMouseWheel;
  NodeEditor.PageModeX:= pmScroll;

  {FBuiltInLeft:= TP3DStreamablePointer.Create( 'BuiltInLeft', TP3DGraphicControl );
  Properties.Add( FBuiltInLeft );
  BuiltInLeft:= TP3DGraphicControl.Create( Controls );
  BuiltInLeft.Align:= alLeft;
  BuiltInLeft.PageModeX:= pmExpand;}

  FNodeList:= TP3DStreamablePointer.Create( 'NodeList', TP3DGraphicControl );
  Properties.Add( FNodeList );
  NodeList:= TP3DTreeViewObjects.Create( Controls );
  with ( NodeList ) do
    begin
      Align:= alRight;
      Width:= 200;
      Obj:= P3DData.P3DShaderNodeLib;
      Update;
      ShowSymbols:= True;
      AllowDrag:= True;
    end;
  TP3DSplitter.Create( Controls ).Align:= alRight;

  {FBuiltInRight:= TP3DStreamablePointer.Create( 'BuiltInRight', TP3DGraphicControl );
  Properties.Add( FBuiltInRight );
  BuiltInRight:= TP3DGraphicControl.Create( Controls );
  BuiltInRight.Align:= alRight;
  BuiltInRight.PageModeX:= pmExpand;}

  FMaterial:= TP3DStreamablePointer.Create( 'Material', TP3DMaterialNode );
  Properties.Add( FMaterial );

  if ( Assigned( P3DMainIntf )) then
    P3DMainIntf.AddSetActiveMaterialHandler( @OnIntfSetSelection );
end;

destructor TP3DShaderNodeEditor.Destroy;
begin
  while ( EventHandler.HandleFirstEvent ) do;

  if ( Assigned( P3DMainIntf )) then
    P3DMainIntf.RemoveSetSelectionHandler( @OnIntfSetSelection );

  Material:= nil;
  inherited Destroy;
end;

procedure TP3DShaderNodeEditor.Draw;
begin
  //Canvas.RenderRect( vec2( 0 ), ClientRect.WidthHeight, Red900 );
  inherited Draw;
end;

procedure TP3DShaderNodeEditor.Update;

  {procedure CheckNodeLevel( ANode: TP3DNodeClone );
  var
    inp: TP3DNodeSocketClone;
    Skip: Boolean;
  begin
    Skip:= False;
    for inp in ANode.Inputs do
      if ( inp.Connected ) then
        begin
          Skip:= True;
          break;
        end;
    if ( Skip ) then
      exit;
  end;}

var
  Node: TP3DNodeClone;
  n: Integer;
  LastTop: Single = 0;
begin
  NodeEditor.Controls.Clear;

  if ( not Assigned( Material )) then
    exit;
  for Node in Material.NodeTree do
    AddAnyNode( Node );
  if ( Assigned( Material.RootNode )) then
    AddRootNode( Material.RootNode );

  if ( Assigned( Material.GeometryNode )) then
    AddGeometryNode( Material.GeometryNode );
  for Node in Material.Maps do
    AddTexNode( Node );
  for Node in Material.Uniforms do
    AddUniformNode( Node );
  inherited Update;
end;

{ TP3DShaderNodeEditor.TP3DNodeEventHandler }

procedure TP3DShaderNodeEditor.TP3DNodeEventHandler.PushEvent(AMethod: TNotifyEvent; Obj: TP3DStreamable);
var
  Event: PP3DNodeEvent;
begin
  Event:= GetMem( SizeOf( TP3DNodeEvent) );
  Event^.Method:= AMethod;
  Event^.Next:= nil;
  Event^.Obj:= Obj;
  if ( Assigned( FLast )) then
    FLast^.Next:= Event
  else if ( Assigned( First )) then
    First^.Next:= Event
  else
    First:= Event;
  Last:= Event;
end;

procedure TP3DShaderNodeEditor.TP3DNodeEventHandler.PushNodeEvent(AMethod: TP3DNodeMasterNodeHandler;
  ANode: TP3DNodeClone);
begin
  PushEvent( TNotifyEvent( AMethod ), ANode );
end;

procedure TP3DShaderNodeEditor.TP3DNodeEventHandler.PushSocketEvent(AMethod: TP3DNodeMasterSocketHandler;
  ASocket: TP3DNodeSocketClone);
begin
  PushEvent( TNotifyEvent( AMethod ), ASocket );
end;

function TP3DShaderNodeEditor.TP3DNodeEventHandler.HandleFirstEvent: Boolean;
var
  _First: PP3DNodeEvent;
begin
  if ( not Assigned( First )) then
    exit( False );

  Result:= True;

  if ( First = Last ) then
    Last:= nil;
  First^.Method( First^.Obj );
  _First:= First;
  First:= First^.Next;
  Freemem( _First );
end;
{$ENDIF}

