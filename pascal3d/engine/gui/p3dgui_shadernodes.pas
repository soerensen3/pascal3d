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
    p3dshadernodes,
    p3dmodel,
    p3dscene,
    p3dgui_sceneviewer,
    p3dbuffers,
    dglOpenGL,
    Classes,
    math,
    Types;

type

  { TP3DGUIShaderOutlineConnector }

  TP3DGUIShaderOutlineConnector = class ( TP3DGraphicControl )
    private
      FColor: TVec4;
      FPEnd: TVec2;
      FPStart: TVec2;

      property PStart: TVec2 read FPStart write FPStart;
      property PEnd: TVec2 read FPEnd write FPEnd;

    public
      constructor Create(AOwner: TObjectList; AManager: TGUIManager;
        const AParent: TP3DGraphicControl=nil);
      procedure Draw; override;
      procedure Update( S: TVec2; E: TVec2 );
      procedure UpdateS( S: TVec2 );
      procedure UpdateE( E: TVec2 );

      property Color: TVec4 read FColor write FColor;
  end;


  { TP3DGUIShaderOutlineFragment }

  TP3DGUIShaderOutlineFragment = class ( TP3DGroupBox )
    private
      FConnectors: TObjectList;
      FOutlineFragment: TP3DShaderNodeOutlineFragment;
      FIsMoving: Boolean;
      FParentConnector: TP3DGUIShaderOutlineConnector;

      procedure SetOutlineFragment(AValue: TP3DShaderNodeOutlineFragment);
      procedure Update;
      procedure OnChange( Sender: TObject );
      procedure MouseMove( X, Y: Integer ); override;
      procedure MouseDownEvnt( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
      procedure UpdatePC( Sender: TObject ); //Update Parent Connector Callback

    public
      procedure Draw; override;

      constructor Create(AOwner: TObjectList; AManager: TGUIManager;
        const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

      property Connectors: TObjectList read FConnectors write FConnectors;
      property ParentConnector: TP3DGUIShaderOutlineConnector read FParentConnector write FParentConnector;

    published
      property OutlineFragment: TP3DShaderNodeOutlineFragment read FOutlineFragment write SetOutlineFragment;
  end;

  { TP3DGUIShaderPreview }

  TP3DGUIShaderPreview = class ( TP3DGUISceneViewer )
  private
    Fworld: TMat4;
    private
      FShaderCompiled: TP3DShaderCompiled;
      FShaderObject: TShader;
      FModelCube: TModelFile;
      FShader: TP3DShaderNodeOutline;
      FMoving: Boolean;
      FXRot: Single;
      FYRot: Single;
      FZoom: Single;

      procedure SetModelCube( AValue: TModelFile );
      procedure SetShader( AValue: TP3DShaderNodeOutline );
      procedure DrawObjects( AScene: tScene );
      procedure MouseMove( X, Y: Integer ); override;
      function MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ): TP3DGraphicControl; override;

      property ShaderCompiled: TP3DShaderCompiled read FShaderCompiled write FShaderCompiled;
      property ShaderObject: TShader read FShaderObject write FShaderObject;
      property Scene;
      property XRot: Single read FXRot write FXRot;
      property YRot: Single read FYRot write FYRot;
      property Zoom: Single read FZoom write FZoom;
      property world: TMat4 read Fworld write Fworld;

    public
      constructor Create(AOwner: TObjectList; AManager: TGUIManager;
        const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;
      procedure Update;

      property ModelCube: TModelFile read FModelCube write SetModelCube;
      property Shader: TP3DShaderNodeOutline read FShader write SetShader;
  end;


implementation

{ TP3DGUIShaderPreview }

procedure TP3DGUIShaderPreview.SetModelCube(AValue: TModelFile);
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

function TP3DGUIShaderPreview.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer
  ): TP3DGraphicControl;
begin
  if (( gcisMouseOver in InputState ) and ( mb1 )) then
    FMoving:= True;
end;

constructor TP3DGUIShaderPreview.Create(AOwner: TObjectList;
  AManager: TGUIManager; const AParent: TP3DGraphicControl);
begin
  inherited;
  Scene:= tScene.Create;
  Scene.Cam:= tCamera.Create;
  FZoom:= 1;
  //Scene.Cam.Position:= vec3( 0, 0, 4 );
  Scene.DrawObjectsObj:= @DrawObjects;
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

{ TP3DGUIShaderOutlineConnector }

constructor TP3DGUIShaderOutlineConnector.Create(AOwner: TObjectList;
  AManager: TGUIManager; const AParent: TP3DGraphicControl);
begin
  inherited;
  Color:= vec4( 0, 0, 0, 1 );
end;

procedure TP3DGUIShaderOutlineConnector.Draw;
begin
  inherited Draw;
  Canvas.RenderLines([ FPStart, vec2(( FPStart.X + FPEnd.X ) / 2, FPStart.Y ),
                       vec2(( FPStart.X + FPEnd.X ) / 2, FPEnd.Y ), FPEnd ], Color );
end;

procedure TP3DGUIShaderOutlineConnector.Update(S: TVec2; E: TVec2);
begin
  Left:= round( min( S.X, E.X ));
  Top:= round( min( S.Y, E.Y ));
  FPStart:= S - vec2( Left, Top );
  FPEnd:= E - vec2( Left, Top );
  Width:= round( FPStart.x + FPEnd.x ) + 2;
  Height:= round( FPStart.y + FPEnd.y ) + 2;
end;

procedure TP3DGUIShaderOutlineConnector.UpdateS(S: TVec2);
var
  E: TVec2;
begin
  E:= PEnd + vec2( Left, Top );
  Update( S, E );
end;

procedure TP3DGUIShaderOutlineConnector.UpdateE(E: TVec2);
var
  S: TVec2;
begin
  S:= PStart + vec2( Left, Top );
  Update( S, E );
end;

{ TP3DGUIShaderOutlineFragment }

procedure TP3DGUIShaderOutlineFragment.SetOutlineFragment(
  AValue: TP3DShaderNodeOutlineFragment);
begin
  if FOutlineFragment=AValue then Exit;
  FOutlineFragment:=AValue;
  FOutlineFragment.OnChange:= @OnChange;
  FOutlineFragment.OnUpdateChild:= @UpdatePC;
  Update;
end;

procedure TP3DGUIShaderOutlineFragment.Draw;
begin
  inherited Draw;
end;

procedure TP3DGUIShaderOutlineFragment.Update;
var
  i: Integer;
  lbl: TP3DLabel;
  Connector: TP3DGUIShaderOutlineConnector;
  j: Integer;
  btn: TP3DButton;
begin
  Controls.Clear( True );
  Connectors.Clear;
  Caption:= OutlineFragment.Name;

  Left:= Round( OutlineFragment.X );
  Top:= Round( OutlineFragment.Y );
  Width:= 200;

  btn:= TP3DButton.Create( ParentList, Manager, Self );
  btn.Left:= 0;
  btn.Top:= 40 - ClientRect.Top - 5;
  btn.Width:= 10;
  btn.Height:= 10;
  btn.Caption:= '';

  if ( Assigned( ParentConnector )) then
    ParentConnector.UpdateE( vec2( OutlineFragment.X, OutlineFragment.Y + 40 ));
  for i:= 0 to OutlineFragment.Inputs.Count - 1 do
    begin
      lbl:= TP3DLabel.Create( ParentList, Manager, Self );
      lbl.Caption:= OutlineFragment.Inputs[ i ].Name;
      lbl.Height:= lbl.Font.Size + 4;
      lbl.Width:= ClientRect.Right - ClientRect.Left - 10;
      lbl.Top:= i * 50;
      lbl.Left:= 0;
      lbl.Alignment:= taRightJustify;
      btn:= TP3DButton.Create( ParentList, Manager, Self );
      btn.Left:= ClientRect.Right - 10;
      btn.Top:= lbl.Top + lbl.Height div 2 - 5;
      btn.Width:= 10;
      btn.Height:= 10;
      btn.Caption:= '';
      for j:= 0 to OutlineFragment.Inputs[ i ].Fragments.Count - 1 do
        begin
          Connector:= TP3DGUIShaderOutlineConnector.Create( ParentList, Manager, Self.Parent );
          Connector.Visible:= False;
          Connector.Update( vec2( Left + Width, i * 50 + Top + 30 ),
                            vec2( OutlineFragment.Inputs[ i ].Fragments[ j ].X,
                                  OutlineFragment.Inputs[ i ].Fragments[ j ].Y + 40 ));
          if ( Assigned( OutlineFragment.Inputs[ i ].Fragments[ j ].OnUpdateChild )) then
            OutlineFragment.Inputs[ i ].Fragments[ j ].OnUpdateChild( Connector );
          Connectors.Add( Connector );
        end;
    end;

  Height:= i * 50 + 70;
end;

procedure TP3DGUIShaderOutlineFragment.OnChange(Sender: TObject);
begin
  Update;
end;

procedure TP3DGUIShaderOutlineFragment.MouseMove(X, Y: Integer);
begin
  inherited MouseMove( X, Y );
  if ( FIsMoving and InputManager.Mouse.Buttons[ 0 ]) then
    begin
      OutlineFragment.X:= Left + InputManager.Mouse.DX;
      OutlineFragment.Y:= Top + InputManager.Mouse.DY;
    end
  else
    FIsMoving:= False;
end;

procedure TP3DGUIShaderOutlineFragment.MouseDownEvnt(
  Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  FIsMoving:= mb1;
end;

procedure TP3DGUIShaderOutlineFragment.UpdatePC(Sender: TObject);
begin
  if ( Sender is TP3DGUIShaderOutlineConnector ) then
    ParentConnector:= TP3DGUIShaderOutlineConnector( Sender );
end;

constructor TP3DGUIShaderOutlineFragment.Create(AOwner: TObjectList;
  AManager: TGUIManager; const AParent: TP3DGraphicControl);
begin
  inherited;
  FOnMouseDown:= @MouseDownEvnt;
  FConnectors:= TObjectList.Create;
end;

destructor TP3DGUIShaderOutlineFragment.Destroy;
begin
  FConnectors.Free;
  inherited Destroy;
end;

end.

