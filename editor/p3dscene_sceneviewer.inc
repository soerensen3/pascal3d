{$IFDEF INTERFACE}

  TEditMode = ( emDefault, emGrab, emRotate, emScale );
  TAxisMode = ( amScreen, amGlobal, amLocal );
  TAxisLock = ( alNone, alX, alY, alZ, alXY, alXZ, alYZ );


  TP3DScenePanel = class;

{ TP3DSceneEditModeDefault }

  TP3DSceneEditModeDefault = class ( TPersistent )
    private
      FAxisLock: TAxisLock;
      FAxisMode: TAxisMode;
      FAxisX: TVec3;
      FAxisY: TVec3;
      FAxisZ: TVec3;
      FMouseDelta: TVec2;
      FMousePos: TVec2;
      FMousePosInitial: TVec2;
      FScenePanel: TP3DScenePanel;
      FTransCenter: TVec3;
      FTransMat: TMat4;

    public
      constructor Create( AScenePanel: TP3DScenePanel );

      procedure Reset; virtual;
      procedure BeforeRender; virtual;
      procedure AfterRender; virtual;
      procedure CheckInput; virtual;
      function PickPixel( P: TVec2 ): TP3DActor;

      procedure FinishEditing;
      procedure CancelTransform;
      procedure RenderSelection( world: TMat4 );
      function StatusText: String; virtual;
      function AxisText: String; virtual;
      procedure UpdateAxis; virtual;
      procedure ToggleAxis( Axis: TAxisLock ); virtual;
      procedure RenderSelectionAxis;
      procedure RenderEditorAxis;
      function ToScreenCoords( p: TVec3 ): TVec3;

      property AxisLock: TAxisLock read FAxisLock write FAxisLock;
      property AxisMode: TAxisMode read FAxisMode write FAxisMode;
      property ScenePanel: TP3DScenePanel read FScenePanel;
      property TransMat: TMat4 read FTransMat write FTransMat;
      property MousePosInitial: TVec2 read FMousePosInitial write FMousePosInitial;
      property MousePos: TVec2 read FMousePos write FMousePos;
      property MouseDelta: TVec2 read FMouseDelta write FMouseDelta;
      property AxisX: TVec3 read FAxisX write FAxisX;
      property AxisY: TVec3 read FAxisY write FAxisY;
      property AxisZ: TVec3 read FAxisZ write FAxisZ;
      property TransCenter: TVec3 read FTransCenter write FTransCenter;
  end;

  { TP3DSceneEditModeGrab }

  TP3DSceneEditModeGrab = class ( TP3DSceneEditModeDefault )
    private
      Translation: TVec3;

    public
      procedure AfterRender; override;
      procedure CheckInput; override;
      function StatusText: String; override;
  end;

  { TP3DSceneEditModeRotate }

  TP3DSceneEditModeRotate = class ( TP3DSceneEditModeDefault )
    private
      Angle: Float;

    public
      procedure CheckInput; override;
      procedure AfterRender; override;
      function StatusText: String; override;
  end;

  { TP3DSceneEditModeScale }

  TP3DSceneEditModeScale = class ( TP3DSceneEditModeDefault )
    protected
      Scale: TVec3;

    public
      function StatusText: String; override;
      procedure CheckInput; override;
  end;

  { TP3DScenePanel }

  TP3DScenePanel = class( TP3DSceneViewer )
    private
      FEditMode: TEditMode;
      FEditor: TP3DSceneEditModeDefault;
      FSelection: TP3DActor;
      FSelectionMaterial: TP3DMaterialBase;
      FPickMaterial: TP3DMaterialBase;
      FInfoText: TP3DText;
      FRenderList: TP3DRenderList;
      FCanvas3D: TP3DCanvas3D;

      procedure Push3DMode;

      function GetEditor: TP3DSceneEditModeDefault;
      procedure SceneDragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean );
      procedure SetEditMode(AValue: TEditMode);
      procedure SetSelection(AValue: TP3DActor);
      procedure RenderCallback( world: TMat4; AActor: TP3DActor );
      procedure DrawArmature( ArmatureObject: TP3DActor ); // TEMP

    public
      constructor Create(const AData: TP3DData = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      procedure KeyboardAction; override;
      procedure MouseMove(X, Y: Integer); override;
      function Pick( x, y: Integer ): TP3DActor;
      function GetSelectionCenter: TVec3;
      procedure Realign; override;
      procedure UpdateCanvas3D;

      procedure BeforeDraw( Sender: TObject );
      procedure AfterDraw( Sender: TObject );

    published
      property SelectionMaterial: TP3DMaterialBase read FSelectionMaterial write FSelectionMaterial;
      property EditMode: TEditMode read FEditMode write SetEditMode;
      property Editor: TP3DSceneEditModeDefault read GetEditor;
      property Selection: TP3DActor read FSelection write SetSelection;
      property RenderList: TP3DRenderList read FRenderList write FRenderList;
      property Cam: TP3DCamera read FCam write FCam;
      property CamObject: TP3DActor read FCamObject write FCamObject;
      property Canvas3D: TP3DCanvas3D read FCanvas3D write FCanvas3D;
  end;


var
  EditorSceneArrow: TP3DScene;
  EditorSceneArrows: TP3DScene;
  EditorSceneLamp: TP3DScene;
  EditorSceneLampMaterial: TP3DMaterialBase;
  EditModeDefault: TP3DSceneEditModeDefault;
  EditModeGrab: TP3DSceneEditModeGrab;
  EditModeScale: TP3DSceneEditModeScale;
  EditModeRotate: TP3DSceneEditModeRotate;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure CreateEditModes;
begin
  if ( not Assigned( EditModeDefault )) then
    EditModeDefault:= TP3DSceneEditModeDefault.Create( SceneView );
  if ( not Assigned( EditModeGrab )) then
    EditModeGrab:= TP3DSceneEditModeGrab.Create( SceneView );
  if ( not Assigned( EditModeScale )) then
    EditModeScale:= TP3DSceneEditModeScale.Create( SceneView );
  if ( not Assigned( EditModeRotate )) then
    EditModeRotate:= TP3DSceneEditModeRotate.Create( SceneView );
end;

procedure DestroyEditModes;
begin
  FreeAndNil( EditModeDefault );
  FreeAndNil( EditModeGrab );
  FreeAndNil( EditModeScale );
  FreeAndNil( EditModeRotate );
end;

procedure CreateEditorScenes;
var
  n: Integer;
begin
  n:= OpenLibrary( 'armature_test/media/EditorArrow.p3d' );
  P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneArrow:= P3DData.Libraries[ n ].Scenes[ 0 ];
  n:= OpenLibrary( 'armature_test/media/EditorArrows.p3d' );
  P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneArrows:= P3DData.Libraries[ n ].Scenes[ 0 ];
  n:= OpenLibrary( 'armature_test/media/EditorLamp.p3d' );
  P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneLamp:= P3DData.Libraries[ n ].Scenes[ 0 ];
  EditorSceneLampMaterial:= TP3DMaterialBase( P3DData.Libraries[ n ].Materials[ 0 ]);
end;

{ TP3DSceneEditModeScale }

function TP3DSceneEditModeScale.StatusText: String;
begin
  Result:= 'Scale: ' + AxisText + Scale.ToString( 2 );
end;

procedure TP3DSceneEditModeScale.CheckInput;
begin
  inherited CheckInput;

  if ( not Assigned( ScenePanel.Selection )) then
    begin
      ScenePanel.EditMode:= emDefault;
      exit;
    end;

  ScenePanel.Selection.Matrix:= TransMat;
  Scale:= vec3( 1 );
  if ( AxisLock in [ alX, alXZ, alXY, alNone ]) then
    Scale.x:= MouseDelta.x;
  if ( AxisLock in [ alY, alXY, alYZ, alNone ]) then
    Scale.y:= MouseDelta.y;
  if ( AxisLock in [ alZ, alXZ, alYZ ]) then
    Scale.z:= MouseDelta.y;
  if ( Scale <> vec3( 1 )) then
    ScenePanel.Selection.Scale:= Scale;
end;

{ TP3DSceneEditModeRotate }

procedure TP3DSceneEditModeRotate.CheckInput;
var
  c: TVec3;
  dir, dirOld: TVec2;
begin
  inherited CheckInput;

  if ( not Assigned( ScenePanel.Selection )) then
    begin
      ScenePanel.EditMode:= emDefault;
      exit;
    end;

  ScenePanel.Selection.Matrix:= TransMat;
  c:= ToScreenCoords( TransCenter );
  dir:= ( MousePos - c.XY ).Normalize;
  dirOld:= ( MousePosInitial - c.XY ).Normalize;
  Angle:= arctan2( dirOld.y, dirOld.x ) - arctan2( dir.y, dir.x );
  P3DApplication.MainWindow.Title:= FloatToStr( Angle );
  if ( AxisLock in [ alX, alYZ ]) then
    ScenePanel.Selection.RotateAroundGlobalAxis( AxisX, Angle );
  if ( AxisLock in [ alY, alXZ, alNone ]) then
    ScenePanel.Selection.RotateAroundGlobalAxis( AxisY, Angle );
  if ( AxisLock in [ alZ, alXY ]) then
    ScenePanel.Selection.RotateAroundGlobalAxis( AxisZ, Angle );
end;

procedure TP3DSceneEditModeRotate.AfterRender;
var
  c, dir, dirOld: TVec2;
  n: Integer;
  Angle1, Angle2: Float;
begin
  with ( ScenePanel ) do
    begin
      Canvas.Lock;
      n:= RenderList.Find( Selection );
      if ( n >= 0 ) then
        begin
          c:= ToScreenCoords( TransCenter ).XY;
          Canvas.RenderCircle( c.XY, 5, 16, vec4( 1, 0, 0, 1 ));
          Canvas.RenderLine( c.XY, vec2( P3DInput.Mouse.X, P3DInput.Mouse.Y ) - vec2( Canvas.Left, Canvas.Top ), vec4( 0, 0, 0, 1 ));
          dir:= ( MousePos - c.XY ).Normalize;
          dirOld:= ( MousePosInitial - c.XY ).Normalize;
          Angle1:= arctan2( dirOld.y, dirOld.x ) / deg2rad;
          Angle2:= arctan2( dir.y, dir.x ) / deg2rad;
          Canvas.RenderArc( c, 50, Angle1, Angle2, 64, vec4( 0, 0, 1, 0.2 ));
        end;
      ScenePanel.Canvas.Unlock();
    end;
end;

function TP3DSceneEditModeRotate.StatusText: String;
begin
  Result:= 'Rotation: ' + AxisText + FloatToStrF( Angle / deg2rad, ffNumber, 8, 2 );
end;

{ TP3DSceneEditModeGrab }

procedure TP3DSceneEditModeGrab.AfterRender;
begin
  inherited AfterRender;
  RenderEditorAxis;
end;

procedure TP3DSceneEditModeGrab.CheckInput;
begin
  ScenePanel.Selection.Matrix:= TransMat;
  inherited CheckInput;
  if ( not Assigned( ScenePanel.Selection )) then
    begin
      ScenePanel.EditMode:= emDefault;
      exit;
    end;
  Translation:= vec3( 0 );
  if ( AxisLock in [ alX, alXZ, alXY, alNone ]) then
    Translation:= AxisX * MouseDelta.x;
  if ( AxisLock in [ alY, alXY, alYZ ]) then
    Translation-= AxisY * MouseDelta.y;
  if ( AxisLock in [ alZ, alXZ, alYZ, alNone ]) then
    Translation-= AxisZ * MouseDelta.y;

  ScenePanel.Selection.Position:= ScenePanel.Selection.Position + Translation;
  TransCenter:= ScenePanel.GetSelectionCenter;
end;

function TP3DSceneEditModeGrab.StatusText: String;
begin
  Result:= 'Translation: ' + AxisText + Translation.ToString( 2 );
end;

{ TP3DSceneEditModeDefault }

constructor TP3DSceneEditModeDefault.Create(AScenePanel: TP3DScenePanel);
begin
  inherited Create;
  FScenePanel:= AScenePanel;
end;

procedure TP3DSceneEditModeDefault.Reset;
begin
  if (( ScenePanel.EditMode <> emDefault ) and Assigned( ScenePanel.Selection )) then
    ScenePanel.Selection.Matrix:= TransMat;
  MouseDelta:= vec2( 0 );
  MousePosInitial:= vec2( -1 );
  AxisMode:= amScreen;
  AxisLock:= alNone;
end;

procedure TP3DSceneEditModeDefault.BeforeRender;
begin

end;

procedure TP3DSceneEditModeDefault.AfterRender;
begin
  glDisable( GL_DEPTH_TEST );
  ScenePanel.Push3DMode;
  RenderSelectionAxis;
  P3DViewports.Pop;
  glEnable( GL_DEPTH_TEST );
end;

procedure TP3DSceneEditModeDefault.CheckInput;
var
  x, y, z: LongInt;
begin
  if ( ScenePanel.EditMode in [ emGrab, emRotate, emScale ]) then
    begin
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_X ] = ksPressed ) then
        ToggleAxis( alX );
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_Y ] = ksPressed ) then
        ToggleAxis( alY );
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_Z ] = ksPressed ) then
        ToggleAxis( alZ );
      if ( P3DInput.Mouse.Buttons[ 0 ]) then
        FinishEditing;
    end;

  if ( P3DInput.Mouse.Buttons[ 2 ] and ( gcisMouseOver in ScenePanel.InputState )) then
    if ( ScenePanel.EditMode = emDefault ) then
      ScenePanel.Selection:= PickPixel( MousePos )
    else
      CancelTransform;
end;

function TP3DSceneEditModeDefault.PickPixel(P: TVec2): TP3DActor;
begin
  Result:= ScenePanel.Pick( round( P.X ), round( P.Y ));
end;

procedure TP3DSceneEditModeDefault.FinishEditing;
begin
  ScenePanel.EditMode:= emDefault;
end;

procedure TP3DSceneEditModeDefault.CancelTransform;
begin
  Reset;
  FinishEditing;
end;

procedure TP3DSceneEditModeDefault.RenderSelection( world: TMat4 );
begin
//  glEnable( GL_DEPTH_TEST );
  glDepthMask( False );
  ScenePanel.SelectionMaterial.Diff:= Accent500.XYZ;//vec3( 1, 1, 0 );
  //SelectionMaterial.Alpha:= 0.2;
  ScenePanel.SelectionMaterial.PassToShader();
  ScenePanel.Cam.PassToShader( Mat4Identity );
  if ( Assigned( ScenePanel.Selection )) then
    begin
      if ( ScenePanel.Selection.Data is TP3DMesh ) then
        begin
          glLineWidth( 2 );
          TP3DMesh( ScenePanel.Selection.Data ).RenderEdges( world, TestScene, ScenePanel.Selection );
          glLineWidth( 1 );
        end;
      ScenePanel.Canvas.Material.PassToShader();
    end;
  glDepthMask( True );
//  glDisable( GL_DEPTH_TEST );
end;

function TP3DSceneEditModeDefault.StatusText: String;
begin
  Result:= ''
end;

function TP3DSceneEditModeDefault.AxisText: String;
begin
  case ( AxisMode ) of
    amScreen: Result:= ' [screen';
    amLocal: Result:= ' [local';
    amGlobal: Result:= ' [global';
  end;

  case ( AxisLock ) of
    alX: Result+= ' X]';
    alY: Result+= ' Y]';
    alZ: Result+= ' Z]';
    alXY: Result+= ' XY]';
    alXZ: Result+= ' XZ]';
    alYZ: Result+= ' YZ]';
    alNone: Result+= ']';
  end;
end;

procedure TP3DSceneEditModeDefault.UpdateAxis;
begin
  case AxisMode of
    amScreen:
      begin
        AxisX:= ScenePanel.Cam.InView.Row[ 0 ].xyz;
        AxisY:= ScenePanel.Cam.InView.Row[ 2 ].xyz;
        AxisZ:= ScenePanel.Cam.InView.Row[ 1 ].xyz;
      end;
    amGlobal:
      begin
        AxisX:= vec3_Axis_PX;
        AxisY:= vec3_Axis_PY;
        AxisZ:= vec3_Axis_PZ;
      end;
    amLocal:
      begin
        AxisX:= TransMat.Row[ 0 ].xyz.Normalize;
        AxisY:= TransMat.Row[ 1 ].xyz.Normalize;
        AxisZ:= TransMat.Row[ 2 ].xyz.Normalize;
      end;
  end;
end;

procedure TP3DSceneEditModeDefault.ToggleAxis(Axis: TAxisLock);
begin
  if ( Axis <> AxisLock ) then
    begin
      AxisMode:= amGlobal;
      AxisLock:= Axis;
    end
  else
    case AxisMode of
      amScreen: AxisMode:= amGlobal;
      amGlobal: AxisMode:= amLocal;
      amLocal: begin AxisMode:= amScreen; AxisLock:= alNone; end;
    end;
end;

procedure TP3DSceneEditModeDefault.RenderSelectionAxis;
begin
  if ( Assigned( ScenePanel.Selection )) then
    EditorSceneArrows.RenderFromCamera( ScenePanel.CamObject, mat4translate( vec4( TransCenter, 1 )));
end;

procedure TP3DSceneEditModeDefault.RenderEditorAxis;
var
  Origin: TVec3;
  procedure DrawAxis( Dir: TVec3; AColor: TVec3 );
  var
    p1, p2: TVec3;
  begin
    with ( ScenePanel ) do
      begin
        p1:= ToScreenCoords( Origin + Dir * 100 );
        p2:= ToScreenCoords( Origin - Dir * 100 );
        Canvas.RenderLine( p1.XY, p2.XY, vec4( AColor, 1.0 ));
      end;
  end;

begin
  ScenePanel.Canvas.Lock;
  Origin:= ScenePanel.Cam.View.Row[ 3 ].xyz + ScenePanel.Cam.View.Row[ 2 ].xyz * 10;
  DrawAxis( AxisX, vec3( 1, 0, 0 ));
  DrawAxis( AxisY, vec3( 0, 1, 0 ));
  DrawAxis( AxisZ, vec3( 0, 0, 1 ));
  ScenePanel.Canvas.Unlock();
end;

function TP3DSceneEditModeDefault.ToScreenCoords(p: TVec3): TVec3;
var
  c: TVec4;
begin
  with ( ScenePanel ) do
    begin
      c:= ( Cam.Proj * Cam.View * vec4( p, 1 ));
      c:= vec4( c.x / c.w, -c.y / c.w, c.z / c.w, 1 );
      c:= vec4(( c.XY * 0.5 + 0.5 ) * vec2( ClientRect.Width, ClientRect.Height ) + ClientRect.TopLeft, ( 1 + c.Z ) * 0.5, 1 );
      Result:= c.XYZ;
    end;
end;

{ TP3DScenePanel }

procedure TP3DScenePanel.SceneDragDrop(Sender, Source: TP3DGraphicControl; X,
  Y: Integer; var Accept: Boolean);
begin
  if ( Source is TP3DListViewFile ) then
    with ( TP3DListViewFile( Source )) do
    begin
      if ( Assigned( Selection )) then
        if ( P3DMatchFileName( Selection.Caption, '*.p3d' )) then
          begin
            try
              TestScene.AppendFile( AppendPathDelim( Directory ) + Selection.Caption );
            except
              on E: Exception do
                SDL_ShowSimpleMessageBox( SDL_MESSAGEBOX_ERROR, PChar( 'Error loading file ' + Selection.Caption ), PChar( E.Message ), Nil );
            end;
            DataView.Update;
            DataView.Children.ExpandAll();
          end;
    end;
  Accept:= False;
end;

procedure TP3DScenePanel.Push3DMode;
begin
  PushVPClientRect;
end;

function TP3DScenePanel.GetEditor: TP3DSceneEditModeDefault;
begin
  if ( not Assigned( FEditor )) then
    FEditor:= EditModeDefault;
  Result:= FEditor;
end;

procedure TP3DScenePanel.SetEditMode(AValue: TEditMode);
var
  n: Integer;
begin
  if FEditMode=AValue then Exit;

  if ( AValue = emDefault ) then
    begin
      FEditMode:= emDefault;
      FEditor:= EditModeDefault;
    end
  else if ( Assigned( Selection )) then
    begin
      Editor.CancelTransform;
      case AValue of
        emGrab: FEditor:= EditModeGrab;
        emRotate: FEditor:= EditModeRotate;
        emScale: FEditor:= EditModeScale;
      end;

      n:= FRenderList.Find( Selection );
      if ( n >= 0 ) then
        Editor.TransMat:= FRenderList[ n ].Matrix;
      Editor.MousePosInitial:= vec2( -1 );
      Editor.TransCenter:= GetSelectionCenter; //For now only one object selectable and center always at origin

      FEditMode:= AValue;
    end;
end;

procedure TP3DScenePanel.SetSelection( AValue: TP3DActor );
begin
  if ( FSelection = AValue ) then Exit;
  FSelection:= AValue;

  if ( Assigned( Selection )) then
    DataView.Selection:= DataView.Children.FindByData( Pointer( Selection ), True )
  else
    DataView.Selection:= nil;

  if ( Assigned( Selection )) then
    Editor.TransCenter:= GetSelectionCenter; //For now only one object selectable and center always at origin
end;

procedure TP3DScenePanel.RenderCallback( world: TMat4; AActor: TP3DActor );
begin
  if ( AActor = Selection ) then
    Editor.RenderSelection( world );
end;

procedure TP3DScenePanel.DrawArmature(ArmatureObject: TP3DActor);

var
  matrix_joint: TMat4;

  procedure RenderBone( Joint: TP3DJoint );
  var
    p, p2: TVec3;
  begin
    p:= Joint.Position;
    //p2:= p + vec3( 0, 0, 10 );
    p2:= p + Joint.Rotation * vec3( 0, 0, 1 );
    Canvas3D.RenderLine( p, p2, vec4( 1, 0, 0, 1 ), vec4( 0, 1, 0, 1 ));
  end;

var
  Joint: TP3DJoint;

begin
  matrix_joint:= mat4( 1,0,0,0,
                       0,0,1,0,
                       0,-1,0,0,
                       0,0,0,1 );
  Canvas3D.Lock;
  if (( Assigned( ArmatureObject )) and ( ArmatureObject.Data is TP3DArmature )) then
    begin
      for Joint in TP3DArmature( ArmatureObject.Data ).Joints do
        RenderBone( Joint );
    end;
  Canvas3D.Unlock();
end;


constructor TP3DScenePanel.Create(const AData: TP3DData;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Scene:= TestScene;
  OrbitCamera:= True;
  Color:= Primary50; //vec4( 225/255, 230/255, 234/255, 1 );
  OnDragDrop:= @SceneDragDrop;
  SelectionMaterial:= TP3DMaterialBase.Create();
  SelectionMaterial.Unlit:= True;
  FPickMaterial:= TP3DMaterialBase.Create();
  FPickMaterial.Unlit:= True;
  AfterRender:= @AfterDraw;
  BeforeRender:= @BeforeDraw;
  Scene.OnRenderCallback:= @RenderCallback;
  FRenderList:= TP3DRenderList.Create;
  FEditor:= EditModeDefault;

  Canvas3D:= TP3DCanvas3D.Create( nil );
  UpdateCanvas3D;
end;

destructor TP3DScenePanel.Destroy;
begin
  FCanvas3D.Free;

  SelectionMaterial.Free;
  SceneView.Free;
  FreeAndNil( FInfoText );
  FRenderList.Free;
  inherited Destroy;
end;

procedure TP3DScenePanel.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
end;

procedure TP3DScenePanel.KeyboardAction;
begin
  inherited KeyboardAction;

  RenderList.Clear;
  Scene.MakeRenderList( Nil, Mat4Identity, FRenderList );


  Editor.UpdateAxis;

  if ( P3DInput.Keyboard.DKeysLayout[ P3DK_G ] = ksPressed ) then
    EditMode:= emGrab;
  if ( P3DInput.Keyboard.DKeysLayout[ P3DK_R ] = ksPressed ) then
    EditMode:= emRotate;
  if ( P3DInput.Keyboard.DKeysLayout[ P3DK_S ] = ksPressed ) then
    EditMode:= emScale;
  if ( P3DInput.Keyboard.DKeysLayout[ P3DK_ESCAPE ] = ksPressed ) then
    Editor.CancelTransform;
  if ( P3DInput.Keyboard.DKeysLayout[ P3DK_RETURN ] = ksPressed ) then
    EditMode:= emDefault;

  Editor.CheckInput;
end;

procedure TP3DScenePanel.MouseMove(X, Y: Integer);
begin
  if ( Editor.MousePosInitial = vec2( -1 )) then
    Editor.MousePosInitial:= vec2( x, y );
  Editor.MouseDelta:= ( vec2( X, Y ) - Editor.MousePosInitial ) * 0.05;
  Editor.MousePos:= vec2( X, Y );
  if ( EditMode = emDefault ) then
    inherited MouseMove( X, Y );
{  if (( Assigned( Selection )) and ( EditMode <> emDefault )) then
    begin
      if ( FMousePosStored = vec2( -1 )) then
        begin
          FMousePosStored:= vec2( x, y );
          exit;
        end;
      MouseDelta:= ( vec2( X, Y ) - FMousePosStored ) * 0.05;
      Selection.Matrix:= FEditTransMat;
      case AxisMode of
        amScreen:
          begin
            AxisX:=  FCam.InView.Row[ 0 ].xyz;
            AxisY:= -FCam.InView.Row[ 1 ].xyz;
            AxisZ:= -FCam.InView.Row[ 2 ].xyz;
          end;
        amGlobal:
          begin
            AxisX:=  vec3_Axis_PX;
            AxisY:=  vec3_Axis_PY;
            AxisZ:=  vec3_Axis_PZ;
          end;
        amLocal:
          begin
            AxisX:= FEditTransMat.Row[ 0 ].xyz.Normalize;
            AxisY:= FEditTransMat.Row[ 1 ].xyz.Normalize;
            AxisZ:= FEditTransMat.Row[ 2 ].xyz.Normalize;
          end;
      end;
      case EditMode of
        emGrab:
          begin
            if ( AxisLock in [ alX, alXZ, alXY, alNone ]) then
              Selection.MoveAlongGlobalAxis( AxisX, MouseDelta.x );
            if ( AxisLock in [ alY, alXY, alYZ, alNone ]) then
              Selection.MoveAlongGlobalAxis( AxisY, MouseDelta.y );
            if ( AxisLock in [ alZ, alXZ, alYZ ]) then
              Selection.MoveAlongGlobalAxis( AxisZ, MouseDelta.y );
          end;
        emRotate:
          begin
            {c:= ( FCam.Proj * FCam.View * vec4( Selection.Position, 1 )).xy;
            c:= c * vec2( ClientRect.Width, ClientRect.Height );
            //Angle:= arctan(( Y - Canvas.Height / 2 ) / ( X - Canvas.Width / 2 )) / deg2rad;
            if ( AxisLock in [ alX, alYZ ]) then
              Selection.RotateAroundGlobalAxis( AxisX, Angle );
            if ( AxisLock in [ alY, alXZ, alNone ]) then
              Selection.RotateAroundGlobalAxis( AxisY, Angle );
            if ( AxisLock in [ alZ, alXY ]) then
              Selection.RotateAroundGlobalAxis( AxisZ, Angle );}
          end;
        emScale:
          begin
            Scale:= vec3( 1 );
            if ( AxisLock in [ alX, alXZ, alXY, alNone ]) then
              Scale.x:= MouseDelta.x;
            if ( AxisLock in [ alY, alXY, alYZ, alNone ]) then
              Scale.y:= MouseDelta.y;
            if ( AxisLock in [ alZ, alXZ, alYZ ]) then
              Scale.z:= MouseDelta.y;
            if ( Scale <> vec3( 1 )) then
              Selection.Scale:= Scale;
          end;
      end;
    end;}
end;

function TP3DScenePanel.Pick(x, y: Integer): TP3DActor;
var
  LRenderList: TP3DRenderList = nil;
  procedure RenderLight( world: TMat4 );
  var
    Obj: TP3DRenderObject;
    decl: TP3DShaderDecl;
  begin
    for Obj in LRenderList do
      if ( Obj.Actor.Data is TP3DMesh ) then
        begin
          if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
            decl.AsMat4:= world * Obj.Matrix;
          TP3DMesh( Obj.Actor.Data ).RenderSimple;
        end;
  end;

var
  Obj: TP3DRenderObject;
  decl: TP3DShaderDecl;
begin
  PushVPClientRect;
  P3DSelectStart( P3DRect( X + Canvas.Left, Y + Canvas.Top, 1, 1 ));
  FCam.PassToShader( Mat4Identity );
  FCam.UpdateMatrices( FCamObject );
  glEnable( GL_DEPTH_TEST );
  EditorSceneLamp.MakeRenderList( Nil, Mat4Identity, LRenderList );
  for Obj in FRenderList do
    begin
      if ( Obj.Actor.Data is TP3DLight ) then
        begin
          P3DSelectPush( Obj.Actor );
          RenderLight( mat4translate( vec4( Obj.Actor.Position, 1 )));
        end;
      if ( Obj.Actor.Data is TP3DMesh ) then
        begin
          P3DSelectPush( Obj.Actor );
          if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
            decl.AsMat4:= Obj.Matrix;
          TP3DMesh( Obj.Actor.Data ).RenderSimple;
        end;
    end;
  LRenderList.Free;
  P3DSelectEnd();
  P3DViewports.Pop;
  glDisable( GL_DEPTH_TEST );
  //Pick first object from selection (should be only one with one pixel)
  if ( P3DSelectionSize > 0 ) then
    Result:= TP3DActor( P3DSelection[ 0 ])
  else
    Result:= nil;
end;

function TP3DScenePanel.GetSelectionCenter: TVec3;
var
  n: Integer;
begin
  n:= RenderList.Find( Selection );
  if ( n >= 0 ) then
    Result:= RenderList[ n ].Matrix.Row[ 3 ].xyz
  else
    Result:= vec3( 0 );
end;

procedure TP3DScenePanel.Realign;
begin
  inherited Realign;
  UpdateCanvas3D;
end;

procedure TP3DScenePanel.UpdateCanvas3D;
begin
  if ( Assigned( Canvas3D )) then
    begin
      Canvas3D.Left:= Canvas.Left + round( BoundsTL.X );
      Canvas3D.Top:= Canvas.Top + round( BoundsTL.Y );
      Canvas3D.Width:= Canvas.Width - round( BoundsTL.X ) - round( BoundsBR.X );
      Canvas3D.Height:= Canvas.Height - round( BoundsTL.Y ) - round( BoundsBR.Y );
    end;
end;

procedure TP3DScenePanel.BeforeDraw(Sender: TObject);
begin
  Editor.BeforeRender;
end;

procedure TP3DScenePanel.AfterDraw(Sender: TObject);
var
  Obj: TP3DRenderObject;
begin
  for Obj in FRenderList do
    if ( Obj.Actor.Data is TP3DLight ) then
      begin
        if ( Assigned( EditorSceneLampMaterial )) then
          if ( Selection = Obj.Actor ) then
            EditorSceneLampMaterial.Diff:= Accent500.XYZ
          else
            EditorSceneLampMaterial.Diff:= TP3DLight( Obj.Actor.Data ).Color;

        if ( TP3DLight( Obj.Actor.Data ).LightType in [ p3dltSun, p3dltSpot ]) then
          EditorSceneArrow.RenderFromCamera( FCamObject, mat4translate( vec4( Obj.Matrix.Row[ 3 ].XYZ, 1 )) * mat4( mat3( Obj.Actor.Quaternion )));
        EditorSceneLamp.RenderFromCamera( FCamObject, mat4translate( vec4( Obj.Matrix.Row[ 3 ].XYZ, 1 )));
      {Canvas.RenderText( TP3DSymbolActor,
                         vec2( ClientRect.Width, ClientRect.Height ) / 2
                         * ( FCam.Proj * FCam.View * vec4( Actor.Position, 1 )).xy
                         + vec2( ClientRect.Width, ClientRect.Height ) / 2,
                         SymbolActor );}
      end
    else if ( Obj.Actor.Data is TP3DArmature ) then
      begin
        FCanvas3D.Proj:= Cam.Proj;
        FCanvas3D.View:= Cam.View;
        DrawArmature( Obj.Actor );
      {Canvas.RenderText( TP3DSymbolActor,
                         vec2( ClientRect.Width, ClientRect.Height ) / 2
                         * ( FCam.Proj * FCam.View * vec4( Actor.Position, 1 )).xy
                         + vec2( ClientRect.Width, ClientRect.Height ) / 2,
                         SymbolActor );}
      end;

  {glScissor( P3DInput.Mouse.X - 50, P3DViewports[ 0 ].Height - P3DInput.Mouse.Y - 50, 100, 100 );
  glEnable( GL_SCISSOR_TEST );
  Pick(P3DInput.Mouse.X,P3DInput.Mouse.Y);
  glDisable( GL_SCISSOR_TEST );}
  Editor.AfterRender;

  Canvas.Lock;
  Canvas.RenderText( Editor.StatusText, vec2( 0, Height - Font.Size ), FInfoText );

  {Canvas.Font.Name:= 'Pascal3D-Symbols';
  Canvas.Font.Size:= 32;}

  Canvas.Unlock();
  {FCanvas3D.Proj:= Cam.Proj;
  FCanvas3D.View:= Cam.View;
  FCanvas3D.Lock;

  FCanvas3D.RenderLine( vec3( 0, 0, 0 ), vec3( 10, 10, 10 ), vec4( 1, 0, 0, 1 ));

  FCanvas3D.Unlock();}
end;

{$ENDIF}
