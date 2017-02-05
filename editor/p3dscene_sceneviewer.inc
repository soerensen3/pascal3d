{$IFDEF INTERFACE}

  TEditMode = ( emDefault, emGrab, emRotate, emScale, emTileGrid );
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

  { TP3DSceneEditModeTileGrid }

  TP3DSceneEditModeTileGrid = class ( TP3DSceneEditModeDefault )
    private
      FBrushGridPos: TIVec2;
      FBrushPos: TVec3;
      FCanPaint: Boolean;
      FGrid: TP3DTileGrid;

      function GetBrushScene: TP3DGridScene;
      procedure SetBrushPos(AValue: TVec3);

    public
      procedure AfterRender; override;
      function StatusText: String; override;
      procedure CheckInput; override;
      procedure Paint;

      property Grid: TP3DTileGrid read FGrid write FGrid;
      property BrushPos: TVec3 read FBrushPos write SetBrushPos;
      property BrushGridPos: TIVec2 read FBrushGridPos;
      property CanPaint: Boolean read FCanPaint;
      property BrushScene: TP3DGridScene read GetBrushScene;
  end;

  { TP3DScenePanelToolbar }

  TP3DScenePanelToolbar = class ( TP3DGraphicControl )
    private
      FNewButton: TP3DGroupButton;
      procedure ButtonMouseClick(Sender:TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);

    public
      procedure ClearButtons;
      procedure UpdateButtons( AViewMode: TP3DDataViewerMode );
      procedure UpdateButtonsFast;
      property NewButton: TP3DGroupButton read FNewButton write FNewButton;
  end;

  { TP3DScenePanel }

  TP3DScenePanel = class( TP3DGraphicControl )
    private
      FActorList: TP3DRenderList;
      FSelectionRenderList: TP3DRenderList;
      FEditMode: TEditMode;
      FEditor: TP3DSceneEditModeDefault;
      FEditorLight: TP3DLight;
      FEditorLightObj: TP3DActor;
      FSelection: TP3DActor;
      FSelectionMaterial: TP3DMaterialBase;
      FPickMaterial: TP3DMaterialBase;
      FInfoText: TP3DText;
      FCanvas3D: TP3DCanvas3D;
      FShowEditorLight: Boolean;
      FToolbar: TP3DScenePanelToolbar;
      FViewer: TP3DSceneViewer;

      function GetCam: TP3DCamera;
      function GetCamObject: TP3DActor;
      function GetScene: TP3DScene;
      procedure Push3DMode;

      function GetEditor: TP3DSceneEditModeDefault;
      procedure SceneDragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean );
      procedure SetCam(AValue: TP3DCamera);
      procedure SetCamObject(AValue: TP3DActor);
      procedure SetEditMode(AValue: TEditMode);
      procedure SetScene(AValue: TP3DScene);
      procedure SetSelection(AValue: TP3DActor);
      procedure RenderCallback( world: TMat4; AActor: TP3DActor );
      procedure DrawArmature( ArmatureObject: TP3DActor ); // TEMP
      procedure RenderListOnAdd( AData: TP3DDataBlock; AWorld: TMat4; AScene: TP3DScene; AFlags: TP3DRenderListFlags; var Accept: Boolean );

      property SelectionRenderList: TP3DRenderList read FSelectionRenderList write FSelectionRenderList;
      property ActorList: TP3DRenderList read FActorList write FActorList;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      procedure KeyboardAction; override;
      procedure MouseMove(X, Y: Integer); override;
      function Pick( x, y: Integer ): TP3DActor;
      function GetSelectionCenter: TVec3;
      procedure UpdateCanvasArea; override;
      procedure UpdateCanvas3D;

      procedure BeforeDraw( Sender: TObject );
      procedure AfterDraw( Sender: TObject );

    published
      property SelectionMaterial: TP3DMaterialBase read FSelectionMaterial write FSelectionMaterial;
      property EditMode: TEditMode read FEditMode write SetEditMode;
      property Editor: TP3DSceneEditModeDefault read GetEditor;
      property Selection: TP3DActor read FSelection write SetSelection;
      property Viewer: TP3DSceneViewer read FViewer write FViewer;
      property ToolBar: TP3DScenePanelToolbar read FToolbar write FToolBar;
      property Cam: TP3DCamera read GetCam write SetCam;
      property CamObject: TP3DActor read GetCamObject write SetCamObject;
      property Scene: TP3DScene read GetScene write SetScene;
      property Canvas3D: TP3DCanvas3D read FCanvas3D write FCanvas3D;
      property ShowEditorLight: Boolean read FShowEditorLight write FShowEditorLight;
      property EditorLight: TP3DLight read FEditorLight write FEditorLight;
      property EditorLightObj: TP3DActor read FEditorLightObj write FEditorLightObj;
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
  EditModeTileGrid: TP3DSceneEditModeTileGrid;

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
  if ( not Assigned( EditModeTileGrid )) then
    EditModeTileGrid:= TP3DSceneEditModeTileGrid.Create( SceneView );
end;

procedure DestroyEditModes;
begin
  FreeAndNil( EditModeDefault );
  FreeAndNil( EditModeGrab );
  FreeAndNil( EditModeScale );
  FreeAndNil( EditModeRotate );
  FreeAndNil( EditModeTileGrid );
end;

procedure CreateEditorScenes;
var
  n: Integer;
begin
  n:= P3DData.OpenLibrary( 'armature_test/media/EditorArrow.p3d' );
  P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneArrow:= P3DData.Libraries[ n ].Scenes[ 0 ];
  n:= P3DData.OpenLibrary( 'armature_test/media/EditorArrows.p3d' );
  P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneArrows:= P3DData.Libraries[ n ].Scenes[ 0 ];
  n:= P3DData.OpenLibrary( 'armature_test/media/EditorLamp.p3d' );
  //P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneLamp:= P3DData.Libraries[ n ].Scenes[ 0 ];
  EditorSceneLampMaterial:= TP3DMaterialBase.Create();//TP3DMaterialBase( P3DData.Libraries[ n ].Materials[ 0 ]); //Error
end;

{ TP3DScenePanelToolbar }

procedure TP3DScenePanelToolbar.ClearButtons;
begin
  Controls.Clear( True );
end;

procedure TP3DScenePanelToolbar.UpdateButtons(AViewMode: TP3DDataViewerMode);
  function CreateButton( Text: String; const Symbol: Boolean = False ): TP3DGroupButton;
  begin
    Result:= TP3DGroupButton.Create( Owner, Self );
    Result.HAlignment:= haCenter;
    Result.VAlignment:= vaCenter;
    Result.Group:= 'DataViewer';
    Result.Font.Size:= 16;
    if ( Symbol ) then
      Result.Font.Name:= P3DSymbolFontName;
    Result.Caption:= Text;
    Result.Align:= alLeft;
    Result.OnMouseClick:= @ButtonMouseClick;
    Result.ResizeToText( 5 );
  end;

  procedure AddButtonsLibraries( ALibraries: TP3DLibraryList );
  var
    i: Integer;
    Btn: TP3DGroupButton;
  begin
    if ( Assigned( ALibraries )) then
      for i:= 0 to ALibraries.Count - 1 do
        if ( not ALibraries[ i ].IsDesignerPackage ) then
          begin
            Btn:= CreateButton( ALibraries[ i ].Name );
            if ( ALibraries[ i ] = SceneMain.ActiveLibrary ) then
              Btn.Down:= True;
            Btn.PointerTag:= ALibraries[ i ];
          end;
  end;


  procedure AddButtonsScenes( AScenes: TP3DSceneList );
  var
    i: Integer;
    Btn: TP3DGroupButton;
  begin
    for i:= 0 to AScenes.Count - 1 do
      begin
        Btn:= CreateButton( AScenes[ i ].Owner.Name + ': ' + AScenes[ i ].Name );
        if ( AScenes[ i ] = SceneMain.ActiveScene ) then
          Btn.Down:= True;
        Btn.PointerTag:= AScenes[ i ];
      end;
  end;

begin
  ClearButtons;
  case ( AViewMode ) of
    dvmData:
      AddButtonsLibraries( P3DData.Libraries );
    dvmLibrary, dvmScene:
      if ( Assigned( SceneMain.ActiveLibrary )) then
        AddButtonsScenes( SceneMain.ActiveLibrary.Scenes );
  end;
  NewButton:= CreateButton( P3DSymbolPlus, True );
  NewButton.PointerTag:= nil;
end;

procedure TP3DScenePanelToolbar.UpdateButtonsFast;
var
  i: Integer;
begin
  for i:= 0 to Controls.Count - 1 do
    case ( SceneMain.ViewMode ) of
      dvmData:
        if ( TPersistent( Controls[ i ].PointerTag ) = SceneMain.ActiveLibrary ) then
          begin
            TP3DGroupButton( Controls[ i ]).Down:= True;
            break;
          end;
      dvmLibrary, dvmScene:
        if ( TPersistent( Controls[ i ].PointerTag ) = SceneMain.ActiveScene ) then
          begin
            TP3DGroupButton( Controls[ i ]).Down:= True;
            break;
          end;
    end;
end;

procedure TP3DScenePanelToolbar.ButtonMouseClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if ( mb1 ) then
    begin
      if ( Sender = NewButton ) then
        begin
          case SceneMain.ViewMode of
            dvmData:
              begin
                SceneMain.ActiveLibrary:= SceneMain.NewLibrary();
                SceneMain.ActiveScene:= SceneMain.NewScene();
              end;
            dvmLibrary, dvmScene:
              SceneMain.ActiveScene:= SceneMain.NewScene();
          end;
        end
      else if ( Sender is TP3DGroupButton ) then
        with ( Sender as TP3DGroupButton ) do
          if ( TPersistent( PointerTag ) is TP3DScene ) then
            SceneView.Scene:= TP3DScene( PointerTag )
          else if ( TPersistent( PointerTag ) is TP3DLibrary ) then
            SceneMain.ActiveLibrary:= TP3DLibrary( PointerTag );
    end;
end;

{ TP3DSceneEditModeTileGrid }

procedure TP3DSceneEditModeTileGrid.SetBrushPos(AValue: TVec3);
var
  p: TVec2;
begin
  if FBrushPos=AValue then Exit;
  FBrushPos:=AValue;
  p:= ( BrushPos.xy - ScenePanel.Selection.Position.xy ) / Grid.GridWorldUnit;
  FBrushGridPos:= ivec2( round( p.x ), round( p.y ));
  FCanPaint:= ( InRange( BrushGridPos.X, 0, Grid.Width - 1 ) and InRange( BrushGridPos.Y, 0, Grid.Height - 1 ));
end;

function TP3DSceneEditModeTileGrid.GetBrushScene: TP3DGridScene;
var
  n, m: Integer;
begin
  Result:= nil;
  if ( FileExistsUTF8( AssetView.AssetView.Directory ) and
     ( Assigned( AssetView.AssetView.Selection )) and
     ( lowercase( ExtractFileExt( AssetView.AssetView.Directory )) = '.p3d' )) then
    begin
      n:= P3DData.OpenLibrary( AssetView.AssetView.Directory );
      if ( n > -1 ) then
        begin
          m:= P3DData.Libraries[ n ].Scenes.FindByName( AssetView.AssetView.Selection.Caption );
          if ( n > -1 ) then
            begin
              m:= Grid.AddScene( P3DData.Libraries[ n ].Scenes[ m ]);
              Result:= Grid.Scenes[ m ];
            end;
        end;
    end;
end;

procedure TP3DSceneEditModeTileGrid.AfterRender;
var
  h_sz, brpos: TVec3;
  col: TVec4;
begin
  if ( not Assigned( ScenePanel.Selection )) then
    exit;

  glDisable( GL_DEPTH_TEST );
  ScenePanel.Cam.PassToShader;
  //ScenePanel.Canvas3D.Material;
  ScenePanel.Canvas3D.View:= ScenePanel.Cam.View;
  ScenePanel.Canvas3D.Proj:= ScenePanel.Cam.Proj;
  ScenePanel.Canvas3D.Lock;

    h_sz:= vec3( 0.5, 0.5, 0 ) * Grid.GridWorldUnit;
    brpos:= vec3( BrushGridPos.xy, 0 ) * Grid.GridWorldUnit;
    if ( InRange( BrushGridPos.X, 0, Grid.Width - 1 ) and InRange( BrushGridPos.Y, 0, Grid.Height - 1 )) then
      col:= vec4( Accent500.xyz, 0.7 )
    else
      col:= vec4( Red500.xyz, 0.7 );
    ScenePanel.Canvas3D.RenderLineRect( brpos - h_sz, brpos + h_sz, vec4( col.xyz, 1 ));
    ScenePanel.Canvas3D.RenderRect( brpos - h_sz, brpos + h_sz, col );
    ScenePanel.Canvas3D.RenderLineRect( ScenePanel.Selection.Position - h_sz,
                                        ScenePanel.Selection.Position - h_sz +
                                          vec3( Grid.Width, Grid.Height, 0 ) * Grid.GridWorldUnit,
                                          Accent300 );
    ScenePanel.Canvas3D.RenderRect( ScenePanel.Selection.Position - h_sz,
                                    ScenePanel.Selection.Position - h_sz +
                                          vec3( Grid.Width, Grid.Height, 0 ) * Grid.GridWorldUnit,
                                          vec4( Accent300.xyz, 0.3 ));
  ScenePanel.Canvas3D.Unlock();
  glEnable( GL_DEPTH_TEST );
end;

function TP3DSceneEditModeTileGrid.StatusText: String;
begin
  Result:= 'Grid Paint Mode'; //inherited StatusText;
end;

procedure TP3DSceneEditModeTileGrid.CheckInput;
var
  ray_dir, ray_pos: TVec3;
  t: Float;
begin
  inherited CheckInput;

  if ( not Assigned( ScenePanel.Selection )) then
    exit;

  ray_pos:= unproject( vec2( P3DInput.Mouse.X - ScenePanel.Canvas3D.Left, P3DInput.Mouse.Y - ScenePanel.Canvas3D.Top ),
                       ScenePanel.Cam.View, ScenePanel.Cam.Proj, ScenePanel.Canvas3D.Width, ScenePanel.Canvas3D.Height, ray_dir );
  if ( ray_dir.Z <> 0 ) then
    t:= - ( ray_pos.z + ScenePanel.Selection.Position.z ) / ray_dir.z
  else
    t:= 0;
  BrushPos:= ray_pos + ray_dir * t;

  if (( gcisMouseBtn1Down in ScenePanel.Viewer.InputState ) and ( InRange( BrushGridPos.X, 0, Grid.Width - 1 )) and ( InRange( BrushGridPos.Y, 0, Grid.Height - 1 ))) then
    Paint;

  if ( P3DInput.Keyboard.DKeys[ P3DK_F2 ] = ksPressed ) then
    Grid.UpdateArrays;
end;

procedure TP3DSceneEditModeTileGrid.Paint;
var
  Scene: TP3DGridScene;
begin
  if ( CanPaint ) then
    begin
      if ( P3DInput.Keyboard.Keys[ P3DK_LSHIFT ]) then
        Grid.SceneAt[ BrushGridPos.X, BrushGridPos.Y ]:= nil
      else
        begin
          Scene:= BrushScene;
          if ( Assigned( Scene )) then
            Grid.SceneAt[ BrushGridPos.X, BrushGridPos.Y ]:= Scene;
        end;
      FCanPaint:= False;
    end;
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
  exit; //TODO: Repair

  with ( ScenePanel ) do
    begin
      Canvas.Lock;

      //n:= RenderList.Find( Selection );
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
  SceneView.ActorList.Clear;
  SceneView.Viewer.OverrideMaterial.PassToShader();
  SceneView.Scene.MakeRenderList( nil, Mat4Identity, SceneView.FActorList, [ rlfActors, rlfScenes, rlfTileGrids ]);

  glDisable( GL_DEPTH_TEST );
  ScenePanel.Push3DMode;
  RenderSelectionAxis;
  P3DViewports.Pop;
  RenderSelection( Mat4Identity );
  glEnable( GL_DEPTH_TEST );
end;

procedure TP3DSceneEditModeDefault.CheckInput;
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
const
  RenderFlags = [ rlfRenderEdges, rlfMeshes, rlfScenes, rlfTileGrids ];
var
  n: Integer;
  decl: TP3DShaderDecl;
  loc, scale: TVec3;
  rot: TMat3;
begin
  glDisable( GL_DEPTH_TEST );
  if ( not Assigned( ScenePanel.Selection )) then
    exit;
  if ( Assigned( ScenePanel.Selection.Data )) then
    with ( ScenePanel ) do
      begin
        glDepthMask( False );
        SelectionMaterial.Diff:= Accent200.XYZ;//vec3( 1, 1, 0 );
        SelectionMaterial.Alpha:= 0.1;
        glLineWidth( 2 );
  //      glDisable( GL_DEPTH_TEST );
        glDepthMask( False );
  //      glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
        SelectionMaterial.PassToShader();

        FreeAndNil( FSelectionRenderList );
        FSelectionRenderList:= TP3DRenderList.Create;
        FSelectionRenderList.OnAdd:= @ScenePanel.RenderListOnAdd;
        n:= ActorList.Find( Selection, ScenePanel.Viewer.OverrideMaterial );

        if ( n >= 0 ) then
          begin
            FSelectionRenderList.Add( ActorList[ n ].Data, {ActorList[ n ].Matrices[ 0 ]}Mat4Identity, Selection.Scene, RenderFlags );
        //FSelectionRenderList.Add( Selection.Data, Selection.MatrixFinal, Selection.Scene, RenderFlags );
            mat4decompose( ActorList[ n ].Matrices[ 0 ], loc, rot, scale );
            Viewer.Caption:= 'Location: (' + loc.ToString( 1 ) + ') Rotation: (' + quat( rot ).ToString( 1 ) + ') Scale: (' + scale.ToString( 1 ) + ')';
          end;
        //Selection.Scene.MakeRenderList( nil, Selection.MatrixFinal, FSelectionRenderList, RenderFlags );
        {n:= .SelectionRenderList.Find( .Selection.Data, .SelectionMaterial );
        if ( n > -1 ) then
          while ( n < .SelectionRenderList.Count ) and ( .SelectionRenderList[ n ].Data = .Selection.Data ) do
            begin
              .SelectionRenderList.SetupMaterial( .SelectionMaterial, .CamObject, False, RenderFlags );
              .SelectionRenderList.RenderMesh( n, .CamObject, RenderFlags );
              Inc( n );
            end;}
        SelectionRenderList.Render( SceneView.CamObject, RenderFlags );
        {if ( P3DMaterialActive.Shader.Attributes.FindByName( 'instanceID', decl )) then
          begin
            glVertexAttrib1f( decl.Addr, 0.0 );
            glVertexAttribDivisorARB( decl.Addr, 0 );
          end;}


  //      glEnable( GL_DEPTH_TEST );
        glDepthMask( True );
        glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
        glLineWidth( 1 );
      end;

  {ScenePanel.Cam.PassToShader;
  if ( Assigned( ScenePanel.Selection )) then
    begin
      if ( ScenePanel.Selection.Data is TP3DMesh ) then
        begin
          glLineWidth( 2 );
          TP3DMesh( ScenePanel.Selection.Data ).RenderEdges( world, SceneMain.ActiveScene, ScenePanel.Selection );
          glLineWidth( 1 );
        end;
      ScenePanel.Canvas.Material.PassToShader();
    end;}
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
        AxisX:= ScenePanel.Cam.InvView.Row[ 0 ].xyz;
        AxisY:= ScenePanel.Cam.InvView.Row[ 2 ].xyz;
        AxisZ:= ScenePanel.Cam.InvView.Row[ 1 ].xyz;
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
var
  Sc: TP3DScene;
begin
  if ( Source is TP3DListViewFile ) then
    with ( TP3DListViewFile( Source )) do
      begin
        if ( Assigned( DragItem )) then
          if ( P3DMatchFileName( DragItem.Caption, '*.p3d' )) then
            begin
              try
                Sc:= SceneMain.ActiveScene;
                if ( Assigned( Sc )) then
                  Sc.AppendFile( AppendPathDelim( Directory ) + DragItem.Caption );
              except
                on E: Exception do
                  SDL_ShowSimpleMessageBox( SDL_MESSAGEBOX_ERROR, PChar( 'Error loading file ' + DragItem.Caption ), PChar( E.Message ), Nil );
              end;
            end;
      end;
  Accept:= False;
end;

procedure TP3DScenePanel.SetCam(AValue: TP3DCamera);
begin
  Viewer.Cam:= AValue;
end;

procedure TP3DScenePanel.SetCamObject(AValue: TP3DActor);
begin
  Viewer.CamObject:= AValue;
end;

procedure TP3DScenePanel.Push3DMode;
begin
  Viewer.PushVPClientRect;
end;

function TP3DScenePanel.GetScene: TP3DScene;
begin
  Result:= Viewer.Scene;
end;

function TP3DScenePanel.GetCam: TP3DCamera;
begin
  Result:= Viewer.Cam;
end;

function TP3DScenePanel.GetCamObject: TP3DActor;
begin
  Result:= Viewer.CamObject;
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
        emTileGrid:
          begin
            FEditor:= EditModeTileGrid;
            if ( Selection is TP3DActor ) and ( Selection.Data is TP3DTileGrid ) then
              EditModeTileGrid.Grid:= TP3DTileGrid( Selection.Data );
          end;
      end;

      //TODO: Repair
      //n:= FRenderList.Find( Selection );
      //if ( n >= 0 ) then
      Editor.TransMat:= Selection.MatrixFinal; //FRenderList[ n ].Matrix;
      Editor.MousePosInitial:= vec2( -1 );
      Editor.TransCenter:= GetSelectionCenter; //For now only one object selectable and center always at origin

      FEditMode:= AValue;
    end;
end;

procedure TP3DScenePanel.SetScene(AValue: TP3DScene);
begin
  Viewer.Scene:= AValue;
  if ( Assigned( Viewer.Scene )) then
    Viewer.Scene.OnRenderCallback:= @RenderCallback;
end;

procedure TP3DScenePanel.SetSelection( AValue: TP3DActor );
begin
  if ( FSelection = AValue ) then Exit;
  FSelection:= AValue;

  if ( Assigned( Selection )) then
    DataView.Viewer.Selection:= DataView.Viewer.Children.FindByData( Pointer( Selection ), True )
  else
    DataView.Viewer.Selection:= nil;

  if ( Assigned( Editor ) and Assigned( Selection )) then
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
  glDisable( GL_DEPTH_TEST );
  if (( Assigned( ArmatureObject )) and ( ArmatureObject.Data is TP3DArmature )) then
    begin
      for Joint in TP3DArmature( ArmatureObject.Data ).Joints do
        RenderBone( Joint );
    end;
  glEnable( GL_DEPTH_TEST );
  Canvas3D.Unlock();
end;

procedure TP3DScenePanel.RenderListOnAdd(AData: TP3DDataBlock; AWorld: TMat4; AScene: TP3DScene; AFlags: TP3DRenderListFlags; var Accept: Boolean);
begin
  Accept:= True;
  if ( AData is TP3DLight ) then
    if ( Assigned( Selection ) and ( Selection.Data = AData )) then
      begin
        SelectionMaterial.Alpha:= 1;
        SelectionMaterial.PassToShader();
        EditorSceneLamp.AddToRenderList( AScene, AWorld, Viewer.RenderList, AFlags - [ rlfMaterials ]);
      end
    else
      EditorSceneLamp.AddToRenderList( AScene, AWorld, Viewer.RenderList, AFlags );
end;


constructor TP3DScenePanel.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );

  ToolBar:= TP3DScenePanelToolbar.Create( AData, Self );
  ToolBar.Align:= alTop;
  ToolBar.Height:= 32;

  Viewer:= TP3DSceneViewer.Create( AData, Self );
  Viewer.Align:= alClient;
  Viewer.OrbitCamera:= True;
  Viewer.Color:= Accent800; //vec4( 225/255, 230/255, 234/255, 1 );
  Viewer.OnDragDrop:= @SceneDragDrop;
  Viewer.Font.Color:= White;
  Viewer.Caption:= 'Scene Viewer';

  SelectionMaterial:= TP3DMaterialBase.Create();
  SelectionMaterial.Name:= 'SelectionMaterial';
  SelectionMaterial.Diff:= Accent100.xyz;
  SelectionMaterial.Unlit:= True;
  SelectionMaterial.Instancing:= True;

  Viewer.OverrideMaterial:= SelectionMaterial;

  FPickMaterial:= TP3DMaterialBase.Create();
  FPickMaterial.Unlit:= True;
  FPickMaterial.Instancing:= True;

  Viewer.AfterRender:= @AfterDraw;
  Viewer.BeforeRender:= @BeforeDraw;
  Viewer.RenderList.OnAdd:= @RenderListOnAdd;

  ActorList:= TP3DRenderList.Create;

  FEditor:= EditModeDefault;

  EditorLight:= TP3DLight.Create( AData );
  EditorLight.LightType:= ltSun;
  EditorLightObj:= TP3DActor.Create( AData );
  EditorLightObj.Data:= EditorLight;
  EditorLightObj.Quaternion:= quat( 0.571, 0.169, 0.272, 0.756 );

  ShowEditorLight:= True;

  Canvas3D:= TP3DCanvas3D.Create( nil );
  UpdateCanvas3D;
end;

destructor TP3DScenePanel.Destroy;
begin
  FCanvas3D.Free;

  ActorList.Free;
  SelectionRenderList.Free;
  Viewer.Free;
  ToolBar.Free;
  SelectionMaterial.Free;
  FreeAndNil( FInfoText );
  //FRenderList.Free;
  EditorLightObj.Free;
  EditorLight.Free;
  inherited Destroy;
end;

procedure TP3DScenePanel.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
end;

procedure TP3DScenePanel.KeyboardAction;
begin
  inherited KeyboardAction;

  if (( Viewer.Focused ) or ( not Assigned( P3DGUIManager.FocusedControl ))) then
    begin
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
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_TAB ] = ksPressed ) then
        begin
          if ( Selection is TP3DActor ) and ( TP3DActor( Selection ).Data is TP3DTileGrid ) then
            EditMode:= emTileGrid;

        end;

      Editor.CheckInput;
    end;
end;

procedure TP3DScenePanel.MouseMove(X, Y: Integer);
begin
  if ( Editor.MousePosInitial = vec2( -1 )) then
    Editor.MousePosInitial:= vec2( x, y );
  Editor.MouseDelta:= ( vec2( X, Y ) - Editor.MousePosInitial ) * 0.05;
  Editor.MousePos:= vec2( X, Y );
  if ( EditMode in [ emDefault, emTileGrid ]) then
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
  {procedure RenderLight( world: TMat4 );
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
  end;}

var
  Obj: TP3DRenderObject;
  decl: TP3DShaderDecl;
begin
  //TODO: FIX THIS
  Viewer.PushVPClientRect;
  P3DSelectStart( P3DRect( X + Canvas.Left, Y + Canvas.Top, 1, 1 ));
  Cam.PassToShader;
  Cam.UpdateMatrices( CamObject );
  glEnable( GL_DEPTH_TEST );
  {EditorSceneLamp.MakeRenderList( Nil, Mat4Identity, LRenderList );
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
  LRenderList.Free;}
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
  Result:= vec3( 0 );
  {n:= RenderList.Find( Selection );
  if ( n >= 0 ) then
    Result:= RenderList[ n ].Matrix.Row[ 3 ].xyz
  else
    Result:= vec3( 0 );}
  //TODO: FIX THIS!
end;

procedure TP3DScenePanel.UpdateCanvasArea;
begin
  inherited UpdateCanvasArea;
  UpdateCanvas3D;
end;

procedure TP3DScenePanel.UpdateCanvas3D;
begin
  if ( Assigned( Canvas3D )) then
    begin
      Canvas3D.Left:= Canvas.Left + round( MarginTL.X );
      Canvas3D.Top:= Canvas.Top + round( MarginTL.Y );
      Canvas3D.Width:= Canvas.Width - round( MarginTL.X ) - round( MarginBR.X );
      Canvas3D.Height:= Canvas.Height - round( MarginTL.Y ) - round( MarginBR.Y );
    end;
end;

procedure TP3DScenePanel.BeforeDraw(Sender: TObject);
begin
  Editor.BeforeRender;
  if ( Assigned( Viewer.RenderList ) and ( Viewer.RenderList.Lights.Count = 0 )) then
    Viewer.RenderList.Lights.Add( EditorLight );
end;

procedure TP3DScenePanel.AfterDraw(Sender: TObject);
var
  Obj: TP3DActor;
  AData: TP3DLibrary;
begin
  //TODO: FIX THIS!

  if ( Assigned( DataView.Viewer.Selection )) and ( TInterfacedPersistent( DataView.Viewer.Selection.Data ) is TP3DDataBlock ) then
    AData:= TP3DDataBlock( DataView.Viewer.Selection.Data ).Owner
  else
    AData:= P3DData;

  if ( not Assigned( AData )) then
    AData:= P3DData;
  for Obj in AData.Objects do
{    if ( Obj.Data is TP3DLight ) then
      begin
        if ( Assigned( EditorSceneLampMaterial )) then
          if ( Selection = Obj ) then
            EditorSceneLampMaterial.Diff:= Accent500.XYZ
          else
            EditorSceneLampMaterial.Diff:= TP3DLight( Obj.Data ).Color;

        if ( TP3DLight( Obj.Data ).LightType in [ p3dltSun, p3dltSpot ]) then
          EditorSceneArrow.RenderFromCamera( FCamObject, mat4translate( vec4( Obj.Matrix.Row[ 3 ].XYZ, 1 )) * mat4( mat3( Obj.Quaternion )));
        EditorSceneLamp.RenderFromCamera( FCamObject, mat4translate( vec4( Obj.Matrix.Row[ 3 ].XYZ, 1 )));
      {Canvas.RenderText( TP3DSymbolActor,
                         vec2( ClientRect.Width, ClientRect.Height ) / 2
                         * ( FCam.Proj * FCam.View * vec4( Actor.Position, 1 )).xy
                         + vec2( ClientRect.Width, ClientRect.Height ) / 2,
                         SymbolActor );}
      end
    else }if ( Obj.Data is TP3DArmature ) then
      begin
        FCanvas3D.Proj:= Cam.Proj;
        FCanvas3D.View:= Cam.View;
        Canvas3D.Pen.Width:= 5;
        DrawArmature( Obj );
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
  Canvas.RenderText( Editor.StatusText, vec2( 0, Height - Viewer.Font.Size ), FInfoText );

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
