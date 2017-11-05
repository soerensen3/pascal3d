{$IFDEF INTERFACE}

  TP3DScenePanel = class;
  TEditMode = ( emDefault, emGrab, emRotate, emScale, emTileGrid );
  TAxisMode = ( amScreen, amGlobal, amLocal );
  TAxisLock = ( alNone, alX, alY, alZ, alXY, alXZ, alYZ );

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

  {$INCLUDE p3dscene_sceneviewer_editmode}

  { TP3DScenePanel }

  TP3DScenePanel = class( TP3DGraphicControl )
    private
      FObjectList: TP3DRenderList;
      FSelectionRenderList: TP3DRenderList;
      FEditMode: TEditMode;
      FEditor: TP3DSceneEditModeDefault;
      FEditorLight: TP3DLight;
      FEditorLightObj: TP3DObject;
      FSelection: TP3DObject;
      FSelectionMaterial: TP3DMaterialBase;
      FPickMaterial: TP3DMaterialBase;
      FInfoText: TP3DText;
      FCanvas3D: TP3DCanvas3D;
      FShowEditorLight: Boolean;
      FToolbar: TP3DScenePanelToolbar;
      FViewer: TP3DSceneViewer;

      function GetCam: TP3DCamera;
      function GetCamObject: TP3DObject;
      function GetScene: TP3DScene;
      function GetSelection: TP3DObject;
      procedure Push3DMode;

      function GetEditor: TP3DSceneEditModeDefault;
      procedure SceneDragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean );
      procedure SetCam(AValue: TP3DCamera);
      procedure SetCamObject(AValue: TP3DObject);
      procedure SetEditMode(AValue: TEditMode);
      procedure SetScene(AValue: TP3DScene);
      procedure SetSelection(AValue: TP3DObject);
      procedure RenderCallback( world: TMat4; AObject: TP3DObject );
      procedure DrawArmature( ArmatureObject: TP3DObject ); // TEMP
      procedure RenderListOnAdd( AData: TP3DDataBlock; AWorld: TMat4; AScene: TP3DScene; AFlags: TP3DRenderListFlags; var Accept: Boolean );

      property SelectionRenderList: TP3DRenderList read FSelectionRenderList write FSelectionRenderList;
      property ObjectList: TP3DRenderList read FObjectList write FObjectList;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      procedure FastDestroy; override;

      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      procedure KeyboardAction; override;
      procedure MouseMove(X, Y: Integer); override;
      function Pick( x, y: Integer ): TP3DObject;
      function GetSelectionCenter: TVec3;
      procedure UpdateCanvasArea;
      procedure UpdateCanvas3D;

      procedure BeforeDraw( Sender: TObject );
      procedure AfterDraw( Sender: TObject );

    published
      property SelectionMaterial: TP3DMaterialBase read FSelectionMaterial write FSelectionMaterial;
      property EditMode: TEditMode read FEditMode write SetEditMode;
      property Editor: TP3DSceneEditModeDefault read GetEditor;
      property Selection: TP3DObject read GetSelection write SetSelection;
      property Viewer: TP3DSceneViewer read FViewer write FViewer;
      property ToolBar: TP3DScenePanelToolbar read FToolbar write FToolBar;
      property Cam: TP3DCamera read GetCam write SetCam;
      property CamObject: TP3DObject read GetCamObject write SetCamObject;
      property Scene: TP3DScene read GetScene write SetScene;
      property Canvas3D: TP3DCanvas3D read FCanvas3D write FCanvas3D;
      property ShowEditorLight: Boolean read FShowEditorLight write FShowEditorLight;
      property EditorLight: TP3DLight read FEditorLight write FEditorLight;
      property EditorLightObj: TP3DObject read FEditorLightObj write FEditorLightObj;
  end;


var
  EditorSceneArrow: TP3DScene;
  EditorSceneArrows: TP3DScene;
  EditorSceneArrowsRL: TP3DRenderList;
  EditorSceneJoint: TP3DScene;
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
  n:= P3DData.OpenLibrary( 'EditorArrow.p3d' );
  P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneArrow:= P3DData.Libraries[ n ].Scenes[ 0 ];
  n:= P3DData.OpenLibrary( 'EditorArrows.p3d' );
  P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneArrows:= P3DData.Libraries[ n ].Scenes[ 0 ];
  n:= P3DData.OpenLibrary( 'EditorLamp.p3d' );
  P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneLamp:= P3DData.Libraries[ n ].Scenes[ 0 ];
  n:= P3DData.OpenLibrary( 'EditorJoint.p3d' );
  P3DData.Libraries[ n ].IsDesignerPackage:= True;
  EditorSceneJoint:= P3DData.Libraries[ n ].Scenes[ 0 ];
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
      end
  else if ( Source is TP3DTreeViewDirectory ) then
    with ( TP3DTreeViewDirectory( Source )) do
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

procedure TP3DScenePanel.SetCamObject(AValue: TP3DObject);
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

function TP3DScenePanel.GetSelection: TP3DObject;
begin
  if ( SceneMain.ActiveDataBlock is TP3DObject ) then
    Result:= TP3DObject( SceneMain.ActiveDataBlock )
  else
    Result:= nil;
end;

function TP3DScenePanel.GetCam: TP3DCamera;
begin
  Result:= Viewer.Cam;
end;

function TP3DScenePanel.GetCamObject: TP3DObject;
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
            if ( Selection is TP3DObject ) and ( Selection.Data is TP3DTileGrid ) then
              EditModeTileGrid.Grid:= TP3DTileGrid( Selection.Data );
          end;
      end;

      //TODO: Repair
      //n:= FRenderList.Find( Selection );
      //if ( n >= 0 ) then
      Editor.MousePosInitial:= vec2( -1 );
      Editor.TransCenter:= GetSelectionCenter; //For now only one object selectable and center always at origin
      Editor.TransMat:= Selection.Transform.Matrix; //FRenderList[ n ].Matrix;

      FEditMode:= AValue;
    end;
end;

procedure TP3DScenePanel.SetScene(AValue: TP3DScene);
begin
  Viewer.Scene:= AValue;
  if ( Assigned( Viewer.Scene )) then
    Viewer.Scene.OnRenderCallback:= @RenderCallback;
end;

procedure TP3DScenePanel.SetSelection( AValue: TP3DObject );
begin
  if ( FSelection = AValue ) then Exit;
  FSelection:= AValue;

  {if ( Assigned( Selection )) then
    DataView.Viewer.Selection:= DataView.Viewer.Children.FindByData( Pointer( Selection ), True )
  else
    DataView.Viewer.Selection:= nil;}
  //SceneMain.ActiveDataBlock:= AValue;

  if ( Assigned( Editor ) and Assigned( Selection )) then
    Editor.TransCenter:= GetSelectionCenter; //For now only one object selectable and center always at origin
end;

procedure TP3DScenePanel.RenderCallback( world: TMat4; AObject: TP3DObject );
begin
  if ( AObject = Selection ) then
    Editor.RenderSelection( world );
end;

procedure TP3DScenePanel.DrawArmature(ArmatureObject: TP3DObject);

var
  trans: TMat4;

  procedure RenderBone( Joint: TP3DJoint );
  var
    p, p2: TVec3;
  begin
    p:= ( trans * vec4( Joint.Position, 1 )).xyz;
    p2:= ( trans * vec4( Joint.Tail, 1 )).xyz;
    Canvas3D.RenderLine( p, p2, vec4( 1, 0, 0, 1 ), vec4( 0, 1, 0, 1 ));
  end;

var
  Joint: TP3DJoint;

begin
  Canvas3D.Lock;
  glDisable( GL_DEPTH_TEST );
  if (( Assigned( ArmatureObject )) and ( ArmatureObject.Data is TP3DArmature )) then
    begin
      trans:= ArmatureObject.CalculateParentMatrix * ArmatureObject.Transform.Matrix;
      for Joint in TP3DArmature( ArmatureObject.Data ).Joints do
        RenderBone( Joint );
    end;
  glEnable( GL_DEPTH_TEST );
  Canvas3D.Unlock();
end;

procedure TP3DScenePanel.RenderListOnAdd(AData: TP3DDataBlock; AWorld: TMat4; AScene: TP3DScene; AFlags: TP3DRenderListFlags; var Accept: Boolean);
var
  i: Integer;
begin
  Accept:= True;
  if ( AData is TP3DLight ) then
    begin
      if ( Assigned( Selection ) and ( Selection.Data = AData )) then
        begin
          SelectionMaterial.Alpha:= 1;
          SelectionMaterial.PassToShader();
          if ( TP3DLight( AData ).LightType in [ ltSun, ltSpot ]) then
            EditorSceneArrow.AddToRenderList( AScene, Selection.Transform.Matrix, Viewer.RenderList, AFlags - [ rlfMaterials, rlfLighting ]);

          EditorSceneLamp.AddToRenderList( AScene, AWorld, Viewer.RenderList, AFlags - [ rlfMaterials, rlfLighting ]);
        end
      else
        EditorSceneLamp.AddToRenderList( AScene, AWorld, Viewer.RenderList, AFlags - [ rlfMaterials, rlfLighting ]);
    end
  else if ( AData is TP3DJoint ) then
    begin
      if ( Assigned( Selection ) and ( Selection.Data = AData )) then
        begin
          SelectionMaterial.Alpha:= 1;
          SelectionMaterial.PassToShader();
          //EditorSceneJoint
          EditorSceneArrows.AddToRenderList( AScene, AWorld, Viewer.RenderList, AFlags );
          //                                  AWorld * mat4scale( vec4( vec3(( AData as TP3DJoint ).Length ), 1 )) {*
                                            //mat4lookatLH( vec3( 0 ), TP3DJoint( Selection.Data ).Tail - TP3DJoint( Selection.Data ).Position, vec3_Axis_PZ ) *
          //                                  mat4scale( vec4( vec3(( AData as TP3DJoint ).Length ), 1 ))}, Viewer.RenderList, AFlags - [ rlfMaterials ]);
        end
      else
        //EditorSceneArrows.AddToRenderList( AScene, AWorld, Viewer.RenderList, AFlags );
        //EditorSceneJoint.AddToRenderList( AScene, AWorld * mat4scale( vec4( vec3(( AData as TP3DJoint ).Length ), 1 )), Viewer.RenderList, AFlags );
    end{
  else if ( AData is TP3DMesh ) then
    if ( Assigned( Selection ) and ( Selection.Data = AData ) and ( Assigned( TP3DMesh( AData ).Armature ))) then
      begin
        SelectionMaterial.Alpha:= 1;
        SelectionMaterial.PassToShader();
        {with ( TP3DMesh( AData ).Armature ) do
          begin
            CurrentAction.UpdateFrameSample;
            for i:= 0 to CurrentAction.FrameSample.Translations.Count - 1 do
              EditorSceneJoint.AddToRenderList( AScene, AWorld * mat4translate( vec4( CurrentAction.FrameSample.Translations[ i ], 1 )) * mat4( mat3scale( vec3( Joints[ i ].Length )) * mat3( CurrentAction.FrameSample.Rotations[ i ])), Viewer.RenderList, AFlags {- [ rlfMaterials ]});
          end;}
      end;}
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

  ObjectList:= TP3DRenderList.Create;

  FEditor:= EditModeDefault;

  EditorLight:= TP3DLight.Create( AData );
  EditorLight.LightType:= ltSun;
  EditorLightObj:= TP3DObject.Create( AData );
  EditorLightObj.Data:= EditorLight;
  EditorLightObj.Transform.Quaternion:= quat( 0.571, 0.169, 0.272, 0.756 );

  ShowEditorLight:= True;

  Canvas3D:= TP3DCanvas3D.Create( nil );
  UpdateCanvas3D;
end;

procedure TP3DScenePanel.FastDestroy;
begin
  FCanvas3D.Free;

  ObjectList.Free;
  SelectionRenderList.Free;
  Viewer.Free;
  ToolBar.Free;
  SelectionMaterial.Free;
  FreeAndNil( FInfoText );
  //FRenderList.Free;
  EditorLightObj.Free;
  EditorLight.Free;

  FreeAndNil( EditorSceneArrowsRL );
  inherited FastDestroy;
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
          if ( Selection is TP3DObject ) and ( TP3DObject( Selection ).Data is TP3DTileGrid ) then
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
  if ( ClientRect.PtInRect( vec2( X, Y ))) then
    //if ( P3DGUIManager.FocusedControl = nil ) then
    Viewer.Focused:= True;
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

function TP3DScenePanel.Pick(x, y: Integer): TP3DObject;
var
  LRenderList: TP3DRenderList = nil;
  {procedure RenderLight( world: TMat4 );
  var
    Obj: TP3DRenderObject;
    decl: TP3DShaderDecl;
  begin
    for Obj in LRenderList do
      if ( Obj.Object.Data is TP3DMesh ) then
        begin
          if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
            decl.AsMat4:= world * Obj.Matrix;
          TP3DMesh( Obj.Object.Data ).RenderSimple;
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
  SceneView.Viewer.RenderList.Render( CamObject, [ rlfIndexColors ]);
  {EditorSceneLamp.MakeRenderList( Nil, Mat4Identity, LRenderList );
  for Obj in FRenderList do
    begin
      if ( Obj.Object.Data is TP3DLight ) then
        begin
          P3DSelectPush( Obj.Object );
          RenderLight( mat4translate( vec4( Obj.Object.Position, 1 )));
        end;
      if ( Obj.Object.Data is TP3DMesh ) then
        begin
          P3DSelectPush( Obj.Object );
          if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
            decl.AsMat4:= Obj.Matrix;
          TP3DMesh( Obj.Object.Data ).RenderSimple;
        end;
    end;
  LRenderList.Free;}
  P3DSelectEnd();
  P3DViewports.Pop;
  glDisable( GL_DEPTH_TEST );
  //Pick first object from selection (should be only one with one pixel)
  if ( P3DSelectionSize > 0 ) then
    Result:= TP3DObject( P3DSelection[ 0 ])
  else
    Result:= nil;
end;

function TP3DScenePanel.GetSelectionCenter: TVec3;
var
  n: Integer;
begin
  WriteLn( 'Selection = ', Selection.Name );
  WriteLn( Selection.Transform.Matrix.ToString );
  if ( Assigned( Selection )) then
    Result:= ( Selection.CalculateParentMatrix * Selection.Transform.Matrix ).Row[ 3 ].xyz
  else
    Result:= vec3( 0 );
  //TODO: FIX THIS!
end;

procedure TP3DScenePanel.UpdateCanvasArea;
begin
//  inherited UpdateCanvasArea;
  UpdateCanvas3D;
end;

procedure TP3DScenePanel.UpdateCanvas3D;
begin
  if ( Assigned( Canvas3D )) then
    begin
      Canvas3D.Rect:= Viewer.Canvas.Rect.Inset( Viewer.Margin.XY, Viewer.Margin.ZW );
    end;
end;

procedure TP3DScenePanel.BeforeDraw(Sender: TObject);
begin
  Editor.BeforeRender;
  if ( Assigned( Viewer.RenderList ) and ( Viewer.RenderList.Lights.Count = 0 )) then
    Viewer.RenderList.Lights.PushRenderObject( EditorLight, EditorLightObj.Transform.Matrix, nil );
end;

procedure TP3DScenePanel.AfterDraw(Sender: TObject);
var
  Obj: TP3DObject;
  AData: TP3DLibrary;
begin
  {glScissor( P3DInput.Mouse.X - 50, P3DViewports[ 0 ].Height - P3DInput.Mouse.Y - 50, 100, 100 );
  glEnable( GL_SCISSOR_TEST );
  Pick(P3DInput.Mouse.X,P3DInput.Mouse.Y);
  glDisable( GL_SCISSOR_TEST );}
  UpdateCanvas3D;
  Editor.AfterRender;

  Canvas.Lock;
  Canvas.Font.Color:= Accent50;
  Canvas.RenderText( Editor.StatusText, vec2( 20, -20 + Height - Viewer.Font.Size ), FInfoText );

  {Canvas.Font.Name:= 'Pascal3D-Symbols';
  Canvas.Font.Size:= 32;}

  Canvas.Unlock();
  {FCanvas3D.Proj:= Cam.Proj;
  FCanvas3D.View:= Cam.View;
  FCanvas3D.Lock;

  FCanvas3D.RenderLine( vec3( 0, 0, 0 ), vec3( 10, 10, 10 ), vec4( 1, 0, 0, 1 ));

  FCanvas3D.Unlock();}
end;

{$INCLUDE p3dscene_sceneviewer_editmode}


{$ENDIF}
