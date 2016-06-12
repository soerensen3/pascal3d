{$IFDEF INTERFACE}

  TEditMode = ( emDefault, emGrab, emRotate, emScale );
  TAxisMode = ( amScreen, amGlobal, amLocal );
  TAxisLock = ( alNone, alX, alY, alZ, alXY, alXZ, alYZ );

  { TP3DScenePanel }

  TP3DScenePanel = class( TP3DSceneViewer )
    private
      FAxisLock: TAxisLock;
      FAxisMode: TAxisMode;
      FEditMode: TEditMode;
      FSelection: TP3DActor;
      FSelectionMaterial: TP3DMaterialBase;
      FPickMaterial: TP3DMaterialBase;
      FInfoText: TP3DText;
      FEditTransMat: TMat4;
      FMousePosStored: TVec2;
      FRenderList: TP3DRenderList;

      procedure SceneDragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean );
      procedure SetEditMode(AValue: TEditMode);
      procedure SetSelection(AValue: TP3DActor);
      procedure RenderCallback( world: TMat4; AActor: TP3DActor );

    public
      constructor Create(const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      procedure KeyboardAction; override;
      procedure MouseMove(X, Y: Integer); override;
      function Pick( x, y: Integer ): TP3DActor;
      procedure ToggleAxis( Axis: TAxisLock );
      procedure CancelTransform;

      procedure AfterDraw( Sender: TObject );

    published
      property SelectionMaterial: TP3DMaterialBase read FSelectionMaterial write FSelectionMaterial;
      property EditMode: TEditMode read FEditMode write SetEditMode;
      property AxisMode: TAxisMode read FAxisMode write FAxisMode;
      property AxisLock: TAxisLock read FAxisLock write FAxisLock;
      property Selection: TP3DActor read FSelection write SetSelection;
  end;


var
  EditorSceneArrow: TP3DScene;
  EditorSceneArrows: TP3DScene;
  EditorSceneLamp: TP3DScene;
  EditorSceneLampMaterial: TP3DMaterialBase;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

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
            TestScene.AppendFile( AppendPathDelim( Directory ) + Selection.Caption );
            DataView.Update;
            DataView.Children.ExpandAll();
          end;
    end;
  Accept:= False;
end;

procedure TP3DScenePanel.SetEditMode(AValue: TEditMode);
begin
  if FEditMode=AValue then Exit;

  if ( AValue = emDefault ) then
    begin
      FEditMode:= emDefault;
    end
  else if ( Assigned( Selection )) then
    begin
      if ( EditMode = emDefault ) then
        begin
          FEditTransMat:= Selection.Matrix;
          FMousePosStored:= vec2( -1 );
        end
      else
        CancelTransform;
      FEditMode:= AValue;
    end;
end;

procedure TP3DScenePanel.SetSelection(AValue: TP3DActor);
begin
  if ( FSelection = AValue ) then Exit;
  FSelection:= AValue;

  if ( Assigned( Selection )) then
    DataView.Selection:= DataView.Children.FindByData( Pointer( Selection ), True )
  else
    DataView.Selection:= nil;
end;

procedure TP3DScenePanel.RenderCallback(world: TMat4; AActor: TP3DActor);
begin
  //FRenderList.Add( AActor );
end;

constructor TP3DScenePanel.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Scene:= TestScene;
  OrbitCamera:= True;
  Color:= vec4( 225/255, 230/255, 234/255, 1 );
  OnDragDrop:= @SceneDragDrop;
  SelectionMaterial:= TP3DMaterialBase.Create();
  SelectionMaterial.Unlit:= True;
  FPickMaterial:= TP3DMaterialBase.Create();
  FPickMaterial.Unlit:= True;
  AfterRender:= @AfterDraw;
  Scene.OnRenderCallback:= @RenderCallback;
  FRenderList:= TP3DRenderList.Create;
end;

destructor TP3DScenePanel.Destroy;
begin
  SelectionMaterial.Free;
  SceneView.Free;
  FreeAndNil( FInfoText );
  FRenderList.Free;
  inherited Destroy;
end;

procedure TP3DScenePanel.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
  if ( mb1 and ( EditMode <> emDefault )) then
    EditMode:= emDefault;

  if ( mb3 ) then
    begin
      if ( EditMode <> emDefault ) then
        CancelTransform
      else
        Selection:= Pick( x, y );
    end;
end;

procedure TP3DScenePanel.KeyboardAction;
begin
  inherited KeyboardAction;
  if ( EditMode in [ emGrab, emRotate, emScale ]) then
    begin
      if ( P3DInput.Keyboard.DKeys[ P3DK_X ] and P3DInput.Keyboard.Keys[ P3DK_X ]) then
        ToggleAxis( alX );
      if ( P3DInput.Keyboard.DKeys[ P3DK_Y ] and P3DInput.Keyboard.Keys[ P3DK_Y ]) then
        ToggleAxis( alY );
      if ( P3DInput.Keyboard.DKeys[ P3DK_Z ] and P3DInput.Keyboard.Keys[ P3DK_Z ]) then
        ToggleAxis( alZ );
    end;
  if ( P3DInput.Keyboard.DKeys[ P3DK_G ] and P3DInput.Keyboard.Keys[ P3DK_G ]) then
    EditMode:= emGrab;
  if ( P3DInput.Keyboard.DKeys[ P3DK_R ] and P3DInput.Keyboard.Keys[ P3DK_R ]) then
    EditMode:= emRotate;
  if ( P3DInput.Keyboard.DKeys[ P3DK_S ] and P3DInput.Keyboard.Keys[ P3DK_S ]) then
    EditMode:= emScale;
  if ( P3DInput.Keyboard.DKeys[ P3DK_ESCAPE ] and P3DInput.Keyboard.Keys[ P3DK_ESCAPE ]) then
    CancelTransform;
  if ( P3DInput.Keyboard.DKeys[ P3DK_RETURN ] and P3DInput.Keyboard.Keys[ P3DK_RETURN ]) then
    EditMode:= emDefault;
end;

procedure TP3DScenePanel.MouseMove(X, Y: Integer);
var
  MouseDelta: TVec2;
  AxisX, AxisY, AxisZ: TVec3;
begin
  inherited MouseMove(X, Y);
  if (( Assigned( Selection )) and ( EditMode <> emDefault )) then
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
            if ( AxisLock in [ alX, alYZ ]) then
              Selection.RotateAroundGlobalAxis( AxisX, MouseDelta.x / 50 );
            if ( AxisLock in [ alY, alXZ, alNone ]) then
              Selection.RotateAroundGlobalAxis( AxisY, MouseDelta.x / 50 );
            if ( AxisLock in [ alZ, alXY ]) then
              Selection.RotateAroundGlobalAxis( AxisZ, MouseDelta.x / 50 );
          end;
      end;
    end;
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
            decl.AsMat4:= Obj.Matrix * world;
          TP3DMesh( Obj.Actor.Data ).RenderSimple;
        end;
  end;

var
  Obj: TP3DRenderObject;
  decl: TP3DShaderDecl;
begin
  P3DViewports.Push( Canvas.Left + ClientRect.LeftI, Canvas.Top + ClientRect.TopI, ClientRect.WidthI, ClientRect.HeightI );
  P3DSelectStart( P3DRect( P3DInput.Mouse.X, P3DInput.Mouse.Y, 1, 1 ));
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
      {if ( Obj.Actor.Data is TP3DLight ) then
        begin
          if ( Assigned( EditorSceneLampMaterial )) then
            EditorSceneLampMaterial.Diff:= TP3DLight( Obj.Actor.Data ).Color;
          if ( TP3DLight( Obj.Actor.Data ).LightType in [ p3dltSun, p3dltSpot ]) then
            EditorSceneArrow.RenderFromCamera( FCamObject, mat4( mat3( Obj.Actor.Quaternion )) * mat4translate( vec4( Obj.Actor.Position, 1 )));
          EditorSceneLamp.RenderFromCamera( FCamObject, mat4translate( vec4( Obj.Actor.Position, 1 )));
        end;}
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

procedure TP3DScenePanel.ToggleAxis(Axis: TAxisLock);
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

procedure TP3DScenePanel.CancelTransform;
begin
  if ( Assigned( Selection ) and ( EditMode <> emDefault )) then
    Selection.Matrix:= FEditTransMat;
  FEditMode:= emDefault;
end;

procedure TP3DScenePanel.AfterDraw(Sender: TObject);
var
  StatusText: String;
  FTransform: TMat4;
  Obj: TP3DRenderObject;
begin
  glClear( GL_DEPTH_BUFFER_BIT );
  SelectionMaterial.Diff:= vec3( 1, 1, 0 );
  SelectionMaterial.PassToShader();
  if ( Assigned( Selection )) then
    begin
      if ( EditMode = emDefault ) then
        FTransform:= Selection.Matrix
      else
        FTransform:= FEditTransMat;
      if ( Selection.Data is TP3DMesh ) then
        TP3DMesh( Selection.Data ).RenderEdges( FTransform, TestScene, Selection );
      Canvas.Material.PassToShader();
      EditorSceneArrows.RenderFromCamera( FCamObject, mat4translate( vec4( Selection.Position, 1 )));
    end;

  FRenderList.Clear;
  Scene.MakeRenderList( Nil, Mat4Identity, FRenderList );
  for Obj in FRenderList do
    if ( Obj.Actor.Data is TP3DLight ) then
      begin
        if ( Assigned( EditorSceneLampMaterial )) then
          if ( Selection = Obj.Actor ) then
            EditorSceneLampMaterial.Diff:= TP3DLight( Obj.Actor.Data ).Color + 0.5 + vec3( 0.5, 0.5, 0.0 )
          else
            EditorSceneLampMaterial.Diff:= TP3DLight( Obj.Actor.Data ).Color;

        if ( TP3DLight( Obj.Actor.Data ).LightType in [ p3dltSun, p3dltSpot ]) then
          EditorSceneArrow.RenderFromCamera( FCamObject, mat4( mat3( Obj.Actor.Quaternion )) * mat4translate( vec4( Obj.Actor.Position, 1 )));
        EditorSceneLamp.RenderFromCamera( FCamObject, mat4translate( vec4( Obj.Actor.Position, 1 )));
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


  case ( EditMode ) of
    emDefault: StatusText:= 'Normal mode...';
    emGrab: StatusText:= 'Grabbing actor ...';
    emRotate: StatusText:= 'Rotating actor ...';
    emScale: StatusText:= 'Scaling actor ...';
  end;

  case ( AxisMode ) of
    amScreen: StatusText+= ' [screen';
    amLocal: StatusText+= ' [local';
    amGlobal: StatusText+= ' [global';
  end;

  case ( AxisLock ) of
    alX: StatusText+= ' X]';
    alY: StatusText+= ' Y]';
    alZ: StatusText+= ' Z]';
    alXY: StatusText+= ' XY]';
    alXZ: StatusText+= ' XZ]';
    alYZ: StatusText+= ' YZ]';
    alNone: StatusText+= ']';
  end;

  Canvas.Lock;
  Canvas.RenderText( StatusText, vec2( 0, Height - Font.Size ), FInfoText );

  {Canvas.Font.Name:= 'Pascal3D-Symbols';
  Canvas.Font.Size:= 32;}

  Canvas.Unlock();
end;
{$ENDIF}
