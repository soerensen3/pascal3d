{$IFDEF INTERFACE}
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
      function PickPixel( P: TVec2 ): TP3DObject;

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
      procedure Paint( const Clear: Boolean = False );

      property Grid: TP3DTileGrid read FGrid write FGrid;
      property BrushPos: TVec3 read FBrushPos write SetBrushPos;
      property BrushGridPos: TIVec2 read FBrushGridPos;
      property CanPaint: Boolean read FCanPaint;
      property BrushScene: TP3DGridScene read GetBrushScene;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DSceneEditModeTileGrid }

procedure TP3DSceneEditModeTileGrid.SetBrushPos(AValue: TVec3);
var
  p: TVec2;
begin
  if FBrushPos=AValue then Exit;
  FBrushPos:=AValue;
  p:= ( BrushPos.xy - ScenePanel.Selection.Transform.Position.xy ) / Grid.GridWorldUnit;
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
  ScenePanel.Canvas3D.RenderLineRect( ScenePanel.Selection.Transform.Position - h_sz,
                                      ScenePanel.Selection.Transform.Position - h_sz +
                                        vec3( Grid.Width, Grid.Height, 0 ) * Grid.GridWorldUnit,
                                        Accent300 );
  ScenePanel.Canvas3D.RenderRect( ScenePanel.Selection.Transform.Position - h_sz,
                                  ScenePanel.Selection.Transform.Position - h_sz +
                                        vec3( Grid.Width, Grid.Height, 0 ) * Grid.GridWorldUnit,
                                        vec4( Accent300.xyz, 0.3 ));
  {WriteLn( P3DShaderActive.DumpAttributes );
  WriteLn( P3DShaderActive.DumpUniforms );
  WriteLn( P3DViewports.DumpViewportInfo());}
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
    t:= - ( ray_pos.z + ScenePanel.Selection.Transform.Position.z ) / ray_dir.z
  else
    t:= 0;
  BrushPos:= ray_pos + ray_dir * t;

  if (( gcisMouseBtn1Down in ScenePanel.Viewer.InputState ) and ( InRange( BrushGridPos.X, 0, Grid.Width - 1 )) and ( InRange( BrushGridPos.Y, 0, Grid.Height - 1 ))) then
    Paint;

  if (( gcisMouseBtn3Down in ScenePanel.Viewer.InputState ) and ( InRange( BrushGridPos.X, 0, Grid.Width - 1 )) and ( InRange( BrushGridPos.Y, 0, Grid.Height - 1 ))) then
    Paint( True );

  if ( P3DInput.Keyboard.DKeys[ P3DK_F2 ] = ksPressed ) then
    Grid.UpdateArrays;
end;

procedure TP3DSceneEditModeTileGrid.Paint(const Clear: Boolean);
var
  Scene: TP3DGridScene;
begin
  if ( CanPaint ) then
    begin
      if ( Clear ) then
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

  ScenePanel.Selection.Transform.Matrix:= TransMat;
  Scale:= vec3( 1 );
  if ( AxisLock in [ alX, alXZ, alXY, alNone ]) then
    Scale.x:= MouseDelta.x;
  if ( AxisLock in [ alY, alXY, alYZ, alNone ]) then
    Scale.y:= MouseDelta.y;
  if ( AxisLock in [ alZ, alXZ, alYZ ]) then
    Scale.z:= MouseDelta.y;
  if ( Scale <> vec3( 1 )) then
    ScenePanel.Selection.Transform.Scale:= Scale;
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

  ScenePanel.Selection.Transform.Matrix:= TransMat;
  c:= ToScreenCoords( TransCenter );
  dir:= ( MousePos - c.XY ).Normalize;
  dirOld:= ( MousePosInitial - c.XY ).Normalize;
  Angle:= arctan2( dirOld.y, dirOld.x ) - arctan2( dir.y, dir.x );
  P3DApplication.MainWindow.Title:= FloatToStr( Angle );
  if ( AxisLock in [ alX, alYZ ]) then
    ScenePanel.Selection.Transform.RotateAroundGlobalAxis( AxisX, Angle );
  if ( AxisLock in [ alY, alXZ, alNone ]) then
    ScenePanel.Selection.Transform.RotateAroundGlobalAxis( AxisY, Angle );
  if ( AxisLock in [ alZ, alXY ]) then
    ScenePanel.Selection.Transform.RotateAroundGlobalAxis( AxisZ, Angle );
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
  ScenePanel.Selection.Transform.Matrix:= TransMat;
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

  ScenePanel.Selection.Transform.Position:= ScenePanel.Selection.Transform.Position + Translation;
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
    ScenePanel.Selection.Transform.Matrix:= TransMat;
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
  SceneView.ObjectList.Clear;
  SceneView.Viewer.OverrideMaterial.PassToShader();
  SceneView.Scene.MakeRenderList( nil, Mat4Identity, SceneView.FObjectList, [ rlfObjects, rlfScenes, rlfTileGrids ]);

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

  if ( P3DInput.Mouse.Buttons[ 2 ] and ( gcisMouseOver in ScenePanel.Viewer.InputState )) then
    if ( ScenePanel.EditMode = emDefault ) then
      ScenePanel.Selection:= PickPixel( MousePos )
    else
      CancelTransform;
end;

function TP3DSceneEditModeDefault.PickPixel(P: TVec2): TP3DObject;
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
        glLineWidth( 1 );
  //      glDisable( GL_DEPTH_TEST );
        glDepthMask( False );
  //      glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
        SelectionMaterial.PassToShader();

        FreeAndNil( FSelectionRenderList );
        FSelectionRenderList:= TP3DRenderList.Create;
        FSelectionRenderList.OnAdd:= @ScenePanel.RenderListOnAdd;
        //n:= ObjectList.Find( Selection, ScenePanel.Viewer.OverrideMaterial );

        FSelectionRenderList.Add( Selection, Selection.CalculateParentMatrix, Selection.Scene, RenderFlags );
        {if ( n >= 0 ) then
          begin
            FSelectionRenderList.Add( ObjectList[ n ].Data, {ObjectList[ n ].Matrices[ 0 ]}Mat4Identity, Selection.Scene, RenderFlags );
        //FSelectionRenderList.Add( Selection.Data, Selection.MatrixFinal, Selection.Scene, RenderFlags );
            mat4decompose( ObjectList[ n ].Matrices[ 0 ], loc, rot, scale );
            Viewer.Caption:= 'Location: (' + loc.ToString( 1 ) + ') Rotation: (' + quat( rot ).ToString( 1 ) + ') Scale: (' + scale.ToString( 1 ) + ')';
          end;}
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
  //if ( ScenePanel.Selection is TP3DArmature ) then
  //  ScenePanel.DrawArmature( ScenePanel.Selection )

  if ( ScenePanel.Selection.Data is TP3DArmature ) then
    begin
      ScenePanel.Cam.PassToShader;
      ScenePanel.Canvas3D.View:= ScenePanel.Cam.View;
      ScenePanel.Canvas3D.Proj:= ScenePanel.Cam.Proj;
      ScenePanel.DrawArmature( ScenePanel.Selection );
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
    alX: Result+= ' X] ';
    alY: Result+= ' Y] ';
    alZ: Result+= ' Z] ';
    alXY: Result+= ' XY] ';
    alXZ: Result+= ' XZ] ';
    alYZ: Result+= ' YZ] ';
    alNone: Result+= '] ';
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
  if ( Assigned( EditorSceneArrowsRL )) then
    EditorSceneArrowsRL.Clear;
  EditorSceneArrows.MakeRenderList( nil, mat4translate( vec4( TransCenter, 1 )), EditorSceneArrowsRL );
  if ( Assigned( ScenePanel.Selection )) then
    EditorSceneArrowsRL.Render( ScenePanel.CamObject );
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

{$ENDIF}
