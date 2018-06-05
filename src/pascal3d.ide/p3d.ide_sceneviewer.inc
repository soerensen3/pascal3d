//p3dgui_sceneviewer.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

type

  { TP3DSceneViewer }

  TP3DSceneViewer = class ( TP3DGroupBox )
    private
      FCamSpeed: Single;
      //FOverrideMaterial: TP3DMaterial;
      //FRenderFlags: TP3DRenderListFlags;
      //FRenderList: TP3DRenderList;
      FAfterRender: TNotifyEvent;
      FBeforeRender: TNotifyEvent;
      FCommandList: TP3DRenderCommandContainerList;
      FOrbitCamera: Boolean;
      FOrbitCenter: TVec3;
      FOrbitDistance: Single;
      FScene: TP3DStreamablePointer;
      FCamObject: TP3DStreamablePointer;
      FZoomSpeed: Single;

      function GetCamObject: TP3DObject;
      function GetScene: TP3DScene;
      procedure SetCamObject(AValue: TP3DObject);
      procedure SetOrbitCamera(AValue: Boolean);
      procedure SetOrbitDistance(AValue: Single);
      procedure SetScene(AValue: TP3DScene);
      procedure UpdateCamPos;
      procedure ResetOrbit;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Paint(); override;
      procedure MouseMove(X, Y: Float); override;
      procedure MouseWheel( deltax, deltay: Integer); override;
      procedure KeyboardAction; override;
      procedure PushVPClientRect; virtual;

      //property Scene: TP3DScene read GetScene write SetScene;
      property OrbitCamera: Boolean read FOrbitCamera write SetOrbitCamera;
//      property OrbitRotation: TVec3 read FCam.Rotation write FCam.Rotation;
      property OrbitDistance: Single read FOrbitDistance write SetOrbitDistance;
      property OrbitCenter: TVec3 read FOrbitCenter write FOrbitCenter;
      property BeforeRender: TNotifyEvent read FBeforeRender write FBeforeRender;
      property AfterRender: TNotifyEvent read FAfterRender write FAfterRender;
      //property RenderList: TP3DRenderList read FRenderList write FRenderList;
      property CommandList: TP3DRenderCommandContainerList read FCommandList write FCommandList;
      property CamObject: TP3DObject read GetCamObject write SetCamObject;
      //property RenderFlags: TP3DRenderListFlags read FRenderFlags write FRenderFlags;
      //property OverrideMaterial: TP3DMaterial read FOverrideMaterial write FOverrideMaterial;
      property CamSpeed: Single read FCamSpeed write FCamSpeed;
      property ZoomSpeed: Single read FZoomSpeed write FZoomSpeed;
  end;

//  procedure InitTriangle;

{  var
    TriangleScene: TP3DScene;
    TriangleCam: TP3DCamera;
    TrianglePoints: TP3DVec3BufferGL;
    TriangleColors: TP3DVec4BufferGL;}

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{
procedure InitTriangle;
begin
  TrianglePoints:= TP3DVec3BufferGL.Create( False );
  TrianglePoints.Add([ vec3( -1.0, -1.0, 0.0 ),
               vec3(  1.0, -1.0, 0.0 ),
               vec3(  0.0,  1.0, 0.0 )]);
  TrianglePoints.PushData;
  TriangleColors:= TP3DVec4BufferGL.Create( False );
  TriangleColors.Add([ vec4( 1.0, 0.0, 0.0, 1.0 ),
               vec4( 0.0, 1.0, 0.0, 1.0 ),
               vec4( 0.0, 0.0, 1.0, 1.0 )]);
  TriangleColors.PushData;
end;


procedure RenderTriangle(Scene: TP3DScene);
begin
  if ( not Assigned( TrianglePoints )) then
    InitTriangle;
  TrianglePoints.SetAttribArray( P3DAttribPosition );
  TriangleColors.SetAttribArray( P3DAttribColor );

  //Scene.Shader.Enable;
  //Scene.Shader.Uniforms[ Scene.Shader.Uniforms.FindByName( 'world' )].AsMat4:= Mat4Identity;

  glDrawArrays( GL_TRIANGLES, 0, 3 );

  TrianglePoints.UnsetAttribArray();
  TriangleColors.UnsetAttribArray();
end;
}

{ TP3DSceneViewer }

procedure TP3DSceneViewer.SetOrbitCamera(AValue: Boolean);
begin
  if FOrbitCamera=AValue then Exit;
  FOrbitCamera:=AValue;
  if ( AValue ) then
    begin
      //CamObject:= TP3DObject.CreateBlock();
      //CamObject.Data:= TP3DCamera.CreateBlock();
      if ( Assigned( CamObject )) then begin
        CamObject.Transform.RotationOrder:= roXZY;
        ResetOrbit;
        UpdateCamPos;
      end;
    end
  else
    FreeAndNil( FOrbitCamera );
end;

function TP3DSceneViewer.GetCamObject: TP3DObject;
begin
  Result:= TP3DObject( FCamObject.AsValue );
end;

function TP3DSceneViewer.GetScene: TP3DScene;
begin
  Result:= TP3DScene( FScene.AsValue );
end;

procedure TP3DSceneViewer.SetCamObject(AValue: TP3DObject);
begin
  FCamObject.AsValue:= AValue;
end;

procedure TP3DSceneViewer.SetOrbitDistance(AValue: Single);
begin
  if FOrbitDistance=AValue then Exit;
  FOrbitDistance:=AValue;
end;

procedure TP3DSceneViewer.SetScene(AValue: TP3DScene);
begin
  FScene.AsValue:= AValue;
end;

procedure TP3DSceneViewer.UpdateCamPos;
begin
  CamObject.Transform.Position:= CamObject.Transform.Matrix.Row[ 2 ].xyz * OrbitDistance + OrbitCenter;
  TP3DCamera( CamObject.Data ).UpdateMatrices( CamObject );
end;

procedure TP3DSceneViewer.ResetOrbit;
begin
  OrbitDistance:= 10;
  OrbitCenter:= vec3( 0 );
  if ( Assigned( CamObject )) then
    begin
      CamObject.Transform.Position:= vec3( 0 );
      CamObject.Transform.Rotation:= vec3( 0 );
    end;
end;

constructor TP3DSceneViewer.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  //RenderFlags:= P3DRenderListFlagsDefault;
  //RenderList:= TP3DRenderList.Create;
  //CamSpeed:= 0.2;
  CamSpeed:= 0.1;
  ZoomSpeed:= 0.2;
  FCamObject:= TP3DStreamablePointer.Create( 'CamObject', TP3DObject );
  FScene:= TP3DStreamablePointer.Create( 'Scene', TP3DScene );
  Properties.Add([ FCamObject, FScene ]);
  //ZoomSpeed:= 1;
end;

procedure TP3DSceneViewer.PushVPClientRect;
begin
  if ( Borderless ) then
    P3DViewports.Push( Round( ClippedRect.Left ), Round( P3DViewports.Screen.Height - ClippedRect.Top - ClippedRect.Height ),
                       Round( ClippedRect.Width ), Round( ClippedRect.Height ))
  else
    P3DViewports.Push( Round( ClippedRect.Left + ClientRect.Left ), Round( P3DViewports.Screen.Height - ( ClippedRect.Top + ClientRect.Top ) - Round( ClientRect.Height )),
                       Round( ClientRect.Width ), Round( ClientRect.Height ));
end;

procedure TP3DSceneViewer.Paint();
var
  txt: TP3DTextBmp;
begin
  inherited Paint();
  //Canvas.Lock;
  PushVPClientRect;

  if ( Assigned( CommandList ) and ( ClientRect.Width * ClientRect.Height > 0 )) then begin
    //if ({( not Assigned( Scene.Cam )) and }( not Assigned( CamObject ))) then
    //  exit;

    {RenderList.Clear;
    if ( rlfMaterials in RenderFlags ) then
      if ( Assigned( OverrideMaterial )) then
        OverrideMaterial.PassToShader();
    Scene.MakeRenderList( nil, Mat4Identity, FRenderList, RenderFlags );
    RenderList.Optimize;}

    if ( Assigned( BeforeRender )) then
      BeforeRender( Self );

    glClear( GL_DEPTH_BUFFER_BIT );
    glEnable( GL_DEPTH_TEST ); cle( Self );

    if ( OrbitCamera ) then //begin
      UpdateCamPos;
      {TP3DCamera( CamObject.Data ).Aspect:= ClientRect.Width / ClientRect.Height;
      TP3DCamera( CamObject.Data ).UpdateProj;
      RenderList.Render( CamObject, RenderFlags );
    end else
      RenderList.Render( Scene.Cam, RenderFlags );}
    CommandList.Execute;

    if ( Assigned( AfterRender )) then
      AfterRender( Self );

    glDisable( GL_DEPTH_TEST ); cle( Self );
  end else begin
    Canvas.Lock;
    txt:= TP3DTextBmp.CreateSimple( 'Warning, no camera set!', P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size );
    Canvas.RenderText( txt, ClientRect.TopLeft );
    Canvas.Unlock();
    txt.Free;
  end;

    {
  else
    TriangleScene.Render}

  P3DViewports.Pop;
  //Canvas.Unlock();
end;

procedure TP3DSceneViewer.MouseMove(X, Y: Float);
var
  m: TMat4;
begin
  inherited MouseMove(X, Y);
  if ( OrbitCamera and ( gcisMouseOver in InputState )) then
    begin
      if (( gcisMouseBtn2Down in InputState ) or (( gcisMouseBtn1Down in InputState ) and P3DInput.Keyboard.Keys[ P3DK_LALT ])) then
        if ( P3DInput.Keyboard.Keys[ P3DK_LSHIFT ] or P3DInput.Keyboard.Keys[ P3DK_LCTRL ]) then
          begin
            m:= TP3DCamera( CamObject.Data ).View;
            mat4inverse( m, m );
            if ( P3DInput.Keyboard.Keys[ P3DK_LSHIFT ]) then
              OrbitCenter:= OrbitCenter - ( m.Row[ 0 ].xyz * P3DInput.Mouse.DX - m.Row[ 1 ].xyz * P3DInput.Mouse.DY ) * CamSpeed;
            if ( P3DInput.Keyboard.Keys[ P3DK_LCTRL ]) then
              OrbitCenter:= OrbitCenter + m.Row[ 2 ].xyz * P3DInput.Mouse.DY * CamSpeed;
          end
        else
          CamObject.Transform.Rotation:= vec3( min( 180, max( 0, CamObject.Transform.Rotation.X - P3DInput.Mouse.DY * 2.5 )), 0, CamObject.Transform.Rotation.Z - P3DInput.Mouse.DX * 2.5 );
    end;
end;

procedure TP3DSceneViewer.MouseWheel(deltax, deltay: Integer);
begin
  inherited MouseWheel( deltax, deltay );
  if ( OrbitCamera and ( gcisMouseOver in InputState )) then
    if ( TP3DCamera( CamObject.Data ).IsIsometric ) then
      TP3DCamera( CamObject.Data ).IsoZoom:= Max( 0.1, TP3DCamera( CamObject.Data ).IsoZoom - deltay * ZoomSpeed / 10 )
    else
      OrbitDistance:= Max( 0.1, OrbitDistance - deltay * ZoomSpeed );
end;

procedure TP3DSceneViewer.KeyboardAction;
  function KeyEvent( Key: Word ): Boolean;
  begin
    Result:= P3DInput.Keyboard.DKeysLayout[ Key ] = ksPressed;
  end;

begin
  inherited KeyboardAction;
  if ( OrbitCamera and ( gcisMouseOver in InputState )) then
    begin
      if ( KeyEvent( P3DK_7 ) or KeyEvent( P3DK_KP_7 )) then
        CamObject.Transform.Rotation:= vec3( 0, 180 * Ord( P3DInput.Keyboard.Keys[ P3DK_LCTRL ]), 0 );
      if ( KeyEvent( P3DK_1 ) or KeyEvent( P3DK_KP_1 )) then
        CamObject.Transform.Rotation:= vec3( 90, 0, 180 * Ord( P3DInput.Keyboard.Keys[ P3DK_LCTRL ]));
      if ( KeyEvent( P3DK_3 ) or KeyEvent( P3DK_KP_3 )) then
        CamObject.Transform.Rotation:= vec3( 90, 0, 90 + 180 * Ord( P3DInput.Keyboard.Keys[ P3DK_LCTRL ]));
      if ( KeyEvent( P3DK_5 ) or KeyEvent( P3DK_KP_5 )) then
        TP3DCamera( CamObject.Data ).IsIsometric:= not TP3DCamera( CamObject.Data ).IsIsometric;
    end;
end;

destructor TP3DSceneViewer.Destroy;
begin
  //if ( Assigned( RenderList )) then
  //  RenderList.Free;
  inherited Destroy;
end;

{
initialization
  TriangleCam:= TP3DCamera.Create;
  TriangleCam.Position:= vec3( 0, 0, 2 );
  TriangleScene:= TP3DScene.Create;
  TriangleScene.Cam:= TriangleCam;
  //TriangleScene.DrawObjects:= @RenderTriangle;

finalization
  TriangleScene.Free;
  TrianglePoints.Free;
  TriangleColors.Free; }


{$ENDIF}

