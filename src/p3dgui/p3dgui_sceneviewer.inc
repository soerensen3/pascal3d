//p3dgui_sceneviewer.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

type

  { TP3DSceneViewer }

  TP3DSceneViewer = class ( TP3DGroupBox )
    protected
      FAfterRender: TNotifyEvent;
      FBeforeRender: TNotifyEvent;
      FOrbitCamera: Boolean;
      FOrbitCenter: TVec3;
      FOrbitDistance: Single;
      FScene: TP3DScene;
      FCam: TP3DCamera;
      FCamObject: TP3DActor;

      procedure SetOrbitCamera(AValue: Boolean);
      procedure UpdateCamPos;
      procedure ResetOrbit;
      procedure PushVPClientRect; virtual;

    public
      procedure Render(BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect); override;
      procedure MouseMove(X, Y: Integer); override;
      procedure MouseWheel(delta: Integer); override;
      procedure KeyboardAction; override;

      property Scene: TP3DScene read FScene write FScene;
      property OrbitCamera: Boolean read FOrbitCamera write SetOrbitCamera;
//      property OrbitRotation: TVec3 read FCam.Rotation write FCam.Rotation;
      property OrbitDistance: Single read FOrbitDistance write FOrbitDistance;
      property OrbitCenter: TVec3 read FOrbitCenter write FOrbitCenter;
      property BeforeRender: TNotifyEvent read FBeforeRender write FBeforeRender;
      property AfterRender: TNotifyEvent read FAfterRender write FAfterRender;
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
      FCam:= TP3DCamera.Create();
      FCamObject:= TP3DActor.Create();
      FCamObject.Data:= FCam;
      FCamObject.RotationOrder:= roXZY;
      ResetOrbit;
      UpdateCamPos;
    end
  else
    FreeAndNil( FOrbitCamera );
end;

procedure TP3DSceneViewer.UpdateCamPos;
begin
  FCam.UpdateMatrices( FCamObject );
  FCamObject.Position:= FCamObject.Matrix.Row[ 2 ].xyz * OrbitDistance + OrbitCenter;
end;

procedure TP3DSceneViewer.ResetOrbit;
begin
  OrbitDistance:= 10;
  OrbitCenter:= vec3( 0 );
  if ( Assigned( FCamObject )) then
    begin
      FCamObject.Position:= vec3( 0 );
      FCamObject.Rotation:= vec3( 0 );
    end;
end;

procedure TP3DSceneViewer.PushVPClientRect;
begin
  P3DViewports.Push( Round( Canvas.Left + ClientRect.Left ), Round( Canvas.Top + ClientRect.Top ),
                     Round( ClientRect.Width ), Round( ClientRect.Height ));
end;

procedure TP3DSceneViewer.Render(BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect);
var
  txt: TP3DText;
begin
  inherited Render(BaseColor, ScrollAcc, ClipRect);
  //Canvas.Lock;
  PushVPClientRect;

  if ( Assigned( Scene ) and ( ClientRect.Width * ClientRect.Height > 0 )) then
    begin
      if ( Assigned( BeforeRender )) then
        BeforeRender( Self );
      glEnable( GL_DEPTH_TEST );
      if ( OrbitCamera ) then
        begin
          UpdateCamPos;
          FCam.Aspect:= ClientRect.Width / ClientRect.Height;
          Scene.RenderFromCamera( FCamObject );
        end
      else
        Scene.Render;
      if ( Assigned( AfterRender )) then
        AfterRender( Self );
      glDisable( GL_DEPTH_TEST );

      if ( not Assigned( Scene.Cam ) and ( not OrbitCamera )) then
        begin
          Canvas.Lock;
          txt:= p3dTextSimple( 'Warning, no camera set!', P3DFontManager[ Font.Name, Font.Size ]);
          Canvas.RenderText( txt, ClientRect.TopLeft );
          Canvas.Unlock();
        end;
    end;
    {
  else
    TriangleScene.Render}

  P3DViewports.Pop;
  //Canvas.Unlock();
end;

procedure TP3DSceneViewer.MouseMove(X, Y: Integer);
var
  m: TMat4;
begin
  inherited MouseMove(X, Y);
  if ( OrbitCamera and ( gcisMouseOver in InputState )) then
    begin
      if ( gcisMouseBtn2Down in InputState ) then
        if ( P3DInput.Keyboard.Keys[ P3DK_LSHIFT ] or P3DInput.Keyboard.Keys[ P3DK_LCTRL ]) then
          begin
            m:= FCam.View;
            mat4inverse( m, m );
            if ( P3DInput.Keyboard.Keys[ P3DK_LSHIFT ]) then
              OrbitCenter:= OrbitCenter - ( m.Row[ 0 ].xyz * P3DInput.Mouse.DX - m.Row[ 1 ].xyz * P3DInput.Mouse.DY ) * 0.2;
            if ( P3DInput.Keyboard.Keys[ P3DK_LCTRL ]) then
              OrbitCenter:= OrbitCenter + m.Row[ 2 ].xyz * P3DInput.Mouse.DY * 0.2;
          end
        else
          FCamObject.Rotation:= FCamObject.Rotation - vec3( P3DInput.Mouse.DY, 0, P3DInput.Mouse.DX ) * 2.5;
    end;
end;

procedure TP3DSceneViewer.MouseWheel(delta: Integer);
begin
  inherited MouseWheel(delta);
  if ( OrbitCamera and ( gcisMouseOver in InputState )) then
    if ( FCam.IsIsometric ) then
      FCam.IsoZoom:= Max( 0.1, FCam.IsoZoom - delta * 0.1 )
    else
      OrbitDistance:= Max( 0.1, OrbitDistance - delta * 1 );
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
        FCamObject.Rotation:= vec3( 0, 180 * Ord( P3DInput.Keyboard.Keys[ P3DK_LCTRL ]), 0 );
      if ( KeyEvent( P3DK_1 ) or KeyEvent( P3DK_KP_1 )) then
        FCamObject.Rotation:= vec3( 90, 0, 180 * Ord( P3DInput.Keyboard.Keys[ P3DK_LCTRL ]));
      if ( KeyEvent( P3DK_3 ) or KeyEvent( P3DK_KP_3 )) then
        FCamObject.Rotation:= vec3( 90, 0, 90 + 180 * Ord( P3DInput.Keyboard.Keys[ P3DK_LCTRL ]));
      if ( KeyEvent( P3DK_5 ) or KeyEvent( P3DK_KP_5 )) then
        FCam.IsIsometric:= not FCam.IsIsometric;
    end;
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

