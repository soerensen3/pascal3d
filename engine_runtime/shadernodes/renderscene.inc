{$IFDEF INTERFACE}
procedure DrawObjects( Scene: tScene ); forward;
procedure ResizeWnd( Sender: TSDLWindow ); forward;
{$UNDEF INTERFACE}
{$ELSE}

procedure Render( Sender: TSDLWindow );
var
  p: TVec4;
begin
  glClearColor($FF / 255, $E0 / 255, $B3 / 255, 1.0);                      // Set the Background colour of or scene
  //glClearColor($EB / 255, $75 / 255, $6B / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  mainscene.Render;
end;

procedure DrawObjects( Scene: tScene );
var
  i: Integer;
  txt: TP3DText;
begin
  for i:= ParentList.Count - 1 downto 0 do
    if ( ParentList[ i ] is TP3DNodeControl ) then
      if ( not Assigned( TP3DNodeControl( ParentList[ i ]).Node )) then
        ParentList[ i ].Free;
  if ( not Assigned( ActShad )) then
    exit;

  glDisable( GL_DEPTH_TEST );
  glDisable( GL_CULL_FACE );
  GUIManager.Input;
  GUIManager.Render;
  {if ( DragDropSrc <> nil ) then
    begin
      GUIManager.ScreenCanvas.Lock;
      txt:= p3dTextSimple( DragDropSrc.Name, dejavu, 16 );
      GUIManager.ScreenCanvas.RenderRect( vec2( 10, 10 ), vec2( 10 + txt.Width, 10 + txt.Height ), vec4( 0.1, 0.3, 0.5, 1 ));
      GUIManager.ScreenCanvas.RenderText( txt, vec2( 10, 10 ));
      txt.Free;
      GUIManager.ScreenCanvas.Unlock();
    end;}

  {canvas.Left:= 150;
  canvas.Top:= 300;
  canvas.Width:= InputManager.Mouse.X - 150;
  canvas.Height:= InputManager.Mouse.Y - 300;

  canvas.Lock;
  canvas.RenderRect( vec2( 0 ), vec2( canvas.Width, canvas.Height ), vec4( vec3( 1 ), 0.2 ));
  canvas.Font.Name:= 'Deja Vu Sans';
  txt:= p3dTextSimple( 'Dies ist Text!', dejavu, 16 );
  canvas.RenderText( txt, vec2( 10, 10 ));
  txt.Free;
  canvas.Unlock();}
  InputManager.NextCycle;
  //DRAW CODE HERE
end;

procedure ResizeWnd( Sender: TSDLWindow );
var
  vp: TP3DViewport;
begin
  Setup2D( 0, 0, Sender.Width, Sender.Height );
  if ( P3DViewports.Count > 0 ) then
    begin
      vp:= P3DViewports.VP[ 0 ];
      vp.Width:= Sender.Width;
      vp.Height:= Sender.Height;
      P3DViewports.VP[ 0 ] := vp;
    end;
  GUIManager.UpdateExtents;
//  canvas.Width:= Sender.Width;
//  canvas.Height:= Sender.Height;
end;


{$ENDIF}
