{$IFDEF INTERFACE}
procedure DrawObjects( Scene: tScene ); forward;
procedure ResizeWnd( Sender: TSDLWindow ); forward;
{$UNDEF INTERFACE}
{$ELSE}

procedure Render( Sender: TSDLWindow );
var
  p: TVec4;
begin
  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  mainscene.Render;
end;

procedure DrawObjects( Scene: tScene );
var
  i: Integer;
  txt: TP3DText;
begin
  if ( not Assigned( ActShad )) then
    exit;
  glDisable( GL_DEPTH_TEST );
  glDisable( GL_CULL_FACE );
  GUIManager.Input;
  GUIManager.Render;
  GUIManager.Controls[ 0 ].Scroll:= vec2( SDL_GetTicks div 10 mod 100 );
  for i:= 0 to GUIManager.Controls.Count - 1 do
    begin
      GUIManager.Controls[ i ].Left:= InputManager.Mouse.X;
      GUIManager.Controls[ i ].Top:= InputManager.Mouse.Y;
    end;
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
  canvas.Unlock(); }
  InputManager.NextCycle;
  //DRAW CODE HERE
end;

procedure ResizeWnd( Sender: TSDLWindow );
begin
  Setup2D( 0, 0, Sender.Width, Sender.Height );
end;


{$ENDIF}
