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
begin
  if ( not Assigned( ActShad )) then
    exit;
  glDisable( GL_DEPTH_TEST );
  glDisable( GL_CULL_FACE );
  GUIManager.Input;
  GUIManager.Render;
  for i:= 0 to GUIManager.Controls.Count - 1 do
    begin
      GUIManager.Controls[ i ].Left:= InputManager.Mouse.X;
      GUIManager.Controls[ i ].Top:= InputManager.Mouse.Y;
    end;
  InputManager.NextCycle;
  //DRAW CODE HERE
end;

procedure ResizeWnd( Sender: TSDLWindow );
begin
  Setup2D( 0, 0, Sender.Width, Sender.Height );
end;


{$ENDIF}
