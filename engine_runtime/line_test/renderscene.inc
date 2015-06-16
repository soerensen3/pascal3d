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
begin
  glDisable( GL_DEPTH_TEST );
  RenderLine2D( vec2( 10, 10 ), vec2( 50, 50 ), vec4( 1, 0, 0, 1 ));
  RenderLines2D( points, vec4( 0, 0, 1, 1 ), 10 );
  RenderLines2D( points, vec4( 1, 0, 0, 1 ));
  RenderLines2D([ vec2( 0.0, 0.0 ), vec2( 799, 0 ), vec2( 799, 599 ), vec2( 0, 599 ), vec2( 0, 0 )], vec4( 0, 1, 1, 1 ), 2 );
end;

procedure ResizeWnd( Sender: TSDLWindow );
begin
  //geometry.Setup2D( Sender.Window^.w, Sender.Window^.h );
//  glViewport( 0, 0, 500, 500 );
end;

{$ENDIF}
