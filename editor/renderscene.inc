

procedure Render( Sender: TP3DWindow );
begin
  //glCullFace( GL_FRONT );
  //glEnable( GL_CULL_FACE );
  glClearColor( 225/255, 230/255, 234/255, 1 );
  glClear( GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT );
  glDisable( GL_DEPTH_TEST );
  P3DGUIManager.Render;
  {P3DGUIManager.ScreenCanvas.Lock;
  P3DGUIManager.ScreenCanvas.RenderRect( vec2( 100, 100 ), vec2( 400, 400 ), vec4( 1, 0, 0, 0.3 ));
  P3DGUIManager.ScreenCanvas.RenderArc( vec2( 250, 250 ), 25, 90, 180, 16, vec4( 0, 0, 1, 0.5 ));
  P3DGUIManager.ScreenCanvas.RenderRoundRect( vec2( 100, 100 ), vec2( 400, 400 ), 25, 16, vec4( 0, 1, 0, 0.5 ));
  P3DGUIManager.ScreenCanvas.Unlock(); }
end;
