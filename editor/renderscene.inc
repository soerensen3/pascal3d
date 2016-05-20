

procedure Render( Sender: TP3DWindow );
begin
  glClearColor( 165/255, 165/255, 237/255, 1 );
  glClear( GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT );
  glDisable( GL_DEPTH_TEST );
  P3DGUIManager.Render;
end;
