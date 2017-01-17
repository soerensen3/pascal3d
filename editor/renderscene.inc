

procedure Render( Sender: TP3DWindow );
begin
  SceneMain.CheckLastChange;
  if ( SceneMain.NeedsUpdate ) then
    SceneMain.Update;
  //glCullFace( GL_FRONT );
  //glEnable( GL_CULL_FACE );
  glClearColor( Primary100.R, Primary100.G, Primary100.B, 1 );
  //glClearColor( 225/255, 230/255, 234/255, 1 );
  glClear( GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT );
  glDisable( GL_DEPTH_TEST );
  P3DGUIManager.Render;
end;
