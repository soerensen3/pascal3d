

procedure Render( Sender: TP3DWindow );
begin
  //glCullFace( GL_FRONT );
  //glEnable( GL_CULL_FACE );
  glClearColor( 225/255, 230/255, 234/255, 1 );
  glClear( GL_DEPTH_BUFFER_BIT or GL_COLOR_BUFFER_BIT );
  glDisable( GL_DEPTH_TEST );
  P3DGUIManager.Render;
  P3DApplication.MainWindow.Title:= IntToStr( Integer( P3DInput.Keyboard.DKeysLayout[ P3DK_Z ]));
end;