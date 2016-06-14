

procedure InitScene;
begin
  P3DSceneGUIInit;
  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
  glEnable( GL_LINE_SMOOTH );
  glEnable( GL_DEPTH_TEST );
  //glCullFace( GL_NONE );
  P3DGUIManager.Window:= P3DApplication.MainWindow;
  //P3DGUIManager.ShowCursor:= True;
  //SDL_ShowCursor( 0 );
end;
