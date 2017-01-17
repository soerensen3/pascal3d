
procedure CreateTestSurface;
var
  S: PSDL_Surface;
begin
  S:= SDL_CreateRGBSurface( SDL_SWSURFACE, 2048, 2048,  32, $000000FF, $0000FF00, $00FF0000, $FF000000 );
  SDL_FreeSurface( S );
end;

procedure InitScene;
begin
  CreateTestSurface;
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
