procedure TP3DSceneApplication.Render;
var
  GlobalTime: Double;
begin
  inherited Render;

  //GlobalTime:= SDL_GetTicks / 100;
  //P3DData.PerformActions( GlobalTime );
  //MainWindow.Title:= FloatToStr( GlobalTime );

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

