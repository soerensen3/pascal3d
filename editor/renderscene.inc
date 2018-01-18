procedure PerformActions( GlobalTime: Double; Lib: TP3DLibrary );
var
  Action: TP3DAction;
  Block: IP3DStreamableUser;
begin
  for Action in Lib.Actions do
    for Block in Action.Users do
      if ( Block.GetUser is TP3DDataBlock ) then
        Action.Perform( GlobalTime, TP3DDataBlock( Block.GetUser ));
end;

procedure TP3DSceneApplication.Render;
var
  GlobalTime: Double;
begin
  inherited Render;

  //GlobalTime:= SDL_GetTicks / 100;
  //MainWindow.Title:= FloatToStr( GlobalTime );
  //if ( Assigned( SceneMain.ActiveLibrary )) then
  //  PerformActions( GlobalTime, SceneMain.ActiveLibrary );

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

