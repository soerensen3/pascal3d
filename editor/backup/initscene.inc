
procedure CreateTestSurface;
var
  S: PSDL_Surface;
begin
  S:= SDL_CreateRGBSurface( SDL_SWSURFACE, 2048, 2048,  32, $000000FF, $0000FF00, $00FF0000, $FF000000 );
  SDL_FreeSurface( S );
end;

procedure Colors;
begin
  Primary50:=                           Indigo50;
  Primary100:=                          Indigo100;
  Primary200:=                          Indigo200;
  Primary300:=                          Indigo300;
  Primary400:=                          Indigo400;
  Primary500:=                          Indigo500;
  Primary600:=                          Indigo600;
  Primary700:=                          Indigo700;
  Primary800:=                          Indigo800;
  Primary900:=                          Indigo900;

  Secondary50:=                         amber50;
  Secondary100:=                        amber100;
  Secondary200:=                        amber200;
  Secondary300:=                        amber300;
  Secondary400:=                        amber400;
  Secondary500:=                        amber500;
  Secondary600:=                        amber600;
  Secondary700:=                        amber700;
  Secondary800:=                        amber800;
  Secondary900:=                        amber900;


  Accent50:=                            Indigo50;
  Accent100:=                           Indigo100;
  Accent200:=                           Indigo200;
  Accent300:=                           Indigo300;
  Accent400:=                           Indigo400;
  Accent500:=                           Indigo500;
  Accent600:=                           Indigo600;
  Accent700:=                           Indigo700;
  Accent800:=                           Indigo800;
  Accent900:=                           Indigo900;
end;

procedure InitScene;
begin
  CreateTestSurface;
  Colors;

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
