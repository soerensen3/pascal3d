{$IFDEF IMPLEMENTATION}
{ __appclassname__ }

procedure __appclassname__.Render;
begin
  inherited Render;
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT ); // Clear the color and depth buffer
  glCullFace( GL_BACK );

  SkyCamObj.Rotation:= CamObj.Rotation;
  TP3DCamera( SkyCamObj.Data ).UpdateMatrices( SkyCamObj );
  SkyRenderList.Render( SkyCamObj ); // Render the skybox

  glClear( GL_DEPTH_BUFFER_BIT ); // Clear the depth buffer because skybox is supposed to be the background

  TP3DCamera( CamObj.Data ).UpdateMatrices( CamObj );
  RenderList.Render( CamObj ); // Render the mainscene

  glClear( GL_DEPTH_BUFFER_BIT ); // Clear the depth buffer because the arrows should be drawn on top
  SkyCamObj.Position:= SkyCamObj.Direction * 2; // go further away to see the arrows from the distance
  TP3DCamera( SkyCamObj.Data ).UpdateMatrices( SkyCamObj );

  P3DViewports.Push( P3DViewports.Screen.Width - 200, 0, 200, 200 );
  ArrowRenderList.Render( SkyCamObj ); // Render arrows
  P3DViewports.Pop;

  SkyCamObj.Position:= vec3( 0 );

  {$IFDEF GUI}
  //DRAW GUI ON TOP
  P3DGUIManager.Render;
  {$ENDIF}
end;



{$ENDIF}
