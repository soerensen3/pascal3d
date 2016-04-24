{$IFDEF INTERFACE}

{$UNDEF INTERFACE}
{$ELSE}

procedure Render( Sender: TP3DWindow );
var
  p: TVec4;
  BBox: TP3DBoundingBox;
  Obj: Integer;
  world, view: TP3DShaderDecl;
begin
  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer
  maincamera.Matrix:= //mat4lookatLH( 5*vec3( cos( SDL_GetTicks / 1000 ), 0, sin( SDL_GetTicks / 1000 ) ), vec3( 0, 0, 1 ), vec3( 0, 1, 0 )); //vec4( maincamera.Position, 1));
  mat4translate( vec4( CamPos, 1 )) * mat4rotateEuler( CamYawPitchRoll * deg2rad );

  {TP3DRenderableObject( mainscene.Objects[ 3 ]).Matrix:= mat4rotate( vec3( 0.5, 0.5, 0 ).Normalize, ( SDL_GetTicks / 10000 )) * mat4translate( vec4( TP3DRenderableObject( mainscene.Objects[ 3 ]).Position, 1 ));

  BBox:= TP3DRenderableObject( mainscene.Objects[ 3 ]).CalcBoundingBox();

  Obj:= mainscene.Objects.FindByName( 'BBox' );
  if ( Obj >= 0 ) then
    with ( mainscene.Objects[ Obj ] as TP3DRenderableObject ) do
      Matrix:= mat4scale( vec4(( BBox.Max - BBox.Min ) / 2, 1 )) * mat4translate( vec4( BBox.Center, 1 )); }
  P3DApplication.MainWindow.Title:= CamPos.ToString();
  mainscene.Render;
  {material.PassToShader();
  TP3DCamera( maincamera.Data ).PassToShader( Mat4Identity );
  world:= P3DShaderActive.Uniforms.Names[ 'world' ];
  if ( Assigned( world )) then
    world.AsMat4:= Mat4Identity;
  view:= P3DShaderActive.Uniforms.Names[ 'view' ];
    if ( Assigned( view )) then
      view.AsMat4:= maincamera.Matrix;


  //P3DLog.LogInfo( nil, P3DViewports.DumpViewportInfo());

  //P3DLog.LogInfo( nil, P3DShaderActive.DumpUniforms );
  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib4f( P3DAttribColor, 1, 1, 1, 1 );
  glVertexAttrib4f( P3DAttribPosition, -5, -5, -1, 1 );
  glVertexAttrib4f( P3DAttribPosition, 5, -5, -1, 1 );
  glVertexAttrib4f( P3DAttribPosition, 5, 5, -1, 1 );
  glVertexAttrib4f( P3DAttribPosition, -5, 5, -1, 1 );
  glEnd();}
end;


procedure ResizeWnd( Sender: TP3DWindow );
begin

end;

{$ENDIF}
