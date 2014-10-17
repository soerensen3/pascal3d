{$IFDEF INTERFACE}
procedure DrawObjects( Scene: tScene ); forward;
procedure DrawShadowObjects( Scene: tScene ); forward;
procedure DrawSkyBoxObjects( Scene: tScene ); forward;
{$UNDEF INTERFACE}
{$ELSE}

procedure CheckFileChanges; //inline;
begin
  if ( WatchTime < GetTickCount ) then
    WatchTime:= GetTickCount + 500
  else
    Exit;
  if ( Assigned( Watch_simpleshader_F ) AND Assigned( Watch_simpleshader_V )) then
    if ( Watch_simpleshader_F.CheckForChange or Watch_simpleshader_V.CheckForChange ) then
      begin
        UnloadShader;
        LoadShader;
      end;
  if ( Assigned( Watch_Mdl )) then
    if ( Watch_Mdl.CheckForChange ) then
      begin
        UnloadModels;
        LoadModels;
      end;
end;

procedure Render( Sender: TSDLWindow );
var
  p: TVec4;
begin
  CheckFileChanges;

  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
//  glClearColor(0.01, 0.02, 0.05, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  //shadowscene.Render;
  skyboxscene.Render;

  glClear( GL_DEPTH_BUFFER_BIT);

  {simpleshader.Enable;
  ShaderSetParameter4fv( simpleshader.ShaderObj, 'world', Mat4Identity );//world );
  ShaderSetParameter4fv( simpleshader.ShaderObj, 'view', maincamera.view );
  ShaderSetParameter4fv( simpleshader.ShaderObj, 'proj', maincamera.proj );
  terrain_collision.Render( Mat4Rot( vec3_Axis_PZ, 90 * mrot * deg2rad ) * MatrixTranslate( vec4( mshiftx, mshifty, 0, 1 )));
  simpleshader.Disable; }

  mainscene.Render;
//  Set2DMode;

//  text_Draw( fntMain, 0, 0, Format( 'FPS = %d', [ zgl_Get( RENDER_FPS )]));
//  pr2d_Line( 0, 0, p.x, p.y );
//  text_Draw( fntMain, 0, 40, Format( 'Light[0].Rotation=X=%f,Y=%f', [ Lxrot1, Lyrot1 ]));
//  text_Draw( fntMain, 0, 80, Format( 'Light[1].Rotation=X=%f,Y=%f', [ Lxrot2, Lyrot2 ]));
//  text_Draw( fntMain, 0, 120, Format( 'mouse=dX=%d,dY=%d', [ mouse_DX(), mouse_DY() ]));
end;

procedure DrawObjects( Scene: tScene );
var
  i: Integer;
  cell: Integer;
  mdlview: TMat4;
begin
  if ( not Assigned( ActShad )) then
    exit;
//  LightSources.PassToActiveShader( maincamera.Position, 8, Scene.view, [ liPosition ]);


//  glActiveTexture( GL_TEXTURE2 );
//  glBindTexture( GL_TEXTURE_2D, ShadowFB.TexDepth );
//  glUniform1i( ActShad.Uniforms.AddrByName( 'shadow' ), 2 );



  for i:= 0 to LightSources.Count - 1 do
    lightbulb.Render( mat4translate( vec4( LightSources[ i ].Position, 1 )), []);
  cell:= 32;

  terrainshader.Enable;

  glUniform1i( ActShad.Uniforms.AddrByName( 'heightMap' ), 0 );
  glUniform1i( ActShad.Uniforms.AddrByName( 'multilayer' ), 1 );
  glUniform1i( ActShad.Uniforms.AddrByName( 'lightmap' ), 2 );

  glUniform1i( ActShad.Uniforms.AddrByName( 'uvX' ), Round( Int( -maincamera.Position.X / cell )));
  glUniform1i( ActShad.Uniforms.AddrByName( 'uvY' ), Round( Int( -maincamera.Position.Y / cell )));
  glUniform1f( ActShad.Uniforms.AddrByName( 'height' ), 4 );
  glUniform1i( ActShad.Uniforms.AddrByName( 'cellNumber' ), 64 );

  mainwnd.Title:= Format( 'uvX: %f, uvY: %f, mrot: %d, mx: %d, my: %d', [ Int( -maincamera.Position.X / cell ) * cell, Int( maincamera.Position.Y / cell ) * cell, mrot, mshiftx, mshifty ]);
  Mdl.Render(
    mat4translate( vec4(
         Int( maincamera.Position.X / cell ) * cell,
         Int( maincamera.Position.Y / cell ) * cell,
      0, 1 )), RenderFlags );
end;

procedure DrawShadowObjects( Scene: tScene );
var
  i: Integer;
  pass: Integer;
begin
  shadowshader.Enable;
  glCullFace( GL_FRONT );
  glDisable( GL_BLEND );

  glViewport ( 0, 0, ShadowRes, ShadowRes );
  ShadowFB.BindAndClear();
  shadowshader.Enable;

  LightSources.PassToActiveShader( maincamera.Position, 8, Scene.view, [ liPosition ]);
{  for i:= 0 to LightSources.Count - 1 do
    begin
      ShaderSetParameteri( ActShad.ShaderObj, 'light', i );
      for pass:= 0 to 1 do
        begin
          glViewPort( Round(( ShadowRes / 4 ) * pass + ShadowRes / 2 * ( i div 4)),
                      Round( ShadowRes / 4 *( i mod 4 )),
                      ShadowRes div 4, ShadowRes div 4 );
          ShaderSetParameteri( ActShad.ShaderObj, 'renderpass', pass );

//          Mdlsphere.Render( MatrixScale( vec4( vec3( sphere.W ), 1 )) * MatrixTranslate( vec4( vec3( sphere ), 1 )), view, proj, RenderMode );
          Mdl.Render( Mat4Identity, []);
        end;
    end;}
  glViewport ( 0, 0, mainwnd.Width, mainwnd.Height );

  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
  glBindRenderbuffer( GL_RENDERBUFFER, 0 );
  glEnable( GL_BLEND );
  glCullFace( GL_BACK );
end;

procedure DrawSkyBoxObjects( Scene: tScene );
begin
  if ( not Assigned( Scene.Shader )) then
    exit;
  glUniformMatrix3fv( ActShad.Uniforms.AddrByName( 'view'), 1, False, @Scene.Cam.mdlview );
  //ShaderSetParameter4fv( ActiveShader, 'view', mat4( Scene.Cam.mdlview ));
  skybox.Render( Mat4Identity );
end;


procedure ResizeWnd( Sender: TSDLWindow );
begin

end;

{$ENDIF}
