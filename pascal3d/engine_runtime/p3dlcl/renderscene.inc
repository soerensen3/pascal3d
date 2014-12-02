{$IFDEF INTERFACE}
procedure DrawObjects( Scene: tScene ); forward;
{$UNDEF INTERFACE}
{$ELSE}

procedure Render( Sender: TSDLWindow );
var
  p: TVec4;
begin
  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  mainscene.Render;
end;

procedure DrawObjects( Scene: tScene );
  function RenderLetter( fnt: TP3DFont; p: TVec2; col: TVec4; idx: Integer ): TVec2;
  var
    cl: GLint;
    vt: GLint;
    tc: GLint;
    p2: TVec2;
    tx: GLint;
    m: TMat4;
  begin
    font_shader.Enable;
    cl:= font_shader.Attributes.AddrByName( 'in_color' );
    vt:= font_shader.Attributes.AddrByName( 'in_vertex' );
//    cl:= font_shader.Attributes.AddrByName( 'color' );
//    vt:= font_shader.Attributes.AddrByName( 'vertex' );
    tc:= font_shader.Attributes.AddrByName( 'in_texc0' );
    tx:= font_shader.Uniforms.AddrByName( 'tex0' );

    p2:= p + ( fnt.Letters[ idx ].uv2 - fnt.Letters[ idx ].uv1 ) * 512;

    glActiveTexture( GL_TEXTURE0 );
    glEnable( GL_TEXTURE_2D );
    glBindTexture( GL_TEXTURE_2D, fnt.Letters[ idx ].Texture.fGLTexture );

    m:= p3dgeometry.proj2D;
    glUniformMatrix4fv( LineShader2D.Uniforms.AddrByName( 'mat' ), 1, False, @m );
    glUniform1i( tx, 0 );
    glVertexAttrib4f( cl, Col.R, Col.G, Col.B, Col.A );
    glBegin( GL_TRIANGLE_FAN );
    glVertexAttrib2f( tc, fnt.Letters[ idx ].uv1.x, fnt.Letters[ idx ].uv1.y );
    glVertexAttrib2f( vt, p.x, p.y );
    glVertexAttrib2f( tc, fnt.Letters[ idx ].uv2.x, fnt.Letters[ idx ].uv1.y );
    glVertexAttrib2f( vt, p2.X, p.Y );
    glVertexAttrib2f( tc, fnt.Letters[ idx ].uv2.x, fnt.Letters[ idx ].uv2.y );
    glVertexAttrib2f( vt, p2.X, p2.Y );
    glVertexAttrib2f( tc, fnt.Letters[ idx ].uv1.x, fnt.Letters[ idx ].uv2.y );
    glVertexAttrib2f( vt, p.X, p2.Y );
    glEnd();
    Result:= p2;
  end;

  function Test( fnt: TP3DFont ): TVec2;
  var
    cl: GLint;
    vt: GLint;
    tc: GLint;
    p2: TVec2;
    tx: GLint;
    p: TVec2;
    mat: GLint;
    m: TMat4;
    mt: TMat4;
  begin
    font_shader.Enable;
    cl:= font_shader.Attributes.AddrByName( 'in_color' );
    vt:= font_shader.Attributes.AddrByName( 'in_vertex' );
//    cl:= font_shader.Attributes.AddrByName( 'color' );
//    vt:= font_shader.Attributes.AddrByName( 'vertex' );

    tc:= font_shader.Attributes.AddrByName( 'in_texc0' );
    tx:= font_shader.Uniforms.AddrByName( 'tex0' );
    mat:= font_shader.Uniforms.AddrByName( 'mat' );

    p:= vec2( sin( SDL_GetTicks div 100 ) * 100, cos( SDL_GetTicks div 200 ) * 100);

    glActiveTexture( GL_TEXTURE0 );
    glEnable( GL_TEXTURE_2D );
    glBindTexture( GL_TEXTURE_2D, fnt.Textures[ 0 ].fGLTexture );

    mt:= mat4translate( vec4( p, 0, 1 ));
    m:= mt * p3dgeometry.proj2D;
    glUniform1i( tx, 0 );
    glUniformMatrix4fv( LineShader2D.Uniforms.AddrByName( 'mat' ), 1, False, @m );
    glVertexAttrib4f( cl, 1, 1, 1, 1 );
    txt.Vertices.Bind();
    txt.Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
    glDrawElements( GL_TRIANGLES, txt.Indices.Count, GL_UNSIGNED_INT, Pointer( 0 ));
    {glBegin( GL_TRIANGLE_FAN );
    glVertexAttrib2f( tc, 0, 0 );
    glVertexAttrib2f( vt, p.x, p.y );
    glVertexAttrib2f( tc, 1, 0 );
    glVertexAttrib2f( vt, p2.X, p.Y );
    glVertexAttrib2f( tc, 1, 1 );
    glVertexAttrib2f( vt, p2.X, p2.Y );
    glVertexAttrib2f( tc, 0, 1 );
    glVertexAttrib2f( vt, p.X, p2.Y );
    glVertexAttr
    glEnd();}
    Result:= p2;
  end;

var
  i: Integer;
  x: Float;
begin
  if ( not Assigned( ActShad )) then
    exit;
  glDisable( GL_DEPTH_TEST );
  glDisable( GL_CULL_FACE );
  MainWindow.Title:= Format( 'Mouse X: %d Y: %d DX: %d DY: %d [%d,%d,%d,%d,%d], D[%d,%d,%d,%d,%d]', [ InputManager.Mouse.X, InputManager.Mouse.Y,
                                                                  InputManager.Mouse.DX, InputManager.Mouse.DY,
                                                                  Ord( InputManager.Mouse.Buttons[ 0 ]), Ord( InputManager.Mouse.Buttons[ 1 ]), Ord( InputManager.Mouse.Buttons[ 2 ]),
                                                                  Ord( InputManager.Mouse.Buttons[ 3 ]), Ord( InputManager.Mouse.Buttons[ 4 ]),
                                                                  Ord( InputManager.Mouse.DButtons[ 0 ]), Ord( InputManager.Mouse.DButtons[ 1 ]), Ord( InputManager.Mouse.DButtons[ 2 ]),
                                                                  Ord( InputManager.Mouse.DButtons[ 3 ]), Ord( InputManager.Mouse.DButtons[ 4 ])]);

  GUIManager.Input;
  GUIManager.Render;

  x:= 0;
  //for i:= 0 to dejavu.Letters.Count - 1 do
  //  x:= RenderLetter( dejavu, vec2( x + 5, 10 ), vec4( 1 ), i ).x;

  //RenderLetter( dejavu, vec2( 10, 100 ), vec4( 1 ), ( SDL_GetTicks div 700 ) mod dejavu.Letters.Count );
  //Test( dejavu );
  //Test( dejavu );
  //p3dgeometry.RenderLine2D( vec2( 10, 10 ), vec2( 200, 200 ), vec4( 1, 0, 0, 1 ), vec4( 0, 1, 0, 1 ));
  //p3dgeometry.RenderQuad2D( vec2( 0, 0 ), vec2( 100, 20 ), vec2( 40, 110 ), vec2( 120, 150 ), vec4( 1, 0, 0, 1 ), vec4( 0, 1, 0, 1 ), vec4( 1, 0, 1, 1 ), vec4( 1, 1, 1, 1 ));
  InputManager.NextCycle;
  //DRAW CODE HERE
end;


procedure ResizeWnd( Sender: TSDLWindow );
begin
  Setup2D( Sender.Width, Sender.Height );
end;

{$ENDIF}
