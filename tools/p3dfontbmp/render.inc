{$IFDEF IMPLEMENTATION}
{ TP3DFontBitmap }

var
  txt: TP3DTextBmp;

procedure TP3DFontBitmap.Render;
var
  i: Integer;
  decl, Uniform: TP3DShaderDecl;
  wh, p1, p2: TVec2;
begin
  glClearColor( 0, 0.5, 1, 1 );
  glClear( GL_COLOR_BUFFER_BIT OR GL_DEPTH_BUFFER_BIT );
  //if ( Canvas.Material.Maps.Count < 1 ) then
  //  Canvas.Material.Maps.Add( TP3DMaterialMapBase.Create( Canvas.Material ));
  //Canvas.Material.Maps[ 0 ].Map:= ActiveFont.Texture;

  P3DFontManagerBmp.TextModifier.Reset;
  P3DFontManagerBmp.TextModifier.TextAlpha:= 1.0;
  Canvas.Lock();
  Canvas.Material.PassToShader();
  ActiveFont.ApplyShader;
  if ( P3DShaderActive.Uniforms.FindByName( 'proj', Uniform )) then
    Uniform.AsMat4:= Canvas.Proj;
  if ( P3DShaderActive.Uniforms.FindByName( 'view', Uniform )) then
    Uniform.AsMat4:= Mat4Identity;
  if ( P3DShaderActive.Uniforms.FindByName( 'world', Uniform )) then
    Uniform.AsMat4:= Canvas.World;
  wh:= vec2( ActiveFont.Texture.Width, ActiveFont.Texture.Height );
  p1:= vec2( 10 );
  p2:= p1 + wh;
  if ( P3DShaderActive.Attributes.FindByName( 'TexCoord0', decl )) then
    begin
      glBegin( GL_TRIANGLE_FAN );

      glVertexAttrib4f( P3DAttribColor, 1, 1, 1, 1 );
      if ( Assigned( decl )) then
        decl.AsVec2:= vec2( 0 );
      glVertexAttrib2f( P3DAttribPosition, p1.X, p1.Y );
//      glVertexAttrib2f( P3DAttribTexCoord0, 1, 0 );

      if ( Assigned( decl )) then
        decl.AsVec2:= vec2( 1, 0 );
      glVertexAttrib2f( P3DAttribPosition, p2.X, p1.Y );
//      glVertexAttrib2f( P3DAttribTexCoord0, 1, 1 );

      if ( Assigned( decl )) then
        decl.AsVec2:= vec2( 1, 1 );
      glVertexAttrib2f( P3DAttribPosition, p2.X, p2.Y );
//      glVertexAttrib2f( P3DAttribTexCoord0, 0, 1 );

      if ( Assigned( decl )) then
        decl.AsVec2:= vec2( 0, 1 );
      glVertexAttrib2f( P3DAttribPosition, p1.X, p2.Y );
      //glVertexAttrib2f( P3DAttribTexCoord0, 0, 0 );
      glEnd();
    end;
  //Canvas.RenderRect( vec2( 0 ), vec2( ActiveFont.Texture.Width, ActiveFont.Texture.Height ), Red100 );
  Canvas.Material.PassToShader();
  Canvas.RenderLineRect( p1, p2, Black );
  for i:= 0 to ActiveFont.Letters.Count - 1 do
    Canvas.RenderLineRect( p1 + ActiveFont.Letters[ i ].uv1 * wh, p1 + ActiveFont.Letters[ i ].uv2 * wh, OrangeA700 );
  Canvas.Font.Name:= ActiveFont.Name;
  Canvas.RenderText( 'Pack my box with five dozen liquor jugs', vec2( p1.x, p2.y ), txt );
  Canvas.Unlock();
end;

{$ENDIF}
