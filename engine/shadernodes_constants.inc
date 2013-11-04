{
uniform
  mat4 $view
  mat4 $world
  mat4 $proj
in
  vec4 $in_position[gl_Vertex]
  vec4 $in_color[gl_Color]
  vec4 $in_normal
out
  vec4 $out_position
  vec4 $out_color
  vec4 $out_normal
}

{$IFDEF FRAGMENTHEADER}
  {$IFDEF DEFAULTSHADER}
'#version 150' + #13#10 +
'uniform mat4 view;' + #13#10 +
'uniform mat4 world;' + #13#10 +
'uniform mat4 proj;' + #13#10 +
'in vec4 out_color;' + #13#10 +
'out vec4 color;' + #13#10
    {$UNDEF DEFAULTSHADER}
  {$ENDIF}
  {$UNDEF FRAGMENTHEADER}
{$ENDIF}
{$IFDEF FRAGMENTCODE}
  {$IFDEF DEFAULTSHADER}
'void main()' + #13#10 +
'{' + #13#10 +
'    color = vec4( 1 );' + #13#10 + //out_color;' + #13#10 +
'}' + #13#10
   {$UNDEF DEFAULTSHADER}
  {$ENDIF}
  {$UNDEF FRAGMENTCODE}
{$ENDIF}

{$IFDEF VERTEXHEADER}
  {$IFDEF DEFAULTSHADER}
'#version 150' + #13#10 +
'uniform mat4 view;' + #13#10 +
'uniform mat4 world;' + #13#10 +
'uniform mat4 proj;' + #13#10 +
'in vec4 in_vertex;' + #13#10 +
'in vec3 in_normal;' + #13#10 +
'in vec4 in_color;' + #13#10 +
'out vec4 out_color;' + #13#10 +
'out vec4 out_vertex;' + #13#10 +
'out vec3 out_normal;' + #13#10
    {$UNDEF DEFAULTSHADER}
  {$ENDIF}
  {$UNDEF VERTEXHEADER}
{$ENDIF}
{
uniform
  mat4 $view
  mat4 $world
  mat4 $proj
out
  vec4 $out_color

}
{$IFDEF VERTEXCODE}
  {$IFDEF DEFAULTSHADER}
'void main() {' + #13#10 +
'  out_vertex = proj * view * world * in_vertex;' + #13#10 +
'	 vec3 out_normal = mat3( view * world ) * in_normal;' + #13#10 +
'	 out_color = in_color * ( 0.5 + vec4( vec3( max(dot(out_normal, normalize( vec3( 1.0, 1.0, 1.0))), 0.0)), 1 ) * 0.5 );' + #13#10 +
'	 out_normal = vec3( proj * vec4( out_normal, 1 ));' + #13#10 +
'}'
    {$UNDEF DEFAULTSHADER}
  {$ENDIF}
  {$UNDEF VERTEXCODE}
{$ENDIF}

