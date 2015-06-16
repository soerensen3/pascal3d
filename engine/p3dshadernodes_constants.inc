{$IFDEF VERTEXHEADER}
  {$IFDEF BASESHADER}
'#version 150' + #13#10 +
'uniform mat4 view;' + #13#10 +
'uniform mat4 world;' + #13#10 +
'uniform mat4 proj;' + #13#10 +
'' + #13#10 +
'in vec4 in_vertex;' + #13#10 +
'in vec3 in_normal;' + #13#10 +
'in vec4 in_color;' + #13#10 +
'' + #13#10 +
'out vec4 v_color;' + #13#10 +
'out vec3 v_normal;' + #13#10
    {$UNDEF BASESHADER}
  {$ENDIF}
  {$UNDEF VERTEXHEADER}
{$ENDIF}

{$IFDEF VERTEXCODE}
  {$IFDEF BASESHADER}
'void main() {' + #13#10 +
'  vec3 N = mat3( view * world ) * in_normal;' + #13#10 +
'	 v_color = in_color * ( 0.5 + vec4( vec3( max(dot(N, normalize( vec3( 1.0, 1.0, 1.0))), 0.0)), 1 ) * 0.5 );' + #13#10 +
'	 v_normal = vec3( proj * vec4( N, 1 ));' + #13#10 +
'	 gl_Position = proj * view * world * in_vertex;' + #13#10 +
'}'
    {$UNDEF BASESHADER}
  {$ENDIF}
  {$UNDEF VERTEXCODE}
{$ENDIF}

{$IFDEF FRAGMENTHEADER}
  {$IFDEF BASESHADER}
'#version 150' + #13#10 +
'in vec4 v_color;' + #13#10 +
'out vec4 out_color;' + #13#10
    {$UNDEF BASESHADER}
  {$ENDIF}
  {$UNDEF FRAGMENTHEADER}
{$ENDIF}

{$IFDEF FRAGMENTCODE}
  {$IFDEF BASESHADER}
'void main()' + #13#10 +
'{' + #13#10 + #13#10 +
'    out_color = v_color;' + #13#10 +
'}' + #13#10
   {$UNDEF BASESHADER}
  {$ENDIF}
  {$UNDEF FRAGMENTCODE}
{$ENDIF}

