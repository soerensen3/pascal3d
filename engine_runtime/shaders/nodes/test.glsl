//FONT SHADER INPUT
<input>
  attribute vec4 in_vertex
  attribute vec4 in_color
  attribute vec2 in_texc0
  uniform   mat4 mat
</input>

//TRANSFORM.xml
<node name="transform">
  <input>
    variable vec4 vec
    variable mat4 mat
  </input>
  <output/ type="vec4">
  <inline src="vec*mat"/>
</node>
<node name="assignment">
  <input>
    variable v1
    variable v2
  </input>
  <inline src="v1=v2;"/>
</node>

//BASE VSHADER
<input>
  attribute vec4 in_vertex
</input>

#version 330

{PREDEFINES}

void main() {
  {calculations} // maybe not necessary
  gl_Position = {$in_vertex};
}

//BASE FSHADER
<input>
  attribute vec4 in_color
</input>
<output>
  attribute vec4 FragColor
</output>

//inline
#version 330

{PREDEFINES}

out vec4 FragColor;

void main()
{
    {for all output}
    {assignment};
}
