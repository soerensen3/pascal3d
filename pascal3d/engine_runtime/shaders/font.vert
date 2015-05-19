#version 330
uniform mat4 mat;
//uniform sampler2D tex0;

layout ( location = 0 ) in vec4 in_vertex;
layout ( location = 3 ) in vec4 in_color;
layout ( location = 8 ) in vec2 in_texc0;

out vec4 out_color;
out vec2 out_texc0;

void main() {
  out_color = in_color;
  out_texc0 = in_texc0;
  gl_Position = mat * in_vertex;
}
