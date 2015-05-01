#version 330
uniform mat4 proj;

layout ( location = 0 ) in vec4 vertex;
layout ( location = 1 ) in vec4 color;

out vec4 out_color;

void main() {
  out_color = color;
  gl_Position = proj * vertex;
}
