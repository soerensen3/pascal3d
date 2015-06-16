#version 330
uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

layout ( location = 0 ) in vec3 vposition;
layout ( location = 3 ) in vec4 vcolor;

out vec4 fcolor;

void main() {
  fcolor = vcolor;
  gl_Position = proj * view * world * vec4( vposition, 1 );
}
