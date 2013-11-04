#version 330

in vec3 normal_vector;
in vec2 tex_coord;
uniform mat4 View;
uniform mat4 Model;
uniform sampler2D surface;
out vec4 fragColor;

void main (void) {
  fragColor = /*vec4( normal_vector, 1 );/*/texture( surface, tex_coord );
}