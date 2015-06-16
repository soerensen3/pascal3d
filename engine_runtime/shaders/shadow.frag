#version 130
in vec3 v_normal;
in vec4 v_pos;

out vec4 frag_color;

#pragma include "shaderutils.glsl"

void main(void){
  frag_color = vec4( 0, 0, 0, 1 );
}
