#version 330
layout ( location = 0 ) in vec3 in_vertex;
layout ( location = 1 ) in vec3 in_normal;

out vec4 v_color;
out vec3 v_normal;
out vec4 v_pos;
//out vec2 v_texc0;

#pragma include "shaderutils.glsl"

void main(void){
    #pragma include "shadow.glsl" 
	
	gl_Position.xy = pos;
	gl_Position.z = -vertex.y; // Der Bereich von +-1 über der Referenzebene wird erfasst
//	v_texc0 = gl_MultiTexCoord0;
}
