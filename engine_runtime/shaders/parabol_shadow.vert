#version 330
uniform int renderpass;
uniform int light;

in  vec4 in_pos;
in  vec3 in_normal;
out vec4 v_pos;

#pragma include "shaderutils.glsl"
 
void main(void){
 
	gl_Position = view * world * in_pos - LightSource[light].position;
 
	float L= length (gl_Position.xyz);
	gl_Position /= -L;
	if (renderpass == 1) gl_Position.z *=-1.0;
	gl_Position.z += 1.0;
	gl_Position.xy /= gl_Position.z;
	if (gl_Position.z >= 0.01){
		gl_Position.z = L / 50.0;//Todo: optimieren
		gl_Position.w = 1.0;
		}
	else{
		gl_Position.z = -1.0;
		gl_Position.w = -1.0;
		}
 
    gl_Position.z = 2.0 * gl_Position.z -1.0; //Todo: optimieren
	v_pos=gl_Position;
	
	//TWO SHADOW MAPS IN ONE PASS
//	gl_Position.y/=2;
//	gl_Position.y+=0.5;	
}
