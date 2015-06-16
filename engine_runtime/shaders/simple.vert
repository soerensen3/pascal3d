#version 330
uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

layout ( location = 0 ) in vec3 vertex;
layout ( location = 1 ) in vec3 normal;
layout ( location = 4 ) in vec2 texcoord;
in vec4 in_color;

out vec2 tc;
out vec4 f_color;

void main() {
//    gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
    vec3 N = mat3( world ) * normal;
	vec4 diff = min( vec4( 1 ), vec4(( vec3( 0.5 ) + vec3( max( dot( N, normalize( vec3( 1.0 ))), 0.0 ) * 0.5 )), 1 ));

	vec4 v = vec4( vertex, 1 ); 
    gl_Position = proj * view * world * v;//vec4( vertex, 1 );
	f_color = /*in_color */ diff;
	tc = texcoord;
}
