#version 150
uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

in vec3 vertex;
in vec3 normal;
in vec4 color;

out vec4 f_color;

void main() {
//    gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
    vec3 N = mat3( view * world ) * normal;
	vec4 diff = ( 0.5 + vec4( vec3( max( dot( N, normalize( vec3( 1.0 ))), 0.0 )), 1 ) * 0.5 );

    gl_Position = proj * view * world * vec4( vertex, 1 );
	f_color = color * diff;
}
