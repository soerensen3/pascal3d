uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;
uniform sampler2D tex0;

attribute vec4 in_vertex;
attribute vec3 in_normal;
attribute vec4 in_color;
attribute vec2 in_texc0;

varying vec4 v_color;
varying vec2 v_texc0;

void main() {
//    gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
    vec3 N = mat3( view * world ) * in_normal;
	vec4 diff = ( 0.5 + vec4( vec3( max( dot( N, normalize( vec3( 1.0 ))), 0.0 )), 1 ) * 0.5 );

    gl_Position = proj * view * world * in_vertex;
	v_color = in_color * diff;
	v_texc0 = vec4( in_texc0, 0.0, 1.0 );
}
