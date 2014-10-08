#version 150
uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;
in vec4 vertex;
in vec4 normal;
in vec4 color;
out vec4 out_color;
out vec4 out_vertex;
out vec4 out_normal;

void main() {
  gl_Position = proj * view * world * vertex;
	 vec4 out_normal = vec4( mat3( view * world ) * normal.xyz, 1 );
	 out_color = color * ( 0.5 + vec4( vec3( max( dot( out_normal.xyz, normalize( vec3( 1.0, 1.0, 1.0 ))), 0.0 )), 1 ) * 0.5 );
	 out_normal = proj * out_normal;
}