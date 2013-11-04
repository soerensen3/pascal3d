#version 120
uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

void main() {
    gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
    gl_TexCoord[1] = gl_TextureMatrix[1] * gl_MultiTexCoord1;
    gl_Position = proj * view * world * gl_Vertex;
	vec3 normal = mat3( view * world ) * gl_Normal;
	gl_FrontColor = gl_Color * ( 0.5 + vec4( vec3( max(dot(normal, normalize( vec3( 1.0, 1.0, 1.0))), 0.0)), 1 ) * 0.5 );
}