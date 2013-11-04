uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

//varying double len;

void main() {
    //gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
    vec4 p4 = proj * view * world * gl_Vertex;
	//len = length( p4.xyz );
	gl_Position	= p4;
}