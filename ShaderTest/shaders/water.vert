varying vec3 worldPos;
varying vec2 texCoord;
uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

void main()
{
	gl_TexCoord[0] = gl_MultiTexCoord0;
	worldPos = vec3(view * world * gl_Vertex);
	gl_Position = proj * view * world * gl_Vertex;
}