varying vec3 worldPos;
varying vec2 texCoord;
uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

float curviness = 0.1;

void main()
{
	gl_TexCoord[0] = gl_MultiTexCoord0;
	worldPos = vec3(view * world * gl_Vertex);
	worldPos.y -= sqrt( pow( worldPos.x, 2 ) + pow( worldPos.y, 2 )+ pow( worldPos.z, 2 )) * curviness;
	
	gl_Position = proj * vec4( worldPos, 1 );
}