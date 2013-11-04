varying vec3 worldPos;
varying vec3 Normal;
varying float foam;
varying vec2 texCoord;
uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;
//uniform float waveHeight;
uniform float waveAmpl;
uniform vec3 windDir;
uniform float worldTime;

const float wavespeed = 0.1;
const float waveHeight = 20.0;
const float horizwave = 4.0;
uniform sampler2D wavemap;

vec3 normal(vec2 coord, vec2 time)
{
  return texture2D(wavemap, coord + worldTime / time).xzy;// - vec3(0.5);
}

void main()
{
	gl_TexCoord[0] = gl_MultiTexCoord0;
	worldPos = vec3( view * ( world * gl_Vertex ));
	gl_Position = proj * vec4( worldPos, 1 );
}


/*
s = windDir * a + windDir_O * b
1 = windDir
a = 
*/