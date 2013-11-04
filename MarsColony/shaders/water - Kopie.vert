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
	vec3 wave = normal(gl_TexCoord[0].st, vec2(10.0, 10.0));
	//vec3 wave = normal(gl_TexCoord[0].st / 50.0, vec2(513.0, 416.0)) * 10.0;
	//wave += normal(gl_TexCoord[0].st / 7.0, vec2(413.0, 526.0));
	//wave += normal(gl_TexCoord[0].st / 23.0, vec2(50.0, 50.0));
	wave = normalize( wave );// * 2 - vec3( 0, 1, 0 );
	//foam = clamp( abs( wave.x+wave.z ), 0, 1);
	foam = 0;//*= pow( foam, 8);

	wave.y *= waveHeight;
	wave.xz *= horizwave;
	Normal = normalize( wave );
	worldPos = vec3( view * ( world * gl_Vertex + vec4( wave /*- vec3( 0, 0.5, 0 )*/, 1 ))); 
	//worldPos+= wave;
	gl_Position = proj * vec4( worldPos, 1);
}


/*
s = windDir * a + windDir_O * b
1 = windDir
a = 
*/