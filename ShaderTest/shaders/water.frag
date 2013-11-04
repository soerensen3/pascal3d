#version 120
uniform sampler2D wavemap;
uniform sampler2D skymap;
 
uniform float exponent = 40.0;
uniform vec3 emission = vec3(0.1, 0.15, 0.12);
uniform vec3 reflection = vec3(0.65, 0.65, 0.65);
 
uniform vec3 camPos;
 
//varying vec2 texCoord;
varying vec3 worldPos;

uniform vec3 sunDir;// = normalize( vec3(0,1,0));// * gl_ModelView;
uniform float worldTime;
 
const float PI = 3.1415926535;
const vec3 Y = vec3(0.0, 1.0, 0.0);
 
vec3 normal(vec2 coord, vec2 time)
{
  return texture2D(wavemap, coord + worldTime / time).xzy - vec3(0.5);
}
 
void main()
{
  vec3 N = vec3(0.0);
 
  vec2 texCoord = gl_TexCoord[0].st;
  // Idea stolen from Source 2007 (thanks!)
  vec2 texCoord2 = vec2(texCoord.x + texCoord.y, texCoord.y - texCoord.x);
 
  // Here be magic numbers.  Don't try to make too much sense of them; they're
  // not calculated from anything but rather the result of much tweaking.
  // There are two kinds:
  //  * The texture coordinate scaling, controlling wave size
  //  * The time scale, controlling wave speed (also influenced by wave size)
  N += normal(texCoord * 5.0, vec2(200.0, 166.0));
  N += normal(texCoord * 31.0, vec2(94.0, 70.0));
  N += normal(texCoord2 * 67.0, vec2(56.0, 34.0));
  N += normal(texCoord * 203.0, vec2(100.0, 20.0));
  N += normal(texCoord2 * 123.0, vec2(60.0, 57.0));
 
  N = normalize(N);
  vec3 V = normalize(camPos - worldPos);
  vec3 R = reflect(-sunDir, N);
 
  // This is far from accurate, but it still looks fairly convincing
  float fresnel = pow(1.0 - dot(N, V), 2.0);
 
  // We fake HDR sunlight for now, until the actual sky is operational
  vec3 fakeSun = vec3(7.0) * pow(clamp(dot(R, V), 0.0, 1.0), exponent);
  vec3 reflected = fakeSun + texture2D(skymap, vec2( 1.0 - dot(V, Y), 0)).rgb;
 
  //fresnel = ((fresnel - 0.5f) * 2) + 0.5f - 0.5f; //- 1.0f;
  vec3 C = mix(emission, reflected * reflection, fresnel);
  //C.rgb = ((C.rgb - 0.5f) * 2) + 0.5f + 0.5f;
 
  gl_FragColor.rgba = vec4(C, 1/*fresnel*/);
}
