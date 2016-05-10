#if (__VERSION__ > 120)
# define IN in
# define OUT out
#else
# define IN attribute
# define OUT varying
#endif

// get textures
uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D tex2;
uniform sampler2D tex3;
uniform sampler2D tex4;

uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;


//get vertex position
IN vec4 Position;
IN vec4 Normal;
IN vec4 Color;
IN vec4 Cotangent;
IN vec4 Tangent;
IN vec4 TexCoord0;

OUT vec4 vPosition;
OUT vec4 vTexCoord0;
OUT vec4 vNormal;
OUT vec4 vTangent;
OUT vec4 vCotangent;

uniform int uvX;
uniform int uvY;
uniform float height;
uniform int cellNumber;



void main()
{
  vTexCoord0.s = TexCoord0.x-float(uvX)/float(cellNumber);
  vTexCoord0.t = TexCoord0.y+float(uvY)/float(cellNumber);
  float offset = texture2D( tex1, vTexCoord0.st ).a;
  vPosition = Position;
  vPosition.z+= offset * height;
  vPosition = view * world * vPosition;
  vPosition.z-= length( vPosition.xy ) / 10.0;
  vNormal = world * vec4( Normal.xyz, 0 );
  vTangent = world * vec4( Tangent.xyz, 0 );
  vCotangent = world * vec4( Cotangent.xyz, 0 );
  gl_Position = proj * vPosition;
}
