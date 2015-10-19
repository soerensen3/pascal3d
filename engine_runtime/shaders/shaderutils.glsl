uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

struct LightSourceParameters {
	vec4 diffuse;
	vec4 specular;
	vec4 position;
	vec4 halfVector;
//	vec3 spotDirection;
//	float spotExponent;
//	float spotCutoff; // (range: [0.0,90.0], 180.0)
//	float spotCosCutoff; // (range: [1.0,0.0],-1.0)
//	float constantAttenuation;
	float linearAttenuation;
	float quadraticAttenuation;
};
uniform int numLightSource;
uniform LightSourceParameters LightSource[8];
uniform vec4 eyeVec;
uniform vec4 eyePos;
uniform vec4 world_ambient;

uniform vec4 mat_diffuse;
uniform vec4 mat_specular;
uniform float mat_hardness;

struct LightModelParameters {

	vec4 ambient;
};

uniform LightModelParameters LightModel;




float calculateAttenuation(in int i, in float dist)

{

    return(1.0 / (max( 0.01, //LightSource[i].constantAttenuation + //useless

                  LightSource[i].linearAttenuation * dist +

                  LightSource[i].quadraticAttenuation * dist * dist)));

}



void util_PointLight( in int i, in float shininess,                  
                      in vec3 V, in vec3 N, inout vec4 diffuse, inout vec4 specular)

{
	  vec3 s = (LightSource[ i ].position.xyz - V);
	  float dist = length( s );
	  s = normalize( s );
	  vec3 v = normalize(-V.xyz);
	  vec3 r = reflect( -s, N );
	  float attenuation = calculateAttenuation(i, dist);
	  float sDotN = dot(s,N);

	  if( sDotN > -0.05 ) //soft edges
		  specular+= clamp( 
		  vec4( LightSource[ i ].specular.rgb * LightSource[ i ].specular.a, 1 ) *
					 pow( dot(r,v), shininess ) * attenuation, 0.0, 1.0 );

	  diffuse += LightSource[ i ].diffuse * sDotN * attenuation;
}

vec4 CalcBumpedNormal(vec4 Normal,vec4 Tangent, vec4 Bitangent, vec4 BumpMapNormal)
{
    BumpMapNormal = vec4( 2.0 * BumpMapNormal.xyz - vec3(1.0, 1.0, 1.0), 1 );
    vec4 NewNormal;
    mat3 TBN = mat3(Tangent.xyz, Bitangent.xyz, Normal.xyz);
    NewNormal = vec4( TBN * BumpMapNormal.xyz, 1 );
    NewNormal = normalize(NewNormal);
    return NewNormal;
}

/*
vec2 DoubleToVec2( double val ){
  vec2 valv;
  valv.x = val;
  valv.y = val - val.x;
  return valv;
}

double Vec2ToDouble( vec2 val ){
  return val.x + val.y;
}
*/
 
