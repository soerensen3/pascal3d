#version 120
#if (__VERSION__ > 120)
# define IN in
# define OUT out
#else
# define IN varying
# define OUT  
#endif

uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D tex2;
uniform sampler2D tex3;
uniform sampler2D tex4;

IN vec4 vNormal;
IN vec4 vColor;
IN vec4 vTangent;
IN vec4 vCotangent;
IN vec4 vPosition;
IN vec3 cameye;
IN vec4 vTexCoord0;

uniform int cellNumber;

OUT vec4 FragColor;

vec3 viewDir;

const int p3dltPoint = 0;
const int p3dltSun = 1;
const int p3dltSpot = 2;

const vec4 mat_specular = vec4( 1 );//vec4( 0.020958, 0.020958, 0.020958, 1 );
const float mat_hardness = 5.000000;

struct LightSourceParameters {
  vec4 color;
  vec4 position;
  vec4 halfvector;
  vec4 direction;
  int type;

  float linearAttenuation;
  float quadraticAttenuation;
};
uniform int numLightSource;
uniform LightSourceParameters LightSource[8];

uniform mat4 view;
uniform mat4 mnormal;

#define saturate(a) clamp( a, 0.0, 1.0 )

vec3 F_Schlick( in vec3 specularColor, in float dotLH ) {

  // Original approximation by Christophe Schlick '94
  //;float fresnel = pow( 1.0 - dotLH, 5.0 );

  // Optimized variant (presented by Epic at SIGGRAPH '13)
  float fresnel = exp2( ( -5.55437 * dotLH - 6.98316 ) * dotLH );

  return ( 1.0 - specularColor ) * fresnel + specularColor;

}

float G_BlinnPhong_Implicit( /* in float dotNL, in float dotNV */ ) {

  // geometry term is (n⋅l)(n⋅v) / 4(n⋅l)(n⋅v)

  return 0.25;

}

float D_BlinnPhong( in float shininess, in float dotNH ) {

  // factor of 1/PI in distribution term omitted

  return ( shininess * 0.5 + 1.0 ) * pow( dotNH, shininess );

}

vec3 BRDF_BlinnPhong( in vec3 specularColor, in float shininess, in vec3 normal, in vec3 lightDir, in vec3 viewDir ) {

  vec3 halfDir = normalize( lightDir + viewDir );

  //float dotNL = saturate( dot( normal, lightDir ) );
  //float dotNV = saturate( dot( normal, viewDir ) );
  float dotNH = saturate( dot( normal, halfDir ) );
  float dotLH = saturate( dot( lightDir, halfDir ) );

  vec3 F = F_Schlick( specularColor, dotLH );

  float G = G_BlinnPhong_Implicit( /* dotNL, dotNV */ );

  float D = D_BlinnPhong( shininess, dotNH );

  return F * G * D;

}

void util_PointLight( in int i, in float shininess,                  
                      in vec3 N, inout vec3 diffuse, inout vec3 specular )

{
    vec3 lVector = ( LightSource[ i ].position.xyz - vPosition.xyz );
    vec3 lightDir = normalize( lVector );

    // attenuation

    float attenuation = 1.0;//calcLightAttenuation( length( lVector ), 1, 0.1 ); //pointLightDistance[ i ], pointLightDecay[ i ] );

    // diffuse

    float cosineTerm = saturate( dot( N, lightDir ) );

    diffuse += LightSource[ i ].color.rgb * attenuation * cosineTerm;

    // specular

    vec3 brdf = BRDF_BlinnPhong( vec3( 1 ), shininess, N, lightDir, viewDir );

    specular += brdf * LightSource[ i ].color.rgb * attenuation * cosineTerm;
}

void util_directionalLight( in int i,
                            in float shininess,
                            in vec3 N,
                            inout vec3 diffuse,
                            inout vec3 specular )
{
    // diffuse

    float cosineTerm = saturate( dot( N, LightSource[ i ].direction.xyz ));

    diffuse += LightSource[ i ].color.rgb * cosineTerm;

    // specular

    vec3 brdf = BRDF_BlinnPhong( vec3( 1 ), shininess, N, LightSource[ i ].direction.xyz, viewDir );

    specular += brdf * LightSource[ i ].color.rgb * cosineTerm;
}

vec4 CalcBumpedNormal(vec4 Normal,vec4 Tangent, vec4 Cotangent, vec3 BumpMapNormal)
{
    vec4 NewNormal;
    mat3 TBN = mat3(Tangent.xyz, Cotangent.xyz, Normal.xyz);
    NewNormal = vec4( TBN * BumpMapNormal, 1.0 );
    NewNormal = normalize(NewNormal);
    return NewNormal;
}

const float fogstart = 50.0;
const float fogend = 1400.0;
const float fogend2 = 2000.0;
const vec3 fogcolor = vec3( 0.165, 0.129, 0.11 );
const vec3 fogcolor2 = vec3( 0.38, 0.318, 0.318 );


void main()
{
  vec4 color = texture2D( tex0, vTexCoord0.st ).rgba;
  vec3 Normal = texture2D( tex1, vTexCoord0.st ).rgb;
  vec3 detail_diff = texture2D( tex2, vTexCoord0.st * float( cellNumber )).rgb;
  vec3 detail_norm = texture2D( tex3, vTexCoord0.st * float( cellNumber )).rgb;
  vec3 detail_spec = texture2D( tex4, vTexCoord0.st * float( cellNumber )).rgb;
  vec3 detail_diff2 = texture2D( tex2, vTexCoord0.ts * float( cellNumber ) / 3.7 ).rgb;
  vec3 detail_norm2 = texture2D( tex3, vTexCoord0.ts * float( cellNumber ) / 3.7 ).rgb;
  vec3 detail_spec2 = texture2D( tex4, vTexCoord0.ts * float( cellNumber ) / 3.7 ).rgb;
  vec3 detail_diff3 = texture2D( tex2, vTexCoord0.ts * float( cellNumber ) * 7.45 ).rgb;
  vec3 detail_norm3 = texture2D( tex3, vTexCoord0.ts * float( cellNumber ) * 7.45 ).rgb;
  vec3 detail_spec3 = texture2D( tex4, vTexCoord0.ts * float( cellNumber ) * 7.45 ).rgb;
  Normal = mix( detail_norm, Normal, 0.5 );
  Normal = mix( detail_norm2, Normal, 0.7 );
  Normal = mix( detail_norm3, Normal, 0.4 );
  Normal = Normal * 2.0 - 1.0;
  //normalize( Normal );
  Normal = CalcBumpedNormal( vNormal, vTangent, vCotangent, Normal ).xyz;
  //Normal = vNormal.xyz;

  vec3 shadow = vec3( 0 );
  vec3 spec = vec3( 0 );
  viewDir = normalize( -vPosition.xyz );
  for ( int i=0; i < numLightSource; i++ ) {
    if ( LightSource[ i ].type == p3dltPoint )
      util_PointLight( i, mat_hardness, Normal.xyz, shadow, spec );
    if ( LightSource[ i ].type == p3dltSun )
      util_directionalLight( i, mat_hardness, Normal.xyz, shadow, spec );
  }
  shadow*= color.a;
  spec = color.a * spec * mix( mix( vec3( 1 ), detail_spec, 0.3 ), detail_spec2, 0.4 );
  FragColor.rgb = mix( color.rgb, detail_diff*detail_diff2*detail_diff3, 0.4 );//mix( color, mix( detail_diff, detail_diff2, 0.6 ), 0.3 );
  FragColor.rgb = FragColor.rgb * shadow + spec;
  FragColor.rgb = mix( FragColor.rgb, mix( fogcolor, fogcolor2, smoothstep( fogend, fogend2, length( vPosition ))), smoothstep( fogstart, fogend, length( vPosition )));

  //FragColor.rgb = vec3( dot( Normal.xyz, LightSource[ 0 ].direction.xyz ));
  FragColor.a = 1;
  #if (__VERSION__ < 130)
  gl_FragColor = FragColor;
  #endif
}
