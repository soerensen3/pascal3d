void util_calc_tangent_binormal( in vec3 normal, out vec3 tangent, out vec3 binormal )
{
	vec3 c1 = cross(normal, vec3(0.0, 0.0, 1.0)); 
	vec3 c2 = cross(normal, vec3(0.0, 1.0, 0.0)); 
	
	if(length(c1)>length(c2))
	{
		tangent = c1;	
	}
	else
	{
		tangent = c2;	
	}
	
	tangent = normalize(tangent);
	
	binormal = cross(normal, tangent); 
	binormal = normalize(binormal);
}

vec4 util_diff_light( in vec3 N, in vec3 light_dir ) {
  return vec4( vec3( max( dot( N, normalize( light_dir )), 0.0 )), 1 );
}

void util_directionalLight( in int i,
                            in vec3 normal,
			    in float shininess,
                            inout    vec4 ambient,
                            inout    vec4 diffuse,
                            inout    vec4 specular )
{
    float nDotVP;   // normal . light direction
    float nDotHV;   // normal . light half vector
    float pf;   // power factor
    
    nDotVP = clamp( dot( normal,
                    normalize( vec3( -LightSource[ i ].position ))), 0.0, 1.0 );
    nDotHV = clamp( dot( normal, vec3( -LightSource[ i ].halfVector )), 0.0, 1.0 );
    
    //if ( nDotVP <= 0.0 )
    //    pf = 0.0;
    //else
    //    pf = pow( nDotHV, shininess);//FrontMaterial.shininess );
    pf = pow( nDotHV, shininess );
    
//    ambient += LightSource[ i ].ambient;
    //diffuse += LightSource[ i ].diffuse * nDotVP;
    specular += LightSource[ i ].specular * pf;    
 }
/*
void util_PointLight( const in int i,
                      const in vec3 eye,
                      const in vec3 ecPosition3,
                      const in vec3 normal,
                      inout    vec4 ambient,
                      inout    vec4 diffuse,
                      inout    vec4 specular )
{                      
    float nDotVP;   // normal . light direction
    float nDotHV;   // normal . light half vector
    float pf;   // power factor
    float attenuation;
    float d;
    vec3  VP;
    vec3  halfVector;
    
    VP = vec3( LightSource[ i ].position ) - ecPosition3;
    
    d= length( VP );
    
    attenuation = 1.0 / ( LightSource[ i ].constantAttenuation +
                          LightSource[ i ].linearAttenuation * d +
                          LightSource[ i ].quadraticAttenuation * d * d );
    
    halfVector = normalize( VP + eye );
    
    nDotVP = max( 0.0, dot( normal, VP ));
    nDotHV = max( 0.0, dot( normal, halfVector ));
    
    if ( nDotVP == 0.0 )
        pf = 0.0;
    else
        pf = pow( nDotHV, 1);//FrontMaterial.shininess );
    
    ambient += LightSource[ i ].ambient * attenuation;
    diffuse += LightSource[ i ].diffuse * nDotVP * attenuation;
    specular += LightSource[ i ].specular * pf* attenuation; 
}
*/
/*
void util_directionalLight(in int i, in vec3 N, in float shininess,
                           inout vec4 ambient, inout vec4 diffuse, inout vec4 specular)
{
    vec3 L = normalize(LightSource[i].position.xyz);
  
    float nDotL = dot(N, L);
  
    if (nDotL > 0.0)
    {   
        vec3 H = LightSource[i].halfVector.xyz;    

        float pf = pow(max(dot(N,H), 0.0), shininess);

        diffuse  += LightSource[i].diffuse  * nDotL;
        specular += LightSource[i].specular * pf;
    }
   
    ambient  += LightSource[i].ambient;
}

*/

