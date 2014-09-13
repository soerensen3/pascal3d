#version 130
uniform sampler2D tex0;
uniform sampler2D tex1;
uniform sampler2D shadow;

uniform int numMaps;

in vec4 v_color;
in vec2 v_texc0;
in vec3 v_normal;
in vec4 v_pos;
in vec3 v_tangent;
in vec3 v_binormal;

//in vec3 spos;

out vec4 frag_color;

#pragma include "shaderutils.glsl"

//  color_map += texture2D( tex, v_texc[ v_texcidx ]);
//  normal_map += texture2D( tex, v_texc[ v_texcidx ]) * 2.0 - 1.0;

vec2 texofset[8];

void main()
{
//    vec4 ambient = vec4( 0, 0, 0, 1 );
    vec4 diffuse = vec4( 0, 0, 0, 0 );
    vec4 specular = vec4( 0, 0, 0, 0 );
    
    vec4 color_map = vec4( 1 );
    vec4 normal_map = vec4( 0 );
  
//    if ( numMaps > 0 )
//      color_map = texture( tex0, v_texc0.st );
   
    if ( numMaps > 0 )
      normal_map = texture( tex0, v_pos.xy ) * 2.0 - 1.0;
//    if ( numMaps > 1 )
//      color_map = texture( tex1, v_texc0.st );

	  vec4 shadowcol = vec4( 0 );
	  texofset[0] = vec2 (0.125,0.125);
	  texofset[1] = vec2 (0.125,0.375);
	  texofset[2] = vec2 (0.125,0.625);
	  texofset[3] = vec2 (0.125,0.875);
	  texofset[4] = vec2 (0.625,0.125);
	  texofset[5] = vec2 (0.625,0.375);
	  texofset[6] = vec2 (0.625,0.625);
	  texofset[7] = vec2 (0.625,0.875);	  

    vec3 N;
    mat3 T2V = mat3( v_tangent, v_binormal, v_normal );                        
    N = mix( v_normal, T2V * normal_map.xyz, 0.1 );
    for ( int i = 0; i<numLightSource; i++ ){
//      if ( normal_map == vec4( 0 )){
//      }
//      else {
//        N = v_normal;
//      }
//      vec2 parabol = 0.5 - ( 0.5 * lightdir.xy / ( abs( lightdir.z ) + 1.0 ));
//      parabol.y /= 2;
//      if (lightdir.z <= 0.0)
//        parabol.y -= 0.5;
//      else
//        parabol.y += 0.5;
  //      shadowcol = vec4( texture( shadow, parabol ).r );
//      shadowcol+= vec4( step( length( lightvec ) / 50 - 0.001, texture( shadow, parabol ).r ));
      vec3 lightvec = v_pos.xyz - LightSource[ i ].position.xyz;
      vec3 lightdir = normalize( lightvec );
      vec2 parabol = texofset[i];
		  if (lightdir.z >= 0.0){
			  parabol.s += 0.25;
			  }
                  // parabolische Projektion für subtextur
		  parabol -=  lightdir.xy * 0.125/ (abs (lightdir.z) + 1.0); 
		  float r = (length(lightvec)-0.001)/50.0;
//		  float off = 0.002 / length(lightvec);
      float s;
      for ( float y = -1.5; y <= 1.5; y+= 1 )
        for ( float x = -1.5; x <= 1.5; x+= 1 )
    		   s+= step( r,  texture2D(shadow, parabol + vec2( x, y ) * 0.0002 ).r );
		  s /= 16;

      if ( s > 0 ) {
        vec4 diff = vec4( 0 );
        util_PointLight( i, mat_hardness, v_pos.xyz, normalize( N ), diff, specular );
  		  diffuse += s*diff;
      }
//		  s =  step( r, s );
    }

/*
    float sillhouette = step( 0.05, dot( N, vec3( 0, 0, 1 )));
	
	  diffuse = vec4( sillhouette ) * smoothstep( 0.30, 0.6, diffuse );// * ( step( 0.8, diffuse ) / 2 + 0.5 ) * normalize( diffuse );
	  specular = smoothstep( 0.75, 0.85, specular );*/
    diffuse = clamp( diffuse, 0, 1 );
    specular = clamp( specular, 0, 1 );    
    frag_color = clamp( 
    ( world_ambient + diffuse ) * mat_diffuse * color_map + specular * mat_specular
    , 0.0, 1.0 );
    frag_color.a = 1;
}
