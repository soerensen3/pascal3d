#version 330

in vec3 normal_vector;
in vec3 light_vector;
in vec3 halfway_vector;
in vec2 tex_coord;
in float fog_factor;
in float afoam;
in vec3 vPos;
in vec3 eyeDir;
uniform sampler2D wavemap;
uniform sampler2D foammap;
uniform samplerCube skymap;
uniform float worldTime;
uniform mat4 View;
uniform mat4 Model;
out vec4 fragColor;


vec3 normal(vec2 coord, vec2 time)
{
  return texture(wavemap, fract( coord + worldTime / time )).xyz - vec3(0.5);
}


void main (void) {
	//fragColor = vec4(1.0, 1.0, 1.0, 1.0);

	vec3 normal1         = normalize(normal_vector);
	vec3 light_vector1   = normalize(light_vector);
	vec3 halfway_vector1 = normalize(halfway_vector);

	vec4 c = vec4(1,1,1,1);//texture(water, tex_coord);

	vec4 emissive_color = vec4(0.0, 0.0, 0.0,  1.0);
	vec4 ambient_color  = vec4(0.22, 0.36, 0.26, 1.0);
	vec4 diffuse_color  = vec4(0.14, 0.26, 0.31, 1.0) - ambient_color;//vec4(0.27, 0.38, 0.45, 1.0);
	vec4 specular_color = vec4(1.0, 0.75, 0.5,  1.0);
	vec4 fog_color 		= vec4(0.27, 0.4, 0.5, 1.0);

	float emissive_contribution = 0.00;
	float ambient_contribution  = 1.00;
	float diffuse_contribution  = 0.50;
	float specular_contribution = 1.80;

	//vec3 N = vec3( texture( wavemap, fract( tex_coord )));	

	vec2 texCoord = tex_coord / 4000;
  // Idea stolen from Source 2007 (thanks!)
	vec2 texCoord2 = vec2(texCoord.x + texCoord.y, texCoord.y - texCoord.x);	
	
	vec3 N = vec3( 0, 0, 1 );
	N += normal(texCoord * 5.0, vec2(200.0, 166.0)) * 10;
	N += normal(texCoord * 31.0, vec2(94.0, 70.0)) * 5;
	//N += normal(texCoord2 * 67.0, vec2(56.0, 34.0)) * 3;
	//N += normal(texCoord * 203.0, vec2(100.0, 20.0)) * 2;
	//N += normal(texCoord2 * 123.0, vec2(60.0, 57.0));
 
    N = normalize(N);	
	
	vec3 Eye = normalize(-vPos); 
 
	vec3 q0 = dFdx(Eye.xyz);
	vec3 q1 = dFdy(Eye.xyz);
	vec2 st0 = vec2( 1, 0 );//dFdx(texCoord.st);
	vec2 st1 = vec2( 0, 1 );//dFdy(texCoord.st);
 
	vec3 S = normalize( q0 * st1.t - q1 * st0.t);
	vec3 T = normalize(-q0 * st1.s + q1 * st0.s);
 
	mat3 M = mat3(-T, -S, normal1);
	normal1 = normalize( M * N/* - vec3(0.5, 0.5, 0.5)*/);	
	
	//normal1 = normalize( mix( normal1, N, 0.5));
	float d = dot( normal1, light_vector1 );
	bool facing = d > 0.0;
	
	vec4 ffoam = vec4( vec3( afoam ), 1 ) * texture( foammap, fract( tex_coord / 20 ));

	vec3 reflection = vec3( vec4( reflect( eyeDir, normal1 ), 1 ) * View);
	vec4 spec = texture(skymap, reflection );
	fragColor = emissive_color * emissive_contribution +
		    ambient_color  * ambient_contribution  * c +
		    diffuse_color  * diffuse_contribution  * c * max(d, 0)+
			spec * dot(normal1, halfway_vector1) * 0.3;
	vec4 polarisation = 
        (facing ?
			spec/*ular_color*/ * specular_contribution * c * step( 0.995, dot(normal1, halfway_vector1)) :
			vec4(0.0, 0.0, 0.0, 0.0));
   
				 
	fragColor = vec4( fragColor.rgb, 1 ) * (1.0-fog_factor);/* + fog_color * (fog_factor)*/// + ffoam + polarisation;
}