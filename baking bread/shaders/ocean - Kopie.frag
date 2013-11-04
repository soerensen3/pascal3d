#version 330

in vec3 normal_vector;
in vec3 light_vector;
in vec3 halfway_vector;
in vec2 tex_coord;
in float fog_factor;
in float afoam;
uniform sampler2D wavemap;
uniform sampler2D foammap;
uniform float worldTime;
out vec4 fragColor;


vec3 normal(vec2 coord, vec2 time)
{
  return texture(wavemap, fract( coord + worldTime / time )).xzy - vec3(0.5);
}


void main (void) {
	//fragColor = vec4(1.0, 1.0, 1.0, 1.0);

	vec3 normal1         = normalize(normal_vector);
	vec3 light_vector1   = normalize(light_vector);
	vec3 halfway_vector1 = normalize(halfway_vector);

	vec4 c = vec4(1,1,1,1);//texture(water, tex_coord);

	vec4 emissive_color = vec4(1.0, 1.0, 1.0,  1.0);
	vec4 ambient_color  = vec4(0.0, 0.65, 0.75, 1.0);
	vec4 diffuse_color  = vec4(0.38, 0.41, 0.51, 1.0);
	vec4 specular_color = vec4(1.0, 0.75, 0.5,  1.0);
	vec4 fog_color 		= vec4(0.15, 0.2, 0.3, 1.0);

	float emissive_contribution = 0.00;
	float ambient_contribution  = 0.30;
	float diffuse_contribution  = 0.30;
	float specular_contribution = 1.80;

	//vec3 N = vec3( texture( wavemap, fract( tex_coord )));
	
 
	vec2 texCoord = tex_coord / 2000;
  // Idea stolen from Source 2007 (thanks!)
	vec2 texCoord2 = vec2(texCoord.x + texCoord.y, texCoord.y - texCoord.x);	
	
	vec3 N = vec3( 0 );
	N += normal(texCoord * 5.0, vec2(200.0, 166.0));
	N += normal(texCoord * 31.0, vec2(94.0, 70.0));
	N += normal(texCoord2 * 67.0, vec2(56.0, 34.0));
	N += normal(texCoord * 203.0, vec2(100.0, 20.0));
	N += normal(texCoord2 * 123.0, vec2(60.0, 57.0));
 
    N = normalize(N);	
	normal1 = normalize( mix( normal1, N, 0.5));
	float d = dot( normal1, light_vector1 );
	bool facing = d > 0.0;
	
	vec4 ffoam = vec4( vec3( afoam ), 1 ) * texture( foammap, fract( tex_coord / 10 ));

	fragColor = emissive_color * emissive_contribution +
		    ambient_color  * ambient_contribution  * c +
		    diffuse_color  * diffuse_contribution  * c * max(d, 0) +
                    (facing ?
			specular_color * specular_contribution * c * max(pow(dot(normal1, halfway_vector1), 5.0), 0) :
			vec4(0.0, 0.0, 0.0, 0.0));
			

	fragColor = fragColor * (1.0-fog_factor) + fog_color * (fog_factor) + ffoam;

	fragColor.a = 1.0;
}