#version 330

in vec3 vertex;
in vec3 normal;
//in vec2 texture;

uniform mat4 Projection;
uniform mat4 View;
uniform mat4 Model;

out vec3 normal_vector;
out vec2 tex_coord;

float curviness = 0.1;

void main() {

	tex_coord = vec2( -0.5 + vertex.x / 512, -0.5 + vertex.z / 512 );

	vec4 vert = View * Model * vec4( vertex, 1 );
	vert.y -= sqrt( pow( vert.x, 2 ) + pow( vert.y, 2 )+ pow( vert.z, 2 )) * curviness;
	vert = Projection * vert;
    gl_Position = vert;
	
	normal_vector = vec3( vertex.y );
//	vec3 normal1 = normalize(normal);

//	light_vector = normalize((View * vec4(light_position, 1.0)).xyz - v.xyz);
//	NormalMatrix = inverse(transpose(View * Model));
//	vPos = v.xyz;
//	normal_vector = ( NormalMatrix * vec4(normal1, 0.0)).xyz;
//        halfway_vector = light_vector + normalize(-v.xyz);
//	eyeDir = normalize( vec3( v ));
	//tex_coord = texture.xy;
}