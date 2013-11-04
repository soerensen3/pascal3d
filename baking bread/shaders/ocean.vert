#version 330

in float foam;
in vec3 vertex;
in vec3 normal;
//in vec2 texture;

uniform mat4 Projection;
uniform mat4 View;
uniform mat4 Model;
uniform vec3 light_position;

out float afoam;
out vec3 light_vector;
out vec3 normal_vector;
out vec3 halfway_vector;
out float fog_factor;
out vec2 tex_coord;
out vec3 vPos;
out vec3 eyeDir;

float curviness = 0.1;

mat4 NormalMatrix;

void main() {
	vec4 v = Model * vec4(vertex, 1.0);
	tex_coord = v.xz;// * 0.5;
	v = View * v;
	v.y -= sqrt( pow( v.x, 2 ) + pow( v.y, 2 )+ pow( v.z, 2 )) * curviness;
	fog_factor = min(v.z/800.0, 1.0);
	afoam = foam;

	gl_Position = Projection * v;
	vec3 normal1 = normalize(normal);

	light_vector = normalize((View * vec4(light_position, 1.0)).xyz - v.xyz);
	NormalMatrix = inverse(transpose(View * Model));
	vPos = v.xyz;
	normal_vector = ( NormalMatrix * vec4(normal1, 0.0)).xyz;
        halfway_vector = light_vector + normalize(-v.xyz);
	eyeDir = normalize( vec3( v ));
	//tex_coord = texture.xy;
}