#version 330
// position of the vertex (and fragment) in view space
out vec4 position; 
out vec2 texcoord;

// getting texture and properties values
uniform sampler2D heightMap;
uniform sampler2D multilayer;

uniform int uvX;
uniform int uvY;
uniform float height;

uniform int cellNumber;

uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

layout ( location = 0 ) in vec4 v_pos;
//layout ( location = 1 ) in vec3 in_normal;
//layout ( location = 2 ) in vec3 in_tangent;
//layout ( location = 3 ) in vec3 in_binormal;
//in vec4 in_color;
layout ( location = 4 ) in vec2 v_texc0;


void main(){

    //get the first UV layout
    texcoord.s = v_texc0.x-float(uvX)/float(cellNumber);
    texcoord.t = v_texc0.y+float(uvY)/float(cellNumber);

    // deform
    vec4 v = v_pos;
    v.z += texture2D(heightMap, /*clamp(*/texcoord.st/*,0.0,1.0)*/).a*(0.03 * height);
	vec2 detail = vec2( 0.166 + fract( texcoord.s * 64 ) * 0.166, 0.833 + fract( texcoord.t * 64 ) * 0.166 );
	//v =	v + vec4( 0.001 * ( vec3( 2.0, 2.0, 1.0 ) * texture2D( multilayer, detail ).rgb - vec3( 1.0, 1.0, 0.0 )), 1 );
    position = world * v;
    gl_Position = view * position;
//	gl_Position.z +=  sqrt( pow( gl_Position.x, 2 ) + pow( gl_Position.y, 2 ));
	gl_Position = proj * gl_Position;
     
    //get the vertex position

}
