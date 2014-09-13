#version 330
uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

layout ( location = 0 ) in vec3 v_pos;
layout ( location = 4 ) in vec2 v_texc0;


out vec2 texc_0;

void main() {
	texc_0 = clamp( vec2( 0.95 ), vec2( 0.05 ), v_texc0 );
    gl_Position = proj * view * world * vec4( v_pos, 1 );
}
