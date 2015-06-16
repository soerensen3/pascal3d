#version 330
layout ( location = 0 ) in vec3 in_vertex;
layout ( location = 1 ) in vec3 in_normal;
layout ( location = 2 ) in vec3 in_tangent;
layout ( location = 3 ) in vec3 in_binormal;
in vec4 in_color;
layout ( location = 4 ) in vec2 in_texc0;

uniform int numMaps;


out vec4 v_color;
out vec3 v_normal;
out vec2 v_texc0;
out vec4 v_pos;
out vec3 v_tangent;
out vec3 v_binormal;

//out vec3 spos;

#pragma include "shaderutils.glsl"


void main() {
    mat3 mat_norm = mat3( view * world );
    vec3 N = mat_norm * in_normal;
        
    gl_Position = proj * view * world * vec4( in_vertex, 1 );
    v_color = in_color;
    v_texc0 = in_texc0;
    v_normal = N;
    v_tangent = mat_norm * in_tangent;
    v_binormal = mat_norm * in_binormal;
    v_pos = view * world * vec4( in_vertex, 1 );

       
//    spos.xy = pos; //Daten in einer Varying verpacken
//    spos.z = -vertex.y * 0.5 + 0.5 - 0.005; // far/near-clamping und Anti-Z-Fightingoffset 
}
