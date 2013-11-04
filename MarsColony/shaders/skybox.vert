#version 150
uniform mat4 world;
uniform mat4 view;
uniform mat4 proj;

uniform mat4 ModelViewMatrix;

in vec3 vertex;
out vec3 texCoord;

void main()
{
    mat4 r = view * world;
    r[3][0] = 0.0;
    r[3][1] = 0.0;
    r[3][2] = 0.0;
    
    vec4 v = inverse( r ) * inverse( proj ) * vec4( vertex.x, vertex.y, vertex.z, 1 );

    texCoord = vec3( v ); 
    gl_Position.xyz = vertex.xyz;
}