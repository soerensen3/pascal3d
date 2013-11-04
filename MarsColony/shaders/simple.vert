uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;

void main() {
    gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
    gl_Position = proj * view * world * gl_Vertex;
}