uniform mat4 view;
uniform mat4 world;

void main() {
    gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;
    gl_Position = gl_ProjectionMatrix * view * world * gl_Vertex;
}