#version 150
uniform samplerCube cubemap;

in vec3 texCoord;

out vec4 FragColor;
void main()
{
    FragColor = texture(cubemap, texCoord);
}