#version 330
uniform sampler2D tex0;

in vec4 out_color;
in vec2 out_texc0;

out vec4 FragColor;

void main()
{

    vec4 c = texture2D( tex0, out_texc0 ) * out_color;
    FragColor = c;
}
