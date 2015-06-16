#version 130
uniform sampler2D tex;
 
in vec4 f_color;

in vec2 tc;

out vec4 frag_color;
 
void main()
{
    vec4 color = texture2D(tex,tc);
    frag_color = color * f_color;
}
