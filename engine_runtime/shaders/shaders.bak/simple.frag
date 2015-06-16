#version 150
uniform sampler2D tex;
 
in vec4 f_color;

out vec4 frag_color;
 
void main()
{
//    vec4 color = texture2D(tex,gl_TexCoord[0].st);
    frag_color = /*color * */ f_color;
}
