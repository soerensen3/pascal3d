#version 330
uniform sampler2D tex0;

in vec2 texc_0; 

void main()
{
    vec4 color = texture2D( tex0, texc_0 );
    gl_FragColor = color;
}
