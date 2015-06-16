uniform sampler2D tex0;
 
attribute vec4 v_color;
attribute vec2 v_texc0;

void main()
{
    vec4 color = texture2D(tex0,v_texc0.st );
    gl_FragColor = color * v_color;
}
