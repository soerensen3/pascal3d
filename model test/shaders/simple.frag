uniform sampler2D tex;
 
void main()
{
    vec4 color = vec4( texture2D(tex,gl_TexCoord[0].st).rgb, 1 );
    gl_FragColor = gl_Color * color;
}
