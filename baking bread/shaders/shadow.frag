uniform sampler2D tex;

//varying vec3 len;
 
void main()
{
    //vec4 color = texture2D(tex,gl_TexCoord[0].st);
    gl_FragColor = vec4( vec3( 0 ), 1 );///*vec4( 0.5, 0.5, 0.5, 0.5 ) +*/ color;
}