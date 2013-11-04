#version 120
varying vec4		position;
varying vec3		normals;
varying mat4		TBN;
uniform sampler2D	tDiffuse;
//uniform gl_Color;


void main( void )
{
	gl_FragData[0] = gl_Color * vec4(texture2D(tDiffuse,gl_TexCoord[0].st).rgb, 0);
	gl_FragData[1] = vec4(position.xyz,0);
	gl_FragData[2] = vec4(normals.xyz,0);
}
