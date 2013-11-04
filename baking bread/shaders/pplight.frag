#version 120
//varying vec3 v;
//varying vec3 lightvec;
uniform vec3 lightdir;
//uniform vec3 lightdiff = vec3(1);
varying vec3 normal;
varying vec4 FrontColor;
varying vec3 v;
vec2 attenuation = vec2( 0.22, 0.2 );
 
uniform sampler2D Texture0;
uniform sampler2D shadow;

float off = 0.01;

vec4 BlurShadow( float offx, float offy ){
  return texture2DProj( shadow, vec4( gl_TexCoord[1].x + offx, gl_TexCoord[1].y + offy, gl_TexCoord[1].zw )); 
}
 
void main(void) {
//  vec3 lvec = normalize( v - lightdir );
//  vec3 light = FrontColor.rgb * max(dot(normal, -lvec), 0.0);
//  float dist = length( v - lightdir );
//  light = vec3( 1 );
//  light *= vec3( 30 / ( attenuation.x * dist + attenuation.y * dist * dist ));
float noise = /*0.5 +*/ 0.05* fract( sin( v.x * 174 ) + cos( v.y * 17 - v.z * 145));
   
  vec4 shadow_col = vec4( 0 );  
  if( gl_TexCoord[0].q>0.0 )
    shadow_col = (BlurShadow( -off, -off ) + BlurShadow( 0.0, -off ) + BlurShadow( off, -off ) +
				  BlurShadow( -off,  0.0 ) + BlurShadow( 0.0,  0.0 ) + BlurShadow( off,  0.0 ) +
				  BlurShadow( -off,  off ) + BlurShadow( 0.0,  off ) + BlurShadow( off,  off )) / 9;
	

  gl_FragColor.rgba   = vec4( vec3( 1-shadow_col.a *0.5 ), 1 ) * vec4( FrontColor.rgb * vec3( texture2D(Texture0, vec2(gl_TexCoord[0]) + noise * 0.02)), FrontColor.a );  
  
}