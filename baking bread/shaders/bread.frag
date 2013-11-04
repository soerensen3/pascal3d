#version 120
//varying vec3 v;
//varying vec3 lightvec;
uniform vec3 lightdir;
//uniform vec3 lightdiff = vec3(1);
varying vec3 normal;
varying vec4 FrontColor;
varying vec3 v;
vec2 attenuation = vec2( 0.2, 0.2 );
uniform float ready;
vec3 ambient = vec3( 1, 0.73, 0.60 )*0.5;
vec3 lightcol = vec3( 1, 0.73, 0.60 ) * 0.3;

 
uniform sampler2D Texture0;
uniform sampler2D dough;
 
void main(void) {
  vec3 lvec = normalize( v - lightdir );
  vec3 light = lightcol * FrontColor.rgb * max(dot(normal, -lvec), 0.0);
  float dist = length( v - lightdir );
  light *= vec3( 30 / ( attenuation.x * dist + attenuation.y * dist * dist ));
  light +=  ambient;
   
  vec3 diff;
  if ( ready > 1.0 ) {
    diff = vec3( texture2D(Texture0, vec2(gl_TexCoord[0])) - ( ready - 1 ));
  } else {
    diff = vec3( mix( texture2D(dough, vec2(gl_TexCoord[0])), texture2D(Texture0, vec2(gl_TexCoord[0])), ready ));
  }
  gl_FragColor.rgba  = vec4( light * diff, FrontColor.a );
  
}