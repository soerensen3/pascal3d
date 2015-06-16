#version 330
uniform int bluramount;
uniform float blurdiameter;
uniform sampler2D tex;
uniform int direction;

in vec2 tx;
//uniform int direction;

vec2 getDepth( vec2 coords, vec2 offset ){
  return texture2D( tex, coords + offset ).rg;
}
void main(){
  vec4 color = vec4( 0 );
  if ( direction == 0 ){
    for (int i = -bluramount; i <= bluramount; i++ )
      color.rg += getDepth( tx.st, vec2( blurdiameter * i, 0 ));
    }
  else {
    for (int i = -bluramount; i <= bluramount; i++ )
      color.rg += getDepth( tx.st, vec2( 0, blurdiameter * i ));
    }
  gl_FragColor = color / ( bluramount * 2 + 1 );
}
