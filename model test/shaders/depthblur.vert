#version 120

varying vec2 tx;

void main(){
  gl_Position = ftransform();
  //gl_TexCoord[0] = gl_MultiTexCoord0;
  tx = gl_MultiTexCoord0.st;
}
