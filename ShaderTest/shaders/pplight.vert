#version 120
//varying vec3 v;
uniform vec3 lightdir;
//varying vec3 lightvec;
//varying vec3 lightdir;
//varying vec3 normal;
varying vec4 FrontColor;
uniform mat4 world;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 normalmatrix;
 
void main(void) {
  //mat4 normalmatrix = transpose(inverse( view * world ));
  vec3 normal         = gl_Normal;//vec3( normalize( normalmatrix * vec4( gl_Normal, 1 )));
  //vec3 v              = vec3(view * world * gl_Vertex);
  vec3 lightvec       = normalize(lightdir);
 
  gl_TexCoord[0] = gl_MultiTexCoord0;
  FrontColor.rgb = gl_Color.rgb * max(dot(normal, lightvec), 0.0);
  FrontColor.a = 1;
 
  gl_Position    = proj*view*world*gl_Vertex;//gl_ProjectionMatrix *gl_ModelViewMatrix*/* view*world */ gl_Vertex;
}