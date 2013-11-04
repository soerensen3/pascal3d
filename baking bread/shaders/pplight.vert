#version 120
//varying vec3 v;
uniform vec3 lightdir;
//varying vec3 lightvec;
//varying vec3 lightdir;
varying vec3 v;
varying vec3 normal;
varying vec4 FrontColor;
uniform mat4 world;
uniform mat4 view;
uniform mat4 proj;
uniform mat4 normalmatrix;
uniform mat4 invview;
uniform mat4 texproj;
uniform bool highlight;
 
void main(void) {
  //mat4 normalmatrix = transpose(inverse( view * world ));
  normal         = /*gl_Normal;//*/vec3( normalize( mat3( world ) * gl_Normal ));
  //vec3 v              = vec3(view * world * gl_Vertex);
  //vec3 lightvec       = normalize(lightdir);
  v 			  = vec3( world*gl_Vertex );
  
  vec4 posEye =  view * world * gl_Vertex;
  vec4 posWorld = invview * posEye;
  gl_TexCoord[1] = texproj * posWorld;  
 
  gl_TexCoord[0] = gl_MultiTexCoord0;
  FrontColor.rgb = gl_Color.rgb + vec3( 0, 0, 1 ) * int( highlight );// * max(dot(normal, lightvec), 0.0);
  FrontColor.a = 1;
 
  gl_Position    = proj*view*world*gl_Vertex;
}