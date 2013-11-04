#version 120
//varying vec3 v;
//varying vec3 lightvec;
//uniform vec3 lightdir;
//uniform vec3 lightdiff = vec3(1);
//varying vec3 normal;
varying vec4 FrontColor;
 
uniform sampler2D Texture0;
 
void main(void) {
  //vec3 Eye       = normalize(-v);
  //vec3 Reflected = normalize(reflect( -lightvec, normal)); 
 
  //vec4 IAmbient  = gl_LightSource[0].ambient * gl_FrontMaterial.ambient;
  //vec4 IDiffuse  = vec4(lightdiff * max(dot(normal, lightdir), 0.0), 1);
  //vec4 ISpecular = gl_LightSource[0].specular * pow(max(dot(Reflected, Eye), 0.0), gl_FrontMaterial.shininess) * gl_FrontMaterial.specular;
 
  gl_FragColor   = FrontColor * texture2D(Texture0, vec2(gl_TexCoord[0]));  
  
}