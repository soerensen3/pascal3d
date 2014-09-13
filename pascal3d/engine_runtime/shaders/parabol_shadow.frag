#pragma include "shaderutils.glsl"
in vec4 v_pos;
void main(void){
  if (length(v_pos.xy)>1.005) discard;  //Diese Zeile kann durchaus entfallen. Kosten/Nutzen unbekannt.
}
