#pragma include "shaderutils.glsl"

vec2 packFloatToVec2i(const float value)
{
        /*float val256 = value*256.0;
        float bigpart = floor(val256);
        float smallpart = floor(fract(val256-bigpart)*256.0);
        return vec2(smallpart/256.0,bigpart/256.0);*/
        
        float val = clamp(value,0.0,1.0);
        float smallpart = mod(val,1.0/256.0);
        return vec2(smallpart*256.0,val-smallpart);
}



in vec4 v_pos;
void main(void){
  if (length(v_pos.xy)>1.005) discard;  //Diese Zeile kann durchaus entfallen. Kosten/Nutzen unbekannt.
//  gl_FragColor.r = gl_FragCoord.z;
//  gl_FragColor.g = ( gl_FragCoord.z - gl_FragColor.x ) * 256 * 256;
//  gl_FragColor.rg = packFloatToVec2i( gl_FragCoord.z );
  float depth = v_pos.z / v_pos.w;
  depth = depth * 0.5 + 0.5;			//Don't forget to move away from unit cube ([-1,1]) to [0,1] coordinate system
	
	float moment1 = depth;
	float moment2 = depth * depth;
	
		// Adjusting moments (this is sort of bias per pixel) using partial derivative
	float dx = dFdx(depth);
	float dy = dFdy(depth);
	moment2 += 0.25*(dx*dx+dy*dy) ;
  gl_FragColor.rg = vec2( moment1, moment2 );
  gl_FragColor.ba = vec2( 0, gl_FragCoord.x ); //gl_FragCoord.z*gl_FragCoord.z, 0, 0.5 );
}
