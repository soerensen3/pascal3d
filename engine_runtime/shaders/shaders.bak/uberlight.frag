uniform vec3 SurfaceColor;

//Light Parameters
uniform vec3 LightColor;
uniform vec3 LightWeights;

//Surface Parameters
uniform vec3 SurfaceWeights;
uniform float SurfaceRoughness;
uniform bool AmbientClamping;

//Super Ellipse Shaping Parameters
uniform bool BarnShaping;
uniform float SeWidth;
uniform float SeHeight;
uniform float SeWidthEdge;
uniform float SeHeightEdge;
uniform float SeRoundness;

//Distance Shaping Parameters
uniform float DsNear;
uniform float DsFar;
uniform float DsNearEdge;
uniform float DsFarEdge;

//Varying Variables
varying vec3 LCpos;
varying vec3 LCnorm;
varying vec3 LCcamera;


/*************************************************************************/


float SuperEllipseShape(vec3 pos) {
	if (!BarnShaping) return 1.0;
	else {
		//Project the point onto the z = 1.0 plane
		vec2 ppos = pos.xy / pos.z;
		vec2 abspos = abs(ppos);
		
		float w = SeWidth;
		float W = SeWidth + SeWidthEdge;
		float h = SeHeight;
		float H = SeHeight + SeHeightEdge;
		
		float exp1 = 2.0 / SeRoundness;
		float exp2 = -SeRoundness / 2.0;
		float inner = w * h * pow(pow(h * abspos.x, exp1) + pow(w * abspos.y, exp1), exp2);
		float outer = W * H * pow(pow(H * abspos.x, exp1) + pow(W * abspos.y, exp1), exp2);
		return 1.0 - smoothstep(inner, outer, 1.0);
	}
}


/*************************************************************************/


float DistanceShape(vec3 pos) {
	float depth = abs(pos.z);
	float dist = smoothstep(DsNear - DsNearEdge, DsNear, depth) * (1.0 - smoothstep(DsFar, DsFar + DsFarEdge, depth));
	return dist;
}


/*************************************************************************/


void main(void) {
	vec3 tmpLightColor = LightColor;
	vec3 N = normalize(LCnorm);
	vec3 L = -normalize(LCpos);
	vec3 V = normalize(LCcamera - LCpos);
	vec3 H = normalize(L + V);
	
	vec3 tmpColor = tmpLightColor;
	float attenuation = 1.0;
	attenuation *= SuperEllipseShape(LCpos);
	attenuation *= DistanceShape(LCpos);
	float ndot1 = dot(N, L);
	float ndoth = dot(N, H);
	//...
}


/*************************************************************************/


