uniform sampler2D depthSampler; // 0
uniform sampler2D specSampler; // 1

float LinearizeDepth(vec2 uv)
{
  float n = 0.1; // camera z near
  float f = 100.0; // camera z far
  float z = texture2D(depthSampler, uv).x;
  return (2.0 * n) / (f + n - z * (f - n));	
}
void main() 
{ 
  /*vec2 uv = gl_TexCoord[0].xy;
  float d;
  if (uv.x < 0.5) // left part
    d = LinearizeDepth(uv);
  else // right part
    d = texture2D(depthSampler, uv).x;
  gl_FragColor.rgb = texture2D(specSampler, vec2( d, 0 )).rgb;*/
  gl_FragColor.rgb = vec3( texture2D(depthSampler, gl_TexCoord[0].xy).rg, 0 );
  gl_FragColor.a = 1.0;
}
