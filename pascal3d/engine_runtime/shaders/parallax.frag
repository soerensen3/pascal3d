varying vec2 tex;
varying vec3 vpos;
varying vec3 normal;
varying vec3 tangent;
varying vec3 binormal;

uniform sampler2D sNormal;
uniform sampler2D sDiffuse;
uniform sampler2D sGloss;

uniform vec3 lightDir;
uniform bool specular;
uniform float specular_exp;
uniform vec3 ambient_color;
uniform vec3 diffuse_color;
uniform vec3 specular_color;

void main()
{
   mat3 TBN = mat3(tangent, binormal, normal);
   vec3 V = normalize(vpos);
   vec3 V_ts = V * TBN;
   float height = texture2D(sNormal, tex).a;
   float offset = height * 0.025 - 0.0125;
   vec2 tc = tex + offset * V_ts.xy;
   height += texture2D(sNormal, tc).a;
   offset = 0.025 * (height - 1.0);
   tc = tex + offset * V_ts.xy;
   vec3 N = texture2D(sNormal, tc).rgb * 2.0 - 1.0;
   N = normalize(N.x * tangent + N.y * binormal + N.z * normal);
   vec3 diffuse = texture2D(sDiffuse, tc).rgb;
   float NdotL = clamp(dot(N, lightDir), 0.0, 1.0);
   vec3 color = diffuse * diffuse_color * NdotL;
   if(specular)
   {
     vec3 gloss = texture2D(sGloss, tc).rgb;
     vec3 R = reflect(V, N);
     float RdotL = clamp(dot(R, lightDir), 0.0, 1.0);
     color += gloss * specular_color * pow(RdotL, specular_exp);
  }
  gl_FragColor.rgb = ambient_color * diffuse + color;
}
