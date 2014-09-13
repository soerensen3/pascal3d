varying vec2 tex;
varying vec3 vpos;
varying vec3 normal;
varying vec3 tangent;
varying vec3 binormal;

uniform vec2 uvscale;

void main()
{
   gl_Position = ftransform();
   tex = gl_MultiTexCoord0.xy * uvscale;
   vpos = (gl_ModelViewMatrix * gl_Vertex).xyz;
   tangent  = gl_NormalMatrix * gl_MultiTexCoord3.xyz;
   binormal = gl_NormalMatrix * gl_MultiTexCoord4.xyz;
   normal   = gl_NormalMatrix * gl_Normal;
};

