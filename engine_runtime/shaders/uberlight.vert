#version 140

uniform mat4 MVMatrix;
uniform mat4 MVPMatrix;
uniform mat3 NormalMatrix;

uniform vec3 WCLightPos; // Position of light in world coordinates
uniform vec4 ViewPosition; // Position of camera in world space
uniform mat4 WCtoLC; // World to light coordinate transform
uniform mat4 WCtoLCit; // World to light inverse transpose
uniform mat4 MCtoWC; // Model to world coordinate transform
uniform mat4 MCtoWCit; // Model to world inverse transpose

in vec4 MCVertex;
in vec3 MCNormal;

out vec3 LCPos; // Vertex position in light coordinates
out vec3 LCnorm; // Normal in light coordinates
out vec3 LCcamera; // Camera position in light coordinates

void main()
{
  gl_Position = MVPMatrix * MCVertex;
  
  // Compute wolrd space position and normal
  vec4 wcPos = MCtoWC * MCVertex;
  vec4 wcNorm = ( MCtoWCit * vec4( MCNormal, 0.0 )).xyz;

  // Compute light coordinate system camera position.
  // vertex position and normal

  LCcamera = ( WCtoLC * ViewPosition ).xyz;
  LCPos    = ( WCtoLC * wcPos ).xyz;
  LCnorm   = ( WCtoLCit * vec4( wcNorm, 0.0 )).xyz;
}
