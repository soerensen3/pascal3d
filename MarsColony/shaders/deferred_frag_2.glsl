uniform sampler2D tDiffuse; 
uniform sampler2D tPosition;
uniform sampler2D tNormals;
uniform vec3 cameraPosition;
uniform mat3 lights[];
//varying gl_Color;


void main( void )
{
    vec4 image = texture2D( tDiffuse, gl_TexCoord[0].xy );
    vec4 position = texture2D( tPosition, gl_TexCoord[0].xy );
    vec4 normal = texture2D( tNormals, gl_TexCoord[0].xy );
    
    vec3 light = vec3(50,100,50);
    vec3 light2 = vec3(10,20,50);
    vec3 light3 = vec3(1,0.5,1);
    vec3 light4 = vec3(0,-30,-30);
    vec3 lightDir = light - position.xyz ;
    vec3 lightDir2 = light2 - position.xyz ;
    vec3 lightDir3 = light3 - position.xyz ;
    vec3 lightDir4 = light4 - position.xyz ;
    
    normal = normalize(normal);
    lightDir = normalize(lightDir);
    lightDir2 = normalize(lightDir2);
    lightDir3 = normalize(lightDir3);
    lightDir4 = normalize(lightDir4);

    
    vec3 eyeDir = normalize(cameraPosition-position.xyz);
    vec3 vHalfVector = normalize(lightDir.xyz+eyeDir);
    
    vec4 cl = max(dot(normal,lightDir),0) * vec4( 1 );
    cl += max(dot(normal,lightDir2),0) * vec4( 0.3, 0, 0, 0 );
    cl += max(dot(normal,lightDir3),0) * vec4( 1 );
    cl += max(dot(normal,lightDir4),0) * vec4( 0, 0, 0.3, 0 );
    cl *= image;
    gl_FragColor = cl;
 //+ 
//                   pow(max(dot(normal,vHalfVector),0.0), 100) * 1.5;
}
