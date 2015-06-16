#version 330
//--------------------------------------------------------------------------------------------------**
// VARIABLES TO TWEAK


//Material settings:
vec3 ambientColor = vec3(0.06,0.15,0.2); 	// Set the ambient color (R,G,B)
float specIntensity = 1.0;					// Set Specular intensity (default value = 1.0)
float shininess = 5.0;						// Set shininess of the specular (between 1.0 and 10.0 is suggested)
float maximumMix = 0.8;						// Set the maximum blending between far and near materials (default value = 1.0, between 0.0 and 1.0)
float matNear = 0.02;						// distance where the far materiat start to appear (between 0.0 and 1.0)
float matFar = 0.2;							// distance where the far material has replaced the near material (between 0.0 and 1.0)

//Lighting settings:
vec3 sunColor = vec3(1.0,0.4,0.2);			// Set the Sun color (R,G,B)
float sunIntensity = 1.5;					// Set the Sun intensity (at least 0.0)
vec3 sunDirection = normalize( vec3(-0.97004,0.21789,0.10745));		// Set the sun direction (X,Y,Z)

//Fog settings:
float maxDist = 1600.0;						// Set the maximum ditance of view (blender units, look at the camera clipping)
float midDist = 0.8;						// Set the intermediate fog distance, in percentage of the max. distance (between 0.0 and 1.0)
float vertDistEnd = -0.8;
float vertDistStart = -1.0;
vec3 fogColor = vec3(0.24,0.17,0.14);			// Set the intermediate fog color (R,G,B)
vec3 fogColor2 = vec3(0.24,0.17,0.14) / 4;			// Set the ending fog color (R,G,B)
vec3 fogColor3 = vec3(0.0,0.1,0.05);

// UV Coord.
float tile = 512.0;							// Set the number of times the texture is repeated (near the player)
float tileFar = 128.0;						// Set the number of times the texture is repeated (far from the player)

// Texture
float margin = 0.8;							// Set the margin around textures, for the multilayer file

//---------------------------------------------------------------------------------------------------**

	

// get textures
uniform sampler2D heightMap;
uniform sampler2D multilayer;
uniform sampler2D lightmap;

uniform mat4 view;
uniform mat4 world;
uniform mat4 proj;


//get vertex position
in vec4 position; 
in vec2 texcoord;



// prepare the tiled UVcoord for tiled multilayer
float marginAdd = (0.25-margin*0.25)/2.0;
vec2 TileUv = vec2 (
                     (
                       (
						 fract( texcoord.s * tile )	
//                         ( texcoord.s * tile ) 
//                           - float( int( texcoord.s * tile ))
                       )
                      / 4.0 
                     )
                     * margin + marginAdd,
					 (
                       (
						 fract( texcoord.t * tile )
//                         ( texcoord.t * tile )
//                           - float( int( texcoord.t * tile ))
                         )
                       / 4.0 
                     )
                     * margin + marginAdd 
                   );
					
vec2 TileUvFar = vec2 (
                     (
                       (
						 fract( texcoord.s * tileFar )	
//                         ( texcoord.s * tileFar ) 
//                           - float( int( texcoord.s * tileFar ))
                       )
                      / 4.0 
                     )
                     * margin + marginAdd,
					 (
                       (
						 fract( texcoord.t * tileFar )
//                         ( texcoord.t * tileFar )
//                           - float( int( texcoord.t * tileFar ))
                         )
                       / 4.0 
                     )
                     * margin + marginAdd 
                   );


vec3 tex3(float posX, float posY){
    vec3 textureMap3 = texture2D(multilayer, vec2(TileUv.x+posX,TileUv.y+posY)).rgb;
	return textureMap3;
}

vec3 tex3Far(float posX, float posY){
    vec3 textureMap3Far = texture2D(multilayer, vec2(TileUvFar.x+posX,TileUvFar.y+posY)).rgb;
	return textureMap3Far;
}

vec3 prepareNM(vec3 exemple){
   	vec3 gut = vec3(2.0, 2.0, 1.0) * exemple - vec3(1.0, 1.0, 1.0);
	return gut;
}


void main()
{

	// get the view vector
	vec3 viewDirection = -normalize(vec3(proj * view * position)); 


	// get normals
	vec3 normalGlobal = normalize(vec3(2.0, 2.0, 1.0) * (texture2D(heightMap, texcoord.st).rgb) - vec3(1.0, 1.0, 0.0) 
                      );//+ 0.1 * texture2D(multilayer, vec2( 0.166 + fract( texcoord.s * 64 ) * 0.166, 0.833 + fract( texcoord.t * 64 ) * 0.166 )).rgb ); 


	// separate textures from multilayer file

/*	//material 1
	vec3 diffuseColor_mat1 = tex3(0.0,0.0);
	float specLevel_mat1 = tex3(0.0,0.25).x;
	vec3 normalDetails_mat1 = prepareNM(tex3(0.0,0.50));
	vec3 diffuseColorFar_mat1 = tex3Far(0.0,0.0);
	vec3 normalDetailsFar_mat1 = prepareNM(tex3Far(0.0,0.5));*/

	//material 1
	vec3 diffuseColor_mat1 =  tex3(0.333,1.0);
	float specLevel_mat1 = tex3(0.333,0.333).x;
	vec3 normalDetails_mat1 = prepareNM(tex3(0.333,0.666));
	//vec3 diffuseColorFar_mat1 = tex3Far(0.333,0.0);
	//vec3 normalDetailsFar_mat1 = prepareNM(tex3Far(0.333,0.666));

	//Material 2
	vec3 diffuseColor_mat2 = tex3(0.666,1.0);
	float specLevel_mat2 = tex3(0.666,0.333).x;
	vec3 normalDetails_mat2 = prepareNM(tex3(0.666,0.666));
	//vec3 diffuseColorFar_mat2 = tex3Far(0.666,0.0);
	//vec3 normalDetailsFar_mat2 =  prepareNM(tex3Far(0.666,0.666));


	// Stencils maps
	float stencil_mat = smoothstep( 0.5, 0.7, texture2D(heightMap, texcoord.st).a );


	//lightmap
	float lightmap = texture2D(lightmap, texcoord.st).r;


	//Near Material
	vec3 normalAllDetails = 
		mix( normalDetails_mat1 ,normalDetails_mat2, stencil_mat );
	float specLevelAll = mix( specLevel_mat1, specLevel_mat2, stencil_mat );
	vec3 normalFinal = normalize( normalGlobal + normalAllDetails );
	vec3 diffuseRawAll = ambientColor + lightmap * sunColor * 
			(sunIntensity * max( 0.0, dot( normalFinal, normalize( sunDirection )))); // compute diffuse lighting
	vec3 diffuseColorAll = 
	    mix( diffuseColor_mat1, diffuseColor_mat2, stencil_mat );
	float specularAll = specIntensity * lightmap * max( 0.0,( 1 * pow(dot( reflect(sunDirection, normalFinal), viewDirection), shininess)));// compute specular reflection
	vec3 materialAll = diffuseRawAll * diffuseColorAll + 
					   specularAll * specLevelAll;

/*
	//Far Material
	vec3 normalDetailsFar = 
		mix( normalDetailsFar_mat1, normalDetailsFar_mat2, stencil_mat );
	vec3 normalFinalFar = normalize( normalGlobal + normalDetailsFar );
	vec3 diffuseRawFar = ambientColor + lightmap*sunColor * 
		( sunIntensity * max( 0.0, dot( normalFinalFar, normalize( sunDirection )))); // compute diffuse lighting
	vec3 diffuseColorFar = 
		mix( diffuseColorFar_mat1, diffuseColorFar_mat2, stencil_mat );
	vec3 materialFar = diffuseRawFar*diffuseColorFar;
*/

	// getting fog distances
	float z = clamp((gl_FragCoord.z / gl_FragCoord.w / maxDist),0.0,1.0); // compute intermediate fog
	float zNear = clamp((1.0-(z/midDist)),0.0,1.0);
	float zFar = clamp((1.0-(z/(1.0-midDist)-midDist)),0.0,1.0);
    float zHigh = clamp(((position.z / 100 )), 0, 1 );//smoothstep( 0, 1, position.z ); //vertDistStart, vertDistEnd, position.z ); 
	float zMat = clamp((1.0-(z/matNear-matFar)),0.0,maximumMix);


	// Mix near and far materials
	vec3 material = materialAll;
//    vec3( specLevelAll );
//	diffuseColor_mat1;
//	mix( materialFar, materialAll, zMat );


	// add fog
	vec3 beauty = //materialAll;
    //vec3( 0.5 + normalDetails_mat1.r );
//    mix( normalDetails_mat1, diffuseColor_mat1, 0.1 );
	material;
	//material - ( fogColor2 * zFar * fogColor * zNear );
//	mix(fogColor2,(mix(fogColor,material,zNear)),zFar);
//    mix(fogColor3, mix(fogColor2,(mix(fogColor,material,zNear)),zFar), zHigh );


	// set final values
	gl_FragColor.rgb = beauty;
	gl_FragColor.a = 1.0;
}
