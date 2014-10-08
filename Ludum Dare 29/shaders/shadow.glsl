vec3 lightdir = /*mat3( view * world ) * */normalize(LightSource[ 0 ].position.xyz); //Lichtvektor in den Modelsprace rotieren  
vec3 vertex =  ( view * world * vec4( in_vertex, 1 )).xyz; //Vertex in den Modelspace projizieren	  
vec2 pos = vertex.xz + lightdir.xz * -vertex.y/lightdir.y; //Schnittpunkt mit der XZ-Referenzebene
	  
pos = pos / (abs(pos)+1.0); //Projektion der Ebene auf ein Quadrat
pos = pos * vec2(1.0,1.8) + vec2(0.0,0.8); //Abschneiden des Bereiches hinter der Kamera

/*
    vec4 shadowcol;
    if (texture(shadow, spos.xy).r > spos.z ){
          //Licht
          shadowcol = vec4(1.0,1.0,1.0,1.0);
          }
    else{
          //Schatten
          shadowcol = vec4(0.5,0.5,0.5,1.0);
          }    
*/
