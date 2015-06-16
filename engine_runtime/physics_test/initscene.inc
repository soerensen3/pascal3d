type

  { TPhysicObject }

  TPhysicObject = object
    Mdl: TP3DScene;
    Body: TDynamicBody;
    matInitial: TMat4;

    procedure Render;
    procedure ForceAndTorque( Sender: TDynamicBody; TimeStep: Single );
  end;

  { THeightMap }
{
  THeightMap = object
    Map: TSDLSurface;
    Mdl: TModel;
    Body: TDynamicBody;

    procedure LoadHeightMap( FName: String; ScaleXY: Float; ScaleZ: Float );
  end;}

  { TMeshPhysics }

  TMeshPhysics = object
    Mesh: TP3DScene;
    Mdl: TP3DScene;
    Body: TDynamicBody;

    procedure Render;
    procedure UpdateCollision;
  end;

var
  mainscene: tScene;
  maincamera: tCamera;

  Physics: TPhysicsEngine;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../physics_test/media';
  pathfonts: String = '../../../fonts';

  WatchTime: Integer;
  LastTime: Integer;

  cubes: array of TPhysicObject;
  cubeMdl: TP3DScene;
  spheres: array of TPhysicObject;
  sphereMdl: TP3DScene;
  plane: TPhysicObject;
  Watch_Mdl: TFileWatch;
  lightbulb: TP3DScene;
  terrain: TMeshPhysics;
  character: TCharacterController;

  terrainshader: TShader;
  simpleshader: TShader;
  skyshader: TShader;
  Watch_simpleshader_V: TFileWatch;
  Watch_simpleshader_F: TFileWatch;

  mrot: Integer;
  mshiftx: Integer;
  mshifty: Integer;

  CatchMouse: Boolean = False;// True;


function LoadShader: Boolean;
begin
  Result:= False;
  SetCurrentDir( pathshaders );

  terrainshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'landscape.vert' ), LoadShaderToText( 'landscape.frag' ));
  skyshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'skybox.vert' ), LoadShaderToText( 'skybox.frag' ));

//  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'shader.vert' ), LoadShaderToText( 'shader.frag' ));
//  shadowshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'parabol_shadow.vert' ), LoadShaderToText( 'parabol_shadow.frag' ));
  Watch_simpleshader_V:= TFileWatch.Create( pathshaders + '/simple.vert' );
  Watch_simpleshader_F:= TFileWatch.Create( pathshaders + '/simple.frag' );

  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' ));;
  InitGeom( CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )),
            CreateVertexAndFragmentShader( LoadShaderToText( 'default.vert' ), LoadShaderToText( 'default.frag' )));
  if ( Assigned( simpleshader )) then
    mainscene.Shader:= simpleshader;
end;

procedure UnloadShader;
begin
  LineShader2D.Free;
  LineShader3D.Free;
  simpleshader.Free;
  terrainshader.Free;
  Watch_simpleshader_F.Free;
  Watch_simpleshader_V.Free;
end;

function LoadModels: Boolean;
var
  i: Integer;
begin
  Result:= False;
  SetCurrentDir( pathmodels );

//  Mdl:= LoadModelFileFromFile( 'moon.model' );
  cubeMdl:= LoadModelFileFromFile( 'cube.model' );
  plane.Mdl:= LoadModelFileFromFile( 'plane.model' );
  sphereMdl:= LoadModelFileFromFile( 'sphere.model' );
  terrain.Mesh:= LoadModelFileFromFile( 'terrain_collision.model' );
  terrain.Mdl:= LoadModelFileFromFile( 'landscape_high.model' );
end;

procedure UnloadModels;
begin
  cubeMdl.Free;
  plane.Mdl.Free;
  sphereMdl.Free;
  terrain.Mdl.Free;
  terrain.Mesh.Free;
end;

procedure LoadTextures;
begin

end;


procedure InitPhysics;
var
  mat: TMat4;
  plane_v: array [ 0..3 ] of TVec3;
  i: Integer;
begin
  Physics:= TPhysicsEngine.Create();
//  cube.Collision:= NewtonCreateBox( Physics.World, 10, 10, 10, 0, nil );
//  mat:= Mat4Identity;
//  cube.Body:= NewtonCreateDynamicBody( Physics.World, cube.Collision, @mat );
//  NewtonBodySetForceAndTorqueCallback( cube.Body, PNewtonApplyForceAndTorque( @TempForceAndTorque ));
//  NewtonBodySetMassProperties( cube.Body, 1.0, cube.Collision );
//  NewtonDestroyCollision( cube.Collision );

{  plane_v[ 0 ]:= vec3( -100.0,  100.0, 0 );
  plane_v[ 1 ]:= vec3( -100.0, -100.0, 0 );
  plane_v[ 2 ]:= vec3(  100.0, -100.0, 0 );
  plane_v[ 3 ]:= vec3(  100.0,  100.0, 0 );

  plane.Body:= TDynamicBody.Create( Physics );
  plane.Body.OnForceAndTorque:= nil;
  plane.matInitial:= MatrixScale( vec4( 1000 ));
  plane.Body.Collision:= NewtonCreateTreeCollision( Physics.World, 0 );
  NewtonTreeCollisionBeginBuild( plane.Body.Collision );
  plane_v[ 0 ]:= vec3( -100.0, -100.0, 0 );
  plane_v[ 1 ]:= vec3( -100.0,  100.0, 0 );
  plane_v[ 2 ]:= vec3(  100.0,  100.0, 0 );
  plane_v[ 3 ]:= vec3(  100.0, -100.0, 0 );
  NewtonTreeCollisionAddFace( plane.Body.Collision, Length( plane_v ), @plane_v, SizeOf( TVec3 ), 0 );
  plane_v[ 0 ]:= vec3( -100.0, -100.0, 0 );
  plane_v[ 1 ]:= vec3( -100.0,  100.0, 0 );
  plane_v[ 2 ]:= vec3(  100.0,  100.0, 0 );
  plane_v[ 3 ]:= vec3(  100.0, -100.0, 0 );
  NewtonTreeCollisionAddFace( plane.Body.Collision, Length( plane_v ), @plane_v, SizeOf( TVec3 ), 0 );
  NewtonTreeCollisionEndBuild( plane.Body.Collision, 1 );}
{  plane.Body:= TBodyCube.Create( Physics );
  TBodyCube( plane.Body ).SetLengths( vec3( 100, 100, 1 ));
  plane.matInitial:= MatrixScale( vec4( 100 ));
  plane.Body.Mass:= 0.0;
  plane.Body.SetPosition( 0, 0, -0.5 );}

  SetLength( cubes, 10 );
  for i:= 0 to 9 do
    begin
      cubes[ i ].Body:= TBodyCube.Create( Physics );
      cubes[ i ].Body.OnForceAndTorque:= @cubes[ i ].ForceAndTorque;
      TBodyCube( cubes[ i ].Body ).SetLengths( vec3( 1 ));
      cubes[ i ].matInitial:= mat4scale( vec4( 1 ));
      cubes[ i ].Body.Mass:= 10.0;
      cubes[ i ].Body.SetPosition( Random( 50 ), Random( 50 ), 150 + Random( 50 ));
      NewtonBodySetLinearDamping ( cubes[ i ].body.Body, 0.0);
      cubes[ i ].Mdl:= cubeMdl;
    end;

  SetLength( spheres, 10 );
  for i:= 0 to 9 do
    begin
      spheres[ i ].Body:= TBodySphere.Create( Physics );
      spheres[ i ].Body.OnForceAndTorque:= @spheres[ i ].ForceAndTorque;
      TBodySphere( spheres[ i ].Body ).SetRadius( 1 );
      spheres[ i ].matInitial:= mat4scale( vec4( 1 ));
      spheres[ i ].Body.Mass:= 10.0;
      spheres[ i ].Body.SetPosition( Random( 50 ), Random( 50 ), Random( 50 ));
      spheres[ i ].Mdl:= sphereMdl;
    end;

//  terrain.LoadHeightMap( pathmodels + '/displace.jpg', 100, 0.0001 );
//  terrain.Mdl.Material:= cubes[ 0 ].Mdl.Materials[ 0 ];
  terrain.Body:= TBodyMesh.Create( Physics );
  terrain.UpdateCollision;

  character:= TCharacterController.Create( Physics );
  character.Body.SetPosition( 0, 0, 100 );
  character.OnForceAndTorque:= @cubes[ 0 ].ForceAndTorque; // workaround
  character.Body.Mass:= 10;
  character.WalkSpeed:= 50;
end;

procedure DeInitPhysics;
var
  cube: TPhysicObject;
  sphere: TPhysicObject;
begin
  for cube in cubes do
    cube.Body.Free;

  terrain.Body.Free;

//  plane.Body.Free;

  for sphere in spheres do
    sphere.Body.Free;

  character.Free;

  Physics.Free;
end;

procedure Init( Sender: TSDLWindow );
begin
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  Sender.Width:= 800;
  Sender.Height:= 600;

  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) + '/../../../fonts' );
//  fntMain := font_LoadFromFile( 'Calibri-Regular-36pt.zfi' );
  mainscene:= tScene.Create;
  maincamera:= tCamera.Create;
  maincamera.Position:= vec3( 4, -1.2+0.05, -4.5 );
//  maincamera.YawPitchRoll.X:= -120;
  maincamera.far:= 1000;

  LoadModels;
  InitPhysics;
  LoadTextures;
  LoadShader;



  mainscene.DrawObjects:= @DrawObjects;
  mainscene.Cam:= maincamera;

  SDL_SetRelativeMouseMode( TSDL_Bool( CatchMouse ));

  WatchTime:= GetTickCount;
end;

procedure DeInit( Sender: TSDLWindow );
begin
  mainscene.Free;
  maincamera.Free;

  DeInitPhysics;

  UnloadShader;
  UnloadModels;
end;

{ THeightMap }

//procedure CreateHeightFieldMesh( collision: NewtonCollision; Mesh: TModel ); forward;

{ TMeshPhysics }

procedure TMeshPhysics.Render;
var
  cell: Integer;
begin
  cell:= 32;

  Mesh.Render( Body.GetMatrix(), [ rfWireFrame ]);

  if ( Assigned( terrainshader )) then
    terrainshader.Enable
  else
    exit;
//  maincamera.UpdateMatrices;
  glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'view'), 1, False, @maincamera.view );
  glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'proj'), 1, False, @maincamera.proj );


  glUniform1i( ActShad.Uniforms.AddrByName( 'heightMap' ), 0 );
  glUniform1i( ActShad.Uniforms.AddrByName( 'multilayer' ), 1 );
  glUniform1i( ActShad.Uniforms.AddrByName( 'lightmap' ), 2 );

  glUniform1i( ActShad.Uniforms.AddrByName( 'uvX' ), Round( Int( -maincamera.Position.X / cell )));
  glUniform1i( ActShad.Uniforms.AddrByName( 'uvY' ), Round( Int( -maincamera.Position.Y / cell )));
  glUniform1f( ActShad.Uniforms.AddrByName( 'height' ), 4 );
  glUniform1i( ActShad.Uniforms.AddrByName( 'cellNumber' ), 64 );

  //mainwnd.Title:= Format( 'uvX: %f, uvY: %f, mrot: %d, mx: %d, my: %d', [ Int( -maincamera.Position.X / cell ) * cell, Int( maincamera.Position.Y / cell ) * cell, mrot, mshiftx, mshifty ]);
  Mdl.Render(
    mat4translate( vec4(
         Int( maincamera.Position.X / cell ) * cell,
         Int( maincamera.Position.Y / cell ) * cell,
      0, 1 )), []); //RenderFlags );
end;

procedure TMeshPhysics.UpdateCollision;
begin
  if ( Mesh.Children.Count > 0 ) then
    with ( Mesh.Children[ 0 ] as TModel ) do
      begin
        TBodyMesh( Body ).UpdateMesh( Indices, Positions, Matrix );
        Body.SetMatrix( mat4( 1 ));
      end;
end;
{
procedure THeightMap.LoadHeightMap(FName: String; ScaleXY: Float; ScaleZ: Float );
var
  sx, sy: Single;
  DataArray: array of ShortInt;
  attributes: array of ShortInt;
  pixels: PByte;
  x: Integer;
  y: Integer;
  Raw: THandle;
begin
  if ( not FileExists( FName )) then
    raise Exception.Create( Format( 'Could not create heightmap. The specified file "%s" was not found!', [ FName ]));

//  Map:= TSDLSurface.Create( FName );

  sx:= ScaleXY / 256; //Map.fSurface^.w;
  sy:= ScaleXY / 256; //Map.fSurface^.h;

  Body:= TDynamicBody.Create( Physics );

  SetLength( DataArray, 256 * 256 ); //Map.fSurface^.w * Map.fSurface^.h );
  SetLength( attributes, Length( DataArray ));
  FillByte( attributes[ 0 ], Length( attributes ), 0 );

  Raw:= FileOpen( 'h2.raw', fmOpenRead );
  FileRead( Raw, DataArray[ 0 ], 256 * 256 );
  FileClose( Raw );

{  SDL_LockSurface( Map.fSurface );
  pixels:= Map.fSurface^.pixels;
  for y:= 0 to Map.fSurface^.h -1 do
    for x:= 0 to Map.fSurface^.w -1 do
      begin
        DataArray[ x + y * Map.fSurface^.w ]:= 0;//pixels^ * ( High( SmallInt ) div 255 );
        Inc( pixels, Map.fSurface^.format^.BytesPerPixel );
      end;
  SDL_UnlockSurface( Map.fSurface );
  Map.Free;}

  Body.Collision:= NewtonCreateHeightFieldCollision( Physics.World, {Map.fSurface^.w, Map.fSurface^.h} 256, 256, 0, 1, @DataArray[ 0 ], @attributes[ 0 ], 1, 4, 0 );// ScaleZ, sx, 0 );
  NewtonBodySetCollision( Body.Body, Body.Collision );
  Body.Mass:= 0.0;
  Mdl:= TModel.Create;
  CreateHeightFieldMesh( Body.Collision, Mdl );
end;

procedure CreateHeightFieldMesh( collision: NewtonCollision; Mesh: TModel );
var
  collisionInfo: TNewtonCollisionInfoRecord;
  width: Integer;
  height: Integer;
  elevations: PdFloat;
  vScale: dFloat;
  hScale: dFloat;
  y: Integer;
  x0: Integer;
  x1: Integer;
  y0: Integer;
  y1: Integer;
  x: Integer;
  p0: TVec3;
  p1: TVec3;
  x10: TVec3;
  q0: TVec3;
  q1: TVec3;
  y10: TVec3;
  normal: TVec3;
  point: TVec3;
  index: Integer;
  vertexIndex: Integer;
begin
  // keep the compiler happy
//  memset (&collisionInfo, 0, sizeof (NewtonCollisionInfoRecord));
  NewtonCollisionGetInfo (collision, @collisionInfo);

  // get the info from the collision mesh and create a visual mesh
  width:= collisionInfo.sdHeightField.m_width;
  height:= collisionInfo.sdHeightField.m_height;
  elevations:= collisionInfo.sdheightField.m_elevation;
  vScale:= collisionInfo.sdHeightField.m_verticalScale;
  hScale:= collisionInfo.sdHeightField.m_horizonalScale;

  // allocate space to store vertex data
{  ent->m_vertexCount = width * height;
  ent->m_vertex = (dFloat*) malloc (3 * width * height * sizeof (dFloat));
  ent->m_normal = (dFloat*) malloc (3 * width * height * sizeof (dFloat));
  ent->m_uv = (dFloat*) malloc (2 * width * height * sizeof (dFloat));}



  // scan the height field and convert every cell into two triangles
  for y:= 0 to height - 1 do
    begin
      if ((y - 1) < 0) then
        y0:= 0
      else
        y0:= y - 1;
      if ((y + 1) > (height - 1)) then
        y1:= height - 1
      else
        y1:= y + 1;
      for x:= 0 to width - 1 do
        begin
          if (( x - 1 ) < 0 ) then
            x0:= 0
          else
            x0:= x - 1;

          if (( x + 1 ) > ( width - 1 )) then
            x1:= width - 1
          else
            x1:= x + 1;

	  p0:= vec3( hScale * x0, hScale * y, elevations[ y * width + x1 ] * vScale );
	  p1:= vec3( hScale * x1, hScale * y, elevations[ y * width + x0 ] * vScale );
	  x10:= ( p1 - p0 );

	  q0:= vec3(hScale * x, hScale * y0, elevations[ y0 * width + x ] * vScale );
	  q1:= vec3(hScale * x, hScale * y1, elevations[ y1 * width + x ] * vScale );
	  y10:= ( q1 - q0 );

	  normal:= VecNormalize( VecCrossProduct( y10, x10 ));
//	  normal:= normal * ( sqrt( 1.0 / ( VecDotProduct( normal, normal ))));
	  point:= vec3( hScale * x, hScale * y, elevations[ y * width + x ] * vScale );

	  Mesh.Positions.Add( point );
          Mesh.Normals.Add( normal );
          //[(y * width + x) * 3 + 0] = point.m_x;
	  //ent->m_vertex[(y * width + x) * 3 + 1] = point.m_y;
	  //ent->m_vertex[(y * width + x) * 3 + 2] = point.m_z;

	  //ent->m_normal[(y * width + x) * 3 + 0] = normal.m_x;
	  //ent->m_normal[(y * width + x) * 3 + 1] = normal.m_y;
	  //ent->m_normal[(y * width + x) * 3 + 2] = normal.m_z;

	  //ent->m_uv[(y * width + x) * 2 + 0] = x * TEXTURE_SCALE;
	  //ent->m_uv[(y * width + x) * 2 + 1] = y * TEXTURE_SCALE;
        end;
    end;

  // since the bitmap sample is 256 x 256, i fix into a single 16 bit index vertex array with
//  ent->m_subMeshCount = 1;
//  ent->m_subMeshes = (Entity::SubMesh*) malloc (sizeof (Entity::SubMesh));

  // allocate space to the index list
//  ent->m_subMeshes[0].m_textureHandle = LoadTexture ("grassAndDirt.tga");
//  ent->m_subMeshes[0].m_indexCount = (width - 1) * (height - 1) * 6;
//  ent->m_subMeshes[0].m_indexArray = (unsigned short*) malloc (ent->m_subMeshes[0].m_indexCount * sizeof (unsigned short));

  // now following the grid pattern and create and index list
  index:= 0;
  vertexIndex:= 0;
  SetLength( Mesh.Faces, ( height ) * ( width ));
  for y:= 0 to height - 2 do
    begin
      vertexIndex:= y * width;
      for x:= 0 to width - 2 do
        begin
          if ( vertexIndex > high( Mesh.Faces )) then
            raise Exception.Create( 'Error' );
          SetLength( Mesh.Faces[ vertexIndex ].verts, 6 );
          {
          Mesh.Faces[ vertexIndex ].verts[ 0 ].v:= index;
          Mesh.Faces[ vertexIndex ].verts[ 0 ].n:= index;
          Mesh.Faces[ vertexIndex ].verts[ 1 ].v:= index + 1;
          Mesh.Faces[ vertexIndex ].verts[ 1 ].n:= index + 1;
          Mesh.Faces[ vertexIndex ].verts[ 2 ].v:= index + 2;
          Mesh.Faces[ vertexIndex ].verts[ 2 ].n:= index + 2;
          Mesh.Faces[ vertexIndex ].verts[ 3 ].v:= index + 3;
          Mesh.Faces[ vertexIndex ].verts[ 3 ].n:= index + 3;
          Mesh.Faces[ vertexIndex ].verts[ 4 ].n:= index + 4;
          Mesh.Faces[ vertexIndex ].verts[ 4 ].v:= index + 4;
          Mesh.Faces[ vertexIndex ].verts[ 5 ].n:= index + 5;
          Mesh.Faces[ vertexIndex ].verts[ 5 ].n:= index + 5;}

//	  ent->m_subMeshes[0].m_indexArray[index + 0] = GLushort (vertexIndex);
//	  ent->m_subMeshes[0].m_indexArray[index + 1] = GLushort (vertexIndex + width);
//	  ent->m_subMeshes[0].m_indexArray[index + 2] = GLushort (vertexIndex + 1);
          Mesh.Indices.Add([ vertexIndex, vertexIndex + width, vertexIndex + 1 ]);
	  index += 3;
          Mesh.Indices.Add([ vertexIndex + 1, vertexIndex + width, vertexIndex + width + 1 ]);

//	  ent->m_subMeshes[0].m_indexArray[index + 0] = GLushort (vertexIndex + 1);
//	  ent->m_subMeshes[0].m_indexArray[index + 1] = GLushort (vertexIndex + width);
//	  ent->m_subMeshes[0].m_indexArray[index + 2] = GLushort (vertexIndex + width + 1);
	  index += 3;
	  Inc( vertexIndex );
        end;
    end;
  Mesh.CalcBoundingBox;
  Mesh.UnpackBuffers;
  WriteLn( 'Mesh.Positions.Count = ', Mesh.Positions.Count );
  WriteLn( 'Mesh.Normals.Count = ', Mesh.Normals.Count );
  WriteLn( 'Mesh.BoundingBox.Max = ', Mesh.BoundingBox.Max.ToString );
  WriteLn( 'Mesh.BoundingBox.Center = ', Mesh.BoundingBox.Center.ToString );
  WriteLn( 'Mesh.BoundingBox.Min = ', Mesh.BoundingBox.Min.ToString );

  // Optimize the mesh for hardware rendering if possible
//  ent->OptimizeMesh();

{
	dVector boxP0;
	dVector boxP1;
	// get the position of the aabb of this geometry
	dMatrix matrix (ent->m_curRotation, ent->m_curPosition);
	NewtonCollisionCalculateAABB (collision, &matrix[0][0], &boxP0.m_x, &boxP1.m_x);

	// place the origin of the visual mesh at the center of the height field
	matrix.m_posit = (boxP0 + boxP1).Scale (-0.5f);
	matrix.m_posit.m_w = 1.0f;
	ent->m_curPosition = matrix.m_posit;
	ent->m_prevPosition = matrix.m_posit;


	// create the level rigid body
	body = NewtonCreateBody(world, collision);

	// release the collision tree (this way the application does not have to do book keeping of Newton objects
	NewtonReleaseCollision (world, collision);


	// save the pointer to the graphic object with the body.
	NewtonBodySetUserData (body, ent);

	// set the global position of this body
	NewtonBodySetMatrix (body, &matrix[0][0]);


	// set the destructor for this object
//	NewtonBodySetDestructorCallback (body, Destructor);

	// get the position of the aabb of this geometry
	NewtonCollisionCalculateAABB (collision, &matrix[0][0], &boxP0.m_x, &boxP1.m_x);

	// add some extra padding the world size
	boxP0.m_x -=  10.0f;
	boxP0.m_y -=  10.0f;
	boxP0.m_z -=  10.0f;
	boxP1.m_x +=  10.0f;
	boxP1.m_y += 400.0f;
	boxP1.m_z +=  10.0f;

	// set the world size
	NewtonSetWorldSize (world, &boxP0.m_x, &boxP1.m_x);
	return body;
}
end;
}
{ TPhysicObject }

procedure TPhysicObject.Render;
var
  mat: TMat4;
begin
  mat:= Body.GetMatrix();
  Mdl.Render( matInitial * mat );
end;

procedure TPhysicObject.ForceAndTorque(Sender: TDynamicBody; TimeStep: Single);
var
  Ix, Iy, Iz, Mass: Float;
begin
  NewtonBodyGetMassMatrix( Sender.Body, @Mass, @Ix, @Iy, @Iz );
  Sender.AddForce( vec3( 0, 0, -9.8 * Mass ));
end;
