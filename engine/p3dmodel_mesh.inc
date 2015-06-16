{$IFDEF INTERFACE}
  { TP3DMesh }

  type
    TP3DMesh = class( TRenderableObject )
      private
        FBoundingBox: TBoundingBox;

        procedure LoadMaterialFromDOM( DOM: TDOMElement; Scene: TP3DScene );

      public
        Positions: TP3DVec3BufferGL;
        Normals: TP3DVec3BufferGL;
        Tangents: TP3DVec3BufferGL;
        Binormals: TP3DVec3BufferGL;
        Faces: TP3DFaceArray;
        TexCoords: TP3DVec2BufferGL;
        Indices: TP3DIntBufferGL;
        Matrix: TMat4;
        Material: TP3DMaterial;
        testTex: Integer;
        VBArray: TP3DVertexBufferArray;

        constructor Create(AParentList: TObjectList);
        constructor CreateFromDOM( AParentList: TObjectList; Scene: TP3DScene; ADOMNode: TDOMElement );

        destructor Destroy; override;

        procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []); override;
        procedure UnpackBuffers;
        procedure ClearVBO;
        procedure CreateVBO;
        procedure Calc_Tangent_Binormal;
        procedure CalcBoundingBox;
        procedure ClearChildren;

        property BoundingBox: TBoundingBox read FBoundingBox write FBoundingBox;
    end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DMesh }

function LoadMat4FromDOM( DOM: TDOMElement ): TMat4;
var
  c0, c1, c2, c3: TVec4;
begin
  c0.FromString( DOM.GetAttribute( 'c0' ));
  c1.FromString( DOM.GetAttribute( 'c1' ));
  c2.FromString( DOM.GetAttribute( 'c2' ));
  c3.FromString( DOM.GetAttribute( 'c3' ));
  Result:= mat4( c0, c1, c2, c3 );
end;

procedure LoadFaceFromStream( Faces: PP3DFaceArray; S: TStream; ACount: Integer );
var
  i: Integer;
  j: Integer;
  k: Integer;
  c, tc: Integer;
  base: Integer;
begin
  base:= Length( Faces^ );
  SetLength( Faces^, base + ACount );
  for i:= 0 to ACount - 1 do
    begin
      S.Read( c, SizeOf( c ));
      S.Read( tc, SizeOf( tc ));

      SetLength( Faces^[ i + base ].verts, c );
      for j:= 0 to c - 1 do
        begin
          SetLength( Faces^[ i + base ].verts[ j ].texc, tc );
          S.Read( Faces^[ i + base ].verts[ j ].v, SizeOf( Integer ));
          S.Read( Faces^[ i + base ].verts[ j ].n, SizeOf( Integer ));
          for k:= 0 to tc - 1 do
            S.Read( Faces^[ i + base ].verts[ j ].texc[ k ], SizeOf( Integer ));
        end;
    end;
end;

procedure LoadVec3FromStream( Vectors: TVec4List; S: TStream; ACount: Integer );
var
  i: Integer;
  vert: TVec4;
begin
  for i:= 0 to ACount - 1 do
    begin
      S.Read( vert, 4*SizeOf( Single ));
      Vectors.Add( vert );
    end;
end;

procedure LoadVec3FromStream( Vectors: TVec3List; S: TStream; ACount: Integer );
var
  i: Integer;
  vert: TVec3;
begin
  for i:= 0 to ACount - 1 do
    begin
      S.Read( vert, 3*SizeOf( Single ));
      Vectors.Add( vert );
    end;
end;

procedure LoadVec2FromStream( Vectors: TVec2List; S: TStream; ACount: Integer );
var
  i: Integer;
  vert: TVec2;
begin
  for i:= 0 to ACount - 1 do
    begin
      S.Read( vert, 2*SizeOf( Single ));
      Vectors.Add( vert );
    end;
end;
procedure TP3DMesh.ClearChildren;
begin
  FChildren.Clear;
end;

procedure TP3DMesh.LoadMaterialFromDOM(DOM: TDOMElement; Scene: TP3DScene);
var
  MatIdx: Integer;
  AName: String;
begin
  AName:= DOM.GetAttribute( 'name' );
  MatIdx:= Scene.Materials.FindByName( AName );
  if ( MatIdx >= 0 ) then
    Material:= Scene.Materials[ MatIdx ]
  else
    begin
      Material:= TP3DMaterial.Create;
      Material.Name:= AName;
      Scene.Materials.Add( Material );
    end;
end;

constructor TP3DMesh.Create( AParentList: TObjectList );
begin
  inherited;

  Positions:= TP3DVec3BufferGL.Create( True );
  Normals:= TP3DVec3BufferGL.Create( True );
  Binormals:= TP3DVec3BufferGL.Create( True );
  Tangents:= TP3DVec3BufferGL.Create( True );
  Indices:= TP3DIntBufferGL.Create( True );
  TexCoords:= TP3DVec2BufferGL.Create( True );
  VBArray:= TP3DVertexBufferArray.Create;

  FVisible:= True;
end;

constructor TP3DMesh.CreateFromDOM(AParentList: TObjectList; Scene: TP3DScene;
  ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  Ext: String;
  Idx: Integer;
  F: TFileStream;
begin
  Create( AParentList );
  Name:= ADOMNode.GetAttribute( 'name' );
  if ( ADOMNode.hasAttribute( 'binary' )) then
    begin
      Ext:= ADOMNode.GetAttribute( 'binary' );
      F:= TFileStream.Create( Ext, fmOpenRead );
      try
        Idx:= StrToInt( ADOMNode.GetAttribute( 'vertices' ));
        LoadVec3FromStream( Positions, F, Idx );
        Idx:= StrToInt( ADOMNode.GetAttribute( 'normals' ));
        LoadVec3FromStream( Normals, F, Idx );
        Idx:= StrToInt( ADOMNode.GetAttribute( 'texcoords' ));
        LoadVec2FromStream( TexCoords, F, Idx );
        Idx:= StrToInt( ADOMNode.GetAttribute( 'faces' ));
        LoadFaceFromStream( @Faces, F, Idx );
      finally
        F.Free;
      end;
    end;

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'matrix': Matrix:= LoadMat4FromDOM( Element );
        'material': LoadMaterialFromDOM( Element, Scene );
        'mesh': Children.Add( TP3DMesh.CreateFromDOM( Children, Scene, Element ));
      else
        raise Exception.Create( 'Unknown tag inside Mesh Element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  Calc_Tangent_Binormal;
  UnpackBuffers;
end;

destructor TP3DMesh.Destroy;
begin
  Positions.Free;
  Normals.Free;
  Binormals.Free;
  Tangents.Free;
  Indices.Free;
  TexCoords.Free;
  VBArray.Free;

  inherited Destroy;
end;

procedure TP3DMesh.Render( world: TMat4; const RenderFlag: TRenderFlags = []);
var
  i: Integer;
  j: Integer;
  matId: Integer;
  vNorm, Vec2: TVec3;
  _world, view, proj: TMat4;
  k: Integer;
  normal, vertex, color: GLint;
  n: Integer;
  _worldview: TMat4;
  p: TVec3;
  BackupShader: TShader;
  //Res: Boolean;

  function SetShader( Shader: TShader ): Boolean;
  begin
    Result:= False;
    if ( Shader = nil ) then
      exit;
    if ( Shader <> ActShad ) then
      Shader.Enable;
    Result:= True;
  end;

begin
  if ( not Visible ) then
    exit;
  if ( Assigned( Material )) then
    begin
      glVertexAttrib4f( P3DAttribColor,
        Material.Diff.R, Material.Diff.G, Material.Diff.B, 1.0 );
      glUniform4f( ActShad.Uniforms.AddrByName( 'mat_diffuse' ), Material.Diff.r, Material.Diff.g, Material.Diff.b, 1 );
      glUniform4f( ActShad.Uniforms.AddrByName( 'mat_specular' ), Material.Spec.r, Material.Spec.g, Material.Spec.b, 1 );
      glUniform1f( ActShad.Uniforms.AddrByName( 'mat_hardness' ), Material.Spec_Hardness );

      glUniform1i( ActShad.Uniforms.AddrByName( 'numMaps' ), Material.NumMaps );
      if ( Material.NumMaps > 0 ) then
        begin
          for j:= 0 to Material.NumMaps - 1 do
            begin
              glActiveTexture( GL_TEXTURE0 + j );
              glEnable( GL_TEXTURE_2D );
              glBindTexture( GL_TEXTURE_2D, Material.Maps[ j ].Map.fGLTexture );
              glUniform1i( ActShad.Uniforms.AddrByName( 'tex' + IntToStr( j )), j );


              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
            end;

        end
      else
        begin
{          glColor3f( 1.0, 1.0, 1.0 );
          glActiveTexture( GL_TEXTURE0 );
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, testTex );
          glActiveTexture( GL_TEXTURE1 );
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, testTex );
          {$IFDEF DEFAULT_TEX}
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, DefaultTex^.ID );
          {$ENDIF}}
        end;
    end
  else
    begin
      {glColor3f( 1.0, 1.0, 1.0 );
      glActiveTexture( GL_TEXTURE0 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, testTex );
      glActiveTexture( GL_TEXTURE1 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, testTex );}
      glVertexAttrib4f( P3DAttribColor,
      1, 1, 1, 1.0 );
    end;
  _world:= Matrix * world;

  //VBArray.Bind;  //Workaround for VAO not really working
  if ( Positions.Count > 0 ) then
    Positions.SetAttribArray( P3DAttribPosition );
  if ( Normals.Count > 0 ) then
    Normals.SetAttribArray( P3DAttribNormal );
  if ( Tangents.Count > 0 ) then
    Tangents.SetAttribArray( P3DAttribTangent );
   if ( TexCoords.Count > 0 ) then
    TexCoords.SetAttribArray( P3DAttribTexCoord0 );

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );

  glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'world' ), 1, False, @_world );

  {if ( rfWireFrame in RenderFlag ) then
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );}
  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 ));
  {
  if ( rfDebugShowLocation in RenderFlag ) then
    begin
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'view' ), @view );
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'proj' ), @proj );

      BackupShader:= ActShad;

      Setup3D( view * proj );
      p:= vec3( vec4( BoundingBox.Center, 1 ) * world );

      RenderLines3D([ p, vec3( 0 )], vec4( 1, 0, 0, 1 ));

      BackupShader.Enable;
    end;
  if ( rfDebugShowBoundingBox in RenderFlag ) then
    begin
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'view' ), @view );
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'proj' ), @proj );

      BackupShader:= ActShad;

      Setup3D( view * proj * world );
      with ( BoundingBox ) do
      RenderLines3D([
         Min, vec3( Min.x, Min.y, Max.Z ),
         vec3( Max.x, Min.y, Max.z ), Max
         ], vec4( 1, 0, 0, 1 ));

      BackupShader.Enable;
    end;
  }
  Children.Render( _world );
  glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'world'), 1, False, @_world );
  if ( rfWireFrame in RenderFlag ) then
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
end;

procedure TP3DMesh.UnpackBuffers;
var
  i: Integer;
  j: Integer;
  ibase: Integer;
  vbase: Integer;
  N: TVec3;
  T: TVec3;
  B: TVec3;

  _Positions,
  _Tangents,
  _Binormals,
  _Normals: TP3DVec3BufferGL;
  _Texures: TP3DVec2BufferGL;

  numTex: Integer;
  k: Integer;
  baseTex: Integer;

begin
  ClearVBO;
  _Positions:= TP3DVec3BufferGL.Create( True );
  _Tangents:= TP3DVec3BufferGL.Create( True );
  _Binormals:= TP3DVec3BufferGL.Create( True );
  _Normals:= TP3DVec3BufferGL.Create( True );
  _Texures:= TP3DVec2BufferGL.Create( True );

  baseTex:= 0;

  if (( Length( Faces ) > 0 ) AND ( Length( Faces[ 0 ].verts ) > 0 )) then
    numTex:= Length( Faces[ 0 ].verts[ 0 ].texc )
  else
    numTex:= 0;

  for i:= 0 to high( Faces ) do
    begin
      vbase:= _Positions.Count;
      _Positions.Count:= vbase + Length( Faces[ i ].verts );
      ibase:= Indices.Count;
      Indices.Count:= ibase + ( Length( Faces[ i ].verts ) - 2 ) * 3;

      for j:= 0 to high( Faces[ i ].verts ) do
        begin
          _Positions[ vbase + j ]:= Positions[ Faces[ i ].verts[ j ].v ];
          Faces[ i ].verts[ j ].v:= vbase + j;
          if ( Faces[ i ].verts[ j ].n >= 0 ) then
            begin
              _Normals.Count:= _Positions.Count;
              _Normals [ vbase + j ]:= Normals[ Faces[ i ].verts[ j ].n ];
            end;

          if ( Faces[ i ].verts[ j ].t >= 0 ) then
            begin
              _Tangents.Count:= _Positions.Count;
              _Tangents[ vbase + j ]:= Tangents[ Faces[ i ].verts[ j ].t ];
            end;

          if ( Faces[ i ].verts[ j ].b >= 0 ) then
            begin
              _Binormals.Count:= _Positions.Count;
              _Binormals[ vbase + j ]:= Binormals[ Faces[ i ].verts[ j ].b ];
            end;

          if ( numTex > 0 ) then
            begin
              baseTex:= _Texures.Count;
              _Texures.Count:= _Texures.Count + numTex;
              for k:= 0 to numTex - 1 do
                _Texures[ baseTex + k ]:= TexCoords[ Faces[ i ].verts[ j ].texc[ k ]];
            end;

          if ( j >= 2 ) then
            begin
              Indices[ ibase + ( j - 2 ) * 3 + 0 ]:= vbase + 0;
              Indices[ ibase + ( j - 2 ) * 3 + 1 ]:= vbase + j - 1;
              Indices[ ibase + ( j - 2 ) * 3 + 2 ]:= vbase + j;
            end;
        end;
    end;

  Positions.Free;
  Positions:= _Positions;
  Normals.Free;
  Normals:= _Normals;
  Tangents.Free;
  Tangents:= _Tangents;
  Binormals.Free;
  Binormals:= _Binormals;
  TexCoords.Free;
  TexCoords:= _Texures;

  CreateVBO;
end;

procedure TP3DMesh.ClearVBO;
begin
  VBArray.Free;
  VBArray:= TP3DVertexBufferArray.Create;
end;

procedure TP3DMesh.CreateVBO;
begin
  VBArray.Bind;
  if ( Positions.Count > 0 ) then
    begin
      Positions.PushData;
      Positions.SetAttribArray( P3DAttribPosition );
    end;
  if ( Normals.Count > 0 ) then
    begin
      Normals.PushData;
      Normals.SetAttribArray( P3DAttribNormal );
    end;
  if ( Binormals.Count > 0 ) then
    begin
      Binormals.PushData;
      Binormals.SetAttribArray( P3DAttribBinormal );
    end;
  if ( Tangents.Count > 0 ) then
    begin
      Tangents.PushData;
      Tangents.SetAttribArray( P3DAttribTangent );
    end;
  if ( TexCoords.Count > 0 ) then
    begin
      TexCoords.PushData;
      TexCoords.SetAttribArray( P3DAttribTexCoord0 );
    end;

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  Indices.PushData;
  VBArray.Unbind;
end;

procedure TP3DMesh.Calc_Tangent_Binormal;
  procedure CalcTriangleTB( f: Integer; v0, v1, v2: Integer );
  var
    deltaPos1: TVec3;  // Edges
    deltaPos2: TVec3;
    deltaUV1: TVec2;  // UV Directions
    deltaUV2: TVec2;
    r: Single;
    tang: TVec3;
    bin: TVec3;

    procedure CalcTriangle_TB( out tangent: TVec3; out binormal: TVec3 );
    begin
      tangent:=  ( deltaPos1 * deltaUV2.y - deltaPos2 * deltaUV1.y ) * r;
      binormal:= ( deltaPos2 * deltaUV1.x - deltaPos1 * deltaUV2.x ) * r;
    end;

  begin
    if ( Length( Faces[ f ].verts[ v1 ].texc ) <= 0 ) then //not failsafe
      begin
        Faces[ f ].verts[ v0 ].t:= -1;
        Faces[ f ].verts[ v0 ].b:= -1;
        Faces[ f ].verts[ v1 ].t:= -1;
        Faces[ f ].verts[ v1 ].b:= -1;
        Faces[ f ].verts[ v2 ].t:= -1;
        Faces[ f ].verts[ v2 ].b:= -1;
      end
    else
      begin
        // Edges of the triangle : postion delta
        if ( Length( Faces[ f ].verts ) < 3 ) then
          raise Exception.Create( 'Cannot calculate Tangents because of illegal faces!' );

        deltaPos1:= Positions[ Faces[ f ].verts[ v1 ].v ] - Positions[ Faces[ f ].verts[ v0 ].v ];
        deltaPos2:= Positions[ Faces[ f ].verts[ v2 ].v ] - Positions[ Faces[ f ].verts[ v0 ].v ];

        // UV delta
        deltaUV1:= TexCoords[ Faces[ f ].verts[ v1 ].texc[ 0 ]] - TexCoords[ Faces[ f ].verts[ v0 ].texc[ 0 ]];
        deltaUV2:= TexCoords[ Faces[ f ].verts[ v2 ].texc[ 0 ]] - TexCoords[ Faces[ f ].verts[ v0 ].texc[ 0 ]];

        r:= 1.0 / Max( 0.00001, ( deltaUV1.x * deltaUV2.y - deltaUV1.y * deltaUV2.x ));

        CalcTriangle_TB( tang, bin );
        Tangents.Add( tang );
        Binormals.Add( bin );

        Faces[ f ].verts[ v0 ].t:= Tangents.Count - 1;
        Faces[ f ].verts[ v0 ].b:= Binormals.Count - 1;
        Faces[ f ].verts[ v1 ].t:= Tangents.Count - 1;
        Faces[ f ].verts[ v1 ].b:= Binormals.Count - 1;
        Faces[ f ].verts[ v2 ].t:= Tangents.Count - 1;
        Faces[ f ].verts[ v2 ].b:= Binormals.Count - 1;
      end;
  end;

var
  i: Integer;
  j: Integer;
  flip: Boolean;
begin
  Tangents.Clear;
  Binormals.Clear;

  for i:= 0 to high( Faces ) do
    begin
      flip:= False; //For now only one tangent/binormal per face
      for j:= 0 to high( Faces[ i ].verts ) - 2 do
        begin
          if ( flip ) then
            CalcTriangleTB( i, j, j+2, j+1)
          else
            CalcTriangleTB( i, j, j+1, j+2);
          flip:= not flip;
        end;
    end;
end;

procedure TP3DMesh.CalcBoundingBox;
var
  p: TVec3;
begin
  FBoundingBox.Min:= vec3( 0 );
  FBoundingBox.Max:= vec3( 0 );
  FBoundingBox.Center:= vec3( 0 );
  for p in Positions do
    begin
      FBoundingBox.Min.X:= Min( p.X, BoundingBox.Min.X );
      FBoundingBox.Min.Y:= Min( p.Y, BoundingBox.Min.Y );
      FBoundingBox.Min.Z:= Min( p.Z, BoundingBox.Min.Z );
      FBoundingBox.Max.X:= Max( p.X, BoundingBox.Max.X );
      FBoundingBox.Max.Y:= Max( p.Y, BoundingBox.Max.Y );
      FBoundingBox.Max.Z:= Max( p.Z, BoundingBox.Max.Z );
    end;
  FBoundingBox.Center:= ( BoundingBox.Min + BoundingBox.Max ) / 2;
end;
{$ENDIF}
