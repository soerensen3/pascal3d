{$IFDEF INTERFACE}
  type
     TP3DRenderFlag = ( rfShadowMap, rfWireFrame, rfDebugShowLocation, rfDebugShowBoundingBox, rfDebugShowArmature );
     TP3DRenderFlags = set of TP3DRenderFlag;

     { TP3DMaterialMap }

     TP3DMapMode = ( p3dmmMix, p3dmmMultiply, p3dmmAdd, p3dmmSubtract );
     TP3DMaterialMap = record
       Map: TP3DTexture;
       DiffuseFactor: Single;
       NormalFactor: Single;
       SpecularFactor: Single;
       TexChannel: Integer;
       Mode: TP3DMapMode;
     end;

    { TP3DMaterial }

     TP3DMaterial = class ( TP3DDataBlock )
       public
         Maps: array [ 0..7 ] of TP3DMaterialMap;
         NumMaps: Integer;
         Diff: TVec3;
         Spec: TVec3;
         Spec_Hardness: Single;
         alpha: Real;
         Shader: TShader;

         procedure BuildShader();

         constructor Create( AParentList: TP3DObjectList );
         destructor Destroy; override;

         procedure LoadFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil ); override;
     end;

     { TP3DMaterialGroup }

     TP3DMaterialGroup = class
       PolyStart, PolyEnd,
         IndexStart, IndexEnd: Integer;
       Material: TP3DMaterial;
     end;

     TP3DMaterialGroupList = specialize gP3DCustomObjectList < TP3DMaterialGroup >;

     TP3DFaceVertex = record
       v, n, t, c: Integer;
       texc: array of Integer;
     end;

     TP3DFace = record
       verts: array of TP3DFaceVertex;
       mat: TP3DMaterial;
     end;


     PP3DFaceArray = ^TP3DFaceArray;
     TP3DFaceArray = array of TP3DFace;


     {$MACRO ON}

     TP3DCustomMaterialList = specialize gP3DCustomDatablockList < TP3DMaterial >;


     { TMaterialList }

     TP3DMaterialList = TP3DCustomMaterialList;//class( TP3DCustomMaterialList )
     //  public
     //    function FindByName( Name: String ): Integer;
     //end;

     { TP3DMesh }

    TP3DMesh = class( TP3DDataBlock )
      private
        procedure LoadMaterialFromDOM( DOM: TDOMElement; AData: TP3DData );

      public
        Positions: TP3DVec3BufferGL;
        Normals: TP3DVec3BufferGL;
        Tangents: TP3DVec3BufferGL;
        Cotangents: TP3DVec3BufferGL;
        Faces: TP3DFaceArray;
        TexCoords: array of TP3DVec2BufferGL;

        Indices: TP3DIntBufferGL;
        VBArray: TP3DVertexBufferArray;
        MaterialGroups: TP3DMaterialGroupList;

        constructor Create( AParentList: TP3DObjectList );
        constructor CreateFromDOM( AParentList: TP3DObjectList; ADOMNode: TDOMElement; const AData: TP3DData = nil );

        destructor Destroy; override;

        procedure LoadFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil ); override;
        procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor ); override;
        procedure RenderMaterialGroup( world: TMat4; Idx: Integer; Scene: TP3DScene );
        procedure UnpackBuffers;
        procedure ClearVBO;
        procedure CreateVBO;
        procedure CalcCotangentsFromTangents;
        procedure Calc_Tangents_Cotangents;
        function CalcBoundingBox(): TP3DBoundingBox; override;
//        procedure ClearChildren;
    end;
    TP3DMeshList = specialize gP3DCustomDatablockList < TP3DMesh >;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DMesh }

procedure LoadFaceFromStream( Faces: PP3DFaceArray; S: TStream; ACount: Integer );
var
  i: Integer;
  j: Integer;
  k: Integer;
  c, tc: LongInt;
  base: LongInt;
begin
  base:= Length( Faces^ );
  SetLength( Faces^, base + ACount );
  for i:= 0 to ACount - 1 do
    begin
      S.Read( c, SizeOf( c ));
      S.Read( tc, SizeOf( tc ));

      if ( c > 30 ) then
        begin
          WriteLn( 'Error: Possible data corruption in file stream "' + TFileStream( S ).FileName + '" at position ' + IntToStr( S.Position ) + '! Too many vertices in one poly!' );
          Exception.Create( 'Error: Possible data corruption in file stream "' + TFileStream( S ).FileName + '" at position ' + IntToStr( S.Position ) + '! Too many vertices in one poly!' )
        end
      else
        begin
          SetLength( Faces^[ i + base ].verts, c );
          for j:= 0 to c - 1 do
            begin
              SetLength( Faces^[ i + base ].verts[ j ].texc, tc );
              S.Read( Faces^[ i + base ].verts[ j ].v, SizeOf( Integer ));
              S.Read( Faces^[ i + base ].verts[ j ].n, SizeOf( Integer ));
              S.Read( Faces^[ i + base ].verts[ j ].t, SizeOf( Integer ));
              S.Read( Faces^[ i + base ].verts[ j ].c, SizeOf( Integer ));
              for k:= 0 to tc - 1 do
                S.Read( Faces^[ i + base ].verts[ j ].texc[ k ], SizeOf( Integer ));
            end;
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

{ TP3DDataBlock }

constructor TP3DDataBlock.Create(AParentList: TP3DObjectList);
begin
  inherited;
  FileWatch:= TP3DFileWatch.Create( '<empty>', Self );
  BoundingBox:= P3DInvalidBoundingBox;
  FUsers:= TP3DActorList.Create;
end;

destructor TP3DDataBlock.Destroy;
var
  User: TP3DActor;
begin
  for User in Users do
    User.FData:= nil;
  FUsers.Free;
  FFileWatch.Free;
  inherited Destroy;
end;

function TP3DDataBlock.CalcBoundingBox: TP3DBoundingBox;
begin
  FBoundingBox.Center:= vec3( 0 );
  FBoundingBox.Max:= vec3( 0 );
  FBoundingBox.Min:= vec3( 0 );
  Result:= BoundingBox;
end;

{
procedure TP3DMesh.ClearChildren;
begin
  FChildren.Clear;
end;
}
procedure TP3DMesh.LoadMaterialFromDOM(DOM: TDOMElement; AData: TP3DData);
var
  MatIdx: Integer;
  AName: String;
  MatPolyStart: Integer;
  MatPolyEnd: Integer;
  MaterialGroup: TP3DMaterialGroup;
begin
  if ( AData = nil ) then
    AData:= P3DData;
  AName:= DOM.GetAttribute( 'name' );
  MatIdx:= AData.Materials.FindByName( AName );
  MatPolyStart:= StrToInt( DOM.GetAttribute( 'start' ));
  MatPolyEnd:= StrToInt( DOM.GetAttribute( 'end' ));

  MaterialGroup:= TP3DMaterialGroup.Create;
  if ( MatIdx >= 0 ) then
    MaterialGroup.Material:= AData.Materials[ MatIdx ]
  else
    begin
      MaterialGroup.Material:= TP3DMaterial.Create( AData.DataBlocks );
      MaterialGroup.Material.Name:= AName;
      AData.Materials.Add( MaterialGroup.Material );
    end;
  MaterialGroup.PolyStart:= MatPolyStart;
  MaterialGroup.PolyEnd:= MatPolyEnd;
  MaterialGroups.Add( MaterialGroup );
end;

constructor TP3DMesh.Create( AParentList: TP3DObjectList );
begin
  inherited;

  Positions:= TP3DVec3BufferGL.Create( True );
  Normals:= TP3DVec3BufferGL.Create( True );
  Cotangents:= TP3DVec3BufferGL.Create( True );
  Tangents:= TP3DVec3BufferGL.Create( True );
  Indices:= TP3DIntBufferGL.Create( True );
  VBArray:= TP3DVertexBufferArray.Create;
  MaterialGroups:= TP3DMaterialGroupList.Create;

  //FVisible:= True;
end;

constructor TP3DMesh.CreateFromDOM(AParentList: TP3DObjectList;
  ADOMNode: TDOMElement; const AData: TP3DData);
begin
  Create( AParentList );
  LoadFromDOM( ADOMNode, AData );
end;

destructor TP3DMesh.Destroy;
var
  i: Integer;
begin
  Positions.Free;
  Normals.Free;
  Cotangents.Free;
  Tangents.Free;
  Indices.Free;
  for i:= 0 to High( TexCoords ) do
    TexCoords[ i ].Free;

  VBArray.Free;
  MaterialGroups.Free;

  inherited Destroy;
end;

procedure TP3DMesh.LoadFromDOM(ADOMNode: TDOMElement; const AData: TP3DData);
var
  Element: TDOMElement;
  Ext: String;
  Idx: Integer;
  F: TFileStream;
begin
  Name:= ADOMNode.GetAttribute( 'name' );

  if ( ADOMNode.hasAttribute( 'binary' )) then
    begin
      Ext:= ADOMNode.GetAttribute( 'binary' );
      try
        F:= TFileStream.Create( Ext, fmOpenRead );
        Idx:= StrToIntDef( ADOMNode.GetAttribute( 'vertices' ), 0 );
        LoadVec3FromStream( Positions, F, Idx );
        Idx:= StrToIntDef( ADOMNode.GetAttribute( 'normals' ), 0 );
        LoadVec3FromStream( Normals, F, Idx );
        Idx:= StrToIntDef( ADOMNode.GetAttribute( 'tangents' ), 0 );
        LoadVec3FromStream( Tangents, F, Idx );
        Idx:= StrToIntDef( ADOMNode.GetAttribute( 'cotangents' ), 0 );
        LoadVec3FromStream( Cotangents, F, Idx );
        Idx:= StrToIntDef( ADOMNode.GetAttribute( 'texcoords' ), 0 );
        SetLength( TexCoords, 1 );
        TexCoords[ 0 ]:= TP3DVec2BufferGL.Create( True );
        LoadVec2FromStream( TexCoords[ 0 ], F, Idx );
        Idx:= StrToInt( ADOMNode.GetAttribute( 'faces' ));
        LoadFaceFromStream( @Faces, F, Idx );
        F.Free;
        FileWatch.FileName:= ExpandFileName( Ext );
      except on E: Exception do
        begin
          WriteLn( E.Message );
          if ( Assigned( F )) then
            F.Free;
          exit;
        end;
      end;
    end;

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'matrix': ;//Matrix:= LoadMat4FromDOM( Element );
        'material': LoadMaterialFromDOM( Element, AData );
        'mesh': ;//Children.Add( TP3DMesh.CreateFromDOM( Children, Scene, Element ));
      else
        raise Exception.Create( 'Unknown tag inside Mesh Element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  //CalcCotangentsFromTangents;
  //Calc_Tangent_Binormal;
  UnpackBuffers;
end;

procedure TP3DMesh.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DActor);
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
  for i:= 0 to MaterialGroups.Count - 1 do
    RenderMaterialGroup( world, i, Scene );
end;

procedure TP3DMesh.RenderMaterialGroup(world: TMat4; Idx: Integer;
  Scene: TP3DScene);
var
  i: Integer;
  _world, view, proj: TMat4;

  Material: TP3DMaterial;
  offset: Integer;
  count: Integer;

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
  if (( Idx < 0 ) or ( Idx >= MaterialGroups.Count )) then
    exit;

  Material:= MaterialGroups[ Idx ].Material;
  if (( not Assigned( Material )) or ( not Assigned( Material.Shader ))) then
    exit;
  SetShader( Material.Shader );
  Scene.PassToShader( world );
  glVertexAttrib4f( P3DAttribColor,
    Material.Diff.R, Material.Diff.G, Material.Diff.B, 1.0 );
  //glUniform4f( ActShad.Uniforms.AddrByName( 'mat_diffuse' ), Material.Diff.r, Material.Diff.g, Material.Diff.b, 1 );
  glUniform4f( ActShad.Uniforms.AddrByName( 'mat_specular' ), Material.Spec.r, Material.Spec.g, Material.Spec.b, 1 );
  glUniform1f( ActShad.Uniforms.AddrByName( 'mat_hardness' ), Material.Spec_Hardness );

  if ( Material.NumMaps > 0 ) then
    for i:= 0 to Material.NumMaps - 1 do
      begin
        glActiveTexture( GL_TEXTURE0 + i );
        glEnable( GL_TEXTURE_2D );
        glBindTexture( GL_TEXTURE_2D, Material.Maps[ i ].Map.fGLTexture );
        glUniform1i( ActShad.Uniforms.AddrByName( 'tex' + IntToStr( i )), i );


        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      end;
  _world:= world;

  if ( Positions.Count > 0 ) then
    Positions.SetAttribArray( P3DAttribPosition );
  if ( Normals.Count > 0 ) then
    Normals.SetAttribArray( P3DAttribNormal );
  if ( Cotangents.Count > 0 ) then
    Cotangents.SetAttribArray( P3DAttribCotangent );
  if ( Tangents.Count > 0 ) then
    Tangents.SetAttribArray( P3DAttribTangent );
  for i:= 0 to High( TexCoords ) do
    TexCoords[ i ].SetAttribArray( P3DAttribTexCoord0 + i );

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );

  glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'world' ), 1, False, @_world );

  offset:= MaterialGroups[ Idx ].IndexStart;
  count:= ( MaterialGroups[ Idx ].IndexEnd - MaterialGroups[ Idx ].IndexStart ) + 1;
  glDrawElements( GL_TRIANGLES, count, GL_UNSIGNED_INT, Pointer( offset * SizeOf( GLint )));
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
  _Cotangents,
  _Normals: TP3DVec3BufferGL;
  _Textures: array of TP3DVec2BufferGL;

  numTex: Integer;
  k: Integer;
  baseTex: Integer;

  function FindMatGroup( Idx: Integer ): Integer;
  var
    l: Integer;
  begin
    Result:= -1;
    for l:= 0 to MaterialGroups.Count - 1 do
      with ( MaterialGroups[ l ]) do
        if ( InRange( Idx, PolyStart, PolyEnd )) then
          begin
            Result:= l;
            break;
          end;
  end;

  procedure SetTexLen( n: Integer );
  var
    c: Integer;
    l: Integer;
  begin
    c:= Length( _Textures );
    if ( n > c ) then
      begin
        SetLength( _Textures, n );
        for l:= c to n - 1 do
          _Textures[ l ]:= TP3DVec2BufferGL.Create( True );
      end;
  end;

  var
    ActiveMatGroup: Integer;

begin
  ClearVBO;
  _Positions:= TP3DVec3BufferGL.Create( True );
  _Tangents:= TP3DVec3BufferGL.Create( True );
  _Cotangents:= TP3DVec3BufferGL.Create( True );
  _Normals:= TP3DVec3BufferGL.Create( True );

  baseTex:= 0;

  ActiveMatGroup:= FindMatGroup( 0 );

  for i:= 0 to high( Faces ) do
    begin
      vbase:= _Positions.Count;
      _Positions.Count:= vbase + Length( Faces[ i ].verts );
      ibase:= Indices.Count;
      Indices.Count:= ibase + ( Length( Faces[ i ].verts ) - 2 ) * 3;
      if ( ActiveMatGroup <> -1 ) then
        with ( MaterialGroups[ ActiveMatGroup ]) do
          if ( not InRange( i, PolyStart, PolyEnd )) then
            begin
              IndexEnd:= ibase - 1;
              ActiveMatGroup:= FindMatGroup( i );
              if ( ActiveMatGroup > -1 ) then
                MaterialGroups[ ActiveMatGroup ].IndexStart:= ibase;
            end;

      for j:= 0 to high( Faces[ i ].verts ) do
        begin
          _Positions[ vbase + j ]:= Positions[ Faces[ i ].verts[ j ].v ];
          Faces[ i ].verts[ j ].v:= vbase + j;
          if ( Faces[ i ].verts[ j ].n >= 0 ) then
            begin
              _Normals.Count:= _Positions.Count;
              _Normals [ vbase + j ]:= Normals[ Faces[ i ].verts[ j ].n ];
              Faces[ i ].verts[ j ].n:= vbase + j;
            end;

          if ( Faces[ i ].verts[ j ].t >= 0 ) then
            begin
              _Tangents.Count:= _Positions.Count;
              _Tangents[ vbase + j ]:= Tangents[ Faces[ i ].verts[ j ].t ];
              Faces[ i ].verts[ j ].t:= vbase + j;
            end;

          if ( Faces[ i ].verts[ j ].c >= 0 ) then
            begin
              _Cotangents.Count:= _Positions.Count;
              _Cotangents[ vbase + j ]:= Cotangents[ Faces[ i ].verts[ j ].c ];
              Faces[ i ].verts[ j ].c:= vbase + j;
            end;


          numTex:= Length( Faces[ i ].verts[ j ].texc );

          if ( numTex > 0 ) then
            begin
              SetTexLen( numTex ); //TODO: This should be fine in the most cases but could cause in some cases
//              baseTex:= _Textures.Count;
//              _Textures.Count:= _Textures.Count + numTex;
              for k:= 0 to numTex - 1 do
                Faces[ i ].verts[ j ].texc[ k ]:= _Textures[ k ].Add( TexCoords[ 0 ][ Faces[ i ].verts[ j ].texc[ k ]]);
            end;

          if ( j >= 2 ) then
            begin
              Indices[ ibase + ( j - 2 ) * 3 + 0 ]:= vbase + 0;
              Indices[ ibase + ( j - 2 ) * 3 + 1 ]:= vbase + j - 1;
              Indices[ ibase + ( j - 2 ) * 3 + 2 ]:= vbase + j;
            end;
        end;
    end;
  if ( ActiveMatGroup <> -1 ) then
    MaterialGroups[ ActiveMatGroup ].IndexEnd:= Indices.Count;
  Positions.Free;
  Positions:= _Positions;
  Normals.Free;
  Normals:= _Normals;
  Tangents.Free;
  Tangents:= _Tangents;
  Cotangents.Free;
  Cotangents:= _Cotangents;
  for i:= 0 to High( TexCoords ) do
    TexCoords[ i ].Free;
  TexCoords:= _Textures;

  CreateVBO;
end;

procedure TP3DMesh.ClearVBO;
begin
  VBArray.Free;
  VBArray:= TP3DVertexBufferArray.Create;
end;

procedure TP3DMesh.CreateVBO;
var
  i: Integer;
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
  if ( Cotangents.Count > 0 ) then
    begin
      Cotangents.PushData;
      Cotangents.SetAttribArray( P3DAttribCotangent );
    end;
  if ( Tangents.Count > 0 ) then
    begin
      Tangents.PushData;
      Tangents.SetAttribArray( P3DAttribTangent );
    end;
  for i:= 0 to high( TexCoords ) do
    if ( TexCoords[ i ].Count > 0 ) then
      begin
        TexCoords[ i ].PushData;
        TexCoords[ i ].SetAttribArray( P3DAttribTexCoord0 + i );
      end;

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  Indices.PushData;
  VBArray.Unbind;
end;

procedure TP3DMesh.CalcCotangentsFromTangents;
var
  i: Integer;
  j: Integer;
begin
  Cotangents.Count:= Tangents.Count;
  for i:= 0 to high( Faces ) - 1 do
    for j:= 0 to high( Faces[ i ].verts) do
      begin
        Cotangents[ Faces[ i ].verts[ j ].t ]:= cross( Tangents[ Faces[ i ].verts[ j ].t ], Normals[ Faces[ i ].verts[ j ].n ]);
        Faces[ i ].verts[ j ].c:= Faces[ i ].verts[ j ].t;
        WriteLn( Faces[ i ].verts[ j ].t );
      end;
end;


procedure TP3DMesh.Calc_Tangents_Cotangents;
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
        Faces[ f ].verts[ v0 ].c:= -1;
        Faces[ f ].verts[ v1 ].t:= -1;
        Faces[ f ].verts[ v1 ].c:= -1;
        Faces[ f ].verts[ v2 ].t:= -1;
        Faces[ f ].verts[ v2 ].c:= -1;
      end
    else
      begin
        // Edges of the triangle : postion delta
        if ( Length( Faces[ f ].verts ) < 3 ) then
          raise Exception.Create( 'Cannot calculate Tangents because of illegal faces!' );

        deltaPos1:= Positions[ Faces[ f ].verts[ v1 ].v ] - Positions[ Faces[ f ].verts[ v0 ].v ];
        deltaPos2:= Positions[ Faces[ f ].verts[ v2 ].v ] - Positions[ Faces[ f ].verts[ v0 ].v ];

        // UV delta
        deltaUV1:= TexCoords[ 0 ][ Faces[ f ].verts[ v1 ].texc[ 0 ]] - TexCoords[ 0 ][ Faces[ f ].verts[ v0 ].texc[ 0 ]];
        deltaUV2:= TexCoords[ 0 ][ Faces[ f ].verts[ v2 ].texc[ 0 ]] - TexCoords[ 0 ][ Faces[ f ].verts[ v0 ].texc[ 0 ]];

        r:= 1.0 / Max( 0.00001, ( deltaUV1.x * deltaUV2.y - deltaUV1.y * deltaUV2.x ));

        CalcTriangle_TB( tang, bin );
        Tangents.Add( tang );
        Cotangents.Add( bin );

        Faces[ f ].verts[ v0 ].t:= Tangents.Count - 1;
        Faces[ f ].verts[ v0 ].c:= Cotangents.Count - 1;
        Faces[ f ].verts[ v1 ].t:= Tangents.Count - 1;
        Faces[ f ].verts[ v1 ].c:= Cotangents.Count - 1;
        Faces[ f ].verts[ v2 ].t:= Tangents.Count - 1;
        Faces[ f ].verts[ v2 ].c:= Cotangents.Count - 1;
      end;
  end;

var
  i: Integer;
  j: Integer;
  flip: Boolean;
begin
  Tangents.Clear;
  Cotangents.Clear;

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

function TP3DMesh.CalcBoundingBox: TP3DBoundingBox;
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
  Result:= FBoundingBox;
end;

{ TMaterial }

procedure TP3DMaterial.BuildShader;
var
  ShaderTree: TP3DShaderNodeTree;
  Node: TP3DShaderNode;
  Compiled: TP3DShaderCompiled;
  i: Integer;

  function AddNode( Name: String ): TP3DShaderNode;
  begin
    Node:= P3DShaderLib.FindNode( Name );
    if ( not Assigned( Node )) then
      raise Exception.Create( 'Cannot build shader: Node "' + Name + '" not found!' );
    Result:= Node.MakeCopy;
    ShaderTree.Nodes.Add( Result );
  end;

  function ChangeSocketValueInt( Node: TP3DShaderNode; Name: String; Value: Integer ): TP3DShaderNode;
  var
    Socket: Integer;
  begin
    Socket:= Node.Inputs.FindSocketByName( Name );
    if ( Socket < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    TP3DShaderNodeSocketInt( Node.Inputs[ Socket ]).Value:= Value;
    Result:= Node;
  end;

  function ChangeSocketValueFloat( Node: TP3DShaderNode; Name: String; Value: Single ): TP3DShaderNode;
  var
    Socket: Integer;
  begin
    Socket:= Node.Inputs.FindSocketByName( Name );
    if ( Socket < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    TP3DShaderNodeSocketFloat( Node.Inputs[ Socket ]).Value:= Value;
    Result:= Node;
  end;

  function ConnectSocket( Node1: TP3DShaderNode; Node2: TP3DShaderNode; Socket1: String; Socket2: String ): TP3DShaderNode;
  var
    Socket1n, Socket2n: Integer;
  begin
    Socket1n:= Node1.Inputs.FindSocketByName( Socket1 );
    Socket2n:= Node2.Outputs.FindSocketByName( Socket2 );
    if ( Socket1n < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Socket1 + '" not found!' );
    if ( Socket2n < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Socket2 + '" not found!' );
    Node1.Inputs[ Socket1n ].Connected:= Node2.Outputs[ Socket2n ];
    Result:= Node1;
  end;

const Mode: array [ p3dmmMix .. p3dmmSubtract ] of String = ( 'mix', 'multiply', 'add', 'subtract' );

begin
  ShaderTree:= TP3DShaderNodeTree.Create;

  try

  AddNode( 'lib_common' );

  AddNode( 'lib_maps' );

  AddNode( 'Pass_Init' );

  AddNode( 'lib_lighting' );


  for i:= 0 to 7 do
    ChangeSocketValueInt( ChangeSocketValueInt( AddNode( 'TC_Init' ), 'Location', i + 8 ), 'TexCoordIndex', i );
  for i:= 0 to NumMaps - 1 do
    begin
      ChangeSocketValueInt( ChangeSocketValueInt( AddNode( 'Pass_Read' ), 'Index', i ), 'TexCoordIndex', Maps[ i ].TexChannel );
      if ( Maps[ i ].DiffuseFactor > 0 ) then
        ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( AddNode( 'Pass_Diffuse' ), 'Index', i ), 'Factor', Maps[ i ].DiffuseFactor ),
        AddNode( 'Pass_Combine' ), 'Mode', Mode[ Maps[ i ].Mode ]);
      if ( Maps[ i ].NormalFactor > 0 ) then
        ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( AddNode( 'Pass_Normal' ), 'Index', i ), 'Factor', Maps[ i ].NormalFactor ),
        AddNode( 'Pass_Combine' ), 'Mode', Mode[ Maps[ i ].Mode ]);
      if ( Maps[ i ].SpecularFactor > 0 ) then
         ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( AddNode( 'Pass_Specular' ), 'Index', i ), 'Factor', Maps[ i ].SpecularFactor ),
         AddNode( 'Pass_Combine' ), 'Mode', Mode[ Maps[ i ].Mode ]);
    end;

  AddNode( 'Pass_Mix' );
  AddNode( 'Lighting' );
  AddNode( 'Pass_Final' );


  Compiled:= ShaderTree.Compile;
  //WriteLn( 'Material: ' + Name );
  //for i:= 0 to NumMaps - 1 do
  //  WriteLn( '  Map: ' + Maps[ i ].Map.FileName + ' Diffuse: ', Maps[ i ].DiffuseFactor, ' Normal: ', Maps[ i ].NormalFactor );
  //WriteLn( 'vshader: ' + Compiled.FindBuffer( 'vshader' ).Code );
  //WriteLn( 'fshader: ' + Compiled.FindBuffer( 'fshader' ).Code );
  if ( Assigned( Shader )) then
    Shader.Free;
  Shader:= p3dshaders.CreateVertexAndFragmentShader( Compiled.FindBuffer( 'vshader' ).Code, Compiled.FindBuffer( 'fshader' ).Code );
  finally
    ShaderTree.Free;
    Compiled.Free;
  end;
end;

constructor TP3DMaterial.Create(AParentList: TP3DObjectList);
begin
  inherited;
  NumMaps:= 0;
//  Diff_Map:= -1;
end;

destructor TP3DMaterial.Destroy;
begin
  if ( Assigned( Shader )) then
    Shader.Free;
  inherited Destroy;
end;

procedure TP3DMaterial.LoadFromDOM(ADOMNode: TDOMElement; const AData: TP3DData
  );
var
  SpecTmp: TVec4;
  tex: TDOMElement;

  procedure LoadTex;
  var
    TexName: String;
    tx: TP3DTexture;
  begin
    if ( NumMaps = 8 ) then
      raise Exception.Create( 'Maximum Number of Maps reached: 8' );
    Maps[ NumMaps ].DiffuseFactor:= StrToFloatDef( tex.GetAttribute( 'diffuse' ), 0.0 );
    Maps[ NumMaps ].NormalFactor:= StrToFloatDef( tex.GetAttribute( 'normal' ), 0.0 );
    Maps[ NumMaps ].SpecularFactor:= StrToFloatDef( tex.GetAttribute( 'specular' ), 0.0 );
    Maps[ NumMaps ].TexChannel:= StrToIntDef( tex.GetAttribute( 'layer' ), 0 );
    case tex.GetAttribute( 'mode' ) of
      'add': Maps[ NumMaps ].Mode:= p3dmmAdd;
      'multiply': Maps[ NumMaps ].Mode:= p3dmmMultiply;
      'subtract': Maps[ NumMaps ].Mode:= p3dmmSubtract;
      else
        Maps[ NumMaps ].Mode:= p3dmmMix;
    end;
    TexName:= tex.GetAttribute( 'file' );
    if ( not FileExists( TexName )) then
      raise Exception.Create( 'Error: The specified texture "' + TexName + '" could not be found!' )
    else
      begin
        tx:= TP3DTexture.Create( TexName );
        if ( Assigned( tx )) then
          Maps[ NumMaps ].Map:= tx
        else
          begin
            WriteLn( 'Error: An unknown error occured while loading texture "' + TexName + '"!' );
            exit;
          end;
      end;
    Inc( NumMaps );
  end;

begin
  Name:= ADOMNode.GetAttribute( 'name' );
  Diff.FromString( ADOMNode.GetAttribute( 'diffuse' ));
  SpecTmp.FromString( ADOMNode.GetAttribute( 'specular' ));
  Spec:= SpecTmp.YZW;
  Spec_Hardness:= SpecTmp.X;
  tex:= TDOMElement( ADOMNode.FirstChild );
  while Assigned( tex ) do
    begin
      case tex.NodeName of
        'texture': LoadTex;
        '#comment':;
      else
        raise Exception.Create( 'Unknown tag inside Material Element: '+ tex.NodeName );
      end;
    tex:= TDOMElement( tex.NextSibling );
  end;
  BuildShader;
  FileWatch.FileName:= '';
end;


{$ENDIF}
