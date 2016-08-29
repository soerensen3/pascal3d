//p3dmesh.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
  type
    TP3DMesh = class;
    TP3DRenderFlag = ( rfShadowMap, rfWireFrame, rfDebugShowLocation, rfDebugShowBoundingBox, rfDebugShowArmature );
    TP3DRenderFlags = set of TP3DRenderFlag;

    { TP3DMeshMaterialGroup }

    TP3DMeshMaterialGroup = class ( TPersistent )
     private
       FIndexEnd: Integer;
       FIndexStart: Integer;
       FMaterial: TP3DMaterialShader;
       FParentMesh: TP3DMesh;
       FPolyEnd: Integer;
       FPolyStart: Integer;

       procedure SetMaterial( AValue: TP3DMaterialShader );

     published
       property PolyStart: Integer read FPolyStart write FPolyStart;
       property PolyEnd: Integer read FPolyEnd write FPolyEnd;
       property IndexStart: Integer read FIndexStart write FIndexStart;
       property IndexEnd: Integer read FIndexEnd write FIndexEnd;
       property Material: TP3DMaterialShader read FMaterial write SetMaterial;
       property ParentMesh: TP3DMesh read FParentMesh write FParentMesh;
    end;

    TP3DMeshMaterialGroupList = specialize gP3DCustomObjectList < TP3DMeshMaterialGroup >;

    TP3DFace = record
     loop_start: Integer;
     loop_count: Integer;
     mat: TP3DMaterialShader;
    end;


    PP3DFaceArray = ^TP3DFaceArray;
    TP3DFaceArray = array of TP3DFace;

    { TP3DMeshModifier }

    TP3DMeshModifier = class( TInterfacedPersistent )
      private
        FMesh: TP3DMesh;

      published
        constructor Create( AMesh: TP3DMesh ); virtual;
        constructor CreateFromDOM( ADOMNode: TDOMElement; AMesh: TP3DMesh );

        procedure LoadFromDOM( ADOMNode: TDOMElement ); virtual; abstract;

        procedure PassToShader( Scene: TP3DScene; Material: TP3DMaterialShader; var world: TMat4 ); virtual; abstract;
        property Mesh: TP3DMesh read FMesh write FMesh;
    end;

    TP3DMeshModifierList = specialize gP3DCustomObjectList < TP3DMeshModifier >;

    { TP3DMesh }

    TP3DMesh = class( TP3DDataBlock )
      private
        FFileWatch: TP3DFileWatch;
        FModifiers: TP3DMeshModifierList;

        procedure LoadMaterialFromDOM(DOM: TDOMElement);
        procedure LoadModifierFromDOM( DOM: TDOMElement );

      public
        Positions: TP3DVec3BufferGL;
        Normals: TP3DVec3BufferGL;
        Tangents: TP3DVec3BufferGL;
        Cotangents: TP3DVec3BufferGL;
        Faces: TP3DFaceArray;
        TexCoords: array of TP3DVec2BufferGL;
        Edges: TP3DIntBufferGL;

        Indices: TP3DIntBufferGL;
        VBArray: TP3DVertexBufferArray;
        MaterialGroups: TP3DMeshMaterialGroupList;

        constructor Create( const AData: TP3DData = nil );
        constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil );

        destructor Destroy; override;

        procedure LoadFromDOM( ADOMNode: TDOMElement ); override;
        procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor ); override;
        procedure RenderSimple;
        procedure RenderMaterialGroup( world: TMat4; Idx: Integer; Scene: TP3DScene );
        procedure RenderEdges( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor );
        function CastRay( Orig, Dir: TVec3; Actor: TP3DActor; out HitPos: TVec3 ): Boolean; override;
        procedure UnpackBuffers;
        procedure ClearVBO;
        procedure CreateVBO;
        procedure CalcCotangentsFromTangents;
        procedure Calc_Tangents_Cotangents;
        function CalcBoundingBox(): TP3DBoundingBox; override;
        procedure NotifyDeleteOfData(Sender: TP3DDataBlock); override;
        //        procedure ClearChildren;
        property Modifiers: TP3DMeshModifierList read FModifiers write FModifiers;
        property FileWatch: TP3DFileWatch read FFileWatch write FFileWatch;
      end;

      TP3DMeshList = specialize gP3DCustomDatablockList < TP3DMesh >;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DMesh }
procedure LoadFaceFromStream( Faces: PP3DFaceArray; S: TStream; ACount, LoopCount: Integer );
var
  ints: array of Integer;
  base, i: Integer;
begin
  if ( ACount = 0 ) then
    exit;
  base:= Length( Faces^ );
  SetLength( Faces^, base + ACount );
  SetLength( ints, base + ACount );
  S.Read( ints[ 0 ], SizeOf( Integer ) * ACount );

  Faces^[ 0 ].loop_start:= 0;

  if ( ACount > 1 ) then
    for i:= 1 to ACount - 1 do
      begin
        Faces^[ i ].loop_start:= ints[ i ];
        Faces^[ i - 1 ].loop_count:= ints[ i ]- Faces^[ i - 1 ].loop_start;
      end;
  Faces^[ ACount - 1 ].loop_count:= LoopCount - Faces^[ ACount - 1 ].loop_start;
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

procedure LoadIntFromStream( IntArray: TIntList; S: TStream; ACount: Integer );
var
  i: Integer;
  int: Integer;
begin
  for i:= 0 to ACount - 1 do
    begin
      S.Read( int, SizeOf( Integer ));
      IntArray.Add( int );
    end;
end;

procedure TP3DMesh.LoadMaterialFromDOM(DOM: TDOMElement);
var
  MatIdx: Integer;
  AName: String;
  MatPolyStart: Integer;
  MatPolyEnd: Integer;
  MaterialGroup: TP3DMeshMaterialGroup;
begin
  AName:= DOM.GetAttribute( 'name' );
  MatIdx:= Parent.AppendFromDom( TDOMElement( DOM.OwnerDocument.FirstChild ), AName, TP3DMaterialShader );
  MatPolyStart:= StrToInt( DOM.GetAttribute( 'start' ));
  MatPolyEnd:= StrToInt( DOM.GetAttribute( 'end' ));

  MaterialGroup:= TP3DMeshMaterialGroup.Create;
  MaterialGroup.Material:= TP3DMaterialShader( Parent.DataBlocks[ MatIdx ]);
  MaterialGroup.PolyStart:= MatPolyStart;
  MaterialGroup.PolyEnd:= MatPolyEnd;
  MaterialGroups.Add( MaterialGroup );
end;

procedure TP3DMesh.LoadModifierFromDOM(DOM: TDOMElement);
var
  M: TP3DMeshModifier;
begin
  M:= nil;
  case DOM.GetAttribute( 'name' ) of
    'terrain': M:= TP3DMeshModifierTerrain.CreateFromDOM( DOM, Self );
    else
      raise Exception.Create( 'Cannot create mesh modifier. Modifier type not found!' ); //TODO: Improve msg
  end;
  if ( Assigned( M )) then
    Modifiers.Add( M );
end;

constructor TP3DMesh.Create(const AData: TP3DData);
begin
  inherited;

  Positions:= TP3DVec3BufferGL.Create( True );
  Normals:= TP3DVec3BufferGL.Create( True );
  Cotangents:= TP3DVec3BufferGL.Create( True );
  Tangents:= TP3DVec3BufferGL.Create( True );
  Indices:= TP3DIntBufferGL.Create( True );
  Edges:= TP3DIntBufferGL.Create( True );
  VBArray:= TP3DVertexBufferArray.Create;
  MaterialGroups:= TP3DMeshMaterialGroupList.Create;
  Modifiers:= TP3DMeshModifierList.Create;

  Parent.Meshes.Add( Self );
  FileWatch:= TP3DFileWatch.Create( '' );
end;

constructor TP3DMesh.CreateFromDOM(ADOMNode: TDOMElement; const AData: TP3DData);
begin
  Create( AData );
  LoadFromDOM( ADOMNode );
end;

destructor TP3DMesh.Destroy;
var
  i: Integer;
begin
  Modifiers.Clear( True );
  Modifiers.Free;
  Positions.Free;
  Normals.Free;
  Cotangents.Free;
  Tangents.Free;
  Indices.Free;
  Edges.Free;
  for i:= 0 to High( TexCoords ) do
    TexCoords[ i ].Free;

  VBArray.Free;
  MaterialGroups.Free;
  FileWatch.Free;

  inherited Destroy;
end;

procedure TP3DMesh.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  ext: String;
  cnt, numTex, i: Integer;
  F: TFileStream;
begin
  Name:= ADOMNode.GetAttribute( 'name' );

  if ( ADOMNode.hasAttribute( 'binary' )) then
    begin
      ext:= ADOMNode.GetAttribute( 'binary' );
      ext:= ExpandFileName( P3DSearchPaths.Models.FindFileEx( ext, GetCurrentDir ));
      try
        if ( not FileExists( ext )) then
          raise Exception.Create( Format( 'Error while loading mesh "%s"! The specified file does not exist!', [ Ext ]));
        F:= TFileStream.Create( Ext, fmOpenRead );
        cnt:= StrToIntDef( ADOMNode.GetAttribute( 'vertices' ), 0 );
        if ( cnt > 0 ) then
          LoadVec3FromStream( Positions, F, cnt );

        cnt:= StrToIntDef( ADOMNode.GetAttribute( 'normals' ), 0 );
        if ( cnt > 0 ) then
          LoadVec3FromStream( Normals, F, cnt );

        cnt:= StrToIntDef( ADOMNode.GetAttribute( 'loops' ), 0 );
        LoadIntFromStream( Edges, F, cnt );

        cnt:= StrToIntDef( ADOMNode.GetAttribute( 'texcoords' ), 0 );
        numtex:= StrToIntDef( ADOMNode.GetAttribute( 'texlayers' ), 0 );
        if ( cnt > 0 ) then
          begin
            SetLength( TexCoords, numTex );
            for i:= 0 to numTex - 1 do
              begin
                TexCoords[ i ]:= TP3DVec2BufferGL.Create( True );
                LoadVec2FromStream( TexCoords[ i ], F, cnt );
              end;

            cnt:= StrToIntDef( ADOMNode.GetAttribute( 'tangents' ), 0 );
            if ( cnt > 0 ) then
              LoadVec3FromStream( Tangents, F, cnt );

            if ( cnt > 0 ) then
              cnt:= StrToIntDef( ADOMNode.GetAttribute( 'cotangents' ), 0 );
            LoadVec3FromStream( Cotangents, F, cnt );
          end;

        cnt:= StrToIntDef( ADOMNode.GetAttribute( 'faces' ), 0 );
        if ( cnt > 0 ) then
          LoadFaceFromStream( @Faces, F, cnt, Edges.Count );

        F.Free;
        FileWatch.FileName:= ext;
      except on E: Exception do
        begin
          P3DLog.LogException( Self, E );
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
        'material': LoadMaterialFromDOM( Element );
        'modifier': LoadModifierFromDOM( Element );
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
begin
  for i:= 0 to MaterialGroups.Count - 1 do
    RenderMaterialGroup( world, i, Scene );
end;

procedure TP3DMesh.RenderSimple;
var
  i: Integer;
begin
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

  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 )); cle( Self );
end;

procedure TP3DMesh.RenderMaterialGroup(world: TMat4; Idx: Integer;
  Scene: TP3DScene);
var
  Material: TP3DMaterialShader;
  Modifier: TP3DMeshModifier;
  offset: Integer;
  count, i: Integer;

begin
  if (( Idx < 0 ) or ( Idx >= MaterialGroups.Count )) then
    exit;

  Material:= MaterialGroups[ Idx ].Material;
  if (( not Assigned( Material )) or ( not Assigned( Material.Shader ))) then
    exit;
  Material.PassToShader();
  for Modifier in Modifiers do
    Modifier.PassToShader( Scene, Material, world );
  Scene.PassToShader( world );

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

  offset:= MaterialGroups[ Idx ].IndexStart;
  count:= ( MaterialGroups[ Idx ].IndexEnd - MaterialGroups[ Idx ].IndexStart ) + 1;
  //P3DLog.LogInfo( Self, Material.Shader.DumpUniforms );
  glDrawElements( GL_TRIANGLES, count, GL_UNSIGNED_INT, Pointer( offset * SizeOf( GLint ))); cle( Self );
end;

procedure TP3DMesh.RenderEdges(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DActor);
var
  i: Integer;
begin
  Scene.PassToShader( world );
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

  Edges.Bind( GL_ELEMENT_ARRAY_BUFFER );

  glDrawElements( GL_LINES, Edges.Count, GL_UNSIGNED_INT, Pointer( 0 )); cle( Self );
end;

function intersectRayTriangle( orig, dir, v0, v1, v2: TVec3; out baryPosition: TVec3 ): Boolean;
var
  e1, e2: TVec3;
  p: TVec3;
  a: Float;
  f: Extended;
  s: TVec3;
  q: TVec3;
begin
  Result:= False;

  e1:= v1 - v0;
  e2:= v2 - v0;

  p:= cross( dir, e2 );

  a:= dot( e1, p );

  if ( a < epsilon ) then
    exit;
  f:= 1 / a;

  s:= orig - v0;
  baryPosition.x:= f * dot( s, p );
  if( baryPosition.x < 0 ) then
    exit;
  if( baryPosition.x > 1 ) then
    exit;

  q:= cross( s, e1 );
  baryPosition.y:= f * dot( dir, q );

  if( baryPosition.y < 0.0 ) then
    exit;

  if( baryPosition.y + baryPosition.x > 1 ) then
    exit;

  baryPosition.z:= f * dot( e2, q );

  Result:= baryPosition.z >= 0.0;
end;


function TP3DMesh.CastRay(Orig, Dir: TVec3; Actor: TP3DActor; out HitPos: TVec3 ): Boolean;
var
  i: Integer;
  mInv: TMat4;
  transOrig, transDir: TVec3;

  function TestPoly( n: Integer ): Boolean;
  var
    v0: TVec3;
    v1: TVec3;
    v2: TVec3;
  begin
    Result:= False;
    v0:= Positions[ Indices[ n * 3 ]];
    v1:= Positions[ Indices[ n * 3 + 1 ]];
    v2:= Positions[ Indices[ n * 3 + 2 ]];
    Result:= intersectRayTriangle( transOrig, transDir, v0, v1, v2, HitPos );
  end;

begin
  Result:= inherited CastRay( Orig, Dir, Actor, HitPos );
  mat4inverse( Actor.Matrix, mInv );
  transOrig:= ( mInv * vec4( Orig, 1 )).xyz;
  transDir:= ( mInv * vec4( Dir, 0 )).xyz.Normalize;
  for i:= 0 to Indices.Count div 3 do
    if ( TestPoly( i )) then
      begin
        Result:= True;
        HitPos:= ( Actor.Matrix * vec4( HitPos, 1 )).xyz;
        break;
      end;
end;

procedure TP3DMesh.UnpackBuffers;
var
  i: Integer;
  j: Integer;
  vbase: Integer;
  Idx: Integer;

  _Positions,
  _Normals: TP3DVec3BufferGL;
  _Edges: TP3DIntBufferGL;

  vidx: Integer;

begin
  ClearVBO;
  _Positions:= TP3DVec3BufferGL.Create( True );
  _Normals:= TP3DVec3BufferGL.Create( True );
  _Normals:= TP3DVec3BufferGL.Create( True );
  _Edges:= TP3DIntBufferGL.Create( True );


  for i:= 0 to high( Faces ) do
    begin
      Idx:= Indices.Count;
      vbase:= _Positions.Count;
      for j:= 0 to Faces[ i ].loop_count - 1 do
        begin
          vidx:= Edges[ Faces[ i ].loop_start + j ];
          _Positions.Add( Positions[ vidx ]);
          _Normals.Add( Normals[ vidx ]);
        end;
      for j:= 1 to Faces[ i ].loop_count - 1 do
        begin
          _Edges.Add([ vbase + j -1, vbase + j ]);
          if ( j > 1 ) then
            Indices.Add([ vbase,
                          vbase + j - 1,
                          vbase + j ]);
        end;
      _Edges.Add([ vbase + Faces[ i ].loop_count - 1, vbase ]);
      Faces[ i ].loop_start:= Idx;
      Faces[ i ].loop_count:= Indices.Count - Idx;
      //WriteLn( 'FACE: ', i, ' (idx) ', Faces[ i ].loop_start, ' (loopstart) ',
      //         Faces[ i ].loop_count, ' (loopcount) ');
    end;
  for i:= 0 to MaterialGroups.Count - 1 do
    begin
      MaterialGroups[ i ].FIndexStart:= Faces[ MaterialGroups[ i ].PolyStart ].loop_start;
      MaterialGroups[ i ].FIndexEnd:= Faces[ MaterialGroups[ i ].PolyEnd ].loop_start
                                    + Faces[ MaterialGroups[ i ].PolyEnd ].loop_count - 1;
//      WriteLn( 'GRP: ', i, ' (idx) ', MaterialGroups[ i ].FIndexStart, ' (loopstart) ',
//               MaterialGroups[ i ].FIndexEnd, ' (loopend) ');
    end;
//  Write( 'Edges: [' );
//  for i:= 0 to _Edges.Count - 1 do
//    Write( _Edges[ i ], ', ' );
//  Write( ']' );
//  WriteLn( 'Edges before: ', Edges.Count, ' Edges now: ', _Edges.Count );
  Positions.Free;
  Positions:= _Positions;
  Normals.Free;
  Normals:= _Normals;
  Edges.Free;
  Edges:= _Edges;

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

  Edges.PushData;

  VBArray.Unbind;
end;

procedure TP3DMesh.CalcCotangentsFromTangents;
var
  i: Integer;
  j: Integer;
begin
  Cotangents.Count:= Tangents.Count;
  for i:= 0 to Tangents.Count do
    Cotangents[ i ]:= cross( Tangents[ i ], Normals[ i ]);
  {for i:= 0 to high( Faces ) - 1 do
    for j:= 0 to high( Faces[ i ].verts) do
      begin
        Cotangents[ Faces[ i ].verts[ j ].t ]:= cross( Tangents[ Faces[ i ].verts[ j ].t ], Normals[ Faces[ i ].verts[ j ].n ]);
        Faces[ i ].verts[ j ].c:= Faces[ i ].verts[ j ].t;
      end;}
end;


procedure TP3DMesh.Calc_Tangents_Cotangents;
begin
{
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
    end;}
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

procedure TP3DMesh.NotifyDeleteOfData(Sender: TP3DDataBlock);
var
  i: Integer;
begin
  inherited NotifyDeleteOfData(Sender);
  for i:= 0 to MaterialGroups.Count - 1 do
    if ( MaterialGroups[ i ].Material = Sender ) then
      MaterialGroups[ i ].Material:= nil;
end;

{ TP3DMeshMaterialGroup }

procedure TP3DMeshMaterialGroup.SetMaterial( AValue: TP3DMaterialShader );
begin
  if FMaterial=AValue then Exit;

  if ( Assigned( FMaterial )) then
    FMaterial.Users.Delete( FMaterial.Users.IndexOf( ParentMesh ));

  FMaterial:= AValue;

  if ( Assigned( FMaterial )) then
    FMaterial.Users.Add( ParentMesh );
end;

{ TP3DMeshModifier }

constructor TP3DMeshModifier.Create(AMesh: TP3DMesh);
begin
  inherited Create;
  Mesh:= AMesh;
end;

constructor TP3DMeshModifier.CreateFromDOM(ADOMNode: TDOMElement; AMesh: TP3DMesh);
begin
  Create( AMesh );
  LoadFromDOM( ADOMNode );
end;



{$ENDIF}
