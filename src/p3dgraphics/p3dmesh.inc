//p3dmesh.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
  type
    TP3DMesh = class;
    TP3DRenderFlag = ( rfShadowMap, rfWireFrame, rfDebugShowLocation, rfDebugShowBoundingBox, rfDebugShowArmature );
    TP3DRenderFlags = set of TP3DRenderFlag;

    { TP3DMeshMaterialGroup }

    TP3DMeshMaterialGroup = class ( TP3DInterfacedPersistent )
     private
       FIndexEnd: Integer;
       FIndexStart: Integer;
       FMaterial: TP3DMaterial;
       FParentMesh: TP3DMesh;
       FPolyEnd: Integer;
       FPolyStart: Integer;

       procedure SetMaterial( AValue: TP3DMaterial );

     public
       constructor Create( AParentMesh: TP3DMesh );
       constructor CreateFromDOM( ADOMNode: TDOMElement; AParentMesh: TP3DMesh );
       class function DOMNodeName: String; override;

     published
       property PolyStart: Integer read FPolyStart write FPolyStart;
       property PolyEnd: Integer read FPolyEnd write FPolyEnd;
       property IndexStart: Integer read FIndexStart write FIndexStart;
       property IndexEnd: Integer read FIndexEnd write FIndexEnd;
       property Material: TP3DMaterial read FMaterial write SetMaterial;
       property ParentMesh: TP3DMesh read FParentMesh write FParentMesh;
    end;

    { TP3DMeshMaterialGroupList }

    TP3DMeshMaterialGroupList = class( specialize gP3DCustomObjectList < TP3DMeshMaterialGroup >)
      private
        FParentMesh: TP3DMesh;

      public
        constructor Create( AParentMesh: TP3DMesh );

        function Find( Material: TP3DMaterial ): Integer;
        procedure SaveToDOM( AParent: TDOMElement );
        procedure LoadFromDOM( ADOMNode: TDOMElement );

        property ParentMesh: TP3DMesh read FParentMesh;
    end;

    // List of Mesh faces, where ...
    // X = start loop index, Y = loop count
    TP3DFaceList = TIVec2List;

    { TP3DMeshModifier }

    TP3DMeshModifier = class( TP3DInterfacedPersistent )
      private
        FMesh: TP3DMesh;

      public
        constructor Create( AMesh: TP3DMesh ); virtual;
        constructor CreateFromDOM( ADOMNode: TDOMElement; AMesh: TP3DMesh );

        procedure LoadFromDOM( ADOMNode: TDOMElement ); virtual; abstract;
        class function DOMNodeName: String; override;
        function GetType: String;

        procedure PassToShader( Material: TP3DMaterial; var world: TMat4 ); virtual; abstract;

      published
        property Mesh: TP3DMesh read FMesh write FMesh;
    end;

    { TP3DMeshModifierList }

    TP3DMeshModifierList = class( specialize gP3DCustomObjectList < TP3DMeshModifier >)
      private
        FParentMesh: TP3DMesh;

      public
        constructor Create( AParentMesh: TP3DMesh );

        procedure SaveToDOM( AParent: TDOMElement );
        procedure LoadFromDOM( ADOMNode: TDOMElement );

        property ParentMesh: TP3DMesh read FParentMesh;
    end;

    TP3DMeshModifierClass = class of TP3DMeshModifier;

    TP3DMeshModifierClassFactory = specialize gP3DClassFactory < TP3DMeshModifierClass >;

    { TP3DMeshWeightGroup }

    TP3DMeshWeightGroup = class( TP3DInterfacedPersistent )
      private
        FMesh: TP3DMesh;
        FName: String;

        function GetWeight( VIndex: Integer ): Float;

      public
        //property Weight[ VIndex: Integer ]: Float read GetWeight;
        constructor Create( AMesh: TP3DMesh );
        constructor CreateFromDOM( ADOMNode: TDOMElement );
        class function DOMNodeName: String; override;

      published
        property Name: String read FName write FName;
        property Mesh: TP3DMesh read FMesh write FMesh;
    end;

    { TP3DMeshWeightGroupList }

    TP3DMeshWeightGroupList = class( specialize gP3DCustomObjectList < TP3DMeshWeightGroup >)
    private
      FParentMesh: TP3DMesh;
    public
      constructor Create( AParentMesh: TP3DMesh );
      procedure SaveToDOM( AParent: TDOMElement );
      procedure LoadFromDOM( ADOMNode: TDOMElement );

      property ParentMesh: TP3DMesh read FParentMesh write FParentMesh;
    end;

    TP3DArmature = class;

    { TP3DMesh }

    TP3DMesh = class( TP3DDataBlock )
      private
        //FArmature: TP3DArmature;
        FFileWatch: TP3DFileWatch;
        FModifiers: TP3DMeshModifierList;

        procedure LoadMaterialFromDOM( DOM: TDOMElement );
        procedure LoadModifierFromDOM( DOM: TDOMElement );
        function LoadWeightGroupsFromDOM( DOM: TDOMElement ): Boolean;

      public
        VertexGroups: TP3DIntBufferGL;
        VertexGroupsf: TP3DVec4BufferGL;
        VertexWeights: TP3DVec3BufferGL;
        Positions: TP3DVec3BufferGL;
        Normals: TP3DVec3BufferGL;
        Tangents: TP3DVec3BufferGL;
        Cotangents: TP3DVec3BufferGL;
        Faces: TP3DFaceList;
        TexCoords: array of TP3DVec2BufferGL;
        Edges: TP3DIntBufferGL;

        Indices: TP3DIntBufferGL;
        VBArray: TP3DVertexBufferArray;
        MaterialGroups: TP3DMeshMaterialGroupList;
        WeightGroups: TP3DMeshWeightGroupList;

        constructor Create( const AData: TP3DLibrary = nil ); override;

        procedure FastDestroy; override;

        procedure LoadFromDOM( ADOMNode: TDOMElement ); override;
        procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
        function SaveToDOM(AParent: TDOMElement): TDOMElement; override;
        class function DOMNodeName: String; override;
        procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor ); override;
        procedure RenderInstanced( world: TMat4List; Scene: TP3DScene; RenderObject: TP3DActor );
        procedure RenderSimple;
        procedure RenderMaterialGroup( world: TMat4; Idx: Integer; Scene: TP3DScene );
        procedure RenderMaterialGroupInstanced( world: TMat4List; Idx: Integer; Scene: TP3DScene );
        procedure RenderMaterialGroupInstanced( world: TP3DTexture; Idx: Integer; Scene: TP3DScene ); //not implemented
        procedure RenderMaterialGroupInstanced( world: TMat4List; Idx: Integer );
        procedure RenderMaterialGroupInstanced( world: TP3DTexture; numInstance: Integer; Idx: Integer );
        procedure RenderMaterialGroupInstanced( numInstance: Integer; MatIdx: Integer );
        procedure RenderEdgesInstanced( numInstance: Integer );
        procedure RenderEdges( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor );
        function CastRay( Orig, Dir: TVec3; Actor: TP3DActor; out HitPos: TVec3 ): Boolean; override;
        procedure UnpackBuffers;
        procedure ClearVBO;
        procedure CreateVBO;
        procedure CalcCotangentsFromTangents;
        procedure Calc_Tangents_Cotangents;
        function CalcBoundingBox(): TP3DBoundingBox; override;
        procedure RemoveFromFields( Block: TP3DDataBlock ); override;
        //        procedure ClearChildren;
        property Modifiers: TP3DMeshModifierList read FModifiers write FModifiers;
        property FileWatch: TP3DFileWatch read FFileWatch write FFileWatch;
//        property Armature: TP3DArmature read FArmature write SetArmature;
      end;
      TP3DPropertyAccessMesh = specialize gP3DPropertyAccessDataBlock < TP3DMesh >;
      TP3DMeshList = specialize gP3DCustomDatablockList < TP3DMesh >;
var
  P3DMeshModifierClassFactory: TP3DMeshModifierClassFactory;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DMesh }
procedure LoadFaceFromStream( Faces: TP3DFaceList; S: TStream; ACount, LoopCount: Integer );
var
  ints: TIntList;
  base, i: Integer;
begin
  if ( ACount = 0 ) then
    exit;
  base:= Faces.Count;
  Faces.Count:= base + ACount;
  ints:= TIntList.Create;
  ints.Count:= ACount;
  ints.LoadFromStream( S, ACount );

  Faces[ 0 ]:= ivec2( 0 );

  //Write( 'ints = ', ints[ 0 ]);
  if ( ACount > 1 ) then
    for i:= 1 to ACount - 1 do
      begin
        //Write( ', ', ints[ i ]);
        Faces[ i ]:= ivec2( ints[ i ], 0 );
        Faces[ i - 1 ]:= ivec2( Faces[ i - 1 ].X, ints[ i ] - ints[ i - 1 ]);
      end;
//  WriteLn();
  Faces[ ACount - 1 ]:= ivec2( Faces[ ACount - 1 ].X, LoopCount - Faces[ ACount - 1 ].X );
  ints.Free;
//  for i:= 0 to ACount - 1 do
//    WriteLn( 'Face ', i, ': ', Faces[ i ].X, '-', Faces[ i ].Y );
end;

procedure LoadVec4FromStream( Vectors: TVec4List; S: TStream; ACount: Integer );
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
  MatIdx:= Owner.AppendFromPath( AName, TP3DMaterial );
  MatPolyStart:= StrToInt( DOM.GetAttribute( 'start' ));
  MatPolyEnd:= StrToInt( DOM.GetAttribute( 'end' ));

  MaterialGroup:= TP3DMeshMaterialGroup.Create( Self );
  MaterialGroup.Material:= TP3DMaterialShader( Owner.DataBlocks[ MatIdx ]);
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
    'armature': M:= TP3DMeshModifierArmature.CreateFromDOM( DOM, Self );
    else
      raise Exception.Create( 'Cannot create mesh modifier. Modifier type not found!' ); //TODO: Improve msg
  end;
  if ( Assigned( M )) then
    Modifiers.Add( M );
end;

function TP3DMesh.LoadWeightGroupsFromDOM(DOM: TDOMElement): Boolean;
var
  L: TDOMNodeList;
  i: Integer;
  grp: TP3DMeshWeightGroup;
begin
  L:= DOM.GetElementsByTagName( 'weightgroup' );

  for i:= 0 to L.Count - 1 do
    begin
      grp:= TP3DMeshWeightGroup.Create( Self );
      grp.Name:= TDOMElement( L.Item[ i ]).GetAttribute( 'name' );
      WeightGroups.Add( grp );
    end;

  Result:= L.Count > 0;
end;

constructor TP3DMesh.Create(const AData: TP3DLibrary);
begin
  inherited;

  Positions:= TP3DVec3BufferGL.Create( True );
  Normals:= TP3DVec3BufferGL.Create( True );
  Cotangents:= TP3DVec3BufferGL.Create( True );
  Tangents:= TP3DVec3BufferGL.Create( True );
  Indices:= TP3DIntBufferGL.Create( True );
  Edges:= TP3DIntBufferGL.Create( True );
  VertexGroups:= TP3DIntBufferGL.Create( True );
  VertexWeights:= TP3DVec3BufferGL.Create( True );
  VertexGroupsf:= TP3DVec4BufferGL.Create( True );
  VBArray:= TP3DVertexBufferArray.Create;
  Faces:= TP3DFaceList.Create;

  MaterialGroups:= TP3DMeshMaterialGroupList.Create( Self );
  WeightGroups:= TP3DMeshWeightGroupList.Create( Self );
  Modifiers:= TP3DMeshModifierList.Create( Self );

  Owner.Meshes.Add( Self );
  FileWatch:= TP3DFileWatch.Create( '' );
end;

procedure TP3DMesh.FastDestroy;
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
  VertexGroups.Free;
  VertexGroupsf.Free;
  VertexWeights.Free;
  Faces.Free;
  for i:= 0 to High( TexCoords ) do
    TexCoords[ i ].Free;

  VBArray.Free;
  MaterialGroups.Clear( True );
  MaterialGroups.Free;
  WeightGroups.Clear( True );
  WeightGroups.Free;
  FileWatch.Free;

  Owner.Meshes.Remove( Self );

  inherited FastDestroy;
end;

procedure TP3DMesh.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  ext: String;
  cnt, numTex, i: Integer;
  F: TFileStream;
begin
  inherited LoadFromDOM( ADOMNode );
  //Name:= ADOMNode.GetAttribute( 'name' );

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
          begin
            LoadVec3FromStream( Positions, F, cnt );
            if ( LoadWeightGroupsFromDOM( ADOMNode )) then
              begin
                LoadVec3FromStream( VertexWeights, F, cnt );
                //for i:= 0 to 50 do
                 // WriteLn( VertexWeights[ i ].ToString());
                LoadIntFromStream( VertexGroups, F, cnt * 4 );
                //for i:= 0 to VertexGroups.Count div 4 - 1 do
                //  WriteLn( Format( '%d, %d, %d, %d', [ VertexGroups[ i * 4 ], VertexGroups[ i * 4 + 1 ], VertexGroups[ i * 4 + 2 ], VertexGroups[ i * 4 + 3 ]]));
              end;
          end;

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
          LoadFaceFromStream( Faces, F, cnt, Edges.Count );
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
        'weightgroup': ; //Do nothing. Loaded elsewhere!
      else
        raise Exception.Create( 'Unknown tag inside Mesh Element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  //CalcCotangentsFromTangents;
  //Calc_Tangent_Binormal;
  UnpackBuffers;
end;

procedure TP3DMesh.LoadFromDOMNew(ADOMNode: TDOMElement);
var
  FN, SPos: String;
  S: TFileStream;
  i: Integer;
  vec: TVec4;

  function SetupStream( AName: String ): Boolean;
  var
    P: Integer;
  begin
    Result:= False;
    SPos:= ADOMNode.AttribStrings[ AName ];
    if ( SPos = '' ) then
      exit;
    FN:= Copy2SymbDel( SPos, ':' );
    P:= StrToInt( SPos );
    if ( Assigned( S )) then
      if ( CompareFilenames( S.FileName, FN ) <> 0 ) then
        S.Free;
    if ( not Assigned( S )) then
      S:= TFileStream.Create( FN, fmOpenRead );
    S.Position:= P;
    Result:= True;
  end;

begin
  S:= nil;
  inherited LoadFromDOMNew(ADOMNode);
  MaterialGroups.LoadFromDOM( ADOMNode );
  WeightGroups.LoadFromDOM( ADOMNode );
  Modifiers.LoadFromDOM( ADOMNode );

  ClearVBO;

  if ( SetupStream( 'Positions' )) then
    Positions.LoadFromStream( S );

  if ( SetupStream( 'Normals' )) then
    Normals.LoadFromStream( S );

  if ( SetupStream( 'Tangents' )) then
    Tangents.LoadFromStream( S );

  if ( SetupStream( 'Cotangents' )) then
    Cotangents.LoadFromStream( S );

  if ( SetupStream( 'Faces' )) then
    Faces.LoadFromStream( S );

  i:= 0;
  for i:= 0 to High( TexCoords ) do
    TexCoords[ i ].Free;

  while ( SetupStream( 'TexCoords' + IntToStr( i ))) do
    begin
      SetLength( TexCoords, i + 1 );
      TexCoords[ i ]:= TP3DVec2BufferGL.Create( True );
      TexCoords[ i ].LoadFromStream( S );
      Inc( i );
    end;

  if ( SetupStream( 'Indices' )) then
    Indices.LoadFromStream( S );

  if ( SetupStream( 'Edges' )) then
    Edges.LoadFromStream( S );

  if ( SetupStream( 'VertexGroups' )) then
    VertexGroups.LoadFromStream( S );

  if ( SetupStream( 'VertexWeights' )) then
    VertexWeights.LoadFromStream( S );

  VertexGroupsf.Count:= VertexGroups.Count;
  for i:= 0 to VertexGroups.Count div 4 - 1 do
    begin
      vec:= vec4( VertexGroups[ i * 4 ], VertexGroups[ i * 4 + 1 ], VertexGroups[ i * 4 + 2 ], VertexGroups[ i * 4 + 3 ]);
      VertexGroupsf[ i ]:= vec;
    end;

  CreateVBO;
  BoundingBox:= CalcBoundingBox();

  S.Free;
end;

function TP3DMesh.SaveToDOM(AParent: TDOMElement): TDOMElement;
var
  fn, comment: String;
  S: TFileStream;
  i: Integer;
begin
  Result:= inherited SaveToDOM( AParent );

  comment:= 'summary for mesh ' + Name + LineEnding;
  comment+= '  Positions: ' + IntToStr( Positions.Count ) + LineEnding;
  comment+= '  Normals: ' + IntToStr( Normals.Count ) + LineEnding;
  comment+= '  Tangents: ' + IntToStr( Tangents.Count ) + LineEnding;
  comment+= '  Cotangents: ' + IntToStr( Cotangents.Count ) + LineEnding;
  comment+= '  Faces: ' + IntToStr( Faces.Count ) + LineEnding;

  for i:= 0 to high( TexCoords ) do
    comment+= '  TexCoords' + IntToStr( i ) + ': ' + IntToStr( TexCoords[ i ].Count ) + LineEnding;

  comment+= '  Edges: ' + IntToStr( Edges.Count ) + LineEnding;
  comment+= '  Indices: ' + IntToStr( Indices.Count ) + LineEnding;

  comment+= '  VertexGroups: ' + IntToStr( VertexGroups.Count ) + LineEnding;
  comment+= '  VertexWeights: ' + IntToStr( VertexWeights.Count ) + LineEnding;

  Result.AppendChild( Result.OwnerDocument.CreateComment( comment ));

  fn:= ExtractFileNameOnly( Owner.Name ) + '.' + Name + '.p3dmesh';
  S:= TFileStream.Create( fn, fmCreate );

  Result.AttribStrings[ 'Positions' ]:= fn + ':' + IntToStr( S.Position );
  Positions.SaveToStream( S );

  Result.AttribStrings[ 'Normals' ]:= fn + ':' + IntToStr( S.Position );
  Normals.SaveToStream( S );

  Result.AttribStrings[ 'Tangents' ]:= fn + ':' + IntToStr( S.Position );
  Tangents.SaveToStream( S );

  Result.AttribStrings[ 'Cotangents' ]:= fn + ':' + IntToStr( S.Position );
  Cotangents.SaveToStream( S );

  Result.AttribStrings[ 'Faces' ]:= fn + ':' + IntToStr( S.Position );
  Faces.SaveToStream( S );

  for i:= 0 to high( TexCoords ) do
    begin
      Result.AttribStrings[ 'TexCoords' + IntToStr( i )]:= fn + ':' + IntToStr( S.Position );
      TexCoords[ i ].SaveToStream( S );
    end;

  Result.AttribStrings[ 'Edges' ]:= fn + ':' + IntToStr( S.Position );
  Edges.SaveToStream( S );

  Result.AttribStrings[ 'Indices' ]:= fn + ':' + IntToStr( S.Position );
  Indices.SaveToStream( S );

  Result.AttribStrings[ 'VertexGroups' ]:= fn + ':' + IntToStr( S.Position );
  VertexGroups.SaveToStream( S );

  Result.AttribStrings[ 'VertexWeights' ]:= fn + ':' + IntToStr( S.Position );
  VertexWeights.SaveToStream( S );

  S.Free;

  MaterialGroups.SaveToDOM( Result );
  WeightGroups.SaveToDOM( Result );
  Modifiers.SaveToDOM( Result );
end;

class function TP3DMesh.DOMNodeName: String;
begin
  Result:= 'mesh';
end;

procedure TP3DMesh.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DActor);
var
  i: Integer;
begin
  for i:= 0 to MaterialGroups.Count - 1 do
    RenderMaterialGroup( world, i, Scene );
end;

procedure TP3DMesh.RenderInstanced( world: TMat4List; Scene: TP3DScene; RenderObject: TP3DActor );
begin

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
  Material: TP3DMaterial;
  Modifier: TP3DMeshModifier;
  offset: Integer;
  count, i, inst: Integer;
  decl: TP3DShaderDecl;

begin
  if (( Idx < 0 ) or ( Idx >= MaterialGroups.Count )) then
    exit;

  Material:= MaterialGroups[ Idx ].Material;
  if (( not Assigned( Material )) or ( not Assigned( Material.Shader ))) then
    exit;
  Material.PassToShader();
  for Modifier in Modifiers do
    Modifier.PassToShader( Material, world );
  Scene.PassToShader;
  if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
    decl.AsMat4:= world;

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
  decl:= Material.Shader.Attributes.Names[ 'fjointID' ];
  if ( Assigned( decl )) and ( VertexGroupsf.Count > 0 ) then
    VertexGroupsf.SetAttribArray( decl.Addr );
  decl:= Material.Shader.Attributes.Names[ 'jointWeight' ];
  if ( Assigned( decl )) and ( VertexWeights.Count > 0 ) then
    VertexWeights.SetAttribArray( decl.Addr );

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );

  offset:= MaterialGroups[ Idx ].IndexStart;
  count:= ( MaterialGroups[ Idx ].IndexEnd - MaterialGroups[ Idx ].IndexStart ) + 1;
  //P3DLog.LogInfo( Self, Material.Shader.DumpUniforms );

  //glDrawElements( GL_TRIANGLES, count, GL_UNSIGNED_INT, Pointer( offset * SizeOf( GLint ))); cle( Self );
  glDrawElementsInstanced( GL_TRIANGLES, count, GL_UNSIGNED_INT, Pointer( offset * SizeOf( GLint )), 1 ); cle( Self );
end;

procedure TP3DMesh.RenderMaterialGroupInstanced(world: TMat4List; Idx: Integer; Scene: TP3DScene);
var
  size: Integer;
  Tex: TP3DTexture;
begin
  //Credit goes to threejs: Skeleton.js
  size:= Max( 4, ceil( log2( sqrt( world.Count * 4 )))); // 4 pixels needed for 1 matrix

  Tex:= TP3DTexture.CreateFromBuffer( size, size, 4, dtFloat, world.Ptr );

  RenderMaterialGroupInstanced( Tex, Idx, Scene );

  Tex.Free;
end;

procedure TP3DMesh.RenderMaterialGroupInstanced(world: TP3DTexture; Idx: Integer; Scene: TP3DScene);
begin

end;

procedure TP3DMesh.RenderMaterialGroupInstanced(world: TMat4List; Idx: Integer);
var
  size: Integer;
  Tex: TP3DTexture;
begin
  //Credit goes to threejs: Skeleton.js
  size:= Max( 4, ceil( log2( sqrt( world.Count * 4 )))); // 4 pixels needed for 1 matrix

  Tex:= TP3DTexture.CreateFromBuffer( size, size, 4, dtFloat, world.Ptr );
  Tex.MipMap:= False;
  Tex.Filtering:= tfNearest;
  Tex.FilteringMipMap:= tfNearest;

  RenderMaterialGroupInstanced( Tex, world.Count, Idx );

  Tex.Free;
end;

procedure TP3DMesh.RenderMaterialGroupInstanced(world: TP3DTexture; numInstance: Integer; Idx: Integer);
var
  decl: TP3DShaderDecl;
  Modifier: TP3DMeshModifier;
  i: Integer;
  offset, count: LongInt;
  mat: TMat4;
begin
  //for Modifier in Modifiers do
  //  Modifier.PassToShader( MaterialGroups[ Idx ].Material, world );

  world.Bind( MaterialGroups[ idx ].Material.Maps.Count );

  P3DShaderActive.Uniforms.FindByName( 'world_instance_w', decl );
  if ( Assigned( decl )) then
    decl.AsInt:= world.Width;
  P3DShaderActive.Uniforms.FindByName( 'world_instance_h', decl );
  if ( Assigned( decl )) then
    decl.AsInt:= world.Height;
  P3DShaderActive.Uniforms.FindByName( 'world_instance', decl );
  if ( Assigned( decl )) then
    decl.AsInt:= MaterialGroups[ idx ].Material.Maps.Count;

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

  mat:= Mat4Identity;
  {for Modifier in Modifiers do
    Modifier.PassToShader( MaterialGroups[ Idx ].Material, mat );

  decl:= P3DShaderActive.Attributes.Names[ 'fjointID' ];
  if ( Assigned( decl )) and ( VertexGroupsf.Count > 0 ) then
    VertexGroupsf.SetAttribArray( decl.Addr );
  decl:= P3DShaderActive.Attributes.Names[ 'jointWeight' ];
  if ( Assigned( decl )) and ( VertexWeights.Count > 0 ) then
    VertexWeights.SetAttribArray( decl.Addr );}

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );

  offset:= MaterialGroups[ Idx ].IndexStart;
  count:= ( MaterialGroups[ Idx ].IndexEnd - MaterialGroups[ Idx ].IndexStart ) + 1;
  //P3DLog.LogInfo( Self, Material.Shader.DumpUniforms );

  glDrawElementsInstanced( GL_TRIANGLES, count, GL_UNSIGNED_INT, Pointer( offset * SizeOf( GLint )), numInstance ); cle( Self );
end;

procedure TP3DMesh.RenderMaterialGroupInstanced(numInstance: Integer; MatIdx: Integer);
var
  i: Integer;
  decl: TP3DShaderDecl;
  Modifier: TP3DMeshModifier;
  offset, count: Integer;
  mat: TMat4;
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
//  VBArray.Bind;

  decl:= P3DShaderActive.Attributes.Names[ 'fjointID' ];
  if ( Assigned( decl )) and ( VertexGroupsf.Count > 0 ) then
    VertexGroupsf.SetAttribArray( decl.Addr );
  decl:= P3DShaderActive.Attributes.Names[ 'jointWeight' ];
  if ( Assigned( decl )) and ( VertexWeights.Count > 0 ) then
    VertexWeights.SetAttribArray( decl.Addr );

  mat:= Mat4Identity; //only a workaround
  for Modifier in Modifiers do
    Modifier.PassToShader( P3DMaterialActive, mat );

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );

  if ( MatIdx < 0 ) then
    begin
      offset:= 0;
      count:= Indices.Count;
    end
  else
    begin
      offset:= MaterialGroups[ MatIdx ].IndexStart;
      count:= ( MaterialGroups[ MatIdx ].IndexEnd - MaterialGroups[ MatIdx ].IndexStart ) + 1;
    end;

  //WriteLn( P3DShaderActive.DumpAttributes );
  glDrawElementsInstanced( GL_TRIANGLES, count, GL_UNSIGNED_INT, Pointer( offset * SizeOf( GLint )), numInstance ); cle( Self );
  //VBArray.Unbind;

  if ( Positions.Count > 0 ) then
    Positions.UnsetAttribArray();
  if ( Normals.Count > 0 ) then
    Normals.UnsetAttribArray();
  if ( Cotangents.Count > 0 ) then
    Cotangents.UnsetAttribArray();
  if ( Tangents.Count > 0 ) then
    Tangents.UnsetAttribArray();
  for i:= 0 to High( TexCoords ) do
    TexCoords[ i ].UnsetAttribArray();
end;

procedure TP3DMesh.RenderEdgesInstanced(numInstance: Integer);
var
  i: Integer;
  decl: TP3DShaderDecl;
  mat: TMat4;
  Modifier: TP3DMeshModifier;
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
//  VBArray.Bind;

  decl:= P3DShaderActive.Attributes.Names[ 'fjointID' ];
  if ( Assigned( decl )) and ( VertexGroupsf.Count > 0 ) then
    VertexGroupsf.SetAttribArray( decl.Addr );
  decl:= P3DShaderActive.Attributes.Names[ 'jointWeight' ];
  if ( Assigned( decl )) and ( VertexWeights.Count > 0 ) then
    VertexWeights.SetAttribArray( decl.Addr );

  mat:= Mat4Identity; //only a workaround
  for Modifier in Modifiers do
    Modifier.PassToShader( P3DMaterialActive, mat );

  Edges.Bind( GL_ELEMENT_ARRAY_BUFFER );

  glDrawElements( GL_LINES, Edges.Count, GL_UNSIGNED_INT, Pointer( 0 )); cle( Self );
  //VBArray.Unbind;

  if ( Positions.Count > 0 ) then
    Positions.UnsetAttribArray();
  if ( Normals.Count > 0 ) then
    Normals.UnsetAttribArray();
  if ( Cotangents.Count > 0 ) then
    Cotangents.UnsetAttribArray();
  if ( Tangents.Count > 0 ) then
    Tangents.UnsetAttribArray();
  for i:= 0 to High( TexCoords ) do
    TexCoords[ i ].UnsetAttribArray();
end;

procedure TP3DMesh.RenderEdges(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DActor);
var
  i: Integer;
  decl: TP3DShaderDecl;
begin
  Scene.PassToShader;

  if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
    decl.AsMat4:= world;

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
  _Normals, _Weights: TP3DVec3BufferGL;
  _Edges, _WeightGroups: TP3DIntBufferGL;

  vidx: Integer;

begin
  ClearVBO;
  _Positions:= TP3DVec3BufferGL.Create( True );
  _Normals:= TP3DVec3BufferGL.Create( True );
  _Edges:= TP3DIntBufferGL.Create( True );
  _Weights:= TP3DVec3BufferGL.Create( True );
  _WeightGroups:= TP3DIntBufferGL.Create( True );
  VertexGroupsf.Clear;

  for i:= 0 to Faces.Count - 1 do
    begin
      Idx:= Indices.Count;
      vbase:= _Positions.Count;
      for j:= 0 to Faces[ i ].Y - 1 do
        begin
          vidx:= Edges[ Faces[ i ].X + j ];
          _Positions.Add( Positions[ vidx ]);
          _Normals.Add( Normals[ vidx ]);
          _Weights.Add( VertexWeights[ vidx ]);
          _WeightGroups.Add([ VertexGroups[ vidx * 4 ],     VertexGroups[ vidx * 4 + 1 ],
                              VertexGroups[ vidx * 4 + 2 ], VertexGroups[ vidx * 4 + 3 ]]);
          VertexGroupsf.Add(vec4( VertexGroups[ vidx * 4 ], VertexGroups[ vidx * 4 + 1 ], VertexGroups[ vidx * 4 + 2 ], VertexGroups[ vidx * 4 + 3 ]));
        end;
      for j:= 1 to Faces[ i ].Y - 1 do
        begin
          _Edges.Add([ vbase + j -1, vbase + j ]);
          if ( j > 1 ) then
            Indices.Add([ vbase,
                          vbase + j - 1,
                          vbase + j ]);
        end;
      _Edges.Add([ vbase + Faces[ i ].Y - 1, vbase ]);
      Faces[ i ]:= ivec2( Idx, Indices.Count - Idx );
      //WriteLn( 'FACE: ', i, ' (idx) ', Faces[ i ].loop_start, ' (loopstart) ',
      //         Faces[ i ].loop_count, ' (loopcount) ');
    end;
  for i:= 0 to MaterialGroups.Count - 1 do
    begin
      MaterialGroups[ i ].FIndexStart:= Faces[ MaterialGroups[ i ].PolyStart ].X;
      MaterialGroups[ i ].FIndexEnd:= Faces[ MaterialGroups[ i ].PolyEnd ].X
                                    + Faces[ MaterialGroups[ i ].PolyEnd ].Y - 1;
//      WriteLn( 'GRP: ', i, ' (idx) ', MaterialGroups[ i ].PolyStart, ' (polystart) ',
//               MaterialGroups[ i ].PolyEnd, ' (polyend) Faces[ polystart ] = ' + Faces[ MaterialGroups[ i ].PolyStart ].ToString() + ' Faces[ polyend ] = ' + Faces[ MaterialGroups[ i ].PolyEnd ].ToString());
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

  VertexGroups.Free;
  VertexGroups:= _WeightGroups;
  VertexWeights.Free;
  VertexWeights:= _Weights;

  CreateVBO;
  BoundingBox:= CalcBoundingBox();
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
  if ( VertexGroups.Count > 0 ) then
    begin
      VertexGroups.PushData;
      //VertexGroups.SetAttribArray( jointID );
    end;
  if ( VertexWeights.Count > 0 ) then
    begin
      VertexWeights.PushData;
      //VertexWeights.SetAttribArray(  );
    end;
  if ( VertexGroupsf.Count > 0 ) then
    begin
      VertexGroupsf.PushData;
      //VertexWeights.SetAttribArray(  );
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

procedure TP3DMesh.RemoveFromFields(Block: TP3DDataBlock);
var
  i: Integer;
begin
  for i:= 0 to MaterialGroups.Count - 1 do
    if ( MaterialGroups[ i ].Material = Block ) then
      MaterialGroups[ i ].Material:= nil;
  inherited RemoveFromFields( Block );
end;

{ TP3DMeshMaterialGroup }

procedure TP3DMeshMaterialGroup.SetMaterial( AValue: TP3DMaterial );
begin
  if FMaterial=AValue then Exit;

  if ( Assigned( FMaterial )) then
    Material.RemoveUser( ParentMesh );

  FMaterial:= AValue;

  if ( Assigned( FMaterial )) then
    Material.AddUser( ParentMesh );
end;

constructor TP3DMeshMaterialGroup.Create(AParentMesh: TP3DMesh);
begin
  ParentMesh:= AParentMesh;
  inherited Create;

  Properties.Context:= AParentMesh;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'PolyStart' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'PolyEnd' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'IndexStart', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'IndexEnd', smAttribute ));
  Properties.Add( TP3DPropertyAccessMaterial.CreateFieldSet( 'Material', @FMaterial, @SetMaterial, smAttribute ));
end;

constructor TP3DMeshMaterialGroup.CreateFromDOM(ADOMNode: TDOMElement; AParentMesh: TP3DMesh);
begin
  Create( AParentMesh );
  LoadFromDOMNew( ADOMNode );
end;

class function TP3DMeshMaterialGroup.DOMNodeName: String;
begin
  Result:= 'materialgroup';
end;

{ TP3DMeshModifier }

constructor TP3DMeshModifier.Create(AMesh: TP3DMesh);
begin
  inherited Create;
  Mesh:= AMesh;
  Properties.Context:= Mesh;
  Properties.Add( TP3DPropertyAccessMesh.CreateField( 'Mesh', @FMesh, smAttribute ));
  Properties.Add( TP3DPropertyAccessString.CreateGetSet( 'Type', @GetType, nil, smAttribute ));
end;

constructor TP3DMeshModifier.CreateFromDOM(ADOMNode: TDOMElement; AMesh: TP3DMesh);
begin
  Create( AMesh );
  LoadFromDOM( ADOMNode );
end;

class function TP3DMeshModifier.DOMNodeName: String;
begin
  Result:= 'modifier';
end;

function TP3DMeshModifier.GetType: String;
begin
  Result:= ClassName;
end;

{ TP3DMeshWeightGroup }

function TP3DMeshWeightGroup.GetWeight( VIndex: Integer ): Float;
begin
  Result:= 0.0;//Mesh.;
end;

constructor TP3DMeshWeightGroup.CreateFromDOM(ADOMNode: TDOMElement);
begin
  inherited Create;
  LoadFromDOMNew( ADOMNode );
end;

class function TP3DMeshWeightGroup.DOMNodeName: String;
begin
  Result:= 'weightgroup';
end;

constructor TP3DMeshWeightGroup.Create(AMesh: TP3DMesh);
begin
  inherited Create;
  Mesh:= AMesh;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Name', smAttribute ));
end;


{ TP3DMeshMaterialGroupList }

constructor TP3DMeshMaterialGroupList.Create(AParentMesh: TP3DMesh);
begin
  inherited Create;
  FParentMesh:= AParentMesh;
end;

function TP3DMeshMaterialGroupList.Find( Material: TP3DMaterial ): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Material = Material ) then
      begin
        Result:= i;
        break;
      end;
end;

procedure TP3DMeshMaterialGroupList.SaveToDOM(AParent: TDOMElement);
var
  Node: TDOMElement;
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    begin
      Items[ i ].SaveToDOM( AParent );
      {Node:= AParent.OwnerDocument.CreateElement( 'material' );
      AParent.AppendChild( Node );
      Node.AttribStrings[ 'start' ]:= IntToStr( Items[ i ].PolyStart );
      Node.AttribStrings[ 'end' ]:= IntToStr( Items[ i ].PolyEnd );
      if ( Assigned( Items[ i ].Material  )) then
        begin
          Items[ i ].Material.SaveToDOM( AParent.OwnerDocument.DocumentElement );
//          Node.AttribStrings[ 'data' ]:= Items[ i ].Material.DOMPath;
        end;}
    end;
end;

procedure TP3DMeshMaterialGroupList.LoadFromDOM(ADOMNode: TDOMElement);
var
  i: Integer;
  lst: TDOMNodeList;
begin
  Clear();
  lst:= ADOMNode.GetElementsByTagName( TP3DMeshMaterialGroup.DOMNodeName );
  try
    Count:= lst.Count;
    for i:= 0 to lst.Count - 1 do
      Items[ i ]:= TP3DMeshMaterialGroup.CreateFromDOM( TDOMElement( lst[ i ]), ParentMesh );

  finally
    lst.Free;
  end;
end;

{ TP3DMeshModifierList }

procedure TP3DMeshModifierList.SaveToDOM(AParent: TDOMElement);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure TP3DMeshModifierList.LoadFromDOM(ADOMNode: TDOMElement);
var
  i: Integer;
  lst: TDOMNodeList;
  ClName: DOMString;
  Tp: TP3DMeshModifierClass;
begin
  Clear();
  lst:= ADOMNode.GetElementsByTagName( TP3DMeshModifier.DOMNodeName );
  try
    Count:= lst.Count;
    for i:= 0 to lst.Count - 1 do
      begin
        ClName:= TDOMElement( lst[ i ]).GetAttribute( 'Type' );
        Tp:= P3DMeshModifierClassFactory.FindByName( ClName );
        if ( Assigned( Tp )) then
          begin
            Items[ i ]:= Tp.Create( ParentMesh );
            Items[ i ].LoadFromDOMNew( TDOMElement( lst[ i ]));
          end
        else
          raise Exception.Create( 'Cannot load mesh modifier from DOM: The specified modifier class was not found ' + ClName );
      end;

  finally
    lst.Free;
  end;
end;

constructor TP3DMeshModifierList.Create(AParentMesh: TP3DMesh);
begin
  inherited Create;
  FParentMesh:= AParentMesh;
end;

{ TP3DMeshWeightGroupList }

constructor TP3DMeshWeightGroupList.Create(AParentMesh: TP3DMesh);
begin
  inherited Create;
  ParentMesh:= AParentMesh;
end;

procedure TP3DMeshWeightGroupList.SaveToDOM(AParent: TDOMElement);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure TP3DMeshWeightGroupList.LoadFromDOM(ADOMNode: TDOMElement);
var
  i: Integer;
  lst: TDOMNodeList;
begin
  Clear();
  lst:= ADOMNode.GetElementsByTagName( TP3DMeshWeightGroup.DOMNodeName );
  try
    Count:= lst.Count;
    for i:= 0 to lst.Count - 1 do
      begin
        Items[ i ]:= TP3DMeshWeightGroup.Create( ParentMesh );
        Items[ i ].LoadFromDOMNew( TDOMElement( lst[ i ]));
      end;

  finally
    lst.Free;
  end;
end;

{$ENDIF}

{$IFDEF INITIALIZATION}
  P3DMeshModifierClassFactory:= TP3DMeshModifierClassFactory.Create;
{$ENDIF}

{$IFDEF FINALIZATION}
  FreeAndNil( P3DMeshModifierClassFactory );
{$ENDIF}
