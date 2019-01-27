//p3dmesh.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
  type
    TP3DMesh = class;
    TP3DRenderFlag = ( rfShadowMap, rfWireFrame, rfDebugShowLocation, rfDebugShowAABB, rfDebugShowArmature );
    TP3DRenderFlags = set of TP3DRenderFlag;

    { TP3DMeshMaterialGroup }

    {TP3DMeshMaterialGroup = class ( TP3DStreamable )
      private
        FIndexEnd: Integer;
        FIndexStart: Integer;
        FMaterial: TP3DStreamablePointer;
        FParentMesh: TP3DMesh;
        FPolyEnd: Integer;
        FPolyStart: Integer;

        function GetMaterial: TP3DMaterial;
        procedure SetMaterial( AValue: TP3DMaterial );
        procedure SetPolyEnd(AValue: Integer);
        procedure SetPolyStart(AValue: Integer);

      public
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      published
        property PolyStart: Integer read FPolyStart write SetPolyStart;
        property PolyEnd: Integer read FPolyEnd write SetPolyEnd;
        property IndexStart: Integer read FIndexStart write FIndexStart;
        property IndexEnd: Integer read FIndexEnd write FIndexEnd;
        property Material: TP3DMaterial read GetMaterial write SetMaterial;
        property ParentMesh: TP3DMesh read FParentMesh write FParentMesh;
    end;

    {TP3DMeshMaterialGroupContainer = specialize gP3DStreamableContainer < TP3DMeshMaterialGroup >;
    TP3DMeshMaterialGroupPointer = specialize gP3DStreamablePointer < TP3DMeshMaterialGroup >;
    //TP3DMeshMaterialGroupContainerList = specialize gP3DStreamableContainerList < TP3DMeshMaterialGroup, TP3DMeshMaterialGroupContainer >;
    TP3DMeshMaterialGroupPointerList = specialize gP3DStreamableList < TP3DMeshMaterialGroup, TP3DMeshMaterialGroupPointer >;
    }
    { TP3DMeshMaterialGroupList }

    TP3DMeshMaterialGroupContainerList = class( specialize gP3DStreamableContainerList < TP3DMeshMaterialGroup >)
      private
        FParentMesh: TP3DMesh;

      public
        function Find( Material: TP3DMaterial ): Integer; overload;
        procedure Add( PolyStart: Integer; PolyEnd: Integer; Material: TP3DMaterial ); overload;
        procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;

        property ParentMesh: TP3DMesh read FParentMesh;
    end;}

    // List of Mesh faces, where ...
    // X = start loop index, Y = loop count
    {TP3DFace = object( TIVec2 )
      property IndexStart: Integer read FCoord[ 0 ] write FCoord[ 0 ];
      property IndexEnd: Integer read FCoord[ 1 ] write FCoord[ 1 ];
    end;}
    TP3DFace = TIVec2;

    TP3DFaceList = TIVec2List;

    { TP3DMeshModifier }

    TP3DMeshModifier = class abstract( TP3DStreamable )
      private
        FMesh: TP3DStreamablePointer;

        function GetMesh: TP3DMesh;
        procedure SetMesh( AValue: TP3DMesh ); virtual;

      public
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

        function GetType: String;

        procedure PassToShader( Material: TP3DMaterial; var world: TMat4 ); virtual; abstract;

      published
        property Mesh: TP3DMesh read GetMesh write SetMesh;
    end;

    {TP3DMeshModifierContainer = specialize gP3DStreamableContainer < TP3DMeshModifier >;
    TP3DMeshModifierPointer = specialize gP3DStreamablePointer < TP3DMeshModifier >;
    //TP3DMeshModifierContainerList = specialize gP3DStreamableContainerList < TP3DMeshModifier, TP3DMeshModifierContainer >;
    TP3DMeshModifierPointerList = specialize gP3DStreamableList < TP3DMeshModifier, TP3DMeshModifierPointer >;
    }
    { TP3DMeshModifierList }

    TP3DMeshModifierContainerList = class( specialize gP3DStreamableContainerList < TP3DMeshModifier >)
      private
        FParentMesh: TP3DMesh;

      public
        procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;

        property ParentMesh: TP3DMesh read FParentMesh;
    end;

    TP3DMeshModifierClass = class of TP3DMeshModifier;

    TP3DMeshModifierClassFactory = specialize gP3DClassFactory < TP3DMeshModifierClass >;

    { TP3DMeshWeightGroup }

    TP3DMeshWeightGroup = class( TP3DNamedStreamable )
      private
        FIndex: Integer;
        FMesh: TP3DMesh;

        function GetWeight( VIndex: Integer ): Float;

      public
        //property Weight[ VIndex: Integer ]: Float read GetWeight;
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      published
        property Mesh: TP3DMesh read FMesh write FMesh;
        property Index: Integer read FIndex write FIndex;
    end;

    {TP3DMeshWeightGroupContainer = specialize gP3DStreamableContainer < TP3DMeshWeightGroup >;
    TP3DMeshWeightGroupPointer = specialize gP3DStreamablePointer < TP3DMeshWeightGroup >;
    //TP3DMeshWeightGroupContainerList = specialize gP3DNamedStreamableContainerList < TP3DMeshWeightGroup, TP3DMeshWeightGroupContainer >;
    TP3DMeshWeightGroupPointerList = specialize gP3DNamedStreamableList < TP3DMeshWeightGroup, TP3DMeshWeightGroupPointer >;
    }

    { TP3DMeshWeightGroupList }

    { TP3DMeshWeightGroupContainerList }

    TP3DMeshWeightGroupContainerList = class( specialize gP3DNamedStreamableContainerList < TP3DMeshWeightGroup >)
      private
        FParentMesh: TP3DMesh;

      public
        procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;

        property ParentMesh: TP3DMesh read FParentMesh write FParentMesh;
    end;

    { TP3DMesh }
    //TP3DCustomMeshRenderable = specialize gP3DRenderable < TP3DDataBlock >;
    TP3DTexCoordList = TP3DVec2BufferGLList;//specialize TFPGList < TP3DVec2BufferGL >>;
    TP3DMesh = class( TP3DDataBlock, IP3DRenderable, IP3DCollidable, IP3DPlottableLayer )
      protected
        FFileWatch: TP3DFilePointer;
        FMaterial: TP3DStreamablePointer;
        FModifiers: TP3DMeshModifierContainerList;

        Loops: TIntList;
        PackedPositions: TVec3List;
        PackedNormals: TVec3List;
        PackedTangents: TVec3List;
        PackedCotangents: TVec3List;
        PackedFaces: TIntList;
        PackedVertexWeights: TVec3List;
        PackedVertexWeightIndices: TIVec4List;
        //PackedMaterialGroups: TP3DMeshMaterialGroupContainerList;

        procedure BuildIndicesFromFaces;
        function GetMaterial: TP3DMaterial;
        procedure SetMaterial(AValue: TP3DMaterial);

      public
        VertexWeightsAuto: specialize TAuto< TP3DVec4BufferGL >;
        VertexWeightIndicesAuto: specialize TAuto< TP3DVec4BufferGL >; //Must be float because passing ivec attributes is not allowed in GLSL (though some vendors support it)
        PositionsAuto: specialize TAuto< TP3DVec3BufferGL >;
        NormalsAuto: specialize TAuto< TP3DVec3BufferGL >;
        TangentsAuto: specialize TAuto< TP3DVec3BufferGL >;
        CotangentsAuto: specialize TAuto< TP3DVec3BufferGL >;
        Faces: TP3DFaceList;
        TexCoords: TP3DTexCoordList;
        EdgesAuto: specialize TAuto< TP3DIntBufferGL >;

        IndicesAuto: specialize TAuto< TP3DIntBufferGL >;
        VBArray: TP3DVertexBufferArray;
        //MaterialGroups: TP3DMeshMaterialGroupContainerList;
        WeightGroups: TP3DMeshWeightGroupContainerList;

        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

        destructor Destroy; override;

        class function GetContainer( ALib: TP3DLibrary): TP3DPropertyAccess; override;

        procedure SetJSON(AValue: TJSONData); override;

        //procedure LoadFromJSONContext( AContext: TP3DJSONContext ); override;

        //IP3DPlottableLayer -->
        function GetLayer( ASettings: TP3DPlotListSetting ): TP3DPlotCommandLayer;
        //<-- IP3DPlottableLayer

        //IP3DRenderable -->
        procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject );
        procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );

        procedure RenderSimple;
{        procedure RenderMaterialGroup( world: TMat4; Idx: Integer; Scene: TP3DScene );
        procedure RenderMaterialGroupInstanced( world: TMat4List; Idx: Integer; Scene: TP3DScene );
        procedure RenderMaterialGroupInstanced( world: TP3DTexture; Idx: Integer; Scene: TP3DScene ); //not implemented
        procedure RenderMaterialGroupInstanced( world: TMat4List; Idx: Integer );
        procedure RenderMaterialGroupInstanced( world: TP3DTexture; numInstance: Integer; Idx: Integer );
        procedure RenderMaterialGroupInstanced( numInstance: Integer; MatIdx: Integer );}
        procedure RenderInstanced( numInstance: Integer );
        procedure RenderEdgesInstanced( numInstance: Integer );
        procedure RenderEdges( world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject );
        procedure UnpackBuffers;
        procedure ClearVBO;
        procedure CreateVBO;
        procedure CalcCotangentsFromTangents;
        procedure Calc_Tangents_Cotangents;

        property VertexWeights: TP3DVec4BufferGL read VertexWeightsAuto.FInstance;
        property VertexWeightIndices: TP3DVec4BufferGL read VertexWeightIndicesAuto.FInstance;
        property Positions: TP3DVec3BufferGL read PositionsAuto.FInstance;
        property Normals: TP3DVec3BufferGL read NormalsAuto.FInstance;
        property Tangents: TP3DVec3BufferGL read TangentsAuto.FInstance;
        property Cotangents: TP3DVec3BufferGL read CotangentsAuto.FInstance;
        property Edges: TP3DIntBufferGL read EdgesAuto.FInstance;
        property Indices: TP3DIntBufferGL read IndicesAuto.FInstance;

        //        procedure ClearChildren;
        property Modifiers: TP3DMeshModifierContainerList read FModifiers write FModifiers;
        property Material: TP3DMaterial read GetMaterial write SetMaterial;

        //IP3DCollidable -->
      private
        FAABB: TP3DAABB;
        FSphere: TP3DSphere;

        function GetAABB(): TP3DAABB;
        function GetSphere: TP3DSphere;
        procedure SetAABB( AValue: TP3DAABB );
        procedure SetSphere( AValue: TP3DSphere );

      public
        function CalcAABB(): TP3DAABB;
        function CalcSphere: TP3DSphere;
        function CastRay( Orig, Dir: TVec3; out HitPos: TVec3 ): Boolean;
        function CastRay( Orig, Dir: TVec3; AObject: TP3DObject; out HitPos: TVec3 ): Boolean;

        function CollideObject( SelfMat: TMat4; ActMat: TMat4; AObject: TP3DObject; out Intersect: TVec3; out N: TVec3 ): Boolean;
        function CollideSphere( Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3 ): Boolean;
        function CollideAABB( Mat: TMat4; AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3 ): Boolean;
        function CollidePoint( Mat: TMat4; APoint: TVec3 ): Boolean;

        property AABB: TP3DAABB read GetAABB write SetAABB;
        property Sphere: TP3DSphere read GetSphere write SetSphere;
        //<-- IP3DCollidable
      end;

    {TP3DMeshContainer = specialize gP3DStreamableContainer < TP3DMesh >;
    TP3DMeshPointer = class( specialize gP3DStreamablePointer < TP3DMesh >);
    TP3DMeshPointerList = specialize gP3DStreamableList < TP3DMesh, TP3DMeshPointer >;}
    TP3DMeshContainerList = specialize gP3DNamedStreamableContainerList < TP3DMesh >;
//var
  //P3DMeshModifierClassFactory: TP3DMeshModifierClassFactory = nil;

function P3DMeshCreatePlane(ASize: TVec2; const AData: TP3DLibrary = nil ): TP3DMesh;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DMesh }

function TP3DMesh.GetMaterial: TP3DMaterial;
begin
  Result:= TP3DMaterial( FMaterial.AsValue );
end;

procedure TP3DMesh.SetMaterial(AValue: TP3DMaterial);
begin
  FMaterial.AsValue:= AValue;
end;

procedure TP3DMesh.BuildIndicesFromFaces;
var
  i, j, vbase: Integer;
begin
  Indices.Clear;
  vbase:= 0;
  for i:= 0 to Faces.Count - 1 do
    begin
      vbase:= Faces[ i ].X;
      for j:= 2 to Faces[ i ].Y - 1 do
        begin
          Indices.AddArray([ vbase,
                             vbase + j - 1,
                             vbase + j ]);
        end;
    end;
end;

class function TP3DMesh.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Meshes' );
end;

procedure TP3DMesh.SetJSON(AValue: TJSONData);
begin
  inherited SetJSON(AValue);
  UnpackBuffers;
end;

constructor TP3DMesh.Create(const AOwner: IP3DStreamableContainer);
var
  PropTexCoord: TP3DPropAccessVec2BufferGLList;
begin
  inherited Create( AOwner );

  PositionsAuto:= TP3DVec3BufferGL.Create;
  PackedPositions:= TVec3List.Create;
  NormalsAuto:= TP3DVec3BufferGL.Create;
  PackedNormals:= TVec3List.Create;
  CotangentsAuto:= TP3DVec3BufferGL.Create;
  PackedCotangents:= TVec3List.Create;
  TangentsAuto:= TP3DVec3BufferGL.Create;
  PackedTangents:= TVec3List.Create;
  IndicesAuto:= TP3DIntBufferGL.Create;
  EdgesAuto:= TP3DIntBufferGL.Create;
  Loops:= TIntList.Create;
  VertexWeightIndicesAuto:= TP3DVec4BufferGL.Create;
  PackedVertexWeightIndices:= TIVec4List.Create;
  VertexWeightsAuto:= TP3DVec4BufferGL.Create;
  PackedVertexWeights:= TVec3List.Create;
  VBArray:= TP3DVertexBufferArray.Create;
  Faces:= TP3DFaceList.Create;
  PackedFaces:= TIntList.Create;
  TexCoords:= TP3DTexCoordList.Create;

  {MaterialGroups:= TP3DMeshMaterialGroupContainerList.Create( 'MaterialGroups' );
  MaterialGroups.FParentMesh:= Self;
  PackedMaterialGroups:= TP3DMeshMaterialGroupContainerList.Create( 'PackedMaterialGroups', smText );
  PackedMaterialGroups.FParentMesh:= Self;}
  FMaterial:= TP3DStreamablePointer.Create( 'Material', TP3DMaterial, smText );
  Properties.Add( FMaterial );

  WeightGroups:= TP3DMeshWeightGroupContainerList.Create( 'WeightGroups', smExternalBinary );
  WeightGroups.FParentMesh:= Self;

  Modifiers:= TP3DMeshModifierContainerList.Create( 'Modifiers', smText );
  Modifiers.FParentMesh:= Self;

  Properties.Add( TP3DPropAccessIntList.CreateField( 'Loops', @Loops, smExternalBinary ));
  Properties.Add( TP3DPropAccessIntBufferGL.CreateField( 'Edges', @Edges, smExternalBinary ));
  Properties.Add( TP3DPropAccessVec3List.CreateField( 'PackedPositions', @PackedPositions, smExternalBinary ));
  Properties.Add( TP3DPropAccessVec3List.CreateField( 'PackedNormals', @PackedNormals, smExternalBinary ));
  Properties.Add( TP3DPropAccessVec3List.CreateField( 'PackedTangents', @PackedTangents, smExternalBinary ));
  Properties.Add( TP3DPropAccessVec3List.CreateField( 'PackedCotangents', @PackedCotangents, smExternalBinary ));
  Properties.Add( TP3DPropAccessIntList.CreateField( 'PackedFaces', @PackedFaces, smExternalBinary ));
  Properties.Add( TP3DPropAccessVec3List.CreateField( 'PackedVertexWeights', @PackedVertexWeights, smExternalBinary ));
  Properties.Add( TP3DPropAccessIVec4List.CreateField( 'PackedVertexWeightIndices', @PackedVertexWeightIndices, smExternalBinary ));
  PropTexCoord:= TP3DPropAccessVec2BufferGLList.CreateField( 'TexCoords', @TexCoords, smText );
  PropTexCoord.ChildrenStoreMode:= smExternalBinary;
  Properties.Add( PropTexCoord );
  Properties.Add( WeightGroups );
//  Properties.Add( PackedMaterialGroups );
  Properties.Add( Modifiers );

  Properties.Add( TP3DPropAccessIntBufferGL.CreateField( 'Indices', @Indices, smDoNotStore ));
//  Properties.Add( MaterialGroups );
  Properties.Add( TP3DPropAccessVec3BufferGL.CreateField( 'Positions', @Positions, smDoNotStore ));
  Properties.Add( TP3DPropAccessVec3BufferGL.CreateField( 'Normals', @Normals, smDoNotStore ));
  Properties.Add( TP3DPropAccessVec3BufferGL.CreateField( 'Tangents', @Tangents, smDoNotStore ));
  Properties.Add( TP3DPropAccessVec3BufferGL.CreateField( 'Cotangents', @Cotangents, smDoNotStore ));
  Properties.Add( TP3DPropAccessIVec2BufferGL.CreateField( 'Faces', @Faces, smDoNotStore ));
  Properties.Add( TP3DPropAccessVec3List.CreateField( 'VertexWeights', @VertexWeights, smDoNotStore ));
  Properties.Add( TP3DPropAccessIVec4BufferGL.CreateField( 'VertexWeightIndices', @VertexWeightIndices, smDoNotStore ));
end;

destructor TP3DMesh.Destroy;
begin
  PositionsAuto.I:= nil;
  NormalsAuto.I:= nil;
  TangentsAuto.I:= nil;
  CotangentsAuto.I:= nil;
  EdgesAuto.I:= nil;
  VertexWeightsAuto.I:= nil;
  VertexWeightIndicesAuto.I:= nil;
  FreeAndNil( PackedPositions );
  FreeAndNil( PackedNormals );
  FreeAndNil( PackedCotangents );
  FreeAndNil( PackedTangents );
  FreeAndNil( Loops );
  FreeAndNil( PackedVertexWeights );
  FreeAndNil( PackedVertexWeightIndices );
  FreeAndNil( Faces );
  FreeAndNil( PackedFaces );

  VBArray.Free;
  //FileWatch.Free;

  //Owner.Meshes.Remove( Self );

  inherited Destroy;
  TexCoords.Clear;
  FreeAndNil( TexCoords );
end;

{
procedure TP3DMesh.LoadFromJSONContext( AContext: TP3DJSONContext );
var
  Prop: Integer;
begin
  inherited LoadFromJSONContext( AContext );
  UnpackBuffers;
  //WriteLn( TexCoords.Count );
  //WriteLn( TexCoords[0].Count );
  {Prop:= Properties.FindByName( 'TexCoords' );
  WriteLn( '----------------- ');
  WriteLn();
  WriteLn();
  WriteLn( Properties[ Prop ].AsString );
  WriteLn();}
end; }

procedure TP3DMesh.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DObject);
var
  i: Integer;
begin
  {for i:= 0 to MaterialGroups.Count - 1 do
    RenderMaterialGroup( world, i, Scene );}
end;

procedure TP3DMesh.RenderSimple;
var
  i: Integer;
  decl: TP3DShaderDecl;
begin
  if ( Positions.Count > 0 ) then
    Positions.SetAttribArray( P3DAttribPosition );
  if ( Normals.Count > 0 ) then
    Normals.SetAttribArray( P3DAttribNormal );
  if ( Cotangents.Count > 0 ) then
    Cotangents.SetAttribArray( P3DAttribCotangent );
  if ( Tangents.Count > 0 ) then
    Tangents.SetAttribArray( P3DAttribTangent );
  for i:= 0 to TexCoords.Count - 1 do
    if ( P3DShaderActive.Attributes.FindByName( 'TexCoord' + IntToStr( i ), decl )) then
      decl.AsVec2List:= TexCoords[ i ].I;

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );

  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 )); cle( Self );
end;
{
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
  for i:= 0 to TexCoords.Count - 1 do
    if ( P3DShaderActive.Attributes.FindByName( 'TexCoord' + IntToStr( i ), decl )) then
      decl.AsVec2List:= TexCoords[ i ];
  decl:= Material.Shader.Attributes.Names[ 'fjointID' ];
  if ( Assigned( decl )) and ( VertexWeightIndices.Count > 0 ) then
    VertexWeightIndices.SetAttribArray( decl.Addr );
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
  for i:= 0 to TexCoords.Count - 1 do
    if ( P3DShaderActive.Attributes.FindByName( 'TexCoord' + IntToStr( i ), decl )) then
      decl.AsVec2List:= TexCoords[ i ];
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
  if ( not Assigned( P3DShaderActive )) then
    exit;
  if ( Positions.Count > 0 ) then
    Positions.SetAttribArray( P3DAttribPosition );
  if ( Normals.Count > 0 ) then
    Normals.SetAttribArray( P3DAttribNormal );
  if ( Cotangents.Count > 0 ) then
    Cotangents.SetAttribArray( P3DAttribCotangent );
  if ( Tangents.Count > 0 ) then
    Tangents.SetAttribArray( P3DAttribTangent );
  for i:= 0 to TexCoords.Count - 1 do
    begin
      decl:= P3DShaderActive.Attributes.GetNames( 'TexCoord' + IntToStr( i ));
      if ( Assigned( decl )) then
        decl.AsVec2List:= TexCoords[ i ];
    end;
  //  VBArray.Bind;

  decl:= P3DShaderActive.Attributes.Names[ 'fjointID' ];
  if ( Assigned( decl )) and ( VertexWeightIndices.Count > 0 ) then
    VertexWeightIndices.SetAttribArray( decl.Addr );
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
  for i:= 0 to TexCoords.Count - 1 do
    TexCoords[ i ].UnsetAttribArray();
end;
}
procedure TP3DMesh.RenderInstanced(numInstance: Integer);
var
  i: Integer;
  decl: TP3DShaderDecl;
  Modifier: TP3DMeshModifier;
  offset, count: Integer;
  mat: TMat4;
begin
  if ( not Assigned( P3DShaderActive )) then
    exit;
  if ( Positions.Count > 0 ) then
    Positions.SetAttribArray( P3DAttribPosition );
  if ( Normals.Count > 0 ) then
    Normals.SetAttribArray( P3DAttribNormal );
  if ( Cotangents.Count > 0 ) then
    Cotangents.SetAttribArray( P3DAttribCotangent );
  if ( Tangents.Count > 0 ) then
    Tangents.SetAttribArray( P3DAttribTangent );
  for i:= 0 to TexCoords.Count - 1 do
    begin
      decl:= P3DShaderActive.Attributes.GetNames( 'TexCoord' + IntToStr( i ));
      if ( Assigned( decl )) then
        decl.AsVec2List:= TexCoords[ i ].I;
    end;
  //  VBArray.Bind;

  decl:= P3DShaderActive.Attributes.Names[ 'fjointID' ];
  if ( Assigned( decl )) and ( VertexWeightIndices.Count > 0 ) then
    VertexWeightIndices.SetAttribArray( decl.Addr );
  decl:= P3DShaderActive.Attributes.Names[ 'jointWeight' ];
  if ( Assigned( decl )) and ( VertexWeights.Count > 0 ) then
    VertexWeights.SetAttribArray( decl.Addr );

  mat:= Mat4Identity; //only a workaround
  for Modifier in Modifiers do
    Modifier.PassToShader( P3DMaterialActive, mat );

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );

  //WriteLn( P3DShaderActive.DumpAttributes );
  glDrawElementsInstanced( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 ), numInstance ); cle( Self );
  //VBArray.Unbind;

  if ( Positions.Count > 0 ) then
    Positions.UnsetAttribArray();
  if ( Normals.Count > 0 ) then
    Normals.UnsetAttribArray();
  if ( Cotangents.Count > 0 ) then
    Cotangents.UnsetAttribArray();
  if ( Tangents.Count > 0 ) then
    Tangents.UnsetAttribArray();
  for i:= 0 to TexCoords.Count - 1 do
    TexCoords[ i ].I.UnsetAttribArray();
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
  for i:= 0 to TexCoords.Count -1 do
    if ( P3DShaderActive.Attributes.FindByName( 'TexCoord' + IntToStr( i ), decl )) then
      decl.AsVec2List:= TexCoords[ i ].I;
  //  VBArray.Bind;

  decl:= P3DShaderActive.Attributes.Names[ 'fjointID' ];
  if ( Assigned( decl )) and ( VertexWeightIndices.Count > 0 ) then
    VertexWeightIndices.SetAttribArray( decl.Addr );
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
  for i:= 0 to TexCoords.Count - 1 do
    TexCoords[ i ].I.UnsetAttribArray();
end;

procedure TP3DMesh.RenderEdges(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DObject);
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
  for i:= 0 to TexCoords.Count - 1 do
    if ( P3DShaderActive.Attributes.FindByName( 'TexCoord' + IntToStr( i ), decl )) then
      decl.AsVec2List:= TexCoords[ i ].I;
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


function TP3DMesh.CastRay(Orig, Dir: TVec3; AObject: TP3DObject; out HitPos: TVec3 ): Boolean;
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
  Result:= False;
  mat4inverse( AObject.Transform.Matrix, mInv );
  transOrig:= ( mInv * vec4( Orig, 1 )).xyz;
  transDir:= ( mInv * vec4( Dir, 0 )).xyz.Normalize;
  for i:= 0 to Indices.Count div 3 do
    if ( TestPoly( i )) then
      begin
        Result:= True;
        HitPos:= ( AObject.Transform.Matrix * vec4( HitPos, 1 )).xyz;
        break;
      end;
end;

function TP3DMesh.CollideObject(SelfMat: TMat4; ActMat: TMat4; AObject: TP3DObject; out Intersect: TVec3; out N: TVec3): Boolean;
begin

end;

function TP3DMesh.CollideSphere(Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3): Boolean;
begin

end;

function TP3DMesh.CollideAABB(Mat: TMat4; AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3): Boolean;
begin

end;

function TP3DMesh.CollidePoint(Mat: TMat4; APoint: TVec3): Boolean;
begin

end;

procedure TP3DMesh.UnpackBuffers;
var
  i: Integer;
  j: Integer;
  vbase: Integer;
  Idx: Integer;

  vidx, LastVertex: Integer;
  vindices: TIVec4;
  Weight: TVec3;

begin
  ClearVBO;

  Positions.Clear;
  Normals.Clear;
  Tangents.Clear;
  Cotangents.Clear;
  Edges.Clear;
  VertexWeights.Clear;
  VertexWeightIndices.Clear;
  Faces.Clear;
  //MaterialGroups.Clear;
  Indices.Clear;

  Faces.Count:= PackedFaces.Count;
  for i:= 0 to PackedFaces.Count - 1 do
    begin
      Idx:= Indices.Count;
      vbase:= Positions.Count;
      if ( i = PackedFaces.Count - 1 ) then
        LastVertex:= Loops.Count - PackedFaces[ i ] - 1
      else
        LastVertex:= PackedFaces[ i + 1 ] - PackedFaces[ i ] - 1;

      for j:= 0 to LastVertex do
        begin
          vidx:= PackedFaces[ i ] + j;
          Positions.Add( PackedPositions[ Loops[ vidx ]]);
          Normals.Add( PackedNormals[ vidx ]);
          Tangents.Add( PackedTangents[ vidx ]);
          Cotangents.Add( PackedCotangents[ vidx ]);
          if ( PackedVertexWeights.Count > 0 ) then
            begin
              Weight:= PackedVertexWeights[ Loops[ vidx ]];
              VertexWeights.Add( vec4( Weight, 1 - Weight.length_manh ));
            end;
          if ( PackedVertexWeightIndices.Count > 0 ) then
            begin
              vindices:= PackedVertexWeightIndices[ Loops[ vidx ]];
              VertexWeightIndices.Add( vec4( vindices[ 0 ], vindices[ 1 ], vindices[ 2 ], vindices[ 3 ]));
            end;
        end;
      for j:= 1 to LastVertex do
        begin
          Edges.AddArray([ vbase + j -1, vbase + j ]);
          if ( j > 1 ) then
            Indices.AddArray([ vbase,
                               vbase + j - 1,
                               vbase + j ]);
        end;
      if ( LastVertex > 0 ) then
        Edges.AddArray([ vbase + LastVertex, vbase ]);
      Faces[ i ]:= ivec2( Idx, Indices.Count - Idx );
    end;
  {for i:= 0 to PackedMaterialGroups.Count - 1 do
    begin
      MaterialGroups.Add( PackedMaterialGroups[ i ].PolyStart, PackedMaterialGroups[ i ].PolyEnd, PackedMaterialGroups[ i ].Material );
      MaterialGroups[ i ].FIndexStart:= Faces[ PackedMaterialGroups[ i ].PolyStart ].X;
      MaterialGroups[ i ].FIndexEnd:=   Faces[ PackedMaterialGroups[ i ].PolyEnd ].X
                                      + Faces[ PackedMaterialGroups[ i ].PolyEnd ].Y - 1;
    end;}

  CreateVBO;
  CalcAABB();
  CalcSphere();
end;

function TP3DMesh.GetLayer( ASettings: TP3DPlotListSetting ): TP3DPlotCommandLayer;
var
  i: Integer;
  newSettings: TP3DPlotListSetting;
begin
  newSettings:= settings([ attrib( P3DAttribNormal, Normals )]);
  if ( Tangents.Count > 0 ) then
    newSettings.Append( attrib( P3DAttribTangent, Tangents ));
  if ( Cotangents.Count > 0 ) then
    newSettings.Append( attrib( P3DAttribCotangent, Cotangents ));
  for i:= 0 to TexCoords.Count - 1 do
    newSettings.Append( texcoord( 'TexCoord' + i.ToString, TexCoords[ i ]));

  if ( VertexWeights.Count > 0 ) then
    newSettings.Append( attrib( 'jointWeight', VertexWeights ));
  if ( VertexWeightIndices.Count > 0 ) then
    newSettings.Append( attrib( 'fjointID', VertexWeightIndices ));

  if ( Assigned( Material )) then
    newSettings:= settings_join([ newSettings, Material.GetSettings ]);

  Result:= geom_polygon( Positions, Indices, settings_join([ newSettings, ASettings ]));
end;

procedure TP3DMesh.ClearVBO;
begin
  VBArray.Free;
  VBArray:= TP3DVertexBufferArray.Create;
end;

procedure TP3DMesh.CreateVBO;
var
  i: Integer;
  decl: TP3DShaderDecl;
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
  if ( VertexWeightIndices.Count > 0 ) then
    begin
      VertexWeightIndices.PushData;
      //VertexGroups.SetAttribArray( jointID );
    end;
  if ( VertexWeights.Count > 0 ) then
    begin
      VertexWeights.PushData;
      //VertexWeights.SetAttribArray(  );
    end;
  for i:= 0 to TexCoords.Count - 1 do
    if ( TexCoords[ i ].I.Count > 0 ) then
      TexCoords[ i ].I.PushData;

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

function TP3DMesh.CalcAABB(): TP3DAABB;
var
  p: TVec3;
begin
  FAABB.Min:= vec3( 0 );
  FAABB.Max:= vec3( 0 );
  FAABB.Position:= vec3( 0 );
  for p in Positions do
    begin
      FAABB.Min.X:= Min( p.X, AABB.Min.X );
      FAABB.Min.Y:= Min( p.Y, AABB.Min.Y );
      FAABB.Min.Z:= Min( p.Z, AABB.Min.Z );
      FAABB.Max.X:= Max( p.X, AABB.Max.X );
      FAABB.Max.Y:= Max( p.Y, AABB.Max.Y );
      FAABB.Max.Z:= Max( p.Z, AABB.Max.Z );
    end;
  Result:= FAABB;
end;

function TP3DMesh.CalcSphere: TP3DSphere;
var
  sqrrad: Float;
  p, ctr: TVec3;
begin
  sqrrad:= 0.0;
  ctr:= ( AABB.Min + AABB.Max ) / 2;
  for p in Positions do
    sqrrad:= max( sqrrad, ( p - ctr ).length_sq );
  FSphere:= P3DSphere( ctr, sqrt( sqrrad ));
  Result:= Sphere;
end;

function TP3DMesh.CastRay(Orig, Dir: TVec3; out HitPos: TVec3): Boolean;
begin
  Result:= False;
end;

function TP3DMesh.GetAABB(): TP3DAABB;
begin
  Result:= FAABB;
end;

procedure TP3DMesh.SetAABB(AValue: TP3DAABB);
begin
  FAABB:= AValue;
end;

function TP3DMesh.GetSphere: TP3DSphere;
begin
  Result:= FSphere;
end;

procedure TP3DMesh.SetSphere(AValue: TP3DSphere);
begin
  FSphere:= AValue;
end;

procedure TP3DMesh.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
var
  i: Integer;
  mat: TP3DMaterial;
begin
  if ( rlfMeshes in AFlags ) then
    begin
      if ( rlfMaterials in AFlags ) then
        {for i:= 0 to MaterialGroups.Count - 1 do
          begin
            mat:= MaterialGroups[ i ].Material;
            if ( not Assigned( mat )) then
              begin
                MaterialGroups[ i ].Material:= PackedMaterialGroups[ i ].Material;
                mat:= MaterialGroups[ i ].Material;
              end;
            AList.PushRenderObject( Self, AWorld, mat );
          end}
        AList.PushRenderObject( Self, AWorld, Material )
      else
        AList.PushRenderObject( Self, AWorld, P3DMaterialActive );
    end;
end;

{ TP3DMeshMaterialGroup }
{
procedure TP3DMeshMaterialGroup.SetMaterial( AValue: TP3DMaterial );
begin
  FMaterial.AsValue:= AValue;
end;

function TP3DMeshMaterialGroup.GetMaterial: TP3DMaterial;
begin
  Result:= TP3DMaterial( FMaterial.AsValue );
end;

procedure TP3DMeshMaterialGroup.SetPolyEnd(AValue: Integer);
begin
  if FPolyEnd=AValue then Exit;
  FPolyEnd:=AValue;
  //FIndexStart:= ParentMesh.Faces[ FPolyStart ].X;
  //FIndexEnd:= ParentMesh.Faces[ FPolyEnd ].X + ParentMesh.Faces[ FPolyEnd ].Y;
end;

procedure TP3DMeshMaterialGroup.SetPolyStart(AValue: Integer);
begin
  if FPolyStart=AValue then Exit;
  FPolyStart:=AValue;
end;

constructor TP3DMeshMaterialGroup.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'PolyStart', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'PolyEnd', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'IndexStart' ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'IndexEnd' ));
  FMaterial:= TP3DStreamablePointer.Create( 'Material', TP3DMaterial, smText );
  Properties.Add( FMaterial );
end;
}
{ TP3DMeshModifier }

procedure TP3DMeshModifier.SetMesh(AValue: TP3DMesh);
begin
  FMesh.AsValue:= AValue;
end;

function TP3DMeshModifier.GetMesh: TP3DMesh;
begin
  Result:= TP3DMesh( FMesh.AsValue );
end;

constructor TP3DMeshModifier.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create;
  FMesh:= TP3DStreamablePointer.Create( 'Mesh', TP3DMesh );
  Properties.Add( FMesh );
  Properties.Add( TP3DPropAccessString.CreateGetSet( 'Type', @GetType, nil, smDoNotStore ));
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

constructor TP3DMeshWeightGroup.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Index', smText ));
end;



{ TP3DMeshMaterialGroupList }
{
function TP3DMeshMaterialGroupContainerList.Find( Material: TP3DMaterial ): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( AsValues[ i ].Material = Material ) then
      begin
        Result:= i;
        break;
      end;
end;


procedure TP3DMeshMaterialGroupContainerList.Add(PolyStart: Integer; PolyEnd: Integer; Material: TP3DMaterial);
var
  Group: TP3DMeshMaterialGroup;
begin
  Group:= TP3DMeshMaterialGroup.Create( Self );
  Group.PolyStart:= PolyStart;
  Group.PolyEnd:= PolyEnd;
  Group.Material:= Material;
end;


procedure TP3DMeshMaterialGroupContainerList.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  inherited Notify( ASender, AItem, AAction );
  if (( AAction = actAdd ) and Assigned( AItem )) then
    TP3DMeshMaterialGroup( AItem ).ParentMesh:= ParentMesh;
end;
}
{ TP3DMeshModifierList }

procedure TP3DMeshModifierContainerList.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  inherited Notify( ASender, AItem, AAction );
  TP3DMeshModifier( AItem ).Mesh:= ParentMesh;
end;

{ TP3DMeshWeightGroupList }

procedure TP3DMeshWeightGroupContainerList.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  inherited Notify( ASender, AItem, AAction );
  TP3DMeshWeightGroup( AItem ).Mesh:= ParentMesh;
end;

function P3DMeshCreatePlane(ASize: TVec2; const AData: TP3DLibrary): TP3DMesh;
var
  hSize: TVec3;
  n: TVec3;
begin
  Result:= TP3DMesh.CreateBlock( AData );
  hSize:= vec3( ASize / 2, 0 );
  n:= vec3( -1, 0, 1 );
  Result.PackedPositions.AddArray([ vec3( -hSize.X, hSize.Y, 0 ), hSize, -hSize, vec3( hSize.X, -hSize.Y, 0 )]);
  Result.PackedFaces.Add( 0 );
  Result.Edges.AddArray([ 0, 1, 1, 2, 2, 3, 3, 0 ]);
  Result.Loops.AddArray([ 0, 1, 3, 2 ]);
  Result.PackedNormals.AddArray([ n.YYZ, n.YYZ, n.YYZ, n.YYZ ]);
  Result.PackedTangents.AddArray([ n.ZYY, n.ZYY, n.ZYY, n.ZYY ]);
  Result.PackedCotangents.AddArray([ n.YZY, n.YZY, n.YZY, n.YZY ]);
  Result.TexCoords.Add( TP3DVec2BufferGL.Create );
  Result.TexCoords[ 0 ].I.AddArray([ vec2( 0, 1 ), vec2( 1, 1 ), vec2( 1, 0 ), vec2( 0, 0 )]);
  Result.UnpackBuffers;
end;
{$ENDIF}

