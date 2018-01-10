//p3dmaterial.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
  type
     TP3DMaterial = class;

     { TP3DMaterialMap }

     TP3DMaterialMap = class ( TP3DStreamable )
       private
         FHasTransparency: Boolean;
         FMap: TP3DTexturePointer;
         FParentMaterial: TP3DMaterial;
         FTexChannel: Integer;

         function GetMap: TP3DTexture;
         function GetName: String;
         procedure SetMap( AValue: TP3DTexture );
         procedure SetTexChannel(AValue: Integer); virtual;

       public
         constructor Create( const AOwner: IP3DStreamableContainer = nil ); override;
         destructor Destroy; override;

       published
         property TexChannel: Integer read FTexChannel write SetTexChannel;
         property Map: TP3DTexture read GetMap write SetMap;
         property ParentMaterial: TP3DMaterial read FParentMaterial write FParentMaterial;
         property HasTransparency: Boolean read FHasTransparency;
         property Name: String read GetName;
     end;

     TP3DMaterialSubsetDict = specialize TFPGMap < String, TP3DShader >;

     TP3DPropAccessMaterialMap = specialize gP3DPropAccessStreamable < TP3DMaterialMap >;

     TP3DMaterialMapClass = class of TP3DMaterialMap;

     TP3DMaterialBase = class;
     TP3DMaterialModifierType = class of TP3DMaterialModifier;

     TP3DMapMode = ( mapMix, mapMultiply, mapAdd, mapSubtract );
     TP3DMapTransparencyMode = ( tmMultiply, tmStep, tmNone );

     { TP3DMaterialMapBase }

     TP3DMaterialMapBase = class ( TP3DMaterialMap )
       private
         FAlphaFactor: Single;
         FDiffuseFactor: Single;
         FDisplaceFactor: Single;
         FMode: TP3DMapMode;
         FNormalFactor: Single;
         FSpecularFactor: Single;
         FTransformDynamic: Boolean;
         FTransform: TP3DTransformContainer;

         function GetTransform: TP3DTransform;
         procedure SetAlphaFactor(AValue: Single);
         procedure SetDiffuseFactor(AValue: Single);
         procedure SetDisplaceFactor(AValue: Single);
         procedure SetMode(AValue: TP3DMapMode);
         procedure SetNormalFactor(AValue: Single);
         procedure SetSpecularFactor(AValue: Single);
         procedure SetTexChannel(AValue: Integer); override;
         procedure SetTransformDynamic( AValue: Boolean );

       public
         constructor Create( const AOwner: IP3DStreamableContainer = nil ); override;

       published
         property DiffuseFactor: Single read FDiffuseFactor write SetDiffuseFactor;
         property AlphaFactor: Single read FAlphaFactor write SetAlphaFactor;
         property NormalFactor: Single read FNormalFactor write SetNormalFactor;
         property SpecularFactor: Single read FSpecularFactor write SetSpecularFactor;
         property DisplaceFactor: Single read FDisplaceFactor write SetDisplaceFactor;
         property Mode: TP3DMapMode read FMode write SetMode;
         property TransformDynamic: Boolean read FTransformDynamic write SetTransformDynamic;

         property Transform: TP3DTransform read GetTransform;
     end;

     TP3DMaterialMapContainer = specialize gP3DStreamableContainer < TP3DMaterialMap >;
     TP3DMaterialMapPointer = specialize gP3DStreamablePointer < TP3DMaterialMap >;
     //TP3DMaterialMapContainerList = specialize gP3DStreamableContainerList < TP3DMaterialMap, TP3DMaterialMapContainer >;
     TP3DMaterialMapPointerList = specialize gP3DStreamableList < TP3DMaterialMap, TP3DMaterialMapPointer >;

     { TP3DMaterialMapList }

     TP3DMaterialMapContainerList = class( specialize gP3DStreamableContainerList < TP3DMaterialMap, TP3DMaterialMapContainer >)
       private
         FMapClass: TP3DMaterialMapClass;
         FParentMaterial: TP3DMaterial;

       public
         procedure LoadFromStream( S: TStream ); //gP3DListPropertyAccessObject needs this to compile
         procedure SaveToStream( S: TStream );
         procedure Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction); override;

         property MapClass: TP3DMaterialMapClass read FMapClass write FMapClass;
         property ParentMaterial: TP3DMaterial read FParentMaterial write FParentMaterial;
         //needs map class
     end;

     TP3DMaterial = class ( TP3DDataBlock )
       private
         FMaps: TP3DMaterialMapContainerList;
         FShader: TP3DShaderPointer;
         ShaderOwned: Boolean;

         function GetHasTransparency: Boolean; virtual;
         function GetShader: TP3DShader;
         procedure SetShader( AValue: TP3DShader );

       public
         class function GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess; override;

         constructor CreateBlock( const AData: TP3DLibrary = nil ); override;
         destructor Destroy; override;

         procedure PassToShader( const ActiveSubset: String = '' ); virtual;

       published
         property Maps: TP3DMaterialMapContainerList read FMaps write FMaps;
         property Shader: TP3DShader read GetShader write SetShader;
         property HasTransparency: Boolean read GetHasTransparency;
     end;

     TP3DMaterialContainer = specialize gP3DStreamableContainer < TP3DMaterial >;
     TP3DMaterialPointer = specialize gP3DStreamablePointer < TP3DMaterial >;
     TP3DMaterialContainerList = specialize gP3DStreamableContainerList < TP3DMaterial, TP3DMaterialContainer >;
     TP3DMaterialPointerList = specialize gP3DStreamableList < TP3DMaterial, TP3DMaterialPointer >;

     { TP3DMaterialShader }

     TP3DMaterialShader = class ( TP3DMaterial )
     end;

     { TP3DMaterialModifier }

     TP3DMaterialModifier = class( TP3DStreamable )
       public
         class function GetIndex: Integer; virtual; abstract;

         procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); virtual; abstract;
         procedure PassToShader( Material: TP3DMaterialBase ); virtual; abstract;
     end;

     TP3DMaterialModifierContainer = specialize gP3DStreamableContainer < TP3DMaterialModifier >;
     TP3DMaterialModifierPointer = specialize gP3DStreamablePointer < TP3DMaterialModifier >;
     //TP3DMaterialModifierContainerList = specialize gP3DStreamableContainerList < TP3DMaterialModifier, TP3DMaterialModifierContainer >;
     TP3DMaterialModifierPointerList = specialize gP3DStreamableList < TP3DMaterialModifier, TP3DMaterialModifierPointer >;

     { TP3DMaterialModifierList }

     TP3DMaterialModifierContainerList = class ( specialize gP3DStreamableContainerList < TP3DMaterialModifier, TP3DMaterialModifierContainer > )
       public
         procedure SortByIndex;
         procedure Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction); override;
         function Find( ModType: TP3DMaterialModifierType ): Integer; reintroduce;
     end;

     { TP3DMaterialModifierTexTransform }

     TP3DMaterialModifierTexTransform = class ( TP3DMaterialModifier )
       public
         class function GetIndex: Integer; override;
         procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
         procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierInit }

     TP3DMaterialModifierInit = class ( TP3DMaterialModifier )
       public
         class function GetIndex: Integer; override;
         procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
         procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierMaps }

     TP3DMaterialModifierMaps = class ( TP3DMaterialModifier )
       public
         class function GetIndex: Integer; override;
         procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
         procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierLightning }

     TP3DMaterialModifierLightning = class ( TP3DMaterialModifier )
       public
         class function GetIndex: Integer; override;
         procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
         procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierTransform }

     TP3DMaterialTransformType = ( mttNone, mttWorld, mttView, mttProj );
     TP3DMaterialTransformTypes = set of TP3DMaterialTransformType;
     TP3DMaterialModifierTransform = class ( TP3DMaterialModifier )
       public
         class function GetIndex: Integer; override;
         procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
         procedure PassToShader( Material: TP3DMaterialBase ); override;

       {property Positions: TP3DMaterialTransformTypes read FPositions write FPositions;
       property Normals: TP3DMaterialTransformTypes read FNormals write FNormals;
       property Tangents: TP3DMaterialTransformTypes read FTangents write FTangents;
       property Cotangents: TP3DMaterialTransformTypes read FCotangents write FCotangents;}
     end;


     { TP3DMaterialModifierCombine }

     TP3DMaterialModifierCombine = class ( TP3DMaterialModifier )
       public
         class function GetIndex: Integer; override;
         procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
         procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierArray }

     TP3DMaterialModifierArray = class ( TP3DMaterialModifier )
       private
         FTransforms: TVec3List;
         FTexture: TP3DTexture;

       public
         constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
         destructor Destroy; override;

         class function GetIndex: Integer; override;
         procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
         procedure PassToShader( Material: TP3DMaterialBase ); override;
         procedure PushTexture;

         property Transforms: TVec3List read FTransforms write FTransforms;
     end;

     TP3DObjectModifierContainerList = class;

     { TP3DMaterialBase }

     TP3DMaterialBase = class ( TP3DMaterial )
       protected
         FAlpha: Single;
         FAlphaThreshhold: Single;
         FDiff: TP3DColorRGB;
         FInstancing: Boolean;
         FIsUpdating: Boolean;
         FModifiers: TP3DMaterialModifierContainerList;
         FShaderSubsets: TP3DMaterialSubsetDict;
         FSpec: TP3DColorRGB;
         FSpec_Hardness: Single;
         FTransparencyMode: TP3DMapTransparencyMode;
         FUnlit: Boolean;
         FUseVertexColor: Boolean;
         FHasTransparency: Boolean;

         procedure SetAlpha(AValue: Single);
         procedure SetAlphaThreshhold(AValue: Single);
         procedure SetInstancing(AValue: Boolean);
         procedure SetSpec_Hardness(AValue: Single);
         procedure SetTransparencyMode(AValue: TP3DMapTransparencyMode);
         procedure SetUnlit( AValue: Boolean );
         procedure AddPasses(ShaderTree: TP3DShaderNodeTree; const AModifiers: TP3DObjectModifierContainerList = nil ); virtual;
         function GetHasTransparency: Boolean; override;
         procedure UpdateHasTransparency;

       public
         constructor CreateBlock(const AData: TP3DLibrary=nil); override;
         destructor Destroy; override;

         function BuildShaderForSubset( AModifiers: TP3DObjectModifierContainerList ): TP3DShader; virtual;
         procedure BuildShader;
         procedure PassToShader( const ActiveSubset: String = '' ); override;

         procedure BeginUpdate;
         procedure EndUpdate;
         procedure BuildShaderSubset( AModifiers: TP3DObjectModifierContainerList; Index: String );

         property Diff: TP3DColorRGB read FDiff write FDiff;
         property Spec: TP3DColorRGB read FSpec write FSpec;

       published
         property Spec_Hardness: Single read FSpec_Hardness write SetSpec_Hardness;
         property Alpha: Single read FAlpha write SetAlpha;
         property UseVertexColor: Boolean read FUseVertexColor write FUseVertexColor;
         property Unlit: Boolean read FUnlit write SetUnlit;
         property Modifiers: TP3DMaterialModifierContainerList read FModifiers write FModifiers;
         property Instancing: Boolean read FInstancing write SetInstancing;
         property TransparencyMode: TP3DMapTransparencyMode read FTransparencyMode write SetTransparencyMode;
         property AlphaThreshhold: Single read FAlphaThreshhold write SetAlphaThreshhold;
         property IsUpdating: Boolean read FIsUpdating;
         property ShaderSubsets: TP3DMaterialSubsetDict read FShaderSubsets write FShaderSubsets;
     end;

     TP3DMaterialBaseContainer = specialize gP3DStreamableContainer < TP3DMaterialBase >;
     TP3DMaterialBasePointer = specialize gP3DStreamablePointer < TP3DMaterialBase >;
     TP3DMaterialBaseContainerList = specialize gP3DNamedStreamableContainerList < TP3DMaterialBase, TP3DMaterialBaseContainer >;
     TP3DMaterialBasePointerList = specialize gP3DStreamableList < TP3DMaterialBase, TP3DMaterialBasePointer >;


  var
    P3DMaterialActive: TP3DMaterial = nil;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DMaterialMapShader }

procedure TP3DMaterialMap.SetMap(AValue: TP3DTexture);
begin
  FMap.AsValue:= AValue;
end;

procedure TP3DMaterialMap.SetTexChannel(AValue: Integer);
begin
  if FTexChannel=AValue then Exit;
  FTexChannel:=AValue;
end;

function TP3DMaterialMap.GetName: String;
begin
  if ( Assigned( Map )) then
    Result:= Map.Name
  else
    Result:= '<None>';
end;

function TP3DMaterialMap.GetMap: TP3DTexture;
begin
  Result:= FMap.AsValue;
end;

constructor TP3DMaterialMap.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FTexChannel:= 0;
  //Properties.Context:= AParentMaterial;
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'TexChannel', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Name', smText ));
  FMap:= TP3DTexturePointer.Create( 'Map', smText );
  Properties.Add( FMap );
end;

{
function TP3DMaterialMap.SaveToDOM(AParent: TDOMElement): TDOMElement;
var
  i: Integer;
begin
  Result:= AParent.OwnerDocument.CreateElement( 'map' );
  AParent.AppendChild( Result );
  for i:= 0 to Properties.Count - 1 do
    Result.SetAttribute( Properties[ i ].Name, Properties[ i ].AsString );
end;
}

destructor TP3DMaterialMap.Destroy;
begin
  Map:= nil;
  inherited Destroy;
end;

{ TP3DMaterialMapBase }

procedure TP3DMaterialMapBase.SetDiffuseFactor(AValue: Single);
begin
  if FDiffuseFactor= AValue then Exit;
  FDiffuseFactor:= AValue;
  if ( Assigned( ParentMaterial )) then
    TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

function TP3DMaterialMapBase.GetTransform: TP3DTransform;
begin
  Result:= FTransform.AsValue;
end;

procedure TP3DMaterialMapBase.SetAlphaFactor(AValue: Single);
begin
  if FAlphaFactor= AValue then Exit;
  FAlphaFactor:= AValue;
  FHasTransparency:= AlphaFactor > 0;
  if ( Assigned( ParentMaterial )) then
    TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetDisplaceFactor(AValue: Single);
begin
  if FDisplaceFactor= AValue then Exit;
  FDisplaceFactor:= AValue;
  if ( Assigned( ParentMaterial )) then
    TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetMode(AValue: TP3DMapMode);
begin
  if FMode= AValue then Exit;
  FMode:= AValue;
  if ( Assigned( ParentMaterial )) then
    TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetNormalFactor(AValue: Single);
begin
  if FNormalFactor= AValue then Exit;
  FNormalFactor:= AValue;
  if ( Assigned( ParentMaterial )) then
    TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetSpecularFactor(AValue: Single);
begin
  if FSpecularFactor= AValue then Exit;
  FSpecularFactor:= AValue;
  if ( Assigned( ParentMaterial )) then
    TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetTexChannel(AValue: Integer);
begin
  inherited SetTexChannel(AValue);
  if ( Assigned( ParentMaterial )) then
    TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetTransformDynamic( AValue: Boolean );
begin
  if ( FTransformDynamic = AValue ) then exit;
  FTransformDynamic:= AValue;
  if ( Assigned( ParentMaterial )) then
    TP3DMaterialBase( ParentMaterial ).BuildShader();
end;


constructor TP3DMaterialMapBase.Create( const AOwner: IP3DStreamableContainer );
begin
  inherited Create( AOwner );
  FDiffuseFactor:= 1.0;
  FNormalFactor:= 0.0;
  FSpecularFactor:= 0.0;
  FMode:= mapMix;
  FTransform:= TP3DTransformContainer.Create( 'Transform', smText );
  Properties.Add( FTransform );
  FTransform.AsValue:= TP3DTransform.Create( FTransform );
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'DiffuseFactor', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'NormalFactor', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'SpecularFactor', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'DisplaceFactor', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'AlphaFactor', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Mode', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'TransformDynamic', smText ));
end;


{ TMaterial }

procedure TP3DMaterialBase.SetUnlit(AValue: Boolean);
begin
  if FUnlit=AValue then Exit;
  FUnlit:=AValue;
  BuildShader();
end;

procedure TP3DMaterialBase.AddPasses(ShaderTree: TP3DShaderNodeTree; const AModifiers: TP3DObjectModifierContainerList);
var
  Modifier: TP3DMaterialModifier;
  OModifier: TP3DObjectModifier;
begin
  for Modifier in Modifiers do
    Modifier.AddPass( ShaderTree, Self );
  if ( Assigned( AModifiers )) then
    for OModifier in AModifiers do
      OModifier.AddPass( ShaderTree, Self );
  ShaderTree.Nodes.SortByName;
end;

function TP3DMaterialBase.GetHasTransparency: Boolean;
begin
  UpdateHasTransparency;
  Result:= FHasTransparency;
end;

procedure TP3DMaterialBase.UpdateHasTransparency;
var
  Map: TP3DMaterialMap;
begin
  FHasTransparency:= FAlpha < 1;

  if (( not FHasTransparency ) and ( TransparencyMode = tmMultiply )) then
    for Map in Maps do
      if ( Map.HasTransparency ) then
        begin
          FHasTransparency:= True;
          break;
        end;
end;

procedure TP3DMaterialBase.SetAlpha(AValue: Single);
begin
  if FAlpha= AValue then Exit;
  FAlpha:= clamp( AValue, 0, 1 );
  UpdateHasTransparency;
end;

procedure TP3DMaterialBase.SetAlphaThreshhold(AValue: Single);
begin
  if FAlphaThreshhold= AValue then Exit;
  FAlphaThreshhold:= AValue;
  BuildShader();
end;

procedure TP3DMaterialBase.SetInstancing(AValue: Boolean);
begin
  if FInstancing=AValue then Exit;
  FInstancing:=AValue;
  BuildShader();
end;

procedure TP3DMaterialBase.SetSpec_Hardness(AValue: Single);
begin
  if FSpec_Hardness= AValue then Exit;
  FSpec_Hardness:= clamp( AValue, 0, 511 );
end;

procedure TP3DMaterialBase.SetTransparencyMode(AValue: TP3DMapTransparencyMode);
begin
  if FTransparencyMode= AValue then Exit;
  FTransparencyMode:= AValue;

  UpdateHasTransparency;
  BuildShader();
end;

constructor TP3DMaterialBase.CreateBlock(const AData: TP3DLibrary);
begin
  inherited CreateBlock( AData );

  Maps.MapClass:= TP3DMaterialMapBase;

  Alpha:= 1.0;
  Diff:= vec3( 1 );
  FTransparencyMode:= tmMultiply;
  FAlphaThreshhold:= 0.9;
  FShaderSubsets:= TP3DMaterialSubsetDict.Create;

  Modifiers:= TP3DMaterialModifierContainerList.Create( 'Modifiers', smDoNotStore );
  Properties.Add( Modifiers );
  TP3DMaterialModifierInit.Create( Modifiers );
  TP3DMaterialModifierTexTransform.Create( Modifiers );
  TP3DMaterialModifierMaps.Create( Modifiers );
  TP3DMaterialModifierLightning.Create( Modifiers );
  TP3DMaterialModifierTransform.Create( Modifiers );
  TP3DMaterialModifierCombine.Create( Modifiers );

  Properties.Add( TP3DPropAccessColorRGB.CreateField( 'Diff', @FDiff, smText ));
  Properties.Add( TP3DPropAccessColorRGB.CreateField( 'Spec', @FSpec, smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Unlit', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Alpha', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'AlphaThreshhold', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'TransparencyMode', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Spec_Hardness', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Instancing', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'UseVertexColor', smText ));
end;

destructor TP3DMaterialBase.Destroy;
begin
  FShaderSubsets.Free;
  inherited Destroy;
end;

function TP3DMaterialBase.BuildShaderForSubset(AModifiers: TP3DObjectModifierContainerList): TP3DShader;
var
  ShaderTree, Passes: TP3DShaderNodeTree;
  Compiled: TP3DShaderCompiled;

  procedure CompileBuffers;
  var
    i, j, n: Integer;
    Buffer: TP3DShaderBuffer;
    extensions: String;
  begin
    Compiled:= TP3DShaderCompiled.Create;
    with( ShaderTree.AddCopyEx( 'GLSLShader' )) do
      for i:= 0 to Outputs.Count - 1 do
        begin
          Buffer:= TP3DShaderBuffer.Create;
          Buffer.Defines.Add( Outputs[ i ].Name );
          Buffer.Name:= Outputs[ i ].Name;
          TP3DShaderNodeSocketInt( Inputs[ Inputs.FindSocketByName( 'version' )]).Value:= StrToInt( P3DGLSLVersion );
          for j:= 0 to Inputs.Count -1 do
            if ( Inputs[ j ] is TP3DShaderNodeSocketShader ) then
              begin
                Buffer.Defines.Add( Inputs[ j ].Name );

                TP3DShaderNodeSocketShader( Inputs[ j ]).Value:= Passes.GetShaderOutputFromNodes( Buffer );
                if ( Buffer.Defines.Find( Inputs[ j ].Name, n )) then
                  Buffer.Defines.Delete( n );
              end;
          if ( Buffer.Defines.Find( Outputs[ i ].Name, n )) then
            Buffer.Defines.Delete( n );
          Buffer.Code:= TP3DShaderNodeSocket( Outputs[ i ]).GetOutput( Buffer );
          extensions:= '';
          for n:= 0 to Passes.Nodes.Count - 1 do
            extensions += Passes.Nodes[ n ].Name + LineEnding;
          P3DLog.LogInfoXML( Self, '<details><summary>Built ' + Buffer.Name +
            ' from chunks <br/></summary> <details><summary>Loaded extensions</summary><div class="whitebox">' +
            extensions + '</div></details><pre><code class="glsl">' + P3DTextToHTML( Buffer.Code ) +
            '</code></pre></details>' );
          Compiled.Buffers.Add( Buffer );
        end;
  end;

begin
  if ( IsUpdating ) then
    exit;

  ShaderTree:= TP3DShaderNodeTree.Create();
  Compiled:= nil;
  Result:= nil;

  try
    Passes:= TP3DShaderNodeTree.Create();
    AddPasses( Passes, AModifiers );

    CompileBuffers;

    //Compiled:= ShaderTree.Compile;

    Passes.Free;

    Result:= TP3DShader.CreateFromVertexAndFragmentShader( Compiled.FindBuffer( 'vshader' ).Code, Compiled.FindBuffer( 'fshader' ).Code, OwnerLib );
  finally
    ShaderTree.Free;
    if ( Assigned( Compiled )) then
      Compiled.Free;
  end;
end;

procedure TP3DMaterialBase.BuildShader;
begin
  if ( IsUpdating ) then
    exit;
  Shader:= BuildShaderForSubset( nil );
  ShaderOwned:= True;
end;

procedure TP3DMaterialBase.PassToShader(const ActiveSubset: String);
var
  i: Integer;
  Sh: TP3DShader;
begin
  if ( not Assigned( Shader )) then
    BuildShader();
  if ( Assigned( Shader )) then
    begin
      P3DMaterialActive:= Self;
      ShaderSubsets.TryGetData( ActiveSubset, Sh );
      if ( not Assigned( Sh )) then
        Sh:= Shader;
      Sh.Enable;
      if ( not UseVertexColor ) then
        begin
          glVertexAttrib4f( P3DAttribColor,
            Diff.R, Diff.G, Diff.B, Alpha ); cle( Self );
        end;

      glUniform4f( Sh.Uniforms.AddrByName( 'mat_specular' ), Spec.r, Spec.g, Spec.b, 1 ); cle( Self );
      glUniform1f( Sh.Uniforms.AddrByName( 'mat_hardness' ), Spec_Hardness ); cle( Self );

      for i:= 0 to Maps.Count - 1 do
        begin
          glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
          if ( Assigned( Maps[ i ].Map )) then
            glBindTexture( GL_TEXTURE_2D, Maps[ i ].Map.GLTexture ); cle( Self );
          glUniform1i( Sh.Uniforms.AddrByName( 'tex' + IntToStr( i )), i ); cle( Self );
        end;
      for i:= 0 to Modifiers.Count - 1 do
        Modifiers[ i ].PassToShader( Self );
    end;
end;

{
procedure TP3DMaterialBase.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  BeginUpdate;
  Maps.ParentMaterial:= Self;
  inherited LoadFromJSONContext(AContext);
  EndUpdate;
end;
}
procedure TP3DMaterialBase.BeginUpdate;
begin
  FIsUpdating:= True;
end;

procedure TP3DMaterialBase.EndUpdate;
begin
  FIsUpdating:= False;
  BuildShader();
end;

procedure TP3DMaterialBase.BuildShaderSubset(AModifiers: TP3DObjectModifierContainerList; Index: String);
var
  outsh: TP3DShader;
  sh: TComponentName;
begin
  outsh:= BuildShaderForSubset( AModifiers );
  ShaderSubsets[ Index ]:= outsh;
  sh:= 'Shader_' + Name;
  if ( Assigned( AModifiers ) and Assigned( AModifiers.ParentObject )) then
    sh:= sh + '_' + AModifiers.ParentObject.Name;
  outsh.Name:= sh;
end;

{ TP3DMaterialModifierTexTransform }

class function TP3DMaterialModifierTexTransform.GetIndex: Integer;
begin
  Result:= 50;
end;

procedure TP3DMaterialModifierTexTransform.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
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

  function ChangeSocketValueMat( Node: TP3DShaderNode; Name: String; Value: TMat4 ): TP3DShaderNode;
  var
    Socket: Integer;
  begin
    Socket:= Node.Inputs.FindSocketByName( Name );
    if ( Socket < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    TP3DShaderNodeSocketMatrix( Node.Inputs[ Socket ]).Value:= Value;
    Result:= Node;
  end;

var
  i: Integer;
begin
  with ( Material ) do
    for i:= 0 to Maps.Count - 1 do
      if ( Maps[ i ] is TP3DMaterialMapBase ) then
        with ( Maps[ i ] as TP3DMaterialMapBase ) do
          if ( TransformDynamic ) then
            ChangeSocketValueInt( ShaderTree.AddCopyEx( '00050_map_texcoord_transform_dynamic' ), 'MapIndex', i )
          else
            ChangeSocketValueMat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '00050_map_texcoord_transform_fixed' ), 'MapIndex', i ),
              'Transform', Transform.Matrix );
end;

procedure TP3DMaterialModifierTexTransform.PassToShader(Material: TP3DMaterialBase);
var
  i: Integer;
  decl: TP3DShaderDecl;
begin
  with ( Material ) do
    for i:= 0 to Maps.Count - 1 do
      if ( Maps[ i ] is TP3DMaterialMapBase ) then
        begin
          if ( P3DShaderActive.Uniforms.FindByName( 'TexMat' + IntToStr( i ), decl )) then
            begin
              decl.AsMat4:= TP3DMaterialMapBase( Maps.Items[ i ]).Transform.Matrix;
            end;
        end;

end;

{ TP3DMaterialModifierInit }

class function TP3DMaterialModifierInit.GetIndex: Integer;
begin
  Result:= 10;
end;

procedure TP3DMaterialModifierInit.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
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
var
  i: Integer;
  TexCoordsAdded: set of Byte;
begin
  ShaderTree.AddCopy( '00005_map_lib' );
  ShaderTree.AddCopy( '00005_attrib_lib' );
  ShaderTree.AddCopy( '00010_map_pass_init' );

  TexCoordsAdded:= [];
  with ( Material ) do
    for i:= 0 to Maps.Count - 1 do
      begin
        if ( not ( Maps[ i ].TexChannel in TexCoordsAdded )) then
          begin
            ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( '00010_map_texcoord_init' ), 'Location', i + 8 ), 'TexCoordIndex', i );
            TexCoordsAdded:= TexCoordsAdded + [ Maps[ i ].TexChannel ];
          end;
        ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( '00010_map_init' ), 'MapIndex', i ), 'TexCoordIndex', Maps[ i ].TexChannel );
      end;
end;

procedure TP3DMaterialModifierInit.PassToShader(Material: TP3DMaterialBase);
begin

end;



{ TP3DMaterialModifierMaps }

class function TP3DMaterialModifierMaps.GetIndex: Integer;
begin
  Result:= 100;
end;

procedure TP3DMaterialModifierMaps.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);

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

const
  Mode: array [ mapMix .. mapSubtract ] of String = ( 'mix', 'multiply', 'add', 'subtract' );

var
  i: Integer;

  HasNormalMaps: Boolean;
begin
  HasNormalMaps:= False;
  with ( Material ) do
    for i:= 0 to Maps.Count - 1 do
      begin
        ChangeSocketValueInt( ShaderTree.AddCopyEx( '00100_map_pass_read_frag' ), 'Index', i );
        ChangeSocketValueInt( ShaderTree.AddCopyEx( '00100_map_pass_read_vert' ), 'Index', i );
        if (( TP3DMaterialMapBase( Maps[ i ]).DiffuseFactor <> 0 ) or ( TP3DMaterialMapBase( Maps[ i ]).AlphaFactor <> 0 )) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '00105_map_pass_diffuse' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps.Items[ i ]).DiffuseFactor ),
                         'Alpha', TP3DMaterialMapBase( Maps.Items[ i ]).AlphaFactor ),
                         ShaderTree.AddCopyEx( '00100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).NormalFactor <> 0 ) then
          begin
            ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '00105_map_pass_normal' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps.Items[ i ]).NormalFactor ),
              ShaderTree.AddCopyEx( '00100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
            HasNormalMaps:= True;
          end;
        if ( TP3DMaterialMapBase( Maps[ i ]).SpecularFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '00105_map_pass_specular' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps.Items[ i ]).SpecularFactor ),
          ShaderTree.AddCopyEx( '00100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).DisplaceFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '00105_map_pass_displace' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps.Items[ i ]).DisplaceFactor ),
          ShaderTree.AddCopyEx( '00100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
      end;

  if ( HasNormalMaps ) then
    ShaderTree.AddCopy( '00200_transform_normals_view' )
  else
    ShaderTree.AddCopy( '00200_transform_normals_view_notangents' )
end;


procedure TP3DMaterialModifierMaps.PassToShader(Material: TP3DMaterialBase);
begin

end;

{ TP3DMaterialModifierTransform }

class function TP3DMaterialModifierTransform.GetIndex: Integer;
begin
  Result:= 5000;
end;

procedure TP3DMaterialModifierTransform.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
begin
  if ( Material.Instancing ) then
    begin
      ShaderTree.AddCopy( '00030_map_pass_world_transform_init_instanced' );
      ShaderTree.AddCopy( '00040_map_pass_world_transform_init_instanced_attrib' );
      ShaderTree.AddCopy( '00050_map_pass_world_transform_init_instanced_grid' );
    end;
  //else
  //  ShaderTree.AddCopy( '00030_map_pass_world_transform_init' );
  ShaderTree.AddCopy( '00030_map_pass_world_transform' );
  ShaderTree.AddCopy( '05000_map_pass_view_transform' );
end;

procedure TP3DMaterialModifierTransform.PassToShader(Material: TP3DMaterialBase);
begin

end;

{ TP3DMaterialModifierCombine }

class function TP3DMaterialModifierCombine.GetIndex: Integer;
begin
  Result:= 10000;
end;

procedure TP3DMaterialModifierCombine.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
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

begin
  if ( Material.TransparencyMode = tmStep ) then
    ChangeSocketValueFloat( ShaderTree.AddCopyEx( '09990_map_pass_clip_transparency' ), 'AlphaTreshhold', Material.AlphaThreshhold )
  else if ( Material.TransparencyMode = tmNone ) then
    ChangeSocketValueFloat( ShaderTree.AddCopyEx( '09990_map_pass_clip_transparency' ), 'AlphaTreshhold', 0 );
  ShaderTree.AddCopy( '10000_map_pass_final' );
end;

procedure TP3DMaterialModifierCombine.PassToShader(Material: TP3DMaterialBase);
begin

end;

{ TP3DMaterialModifierLightning }

class function TP3DMaterialModifierLightning.GetIndex: Integer;
begin
  Result:= 1000;
end;

procedure TP3DMaterialModifierLightning.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
begin
  if ( not Material.Unlit ) then
    begin
      ShaderTree.AddCopy( '01000_common_lib' );
      ShaderTree.AddCopy( '01000_light_lib' );
      ShaderTree.AddCopy( '01000_light_pass_calc_lighting' );
    end
  else
    ShaderTree.AddCopy( '01000_light_pass_unlit' );
end;

procedure TP3DMaterialModifierLightning.PassToShader(Material: TP3DMaterialBase);
begin

end;

{ TP3DMaterialModifierArray }

constructor TP3DMaterialModifierArray.Create(
  const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FTransforms:= TVec3List.Create;
  FTexture:= TP3DTexture.CreateFromEmpty( 1, 1, 3, dtFloat );
  FTexture.MipMap:= False;
end;

destructor TP3DMaterialModifierArray.Destroy;
begin
  Transforms.Free;
  if ( P3DDataBlockCache.IsValid( FTexture )) then
    FreeAndNil( FTexture );
  inherited Destroy;
end;

class function TP3DMaterialModifierArray.GetIndex: Integer;
begin
  Result:= 5;
end;

procedure TP3DMaterialModifierArray.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
begin
  ShaderTree.AddCopy( 'instance_init' );
  ShaderTree.AddCopy( 'instance_array_texture_translate' );
end;

procedure TP3DMaterialModifierArray.PassToShader(Material: TP3DMaterialBase);
var
  decl: TP3DShaderDecl;
begin
  Material.Shader.Uniforms.FindByName( 'instance_texture_translate_w', decl );
  if ( Assigned( decl )) then
    decl.AsInt:= FTexture.Width;
  Material.Shader.Uniforms.FindByName( 'instance_texture_translate', decl );
  if ( Assigned( decl )) then
    begin
      FTexture.Bind( Material.Maps.Count );
      decl.AsInt:= Material.Maps.Count;
    end;
end;

procedure TP3DMaterialModifierArray.PushTexture;
begin
  if ( P3DDataBlockCache.IsValid( FTexture )) then
    FreeAndNil( FTexture );
  FTexture:= TP3DTexture.CreateFromBuffer( Transforms.Count, 1, 3, dtFloat, Transforms.Ptr );
  FTexture.MipMap:= False;
end;


{ TP3DMaterialModifierContainerList }

procedure TP3DMaterialModifierContainerList.SortByIndex;

  procedure swap( a, b: Integer );
  var
    Element: TP3DMaterialModifierContainer;
  begin
    Element:= Self.FItems[ a ];
    FItems[ a ]:= Self.FItems[ b ];
    FItems[ b ]:= Element;
  end;

  function partition( lo, hi: Integer ): Integer;
  var
    pivot, i, j: Integer;
  begin
    pivot:= Self[ hi ].GetIndex;
    i:= lo;        // place for swapping
    for j := lo to hi - 1 do
      if ( Self[ j ].GetIndex <= pivot ) then
        begin
          swap( i, j );
          Inc( i );
        end;
    swap( i, hi );
    Result:= i;
  end;

  procedure quicksort( lo, hi: Integer );
  var
    p: Integer;
  begin
    if lo < hi then
      begin
        p:= partition( lo, hi );
        quicksort( lo, p - 1 );
        quicksort( p + 1, hi );
      end;
  end;

begin
  quicksort( 0, Count - 1 );
end;

procedure TP3DMaterialModifierContainerList.Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction);
begin
  inherited Notify( ASender, AItem, AAction );
  SortByIndex;
end;

function TP3DMaterialModifierContainerList.Find(ModType: TP3DMaterialModifierType): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( AsValues[ i ] is ModType ) then
      begin
        Result:= i;
        break;
      end;
end;

{ TP3DMaterial }

procedure TP3DMaterial.SetShader(AValue: TP3DShader);
begin
  FShader.AsValue:= AValue;
  ShaderOwned:= False;
end;

class function TP3DMaterial.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Materials' );
end;

function TP3DMaterial.GetHasTransparency: Boolean;
begin
  Result:= False;
end;

function TP3DMaterial.GetShader: TP3DShader;
begin
  Result:= FShader.AsValue;
end;

constructor TP3DMaterial.CreateBlock(const AData: TP3DLibrary);
begin
  inherited CreateBlock(AData);
  //Owner.Materials.Add( Self );

  Maps:= TP3DMaterialMapContainerList.Create( 'Maps', smText );
  Maps.ParentMaterial:= Self;
  Maps.MapClass:= TP3DMaterialMap;
  Properties.Add( Maps );
  FShader:= TP3DShaderPointer.Create( 'Shader' );
  Properties.Add( FShader );
end;

destructor TP3DMaterial.Destroy;
begin
  Maps.Free;

  //Owner.Materials.Remove( Self );

  inherited Destroy;
end;

procedure TP3DMaterial.PassToShader(const ActiveSubset: String);
var
  i: Integer;
begin
  if ( Assigned( Shader )) then
    begin
      Shader.Enable;

      for i:= 0 to Maps.Count - 1 do
        begin
          glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
          glBindTexture( GL_TEXTURE_2D, Maps[ i ].Map.GLTexture ); cle( Self );
          glUniform1i( Shader.Uniforms.AddrByName( 'tex' + IntToStr( i )), i ); cle( Self );
        end;
    end;
end;

{ TP3DMaterialMapContainerList }

procedure TP3DMaterialMapContainerList.LoadFromStream(S: TStream);
begin

end;

procedure TP3DMaterialMapContainerList.SaveToStream(S: TStream);
begin

end;

procedure TP3DMaterialMapContainerList.Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction);
begin
  inherited Notify( ASender, AItem, AAction );
  AItem.ParentMaterial:= ParentMaterial;
end;


{$ENDIF}

{$IFDEF FINALIZATION}
  P3DMaterialActive:= nil;
{$ENDIF}

