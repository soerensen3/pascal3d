{ p3d.core.material.deprecated.inc }

{$IFDEF INTERFACE}

TP3DMaterialOld = class;

TP3DMaterialMapOld = class ( TP3DNodeClone )
  protected
    FHasTransparency: Boolean;
    FMap: TP3DStreamablePointer;
    FParentMaterial: TP3DStreamablePointer;
    FTexChannel: Integer;

    function GetMap: TP3DTexture;
    function GetName: String;
    function GetParentMaterial: TP3DMaterialOld;
    procedure SetMap( AValue: TP3DTexture );
    procedure SetTexChannel(AValue: Integer); virtual;

  public
    constructor Create( const AOwner: IP3DStreamableContainer = nil ); override;
    destructor Destroy; override;

  published
    property TexChannel: Integer read FTexChannel write SetTexChannel;
    property Map: TP3DTexture read GetMap write SetMap;
    property ParentMaterial: TP3DMaterialOld read GetParentMaterial;
    property HasTransparency: Boolean read FHasTransparency;
    property Name: String read GetName;
end;

TP3DMaterialMapContainerListOld = class( specialize gP3DShaderNodeTree < TP3DMaterialMapOld >)
  private
    FMapClass: TP3DMaterialMapClass;
    FParentMaterial: TP3DMaterialOld;

  public
    procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;

    property ParentMaterial: TP3DMaterialOld read FParentMaterial write FParentMaterial;
    //needs map class
end;

TP3DMaterialOld = class ( TP3DDataBlock, IP3DAnimatable )
  private
    FMaps: TP3DMaterialMapContainerListOld;
    FShader: TP3DStreamableContainer;
    FAction: TP3DStreamablePointer;

    function GetAction: TP3DAction;

    function GetHasTransparency: Boolean; virtual;
    procedure SetAction(AValue: TP3DAction);
    function GetShader: TP3DShader; virtual;
    procedure SetShader( AValue: TP3DShader ); virtual;
    function FindMap( AMap: TP3DTexture ): Integer;

  public
    class function GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess; override;

    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
    destructor Destroy; override;

    procedure PassToActiveShader; virtual;
    procedure PassToShader( const ActiveSubset: String='' );
    procedure BuildShader( AOwner: TP3DStreamableContainer ); virtual; abstract; overload;
    procedure BuildShader; overload;

  published
    property Maps: TP3DMaterialMapContainerListOld read FMaps write FMaps;
    property Shader: TP3DShader read GetShader write SetShader;
    property HasTransparency: Boolean read GetHasTransparency;
    property Action: TP3DAction read GetAction write SetAction;
end;



TP3DMaterialBase = class;
TP3DMaterialModifierType = class of TP3DMaterialModifier;

TP3DMapMode = ( mapMix, mapMultiply, mapAdd, mapSubtract );
TP3DMapTransparencyMode = ( tmMultiply, tmStep, tmNone );

{ TP3DMaterialMapBase }

TP3DMaterialMapBase = class ( TP3DMaterialMapOld )
  private
    FAlphaFactor: Single;
    FDiffuseFactor: Single;
    FDisplaceFactor: Single;
    FMode: TP3DMapMode;
    FNormalFactor: Single;
    FSpecularFactor: Single;
    FHardnessFactor: Single;
    FTransformDynamic: Boolean;
    FTransform: TP3DStreamableContainer;

    function GetTransform: TP3DTransform;
    procedure SetAlphaFactor(AValue: Single);
    procedure SetDiffuseFactor(AValue: Single);
    procedure SetDisplaceFactor(AValue: Single);
    procedure SetMode(AValue: TP3DMapMode);
    procedure SetNormalFactor(AValue: Single);
    procedure SetSpecularFactor(AValue: Single);
    procedure SetHardnessFactor(AValue: Single);
    procedure SetTexChannel(AValue: Integer); override;
    procedure SetTransformDynamic( AValue: Boolean );

  public
    constructor Create( const AOwner: IP3DStreamableContainer = nil ); override;

  published
    property DiffuseFactor: Single read FDiffuseFactor write SetDiffuseFactor;
    property AlphaFactor: Single read FAlphaFactor write SetAlphaFactor;
    property NormalFactor: Single read FNormalFactor write SetNormalFactor;
    property SpecularFactor: Single read FSpecularFactor write SetSpecularFactor;
    property HardnessFactor: Single read FHardnessFactor write SetHardnessFactor;
    property DisplaceFactor: Single read FDisplaceFactor write SetDisplaceFactor;
    property Mode: TP3DMapMode read FMode write SetMode;
    property TransformDynamic: Boolean read FTransformDynamic write SetTransformDynamic;

    property Transform: TP3DTransform read GetTransform;
end;

    { TP3DMaterialNode }

     TP3DMaterialNode = class ( TP3DMaterialOld )
       protected type
         TOnPassUniform = procedure ( Material: TP3DMaterialNode; Uniform: TP3DNodeClone ) of object;

       protected
         FOnPassUniform: TOnPassUniform;
         FAutoRecompile: Boolean;
         FNodeMaster: TP3DNodeMaster;
         //FTexNodes: TP3DShaderNodeTree;
         FRootNode: TP3DStreamableContainer;
         FGeometryNode: TP3DStreamableContainer;
         FNodeTree: TP3DShaderNodeTree;
         FUniforms: TP3DShaderNodeTree;
         FLibraries: TP3DShaderNodeTree;

         function GetGeometryNode: TP3DNodeClone;
         procedure SetGeometryNode(AValue: TP3DNodeClone);
         function GetRootNode: TP3DNodeClone;
         procedure SetRootNode(AValue: TP3DNodeClone);
         procedure SetupRootNode;
         procedure MapsChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );
         procedure NodesChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );
         procedure NodeChange( ANewValue: TP3DStreamable );
         procedure UpdateTexNodes;
         function GetShader: TP3DShader; override;
         procedure SocketHandler( Sender: TP3DNodeSocketClone );

       public
         constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
         destructor Destroy; override;

         procedure MakeRootNode( AName: String );
         procedure MakeRootNode( ANode: TP3DNode );
         procedure BuildShader( AOwner: TP3DStreamableContainer ); override;
         procedure PassToActiveShader; override;
         function FindUniform( AName: String ): TP3DNodeClone;
         function AddMap( ATexture: TP3DTexture; ATexChannel: Integer ): TP3DMaterialMapOld;

         property RootNode: TP3DNodeClone read GetRootNode write SetRootNode;
         property GeometryNode: TP3DNodeClone read GetGeometryNode write SetGeometryNode;
         //property TexNodes: TP3DShaderNodeTree read FTexNodes;
         property NodeTree: TP3DShaderNodeTree read FNodeTree;
         property Uniforms: TP3DShaderNodeTree read FUniforms;
         property Libraries: TP3DShaderNodeTree read FLibraries;
         property NodeMaster: TP3DNodeMaster read FNodeMaster;
         property AutoRecompile: Boolean read FAutoRecompile write FAutoRecompile;
         property OnPassUniform: TOnPassUniform read FOnPassUniform write FOnPassUniform;
     end;

{ TP3DMaterialShader }

TP3DMaterialShader = class ( TP3DMaterialOld )
end;

{ TP3DMaterialModifier }

TP3DMaterialModifier = class( TP3DStreamable )
 public
   class function GetIndex: Integer; virtual; abstract;

   procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); virtual; abstract;
   procedure PassToShader( Material: TP3DMaterialBase ); virtual; abstract;
end;

{TP3DMaterialModifierContainer = specialize gP3DStreamableContainer < TP3DMaterialModifier >;
TP3DMaterialModifierPointer = specialize gP3DStreamablePointer < TP3DMaterialModifier >;
//TP3DMaterialModifierContainerList = specialize gP3DStreamableContainerList < TP3DMaterialModifier, TP3DMaterialModifierContainer >;
TP3DMaterialModifierPointerList = specialize gP3DStreamableList < TP3DMaterialModifier, TP3DMaterialModifierPointer >;
}
{ TP3DMaterialModifierList }

TP3DMaterialModifierContainerList = class ( specialize gP3DStreamableContainerList < TP3DMaterialModifier >)
 public
   procedure SortByIndex;
   procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;
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

TP3DMaterialBase = class ( TP3DMaterialOld )
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
   function GetHasTransparency: Boolean; override;
   procedure UpdateHasTransparency;
   procedure SetJSON(AValue: TJSONData); override;
   procedure SortPassesByName( ShaderNodeTree: TP3DShaderNodeTree );

 public
   constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
   destructor Destroy; override;

   procedure AddPasses(ShaderTree: TP3DShaderNodeTree; const AModifiers: TP3DObjectModifierContainerList = nil ); virtual;
   procedure BuildShaderForSubset( AModifiers: TP3DObjectModifierContainerList; AOwner: IP3DStreamableContainer ); virtual;
   procedure BuildShader;
   procedure PassToActiveShader; override;

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


{TP3DMaterialBaseContainer = specialize gP3DStreamableContainer < TP3DMaterialBase >;
TP3DMaterialBasePointer = specialize gP3DStreamablePointer < TP3DMaterialBase >;
TP3DMaterialBaseContainerList = specialize gP3DNamedStreamableContainerList < TP3DMaterialBase, TP3DMaterialBaseContainer >;
TP3DMaterialBasePointerList = specialize gP3DStreamableList < TP3DMaterialBase, TP3DMaterialBasePointer >;
}

{$ENDIF}

{$IFDEF IMPLEMENTATION}

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
  Result:= TP3DTransform( FTransform.AsValue );
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

procedure TP3DMaterialMapBase.SetHardnessFactor(AValue: Single);
begin
  if FHardnessFactor= AValue then Exit;
  FHardnessFactor:= AValue;
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
  FTransform:= TP3DStreamableContainer.Create( 'Transform', TP3DTransform, smText );
  Properties.Add( FTransform );
  FTransform.AsValue:= TP3DTransform.Create( FTransform );
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'DiffuseFactor', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'NormalFactor', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'SpecularFactor', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'HardnessFactor', smText ));
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
  Modifier: TP3DStreamable;
begin
  for Modifier in Modifiers do
    TP3DMaterialModifier( Modifier ).AddPass( ShaderTree, Self );
  if ( Assigned( AModifiers )) then
    for Modifier in AModifiers do
      TP3DObjectModifier( Modifier ).AddPass( ShaderTree, Self );
  ShaderTree.SortByName;
end;

function TP3DMaterialBase.GetHasTransparency: Boolean;
begin
  UpdateHasTransparency;
  Result:= FHasTransparency;
end;

procedure TP3DMaterialBase.UpdateHasTransparency;
var
  Map: TP3DStreamable;
begin
  FHasTransparency:= FAlpha < 1;

  if (( not FHasTransparency ) and ( TransparencyMode = tmMultiply )) then
    for Map in Maps do
      if ( TP3DMaterialMapOld( Map ).HasTransparency ) then
        begin
          FHasTransparency:= True;
          break;
        end;
end;

procedure TP3DMaterialBase.SetJSON(AValue: TJSONData);
begin
  BeginUpdate;
  inherited SetJSON(AValue);
  EndUpdate;
end;

procedure TP3DMaterialBase.SortPassesByName(ShaderNodeTree: TP3DShaderNodeTree);
begin
  ShaderNodeTree.Compile;
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

constructor TP3DMaterialBase.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  Maps.FItemClass:= TP3DMaterialMapBase;

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

procedure TP3DMaterialBase.BuildShaderForSubset(AModifiers: TP3DObjectModifierContainerList;
  AOwner: IP3DStreamableContainer);
var
  ShaderTree, Passes: TP3DShaderNodeTree;
  Compiled: TP3DShaderCompiled;

  procedure CompileBuffers;
  var
    i, j, n: Integer;
    Buffer: TP3DShaderBuffer;
    extensions, s: String;
  begin
    Compiled:= TP3DShaderCompiled.Create;
    with( ShaderTree.AddCopy( 'GLSLShader' )) do
      for i:= 0 to Outputs.Count - 1 do
        begin
          Buffer:= TP3DShaderBuffer.Create;
          Buffer.Defines.Add( Outputs[ i ].CloneOf.Name );
          Buffer.Name:= Outputs[ i ].CloneOf.Name;
          FindInput( 'version' ).ValueProp.AsString:= P3DGLSLVersion;
          for j:= 0 to Inputs.Count -1 do
            if ( Inputs[ j ].CloneOf.SocketType = 'text' ) then
              begin
                Buffer.Defines.Add( Inputs[ j ].CloneOf.Name );

                s:= Passes.GetShaderOutputFromNodes( Buffer );
                Inputs[ j ].ValueProp.AsString:= s;
                if ( Buffer.Defines.Find( Inputs[ j ].CloneOf.Name, n )) then
                  Buffer.Defines.Delete( n );
              end;
          if ( Buffer.Defines.Find( Outputs[ i ].CloneOf.Name, n )) then
            Buffer.Defines.Delete( n );
          Buffer.Code:= TP3DShaderNodeSocket(( Outputs[ i ] as TP3DNodeSocketClone ).CloneOf ).GetOutput( Buffer, Outputs[ i ] as TP3DNodeSocketClone );
          extensions:= '';
          for n:= 0 to Passes.Count - 1 do
            extensions += Passes[ n ].CloneOf.Name + LineEnding;
          log_info( message_header( Self ) + '<details><summary>Built ' + Buffer.Name +
            ' from chunks <br/></summary> <details><summary>Loaded extensions</summary><div class="whitebox">' +
            extensions + '</div></details><pre><code class="glsl">' + P3DTextToHTML( Buffer.Code ) +
            '</code></pre></details>' );
          Compiled.Buffers.Add( Buffer );
        end;
  end;

begin
  if ( IsUpdating ) then
    exit;

  ShaderTree:= TP3DShaderNodeTree.Create( 'ShaderTree' );
  ShaderTree.Root:= Root;
  Compiled:= nil;

  try
    Passes:= TP3DShaderNodeTree.Create( 'Passes' );
    Passes.Root:= Root;
    AddPasses( Passes, AModifiers );

    CompileBuffers;

    //Compiled:= ShaderTree.Compile;

    TP3DShader.CreateFromVertexAndFragmentShader( Compiled.FindBuffer( 'vshader' ).Code, Compiled.FindBuffer( 'fshader' ).Code, AOwner );
  finally
    if ( Assigned( ShaderTree )) then
      ShaderTree.Root:= nil;
    ShaderTree.Free;
    if ( Assigned( Passes )) then
      Passes.Root:= nil;
    Passes.Free;
    if ( Assigned( Compiled )) then
      Compiled.Free;
  end;
end;

procedure TP3DMaterialBase.BuildShader;
begin
  if ( IsUpdating ) then
    exit;
  BuildShaderForSubset( nil, FShader );
end;

procedure TP3DMaterialBase.PassToActiveShader;
var
  i: Integer;
  Sh: TP3DShader;
begin
  {
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
          if ( Assigned( TP3DMaterialMapOld( Maps[ i ]).Map )) then
            Maps[ i ].Map.Bind( i );
            //glBindTexture( Maps[ i ].Map.Target, Maps[ i ].Map.GLTexture ); cle( Self );
          glUniform1i( Sh.Uniforms.AddrByName( 'tex' + IntToStr( i )), i ); cle( Self );
        end;
      for i:= 0 to Modifiers.Count - 1 do
        TP3DMaterialModifier( Modifiers[ i ]).PassToShader( Self );
    end;}
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
  {outsh:= BuildShaderForSubset( AModifiers );
  ShaderSubsets[ Index ]:= outsh;
  sh:= 'Shader_' + Name;
  if ( Assigned( AModifiers ) and Assigned( AModifiers.ParentObject )) then
    sh:= sh + '_' + AModifiers.ParentObject.Name;
  //outsh.Name:= sh;}
end;

{ TP3DMaterialModifierTexTransform }

class function TP3DMaterialModifierTexTransform.GetIndex: Integer;
begin
  Result:= 50;
end;

procedure TP3DMaterialModifierTexTransform.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
  function ChangeSocketValueInt( Node: TP3DNodeClone; Name: String; Value: Integer ): TP3DNodeClone;
  var
    Socket: TP3DNodeSocketClone;
  begin
    Socket:= Node.FindInput( Name );
    if ( not Assigned( Socket )) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    Socket.ValueProp.AsString:= IntToStr( Value );
    Result:= Node;
  end;

  function ChangeSocketValueMat( Node: TP3DNodeClone; Name: String; Value: TMat4 ): TP3DNodeClone;
  var
    Socket: TP3DNodeSocketClone;
  begin
    Socket:= Node.FindInput( Name );
    if ( not Assigned( Socket )) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    Socket.ValueProp.AsString:= Value.ToString();
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
            ChangeSocketValueInt( ShaderTree.AddCopy( '00050_map_texcoord_transform_dynamic' ), 'MapIndex', i )
          else
            ChangeSocketValueMat( ChangeSocketValueInt( ShaderTree.AddCopy( '00050_map_texcoord_transform_fixed' ), 'MapIndex', i ),
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
              decl.AsMat4:= TP3DMaterialMapBase( Maps[ i ]).Transform.Matrix;
            end;
        end;

end;

{ TP3DMaterialModifierInit }

class function TP3DMaterialModifierInit.GetIndex: Integer;
begin
  Result:= 10;
end;

procedure TP3DMaterialModifierInit.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
  function ChangeSocketValueInt( Node: TP3DNodeClone; Name: String; Value: Integer ): TP3DNodeClone;
  var
    Socket: TP3DNodeSocketClone;
  begin
    Socket:= Node.FindInput( Name );
    if ( not Assigned( Socket )) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    Socket.ValueProp.AsString:= IntToStr( Value );
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
      if ( Assigned( Maps[ i ])) then
        begin
          if ( not ( TP3DMaterialMapOld( Maps[ i ]).TexChannel in TexCoordsAdded )) then
            begin
              ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopy( '00010_map_texcoord_init' ), 'Location', i + 8 ), 'TexCoordIndex', i );
              TexCoordsAdded:= TexCoordsAdded + [ TP3DMaterialMapOld( Maps[ i ]).TexChannel ];
            end;
          ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopy( '00010_map_init' ), 'MapIndex', i ), 'TexCoordIndex', TP3DMaterialMapOld( Maps[ i ]).TexChannel );
        end
      //else
      //  raise Exception.Create( Material.Name + '.Maps[' + IntToStr( i ) + '] is not assigned!' );
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

  function ChangeSocketValueInt( Node: TP3DNodeClone; Name: String; Value: Integer ): TP3DNodeClone;
  var
    Socket: TP3DNodeSocketClone;
  begin
    Socket:= Node.FindInput( Name );
    if ( not Assigned( Socket )) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    Socket.ValueProp.AsString:= IntToStr( Value );
    Result:= Node;
  end;

  function ChangeSocketValueFloat( Node: TP3DNodeClone; Name: String; Value: Float ): TP3DNodeClone;
  var
    Socket: TP3DNodeSocketClone;
  begin
    Socket:= Node.FindInput( Name );
    if ( not Assigned( Socket )) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    Socket.ValueProp.AsString:= FloatToStr( Value );
    Result:= Node;
  end;

  function ConnectSocket( Node1: TP3DNodeClone; Node2: TP3DNodeClone; Socket1: String; Socket2: String ): TP3DNodeClone;
  var
    Socket1n, Socket2n: TP3DNodeSocketClone;
  begin
    Socket1n:= Node1.FindInput( Socket1 );
    Socket2n:= Node2.FindOutput( Socket2 );
    if ( not Assigned( Socket1n )) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Socket1 + '" not found!' );
    if ( not Assigned( Socket2n )) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Socket2 + '" not found!' );
    Socket1n.Connected:= Socket2n;
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
      if ( Assigned( Maps[ i ])) then
      begin
        ChangeSocketValueInt( ShaderTree.AddCopy( '00100_map_pass_read_frag' ), 'Index', i );
        ChangeSocketValueInt( ShaderTree.AddCopy( '00100_map_pass_read_vert' ), 'Index', i );
        if (( TP3DMaterialMapBase( Maps[ i ]).DiffuseFactor <> 0 ) or ( TP3DMaterialMapBase( Maps[ i ]).AlphaFactor <> 0 )) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopy( '00105_map_pass_diffuse' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).DiffuseFactor ),
                         'Alpha', TP3DMaterialMapBase( Maps[ i ]).AlphaFactor ),
                         ShaderTree.AddCopy( '00100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).NormalFactor <> 0 ) then
          begin
            ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopy( '00105_map_pass_normal' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).NormalFactor ),
              ShaderTree.AddCopy( '00100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
            HasNormalMaps:= True;
          end;
        if ( TP3DMaterialMapBase( Maps[ i ]).SpecularFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopy( '00105_map_pass_specular' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).SpecularFactor ),
          ShaderTree.AddCopy( '00100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).DisplaceFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopy( '00105_map_pass_displace' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).DisplaceFactor ),
          ShaderTree.AddCopy( '00100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
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
  //ShaderTree.AddCopy( '00030_map_pass_world_transform' );
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
  function ChangeSocketValueFloat( Node: TP3DNodeClone; Name: String; Value: Float ): TP3DNodeClone;
  var
    Socket: TP3DNodeSocketClone;
  begin
    Socket:= Node.FindInput( Name );
    if ( not Assigned( Socket )) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    Socket.ValueProp.AsString:= FloatToStr( Value );
    Result:= Node;
  end;

begin
  if ( Material.TransparencyMode = tmStep ) then
    ChangeSocketValueFloat( ShaderTree.AddCopy( '09990_map_pass_clip_transparency' ), 'AlphaTreshhold', Material.AlphaThreshhold )
  else if ( Material.TransparencyMode = tmNone ) then
    ChangeSocketValueFloat( ShaderTree.AddCopy( '09990_map_pass_clip_transparency' ), 'AlphaTreshhold', 0 );
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
function MaterialModifierCompare(Item1, Item2: Pointer): Integer;
var
  mod1, mod2: TP3DMaterialModifier;
begin
  mod1:= TP3DMaterialModifier( TP3DStreamableContainer( Item1 ).AsValue );
  mod2:= TP3DMaterialModifier( TP3DStreamableContainer( Item2 ).AsValue );
  Result:= CompareValue( mod1.GetIndex, mod2.GetIndex );
end;

procedure TP3DMaterialModifierContainerList.SortByIndex;

  procedure swap( a, b: Integer );
  var
    Element: TP3DStreamableContainer;
  begin
    Element:= TP3DStreamableContainer( Self.FItems[ a ] );
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
  //quicksort( 0, Count - 1 );
  FItems.Sort( @MaterialModifierCompare );
end;

procedure TP3DMaterialModifierContainerList.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
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

{ TP3DMaterialNode }

function TP3DMaterialNode.GetGeometryNode: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FGeometryNode.AsValue );
end;

procedure TP3DMaterialNode.SetGeometryNode(AValue: TP3DNodeClone);
begin
  if ( Assigned( GeometryNode )) then begin
    if ( Assigned( NodeMaster )) then
      NodeMaster.RemoveNode( GeometryNode );
    GeometryNode.Free;
  end;
  FGeometryNode.AsValue:= AValue;
  FNeedsUpdate:= True;
  if ( Assigned( GeometryNode ) and Assigned( NodeMaster )) then begin
    GeometryNode.NodeMaster:= NodeMaster;
    NodeMaster.AddNode( GeometryNode );
  end;
end;

function TP3DMaterialNode.GetRootNode: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FRootNode.AsValue );
end;

procedure TP3DMaterialNode.SetRootNode(AValue: TP3DNodeClone);
begin
  if ( Assigned( RootNode )) then begin
    if ( Assigned( NodeMaster )) then
      NodeMaster.RemoveNode( RootNode );
    RootNode.Free;
  end;
  FRootNode.AsValue:= AValue;
  FNeedsUpdate:= True;
  if ( Assigned( RootNode ) and Assigned( NodeMaster )) then begin
    RootNode.NodeMaster:= NodeMaster;
    NodeMaster.AddNode( RootNode );
  end;
end;

procedure TP3DMaterialNode.SetupRootNode;
var
  p: TP3DNodeSocketClone;
begin
  if ( Assigned( RootNode )) then
    begin
      RootNode.NodeMaster:= NodeMaster;
      if ( not Assigned( GeometryNode )) then
        P3DShaderNodeLib.FindNode( 'geometry' ).Clone( FGeometryNode );
      GeometryNode.NodeMaster:= NodeMaster;
      p:= RootNode.FindInput( 'VertexPosition' );
      if ( Assigned( p )) then
        p.Connected:= GeometryNode.FindOutput( 'Attribute' );
      p:= RootNode.FindInput( 'Color' );
      if ( Assigned( p )) then
        p.Connected:= GeometryNode.FindOutput( 'Color' );
    end;
end;

procedure TP3DMaterialNode.MapsChange(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
var
  Nd: TP3DNode;
begin
  FNeedsUpdate:= True;
  UpdateTexNodes;
  //TP3DMaterialMapOld( AItem ).;

  {if ( Assigned( AItem )) then
    if ( AAction = actAdd ) then
      begin
        Nd:= P3DData.P3DShaderNodeLib.FindNode( 'texture2D' );
        Result:= Nd.Clone( TexNodes );
        Result.FindInput( 'texture2D' ).ValueProp.AsString:= IntToStr( Index );
        if ( not Assigned( Nd )) then
          exit;
        NdC:= Nd.Clone( TexNodes );
        NdC.NodeMaster:= NodeMaster
        if ( Assigned( NodeMaster )) then
          NodeMaster.AddNode( NdC );
      end
    else
      begin
        TP3DNodeClone( AItem ).NodeMaster:= nil;
        if ( Assigned( NodeMaster )) then
          NodeMaster.RemoveNode( TP3DNodeClone( AItem ))
      end;}
  {UpdateTexNodes;

  if ( AAction = actAdd ) then
    TP3DNodeClone( AItem ).NodeMaster:= NodeMaster
  else
    TP3DNodeClone( AItem ).NodeMaster:= nil;

  if ( Assigned( NodeMaster )) then
    case AAction of
      actAdd: NodeMaster.AddNode( TP3DNodeClone( AItem ));
      actDelete, actExtract, actClear: NodeMaster.RemoveNode( TP3DNodeClone( AItem ));
    end;}

end;

procedure TP3DMaterialNode.NodesChange(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  FNeedsUpdate:= True;

  {if ( AAction = actAdd ) then
    TP3DNodeClone( AItem ).NodeMaster:= NodeMaster
  else if ( Assigned( AItem )) then
    TP3DNodeClone( AItem ).NodeMaster:= nil;}

  if ( Assigned( NodeMaster )) then
    case AAction of
      actAdd: NodeMaster.AddNode( TP3DNodeClone( AItem ));
      actDelete, actExtract, actClear: NodeMaster.RemoveNode( TP3DNodeClone( AItem ));
    end;
end;

procedure TP3DMaterialNode.NodeChange(ANewValue: TP3DStreamable);
begin
  FNeedsUpdate:= True;
  if ( ANewValue is TP3DNodeClone ) then
    TP3DNodeClone( ANewValue ).NodeMaster:= NodeMaster;
end;

procedure TP3DMaterialNode.UpdateTexNodes;
var
  i: Integer;
  Nd: TP3DNode;
  TexNd: TP3DNodeClone;
  s: TP3DNodeSocketClone;

  {function AddTexNode( Index: Integer ): TP3DNodeClone;
  begin
    //if ( TexNodes.Count <= Index ) then
      begin
        Result:= Nd.Clone( TexNodes );
        Result.FindInput( 'texture2D' ).ValueProp.AsString:= IntToStr( Index );
        Result.NodeMaster:= NodeMaster;
        if ( Assigned( NodeMaster )) then
          NodeMaster.AddNode( Result );
        //TexNodes.Add( Result )
      end
    {else
      begin
        Result:= TexNodes[ Index ];
        Result.FindInput( 'texture2D' ).ValueProp.AsString:= IntToStr( Index );
      end;}
  end;}

begin
  for i:= 0 to Maps.Count - 1 do begin
    s:= Maps[ i ].FindInput( 'Map' );
    if ( s is TP3DShaderNodeSocketClone ) then
      TP3DShaderNodeSocketClone( s ).ValueProp.AsString:= IntToStr( FindMap( Maps[ i ].Map ));
  end;

  {Nd:= P3DData.P3DShaderNodeLib.FindNode( 'texture2D' );
  if ( not Assigned( Nd )) then
    exit;
  //TexNodes.Clear;
  for i:= 0 to Maps.Count - 1 do
    if ( i >= TexNodes.Count ) then
      AddTexNode( i );
  FNeedsUpdate:= True;
  Inc( i );
  while ( i < TexNodes.Count - 1 ) do
    begin
      TexNd:= TexNodes[ i ];
      TexNodes.Delete( i );
      TexNd.Free;
    end;}
end;

function TP3DMaterialNode.GetShader: TP3DShader;
begin
  if ( NeedsUpdate ) then
    inherited BuildShader;
  Result:= inherited GetShader;
end;

procedure TP3DMaterialNode.SocketHandler(Sender: TP3DNodeSocketClone);
begin
  FNeedsUpdate:= True;
end;

function TP3DMaterialNode.FindUniform(AName: String): TP3DNodeClone;
var
  UnName: String;
  Node: TP3DNodeClone;
begin
  Result:= nil;
  for Node in Uniforms do
    begin
      UnName:= Node.FindInput( 'UnName' ).ValueProp.AsString;
      if ( UnName = AName ) then begin
        Result:= Node;
        Break;
      end;
    end;
end;

function TP3DMaterialNode.AddMap(ATexture: TP3DTexture; ATexChannel: Integer): TP3DMaterialMapOld;
begin
  Result:= Maps.AddCopy( 'texture2D' );
  Result.Map:= ATexture;
  Result.TexChannel:= ATexChannel;
end;

constructor TP3DMaterialNode.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FNodeMaster:= TP3DNodeMaster.Create;

  FNodeTree:= TP3DShaderNodeTree.Create( 'NodeTree', smText );
  FNodeTree.OnChange:= @NodesChange;
  FNodeTree.NodeMaster:= NodeMaster;
  FRootNode:= TP3DStreamableContainer.Create( 'RootNode', TP3DNodeClone, smText );
  FRootNode.OnChange:= @NodeChange;
  FGeometryNode:= TP3DStreamableContainer.Create( 'GeometryNode', TP3DNodeClone, smText );
  FGeometryNode.OnChange:= @NodeChange;
  //FTexNodes:= TP3DShaderNodeTree.Create( 'TexNodes', smText );
  //FTexNodes.NodeMaster:= NodeMaster;
  Maps.NodeMaster:= NodeMaster;
  FUniforms:= TP3DShaderNodeTree.Create( 'Uniforms', smText );
  FUniforms.NodeMaster:= NodeMaster;
  FUniforms.OnChange:= @NodesChange;
  Properties.Add([ FNodeTree, FRootNode, FGeometryNode, {FTexNodes,} FUniforms ]);
  Maps.OnChange:= @NodesChange;
  FNeedsUpdate:= True;
  FAutoRecompile:= True;

  NodeMaster.AddSocketSetConnectedHandler( @SocketHandler );
  NodeMaster.AddSocketSetValueHandler( @SocketHandler );
end;

destructor TP3DMaterialNode.Destroy;
begin
  if ( Assigned( Maps )) then
    begin
      Maps.OnChange:= nil;
      Maps.AfterChange:= nil;
    end;
  //if ( Self = P3DMaterialActive ) then
  //  P3DMaterialActive:= nil;
  inherited Destroy;
  FreeAndNil( FNodeMaster );
end;

procedure TP3DMaterialNode.MakeRootNode(AName: String);
var
  nd: TP3DNode;
begin
  nd:= P3DShaderNodeLib.FindNode( AName );
  if ( Assigned( nd )) then
    MakeRootNode( P3DShaderNodeLib.FindNode( AName ))
  else
    raise Exception.Create( 'Cannot make copy to material root node. The specified node was not found: ' + AName );
end;

procedure TP3DMaterialNode.MakeRootNode(ANode: TP3DNode);
begin
  if ( Assigned( ANode )) then
    begin
      ANode.Clone( FRootNode );
      SetupRootNode;
    end
  else
    raise Exception.Create( 'Cannot make copy to material root node. The passed node was not assigned!' );
end;

procedure TP3DMaterialNode.BuildShader(AOwner: TP3DStreamableContainer);
var
  sh: TP3DShaderCompiled;
  i: Integer;
  Buffer, frag, vert: TP3DShaderBuffer;
  Socket: TP3DNodeSocketClone;
begin
  try
    sh:= //MatNode.NodeTree.Compile;
    TP3DShaderCompiled.Create;
    if ( Assigned( RootNode )) then
      for Socket in RootNode.Outputs do
        if ( Socket.CloneOf.SocketType = 'text' ) then
          begin
            Buffer:= TP3DShaderBuffer.Create;
            Buffer.Name:= Socket.CloneOf.Name;
            Buffer.Code:= TP3DShaderNodeSocketClone( Socket ).GetOutput( Buffer );
            sh.Buffers.Add( Buffer );
            log_info( message_header( Self ) + '<details><summary>Built ' + Buffer.Name + ' from chunks <br/></summary><pre><code class="glsl">' + P3DTextToHTML( Buffer.Code ) + '</code></pre></details>' );
          end;
  except
    on E: Exception do
      log_exception( message_header( Self ) + E.Message );
  end;
  //for i:= 0 to sh.Buffers.Count - 1 do
  //  P3DSaveStringToFile( '/tmp/' + sh.Buffers[ i ].Name + '.glsl', sh.Buffers[ i ].Code );

  try
    frag:= sh.FindBuffer( '__fshader' );
    vert:= sh.FindBuffer( '__vshader' );
    if ( Assigned( frag ) and Assigned( vert )) then
      TP3DShader.CreateFromVertexAndFragmentShader( vert.Code, frag.Code, AOwner );
  finally
    sh.Free;
  end;
end;

procedure TP3DMaterialNode.PassToActiveShader;
var
  Node: TP3DNodeClone;
  UnName: String;
  UnValue: TP3DPropertyAccess;
  decl: TP3DShaderDecl;
begin
  inherited PassToActiveShader;

  for Node in Uniforms do
    begin
      if ( Assigned( FOnPassUniform )) then
        FOnPassUniform( Self, Node );
      UnName:= Node.FindInput( 'UnName' ).ValueProp.AsString;
      UnValue:= Node.FindInput( 'Value' ).ValueProp;
      if ( Shader.Uniforms.FindByName( UnName, decl )) then
        begin
          case decl.VarType of
            GL_INT,
            GL_SAMPLER_1D,
            GL_SAMPLER_2D,
            GL_SAMPLER_3D,
            GL_SAMPLER_CUBE:
              if ( UnValue is TP3DPropAccessInt ) then
                decl.AsInt:= TP3DPropAccessInt( UnValue ).AsValue
              else
                decl.AsInt:= StrToInt( UnValue.AsString );
            GL_FLOAT:
              if ( UnValue is TP3DPropAccessFloat ) then
                decl.AsFloat:= TP3DPropAccessFloat( UnValue ).AsValue
              else
                decl.AsFloat:= StrToFloat( UnValue.AsString );
            GL_FLOAT_VEC2:
              if ( UnValue is TP3DPropAccessVec2 ) then
                decl.AsVec2:= TP3DPropAccessVec2( UnValue ).AsValue
              else
                decl.AsVec2:= vec2( UnValue.AsString );
            GL_FLOAT_VEC3:
              if ( UnValue is TP3DPropAccessVec3 ) then
                decl.AsVec3:= TP3DPropAccessVec3( UnValue ).AsValue
              else
                decl.AsVec3:= vec3( UnValue.AsString );
            GL_FLOAT_VEC4:
              if ( UnValue is TP3DPropAccessVec4 ) then
                decl.AsVec4:= TP3DPropAccessVec4( UnValue ).AsValue
              else
                decl.AsVec4:= vec4( UnValue.AsString );
            {GL_FLOAT_MAT2: Result:= 'mat2';
            GL_FLOAT_MAT3: Result:= 'mat3';
            GL_FLOAT_MAT4: Result:= 'mat4';
            GL_FLOAT_MAT2x3: Result:= 'mat2x3';
            GL_FLOAT_MAT2x4: Result:= 'mat2x3';
            GL_FLOAT_MAT3x2: Result:= 'mat3x2';
            GL_FLOAT_MAT3x4: Result:= 'mat3x4';
            GL_FLOAT_MAT4x2: Result:= 'mat4x2';
            GL_FLOAT_MAT4x3: Result:= 'mat4x3';}
          end;
        end;
    end;
end;

{ TP3DMaterialOld }

procedure TP3DMaterialOld.SetShader(AValue: TP3DShader);
begin
  FShader.AsValue:= AValue;
end;

function TP3DMaterialOld.FindMap(AMap: TP3DTexture): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Maps.Count - 1 do
    if ( Maps[ i ].Map = AMap ) then begin
      Result:= i;
      break;
    end;
end;


class function TP3DMaterialOld.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Materials' );
end;

function TP3DMaterialOld.GetAction: TP3DAction;
begin
  Result:= TP3DAction( FAction.AsValue );
end;

function TP3DMaterialOld.GetHasTransparency: Boolean;
begin
  Result:= False;
end;

procedure TP3DMaterialOld.SetAction(AValue: TP3DAction);
begin
  FAction.AsValue:= AValue;
end;

function TP3DMaterialOld.GetShader: TP3DShader;
begin
  Result:= TP3DShader( FShader.AsValue );
end;

constructor TP3DMaterialOld.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  Maps:= TP3DMaterialMapContainerListOld.Create( 'Maps', smText );
  Maps.ParentMaterial:= Self;
  Maps.FItemClass:= TP3DMaterialMapOld;
  Properties.Add( Maps );
  FShader:= TP3DStreamableContainer.Create( 'Shader', TP3DShader );
  Properties.Add( FShader );
end;

destructor TP3DMaterialOld.Destroy;
begin
  //Owner.Materials.Remove( Self );
  //WriteLn( 'Destroying Material: ' + Name );
  inherited Destroy;
end;

procedure TP3DMaterialOld.PassToActiveShader;
var
  i: Integer;
  decl: TP3DShaderDecl;
  S: TP3DNodeSocketClone;
  n: LongInt;
begin

  for i:= 0 to Maps.Count - 1 do
    if ( Shader.Uniforms.FindByName( 'tex' + IntToStr( i ), decl )) then
      begin
        glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
        {glBindTexture( Maps[ i ].Map.Target, Maps[ i ].Map.GLTexture ); cle( Self );}
        S:= Maps[ i ].FindInput( 'Map' );
        if ( Assigned( Maps[ i ].Map ) and Assigned( S )) then begin
          n:= StrToInt( S.ValueProp.AsString );
          Maps[ i ].Map.Bind( n );
          decl.AsInt:= n;
        end else begin
          glBindTexture( GL_TEXTURE_2D, 0 );
          decl.AsInt:= i;
        end;
      end;
  Inc( i );
  while ( Shader.Uniforms.FindByName( 'tex' + IntToStr( i ), decl )) do
    begin
      glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
      glBindTexture( GL_TEXTURE_2D, 0 );
      decl.AsInt:= i;
    end;
end;

procedure TP3DMaterialOld.PassToShader(const ActiveSubset: String);
begin
  //P3DMaterialActive:= Self;
  if ( Assigned( Shader )) then
    begin
      Shader.Enable;
      PassToActiveShader();
    end;
end;

procedure TP3DMaterialOld.BuildShader;
begin
  BuildShader( FShader );
  if ( not Assigned( FShader.AsValue )) then
    raise Exception.Create( 'Failed to build shader!' );
  FNeedsUpdate:= False;
end;

{ TP3DMaterialMapContainerListOld }

procedure TP3DMaterialMapContainerListOld.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  inherited Notify( ASender, AItem, AAction );
end;

{ TP3DMaterialMapShader }

procedure TP3DMaterialMapOld.SetMap(AValue: TP3DTexture);
var
  S: TP3DNodeSocketClone;
  n: String;
begin
  FMap.AsValue:= AValue;
  if ( Assigned( ParentMaterial )) then begin
    S:= FindInput( 'Map' );
    if ( S is TP3DShaderNodeSocketClone ) then begin
      n:= IntToStr( ParentMaterial.FindMap( AValue ));
      if ( n <> TP3DShaderNodeSocketClone( S ).ValueProp.AsString ) then
        TP3DShaderNodeSocketClone( S ).ValueProp.AsString:= n;
    end;
  end;
end;

procedure TP3DMaterialMapOld.SetTexChannel(AValue: Integer);
begin
  if FTexChannel=AValue then Exit;
  FTexChannel:=AValue;
end;


function TP3DMaterialMapOld.GetName: String;
begin
  if ( Assigned( Map )) then
    Result:= Map.Name
  else
    Result:= '<None>';
end;

function TP3DMaterialMapOld.GetParentMaterial: TP3DMaterialOld;
begin
  Result:= TP3DMaterialOld( FParentMaterial.AsValue );
end;

function TP3DMaterialMapOld.GetMap: TP3DTexture;
begin
  Result:= TP3DTexture( FMap.AsValue );
end;

constructor TP3DMaterialMapOld.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FTexChannel:= 0;
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'TexChannel', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Name' ));
  FMap:= TP3DStreamablePointer.Create( 'Map', TP3DTexture, smText );
  FParentMaterial:= TP3DStreamablePointer.Create( 'ParentMaterial', TP3DMaterialOld );
  FParentMaterial.AsValue:= TP3DStreamable( TP3DPropAccessStreamable( Owner.GetContainer.Context.GetInstance ).Context.GetInstance );

  Properties.Add([ FParentMaterial, FMap ]);
end;

destructor TP3DMaterialMapOld.Destroy;
begin
  if ( Assigned( FMap ) and Assigned( Map )) then
    Map:= nil;
  inherited Destroy;
end;
{$ENDIF}

