//p3dmaterial.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
  type
     { TP3DMaterialMap }

     TP3DMaterial = class;

     { TP3DMaterialMap }

     TP3DMaterialMap = class ( TP3DInterfacedPersistent )
       private
         FHasTransparency: Boolean;
         FMap: TP3DTexture;
         FParentMaterial: TP3DMaterial;
         FTexChannel: Integer;

         function GetName: String;
         procedure SetMap( AValue: TP3DTexture );

       public
         constructor Create( AParentMaterial: TP3DMaterial ); virtual;
         constructor CreateFromDOM( AParentMaterial: TP3DMaterial; ADOMNode: TDOMElement; const AData: TP3DLibrary = nil );
//         function SaveToDOM( AParent: TDOMElement ): TDOMElement; override;
         destructor Destroy; override;

         class function DOMNodeName: String; override;

       published
         property TexChannel: Integer read FTexChannel write FTexChannel;
         property Map: TP3DTexture read FMap write SetMap;
         property ParentMaterial: TP3DMaterial read FParentMaterial write FParentMaterial;
         property HasTransparency: Boolean read FHasTransparency;
         property Name: String read GetName;
     end;

     TP3DMaterialMapClass = class of TP3DMaterialMap;

     TP3DMaterialBase = class;
     TP3DMaterialModifierType = class of TP3DMaterialModifier;

     TP3DMapMode = ( p3dmmMix, p3dmmMultiply, p3dmmAdd, p3dmmSubtract );
     TP3DMapTransparencyMode = ( p3dmtMultiply, p3dmtStep, p3dmtNone );

     { TP3DMaterialMapBase }

     TP3DMaterialMapBase = class ( TP3DMaterialMap )
       private
         FAlphaFactor: Single;
         FDiffuseFactor: Single;
         FDisplaceFactor: Single;
         FMode: TP3DMapMode;
         FNormalFactor: Single;
         FSpecularFactor: Single;
         FTransform: TMat4;
         FTransformDynamic: Boolean;
         procedure LoadTransformFromDOM(DOM: TDOMElement);
         procedure SetAlphaFactor(AValue: Single);
         procedure SetDiffuseFactor(AValue: Single);
         procedure SetDisplaceFactor(AValue: Single);
         procedure SetMode(AValue: TP3DMapMode);
         procedure SetNormalFactor(AValue: Single);
         procedure SetSpecularFactor(AValue: Single);

       public
         constructor Create( AParentMaterial: TP3DMaterial ); override;
         constructor CreateFromDOM( AParentMaterial: TP3DMaterialBase; ADOMNode: TDOMElement; const AData: TP3DLibrary = nil );

         property Transform: TMat4 read FTransform write FTransform;
         property TransformDynamic: Boolean read FTransformDynamic write FTransformDynamic;

       published
         property DiffuseFactor: Single read FDiffuseFactor write SetDiffuseFactor;
         property AlphaFactor: Single read FAlphaFactor write SetAlphaFactor;
         property NormalFactor: Single read FNormalFactor write SetNormalFactor;
         property SpecularFactor: Single read FSpecularFactor write SetSpecularFactor;
         property DisplaceFactor: Single read FDisplaceFactor write SetDisplaceFactor;
         property Mode: TP3DMapMode read FMode write SetMode;
     end;

     { TP3DMaterialMapList }

     TP3DMaterialMapList = class( specialize gP3DCustomObjectList < TP3DMaterialMap >)
       private
         FMapClass: TP3DMaterialMapClass;
         FParentMaterial: TP3DMaterial;

       public
         constructor Create( AParentMaterial: TP3DMaterial );
         procedure SaveToDOM( AParent: TDOMElement );
         procedure LoadFromDOM( ADOMNode: TDOMElement );

         property MapClass: TP3DMaterialMapClass read FMapClass write FMapClass;
         property ParentMaterial: TP3DMaterial read FParentMaterial write FParentMaterial;
         //needs map class
     end;

     TP3DPropertyAccessMaterialMapList = specialize gP3DListPropertyAccess < TP3DMaterialMapList, TP3DMaterialMap >;


     TP3DMaterial = class ( TP3DDataBlock )
       private
         FMaps: TP3DMaterialMapList;
         FShader: TP3DShader;
         ShaderOwned: Boolean;

         function GetHasTransparency: Boolean; virtual;
         procedure SetShader( AValue: TP3DShader );

       public
         procedure ClearFields; override;

         constructor Create( const AData: TP3DLibrary = nil ); override;
         procedure FastDestroy; override;
         class function DOMNodeName: String; override;
         function SaveToDOM(AParent: TDOMElement): TDOMElement; override;
         procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;

         procedure PassToShader(); virtual;
         procedure RemoveFromFields( Block: TP3DDataBlock ); override;

       published
         property Maps: TP3DMaterialMapList read FMaps write FMaps;
         property Shader: TP3DShader read FShader write SetShader;
         property HasTransparency: Boolean read GetHasTransparency;
     end;

     TP3DPropertyAccessMaterial = specialize gP3DPropertyAccessDataBlock < TP3DMaterial >;

     { TP3DMaterialShader }

     TP3DMaterialShader = class ( TP3DMaterial )
       public
         procedure LoadFromDOM( ADOMNode: TDOMElement ); override;
         class function DOMNodeName: String; override;
     end;

     { TP3DMaterialModifier }

     TP3DMaterialModifier = class( TInterfacedPersistent )
       class function GetIndex: Integer; virtual; abstract;
       procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); virtual; abstract;
       procedure PassToShader( Material: TP3DMaterialBase ); virtual; abstract;
     end;

     { TP3DMaterialModifierList }

     TP3DMaterialModifierList = class ( specialize gP3DCustomObjectList < TP3DMaterialModifier >)
       public
         procedure SortByIndex;
         function Add( Item: TP3DMaterialModifier ): Integer; override;
         function Find( ModType: TP3DMaterialModifierType ): Integer;
     end;

     { TP3DMaterialModifierTexTransform }

     TP3DMaterialModifierTexTransform = class ( TP3DMaterialModifier )
       class function GetIndex: Integer; override;
       procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
       procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierInit }

     TP3DMaterialModifierInit = class ( TP3DMaterialModifier )
       class function GetIndex: Integer; override;
       procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
       procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierMaps }

     TP3DMaterialModifierMaps = class ( TP3DMaterialModifier )
       class function GetIndex: Integer; override;
       procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
       procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierLightning }

     TP3DMaterialModifierLightning = class ( TP3DMaterialModifier )
       class function GetIndex: Integer; override;
       procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
       procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierWorldTransform }

     TP3DMaterialModifierWorldTransform = class ( TP3DMaterialModifier )
       class function GetIndex: Integer; override;
       procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
       procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;

     { TP3DMaterialModifierTransform }

     TP3DMaterialModifierTransform = class ( TP3DMaterialModifier )
       class function GetIndex: Integer; override;
       procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
       procedure PassToShader( Material: TP3DMaterialBase ); override;
     end;


     { TP3DMaterialModifierCombine }

     TP3DMaterialModifierCombine = class ( TP3DMaterialModifier )
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
         constructor Create;
         destructor Destroy; override;

         class function GetIndex: Integer; override;
         procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
         procedure PassToShader( Material: TP3DMaterialBase ); override;
         procedure PushTexture;

         property Transforms: TVec3List read FTransforms write FTransforms;
     end;


     { TP3DMaterialBase }

     TP3DMaterialBase = class ( TP3DMaterial )
       private
         FAlpha: Single;
         FAlphaThreshhold: Single;
         FDiff: TVec3;
         FInstancing: Boolean;
         FModifiers: TP3DMaterialModifierList;
         FSpec: TVec3;
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
         procedure AddPasses(ShaderTree: TP3DShaderNodeTree); virtual;
         function GetHasTransparency: Boolean; override;
         procedure UpdateHasTransparency;

       public
         constructor Create(const AData: TP3DLibrary=nil); override;
         procedure FastDestroy; override;

         procedure BuildShader(); virtual;
         procedure PassToShader(); override;

         procedure LoadFromDOM( ADOMNode: TDOMElement ); override;
         procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
         class function DOMNodeName: String; override;

         property Diff: TVec3 read FDiff write FDiff;
         property Spec: TVec3 read FSpec write FSpec;

       published
         property Spec_Hardness: Single read FSpec_Hardness write SetSpec_Hardness;
         property Alpha: Single read FAlpha write SetAlpha;
         property UseVertexColor: Boolean read FUseVertexColor write FUseVertexColor;
         property Unlit: Boolean read FUnlit write SetUnlit;
         property Modifiers: TP3DMaterialModifierList read FModifiers write FModifiers;
         property Instancing: Boolean read FInstancing write SetInstancing;
         property TransparencyMode: TP3DMapTransparencyMode read FTransparencyMode write SetTransparencyMode;
         property AlphaThreshhold: Single read FAlphaThreshhold write SetAlphaThreshhold;
     end;

     TP3DMaterialList = specialize gP3DCustomDatablockList < TP3DMaterial >;

  var
    P3DMaterialActive: TP3DMaterial = nil;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DMaterialMapShader }

procedure TP3DMaterialMap.SetMap(AValue: TP3DTexture);
begin
  if FMap=AValue then Exit;

  if ( Assigned( FMap ) and P3DData.IsValid( FMap )) then
    Map.RemoveUser( ParentMaterial );

  FMap:= AValue;

  if ( Assigned( FMap )) then
    Map.AddUser( ParentMaterial );
end;

function TP3DMaterialMap.GetName: String;
begin
  if ( Assigned( Map )) then
    Result:= Map.Name
  else
    Result:= '<empty>';
end;

constructor TP3DMaterialMap.Create( AParentMaterial: TP3DMaterial );
begin
  inherited Create;
  FParentMaterial:= AParentMaterial;

  FTexChannel:= 0;
  Properties.Context:= AParentMaterial;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'TexChannel', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Name' ));
  Properties.Add( TP3DPropertyAccessTexture.CreateFieldSet( 'Map', @FMap, @SetMap, smAttribute ));
end;

constructor TP3DMaterialMap.CreateFromDOM(AParentMaterial: TP3DMaterial; ADOMNode: TDOMElement; const AData: TP3DLibrary);
var
  TexName: DOMString;
  _Data: TP3DLibrary;
  n: LongInt;
begin
  Create( AParentMaterial );
  TexName:= ADOMNode.GetAttribute( 'file' );

  if ( not FileExists( TexName )) then
    raise Exception.Create( 'Error: The specified texture "' + TexName + '" could not be found!' );

  if ( not Assigned( AData )) then
    _Data:= P3DData
  else
    _Data:= AData;

  n:= _Data.Textures.FindByFileName( TexName );
  if ( n >= 0 ) then
    Map:= _Data.Textures[ n ]
  else
    begin
      Map:= TP3DTexture.CreateFromFile( TexName, AData );
    end;
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

class function TP3DMaterialMap.DOMNodeName: String;
begin
  Result:= 'map';
end;

{ TP3DMaterialMapBase }

procedure TP3DMaterialMapBase.SetDiffuseFactor(AValue: Single);
begin
  if FDiffuseFactor= AValue then Exit;
  FDiffuseFactor:= AValue;
  TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetAlphaFactor(AValue: Single);
begin
  if FAlphaFactor= AValue then Exit;
  FAlphaFactor:= AValue;
  FHasTransparency:= AlphaFactor > 0;
  TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetDisplaceFactor(AValue: Single);
begin
  if FDisplaceFactor= AValue then Exit;
  FDisplaceFactor:= AValue;
  TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetMode(AValue: TP3DMapMode);
begin
  if FMode= AValue then Exit;
  FMode:= AValue;
  TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetNormalFactor(AValue: Single);
begin
  if FNormalFactor= AValue then Exit;
  FNormalFactor:= AValue;
  TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetSpecularFactor(AValue: Single);
begin
  if FSpecularFactor= AValue then Exit;
  FSpecularFactor:= AValue;
  TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

constructor TP3DMaterialMapBase.Create(AParentMaterial: TP3DMaterial);
begin
  inherited;
  FDiffuseFactor:= 1.0;
  FNormalFactor:= 0.0;
  FSpecularFactor:= 0.0;
  FMode:= p3dmmMix;
  FTransform:= Mat4Identity;
  TexChannel:= 0;
  Properties.Context:= AParentMaterial;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'DiffuseFactor', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'SpecularFactor', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'DisplaceFactor', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'AlphaFactor', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Mode', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'TexChannel', smAttribute ));
end;

procedure TP3DMaterialMapBase.LoadTransformFromDOM( DOM: TDOMElement );
var
  Position, Scale: TVec3;
  Quaternion: TQuaternion;
begin
  Position.FromString( DOM.GetAttribute( 'position' ));
  //Quaternion.FromString( DOM.GetAttribute( 'quaternion' ));
  //Quaternion:= quat( 1 );
  Scale.FromString( DOM.GetAttribute( 'scale' ));
  Transform:= mat4translate( vec4( Position, 1 )){ * mat4( mat3( Quaternion ))} * mat4scale( vec4( Scale, 1 ));
end;

constructor TP3DMaterialMapBase.CreateFromDOM(AParentMaterial: TP3DMaterialBase; ADOMNode: TDOMElement; const AData: TP3DLibrary);
var
  TexName, FullTexName, DataName: DOMString;
  n: LongInt;
  _Data: TP3DLibrary;
  TransSect: TDOMNodeList;
begin
  Create( AParentMaterial );

  TexName:= ADOMNode.GetAttribute( 'file' );
  FullTexName:= P3DSearchPaths.Models.FindFileEx( TexName, GetCurrentDir );
  if ( not FileExists( FullTexName )) then
    raise Exception.Create( 'Error: The specified texture "' + TexName + '" could not be found!' );

  if ( not Assigned( AData )) then
    _Data:= P3DData
  else
    _Data:= AData;

  DataName:= ADOMNode.GetAttribute( 'data' );
  if ( DataName > '' ) then
    begin
      n:= AParentMaterial.Owner.AppendFromPath( DataName, TP3DTexture );
      Map:= TP3DTexture( AParentMaterial.Owner.DataBlocks[ n ]);
    end
  else
    Map:= nil;

  DiffuseFactor:= StrToFloatDef( ADOMNode.GetAttribute( 'diffuse' ), 0.0 );
  NormalFactor:= StrToFloatDef( ADOMNode.GetAttribute( 'normal' ), 0.0 );
  SpecularFactor:= StrToFloatDef( ADOMNode.GetAttribute( 'specular' ), 0.0 );
  DisplaceFactor:= StrToFloatDef( ADOMNode.GetAttribute( 'displace' ), 0.0 );
  AlphaFactor:= StrToFloatDef( ADOMNode.GetAttribute( 'alpha' ), 0.0 );
  TexChannel:= StrToIntDef( ADOMNode.GetAttribute( 'layer' ), 0 );

  case ADOMNode.GetAttribute( 'mode' ) of
    'add': Mode:= p3dmmAdd;
    'multiply': Mode:= p3dmmMultiply;
    'subtract': Mode:= p3dmmSubtract;
    else
      Mode:= p3dmmMix;
  end;

  TransSect:= ADOMNode.GetElementsByTagName( 'transform' );
  if ( TransSect.Count > 0 ) then
    LoadTransformFromDOM( TDOMElement( TransSect.Item[ 0 ]));
  FreeAndNil( TransSect );
end;


{ TMaterial }

procedure TP3DMaterialBase.SetUnlit(AValue: Boolean);
begin
  if FUnlit=AValue then Exit;
  FUnlit:=AValue;
  BuildShader();
end;

procedure TP3DMaterialBase.AddPasses(ShaderTree: TP3DShaderNodeTree);
var
  Modifier: TP3DMaterialModifier;
begin
  for Modifier in Modifiers do
    Modifier.AddPass( ShaderTree, Self );
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

  if (( not FHasTransparency ) and ( TransparencyMode = p3dmtMultiply )) then
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

constructor TP3DMaterialBase.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );

  Maps.MapClass:= TP3DMaterialMapBase;

  Alpha:= 1.0;
  Diff:= vec3( 1 );
  FTransparencyMode:= p3dmtMultiply;
  FAlphaThreshhold:= 0.9;

  Modifiers:= TP3DMaterialModifierList.Create;
  Modifiers.Add( TP3DMaterialModifierInit.Create );
  Modifiers.Add( TP3DMaterialModifierWorldTransform.Create );
  Modifiers.Add( TP3DMaterialModifierTexTransform.Create );
  Modifiers.Add( TP3DMaterialModifierMaps.Create );
  Modifiers.Add( TP3DMaterialModifierLightning.Create );
  Modifiers.Add( TP3DMaterialModifierTransform.Create );
  Modifiers.Add( TP3DMaterialModifierCombine.Create );

  Properties.Add( TP3DPropertyAccessVec3.CreateField( 'Diff', @FDiff, smAttribute ));
  Properties.Add( TP3DPropertyAccessVec3.CreateField( 'Spec', @FSpec, smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Unlit', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Alpha', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'AlphaThreshhold', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'TransparencyMode', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Spec_Hardness', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Instancing', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'UseVertexColor', smAttribute ));
end;

procedure TP3DMaterialBase.FastDestroy;
begin
  Modifiers.Clear( True );
  Modifiers.Free;
  inherited FastDestroy;
end;

procedure TP3DMaterialBase.BuildShader;
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
    WriteLn( Compiled.Buffers.Count, ' buffers created' );
  end;

begin
  ShaderTree:= TP3DShaderNodeTree.Create;
  Compiled:= nil;

  try
    Passes:= TP3DShaderNodeTree.Create;
    AddPasses( Passes );

    CompileBuffers;

    //Compiled:= ShaderTree.Compile;

    Passes.Free;

    Shader:= TP3DShader.CreateFromVertexAndFragmentShader( Compiled.FindBuffer( 'vshader' ).Code, Compiled.FindBuffer( 'fshader' ).Code, Owner );
    ShaderOwned:= True;
  finally
    ShaderTree.Free;
    if ( Assigned( Compiled )) then
      Compiled.Free;
  end;
end;

procedure TP3DMaterialBase.PassToShader;
var
  i: Integer;
begin
  if ( not Assigned( Shader )) then
    BuildShader();
  if ( Assigned( Shader )) then
    begin
      P3DMaterialActive:= Self;
      Shader.Enable;
      if ( not UseVertexColor ) then
        begin
          glVertexAttrib4f( P3DAttribColor,
            Diff.R, Diff.G, Diff.B, Alpha ); cle( Self );
        end;

      glUniform4f( Shader.Uniforms.AddrByName( 'mat_specular' ), Spec.r, Spec.g, Spec.b, 1 ); cle( Self );
      glUniform1f( Shader.Uniforms.AddrByName( 'mat_hardness' ), Spec_Hardness ); cle( Self );

      for i:= 0 to Maps.Count - 1 do
        begin
          glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
          if ( Assigned( Maps[ i ].Map )) then
            glBindTexture( GL_TEXTURE_2D, Maps[ i ].Map.GLTexture ); cle( Self );
          glUniform1i( Shader.Uniforms.AddrByName( 'tex' + IntToStr( i )), i ); cle( Self );
        end;
      for i:= 0 to Modifiers.Count - 1 do
        Modifiers[ i ].PassToShader( Self );
    end;
end;

procedure TP3DMaterialBase.LoadFromDOM(ADOMNode: TDOMElement);
var
  SpecTmp: TVec4;
  tex: TDOMElement;

  procedure LoadTex;
  var
    Map: TP3DMaterialMapBase;
  begin
    if ( Maps.Count > 7 ) then
      raise Exception.Create( 'Maximum Number of Maps reached: 8' );
    Map:= TP3DMaterialMapBase.CreateFromDOM( Self, tex, Owner );

    Maps.Add( Map );
  end;

begin
  inherited LoadFromDOM( ADOMNode );
  Name:= ADOMNode.GetAttribute( 'name' );
  Alpha:= StrToFloatDef( ADOMNode.GetAttribute( 'alpha' ), 1.0 );
  Diff.FromString( ADOMNode.GetAttribute( 'diffuse' ));
  SpecTmp.FromString( ADOMNode.GetAttribute( 'specular' ));
  Spec:= SpecTmp.YZW;
  Spec_Hardness:= SpecTmp.X;

  if ( ADOMNode.GetAttribute( 'unlit' ) = 'yes' ) then
    FUnlit:= True;

  FTransparencyMode:= p3dmtStep;

  Instancing:= True;

  Maps.Clear( True );
  tex:= TDOMElement( ADOMNode.FirstChild );
  while Assigned( tex ) do
    begin
      case tex.NodeName of
        'map': LoadTex;
        '#comment':;
      else
        raise Exception.Create( 'Unknown tag inside Material Element: '+ tex.NodeName );
      end;
    tex:= TDOMElement( tex.NextSibling );
  end;
  BuildShader;
end;

procedure TP3DMaterialBase.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew(ADOMNode);
  BuildShader();
end;

class function TP3DMaterialBase.DOMNodeName: String;
begin
  Result:= 'materialbase';
end;

procedure TP3DMaterialShader.LoadFromDOM(ADOMNode: TDOMElement);
var
  tex: TDOMElement;

  procedure LoadTex;
  var
    Map: TP3DMaterialMap;
  begin
    if ( Maps.Count > 7 ) then
      raise Exception.Create( 'Maximum Number of Maps reached: 8' );
    Map:= TP3DMaterialMap.CreateFromDOM( Self, tex, Owner );

    Maps.Add( Map );
  end;
var
  VShaderFile, FShaderFile: DOMString;
begin
  inherited LoadFromDOM( ADOMNode );
  VShaderFile:= ADOMNode.GetAttribute( 'vshader' );
  FShaderFile:= ADOMNode.GetAttribute( 'fshader' );
  Shader:= TP3DShader.CreateFromVertexAndFragmentShaderFile( VShaderFile, FShaderFile, Owner );

  tex:= TDOMElement( ADOMNode.FirstChild );
  Maps.Clear( True );
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
end;

class function TP3DMaterialShader.DOMNodeName: String;
begin
  Result:= 'materialshader';
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
            ChangeSocketValueInt( ShaderTree.AddCopyEx( '50_map_texcoord_transform_dynamic' ), 'TexCoordIndex', i )
          else
            ChangeSocketValueMat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '50_map_texcoord_transform_fixed' ), 'TexCoordIndex', i ),
              'Transform', Transform );
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
              decl.AsMat4:= TP3DMaterialMapBase( Maps[ i ]).FTransform;
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
  ShaderTree.AddCopy( '10_map_lib' );
  ShaderTree.AddCopy( '10_map_pass_init' );

  TexCoordsAdded:= [];
  with ( Material ) do
    for i:= 0 to Maps.Count - 1 do
      begin
        if ( not ( Maps[ i ].TexChannel in TexCoordsAdded )) then
          begin
            ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( '10_map_texcoord_init' ), 'Location', i + 8 ), 'TexCoordIndex', i );
            TexCoordsAdded:= TexCoordsAdded + [ Maps[ i ].TexChannel ];
          end;
        ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( '10_map_init' ), 'MapIndex', i ), 'TexCoordIndex', Maps[ i ].TexChannel );
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
  Mode: array [ p3dmmMix .. p3dmmSubtract ] of String = ( 'mix', 'multiply', 'add', 'subtract' );

var
  i: Integer;
begin
  with ( Material ) do
    for i:= 0 to Maps.Count - 1 do
      begin
        ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( '100_map_pass_read_frag' ), 'Index', i ), 'TexCoordIndex', i );
        ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( '100_map_pass_read_vert' ), 'Index', i ), 'TexCoordIndex', i );
        if (( TP3DMaterialMapBase( Maps[ i ]).DiffuseFactor <> 0 ) or ( TP3DMaterialMapBase( Maps[ i ]).AlphaFactor <> 0 )) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '100_map_pass_diffuse' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).DiffuseFactor ),
                         'Alpha', TP3DMaterialMapBase( Maps[ i ]).AlphaFactor ),
                         ShaderTree.AddCopyEx( '100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).NormalFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '100_map_pass_normal' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).NormalFactor ),
          ShaderTree.AddCopyEx( '100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).SpecularFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '100_map_pass_specular' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).SpecularFactor ),
          ShaderTree.AddCopyEx( '100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).DisplaceFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( '100_map_pass_displace' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).DisplaceFactor ),
          ShaderTree.AddCopyEx( '100_map_pass_combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
      end;

  ShaderTree.AddCopy( '100_map_pass_mix' );
end;


procedure TP3DMaterialModifierMaps.PassToShader(Material: TP3DMaterialBase);
begin

end;

{ TP3DMaterialModifierWorldTransform }

class function TP3DMaterialModifierWorldTransform.GetIndex: Integer;
begin
  Result:= 30;

end;

procedure TP3DMaterialModifierWorldTransform.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
begin
  if ( Material.Instancing ) then
    begin
      ShaderTree.AddCopy( '30_map_pass_world_transform_init_instanced' );
      ShaderTree.AddCopy( '40_map_pass_world_transform_init_instanced_attrib' );
      ShaderTree.AddCopy( '50_map_pass_world_transform_init_instanced_grid' );
    end;
  //else
  //  ShaderTree.AddCopy( '30_map_pass_world_transform_init' );
  ShaderTree.AddCopy( '30_map_pass_world_transform' );
end;

procedure TP3DMaterialModifierWorldTransform.PassToShader(Material: TP3DMaterialBase);
begin

end;


{ TP3DMaterialModifierTransform }

class function TP3DMaterialModifierTransform.GetIndex: Integer;
begin
  Result:= 5000;
end;

procedure TP3DMaterialModifierTransform.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
begin
  ShaderTree.AddCopy( '5000_map_pass_view_transform' );
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
  if ( Material.TransparencyMode = p3dmtStep ) then
    ChangeSocketValueFloat( ShaderTree.AddCopyEx( '9990_map_pass_clip_transparency' ), 'AlphaTreshhold', Material.AlphaThreshhold )
  else if ( Material.TransparencyMode = p3dmtNone ) then
    ChangeSocketValueFloat( ShaderTree.AddCopyEx( '9990_map_pass_clip_transparency' ), 'AlphaTreshhold', 0 );
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
      ShaderTree.AddCopy( '1000_common_lib' );
      ShaderTree.AddCopy( '1000_light_lib' );
      ShaderTree.AddCopy( '1000_light_pass_calc_lighting' );
    end
  else
    ShaderTree.AddCopy( '1000_light_pass_unlit' );
end;

procedure TP3DMaterialModifierLightning.PassToShader(Material: TP3DMaterialBase);
begin

end;

{ TP3DMaterialModifierArray }

constructor TP3DMaterialModifierArray.Create;
begin
  inherited;
  FTransforms:= TVec3List.Create;
  FTexture:= TP3DTexture.CreateFromEmpty( 1, 1, 3, dtFloat );
  FTexture.MipMap:= False;
end;

destructor TP3DMaterialModifierArray.Destroy;
begin
  Transforms.Free;
  if ( P3DData.IsValid( FTexture )) then
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
  if ( P3DData.IsValid( FTexture )) then
    FreeAndNil( FTexture );
  FTexture:= TP3DTexture.CreateFromBuffer( Transforms.Count, 1, 3, dtFloat, Transforms.Ptr );
  FTexture.MipMap:= False;
end;


{ TP3DMaterialModifierList }

procedure TP3DMaterialModifierList.SortByIndex;

  procedure swap( a, b: Integer );
  var
    Element: TP3DMaterialModifier;
  begin
    Element:= Self[ a ];
    FItems[ a ]:= Self[ b ];
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

function TP3DMaterialModifierList.Add(Item: TP3DMaterialModifier): Integer;
begin
  inherited Add(Item);
  SortByIndex;
  Result:= IndexOf( Item );
end;

function TP3DMaterialModifierList.Find(ModType: TP3DMaterialModifierType): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ] is ModType ) then
      begin
        Result:= i;
        break;
      end;
end;

{ TP3DMaterial }

procedure TP3DMaterial.SetShader(AValue: TP3DShader);
begin
  if ( FShader = AValue ) then Exit;
  if ( Assigned( Shader ) and P3DData.IsValid( Shader )) then
    begin
      Shader.RemoveUser( Self );
      if ( ShaderOwned ) then
        Shader.Free;
    end;
  FShader:= AValue;
  ShaderOwned:= False;
  if ( Assigned( Shader )) then
    Shader.AddUser( Self );
end;

function TP3DMaterial.GetHasTransparency: Boolean;
begin
  Result:= False;
end;

procedure TP3DMaterial.ClearFields;
begin
  Shader:= nil;
  Maps.Clear( True );
  inherited ClearFields;
end;

constructor TP3DMaterial.Create(const AData: TP3DLibrary);
begin
  inherited Create(AData);
  Owner.Materials.Add( Self );

  Maps:= TP3DMaterialMapList.Create( Self );
  Maps.MapClass:= TP3DMaterialMap;
  Properties.Add( TP3DPropertyAccessDataBlockList.CreateField( 'Maps', @FMaps ));
end;

procedure TP3DMaterial.FastDestroy;
begin
  Maps.Free;

  Owner.Materials.Remove( Self );

  inherited FastDestroy;
end;

class function TP3DMaterial.DOMNodeName: String;
begin
  Result:= 'material';
end;

function TP3DMaterial.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM( AParent );
  Maps.SaveToDOM( Result );
end;

procedure TP3DMaterial.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew(ADOMNode);
  Maps.LoadFromDOM( ADOMNode );
end;

procedure TP3DMaterial.PassToShader;
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

procedure TP3DMaterial.RemoveFromFields(Block: TP3DDataBlock);
var
  Map: TP3DMaterialMap;
begin
  inherited RemoveFromFields( Block );
  if ( Block is TP3DTexture ) then
    for Map in Maps do
      if ( Block = Map.Map ) then
        Map.Map:= nil;
  if ( Block is TP3DShader ) then
    if ( Block = Shader ) then
      FShader:= nil;
end;


{ TP3DMaterialMapList }

constructor TP3DMaterialMapList.Create(AParentMaterial: TP3DMaterial);
begin
  inherited Create;
  ParentMaterial:= AParentMaterial;
end;

procedure TP3DMaterialMapList.SaveToDOM( AParent: TDOMElement );
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure TP3DMaterialMapList.LoadFromDOM(ADOMNode: TDOMElement);
var
  i: Integer;
  lst: TDOMNodeList;
  Item: TP3DMaterialMap;
begin
  Clear();
  if ( Assigned( MapClass )) then
    begin
      lst:= ADOMNode.GetElementsByTagName( MapClass.DOMNodeName );
      try
        for i:= 0 to lst.Count - 1 do
          begin
            Item:= MapClass.Create( ParentMaterial );
            Item.LoadFromDOMNew( TDOMElement( lst[ i ]));
            Add( Item );
          end;

      finally
        lst.Free;
      end;
    end;
end;


{$ENDIF}

{$IFDEF FINALIZATION}
  P3DMaterialActive:= nil;
{$ENDIF}

