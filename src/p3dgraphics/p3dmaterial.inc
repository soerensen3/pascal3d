//p3dmaterial.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
  type
     { TP3DMaterialMap }

     TP3DMaterial = class;

     { TP3DMaterialMap }

     TP3DMaterialMap = class ( TInterfacedPersistent )
       private
         FMap: TP3DTexture;
         FParentMaterial: TP3DMaterial;
         FTexChannel: Integer;

         procedure SetMap( AValue: TP3DTexture );

       public
         constructor Create( AParentMaterial: TP3DMaterial );
         constructor CreateFromDOM( AParentMaterial: TP3DMaterial; ADOMNode: TDOMElement; const AData: TP3DData = nil );
         destructor Destroy; override;

       published
         property TexChannel: Integer read FTexChannel write FTexChannel;
         property Map: TP3DTexture read FMap write SetMap;
         property ParentMaterial: TP3DMaterial read FParentMaterial write FParentMaterial;
     end;

     TP3DMaterialBase = class;

     TP3DMapMode = ( p3dmmMix, p3dmmMultiply, p3dmmAdd, p3dmmSubtract );

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
         constructor Create( AParentMaterial: TP3DMaterial );
         constructor CreateFromDOM( AParentMaterial: TP3DMaterialBase; ADOMNode: TDOMElement; const AData: TP3DData = nil );

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

     TP3DMaterialMapList = specialize gP3DCustomObjectList < TP3DMaterialMap >;

     TP3DMaterial = class ( TP3DDataBlock )
       private
         FMaps: TP3DMaterialMapList;
         FShader: TP3DShader;
         ShaderOwned: Boolean;

         procedure SetShader( AValue: TP3DShader );

       public
         procedure ClearFields; override;

         constructor Create( const AData: TP3DData = nil ); override;
         procedure FastDestroy; override;

         procedure PassToShader(); virtual;
         procedure RemoveFromFields( Block: TP3DDataBlock ); override;

       published
         property Maps: TP3DMaterialMapList read FMaps write FMaps;
         property Shader: TP3DShader read FShader write SetShader;
     end;

     { TP3DMaterialShader }

     TP3DMaterialShader = class ( TP3DMaterial )
       public
         procedure LoadFromDOM( ADOMNode: TDOMElement ); override;
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

     { TP3DMaterialBase }

     TP3DMaterialBase = class ( TP3DMaterial )
       private
         FAlpha: Single;
         FDiff: TVec3;
         FModifiers: TP3DMaterialModifierList;
         FSpec: TVec3;
         FSpec_Hardness: Single;
         FUnlit: Boolean;
         FUseVertexColor: Boolean;

         procedure SetAlpha(AValue: Single);
         procedure SetSpec_Hardness(AValue: Single);
         procedure SetUnlit( AValue: Boolean );
         procedure AddPasses(ShaderTree: TP3DShaderNodeTree); virtual;

       public
         constructor Create(const AData: TP3DData=nil); override;
         procedure FastDestroy; override;

         procedure BuildShader(); virtual;
         procedure PassToShader(); override;

         procedure LoadFromDOM( ADOMNode: TDOMElement ); override;

         property Diff: TVec3 read FDiff write FDiff;
         property Spec: TVec3 read FSpec write FSpec;

       published
         property Spec_Hardness: Single read FSpec_Hardness write SetSpec_Hardness;
         property Alpha: Single read FAlpha write SetAlpha;
         property UseVertexColor: Boolean read FUseVertexColor write FUseVertexColor;
         property Unlit: Boolean read FUnlit write SetUnlit;
         property Modifiers: TP3DMaterialModifierList read FModifiers write FModifiers;
     end;

     TP3DMaterialList = specialize gP3DCustomDatablockList < TP3DMaterial >;

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

constructor TP3DMaterialMap.Create(AParentMaterial: TP3DMaterial);
begin
  inherited Create;
  FParentMaterial:= AParentMaterial;
  FTexChannel:= 0;
end;

constructor TP3DMaterialMap.CreateFromDOM(
  AParentMaterial: TP3DMaterial; ADOMNode: TDOMElement;
  const AData: TP3DData);
var
  TexName: DOMString;
  _Data: TP3DData;
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
  TP3DMaterialBase( ParentMaterial ).BuildShader();
end;

procedure TP3DMaterialMapBase.SetAlphaFactor(AValue: Single);
begin
  if FAlphaFactor= AValue then Exit;
  FAlphaFactor:= AValue;
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

constructor TP3DMaterialMapBase.CreateFromDOM(AParentMaterial: TP3DMaterialBase;
  ADOMNode: TDOMElement; const AData: TP3DData);
var
  TexName, FullTexName, DataName: DOMString;
  n: LongInt;
  _Data: TP3DData;
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
      n:= AParentMaterial.Owner.AppendFromDom( TDOMElement( ADOMNode.OwnerDocument.DocumentElement ), DataName, TP3DTexture );
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

procedure TP3DMaterialBase.SetAlpha(AValue: Single);
begin
  if FAlpha= AValue then Exit;
  FAlpha:= clamp( AValue, 0, 1 );
end;

procedure TP3DMaterialBase.SetSpec_Hardness(AValue: Single);
begin
  if FSpec_Hardness= AValue then Exit;
  FSpec_Hardness:= clamp( AValue, 0, 511 );
end;

constructor TP3DMaterialBase.Create(const AData: TP3DData);
begin
  inherited Create( AData );
  Alpha:= 1.0;
  Diff:= vec3( 1 );
  Modifiers:= TP3DMaterialModifierList.Create;
  Modifiers.Add( TP3DMaterialModifierInit.Create );
  Modifiers.Add( TP3DMaterialModifierWorldTransform.Create );
  Modifiers.Add( TP3DMaterialModifierTexTransform.Create );
  Modifiers.Add( TP3DMaterialModifierMaps.Create );
  Modifiers.Add( TP3DMaterialModifierLightning.Create );
  Modifiers.Add( TP3DMaterialModifierTransform.Create );
  Modifiers.Add( TP3DMaterialModifierCombine.Create );
end;

procedure TP3DMaterialBase.FastDestroy;
begin
  Modifiers.Free;
  inherited FastDestroy;
end;

procedure TP3DMaterialBase.BuildShader;
var
  ShaderTree: TP3DShaderNodeTree;
  Compiled: TP3DShaderCompiled;

begin
  ShaderTree:= TP3DShaderNodeTree.Create;
  Compiled:= nil;

  try
    AddPasses( ShaderTree );

    Compiled:= ShaderTree.Compile;

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
            ChangeSocketValueInt( ShaderTree.AddCopyEx( 'TC_TransformDynamic' ), 'TexCoordIndex', i )
          else
            ChangeSocketValueMat( ChangeSocketValueInt( ShaderTree.AddCopyEx( 'TC_TransformFixed' ), 'TexCoordIndex', i ),
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
  Result:= 0;
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
  ShaderTree.AddCopy( 'lib_maps' );
  ShaderTree.AddCopy( 'Pass_Init' );

  TexCoordsAdded:= [];
  with ( Material ) do
    for i:= 0 to Maps.Count - 1 do
      begin
        if ( not ( Maps[ i ].TexChannel in TexCoordsAdded )) then
          begin
            ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( 'TC_Init' ), 'Location', i + 8 ), 'TexCoordIndex', i );
            TexCoordsAdded:= TexCoordsAdded + [ Maps[ i ].TexChannel ];
          end;
        ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( 'Map_Init' ), 'MapIndex', i ), 'TexCoordIndex', Maps[ i ].TexChannel );
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
        ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( 'Pass_ReadF' ), 'Index', i ), 'TexCoordIndex', i );
        ChangeSocketValueInt( ChangeSocketValueInt( ShaderTree.AddCopyEx( 'Pass_ReadV' ), 'Index', i ), 'TexCoordIndex', i );
        if (( TP3DMaterialMapBase( Maps[ i ]).DiffuseFactor <> 0 ) or ( TP3DMaterialMapBase( Maps[ i ]).AlphaFactor <> 0 )) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( 'Pass_Diffuse' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).DiffuseFactor ),
                         'Alpha', TP3DMaterialMapBase( Maps[ i ]).AlphaFactor ),
                         ShaderTree.AddCopyEx( 'Pass_Combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).NormalFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( 'Pass_Normal' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).NormalFactor ),
          ShaderTree.AddCopyEx( 'Pass_Combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).SpecularFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( 'Pass_Specular' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).SpecularFactor ),
          ShaderTree.AddCopyEx( 'Pass_Combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
        if ( TP3DMaterialMapBase( Maps[ i ]).DisplaceFactor <> 0 ) then
          ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( ShaderTree.AddCopyEx( 'Pass_Displace' ), 'Index', i ), 'Factor', TP3DMaterialMapBase( Maps[ i ]).DisplaceFactor ),
          ShaderTree.AddCopyEx( 'Pass_Combine' ), 'Mode', Mode[ TP3DMaterialMapBase( Maps[ i ]).Mode ]);
      end;

  ShaderTree.AddCopy( 'Pass_Mix' );
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
  ShaderTree.AddCopy( 'Pass_WorldTransform' );
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
  ShaderTree.AddCopy( 'Pass_Transform' );
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
begin
  ShaderTree.AddCopy( 'Pass_Final' );
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
      ShaderTree.AddCopy( 'lib_common' );
      ShaderTree.AddCopy( 'lib_lighting' );
      ShaderTree.AddCopy( 'Lighting' );
    end
  else
    ShaderTree.AddCopy( 'Unlit' );
end;

procedure TP3DMaterialModifierLightning.PassToShader(Material: TP3DMaterialBase);
begin

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
  Result:= inherited Add(Item);
  SortByIndex;
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

procedure TP3DMaterial.ClearFields;
begin
  Shader:= nil;
  Maps.Clear( True );
  inherited ClearFields;
end;

constructor TP3DMaterial.Create(const AData: TP3DData);
begin
  inherited Create(AData);
  Owner.Materials.Add( Self );

  Maps:= TP3DMaterialMapList.Create;
end;

procedure TP3DMaterial.FastDestroy;
begin
  Maps.Free;

  Owner.Materials.Remove( Self );

  inherited FastDestroy;
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




{$ENDIF}
