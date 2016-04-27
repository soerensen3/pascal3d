//p3dmaterial.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
  type
     { TP3DMaterialMap }

     TP3DMapMode = ( p3dmmMix, p3dmmMultiply, p3dmmAdd, p3dmmSubtract );

     TP3DMaterial = class;
     TP3DMaterialMap = class ( TPersistent )
       private
         FDiffuseFactor: Single;
         FMap: TP3DTexture;
         FMode: TP3DMapMode;
         FNormalFactor: Single;
         FParentMaterial: TP3DMaterial;
         FSpecularFactor: Single;
         FTexChannel: Integer;
         procedure SetMap(AValue: TP3DTexture);

       public
         constructor Create;
         constructor CreateFromDOM( AParentMaterial: TP3DMaterial; ADOMNode: TDOMElement; const AData: TP3DData = nil );

       published
         property DiffuseFactor: Single read FDiffuseFactor write FDiffuseFactor;
         property NormalFactor: Single read FNormalFactor write FNormalFactor;
         property SpecularFactor: Single read FSpecularFactor write FSpecularFactor;
         property TexChannel: Integer read FTexChannel write FTexChannel;
         property Mode: TP3DMapMode read FMode write FMode;
         property Map: TP3DTexture read FMap write SetMap;
         property ParentMaterial: TP3DMaterial read FParentMaterial write FParentMaterial;
     end;

     TP3DMaterialMapList = specialize gP3DCustomObjectList < TP3DMaterialMap >;

    { TP3DMaterial }

     TP3DMaterial = class ( TP3DDataBlock )
       private
         FAlpha: Single;
         FDiff: TVec3;
         FMaps: TP3DMaterialMapList;
         FShader: TP3DShader;
         FSpec: TVec3;
         FSpec_Hardness: Single;
         FUnlit: Boolean;
         FUseVertexColor: Boolean;

         procedure SetUnlit(AValue: Boolean);

       public
         procedure BuildShader();
         procedure PassToShader();

         constructor Create( const AData: TP3DData = nil );
         destructor Destroy; override;

         procedure LoadFromDOM( ADOMNode: TDOMElement ); override;

         property Diff: TVec3 read FDiff write FDiff;
         property Spec: TVec3 read FSpec write FSpec;

       published
         property Maps: TP3DMaterialMapList read FMaps write FMaps;
         property Spec_Hardness: Single read FSpec_Hardness write FSpec_Hardness;
         property Alpha: Single read FAlpha write FAlpha;
         property Shader: TP3DShader read FShader write FShader;
         property UseVertexColor: Boolean read FUseVertexColor write FUseVertexColor;
         property Unlit: Boolean read FUnlit write SetUnlit;
     end;

     TP3DMaterialList = specialize gP3DCustomDatablockList < TP3DMaterial >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DMaterialMap }

procedure TP3DMaterialMap.SetMap(AValue: TP3DTexture);
begin
  if FMap=AValue then Exit;

  if ( Assigned( FMap )) then
    FMap.Users.Delete( FMap.Users.IndexOf( ParentMaterial ));

  FMap:= AValue;

  if ( Assigned( FMap )) then
    FMap.Users.Add( ParentMaterial );
end;

constructor TP3DMaterialMap.Create;
begin
  inherited;
  FDiffuseFactor:= 1.0;
  FNormalFactor:= 0.0;
  FSpecularFactor:= 0.0;
  FMode:= p3dmmMix;
  FTexChannel:= 0;
end;

constructor TP3DMaterialMap.CreateFromDOM(AParentMaterial: TP3DMaterial;
  ADOMNode: TDOMElement; const AData: TP3DData);
var
  TexName: DOMString;
  n: LongInt;
  _Data: TP3DData;
begin
  Create;
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
      Map:= TP3DTexture.Create( TexName, AData );
      _Data.Textures.Add( Map );
    end;
  DiffuseFactor:= StrToFloatDef( ADOMNode.GetAttribute( 'diffuse' ), 0.0 );
  NormalFactor:= StrToFloatDef( ADOMNode.GetAttribute( 'normal' ), 0.0 );
  SpecularFactor:= StrToFloatDef( ADOMNode.GetAttribute( 'specular' ), 0.0 );
  TexChannel:= StrToIntDef( ADOMNode.GetAttribute( 'layer' ), 0 );
  case ADOMNode.GetAttribute( 'mode' ) of
    'add': Mode:= p3dmmAdd;
    'multiply': Mode:= p3dmmMultiply;
    'subtract': Mode:= p3dmmSubtract;
    else
      Mode:= p3dmmMix;
  end;
end;


{ TMaterial }

procedure TP3DMaterial.SetUnlit(AValue: Boolean);
begin
  if FUnlit=AValue then Exit;
  FUnlit:=AValue;
  BuildShader();
end;

procedure TP3DMaterial.BuildShader;
var
  ShaderTree: TP3DShaderNodeTree;
  Node: TP3DShaderNode;
  Compiled: TP3DShaderCompiled;
  i: Integer;

  function AddNode( Name: String ): TP3DShaderNode;
  begin
    Node:= P3DShaderNodeLib.FindNode( Name );
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
  Compiled:= nil;

  try
    if ( not Unlit ) then
      AddNode( 'lib_common' );

    AddNode( 'lib_maps' );

    AddNode( 'Pass_Init' );

    if ( not Unlit ) then
      AddNode( 'lib_lighting' );


    for i:= 0 to 7 do
      ChangeSocketValueInt( ChangeSocketValueInt( AddNode( 'TC_Init' ), 'Location', i + 8 ), 'TexCoordIndex', i );

    for i:= 0 to Maps.Count - 1 do
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
    if ( Unlit ) then
      AddNode( 'Unlit' )
    else
      AddNode( 'Lighting' );
    AddNode( 'Pass_Final' );
    //AddNode( 'lib_test' );


    Compiled:= ShaderTree.Compile;

    if ( Assigned( Shader )) then
      Shader.Free;
    Shader:= TP3DShader.CreateFromVertexAndFragmentShader( Compiled.FindBuffer( 'vshader' ).Code, Compiled.FindBuffer( 'fshader' ).Code, Parent );
  finally
    ShaderTree.Free;
    if ( Assigned( Compiled )) then
      Compiled.Free;
  end;
end;

procedure TP3DMaterial.PassToShader;
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
            Diff.R, Diff.G, Diff.B, 1.0 ); cle( Self );
        end;

      glUniform4f( Shader.Uniforms.AddrByName( 'mat_specular' ), Spec.r, Spec.g, Spec.b, 1 ); cle( Self );
      glUniform1f( Shader.Uniforms.AddrByName( 'mat_hardness' ), Spec_Hardness ); cle( Self );

      for i:= 0 to Maps.Count - 1 do
        begin
          glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
          //glEnable( GL_TEXTURE_2D ); cle( Self );
          glBindTexture( GL_TEXTURE_2D, Maps[ i ].Map.GLTexture ); cle( Self );
          glUniform1i( Shader.Uniforms.AddrByName( 'tex' + IntToStr( i )), i ); cle( Self );
        end;
    end;
end;

constructor TP3DMaterial.Create(const AData: TP3DData);
begin
  inherited;
  Maps:= TP3DMaterialMapList.Create;
end;

destructor TP3DMaterial.Destroy;
begin
  Maps.Free;
  if ( Assigned( Shader )) then
    Shader.Free;
  inherited Destroy;
end;

procedure TP3DMaterial.LoadFromDOM(ADOMNode: TDOMElement);
var
  SpecTmp: TVec4;
  tex: TDOMElement;

  procedure LoadTex;
  var
    Map: TP3DMaterialMap;
  begin
    if ( Maps.Count > 7 ) then
      raise Exception.Create( 'Maximum Number of Maps reached: 8' );
    Map:= TP3DMaterialMap.CreateFromDOM( Self, tex, Parent );

    Maps.Add( Map );
  end;

begin
  Name:= ADOMNode.GetAttribute( 'name' );
  Diff.FromString( ADOMNode.GetAttribute( 'diffuse' ));
  SpecTmp.FromString( ADOMNode.GetAttribute( 'specular' ));
  Spec:= SpecTmp.YZW;
  Spec_Hardness:= SpecTmp.X;
  tex:= TDOMElement( ADOMNode.FirstChild );
  if ( ADOMNode.GetAttribute( 'specular' ) = 'yes' ) then
    FUnlit:= True;
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
