{ p3d.base.plot.inc }

{$IFDEF INTERFACE}
type

  { TP3DPlotCommandCamera }

  TP3DPlotCommandCamera = class ( TP3DPlotCommandSetting )
    private
      FCamera: TP3DCamera;

    public
      constructor Create( ACamera: TP3DCamera );

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      procedure GetExtensions(Extension: TStringList); override;
      function Clone: TP3DPlotCommandSetting; override;

      property Camera: TP3DCamera read FCamera write FCamera;
  end;

  { TP3DPlotCommandLight }

  TP3DPlotCommandLight = class ( TP3DPlotCommandSetting )
    private
      FIndex: Integer;
      FLight: TP3DObject;

    public
      constructor Create( AIndex: Integer; ALightObj: TP3DObject );

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandSetting; override;

      property Light: TP3DObject read FLight write FLight;
      property Index: Integer read FIndex write FIndex;
  end;


  { TP3DPlotCommandUniformLight }

  TP3DPlotCommandUniformLight = class ( specialize TP3DPlotCommandShaderDecl < TP3DObject >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;


  { TP3DPlotCommandMaterial }

  {TP3DPlotCommandMaterial = class ( TP3DPlotCommandSettingShader )
    private
      FMaterial: TP3DMaterial;

    public
      constructor Create( AMaterial: TP3DMaterial );

      property Material: TP3DMaterial read FMaterial write FMaterial;
      procedure Execute( AContext: TP3DPlotCommandContext ); override;

      procedure BuildShader(AOwner: TP3DStreamableContainer); override;

      function Clone: TP3DPlotCommandSetting; override;
  end;}

  { TP3DPlotCommandScene }

  TP3DPlotCommandScene = class ( TP3DPlotCommandLayer )
    private
      FScene: TP3DScene;

    public
      constructor Create( AScene: TP3DScene );

      function Debug(LevelIndent: Integer): String; override;
      property Scene: TP3DScene read FScene write FScene;
      procedure Execute( AContext: TP3DPlotCommandContext ); override;
  end;

  { TP3DPlotCommandSDF }

  TP3DPlotCommandSDF = class ( TP3DPlotCommandSettingShader )
    private
      FBaseAlpha: Float;
      FBias: Float;
      FColor: TP3DPlotCommandSetting;
      FSmoothing: Float;
      FUV: TP3DPlotCommandSetting;

    public
      constructor Create( const Color: TP3DPlotCommandSetting = nil; const UV: TP3DPlotCommandSetting = nil; const sdf_bias: Float = -0.12; const sdf_smoothing: Float = -0.004; const sdf_basealpha: Float = -1.0 );

      procedure MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder); override;

      property Bias: Float read FBias write FBias;
      property Smoothing: Float read FSmoothing write FSmoothing;
      property BaseAlpha: Float read FBaseAlpha write FBaseAlpha;
      property Color: TP3DPlotCommandSetting read FColor write FColor;
      property UV: TP3DPlotCommandSetting read FUV write FUV;
  end;

  { TP3DPlotCommandMaterialPBR }

  TP3DPlotCommandMaterialPBR = class ( TP3DPlotCommandSetting )
    public
      constructor Create();

      procedure MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder); override;
      procedure Execute(AContext: TP3DPlotCommandContext); override;
  end;





{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DPlotCommandLight }

constructor TP3DPlotCommandLight.Create(AIndex: Integer; ALightObj: TP3DObject);
begin
  inherited Create;
  FIndex:= AIndex;
  FLight:= ALightObj;
end;

procedure TP3DPlotCommandLight.Execute(AContext: TP3DPlotCommandContext);
var
  l: String;
  pos: TVec4;
begin
  if ( Assigned( Light ) and ( Light.Data is TP3DLight )) then
    with ( TP3DLight( Light.Data )) do begin
      l:= 'LightSource[' + IntToStr( Index ) + ']';

      //Color
      glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.color' ), Color.R, Color.G, Color.B, Energy );

      //linear Attenuation
      glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.linearAttenuation' ), LinearAttenuation );

      //quadratic Attenuation
      glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.quadraticAttenuation' ), QuadraticAttenuation );

      //range
      //glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.range' ), GetLightRange );

      //type
      glUniform1i( P3DShaderActive.Uniforms.AddrByName( l + '.type' ), Ord( LightType ));
      //position in viewspace
      //pos:= matView * vec4( Items[ i ].Positions[ j ], 1 );
      //glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.position' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
      pos:= vec4( Light.Transform.Position, 1 );
      glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.worldposition' ), pos.X, pos.Y, pos.Z, pos.W );
      //pos:= vec4( normalize( mat3( matView ) * Items[ i ].Matrices[ j ].Row[ 2 ].XYZ ), 0 );
      //glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.direction' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
      pos:= vec4( normalize( Light.Transform.Matrix.Row[ 2 ].XYZ ), 0 );
      glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.worlddirection' ), pos.X, pos.Y, pos.Z, pos.W );
      pos:= normalize( -pos + vec4( 0, 0, 1, 0 ));
      glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.halfvector' ), pos.X, pos.Y, pos.Z, pos.W );
//      glUniformMatrix4fv( P3DShaderActive.Uniforms.AddrByName( 'LightMat' + IntToStr( Index )), 1, False, @FLightMatrix );
    end;
end;

function TP3DPlotCommandLight.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandLight.Create( Index, Light );
end;

{ TP3DPlotCommandCamera }

constructor TP3DPlotCommandCamera.Create(ACamera: TP3DCamera);
begin
  inherited Create;
  FCamera:= ACamera;
end;

procedure TP3DPlotCommandCamera.Execute(AContext: TP3DPlotCommandContext);
begin
  if ( Assigned( FCamera )) then
    FCamera.PassToShader;
end;

procedure TP3DPlotCommandCamera.GetExtensions(Extension: TStringList);
begin
  inherited GetExtensions(Extension);
  Extension.Add( 'camera' );
end;

function TP3DPlotCommandCamera.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandCamera.Create( FCamera );
end;

{ TP3DPlotCommandMaterial }
{
constructor TP3DPlotCommandMaterial.Create(AMaterial: TP3DMaterial);
begin
  inherited Create;
  if ( not Assigned( AMaterial )) then
    raise Exception.Create( 'Can''t create material plot command with null material.' );
  FMaterial:= AMaterial;
end;

procedure TP3DPlotCommandMaterial.Execute(AContext: TP3DPlotCommandContext );
begin
  if ( Assigned( Material )) then begin
//    AContext.Shader:= Material.Shader;
//    AContext.Shader.Enable;
    Material.PassToShader();
    if ( Assigned( AContext.LayerDefault )) then
      AContext.LayerDefault.Execute( AContext );
  end;
end;

procedure TP3DPlotCommandMaterial.BuildShader(AOwner: TP3DStreamableContainer);
begin
  Material.BuildShader( AOwner );
end;

function TP3DPlotCommandMaterial.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandMaterial.Create( FMaterial );
end;}

{ TP3DPlotCommandMaterialPBR }

constructor TP3DPlotCommandMaterialPBR.Create();
begin
  inherited Create;
  FCategory:= 'PBR';
end;

procedure TP3DPlotCommandMaterialPBR.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
var
  setting: TP3DPlotCommandSetting;
  Outp: TP3DNodeSocketClone;
  mat: TP3DNodeClone;
begin
  inherited MakeShaderModifications(ASender, ASenderIdx, ABuilder);
  mat:= ABuilder.Uniforms.AddCopy( 'material_pbr' );
  with ( mat ) do begin
    FindInput( 'Albedo' ).Connected:= ABuilder.RootNode.FindInput( 'Color' ).Connected;
    FindOutput( 'Color' ).Connected:= ABuilder.RootNode.FindInput( 'Color' );

    setting:= ASender.Items[ ASender.FindLast( ASenderIdx, 'pbr_roughness' )];
    if (( setting is IP3DPlotCommandSettingNode ) and
        ( Assigned(( setting as IP3DPlotCommandSettingNode ).ShaderNode ))) then begin
      Outp:= ( setting as IP3DPlotCommandSettingNode ).ShaderNode.FindOutputFirst([ 'Value', 'Color' ]);
      FindInput( 'Roughness' ).Connected:= outp;
    end;

    setting:= ASender.Items[ ASender.FindLast( ASenderIdx, 'pbr_metalness' )];
    if (( setting is IP3DPlotCommandSettingNode ) and
        ( Assigned(( setting as IP3DPlotCommandSettingNode ).ShaderNode ))) then begin
      FindInput( 'Metalness' ).Connected:= ( setting as IP3DPlotCommandSettingNode ).ShaderNode.FindOutputFirst([ 'Value', 'Color' ]);
    end;

    setting:= ASender.Items[ ASender.FindLast( ASenderIdx, 'pbr_occlusion' )];
    if (( setting is IP3DPlotCommandSettingNode ) and
        ( Assigned(( setting as IP3DPlotCommandSettingNode ).ShaderNode ))) then begin
      FindInput( 'Occlusion' ).Connected:= ( setting as IP3DPlotCommandSettingNode ).ShaderNode.FindOutputFirst([ 'Value', 'Color' ]);
    end;

    setting:= ASender.Items[ ASender.FindLast( ASenderIdx, 'normal_map' )];
    if (( setting is IP3DPlotCommandSettingNode ) and
        ( Assigned(( setting as IP3DPlotCommandSettingNode ).ShaderNode ))) then begin
      Outp:= ( setting as IP3DPlotCommandSettingNode ).ShaderNode.FindOutput( 'Normal' );
      if ( Assigned( Outp )) then
        //mat.FindInput( 'WorldNormal' ).Connected:= Outp;
        with ( ABuilder.Uniforms.AddCopy( 'perturb_normal_world' )) do begin
          FindInput( 'Normal' ).Connected:= Outp;
          mat.FindInput( 'WorldNormal' ).Connected:= FindOutput( 'Normal' );
        end;
    end;

  end;
end;

procedure TP3DPlotCommandMaterialPBR.Execute(AContext: TP3DPlotCommandContext);
begin

end;

{ TP3DPlotCommandSDF }

constructor TP3DPlotCommandSDF.Create(const Color: TP3DPlotCommandSetting; const UV: TP3DPlotCommandSetting; const sdf_bias: Float;
  const sdf_smoothing: Float; const sdf_basealpha: Float);
begin
  inherited Create;
  Bias:= sdf_bias;
  Smoothing:= sdf_smoothing;
  BaseAlpha:= sdf_basealpha;
end;

procedure TP3DPlotCommandSDF.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
begin
  with ( ABuilder.Uniforms.AddCopy( 'signed_distance_field' )) do begin
    {FindInput( 'uv' ).ValueProp.AsString:= Name;
    texture2D:= ABuilder.Uniforms.AddCopy( 'texture2D' );
    texture2D.FindInput( 'Map' ).Connected:= FindOutput( 'Value' );

    with ( ABuilder.Uniforms.AddCopy( 'texcoord' )) do begin
      texture2D.FindInput( 'UV' ).Connected:= FindOutput( 'UV' );
    end;

    texture2D.FindOutput( 'Color' ).Connected:= ABuilder.RootNode.FindInput( 'Color' );}
  end;
end;

{ TP3DPlotCommandScene }

constructor TP3DPlotCommandScene.Create(AScene: TP3DScene);
begin
  inherited Create;
  FScene:= AScene;
end;

function TP3DPlotCommandScene.Debug(LevelIndent: Integer): String;
begin
  Result:= inherited Debug( LevelIndent ) + FScene.FPlotList.Debug( LevelIndent + 1 );
end;

procedure TP3DPlotCommandScene.Execute(AContext: TP3DPlotCommandContext);
begin
  //inherited Execute(AContext);
  //AContext.LayerDefault;
  if ( Assigned( FScene )) then
    FScene.FPlotList.Execute( AContext );
end;

{ TP3DPlotCommandUniformLight }

procedure TP3DPlotCommandUniformLight.Execute(AContext: TP3DPlotCommandContext);
var
  l: String;
  pos: TVec4;
  Light: TP3DLight;
  Index: Integer;
begin
  if ( not Assigned( Value ) or ( not ( Value.Data is TP3DLight )) or not Assigned( P3DShaderActive )) then
    exit;
  Light:= TP3DLight( Value.Data );
  case Light.LightType of
    ltPoint: begin
      Index:= AContext.NumPointLights;
      l:= 'PointLights[' + IntToStr( Index ) + ']';

      //Color
      glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.Color' ), Light.Color.R, Light.Color.G, Light.Color.B, Light.Energy );

      //linear Attenuation
      glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.linearAttenuation' ), Light.LinearAttenuation );

      //quadratic Attenuation
      glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.quadraticAttenuation' ), Light.QuadraticAttenuation );

      //position in view space
      //pos:= matView * vec4( Value.Transform.Position, 1 );
      //glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.Position' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );

      //position in world space
      pos:= vec4( Value.Transform.Position, 1 );
      glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.WorldPosition' ), pos.X, pos.Y, pos.Z, pos.W );
      Inc( AContext.FNumPointLights );
      glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'numPointLights' ), AContext.NumPointLights );
    end;
    ltSun: begin
      Index:= AContext.NumDirectionalLights;
      l:= 'DirectionalLights[' + IntToStr( Index ) + ']';

      //Color
      glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.Color' ), Light.Color.R, Light.Color.G, Light.Color.B, Light.Energy );

      //pos:= vec4( normalize( mat3( matView ) * Items[ i ].Matrices[ j ].Row[ 2 ].XYZ ), 0 );
      //glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.Direction' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );

      pos:= vec4( normalize( Value.Transform.Matrix.Row[ 2 ].XYZ ), 0 );
      glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.WorldDirection' ), pos.X, pos.Y, pos.Z, pos.W );
      Inc( AContext.FNumDirectionalLights );
      glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'numDirectionalLights' ), AContext.NumDirectionalLights );
    end;
  end;
end;

function TP3DPlotCommandUniformLight.Clone: TP3DPlotCommandSetting;
begin

end;

{$ENDIF}

