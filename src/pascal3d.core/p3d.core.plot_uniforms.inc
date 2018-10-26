{ p3dplot_uniforms.inc }

{$IFDEF INTERFACE}
type

  { TP3DPlotCommandUniformInt }

  TP3DPlotCommandUniformInt = class ( specialize TP3DPlotCommandShaderDecl < Integer >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandUniformFloat }

  TP3DPlotCommandUniformFloat = class ( specialize TP3DPlotCommandShaderDecl < Float >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandUniformVec2 }

  TP3DPlotCommandUniformVec2 = class ( specialize TP3DPlotCommandShaderDecl < TVec2 >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandUniformVec3 }

  TP3DPlotCommandUniformVec3 = class ( specialize TP3DPlotCommandShaderDecl < TVec3 >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandUniformVec4 }

  TP3DPlotCommandUniformVec4 = class ( specialize TP3DPlotCommandShaderDecl < TVec4 >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandUniformMat2 }

  TP3DPlotCommandUniformMat2 = class ( specialize TP3DPlotCommandShaderDecl < TMat2 >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandUniformMat3 }

  TP3DPlotCommandUniformMat3 = class ( specialize TP3DPlotCommandShaderDecl < TMat3 >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandUniformMat4 }

  TP3DPlotCommandUniformMat4 = class ( specialize TP3DPlotCommandShaderDecl < TMat4 >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}



{ TP3DPlotCommandUniformFloat }

procedure TP3DPlotCommandUniformFloat.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glUniform1f( FLocation, FValue );
end;

function TP3DPlotCommandUniformFloat.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformFloat.Create( FLocation, FValue );
end;

{ TP3DPlotCommandUniformInt }

procedure TP3DPlotCommandUniformInt.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glUniform1i( FLocation, FValue );
end;

function TP3DPlotCommandUniformInt.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformInt.Create( FLocation, FValue );
end;


{ TP3DPlotCommandUniformVec4 }

procedure TP3DPlotCommandUniformVec4.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glUniform4f( FLocation, FValue.X, FValue.Y, FValue.Z, FValue.W );
end;

function TP3DPlotCommandUniformVec4.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformVec4.Create( FLocation, FValue );
end;

{ TP3DPlotCommandUniformVec3 }

procedure TP3DPlotCommandUniformVec3.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glUniform3f( FLocation, FValue.X, FValue.Y, FValue.Z );
end;

function TP3DPlotCommandUniformVec3.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformVec3.Create( FLocation, FValue );
end;

{ TP3DPlotCommandUniformVec2 }

procedure TP3DPlotCommandUniformVec2.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glUniform2f( FLocation, FValue.X, FValue.Y );
end;

function TP3DPlotCommandUniformVec2.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformVec2.Create( FLocation, FValue );
end;

{ TP3DPlotCommandUniformMat2 }

procedure TP3DPlotCommandUniformMat2.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glUniformMatrix2fv( FLocation, 1, False, @FValue.m[ 0 ]);
end;

function TP3DPlotCommandUniformMat2.Clone: TP3DPlotCommandSetting;
begin

end;

{ TP3DPlotCommandUniformMat3 }

procedure TP3DPlotCommandUniformMat3.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glUniformMatrix3fv( FLocation, 1, False, @FValue.m[ 0 ]);
end;

function TP3DPlotCommandUniformMat3.Clone: TP3DPlotCommandSetting;
begin

end;

{ TP3DPlotCommandUniformMat4 }

procedure TP3DPlotCommandUniformMat4.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glUniformMatrix4fv( FLocation, 1, False, @FValue.m[ 0 ]);
end;

function TP3DPlotCommandUniformMat4.Clone: TP3DPlotCommandSetting;
begin

end;


{ TP3DPlotCommandUniformTexture }

procedure TP3DPlotCommandUniformTexture.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
  TexIndex: Integer;
begin
  if ( not Assigned( FValue )) then
    exit;
  if ( Location < 0 ) then
    if ( Assigned( AContext.Shader ) and AContext.Shader.Uniforms.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  TexIndex:= AContext.NumTextures;
  Inc( AContext.NumTextures );
  FValue.Bind( TexIndex );
  glUniform1i( FLocation, TexIndex );
end;

function TP3DPlotCommandUniformTexture.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformTexture.Create( FLocation, FValue );
end;

{ TP3DPlotCommandUniformTransform }

procedure TP3DPlotCommandUniformTransform.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
  TexIndex: Integer;
begin
  if ( not Assigned( FValue )) then
    exit;
  if ( Assigned( AContext.Shader )) then begin
    if ( Location < 0 ) then
      AContext.Shader.Uniforms.FindByName( Name, decl )
    else
      AContext.Shader.Uniforms.FindByAddr( Location, decl );
    if ( not Assigned( decl )) then
      exit;

    decl.AsMat4:= Value.Matrix;
  end;
end;

{ TP3DPlotCommandUniformLight }

procedure TP3DPlotCommandUniformLight.Execute(AContext: TP3DPlotCommandContext);
var
  l: String;
  pos: TVec4;
  Light: TP3DLight;
  Index: Integer;
begin
  if ( not Assigned( Value ) or ( not ( Value.Data is TP3DLight ))) then
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
      Inc( AContext.NumPointLights );
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
      Inc( AContext.NumDirectionalLights );
      glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'numDirectionalLights' ), AContext.NumDirectionalLights );
    end;
  end;
end;

function TP3DPlotCommandUniformLight.Clone: TP3DPlotCommandSetting;
begin

end;

{$ENDIF}

