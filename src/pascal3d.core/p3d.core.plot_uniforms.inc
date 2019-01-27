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
    procedure MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder); override;
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

  { TP3DPlotCommandUniformTransform }

  TP3DPlotCommandUniformTransform = class ( specialize TP3DPlotCommandShaderDecl < TP3DTransform >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
  end;

  { TP3DPlotCommandUniformTransformView }

  TP3DPlotCommandUniformTransformView = class ( TP3DPlotCommandUniformTransform )
    public
      constructor Create( AValue: TP3DTransform );

      procedure GetExtensions(Extension: TStringList); override;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}



{ TP3DPlotCommandUniformFloat }

procedure TP3DPlotCommandUniformFloat.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Uniforms.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glUniform1f( FLocation, FValue );
end;

procedure TP3DPlotCommandUniformFloat.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer; ABuilder: TP3DShaderBuilder);
begin
  inherited MakeShaderModifications(ASender, ASenderIdx, ABuilder);
  FShaderNode:= ABuilder.NodeTree.AddCopy( 'uniform_float' );
  FShaderNode.Inputs.GetByName( 'UnName' ).ValueProp.AsString:= Name;
  FShaderNode.Inputs.GetByName( 'Value' ).ValueProp.AsString:= FloatToStr( Value );
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
    if ( AContext.Shader.Uniforms.FindByName( Name, decl )) then
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
    if ( AContext.Shader.Uniforms.FindByName( Name, decl )) then
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
    if ( AContext.Shader.Uniforms.FindByName( Name, decl )) then
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
    if ( AContext.Shader.Uniforms.FindByName( Name, decl )) then
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
    if ( AContext.Shader.Uniforms.FindByName( Name, decl )) then
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
    if ( AContext.Shader.Uniforms.FindByName( Name, decl )) then
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
    if ( AContext.Shader.Uniforms.FindByName( Name, decl )) then
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
  Inc( AContext.FNumTextures );
  FValue.Bind( TexIndex );
  glUniform1i( FLocation, TexIndex );
end;

function TP3DPlotCommandUniformTexture.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandUniformTexture.Create( FLocation, FValue );
end;

function TP3DPlotCommandUniformSampleTexture.GetShaderNode: TP3DNodeClone;
begin
  Result:= FShaderNode;
end;

procedure TP3DPlotCommandUniformSampleTexture.MakeShaderModifications(ASender: TP3DPlotListSetting; ASenderIdx: Integer;
  ABuilder: TP3DShaderBuilder);
var
  setting: TP3DPlotCommandSetting;
  Outp: TP3DNodeSocketClone;
begin
  if ( Name > '' ) then begin
    with ( ABuilder.Uniforms.AddCopy( 'uniform_sampler2D' )) do begin
      FindInput( 'UnName' ).ValueProp.AsString:= Name;
      FShaderNode:= ABuilder.Uniforms.AddCopy( 'texture2D' );
      FShaderNode.FindInput( 'Map' ).Connected:= FindOutput( 'Value' );
      setting:= ASender.Items[ ASender.FindLast( ASenderIdx, 'texcoord' )];
      if (( setting is IP3DPlotCommandSettingNode ) and
        ( Assigned(( setting as IP3DPlotCommandSettingNode ).ShaderNode ))) then begin
        Outp:= ( setting as IP3DPlotCommandSettingNode ).ShaderNode.FindOutputFirst([ 'UV', 'Value', 'Vector' ]);

        if ( Assigned( Outp )) then
          FShaderNode.FindInput( 'UV' ).Connected:= Outp;
      end;

      //with ( ABuilder.Uniforms.AddCopy( 'texcoord' )) do begin
      //  FShaderNode.FindInput( 'UV' ).Connected:= FindOutput( 'UV' );
      //end;

      if ( not NoConnectToColor ) then
        FShaderNode.FindOutput( 'Color' ).Connected:= ABuilder.RootNode.FindInput( 'Color' );
    end;
  end;
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


{$ENDIF}

