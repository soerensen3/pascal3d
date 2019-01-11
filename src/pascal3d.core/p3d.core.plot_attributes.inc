{ p3dplot_attributes.inc }

{$IFDEF INTERFACE}
type
  TVec2Array = array of TVec2;
  TVec3Array = array of TVec3;
  TVec4Array = array of TVec4;

  { TP3DPlotCommandAttribFloat }

  TP3DPlotCommandAttribFloat = class ( specialize TP3DPlotCommandShaderDecl < Float >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandAttribVec2 }

  TP3DPlotCommandAttribVec2 = class ( specialize TP3DPlotCommandShaderDecl < TVec2 >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandAttribVec3 }

  TP3DPlotCommandAttribVec3 = class ( specialize TP3DPlotCommandShaderDecl < TVec3 >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandAttribVec4 }

  TP3DPlotCommandAttribVec4 = class ( specialize TP3DPlotCommandShaderDecl < TVec4 >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandAttribArray }

  generic TP3DPlotCommandAttribArray < T; VecArray > = class ( TP3DPlotCommandSetting )
    private
      FLocation: GLint;
      FName: String;
      FPoints: T;

    public
      constructor CreateArray( ALocation: GLint; APoints: VecArray );
      constructor Create( ALocation: GLint; APoints: T );
      constructor CreateArray( AName: String; APoints: VecArray );
      constructor Create( AName: String; APoints: T );
      destructor Destroy; override;

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandSetting; override;

      property Points: T read FPoints write FPoints;
      property Location: GLint read FLocation write FLocation;
      property Name: String read FName write FName;
  end;

  TP3DPlotCommandAttribVec2Array = specialize TP3DPlotCommandAttribArray < TP3DVec2BufferGLAuto, TVec2Array >;
  TP3DPlotCommandAttribVec3Array = specialize TP3DPlotCommandAttribArray < TP3DVec3BufferGLAuto, TVec3Array >;
  TP3DPlotCommandAttribVec4Array = specialize TP3DPlotCommandAttribArray < TP3DVec4BufferGLAuto, TVec4Array >;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DPlotCommandAttribFloat }

procedure TP3DPlotCommandAttribFloat.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glDisableVertexAttribArray( FLocation );
  glVertexAttrib1f( FLocation, FValue );
end;

function TP3DPlotCommandAttribFloat.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribFloat.Create( FLocation, FValue );
end;


{ TP3DPlotCommandAttribVec2 }

procedure TP3DPlotCommandAttribVec2.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glDisableVertexAttribArray( FLocation );
  glVertexAttrib2f( FLocation, FValue.X, FValue.Y );
end;

function TP3DPlotCommandAttribVec2.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec2.Create( FLocation, FValue );
end;

{ TP3DPlotCommandAttribVec3 }

procedure TP3DPlotCommandAttribVec3.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glDisableVertexAttribArray( FLocation );
  glVertexAttrib3f( FLocation, FValue.X, FValue.Y, FValue.Z );
end;

function TP3DPlotCommandAttribVec3.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec3.Create( FLocation, FValue );
end;

{ TP3DPlotCommandAttribVec4 }

procedure TP3DPlotCommandAttribVec4.Execute(AContext: TP3DPlotCommandContext );
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  glDisableVertexAttribArray( FLocation );
  glVertexAttrib4f( FLocation, FValue.X, FValue.Y, FValue.Z, FValue.W );
end;

function TP3DPlotCommandAttribVec4.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandAttribVec4.Create( FLocation, FValue );
end;

constructor TP3DPlotCommandAttribArray.CreateArray(ALocation: GLint;
  APoints: VecArray);
begin
  inherited Create;
  FPoints:= T.TAutoType.Create;
  FPoints.I.AddArray( APoints );
  FPoints.I.PushData;
  FLocation:= ALocation;
end;

constructor TP3DPlotCommandAttribArray.Create(ALocation: GLint; APoints: T);
begin
  inherited Create;
  FPoints:= APoints;
  FLocation:= ALocation;
end;

constructor TP3DPlotCommandAttribArray.CreateArray(AName: String; APoints: VecArray);
begin
  inherited Create;
  FPoints:= T.TAutoType.Create;
  FPoints.I.AddArray( APoints );
  FPoints.I.PushData;
  FLocation:= -1;
  FName:= AName;
end;

constructor TP3DPlotCommandAttribArray.Create(AName: String; APoints: T);
begin
  inherited Create;
  FPoints:= APoints;
  FLocation:= -1;
  FName:= AName;
end;

destructor TP3DPlotCommandAttribArray.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DPlotCommandAttribArray.Execute(AContext: TP3DPlotCommandContext);
var
  decl: TP3DShaderDecl;
begin
  if ( Location < 0 ) then
    if ( Assigned( AContext.Shader ) and AContext.Shader.Attributes.FindByName( Name, decl )) then
      FLocation:= decl.Addr
    else
      exit;
  FPoints.I.SetAttribArray( FLocation );
end;

function TP3DPlotCommandAttribArray.Clone: TP3DPlotCommandSetting;
begin
  if ( FLocation < 0 ) then
    Result:= ( specialize TP3DPlotCommandAttribArray< T, VecArray >).Create( FName, FPoints )
  else
    Result:= ( specialize TP3DPlotCommandAttribArray< T, VecArray >).Create( FLocation, FPoints );
end;


{$ENDIF}

