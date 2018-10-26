{ p3dplot_advanced.inc }

{$IFDEF INTERFACE}
type

  { TP3DPlotCommandWorld }

  TP3DPlotCommandWorld = class ( TP3DPlotCommandSetting )
    private
      FWorld: TMat4;

    public
      constructor Create( AWorld: TMat4 );

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandSetting; override;

      property World: TMat4 read FWorld write FWorld;
  end;


  { TP3DPlotCommandView }

  TP3DPlotCommandView = class ( TP3DPlotCommandSetting )
    private
      FView: TMat4;

    public
      constructor Create( AView: TMat4 );

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandSetting; override;

      property View: TMat4 read FView write FView;
  end;

  { TP3DPlotCommandProj }

  TP3DPlotCommandProj = class ( TP3DPlotCommandSetting )
    private
      FProj: TMat4;

    public
      constructor Create( AProj: TMat4 );

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandSetting; override;

      property Proj: TMat4 read FProj write FProj;
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}


{ TP3DPlotCommandWorld }

constructor TP3DPlotCommandWorld.Create(AWorld: TMat4);
begin
  inherited Create;
  FWorld:= AWorld;
end;

procedure TP3DPlotCommandWorld.Execute(AContext: TP3DPlotCommandContext);
var
  mworld: TP3DShaderDecl;
begin
  if ( Assigned( AContext.Shader ) and AContext.Shader.Uniforms.FindByName( 'world', mworld )) then
    mworld.AsMat4:= World;
end;

function TP3DPlotCommandWorld.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandWorld.Create( FWorld );
end;

{ TP3DPlotCommandProj }

constructor TP3DPlotCommandProj.Create(AProj: TMat4);
begin
  inherited Create;
  FProj:= AProj;
end;

procedure TP3DPlotCommandProj.Execute(AContext: TP3DPlotCommandContext);
var
  mproj: TP3DShaderDecl;
begin
  if ( Assigned( AContext.Shader ) and AContext.Shader.Uniforms.FindByName( 'proj', mproj )) then
    mproj.AsMat4:= Proj;
end;

function TP3DPlotCommandProj.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandProj.Create( FProj );
end;

{ TP3DPlotCommandView }

constructor TP3DPlotCommandView.Create(AView: TMat4);
begin
  inherited Create;
  FView:= AView;
end;

procedure TP3DPlotCommandView.Execute(AContext: TP3DPlotCommandContext);
var
  mview: TP3DShaderDecl;
begin
  if ( Assigned( AContext.Shader ) and AContext.Shader.Uniforms.FindByName( 'view', mview )) then
    mview.AsMat4:= View;
end;

function TP3DPlotCommandView.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandView.Create( FView );
end;

{$ENDIF}

