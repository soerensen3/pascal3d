{ p3d.base.plot_constructors.inc }

{$IFDEF INTERFACE}
{Material PBR}
function uniform_roughness( AName: String; AValue: Float ): TP3DPlotCommandSetting;
function sample_roughness( AName: String; AValue: TP3DTexture ): TP3DPlotCommandSetting;
function uniform_metalness( AName: String; AValue: Float ): TP3DPlotCommandSetting;
function uniform_occlusion( AName: String; AValue: Float ): TP3DPlotCommandSetting;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
function uniform_roughness( AName: String; AValue: Float ): TP3DPlotCommandSetting;
begin
  Result:= uniform( AName, AValue );
  Result.Category:= 'pbr_roughness';
end;

function sample_roughness(AName: String; AValue: TP3DTexture): TP3DPlotCommandSetting;
begin
  Result:= sample( AName, AValue );
  TP3DPlotCommandUniformSampleTexture( Result ).NoConnectToColor:= True;
  Result.Category:= 'pbr_roughness';
end;

function uniform_metalness( AName: String; AValue: Float ): TP3DPlotCommandSetting;
begin
  Result:= uniform( AName, AValue );
  Result.Category:= 'pbr_metalness';
end;

function uniform_occlusion( AName: String; AValue: Float ): TP3DPlotCommandSetting;
begin
  Result:= uniform( AName, AValue );
  Result.Category:= 'pbr_occlusion';
end;
{$ENDIF}

