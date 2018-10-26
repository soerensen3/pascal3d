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

  { TP3DPlotCommandRenderToTexture}

  TP3DPlotCommandRenderToTexture = class ( TP3DPlotCommandLayer )
    private
      FFrameBuffer: TP3DFrameBuffer;
      FFrameBufferProp: TP3DStreamableContainer;
      FLayers: TP3DPlotListLayer;
      FTexture: TP3DTexture;

    public
      constructor Create( ATexture: TP3DTexture; ALayers: TP3DPlotListLayer );
      destructor Destroy; override;

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandLayer; override;

      property Texture: TP3DTexture read FTexture;
      property Layers: TP3DPlotListLayer read FLayers;
  end;

  { TP3DPlotCommandRestoreFB }

  TP3DPlotCommandRestoreFB = class ( TP3DPlotCommandLayer )
    public
      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandLayer; override;
  end;

  { TP3DPlotCommandUniformTexture }

  TP3DPlotCommandUniformTexture = class ( specialize TP3DPlotCommandShaderDecl < TP3DTexture >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandUniformLight }

  TP3DPlotCommandUniformLight = class ( specialize TP3DPlotCommandShaderDecl < TP3DObject >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
    function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandUniformTransform }

  TP3DPlotCommandUniformTransform = class ( specialize TP3DPlotCommandShaderDecl < TP3DTransform >)
    procedure Execute( AContext: TP3DPlotCommandContext ); override;
  end;

  { TP3DPlotCommandMaterial }

  TP3DPlotCommandMaterial = class ( TP3DPlotCommandSetting )
    private
      FMaterial: TP3DMaterial;

    public
      constructor Create( AMaterial: TP3DMaterial );

      property Material: TP3DMaterial read FMaterial write FMaterial;
      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandSetting; override;
  end;

  { TP3DPlotCommandScene }

  TP3DPlotCommandScene = class ( TP3DPlotCommandLayer )
    private
      FScene: TP3DScene;

    public
      constructor Create( AScene: TP3DScene );

      property Scene: TP3DScene read FScene write FScene;
      procedure Execute( AContext: TP3DPlotCommandContext ); override;
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

function TP3DPlotCommandCamera.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandCamera.Create( FCamera );
end;

{ TP3DPlotCommandMaterial }

constructor TP3DPlotCommandMaterial.Create(AMaterial: TP3DMaterial);
begin
  inherited Create;
  FMaterial:= AMaterial;
end;

procedure TP3DPlotCommandMaterial.Execute(AContext: TP3DPlotCommandContext );
begin
  if ( Assigned( Material )) then begin
    AContext.Shader:= Material.Shader;
    AContext.Shader.Enable;
    Material.PassToShader();
    if ( Assigned( AContext.LayerDefault )) then
      AContext.LayerDefault.Execute( AContext );
  end;
end;

function TP3DPlotCommandMaterial.Clone: TP3DPlotCommandSetting;
begin
  Result:= TP3DPlotCommandMaterial.Create( FMaterial );
end;
{$ENDIF}

