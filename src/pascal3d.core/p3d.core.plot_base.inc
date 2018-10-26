{ p3dplot_base.inc }
{$IFDEF INTERFACE}
type

  { TP3DPlotCommandShaderDecl }

  generic TP3DPlotCommandShaderDecl < T > = class ( TP3DPlotCommandSetting )
    private
      FLocation: GLint;
      FName: String;
      FValue: T;

    public
      constructor Create( ALocation: GLint; AValue: T );
      constructor Create( AName: String; AValue: T );

      property Value: T read FValue write FValue;
      property Location: GLint read FLocation write FLocation;
      property Name: String read FName write FName;
  end;

  TP3DPlotCommandClearFlag = (
    cfDepth,
    cfColor );

  TP3DPlotCommandClearFlags = set of TP3DPlotCommandClearFlag;

  { TP3DPlotCommandClear }

  TP3DPlotCommandClear = class ( TP3DPlotCommandLayer )
    private
      FClearColor: TP3DColorRGBA;
      FClearFlags: TP3DPlotCommandClearFlags;

    public
      constructor Create( AClearFlags: TP3DPlotCommandClearFlags; AClearColor: TP3DColorRGBA );
      procedure Execute(AContext: TP3DPlotCommandContext); override;
      function Clone: TP3DPlotCommandLayer; override;

      property ClearColor: TP3DColorRGBA read FClearColor write FClearColor;
      property ClearFlags: TP3DPlotCommandClearFlags read FClearFlags write FClearFlags;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DPlotCommandClear }

constructor TP3DPlotCommandClear.Create(AClearFlags: TP3DPlotCommandClearFlags; AClearColor: TP3DColorRGBA);
begin
  inherited Create;
  ClearFlags:= AClearFlags;
  ClearColor:= AClearColor;
end;

procedure TP3DPlotCommandClear.Execute(AContext: TP3DPlotCommandContext);
var
  Mask: GLuint;
begin
  Mask:= 0;
  glClearColor( ClearColor.R, ClearColor.G, ClearColor.B, ClearColor.A );
  if ( cfColor in ClearFlags ) then
    Mask:= GL_COLOR_BUFFER_BIT;
  if ( cfDepth in ClearFlags ) then
    Mask:= Mask or GL_DEPTH_BUFFER_BIT;
  glClear( Mask ); //cle( Self );
end;

function TP3DPlotCommandClear.Clone: TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandClear.Create( ClearFlags, ClearColor );
end;

{ TP3DPlotCommandRestoreFB }

procedure TP3DPlotCommandRestoreFB.Execute(AContext: TP3DPlotCommandContext);
begin
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
  P3DViewports.Pop;
end;

function TP3DPlotCommandRestoreFB.Clone: TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandRestoreFB.Create;
end;

{ TP3DPlotCommandRenderToTexture }

constructor TP3DPlotCommandRenderToTexture.Create(ATexture: TP3DTexture; ALayers: TP3DPlotListLayer);
begin
  inherited Create;
  FFrameBufferProp:= TP3DStreamableContainer.Create( 'FrameBuffer', TP3DFrameBuffer );
  FFrameBufferProp.Root:= P3DData;
  FTexture:= ATexture;
  FLayers:= ALayers;
end;

destructor TP3DPlotCommandRenderToTexture.Destroy;
begin
  FFrameBufferProp.Free;
  FLayers.CheckDestroy;
  inherited Destroy;
end;

procedure TP3DPlotCommandRenderToTexture.Execute(AContext: TP3DPlotCommandContext);
begin
  Texture.Unbind();
  if ( not Assigned( FFrameBuffer )) then begin
    FFrameBuffer:= TP3DFrameBuffer.Create( FFrameBufferProp );
    FFrameBuffer.AutoViewPort:= True;
    FFrameBuffer.AttachColor( FTexture );
    FFrameBuffer.AttachDepth( Texture.Width, Texture.Height );
  end;
  FFrameBuffer.BindAll();
  FLayers.Execute( AContext );
  FFrameBuffer.Unbind();
  Texture.UpdateMipMaps();
end;

function TP3DPlotCommandRenderToTexture.Clone: TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandRenderToTexture.Create( Texture, FLayers );
end;




{ TP3DPlotCommandShaderDecl }

constructor TP3DPlotCommandShaderDecl.Create(ALocation: GLint; AValue: T);
begin
  inherited Create;
  FValue:= AValue;
  FLocation:= ALocation;
end;

constructor TP3DPlotCommandShaderDecl.Create(AName: String; AValue: T);
begin
  inherited Create;
  FValue:= AValue;
  FName:= AName;
  FLocation:= -1;
end;

{$ENDIF}

