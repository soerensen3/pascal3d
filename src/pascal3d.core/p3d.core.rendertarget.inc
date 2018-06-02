{$IFDEF INTERFACE}
type

  { TP3DRenderBuffer }

  TP3DRenderBuffer = class ( TPersistent )
    private
      FHeight: Integer;
      FRBO: GLint;
      FWidth: Integer;

    public
      constructor Create(AWidth, AHeight: Integer);
      destructor Destroy; override;

    published
      property RBO: GLint read FRBO;
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
  end;

  { TP3DFrameBuffer }

  TP3DFrameBuffer = class ( TPersistent )
    private
      FAutoViewPort: Boolean;
      FViewPortIdx: Integer;
      FTexCount: Byte;
      FFBO: GLint;
      FWidthHeight: TIVec2;

    public
      constructor Create();
      destructor Destroy(); override;

      procedure Bind( Index: Integer );
      procedure BindAll();
      procedure Clear();
      procedure Unbind();

      procedure CreateAndAttachMulti(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType;
        ADepth: Boolean; ANames: array of String; const AData: TP3DLibrary = nil);
      procedure AttachColor( Tex: TP3DTexture; const MipLevel: Integer = 0 );
      procedure AttachDepth( Tex: TP3DTexture );
      procedure AttachDepth( RB: TP3DRenderBuffer );
      procedure AttachStencil( Tex: TP3DTexture );
      procedure DetachAll();

      procedure CheckFBO;

      property FBO: GLint read FFBO;
      property TexCount: Byte read FTexCount;
      property AutoViewPort: Boolean read FAutoViewPort write FAutoViewPort;
      property WidthHeight: TIVec2 read FWidthHeight write FWidthHeight;
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DRenderBuffer }

constructor TP3DRenderBuffer.Create( AWidth, AHeight: Integer );
begin
  inherited Create;
  glGenRenderbuffers( 1, @FRBO );
  glBindRenderbuffer( GL_RENDERBUFFER, FRBO );
  glRenderbufferStorage( GL_RENDERBUFFER, GL_DEPTH_COMPONENT, AWidth, AHeight );
end;

destructor TP3DRenderBuffer.Destroy;
begin
  glDeleteRenderbuffers( 1, @FRBO );
  inherited Destroy;
end;


{ TP3DFrameBuffer }

constructor TP3DFrameBuffer.Create();
begin
  inherited Create;
  glGenFramebuffers( 1, @FBO );
  FTexCount:= 0;
  FAutoViewPort:= True;
  FViewPortIdx:= -1;
end;

destructor TP3DFrameBuffer.Destroy();
begin
  glDeleteFramebuffers( 1, @FFBO );
  inherited Destroy;
end;

procedure TP3DFrameBuffer.Bind(Index: Integer);
begin
  if (( AutoViewPort ) and ( WidthHeight.x * WidthHeight.y > 0 ) and ( FViewPortIdx = -1 )) then
    FViewPortIdx:= P3DViewports.Push( 0, 0, WidthHeight.x, WidthHeight.Y );
  glBindFramebuffer( GL_FRAMEBUFFER, FFBO ); cle( Self, 'Error while binding frame buffer: ' );
  glDrawBuffer( GL_COLOR_ATTACHMENT0 + Index ); cle( Self, 'Error while binding frame buffer attachments: ' );
end;

procedure TP3DFrameBuffer.BindAll();
var
  i: Integer;
  Buffers: array [ 0..255 ] of GLint;
begin
  if (( AutoViewPort ) and ( WidthHeight.x * WidthHeight.y > 0 ) and ( FViewPortIdx = -1 )) then
    FViewPortIdx:= P3DViewports.Push( 0, 0, WidthHeight.x, WidthHeight.Y );
  glBindFramebuffer( GL_FRAMEBUFFER, FFBO ); cle( Self, 'Error while binding frame buffer: ' );
  for i:= 0 to TexCount - 1 do
    Buffers[ i ]:= GL_COLOR_ATTACHMENT0 + i;
  glDrawBuffers( TexCount, @Buffers[ 0 ]); cle( Self, 'Error while binding frame buffer attachments: ' );
end;

procedure TP3DFrameBuffer.Clear();
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT ); cle( Self, 'Error while clearing frame buffer: ' );
end;

procedure TP3DFrameBuffer.Unbind();
begin
  if (( AutoViewPort ) and ( WidthHeight.x * WidthHeight.y > 0 ) and ( FViewPortIdx > -1 )) then begin
    P3DViewports.Pop;
    FViewPortIdx:= -1;
  end;
  glBindFramebuffer( GL_FRAMEBUFFER, 0 ); cle( Self, 'Error unbinding frame buffer: ' );
end;

procedure TP3DFrameBuffer.CreateAndAttachMulti(AWidth, AHeight: Integer; ANumChannels: Integer;
  ADataType: TP3DTextureDataType; ADepth: Boolean; ANames: array of String; const AData: TP3DLibrary);
var
  Name: String;
  Tx: TP3DTexture;
begin
  for Name in ANames do begin
    Tx:= TP3DTexture.CreateFromEmpty( AWidth, AHeight, ANumChannels, ADataType, AData );
    Tx.Name:= Name;
    AttachColor( Tx );
  end;
  if ( ADepth ) then
    AttachDepth( TP3DRenderBuffer.Create( AWidth, AHeight ));
end;

procedure TP3DFrameBuffer.AttachColor(Tex: TP3DTexture; const MipLevel: Integer);
begin
  if ( FTexCount = 0 ) then
    WidthHeight:= ivec2( Tex.Width, Tex.Height );
  glBindFramebuffer( GL_FRAMEBUFFER, FBO );
  glFramebufferTexture2D( GL_FRAMEBUFFER,                  // 1. fbo target: GL_FRAMEBUFFER
                          GL_COLOR_ATTACHMENT0 + TexCount, // 2. attachment point
                          Tex.Target,                      // 3. tex target: GL_TEXTURE_2D
                          Tex.GLTexture,                   // 4. tex ID
                          MipLevel );                      // 5. mipmap level: 0(base)
  Inc( FTexCount );
  cle( Self, 'Error while attaching texture to frame buffer: ' );
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

procedure TP3DFrameBuffer.AttachDepth(Tex: TP3DTexture);
begin
  glBindFramebuffer( GL_FRAMEBUFFER, FBO );
  glFramebufferTexture2D( GL_FRAMEBUFFER,                // 1. fbo target: GL_FRAMEBUFFER
                          GL_DEPTH,                      // 2. attachment point
                          GL_TEXTURE_2D,                 // 3. tex target: GL_TEXTURE_2D
                          Tex.GLTexture,                 // 4. tex ID
                          0 );                           // 5. mipmap level: 0(base)
  cle( Self, 'Error while attaching texture to frame buffer: ' );
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

procedure TP3DFrameBuffer.AttachDepth(RB: TP3DRenderBuffer);
begin
  glBindFramebuffer( GL_FRAMEBUFFER, FBO );
  glBindRenderbuffer( GL_RENDERBUFFER, RB.RBO );
  glFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, RB.RBO );
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

procedure TP3DFrameBuffer.AttachStencil(Tex: TP3DTexture);
begin
  glBindFramebuffer( GL_FRAMEBUFFER, FBO );
  glFramebufferTexture2D( GL_FRAMEBUFFER,                // 1. fbo target: GL_FRAMEBUFFER
                          GL_STENCIL,                    // 2. attachment point
                          Tex.Target,                    // 3. tex target: GL_TEXTURE_2D
                          Tex.GLTexture,                 // 4. tex ID
                          0 );                           // 5. mipmap level: 0(base)
  cle( Self, 'Error while attaching texture to frame buffer: ' );
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

procedure TP3DFrameBuffer.DetachAll();
begin
  glDeleteFramebuffers( 1, @FFBO );
  cle( Self, 'Error while deleting frame buffer: ' );
  glGenFramebuffers( 1, @FFBO );
  cle( Self, 'Error while creating frame buffer: ' );
  FTexCount:= 0;
end;

procedure TP3DFrameBuffer.CheckFBO;
var
  error: GlEnum;
begin
  error := glCheckFramebufferStatus(GL_FRAMEBUFFER); cle( Self, 'Error while checking frame buffer status: ' );
  case error of
    GL_FRAMEBUFFER_COMPLETE:
      Exit;
    GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT:
      raise Exception.Create('Incomplete attachment');
    GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT:
      raise Exception.Create('Missing attachment');
    GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT:
      raise Exception.Create('Incomplete dimensions');
    GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT:
      raise Exception.Create('Incomplete formats');
    GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER:
      raise Exception.Create('Incomplete draw buffer');
    GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER:
      raise Exception.Create('Incomplete read buffer');
    GL_FRAMEBUFFER_UNSUPPORTED:
      raise Exception.Create('Framebufferobjects unsupported');
  end;
end;
{$ENDIF}
