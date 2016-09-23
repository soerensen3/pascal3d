{$IFDEF INTERFACE}
type

  { TP3DFrameBuffer }

  TP3DFrameBuffer = class ( TPersistent )
    private
      FTexCount: Byte;
      FFBO: GLint;

    public
      constructor Create();
      destructor Destroy(); override;

      procedure Bind( Index: Integer );
      procedure BindAll();
      procedure Clear();
      procedure Unbind();

      procedure AttachColor(Tex: TP3DTexture);
      procedure AttachDepth( Tex: TP3DTexture );
      procedure AttachStencil( Tex: TP3DTexture );
      procedure DetachAll();

      procedure CheckFBO;

      property FBO: GLint read FFBO;
      property TexCount: Byte read FTexCount;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DFrameBuffer }

constructor TP3DFrameBuffer.Create;
begin
  inherited Create;
  glGenFramebuffersEXT( 1, @FBO );
  FTexCount:= 0;
end;

destructor TP3DFrameBuffer.Destroy;
begin
  glDeleteFramebuffersEXT( 1, @FFBO );
  inherited Destroy;
end;

procedure TP3DFrameBuffer.Bind(Index: Integer);
begin
  glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, FFBO );
  //glDrawBuffer( GL_COLOR_ATTACHMENT0_EXT + Index );
end;

procedure TP3DFrameBuffer.BindAll;
var
  i: Integer;
  Buffers: array [ 0..255 ] of GLint;
begin
  glBindFramebufferEXT( GL_FRAMEBUFFER, FFBO ); cle( Self, 'Error while binding frame buffer: ' );
  for i:= 0 to TexCount - 1 do
    Buffers[ i ]:= GL_COLOR_ATTACHMENT0 + i;
  glDrawBuffers( TexCount, @Buffers[ 0 ]); cle( Self, 'Error while setting frame buffer buffers: ' );
end;

procedure TP3DFrameBuffer.Clear;
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BITS ); cle( Self, 'Error while clearing frame buffer: ' );
end;

procedure TP3DFrameBuffer.Unbind;
begin
  glBindFramebufferEXT( GL_FRAMEBUFFER_EXT, 0 ); cle( Self, 'Error unbinding frame buffer: ' );
end;

procedure TP3DFrameBuffer.AttachColor( Tex: TP3DTexture );
begin
  Inc( FTexCount );
  glFramebufferTexture2DEXT( GL_FRAMEBUFFER,                  // 1. fbo target: GL_FRAMEBUFFER
                          GL_COLOR_ATTACHMENT0 + TexCount, // 2. attachment point
                          GL_TEXTURE_2D,                   // 3. tex target: GL_TEXTURE_2D
                          Tex.GLTexture,                   // 4. tex ID
                          0 );                             // 5. mipmap level: 0(base)
  cle( Self, 'Error while attaching texture to frame buffer: ' );
end;

procedure TP3DFrameBuffer.AttachDepth(Tex: TP3DTexture);
begin
  glFramebufferTexture2DEXT( GL_FRAMEBUFFER,                // 1. fbo target: GL_FRAMEBUFFER
                          GL_DEPTH,                      // 2. attachment point
                          GL_TEXTURE_2D,                 // 3. tex target: GL_TEXTURE_2D
                          Tex.GLTexture,                 // 4. tex ID
                          0 );                           // 5. mipmap level: 0(base)
  cle( Self, 'Error while attaching texture to frame buffer: ' );
end;

procedure TP3DFrameBuffer.AttachStencil(Tex: TP3DTexture);
begin
  glFramebufferTexture2DEXT( GL_FRAMEBUFFER,                // 1. fbo target: GL_FRAMEBUFFER
                          GL_STENCIL,                    // 2. attachment point
                          GL_TEXTURE_2D,                 // 3. tex target: GL_TEXTURE_2D
                          Tex.GLTexture,                 // 4. tex ID
                          0 );                           // 5. mipmap level: 0(base)
  cle( Self, 'Error while attaching texture to frame buffer: ' );
end;

procedure TP3DFrameBuffer.DetachAll;
begin
  glDeleteFramebuffersEXT( 1, @FFBO );
  cle( Self, 'Error while deleting frame buffer: ' );
  glGenFramebuffersEXT( 1, @FFBO );
  cle( Self, 'Error while creating frame buffer: ' );
  FTexCount:= 0;
end;

procedure TP3DFrameBuffer.CheckFBO;
var
  error: GlEnum;
begin
  error := glCheckFramebufferStatusEXT(GL_FRAMEBUFFER); cle( Self, 'Error while checking frame buffer status: ' );
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
