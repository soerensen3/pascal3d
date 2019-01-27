{$IFDEF INTERFACE}
type

  { TP3DRenderBuffer }

  TP3DRenderBuffer = class ( TP3DStreamable )
    private
      FHeight: Integer;
      FRBO: GLint;
      FWidth: Integer;

    public
      constructor Create(AWidth, AHeight: Integer; AOwner: IP3DStreamableContainer );
      destructor Destroy; override;

    published
      property RBO: GLint read FRBO;
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
  end;

  { TP3DFrameBuffer }

  TP3DFrameBuffer = class ( TP3DStreamable )
    protected
      FAutoViewPort: Boolean;
      FMaps: specialize gP3DStreamablePointerList<TP3DTexture>;
      FRenderbuffer: TP3DStreamableContainer;
      FViewPortIdx: Integer;
      FTexCount: Byte;
      FFBO: GLint;
      FWidthHeight: TIVec2;

      function GetRenderbuffer: TP3DRenderBuffer;
      procedure MapsChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );
      procedure SetRenderbuffer(AValue: TP3DRenderBuffer);

      procedure RenderBufferChange(ANewValue: TP3DStreamable);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy(); override;

      procedure Bind( Index: Integer );
      procedure BindAll();
      procedure Clear();
      procedure Unbind();

      procedure CreateAndAttachMulti(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType;
        ADepth: Boolean; ANames: array of String; const ATexOwner: IP3DStreamableContainer=nil);
      procedure AttachColor( Tex: TP3DTexture; const MipLevel: Integer = 0 );
      procedure AttachDepth( Tex: TP3DTexture );
      procedure AttachDepth( AWidth, AHeight: Integer );
      procedure AttachStencil( Tex: TP3DTexture );
      procedure DetachAll();

      function CheckFBO( const RaiseException: Boolean = True ): Boolean;

      property FBO: GLint read FFBO;
      property TexCount: Byte read FTexCount;
      property AutoViewPort: Boolean read FAutoViewPort write FAutoViewPort;
      property WidthHeight: TIVec2 read FWidthHeight write FWidthHeight;
      property Maps: specialize gP3DStreamablePointerList < TP3DTexture > read FMaps;
      property Renderbuffer: TP3DRenderBuffer read GetRenderbuffer write SetRenderbuffer;
  end;

  TP3DFrameBufferContainerList = specialize gP3DStreamableContainerList < TP3DFrameBuffer >;


{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DRenderBuffer }

constructor TP3DRenderBuffer.Create(AWidth, AHeight: Integer; AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
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

procedure TP3DFrameBuffer.MapsChange(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  if ( AAction = actAdd ) then
    AttachColor( TP3DTexture( AItem ));
end;

function TP3DFrameBuffer.GetRenderbuffer: TP3DRenderBuffer;
begin
  Result:= TP3DRenderBuffer( FRenderbuffer.AsValue );
end;

procedure TP3DFrameBuffer.SetRenderbuffer(AValue: TP3DRenderBuffer);
begin
  FRenderbuffer.AsValue:= AValue;
end;

procedure TP3DFrameBuffer.RenderBufferChange(ANewValue: TP3DStreamable);
begin
  if ( Assigned( ANewValue )) then with ( ANewValue as TP3DRenderBuffer ) do begin
    glBindFramebuffer( GL_FRAMEBUFFER, FBO );
    glBindRenderbuffer( GL_RENDERBUFFER, RBO );
    glFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, RBO );
    glBindFramebuffer( GL_FRAMEBUFFER, 0 );
  end else begin
    glBindFramebuffer( GL_FRAMEBUFFER, FBO );
    glBindRenderbuffer( GL_RENDERBUFFER, 0 );
    glFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, 0 );
    glBindFramebuffer( GL_FRAMEBUFFER, 0 );
  end;
end;

constructor TP3DFrameBuffer.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  glGenFramebuffers( 1, @FBO );
  FTexCount:= 0;
  FAutoViewPort:= True;
  FViewPortIdx:= -1;
  Properties.Add([ TP3DPropAccessBoolean.CreateField( 'AutoViewPort', @FAutoViewPort, smText )]);
  FMaps:= ( specialize gP3DStreamablePointerList < TP3DTexture >).Create( 'Maps', smText );
  FMaps.OnChange:= @MapsChange;
  FRenderbuffer:= TP3DStreamableContainer.Create( 'Renderbuffer', TP3DRenderBuffer, smText );
  FRenderbuffer.OnChange:=@RenderBufferChange;
  Properties.Add([ FMaps, FRenderbuffer ]);
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
  if ( FViewPortIdx >= P3DViewports.Count ) then
    FViewPortIdx:= -1;
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

procedure TP3DFrameBuffer.CreateAndAttachMulti(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; ADepth: Boolean; ANames: array of String; const ATexOwner: IP3DStreamableContainer);
var
  Name: String;
  Tx: TP3DTexture;
begin
  for Name in ANames do begin
    Tx:= TP3DTexture.CreateFromEmpty( AWidth, AHeight, ANumChannels, ADataType, ATexOwner );
    Tx.Name:= Name;
    AttachColor( Tx );
  end;
  if ( ADepth ) then
    AttachDepth( AWidth, AHeight );
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
  Renderbuffer:= nil;
  glBindFramebuffer( GL_FRAMEBUFFER, FBO );
  glFramebufferTexture2D( GL_FRAMEBUFFER,                // 1. fbo target: GL_FRAMEBUFFER
                          GL_DEPTH,                      // 2. attachment point
                          GL_TEXTURE_2D,                 // 3. tex target: GL_TEXTURE_2D
                          Tex.GLTexture,                 // 4. tex ID
                          0 );                           // 5. mipmap level: 0(base)
  cle( Self, 'Error while attaching texture to frame buffer: ' );
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

procedure TP3DFrameBuffer.AttachDepth(AWidth, AHeight: Integer);
begin
  glBindFramebuffer( GL_FRAMEBUFFER, FBO );
  TP3DRenderBuffer.Create( AWidth, AHeight, FRenderbuffer );
  glFramebufferRenderbuffer( GL_FRAMEBUFFER,                // 1. fbo target: GL_FRAMEBUFFER
                             GL_DEPTH_ATTACHMENT,           // 2. attachment point
                             GL_RENDERBUFFER,               // 3. renderbuffer target: GL_RENDERBUFFER
                             Renderbuffer.RBO );            // 4. renderbuffer ID

  cle( Self, 'Error while attaching renderbuffer to frame buffer: ' );
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

function TP3DFrameBuffer.CheckFBO(const RaiseException: Boolean): Boolean;
var
  error: GlEnum;
begin
  error := glCheckFramebufferStatus(GL_FRAMEBUFFER); cle( Self, 'Error while checking frame buffer status: ' );
  Result:= error = GL_FRAMEBUFFER_COMPLETE;
  if ( RaiseException ) then
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
