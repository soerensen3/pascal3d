unit p3dframebuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dglOpenGL;

type

  { TFrameBuffer }

  TFrameBuffer = class
    FBOs: array of GLint;
    RBOs: array of GLint;

    {Binds one Framebuffer to a render context. Must be in range of FBOs array.}
    procedure BindFB( const id: Integer = 0 );
    {Binds one Renderbuffer to a render context. Must be in range of RBOs array.}
    procedure BindRB( const id: Integer = 0 );
    {Binds and clears a render buffer or framebuffer to clear it. Must either be in range of RBOs array or negative to be ignored.}
    procedure BindAndClear( const idFB: Integer = 0; const idRB: Integer = -1 );
    {Creates multiple framebufferobjects and/or renderbufferobjects.}
    constructor Create( nFB: Integer; nRB: Integer );
    destructor Destroy; override;
    //NO FRAMEBUFFER READING SO FAR
  end;

  { TFrameBufferTex2D }

  TFrameBufferTex2D = class( TFrameBuffer )
    Textures: array of GLint;
    TexDepth: GLint;
    {Create an arbitrary number of textures of the given size and the corresponding framebuffer object.
    No textureformat, mip maps and the like supported yet.}
    constructor Create( n: Integer; DepthTex: Boolean; XRes, YRes: Integer; const MipMap: Boolean = True );
    destructor Destroy; override;
  end;

implementation

{ TFrameBufferTex2D }

constructor TFrameBufferTex2D.Create(n: Integer; DepthTex: Boolean; XRes,
  YRes: Integer; const MipMap: Boolean);
  procedure CreateTex;
  begin
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    if ( MipMap ) then
      begin
        glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
        glTexParameteri( GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE ); // automatic mipmap
      end;
    glTexImage2D( GL_TEXTURE_2D, 0, GL_RGBA16, XRes, YRes, 0,
                  GL_RGBA, GL_UNSIGNED_BYTE, nil );
  end;

  procedure AttachTex( n: Integer );
  begin
    // attach the texture to FBO color attachment point
    glFramebufferTexture2D( GL_FRAMEBUFFER,        // 1. fbo target: GL_FRAMEBUFFER
                            GL_COLOR_ATTACHMENT0 + n,  // 2. attachment point
                            GL_TEXTURE_2D,         // 3. tex target: GL_TEXTURE_2D
                            Textures[ n ],             // 4. tex ID
                            0 );                    // 5. mipmap level: 0(base)
  end;

  procedure AttachDepthTex;
  begin
    // attach the texture to FBO color attachment point
    glFramebufferTexture2D( GL_FRAMEBUFFER,        // 1. fbo target: GL_FRAMEBUFFER
                            GL_COLOR_ATTACHMENT0 + n,  // 2. attachment point
                            GL_TEXTURE_2D,         // 3. tex target: GL_TEXTURE_2D
                            Textures[ n ],             // 4. tex ID
                            0 );                    // 5. mipmap level: 0(base)
  end;

var
  i: Integer;
begin
  inherited Create( 1, Ord( not DepthTex ));

  SetLength( Textures, n );
  glGenTextures( n, @Textures[ 0 ]);

  for i:= 0 to n - 1 do
    begin
      glBindTexture( GL_TEXTURE_2D, Textures[ i ]);
      CreateTex;
    end;

  BindFB();

  if ( n = 0 ) then
    begin
      glDrawBuffer( GL_NONE );
      glReadBuffer( GL_NONE );
    end;
  if ( DepthTex ) then
    begin
      glGenTextures( 1, @TexDepth );
      glBindTexture( GL_TEXTURE_2D, TexDepth );
      glTexImage2D( GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT32, XRes, YRes, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_BYTE, nil );
      glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
      glFramebufferTexture2D( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, TexDepth, 0 );
      //glRenderbufferStorage( GL_RENDERBUFFER, GL_DEPTH_COMPONENT,
//                             XRes, YRes );

    end
  else
    begin
      BindRB();
      glRenderbufferStorage( GL_RENDERBUFFER, GL_DEPTH_COMPONENT,
                           XRes, YRes );

      glBindRenderbuffer( GL_RENDERBUFFER, 0 );
      // attach the renderbuffer to depth attachment point
      glFramebufferRenderbuffer( GL_FRAMEBUFFER,      // 1. fbo target: GL_FRAMEBUFFER
                                 GL_DEPTH_ATTACHMENT, // 2. attachment point
                                 GL_RENDERBUFFER,     // 3. rbo target: GL_RENDERBUFFER
                                 RBOs[ 0 ]);              // 4. rbo ID
    end;

  for i:= 0 to n - 1 do
    AttachTex( i );

  glBindTexture( GL_TEXTURE_2D, 0 );

  // check FBO status
//  Status:= glCheckFramebufferStatus( GL_FRAMEBUFFER );
//  if(status != GL_FRAMEBUFFER_COMPLETE)
//      fboUsed = false;

  // switch back to window-system-provided framebuffer
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;

destructor TFrameBufferTex2D.Destroy;
begin
  glDeleteTextures( Length( Textures ), @Textures[ 0 ]);
  inherited Destroy;
end;

{ TFrameBuffer }

procedure TFrameBuffer.BindFB( const id: Integer );
begin
  glBindFramebuffer( GL_FRAMEBUFFER, FBOs[ id ]);
end;

procedure TFrameBuffer.BindRB( const id: Integer );
begin
  glBindRenderbuffer( GL_FRAMEBUFFER, RBOs[ id ]);
end;

procedure TFrameBuffer.BindAndClear( const idFB: Integer; const idRB: Integer );
var
  BitMask: Integer;
begin
  if ( idFB >= 0 ) then
    BindFB( idFB );
  if ( idRB >= 0 ) then
    BindRB( idRB );

  BitMask:= GL_DEPTH_BUFFER_BIT or // * Ord ( idRB >= 0 ) +
            GL_COLOR_BUFFER_BIT; // * Ord ( idFB >= 0 );
  glClear( BitMask );
end;

constructor TFrameBuffer.Create( nFB: Integer; nRB: Integer );
begin
  inherited Create;
  SetLength( FBOs, nFB );
  glGenFramebuffers( nFB, @FBOs[ 0 ]);

  SetLength( RBOs, nFB );
  glGenRenderbuffers( nRB, @RBOs[ 0 ]);
end;

destructor TFrameBuffer.Destroy;
begin
  glDeleteFramebuffers( Length( FBOs ), @FBOs[ 0 ]);
  glDeleteRenderbuffers( Length( RBOs ), @RBOs[ 0 ]);
  inherited Destroy;
end;

end.

