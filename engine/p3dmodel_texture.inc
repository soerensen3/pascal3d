{$IFDEF INTERFACE}
type

  { TP3DTexture }

  TP3DTexture = class ( TP3DDataBlock )
    private
      FGLTexture: Integer;
      FHeight: Integer;
      FWidth: Integer;

    public
      constructor Create( AName: String; const AData: TP3DData = nil );
      constructor CreateFromSurface( ASurface: PSDL_Surface; const AData: TP3DData = nil );
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil );
      destructor Destroy; override;

    published
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property GLTexture: Integer read FGLTexture write FGLTexture;
  end;
  TP3DTextureList = specialize gP3DCustomDatablockList < TP3DTexture >;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
 { TP3DTexture }

constructor TP3DTexture.Create(AName: String; const AData: TP3DData);
var
  Mode: Integer;
  fSurface: PSDL_Surface;
begin
  AName:= ExpandFileName( AName );
  if ( not FileExists( AName )) then
    raise Exception.Create( Format( 'The specified file "%s" was not found!', [ AName ]));
  inherited Create( AData );
  FSurface:= IMG_Load( PChar( AName ));
  FileWatch.FileName:= AName;
  if ( fSurface = nil ) then
    raise Exception.Create( Format( 'The specified file "%s" could not be loaded!', [ AName ]));

  WriteLn( 'Texture loaded: ' + AName );
  if ( not Assigned( glGenTextures )) then
    exit;
  glGenTextures(1, @FGLTexture );
  glBindTexture(GL_TEXTURE_2D, FGLTexture );

  if ( fSurface^.format^.BytesPerPixel = 4 ) then
    Mode:= GL_RGBA
  else
    Mode:= GL_RGB;

  glTexImage2D( GL_TEXTURE_2D, 0, Mode, FSurface^.w, FSurface^.h, 0, Mode, GL_UNSIGNED_BYTE, fSurface^.pixels );

  glGenerateMipmap( GL_TEXTURE_2D );

  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER );

  FWidth:= FSurface^.w;
  FHeight:= FSurface^.h;

  SDL_FreeSurface( FSurface );
end;

constructor TP3DTexture.CreateFromSurface(ASurface: PSDL_Surface;
  const AData: TP3DData);
var
  Mode: Integer;
  fSurface: PSDL_Surface;
begin
  if ( not Assigned( glGenTextures )) then
    exit;
  inherited Create( AData );
  glGenTextures(1, @fGLTexture );
  glBindTexture(GL_TEXTURE_2D, FGLTexture );

  fSurface:= ASurface;

  if ( FSurface^.format^.BytesPerPixel = 4 ) then
    Mode:= GL_RGBA
  else
    Mode:= GL_RGB;

  glTexImage2D( GL_TEXTURE_2D, 0, Mode, FSurface^.w, FSurface^.h, 0, Mode, GL_UNSIGNED_BYTE, FSurface^.pixels );

  glGenerateMipmap( GL_TEXTURE_2D );

  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER );

  FWidth:= FSurface^.w;
  FHeight:= FSurface^.h;

  SDL_FreeSurface( FSurface );
end;

constructor TP3DTexture.CreateFromDOM(ADOMNode: TDOMElement;
  const AData: TP3DData);
var
  TexName: DOMString;
begin
  TexName:= ADOMNode.GetAttribute( 'file' );
  if ( not FileExists( TexName )) then
    raise Exception.Create( 'Error: The specified texture "' + TexName + '" could not be found!' );

  Create( TexName, AData );
end;

destructor TP3DTexture.Destroy;
begin
  if ( Assigned( glDeleteTextures )) then
    glDeleteTextures( 1, @FGLTexture );
  inherited Destroy;
end;
{$ENDIF}
