//p3dtexture.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
type
  TP3DTextureRepeat = ( trRepeat = GL_REPEAT, trRepeatMiror = GL_MIRRORED_REPEAT );
  TP3DTextureFiltering = ( tfNearest, tfLinear );
  TP3DTextureDataType = ( dtByte = GL_BYTE, dtUByte = GL_UNSIGNED_BYTE,
                          dtShort = GL_SHORT, dtUShort = GL_UNSIGNED_SHORT,
                          dtInt = GL_INT, dtUInt = GL_UNSIGNED_INT,
                          dtFloat = GL_FLOAT,
                          dtDouble = GL_DOUBLE,
                          dtHalfFloat = GL_HALF_FLOAT
                          );

  TP3DTextureType = ( tex1D, tex2D, tex3D, texCubeMap );



const
  P3DTextureFilteringToGL: array [ tfNearest..tfLinear ] of Cardinal = ( GL_NEAREST, GL_LINEAR );
  P3DTextureFilteringMipMapToGL: array [ tfNearest..tfLinear, tfNearest..tfLinear ] of Cardinal = (( GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST ), ( GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR ));
  //P3DGLToTextureFiltering: array [ GL_NEAREST..GL_LINEAR_MIPMAP_LINEAR ] of array [ 0..1 ] of TP3DTextureFiltering = (( tfNearest, tfNearest ), ( tfLinear, tfNearest ), ( tfNearest, tfNearest ), ( tfLinear, tfLinear )));

type
  { TP3DTexture }

  TP3DTexture = class ( TP3DDataBlock )
    private
      FDataType: TP3DTextureDataType;
      FFileWatch: TP3DFileWatch;
      FFiltering: TP3DTextureFiltering;
      FFilteringMipMap: TP3DTextureFiltering;
      FGLTexture: Integer;
      FHeight: Integer;
      FMipMap: Boolean;
      FNumChannels: Integer;
      FWidth: Integer;

      procedure SetFiltering(AValue: TP3DTextureFiltering);
      procedure SetFilteringMipMap(AValue: TP3DTextureFiltering);
      procedure SetMipMap(AValue: Boolean);
      procedure UpdateFiltering;
      procedure LoadFromSurface( ASurface: PSDL_Surface );
      procedure LoadFromBuffer( AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; ABuffer: Pointer );
      procedure LoadEmpty( AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType );
      procedure LoadFromGLTexture( Tex: GLint );
      procedure LoadFromTextureFile( FileName: String );
      procedure FileNameChanged( Sender: TP3DFileWatch; ANewFileName: String );

    public
      constructor Create(const AData: TP3DLibrary=nil); override;
      constructor CreateFromFile( AFileName: String; const AData: TP3DLibrary = nil );
      constructor CreateFromBuffer( AWidth, AHeight: Integer; ANumChannels: Integer; ADataType:TP3DTextureDataType; ABuffer: Pointer; const AData: TP3DLibrary = nil);
      constructor CreateFromSurface( ASurface: PSDL_Surface; const AData: TP3DLibrary = nil );
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DLibrary = nil ); override;
      constructor CreateFromEmpty( AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; const AData: TP3DLibrary = nil );
      constructor CreateFromGLTexture( Tex: GLint; const AData: TP3DLibrary = nil );

      procedure LoadMipMapFromSurface( level: Integer; ASurface: PSDL_Surface );

      class function DOMNodeName: String; override;
      procedure Bind( const Location: Integer = 0 );
      procedure Unbind();
      procedure LoadFromBinary( FileName: String ); override;
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      procedure FastDestroy; override;
      procedure Reload; reintroduce;
      procedure SaveToFile;
      procedure SaveToFile( FileName: String );

    published
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property GLTexture: Integer read FGLTexture write FGLTexture;
      property Filtering: TP3DTextureFiltering read FFiltering write SetFiltering;
      property FilteringMipMap: TP3DTextureFiltering read FFilteringMipMap write SetFilteringMipMap;
      property MipMap: Boolean read FMipMap write SetMipMap;
      property FileWatch: TP3DFileWatch read FFileWatch write FFileWatch;
      property DataType: TP3DTextureDataType read FDataType;
      property NumChannels: Integer read FNumChannels;
  end;

  TP3DPropertyAccessTexture = specialize gP3DPropertyAccessDataBlock < TP3DTexture >;
  TP3DTextureList = specialize gP3DCustomDatablockList < TP3DTexture >;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

function StrToFilterDef( S: String; Default: TP3DTextureFiltering ): TP3DTextureFiltering;
begin
  case S of
    'linear': Result:= tfLinear;
    'nearest': Result:= tfNearest;
  else
    Result:= Default;
  end;
end;

function P3DChooseFormatByType( ADataType: TP3DTextureDataType; ANumChannels: Integer ): GLenum;
begin
  if ( ANumChannels = GL_STENCIL ) then
    Result:= GL_STENCIL
  else if ( ANumChannels = GL_DEPTH_COMPONENT ) then
    Result:= GL_DEPTH_COMPONENT
  else case ADataType of
    dtByte:
      case ANumChannels of
        1: Result:= GL_R8;
        2: Result:= GL_RG8;
        3: Result:= GL_RGB8;
        4: Result:= GL_RGBA8;
      end;
    dtUByte:
      case ANumChannels of
        1: Result:= GL_R;//GL_R8UI;
        2: Result:= GL_RG;//GL_RG8UI;
        3: Result:= GL_RGB;//GL_RGB8UI;
        4: Result:= GL_RGBA;//GL_RGBA8UI;
      end;
    dtShort:
      case ANumChannels of
        1: Result:= GL_R16;
        2: Result:= GL_RG16;
        3: Result:= GL_RGB16;
        4: Result:= GL_RGBA16;
      end;
    dtUShort:
      case ANumChannels of
        1: Result:= GL_R16UI;
        2: Result:= GL_RG16UI;
        3: Result:= GL_RGB16UI;
        4: Result:= GL_RGBA16UI;
      end;
    dtInt:
      case ANumChannels of
        1: Result:= GL_R32I;
        2: Result:= GL_RG32I;
        3: Result:= GL_RGB32I;
        4: Result:= GL_RGBA32I;
      end;
    dtUInt:
      case ANumChannels of
        1: Result:= GL_R32UI;
        2: Result:= GL_RG32UI;
        3: Result:= GL_RGB32UI;
        4: Result:= GL_RGBA32UI;
      end;
    dtHalfFloat:
      case ANumChannels of
        1: Result:= GL_R16F;
        2: Result:= GL_RG16F;
        3: Result:= GL_RGB16F;
        4: Result:= GL_RGBA16F;
      end;
    dtFloat, dtDouble:
      case ANumChannels of
        1: Result:= GL_R32F;
        2: Result:= GL_RG32F;
        3: Result:= GL_RGB32F;
        4: Result:= GL_RGBA32F;
      end;
  end;
end;

function P3DChooseFormatBySDLPixelFormat( PixelFormat: PSDL_PixelFormat ): GLenum;
begin
  case PixelFormat^.format of
    //SDL_PIXELFORMAT_UNKNOWN: Result:= GL_RGBA;
    //SDL_PIXELFORMAT_INDEX1LSB: Result:= GL_RGBA;
    //SDL_PIXELFORMAT_INDEX1MSB: Result:= GL_RGBA;
    //SDL_PIXELFORMAT_INDEX4LSB: Result:= GL_RGBA;
    //SDL_PIXELFORMAT_INDEX4MSB: Result:= GL_RGBA;
    //SDL_PIXELFORMAT_INDEX8: Result:= GL_RGBA;
    //SDL_PIXELFORMAT_RGB332: Result:= GL_RGBA;
    SDL_PIXELFORMAT_RGB444: Result:= GL_RGB4;
    //SDL_PIXELFORMAT_RGB555:
    //SDL_PIXELFORMAT_BGR555
    SDL_PIXELFORMAT_ARGB4444: Result:= GL_RGBA4;
    SDL_PIXELFORMAT_RGBA4444: Result:= GL_RGBA4;
    SDL_PIXELFORMAT_ABGR4444: Result:= GL_RGBA4;
    SDL_PIXELFORMAT_BGRA4444: Result:= GL_RGBA4;
    //SDL_PIXELFORMAT_ARGB1555: Result:= GL_RGBA;
    //SDL_PIXELFORMAT_RGBA5551
    //SDL_PIXELFORMAT_ABGR1555
    //SDL_PIXELFORMAT_BGRA5551
    //SDL_PIXELFORMAT_RGB565
    //SDL_PIXELFORMAT_BGR565
    //SDL_PIXELFORMAT_RGB24: Result:= GL_RGBA;
    //SDL_PIXELFORMAT_BGR24: Result:= GL_RGBA;
    SDL_PIXELFORMAT_RGB888: Result:= GL_RGB8;
    SDL_PIXELFORMAT_RGBX8888: Result:= GL_RGBA8;
    SDL_PIXELFORMAT_BGR888: Result:= GL_RGB8;
    SDL_PIXELFORMAT_BGRX8888: Result:= GL_RGBA8;
    SDL_PIXELFORMAT_ARGB8888: Result:= GL_RGBA8;
    SDL_PIXELFORMAT_RGBA8888: Result:= GL_RGBA8;
    SDL_PIXELFORMAT_ABGR8888: Result:= GL_RGBA8;
    SDL_PIXELFORMAT_BGRA8888: Result:= GL_RGBA8;
    //SDL_PIXELFORMAT_ARGB2101010
    //SDL_PIXELFORMAT_YV12
    //SDL_PIXELFORMAT_IYUV
    //SDL_PIXELFORMAT_YUY2
    //SDL_PIXELFORMAT_UYVY
    //SDL_PIXELFORMAT_YVYU
  else
    Result:= GL_RGBA;
  end;
end;

 { TP3DTexture }

procedure TP3DTexture.SetFiltering(AValue: TP3DTextureFiltering);
begin
  if FFiltering=AValue then Exit;
  FFiltering:=AValue;
  UpdateFiltering;
end;

procedure TP3DTexture.SetFilteringMipMap(AValue: TP3DTextureFiltering);
begin
  if FFilteringMipMap=AValue then Exit;
  FFilteringMipMap:=AValue;
  UpdateFiltering;
end;

procedure TP3DTexture.SetMipMap(AValue: Boolean);
begin
  if FMipMap=AValue then Exit;
  FMipMap:=AValue;
  UpdateFiltering;
end;

procedure TP3DTexture.UpdateFiltering;
begin
  glBindTexture( GL_TEXTURE_2D, FGLTexture ); cle( Self );
  if ( MipMap ) then
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, P3DTextureFilteringMipMapToGL[ Filtering, FilteringMipMap ])
  else
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, P3DTextureFilteringToGL[ Filtering ]);
  cle( Self );

  glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, P3DTextureFilteringToGL[ Filtering ]); cle( Self );
  glBindTexture( GL_TEXTURE_2D, 0 ); cle( Self );
end;

procedure TP3DTexture.LoadFromSurface(ASurface: PSDL_Surface);
var
  Tex: GLint;
  InternalMode, Mode: GLenum;
begin
  glGenTextures( 1, @Tex ); cle( Self );
  glBindTexture( GL_TEXTURE_2D, Tex ); cle( Self );

  case ( ASurface^.format^.BytesPerPixel ) of
    1: Mode:= GL_R;
    2: Mode:= GL_RG;
    3: Mode:= GL_RGB;
    4: Mode:= GL_RGBA;
    else
      Mode:= GL_RGBA;
  end;

  InternalMode:= P3DChooseFormatBySDLPixelFormat( ASurface^.format );

  glTexImage2D( GL_TEXTURE_2D, 0, InternalMode, ASurface^.w, ASurface^.h, 0, Mode, GL_UNSIGNED_BYTE, ASurface^.pixels ); cle( Self );

  glGenerateMipmap( GL_TEXTURE_2D ); cle( Self );

  FFiltering:= tfLinear;
  FFilteringMipMap:= tfLinear;
  FMipMap:= True;
  UpdateFiltering;
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT ); cle( Self );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT ); cle( Self );

  FWidth:= ASurface^.w;
  FHeight:= ASurface^.h;

  if ( FGLTexture > 0 ) then
    glDeleteTextures( 1, @FGLTexture ); cle( Self );
  FGLTexture:= Tex;
end;

procedure TP3DTexture.LoadFromBuffer(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; ABuffer: Pointer);
var
  Mode: Integer;
  InternalMode: Integer;
  Tex: GLint;
begin
  glGenTextures( 1, @Tex ); cle( Self );
  glBindTexture( GL_TEXTURE_2D, Tex ); cle( Self );

  FNumChannels:= ANumChannels;
  FDataType:= ADataType;
  case ( ANumChannels ) of
    1: Mode:= GL_R;
    2: Mode:= GL_RG;
    3: Mode:= GL_RGB;
    4: Mode:= GL_RGBA;
    else
      Mode:= NumChannels;
  end;
  InternalMode:= P3DChooseFormatByType( ADataType, ANumChannels );
  glTexImage2D( GL_TEXTURE_2D, 0, InternalMode, AWidth, AHeight, 0, Mode, GLenum( ADataType ), ABuffer ); cle( Self );

  glGenerateMipmap( GL_TEXTURE_2D ); cle( Self );

  FFiltering:= tfNearest;
  FFilteringMipMap:= tfNearest;
  FMipMap:= False;
  UpdateFiltering;
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT ); cle( Self );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT ); cle( Self );

  FWidth:= AWidth;
  FHeight:= AHeight;

  if ( FGLTexture > 0 ) then
    glDeleteTextures( 1, @FGLTexture ); cle( Self );
  FGLTexture:= Tex;
end;

procedure TP3DTexture.LoadEmpty(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType);
var
  Mode: Integer;
  InternalMode: Integer;
  Tex: GLint;
begin
  glGenTextures( 1, @Tex ); cle( Self );
  glBindTexture( GL_TEXTURE_2D, Tex ); cle( Self );

  FNumChannels:= ANumChannels;
  FDataType:= ADataType;
  case ( ANumChannels ) of
    1: Mode:= GL_R;
    2: Mode:= GL_RG;
    3: Mode:= GL_RGB;
    4: Mode:= GL_RGBA;
    else
      Mode:= NumChannels;
  end;
  InternalMode:= P3DChooseFormatByType( ADataType, ANumChannels );
  glTexImage2D( GL_TEXTURE_2D, 0, InternalMode, AWidth, AHeight, 0, Mode, GLenum( ADataType ), nil ); cle( Self );

  glGenerateMipmap( GL_TEXTURE_2D ); cle( Self );

  FFiltering:= tfLinear;
  FFilteringMipMap:= tfLinear;
  FMipMap:= True;
  UpdateFiltering;
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT ); cle( Self );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT ); cle( Self );

  FWidth:= AWidth;
  FHeight:= AHeight;

  if ( FGLTexture > 0 ) then
    glDeleteTextures( 1, @FGLTexture ); cle( Self );
  FGLTexture:= Tex;
end;

procedure TP3DTexture.LoadFromGLTexture(Tex: GLint);
var
  Filter: Integer;
begin
  FGLTexture:= Tex;
  Bind();
  glGetTexLevelParameteriv( GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @Width ); cle( Self );
  glGetTexLevelParameteriv( GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @Height ); cle( Self );
  //glGetTexParameteriv( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, @Filter );
  //case Filter of
  //  GL_LINEAR: FFiltering:= tfLinear;
  //  GL_NEAREST: FFiltering:= tfNearest;
  //end;
  glGetTexParameteriv( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, @Filter );
  case Filter of
    GL_LINEAR: begin FMipMap:= False; FFiltering:= tfLinear; FFilteringMipMap:= tfNearest; end;
    GL_NEAREST: begin FMipMap:= False; FFiltering:= tfNearest; FFilteringMipMap:= tfNearest; end;
    GL_LINEAR_MIPMAP_LINEAR: begin FMipMap:= True; FFiltering:= tfLinear; FFilteringMipMap:= tfLinear end;
    GL_LINEAR_MIPMAP_NEAREST: begin FMipMap:= True; FFiltering:= tfNearest; FFilteringMipMap:= tfNearest; end;
    GL_NEAREST_MIPMAP_LINEAR: begin FMipMap:= True; FFiltering:= tfNearest; FFilteringMipMap:= tfLinear end;
    GL_NEAREST_MIPMAP_NEAREST: begin FMipMap:= True; FFiltering:= tfNearest; FFilteringMipMap:= tfNearest; end;
  end;
  //glGetTexLevelParameteriv( GL_TEXTURE_2D, 0, GL_TEX ;
  FMipMap:= True;
  Unbind();
end;

procedure TP3DTexture.LoadFromTextureFile(FileName: String);
var
  FSurface: PSDL_Surface;
begin
  FSurface:= IMG_Load( PChar( FileName ));
  if ( FSurface = nil ) then
    raise Exception.Create( Format( 'The specified file "%s" could not be loaded!', [ ExtractFileName( FileName )]));

  {$IFDEF VERBOSE}
  WriteLn( 'Texture loaded: ' + ExtractFileName( FileName ));
  {$ENDIF}
  LoadFromSurface( FSurface );
  SDL_FreeSurface( FSurface );
end;

procedure TP3DTexture.FileNameChanged(Sender: TP3DFileWatch; ANewFileName: String);
begin
  LoadFromTextureFile( ANewFileName );
end;

constructor TP3DTexture.Create(const AData: TP3DLibrary);
begin
  inherited Create(AData);
  FFileWatch:= TP3DFileWatch.Create( '' );
  FileWatch.OnFileNameChange:= @FileNameChanged;
  Owner.Textures.Add( Self );

  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Width' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Height' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'GLTexture' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Filtering', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'FilteringMipMap', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'MipMap', smAttribute ));
  Properties.Add( TP3DPropertyAccessFileWatch.CreateField( 'File', @FFileWatch, smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'DataType' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'NumChannels' ));
end;

procedure TP3DTexture.LoadFromBinary(FileName: String );
var
  FSurface: PSDL_Surface;
begin
  if (( FileName = FileWatch.FileName ) and ( not FileWatch.CheckForChange )) then
    begin
      {$IFDEF VERBOSE}
      WriteLn( 'File did not change. Skipped reload of ' + ExtractFileName( FileName ));
      {$ENDIF}
      exit;
    end;

  inherited LoadFromBinary( FileName );
  FileWatch.FileName:= FileName;

  LoadFromTextureFile( FileName );
end;

procedure TP3DTexture.LoadFromDOM(ADOMNode: TDOMElement);
var
  FullTexName: String;
  FileName: DOMString;
begin
  inherited LoadFromDOM(ADOMNode);
  FileName:= ADOMNode.GetAttribute( 'file' );
  FullTexName:= P3DSearchPaths.Models.FindFileEx( FileName, GetCurrentDir );

  if ( not FileExists( FullTexName )) then
    raise Exception.Create( 'Error: The specified texture "' + ExtractFileName( FileName ) + '" could not be found!' );

  LoadFromBinary( FullTexName );
  FMipMap:= P3DStrToBoolDef( ADOMNode.GetAttribute( 'mipmap' ), True );
  FFilteringMipMap:= StrToFilterDef( ADOMNode.GetAttribute( 'mipmap_filter' ), tfLinear );
  FFiltering:= StrToFilterDef( ADOMNode.GetAttribute( 'texture_filter' ), tfLinear );
  UpdateFiltering;
end;

procedure TP3DTexture.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew(ADOMNode);
end;

constructor TP3DTexture.CreateFromFile(AFileName: String; const AData: TP3DLibrary);
var
  FileName: String;
begin
  FileName:= P3DSearchPaths.Models.FindFileEx( AFileName, GetCurrentDir );
  if ( FileName = '' ) then
    raise Exception.Create( Format( 'The specified file "%s" was not found!', [ AFileName ]));
  Create( AData );
  //FFileWatch:= TP3DFileWatch.Create( FileName );
  //FFileWatch.FileName:= FileName;
  LoadFromFile( FileName );
end;

constructor TP3DTexture.CreateFromBuffer(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; ABuffer: Pointer; const AData: TP3DLibrary);
begin
  if ( not Assigned( glGenTextures )) then
    exit;
  Create( AData );
  //FFileWatch:= TP3DFileWatch.Create( '' );
  LoadFromBuffer( AWidth, AHeight, ANumChannels, ADataType, ABuffer );
end;

constructor TP3DTexture.CreateFromSurface(ASurface: PSDL_Surface; const AData: TP3DLibrary);
begin
  if ( not Assigned( glGenTextures )) then
    exit;
  Create( AData );
  //FFileWatch:= TP3DFileWatch.Create( '' );
  LoadFromSurface( ASurface );
end;

constructor TP3DTexture.CreateFromDOM(ADOMNode: TDOMElement; const AData: TP3DLibrary);
begin
  inherited CreateFromDOM( ADOMNode, AData );
  //FFileWatch:= TP3DFileWatch.Create( '' );
end;

constructor TP3DTexture.CreateFromEmpty(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; const AData: TP3DLibrary);
begin
  if ( not Assigned( glGenTextures )) then
    exit;
  Create( AData );
  //FFileWatch:= TP3DFileWatch.Create( '' );
  LoadEmpty( AWidth, AHeight, ANumChannels, ADataType );
end;

constructor TP3DTexture.CreateFromGLTexture(Tex: GLint; const AData: TP3DLibrary);
begin
  if ( not Assigned( glGenTextures )) then
    exit;
  Create( AData );
  //FFileWatch:= TP3DFileWatch.Create( '' );
  LoadFromGLTexture( Tex );
end;

procedure TP3DTexture.LoadMipMapFromSurface(level: Integer;
  ASurface: PSDL_Surface);
var
  Mode: GLenum;
  InternalMode: GLenum;
begin
  case ( ASurface^.format^.BytesPerPixel ) of
    1: Mode:= GL_R;
    2: Mode:= GL_RG;
    3: Mode:= GL_RGB;
    4: Mode:= GL_RGBA;
    else
      Mode:= GL_RGBA;
  end;

  Bind();

  InternalMode:= P3DChooseFormatBySDLPixelFormat( ASurface^.format );

  glTexImage2D( GL_TEXTURE_2D, level, InternalMode, ASurface^.w, ASurface^.h, 0, Mode, GL_UNSIGNED_BYTE, ASurface^.pixels ); cle( Self );
end;

class function TP3DTexture.DOMNodeName: String;
begin
  Result:= 'texture';
end;

procedure TP3DTexture.Bind(const Location: Integer);
begin
  glActiveTexture( GL_TEXTURE0 + Location );
  glBindTexture( GL_TEXTURE_2D, GLTexture );
end;

procedure TP3DTexture.Unbind;
begin
  glBindTexture( GL_TEXTURE_2D, 0 );
end;

procedure TP3DTexture.FastDestroy;
begin
  if ( Assigned( glDeleteTextures )) then
    glDeleteTextures( 1, @FGLTexture ); cle( Self );
  FileWatch.Free;

  Owner.Textures.Remove( Self );

  inherited FastDestroy;
end;

procedure TP3DTexture.Reload;
var
  tex: Integer;
begin
  tex:= FGLTexture;
  LoadFromFile( FileWatch.FileName );
  if ( Assigned( glDeleteTextures )) then
    glDeleteTextures( 1, @tex ); cle( Self );
end;

procedure TP3DTexture.SaveToFile;
begin
  SaveToFile( FileWatch.FileName );
end;

procedure TP3DTexture.SaveToFile(FileName: String);
var
  Pixels: PInteger;
  surface: PSDL_Surface;
begin
  Bind();
  GetMem( Pixels, Width * Height * 4 );
  glGetTexImage( GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_BYTE, Pixels );
  surface:= SDL_CreateRGBSurfaceFrom( Pixels, Width, Height, 32, Width * 4, $000000FF, $0000FF00, $00FF0000, $FF000000 );
  IMG_SavePNG( surface, PChar( FileName ));
  SDL_FreeSurface( surface );
  Freemem( Pixels, Width * Height * 4 );
end;

{$ENDIF}
