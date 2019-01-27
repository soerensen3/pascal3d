//p3dtexture.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
type
  TP3DTextureWrap = ( twRepeat = GL_REPEAT, twRepeatMiror = GL_MIRRORED_REPEAT,
                      twClampToEdge = GL_CLAMP_TO_EDGE, twClampToBorder = GL_CLAMP_TO_BORDER );
  TP3DTextureFiltering = ( tfNearest, tfLinear );
  TP3DTextureDataType = ( dtByte = GL_BYTE, dtUByte = GL_UNSIGNED_BYTE,
                          dtShort = GL_SHORT, dtUShort = GL_UNSIGNED_SHORT,
                          dtInt = GL_INT, dtUInt = GL_UNSIGNED_INT,
                          dtFloat = GL_FLOAT,
                          dtDouble = GL_DOUBLE,
                          dtHalfFloat = GL_HALF_FLOAT
                          );

  TP3DTextureType = ( tex1D, tex2D, tex3D, texCubeMap );

  TP3DCubeMapSides = ( cmsPX, //= GL_TEXTURE_CUBE_MAP_POSITIVE_X,
                       cmsNX, //= GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
                       cmsPY, //= GL_TEXTURE_CUBE_MAP_POSITIVE_Y,
                       cmsNY, //= GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
                       cmsPZ, //= GL_TEXTURE_CUBE_MAP_POSITIVE_Z,
                       cmsNZ ); //= GL_TEXTURE_CUBE_MAP_NEGATIVE_Z );

const
  P3DCubeMapSidesToString: array [ TP3DCubeMapSides ] of String = ( 'PX', 'NX', 'PY', 'NY', 'PZ', 'NZ' );
  P3DTextureFilteringToGL: array [ tfNearest..tfLinear ] of Cardinal = ( GL_NEAREST, GL_LINEAR );
  P3DTextureFilteringMipMapToGL: array [ tfNearest..tfLinear, tfNearest..tfLinear ] of Cardinal = (( GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST ), ( GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR ));
  //P3DGLToTextureFiltering: array [ GL_NEAREST..GL_LINEAR_MIPMAP_LINEAR ] of array [ 0..1 ] of TP3DTextureFiltering = (( tfNearest, tfNearest ), ( tfLinear, tfNearest ), ( tfNearest, tfNearest ), ( tfLinear, tfLinear )));

type
  { TP3DTexture }

  TP3DTexture = class ( TP3DDataBlock, IP3DPlottableLayer )
    private
      FDataType: TP3DTextureDataType;
      FFileWatch: TP3DFilePointer;
      FFiltering: TP3DTextureFiltering;
      FFilteringMipMap: TP3DTextureFiltering;
      FGLTexture: Integer;
      FHeight: Integer;
      FMipMap: Boolean;
      FNumChannels: Integer;
      FWidth: Integer;
      FTarget: GLenum;
      FWrapModeX: TP3DTextureWrap;
      FWrapModeY: TP3DTextureWrap;

      procedure SetFiltering(AValue: TP3DTextureFiltering);
      procedure SetFilteringMipMap(AValue: TP3DTextureFiltering);
      procedure SetMipMap(AValue: Boolean);
      procedure SetWrapModeX(AValue: TP3DTextureWrap);
      procedure SetWrapModeY(AValue: TP3DTextureWrap);
      procedure UpdateFiltering( const ATarget: GLenum = 0 );
      procedure UpdateWrapMode( const ATarget: GLenum = 0 );
      function CreateNewTexAndBind( const ATarget: GLenum = 0 ): GLint;
      procedure LoadFromSurface( ASurface: PSDL_Surface; const ATarget: GLenum = 0 );
      procedure LoadFromBuffer( AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; ABuffer: Pointer; const ATarget: GLenum = 0 );
      procedure LoadEmpty( AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; const ATarget: GLenum = 0 );
      procedure LoadFromGLTexture( Tex: GLint; const ATarget: GLenum = 0 );
      procedure LoadFromTextureFile( FileName: String );
      procedure FileNameChanged( Sender: TP3DFilePointer; ANewFileName: String );
      function GetFile( FN: String ): String;

   public
      class function GetContainer( ALib: TP3DLibrary): TP3DPropertyAccess; override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override; overload;
      constructor CreateFromFile( AFileName: String; const AData: TP3DLibrary = nil );
      constructor CreateFromBuffer( AWidth, AHeight: Integer; ANumChannels: Integer; ADataType:TP3DTextureDataType; ABuffer: Pointer; const AData: TP3DLibrary = nil);
      constructor CreateFromSurface( ASurface: PSDL_Surface; const AData: TP3DLibrary = nil );
      constructor CreateFromEmpty( AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; const AData: TP3DLibrary = nil );
      constructor CreateFromGLTexture( Tex: GLint; const AData: TP3DLibrary = nil );
      constructor CreateFromGLTextureCube( PX, NX, PY, NY, PZ, NZ: GLint; const AData: TP3DLibrary = nil );
      constructor CreateFromFileCube( PX, NX, PY, NY, PZ, NZ: String; const AData: TP3DLibrary = nil );
      constructor CreateFromBufferCube( AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; PX, NX, PY, NY, PZ, NZ: Pointer; const AData: TP3DLibrary = nil );
      constructor CreateFromSurfaceCube(PX, NX, PY, NY, PZ, NZ: PSDL_Surface; const AData: TP3DLibrary = nil );

      procedure LoadMipMapFromSurface( level: Integer; ASurface: PSDL_Surface; const ATarget: GLenum = GL_TEXTURE_2D );

      procedure Bind( const Location: Integer = 0 );
      procedure Unbind();
      procedure LoadFromBinary( FileName: String ); //override;
      destructor Destroy; override;
      procedure Reload; reintroduce;
      procedure SaveToFile;
      procedure SaveToFile( FileName: String );
      procedure UpdateMipMaps( const ATarget: GLenum = 0 );

      //IP3DPlottable -->
      function GetLayer( ASettings: TP3DPlotListSetting ): TP3DPlotCommandLayer;
      //<-- IP3DPlottable

    published
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property GLTexture: Integer read FGLTexture write FGLTexture;
      property Filtering: TP3DTextureFiltering read FFiltering write SetFiltering;
      property FilteringMipMap: TP3DTextureFiltering read FFilteringMipMap write SetFilteringMipMap;
      property WrapModeX: TP3DTextureWrap read FWrapModeX write SetWrapModeX;
      property WrapModeY: TP3DTextureWrap read FWrapModeY write SetWrapModeY;
      property MipMap: Boolean read FMipMap write SetMipMap;
      property FileWatch: TP3DFilePointer read FFileWatch write FFileWatch;
      property DataType: TP3DTextureDataType read FDataType;
      property NumChannels: Integer read FNumChannels;
      property Target: GLenum read FTarget write FTarget;
  end;

  {TP3DTextureContainer = specialize gP3DStreamableContainer < TP3DTexture >;
  TP3DTexturePointer = specialize gP3DStreamablePointer < TP3DTexture >;
  TP3DTexturePointerList = specialize gP3DStreamableList < TP3DTexture, TP3DTexturePointer >;}
  TP3DTextureContainerList = specialize gP3DNamedStreamableContainerList < TP3DTexture >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{.$MACRO ON}
{.$define SDL_LOCKIFMUST(s) := if(SDL_MUSTLOCK(s)) then SDL_LockSurface(s);}
{.$define SDL_UNLOCKIFMUST(s) := if(SDL_MUSTLOCK(s)) then SDL_UnlockSurface(s);}

procedure SDL_LOCKIFMUST( S: PSDL_Surface ); inline;
begin
  if ( SDL_MUSTLOCK( s )) then SDL_LockSurface( s );
end;

procedure SDL_UNLOCKIFMUST( S: PSDL_Surface ); inline;
begin
  if ( SDL_MUSTLOCK( s )) then SDL_UnlockSurface( s );
end;

function SDL_FlipSurface( S: PSDL_Surface ): PSDL_Surface;
var
  i: Integer;
  p1, p2: Pointer;
begin
  Result:= SDL_CreateRGBSurfaceWithFormat( S^.flags, S^.w, S^.h, S^.format^.BitsPerPixel, S^.format^.format );

  SDL_LOCKIFMUST( S );
  SDL_LOCKIFMUST( Result );

  for i:= 0 to S^.h - 1 do begin
    p1:= S^.pixels + i * S^.pitch;
    p2:= Result^.pixels + i * Result^.pitch;
    p2:= Result^.pixels + ( Result^.h - 1 - i ) * S^.pitch;

    Move( p1^, p2^, S^.pitch );
  end;

  SDL_UNLOCKIFMUST( S );
  SDL_UNLOCKIFMUST( Result );
end;

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
    dtHalfFloat,dtByte:
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
    dtFloat:
      case ANumChannels of
        1: Result:= GL_R16F;
        2: Result:= GL_RG16F;
        3: Result:= GL_RGB16F;
        4: Result:= GL_RGBA16F;
      end;
    dtDouble:
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
    case PixelFormat^.BitsPerPixel of
      1: Result:= GL_R;
      2: Result:= GL_RG;
      3: Result:= GL_RGB;
      4: Result:= GL_RGBA;
      else
        Result:= GL_RGBA;
    end;

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

procedure TP3DTexture.SetWrapModeX(AValue: TP3DTextureWrap);
begin
  if FWrapModeX=AValue then Exit;
  FWrapModeX:=AValue;
  UpdateWrapMode();
end;

procedure TP3DTexture.SetWrapModeY(AValue: TP3DTextureWrap);
begin
  if FWrapModeY=AValue then Exit;
  FWrapModeY:=AValue;
  UpdateWrapMode();
end;

procedure TP3DTexture.UpdateFiltering(const ATarget: GLenum);
var
  _Target: GLenum;
begin
  if ( ATarget = 0 ) then
    _Target:= Target
  else
    _Target:= ATarget;
  glBindTexture( _Target, FGLTexture ); cle( Self );
  if ( MipMap ) then
    glTexParameteri( _Target, GL_TEXTURE_MIN_FILTER, P3DTextureFilteringMipMapToGL[ Filtering, FilteringMipMap ])
  else
    glTexParameteri( _Target, GL_TEXTURE_MIN_FILTER, P3DTextureFilteringToGL[ Filtering ]);
  cle( Self );

  glTexParameterf( _Target, GL_TEXTURE_MAG_FILTER, P3DTextureFilteringToGL[ Filtering ]); cle( Self );

  UpdateMipMaps( _Target );
end;

procedure TP3DTexture.UpdateWrapMode(const ATarget: GLenum);
var
  _Target: GLenum;
begin
  if ( ATarget = 0 ) then
    _Target:= Target
  else
    _Target:= ATarget;
  glBindTexture( _Target, FGLTexture ); cle( Self );

  glTexParameterf( _Target, GL_TEXTURE_MAG_FILTER, P3DTextureFilteringToGL[ Filtering ]); cle( Self, 'Error setting texture filtering to value = ' + GetEnumProp( Self, 'Filtering' ) + ': ' );

  glTexParameteri( _Target, GL_TEXTURE_WRAP_S, GLenum( WrapModeX )); cle( Self, 'Error setting texture wrap X to value = ' + GetEnumProp( Self, 'WrapModeX' ) + ': ' );
  glTexParameteri( _Target, GL_TEXTURE_WRAP_T, GLenum( WrapModeY )); cle( Self, 'Error setting texture wrap Y to value = ' + GetEnumProp( Self, 'WrapModeY' ) + ': ' );

  glBindTexture( _Target, 0 ); cle( Self );
end;

function TP3DTexture.CreateNewTexAndBind(const ATarget: GLenum): GLint;
var
  _Target: GLenum;
begin
  if ( ATarget = 0 ) then
    _Target:= Target
  else
    _Target:= ATarget;
  cle( Self );
  glGenTextures( 1, @Result ); cle( Self );
  glBindTexture( _Target, Result ); cle( Self );
end;

procedure TP3DTexture.LoadFromSurface(ASurface: PSDL_Surface; const ATarget: GLenum);
var
  Mode, InternalMode, _Target: GLenum;
  S: PSDL_Surface;
begin
  if ( ATarget = 0 ) then
    _Target:= Target
  else
    _Target:= ATarget;
  case ( ASurface^.format^.BytesPerPixel ) of
    1: Mode:= GL_R;
    2: Mode:= GL_RG;
    3: Mode:= GL_RGB;
    4: Mode:= GL_RGBA;
    else
      Mode:= GL_RGBA;
  end;

  InternalMode:= P3DChooseFormatBySDLPixelFormat( ASurface^.format );

  S:= SDL_FlipSurface( ASurface );

  glTexImage2D( _Target, 0, InternalMode, S^.w, S^.h, 0, Mode, GL_UNSIGNED_BYTE, S^.pixels ); cle( Self );

  SDL_FreeSurface( S );

  FFiltering:= tfLinear;
  FFilteringMipMap:= tfLinear;
  FMipMap:= True;
end;

procedure TP3DTexture.LoadFromBuffer(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType;
  ABuffer: Pointer; const ATarget: GLenum);
var
  Mode: Integer;
  InternalMode: Integer;
  Tex, _Target: GLint;
begin
  if ( ATarget = 0 ) then
    _Target:= Target
  else
    _Target:= ATarget;
  FDataType:= ADataType;
  case ( ANumChannels ) of
    1: Mode:= GL_R;
    2: Mode:= GL_RG;
    3: Mode:= GL_RGB;
    4: Mode:= GL_RGBA;
    else
      Mode:= ANumChannels;
  end;
  InternalMode:= P3DChooseFormatByType( ADataType, ANumChannels );
  glTexImage2D( _Target, 0, InternalMode, AWidth, AHeight, 0, Mode, GLenum( ADataType ), ABuffer ); cle( Self );

  FFiltering:= tfNearest;
  FFilteringMipMap:= tfNearest;
  FMipMap:= False;
  UpdateFiltering;
  glTexParameteri( _Target, GL_TEXTURE_WRAP_S, GL_REPEAT ); cle( Self );
  glTexParameteri( _Target, GL_TEXTURE_WRAP_T, GL_REPEAT ); cle( Self );
end;

procedure TP3DTexture.LoadEmpty(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType;
  const ATarget: GLenum);
var
  Mode: Integer;
  InternalMode: Integer;
  _Target: GLenum;
begin
  if ( ATarget = 0 ) then
    _Target:= Target
  else
    _Target:= ATarget;
  FDataType:= ADataType;
  case ( ANumChannels ) of
    1: Mode:= GL_R;
    2: Mode:= GL_RG;
    3: Mode:= GL_RGB;
    4: Mode:= GL_RGBA;
    else
      Mode:= ANumChannels;
  end;
  InternalMode:= P3DChooseFormatByType( ADataType, ANumChannels );
  if ( ADataType = dtDouble ) then
    glTexImage2D( _Target, 0, InternalMode, AWidth, AHeight, 0, Mode, GL_FLOAT, nil )
  else
    glTexImage2D( _Target, 0, InternalMode, AWidth, AHeight, 0, Mode, GLenum( ADataType ), nil ); cle( Self );

  FFiltering:= tfLinear;
  FFilteringMipMap:= tfLinear;
  FMipMap:= True;
  UpdateFiltering;
  glTexParameteri( _Target, GL_TEXTURE_WRAP_S, GL_REPEAT ); cle( Self );
  glTexParameteri( _Target, GL_TEXTURE_WRAP_T, GL_REPEAT ); cle( Self );
end;

procedure TP3DTexture.LoadFromGLTexture(Tex: GLint; const ATarget: GLenum);
var
  Filter: Integer;
  _Target: GLenum;
begin
  if ( ATarget = 0 ) then
    _Target:= Target
  else
    _Target:= ATarget;
  FGLTexture:= Tex;
  Bind();
  //glGetTexLevelParameteriv( _Target, 0, GL_TEXTURE_WIDTH, @Width ); cle( Self );
  //glGetTexLevelParameteriv( _Target, 0, GL_TEXTURE_HEIGHT, @Height ); cle( Self );
  //glGetTexParameteriv( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, @Filter );
  //case Filter of
  //  GL_LINEAR: FFiltering:= tfLinear;
  //  GL_NEAREST: FFiltering:= tfNearest;
  //end;
  glGetTexParameteriv( _Target, GL_TEXTURE_MAG_FILTER, @Filter );
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
  UpdateFiltering;
  FWidth:= FSurface^.w;
  FHeight:= FSurface^.h;
  FNumChannels:= FSurface^.format^.BytesPerPixel;

  SDL_FreeSurface( FSurface );
end;

procedure TP3DTexture.FileNameChanged(Sender: TP3DFilePointer; ANewFileName: String);
var
  Tex: GLint;
begin
  Tex:= CreateNewTexAndBind();
  LoadFromTextureFile( ANewFileName );
  if ( FGLTexture > 0 ) then
    glDeleteTextures( 1, @FGLTexture );
  FGLTexture:= Tex;
  UpdateFiltering();
end;

function TP3DTexture.GetFile(FN: String): String;
begin
  Result:= P3DSearchPaths.FindFileEx( FN, GetCurrentDirUTF8 );
  if ( Result = '' ) then
    Result:= FN;
end;

class function TP3DTexture.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Textures' );
end;

constructor TP3DTexture.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FFileWatch:= TP3DFilePointer.Create( '' );
  FileWatch.OnFileNameChange:= @FileNameChanged;
  //OwnerLib.Textures.Add( Self );

  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Width' ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Height' ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'GLTexture' ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Filtering', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'FilteringMipMap', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'MipMap', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'WrapModeX', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'WrapModeY', smText ));
  Properties.Add( TP3DPropAccessFilePointer.CreateField( 'File', @FFileWatch, smText, pmRelativeToJSONFile ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'DataType' ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'NumChannels' ));
  FTarget:= GL_TEXTURE_2D;
  FWrapModeX:= twRepeat;
  FWrapModeY:= twRepeat;
end;

procedure TP3DTexture.LoadFromBinary(FileName: String );
begin
  if (( FileName = FileWatch.FileName ) and ( not FileWatch.CheckForChange )) then
    begin
      {$IFDEF VERBOSE}
      WriteLn( 'File did not change. Skipped reload of ' + ExtractFileName( FileName ));
      {$ENDIF}
      exit;
    end;

  //inherited LoadFromBinary( FileName );
  FileWatch.OnFileNameChange:= nil;
  FileWatch.FileName:= FileName;
  FileWatch.OnFileNameChange:= @FileNameChanged;

  LoadFromTextureFile( FileName );
end;


constructor TP3DTexture.CreateFromFile(AFileName: String; const AData: TP3DLibrary);
var
  FileName: String;
begin
  FileName:= GetFile( AFileName );
  if ( FileName = '' ) then
    raise Exception.Create( Format( 'The specified file "%s" was not found!', [ AFileName ]));
  CreateBlock( AData );
  FGLTexture:= CreateNewTexAndBind();
  LoadFromBinary( FileName );
end;

constructor TP3DTexture.CreateFromBuffer(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; ABuffer: Pointer; const AData: TP3DLibrary);
begin
  CreateBlock( AData );
  FGLTexture:= CreateNewTexAndBind();
  LoadFromBuffer( AWidth, AHeight, ANumChannels, ADataType, ABuffer );
  FWidth:= AWidth;
  FHeight:= AHeight;
  FNumChannels:= ANumChannels;
end;

constructor TP3DTexture.CreateFromSurface(ASurface: PSDL_Surface; const AData: TP3DLibrary);
begin
  if ( not Assigned( glGenTextures )) then
    exit;
  CreateBlock( AData );

  FGLTexture:= CreateNewTexAndBind();
  LoadFromSurface( ASurface );
  UpdateFiltering;
  FWidth:= ASurface^.w;
  FHeight:= ASurface^.h;
  FNumChannels:= ASurface^.format^.BytesPerPixel;
end;

constructor TP3DTexture.CreateFromEmpty(AWidth, AHeight: Integer; ANumChannels: Integer; ADataType: TP3DTextureDataType; const AData: TP3DLibrary);
begin
  if ( not Assigned( glGenTextures )) then
    exit;
  CreateBlock( AData );

  FGLTexture:= CreateNewTexAndBind();
  LoadEmpty( AWidth, AHeight, ANumChannels, ADataType );

  FWidth:= AWidth;
  FHeight:= AHeight;
  FNumChannels:= ANumChannels;
end;

constructor TP3DTexture.CreateFromGLTexture(Tex: GLint; const AData: TP3DLibrary);
begin
  CreateBlock( AData );
  //FFileWatch:= TP3DFilePointer.Create( '' );
  LoadFromGLTexture( Tex );
  glGetTexLevelParameteriv( GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @FWidth ); cle( Self );
  glGetTexLevelParameteriv( GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @FHeight ); cle( Self );
  Unbind();
end;

constructor TP3DTexture.CreateFromGLTextureCube(PX, NX, PY, NY, PZ, NZ: GLint; const AData: TP3DLibrary);
const
  P3DCubeMapSideToGL: array [ TP3DCubeMapSides ] of GLenum = ( GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
                                                               GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
                                                               GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z );

  procedure AddCubeSide( Side: TP3DCubeMapSides; T: GLint );
  var
    Internal: GLint;
  begin
    glBindTexture( GL_TEXTURE_2D, T ); cle( Self );
    glGetTexLevelParameteriv( GL_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, @Width ); cle( Self );
    glGetTexLevelParameteriv( GL_TEXTURE_2D, 0, GL_TEXTURE_HEIGHT, @Height ); cle( Self );
    glGetTexLevelParameteriv( GL_TEXTURE_2D, 0, GL_TEXTURE_INTERNAL_FORMAT, @Internal ); cle( Self );
    glCopyTexImage2D( P3DCubeMapSideToGL[ side ], 0, Internal, 0, 0, Width, Height, 0 ); cle( Self );
    glDeleteTextures( 0, @T ); cle( Self );
    //LoadFromGLTexture( T, P3DCubeMapSideToGL[ side ]);
  end;

begin
  CreateBlock( AData );
  FTarget:= GL_TEXTURE_CUBE_MAP;
  FGLTexture:= CreateNewTexAndBind( Target );

  //FWidth:= PX^.w;
  //FHeight:= PX^.h;
  //FNumChannels:= PX^.format^.BytesPerPixel;

  AddCubeSide( cmsPX, PX );
  AddCubeSide( cmsNX, NX );
  AddCubeSide( cmsPY, PY );
  AddCubeSide( cmsNY, NY );
  AddCubeSide( cmsPZ, PZ );
  AddCubeSide( cmsNZ, NZ );
  UpdateFiltering( Target );
end;

constructor TP3DTexture.CreateFromFileCube(PX, NX, PY, NY, PZ, NZ: String; const AData: TP3DLibrary);
const
  P3DCubeMapSideToGL: array [ TP3DCubeMapSides ] of GLenum = ( GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
                                                               GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
                                                               GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z );

  procedure AddCubeSide( Side: TP3DCubeMapSides; FN: String );
  var
    FSurface: PSDL_Surface;
  begin
    FSurface:= IMG_Load( PChar( GetFile( FN )));
    if ( FSurface = nil ) then
      raise Exception.Create( Format( 'The specified file "%s" could not be loaded!', [ ExtractFileName( FN )]));
    LoadFromSurface( FSurface, P3DCubeMapSideToGL[ side ]);
    FWidth:= FSurface^.w;
    FHeight:= FSurface^.h;
    FNumChannels:= FSurface^.format^.BytesPerPixel;

    SDL_FreeSurface( FSurface );
  end;

begin
  CreateBlock( AData );
  FTarget:= GL_TEXTURE_CUBE_MAP;
  FGLTexture:= CreateNewTexAndBind( Target );

  AddCubeSide( cmsPX, PX );
  AddCubeSide( cmsNX, NX );
  AddCubeSide( cmsPY, PY );
  AddCubeSide( cmsNY, NY );
  AddCubeSide( cmsPZ, PZ );
  AddCubeSide( cmsNZ, NZ );
  UpdateFiltering( Target );
end;

constructor TP3DTexture.CreateFromBufferCube(AWidth, AHeight: Integer; ANumChannels: Integer;
  ADataType: TP3DTextureDataType; PX, NX, PY, NY, PZ, NZ: Pointer; const AData: TP3DLibrary);
const
  P3DCubeMapSideToGL: array [ TP3DCubeMapSides ] of GLenum = ( GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
                                                               GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
                                                               GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z );

  procedure AddCubeSide( Side: TP3DCubeMapSides; Buffer: Pointer );
  begin
    LoadFromBuffer( AWidth, AHeight, ANumChannels, ADataType, Buffer, P3DCubeMapSideToGL[ side ]);
  end;

begin
  CreateBlock( AData );
  FTarget:= GL_TEXTURE_CUBE_MAP;
  FGLTexture:= CreateNewTexAndBind( Target );

  FWidth:= AWidth;
  FHeight:= AHeight;
  FNumChannels:= NumChannels;

  AddCubeSide( cmsPX, PX );
  AddCubeSide( cmsNX, NX );
  AddCubeSide( cmsPY, PY );
  AddCubeSide( cmsNY, NY );
  AddCubeSide( cmsPZ, PZ );
  AddCubeSide( cmsNZ, NZ );
  UpdateFiltering( Target );
end;

constructor TP3DTexture.CreateFromSurfaceCube(PX, NX, PY, NY, PZ, NZ: PSDL_Surface; const AData: TP3DLibrary);
const
  P3DCubeMapSideToGL: array [ TP3DCubeMapSides ] of GLenum = ( GL_TEXTURE_CUBE_MAP_POSITIVE_X, GL_TEXTURE_CUBE_MAP_NEGATIVE_X,
                                                               GL_TEXTURE_CUBE_MAP_POSITIVE_Y, GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,
                                                               GL_TEXTURE_CUBE_MAP_POSITIVE_Z, GL_TEXTURE_CUBE_MAP_NEGATIVE_Z );

  procedure AddCubeSide( Side: TP3DCubeMapSides; S: PSDL_Surface );
  begin
    LoadFromSurface( S, P3DCubeMapSideToGL[ side ]);
    FWidth:= S^.w;
    FHeight:= S^.h;
    FNumChannels:= S^.format^.BytesPerPixel;
  end;

begin
  CreateBlock( AData );
  FTarget:= GL_TEXTURE_CUBE_MAP;
  FGLTexture:= CreateNewTexAndBind( Target );

  FWidth:= PX^.w;
  FHeight:= PX^.h;
  FNumChannels:= PX^.format^.BytesPerPixel;

  AddCubeSide( cmsPX, PX );
  AddCubeSide( cmsNX, NX );
  AddCubeSide( cmsPY, PY );
  AddCubeSide( cmsNY, NY );
  AddCubeSide( cmsPZ, PZ );
  AddCubeSide( cmsNZ, NZ );
  UpdateFiltering( Target );
end;

procedure TP3DTexture.LoadMipMapFromSurface(level: Integer; ASurface: PSDL_Surface; const ATarget: GLenum);
var
  Mode: GLenum;
  InternalMode, _Target: GLenum;
begin
  if ( ATarget = 0 ) then
    _Target:= Target
  else
    _Target:= ATarget;
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

  glTexImage2D( _Target, level, InternalMode, ASurface^.w, ASurface^.h, 0, Mode, GL_UNSIGNED_BYTE, ASurface^.pixels ); cle( Self );
end;

procedure TP3DTexture.Bind(const Location: Integer);
begin
  glActiveTexture( GL_TEXTURE0 + Location );
  glBindTexture( Target, GLTexture );
end;

procedure TP3DTexture.Unbind();
begin
  glBindTexture( Target, 0 );
end;

destructor TP3DTexture.Destroy;
begin
  if ( Assigned( glDeleteTextures )) then
    glDeleteTextures( 1, @FGLTexture ); cle( Self );
  FileWatch.Free;

  inherited Destroy;
end;

procedure TP3DTexture.Reload;
var
  tex: Integer;
begin
  tex:= FGLTexture;
  LoadFromTextureFile( FileWatch.FileName );
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
  surface, s: PSDL_Surface;
begin
  Bind();
  GetMem( Pixels, Width * Height * 4 );
  glGetTexImage( Target, 0, GL_RGBA, GL_UNSIGNED_BYTE, Pixels );
  surface:= SDL_CreateRGBSurfaceFrom( Pixels, Width, Height, 32, Width * 4, $000000FF, $0000FF00, $00FF0000, $FF000000 );
  s:= SDL_FlipSurface( surface );
  IMG_SavePNG( s, PChar( FileName ));
  SDL_FreeSurface( surface );
  SDL_FreeSurface( S );
  Freemem( Pixels, Width * Height * 4 );
end;

procedure TP3DTexture.UpdateMipMaps(const ATarget: GLenum);
var
  _Target: GLenum;
begin
  if ( FGLTexture = 0 ) then
    exit;
  if ( ATarget = 0 ) then
    _Target:= Target
  else
    _Target:= ATarget;

  glBindTexture( _Target, FGLTexture ); cle( Self );
  glGenerateMipmap( _Target ); cle( Self, 'generating mip map failed with target: ' + IntToHex( _Target, 4 ));

  glBindTexture( _Target, 0 ); cle( Self );
end;

function TP3DTexture.GetLayer(ASettings: TP3DPlotListSetting): TP3DPlotCommandLayer;
var
  indices: TP3DIntBufferGLAuto;
begin
  indices:= TP3DIntBufferGL.Create;
  indices.I.AddArray([ 0, 2, 1, 1, 2, 3 ]);
  indices.I.PushData;                     //[ vec3( -hSize.X, hSize.Y, 0 ), hSize, -hSize, vec3( hSize.X, -hSize.Y, 0 )]
  Result:= geom_polygon([ vec3( -1, 1, 0 ), vec3( 1, 1, 0 ), vec3( -1, -1, 0 ), vec3( 1, -1, 0 )], indices,
                          settings_join([ ASettings,
                            settings([
                              attrib( P3DAttribNormal, [ vec3( 0, 0, 1 ), vec3( 0, 0, 1 ), vec3( 0, 0, 1 ), vec3( 0, 0, 1 )]),
                              attrib( 'TexCoord0', [ vec2( 0, 1 ), vec2( 1, 1 ), vec2( 0, 0 ), vec2( 1, 0 )]),
                              uniform( 'tex0', Self )
                            ])
                          ])
                        );
end;

{$ENDIF}
