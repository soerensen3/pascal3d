//p3dtexture.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
type
  TP3DTextureRepeat = ( trRepeat = GL_REPEAT, trRepeatMiror = GL_MIRRORED_REPEAT );
  TP3DTextureFiltering = ( tfNearest, tfLinear );

const
  P3DTextureFilteringToGL: array [ tfNearest..tfLinear ] of Cardinal = ( GL_NEAREST, GL_LINEAR );
  P3DTextureFilteringMipMapToGL: array [ tfNearest..tfLinear, tfNearest..tfLinear ] of Cardinal = (( GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST ), ( GL_NEAREST_MIPMAP_LINEAR, GL_LINEAR_MIPMAP_LINEAR ));

type
  { TP3DTexture }

  TP3DTexture = class ( TP3DDataBlock )
    private
      FFiltering: TP3DTextureFiltering;
      FFilteringMipMap: TP3DTextureFiltering;
      FGLTexture: Integer;
      FHeight: Integer;
      FMipMap: Boolean;
      FWidth: Integer;
      procedure SetFiltering(AValue: TP3DTextureFiltering);
      procedure SetFilteringMipMap(AValue: TP3DTextureFiltering);
      procedure SetMipMap(AValue: Boolean);
      procedure UpdateFiltering;
      procedure LoadFromSurface( ASurface: PSDL_Surface );

    public
      constructor Create( AName: String; const AData: TP3DData = nil );
      constructor CreateFromSurface( ASurface: PSDL_Surface; const AData: TP3DData = nil );
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil );
      destructor Destroy; override;

    published
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property GLTexture: Integer read FGLTexture write FGLTexture;
      property Filtering: TP3DTextureFiltering read FFiltering write SetFiltering;
      property FilteringMipMap: TP3DTextureFiltering read FFilteringMipMap write SetFilteringMipMap;
      property MipMap: Boolean read FMipMap write SetMipMap;
  end;
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
  Mode: Integer;
begin
  glGenTextures( 1, @FGLTexture ); cle( Self );
  glBindTexture( GL_TEXTURE_2D, FGLTexture ); cle( Self );

  if ( ASurface^.format^.BytesPerPixel = 4 ) then
    Mode:= GL_RGBA
  else
    Mode:= GL_RGB;

  glTexImage2D( GL_TEXTURE_2D, 0, Mode, ASurface^.w, ASurface^.h, 0, Mode, GL_UNSIGNED_BYTE, ASurface^.pixels ); cle( Self );

  glGenerateMipmap( GL_TEXTURE_2D ); cle( Self );

  FFiltering:= tfLinear;
  FFilteringMipMap:= tfLinear;
  FMipMap:= True;
  UpdateFiltering;
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT ); cle( Self );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT ); cle( Self );

  FWidth:= ASurface^.w;
  FHeight:= ASurface^.h;

  SDL_FreeSurface( ASurface );
end;

constructor TP3DTexture.Create( AName: String; const AData: TP3DData );
var
  FSurface: PSDL_Surface;
  FileName: String;
begin
  FileName:= P3DSearchPaths.Models.FindFileEx( AName, GetCurrentDir );
  if ( FileName = '' ) then
    raise Exception.Create( Format( 'The specified file "%s" was not found!', [ AName ]));
  inherited Create( AData );
  FSurface:= IMG_Load( PChar( FileName ));
  FileWatch.FileName:= FileName;
  if ( FSurface = nil ) then
    raise Exception.Create( Format( 'The specified file "%s" could not be loaded!', [ AName ]));

  {$IFDEF VERBOSE}
  WriteLn( 'Texture loaded: ' + AName );
  {$ENDIF}
  LoadFromSurface( FSurface );
end;

constructor TP3DTexture.CreateFromSurface(ASurface: PSDL_Surface;
  const AData: TP3DData);
begin
  if ( not Assigned( glGenTextures )) then
    exit;
  inherited Create( AData );
  LoadFromSurface( ASurface );
end;

constructor TP3DTexture.CreateFromDOM(ADOMNode: TDOMElement;
  const AData: TP3DData);
var
  TexName: DOMString;
  FullTexName: String;
begin
  FullTexName:= P3DSearchPaths.Models.FindFileEx( ADOMNode.GetAttribute( 'file' ), GetCurrentDir );

  if ( not FileExists( FullTexName )) then
    raise Exception.Create( 'Error: The specified texture "' + TexName + '" could not be found!' );

  Create( FullTexName, AData );
  FMipMap:= P3DStrToBoolDef( ADOMNode.GetAttribute( 'mipmap' ), True );
  FFilteringMipMap:= StrToFilterDef( ADOMNode.GetAttribute( 'mipmap_filter' ), tfLinear );
  FFiltering:= StrToFilterDef( ADOMNode.GetAttribute( 'texture_filter' ), tfLinear );
  UpdateFiltering;
end;

destructor TP3DTexture.Destroy;
begin
  if ( Assigned( glDeleteTextures )) then
    glDeleteTextures( 1, @FGLTexture ); cle( Self );
  inherited Destroy;
end;
{$ENDIF}
