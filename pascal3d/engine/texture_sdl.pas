unit texture_sdl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sdl2, sdl2_image, dglOpenGL;

type
  {PSDL_SW_YUVTexture = Pointer;
  PSDL_Texture = ^TSDL_Texture;
  TSDL_Texture = record
    magic: Pointer;
    format: Uint32;
    access: Integer;
    w: SInt32;
    h: SInt32;
    modMode: SInt32;
    blendMode: TSDL_BlendMode;
    r: UInt8;
    g: UInt8;
    b: UInt8;
    a: UInt8;
    renderer: PSDL_Renderer;
    native: PSDL_Texture;
    yuv: PSDL_SW_YUVTexture;
    pixels: Pointer;
    pitch: SInt32;
    locked_rect: TSDL_Rect;

    driverdata: Pointer;
    prev: PSDL_Texture;
    next: PSDL_Texture;
  end;}

  { TSDLTexture }

  TSDLSurface = class
  private
    fFileName: String;
  public
    fSurface: PSDL_Surface;
    fGLTexture: Integer;

    constructor Create(AName: String );
    destructor Destroy; override;

    property FileName: String read fFileName;
  end;

implementation

{ TSDLTexture }

constructor TSDLSurface.Create( AName: String );
var
  Mode: Integer;
begin
  if ( not FileExists( AName )) then
    raise Exception.Create( Format( 'The specified file "%s" was not found!', [ AName ]));
  fSurface:= IMG_Load( PChar( AName ));
  fFileName:= AName;
  if ( fSurface = nil ) then
    raise Exception.Create( Format( 'The specified file "%s" could not be loaded!', [ AName ]));

  WriteLn( 'Texture loaded: ' + AName );
  glGenTextures(1, @fGLTexture );
  glBindTexture(GL_TEXTURE_2D, fGLTexture );

  if ( fSurface^.format^.BytesPerPixel = 4 ) then
    Mode:= GL_RGBA
  else
    Mode:= GL_RGB;

  glTexImage2D( GL_TEXTURE_2D, 0, Mode, fSurface^.w, fSurface^.h, 0, Mode, GL_UNSIGNED_BYTE, fSurface^.pixels );

  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER );
  glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER );
end;

destructor TSDLSurface.Destroy;
begin
  SDL_FreeSurface( fSurface );
  glDeleteTextures( 1, @fGLTexture );
  inherited Destroy;
end;

end.

