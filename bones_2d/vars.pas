unit vars;

{$mode objfpc}{$H+}
{$DEFINE USE_ZENGL_STATIC}

interface

uses
  zglHeader,
  Classes,
  SysUtils,
  compo,

  {$IFDEF USE_ZENGL_STATIC}
  // RU: При использовании статической компиляции необходимо подключать модули ZenGL содержащие необходимый функционал.
  // EN: Using static compilation needs to use ZenGL units with needed functionality.
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_utils,
  zgl_mouse,
  zgl_font,
  zgl_text,
  zgl_primitives_2d,
  zgl_sprite_2d,
  zgl_textures
  {$ELSE}
  // RU: Используя ZenGL в качестве библиотеки(so, dll или dylib) нужен всего один заголовочный файл.
  // EN: Using ZenGL as a shared library(so, dll or dylib) needs only one header.
  zglHeader
  {$ENDIF}
  ;

var
  dirRes     : UTF8String {$IFNDEF MACOSX} = 'data/' {$ENDIF};
  {.$DEFINE FULLSCREEN}
  {$IFDEF FULLSCREEN}
  XRes       : Integer = 1280;
  YRes       : Integer = 1024;
  HalfX      : Integer = 640;
  HalfY      : Integer = 512;
  Fullscreen : Boolean = True;
  {$ELSE}
  XRes       : Integer = 800;
  YRes       : Integer = 600;
  HalfX      : Integer = 400;
  HalfY      : Integer = 300;
  Fullscreen : Boolean = False;
  {$ENDIF}

  step       : Single;
  laststep   : QWord;

  fntMain    : zglPFont;

  joyCount   : Byte;

  Buttons    : TGraphicList;
  Hud        : TGraphicList;

  mx, my     : Integer;


implementation

end.

