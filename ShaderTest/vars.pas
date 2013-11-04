unit vars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_mouse,
  zgl_keyboard,
  zgl_joystick,
  zgl_textures,
  zgl_sprite_2d,
  zgl_primitives_2d,
  zgl_font,
  zgl_text,
  zgl_textures_png,
  zgl_math_2d,
  zgl_collision_2d,
  zgl_utils,
  zgl_render,
  LCLIntf,
  zgl_opengl,
  zgl_opengl_all,
  Math3D,
  dglOpenGL,
  wavefront,
  tiles3d,
  shaders;

type
  {TMP}
  TCam = record
    P: TVector;
    R: TVector;
  end;

var
  dirRes  : UTF8String {$IFNDEF MACOSX} = '/fonts/' {$ENDIF};

  fntMain   : zglPFont;

  joyCount   : Integer;
  userInput  : UTF8String;
  trackInput : Boolean;
  inputRect  : zglTRect;
  lineAlpha  : Byte;

  rtri: GLfloat = 0;
  rquad: GLfloat = 0;

  TileSet: TTileSet;
  TileMap: TTileMap;

  Character_Stand: zglPTexture;
  Character_Walk: zglPTexture;
  metal: zglPTexture;

  offx, offy: Integer;
  zoom: Single;
  mx, my: Integer;

  dir: Byte;
  walking: Boolean;
  speed: Integer;
  pos: zglTPoint2D;
  xrot, yrot: Real;

  SunDir: TVector;


  mdl: TModel;
  water: TModel;
  skymap: zglPTexture;
  pointmap: zglPTexture;

  watershader: GLHandleARB;
  pplight: GLHandleARB;
  simpleshader: GLHandleARB;

  Point1, Point2: TVector;

  world, view, proj,
    view3x3: Math3D.TMatrix4f;

  Cam: TCam;

{  Wall_Normal,
  Wall_Corner,
  Floor_Normal: TModel;}


implementation

end.

