unit vars;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
{  zgl_main,
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
  zgl_render_target,}
  zglHeader,
  Math3D,
  dglOpenGL,
  shaders,
  Model,
  framebuffer,
  lighting;

var
  fntMain   : zglPFont;

  testTex: zglPTexture;
  specTex: zglPTexture;
  cursorTex: zglPTexture;

  mx, my: Integer;
  xrot, yrot, Zoom: Single;
  Lxrot1, Lyrot1, LZoom1: Single;
  Lxrot2, Lyrot2, LZoom2: Single;
  LS: Integer = 0;
  CamPos: TVec4;

  simpleshader: TShader;
  shadowshader: TShader;
  depthshader: TShader;
//  depthblurshader: TShader;
  HBlur, VBlur: TShader;
  world, view, proj,
    invview: TMat4;
  view3x3: TMat3;

  ShadowFB: TFrameBufferTex2D;
  ShadowRes: Integer;
  LightSource: TLightList;

  Mdl: TModelFile;
  lightbulb: TModelFile;
  Mdlsphere: TModelFile;
  sphere: TVec4;
  sphcoll: Boolean;

  timeGone: Integer;

implementation

end.
