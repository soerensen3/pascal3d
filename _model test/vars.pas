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
  zgl_render_target,
  Math3D,
  dglOpenGL,
  shaders,
  Model;

var
  fntMain   : zglPFont;

  mx, my: Integer;
  xrot, yrot, Zoom: Single;
  CamPos: TVec3;

  simpleshader: GLHandleARB;
  world, view, proj,
    invview: TMat4;
  view3x3: TMat3;

  Mdl: TModelFile;

  timeGone: Integer;

implementation

end.

