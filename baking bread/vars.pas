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
  wavefront,
  shaders;
type
  {TMP}
  TCam = record
    P: TVector;
    R: TVector;
  end;

  TBreadType = ( btBread, btRoll );
  TBread = record
    P: TVector;
    time: Single;
    btype: TBreadType;
    finished: Boolean;
  end;

var
  dirRes  : UTF8String {$IFNDEF MACOSX} = '/fonts/' {$ENDIF};

  fntMain   : zglPFont;

  joyCount   : Integer;

  mx, my: Integer;

  dMove, dOff: TVector;

  bread,
    roll,
    kitchen,
    oven,
    bowl,
    dough: TModel;

  pplight: GLHandleARB;
  breadshader: GLHandleARB;
  simpleshader: GLHandleARB;
  shadowshader_pass1: GLHandleARB;

  ShadowRT: zglPRenderTarget;

  matShadow: Math3D.TMatrix4f;


  doughmap: zglPTexture;
  titlemap: zglPTexture;

  titleAlpha: Single = 3;

  lightDir: TVector;
  breads: array of TBread;
  breadSel: Integer;
  breadInOven: Integer = - 1;
  ovenHighLight: Boolean;
  bowlHighLight: Boolean;
  ovenMat: Math3D.TMatrix4f;

  doughSize: Single;

  world, view, proj,
    view3x3, invview: Math3D.TMatrix4f;

  Cam: TCam;

  timeGone: Integer;

  Score: Integer;
  Msg: String;
  MsgAlpha: Integer;
  MsgEnd: String;

implementation

end.

