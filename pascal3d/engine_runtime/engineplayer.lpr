program engineplayer;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
  {$R *.res}
{$ENDIF}

{.$DEFINE DEBUG_DEFERRED}

uses
//  {.$IFDEF USE_ZENGL_STATIC}
  strutils,
  LCLIntf,
  dglOpenGL,
  window_sdl,
  sdl2,
  sysutils,
  Classes,
  Interfaces, //crashes in linux without
  shaders,
  Math,
  Math3d,
  Model,
  loadmodelfile,
  vars,
  scene,
  framebuffer,
  lighting,
  RPhysics,
  Newton,
  texture_sdl,
  filewatch,
  charactercontroller,
  geometry;
//  zglHeader;

{$DEFINE INTERFACE}
         {$INCLUDE renderscene.inc}

{$INCLUDE initscene.inc}

{$INCLUDE renderscene.inc}

{$INCLUDE inputscene.inc}

Begin
{  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Render );
  zgl_Reg( SYS_EXIT, @DeInit );
  timer_Add( @Input, 16 );}
  mainwnd:= TSDLWindow.Create;
  mainwnd.OnInit:= @Init;
  mainwnd.OnDeinit:= @DeInit;
  mainwnd.OnRender:= @Render;
  mainwnd.OnMouseButton:= @OnMouseButton;
  mainwnd.OnMouseMotion:= @OnMouseMotion;
  mainwnd.OnMouseWheel:= @OnMouseWheel;
  mainwnd.OnKey:= @OnKey;
  mainwnd.OnInput:= @OnInput;
  mainwnd.OnResize:= @ResizeWnd;
//  wnd_ShowCursor( False );
//  wnd_SetCaption( 'model test' );

//  wnd_ShowCursor( TRUE );

//  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

//  zgl_Init();
  mainwnd.Run;
  mainwnd.Free;
End.