program engineplayer;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
  {$R *.res}
{$ENDIF}

{.$DEFINE DEBUG_DEFERRED}

uses
  strutils,
  LCLIntf,
  SDL2,
  dglOpenGL,
  p3dwindow,
  p3dSDLApplication,
  sysutils,
  Classes,
  Interfaces, //crashes in linux without
  p3dshaders,
  Math,
  p3dMath,
  p3dmodel,
  p3dinput,
  p3dframebuffer,
  p3dfilewatch,
  p3dgeometry,
  p3dobjects,
  p3dgui,
  p3dgui_buttons,
  p3dgui_stdctrls,
  p3dgui_menus,
  p3dtext,
  p3dcanvas,
  //p3dshadernodes,
  //p3dgui_shadernodes,
  p3dNodes,
  p3dviewport,
  p3dgenerics,
  p3dmarkdown,
  XMLRead,
  DOM,
  p3dlogging
  //p3dgui_sceneviewer
  ;

{$DEFINE INTERFACE}
         {$INCLUDE renderscene.inc}

{$INCLUDE initscene.inc}

{$INCLUDE renderscene.inc}

{$INCLUDE inputscene.inc}

Begin
  //SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
  //                  exOverflow, exUnderflow, exPrecision]);
  P3DLog.FileName:= 'engineplayer.xml';
  P3DApplication.MainWindow:= TSDLWindow.Create;
  P3DApplication.OnInit:= @Init;
  P3DApplication.OnDeinit:= @DeInit;

  P3DApplication.MainWindow.OnRender:= @Render;
  P3DApplication.OnMouseButton:= @OnMouseButton;
  P3DApplication.OnMouseMotion:= @OnMouseMotion;
  P3DApplication.OnMouseWheel:= @OnMouseWheel;
  P3DApplication.OnKey:= @OnKey;
  P3DApplication.OnInput:= @OnInput;
  P3DApplication.MainWindow.OnResize:= @ResizeWnd;

  P3DApplication.Initialize;

  P3DApplication.Run;

  P3DApplication.MainWindow.Free;
End.
