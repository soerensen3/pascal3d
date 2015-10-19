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
  vars,
  p3dinput,
  p3dscene,
  p3dframebuffer,
  p3dtexture,
  p3dfilewatch,
  p3dgeometry,
  p3dobjects,
  p3dgui,
  p3dgui_buttons,
  p3dgui_stdctrls,
  p3dgui_menus,
  p3dtext,
  p3dcanvas,
  p3dshadernodes,
  p3dgui_shadernodes,
  p3dNodes,
  p3dviewport,
  p3dgenerics,
  XMLRead,
  DOM,
  p3dgui_sceneviewer;

{$DEFINE INTERFACE}
         {$INCLUDE renderscene.inc}

{$INCLUDE initscene.inc}

{$INCLUDE renderscene.inc}

{$INCLUDE inputscene.inc}

Begin
  P3DApplication.Initialize;

  mainwnd:= TSDLWindow.Create;
  P3DApplication.MainWindow:= mainwnd;
  mainwnd.OnInit:= @Init;
  mainwnd.OnDeinit:= @DeInit;
  Init( mainwnd );

  mainwnd.OnRender:= @Render;
  P3DApplication.OnMouseButton:= @OnMouseButton;
  P3DApplication.OnMouseMotion:= @OnMouseMotion;
  P3DApplication.OnMouseWheel:= @OnMouseWheel;
  P3DApplication.OnKey:= @OnKey;
  P3DApplication.OnInput:= @OnInput;
  mainwnd.OnResize:= @ResizeWnd;


  P3DApplication.Run;
  mainwnd.Free;
End.
