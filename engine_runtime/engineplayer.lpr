program engineplayer;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
  {$R *.res}
{$ENDIF}

{.$DEFINE DEBUG_DEFERRED}

uses
  //heaptrc,
  strutils,
  LCLIntf,
  SDL2,
  dglOpenGL,
  p3dutils,
  p3devents,
  p3dgraphics,
  sysutils,
  Classes,
  Interfaces, //crashes in linux without
  Math,
  p3dMath,
  p3dgui,
  XMLRead,
  DOM
  ;

{$DEFINE INTERFACE}
  {$INCLUDE renderscene.inc}

{$INCLUDE initscene.inc}

{$INCLUDE renderscene.inc}

{$INCLUDE inputscene.inc}

Begin
  // Assuming your build mode sets -dDEBUG in Project Options/Other when defining -gh
  // This avoids interference when running a production/default build without -gh

  // Set up -gh output for the Leakview package:
  {if FileExists('heap.trc') then
    DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
                    exOverflow, exUnderflow, exPrecision]);}
  P3DEventsInit;
  P3DLog.FileName:= 'engineplayer.xml';
  P3DApplication.MainWindow:= TP3DWindow.Create;
  //P3DApplication.OnInit:= @Init;
  P3DApplication.OnDeinit:= @DeInit;

  P3DApplication.MainWindow.OnRender:= @Render;
  P3DApplication.OnMouseButton:= @OnMouseButton;
  P3DApplication.OnMouseMotion:= @OnMouseMotion;
  P3DApplication.OnMouseWheel:= @OnMouseWheel;
  P3DApplication.OnKey:= @OnKey;
  P3DApplication.OnInput:= @OnInput;
  P3DApplication.MainWindow.OnResize:= @ResizeWnd;

  P3DApplication.Initialize;
  P3DUtilsInit;
  P3DLoadConfig( '../../../settings_default.xml' );
  P3DGraphicsInit;

  Init( P3DApplication );

  P3DApplication.Run;

  P3DUtilsFinish;
  P3DGraphicssFinish;
  P3DApplication.MainWindow.Free;
  P3DEventsFinish;
  SDL_Quit()
End.
