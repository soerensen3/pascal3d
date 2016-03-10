program __PROJNAME__;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
  {$R *.res}
{$ENDIF}

uses
  strutils,
  LCLIntf,
  sysutils,
  Classes,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  Interfaces, //crashes in linux without
  {$ENDIF}{$ENDIF}
  p3dSDLApplication,
  p3dwindow,
  p3dlogging,
  __MAINUNIT__;


Begin
  try
    P3DLog.FileName:= '__PROJNAME__.xml';
    P3DApplication.MainWindow:= TSDLWindow.Create;
    P3DApplication.OnInit:= @Init;
    P3DApplication.OnDeinit:= @DeInit;

    P3DApplication.MainWindow.OnRender:= @__MAINUNIT__.Render;
    P3DApplication.OnMouseButton:= @__MAINUNIT__.OnMouseButton;
    P3DApplication.OnMouseMotion:= @__MAINUNIT__.OnMouseMotion;
    P3DApplication.OnMouseWheel:= @__MAINUNIT__.OnMouseWheel;
    P3DApplication.OnKey:= @__MAINUNIT__.OnKey;
    P3DApplication.OnInput:= @__MAINUNIT__.OnInput;
    P3DApplication.MainWindow.OnResize:= @__MAINUNIT__.OnWndResize;

    P3DApplication.Initialize;
    P3DApplication.Run;
  except
    On E: Exception do
      P3DLog.LogException( nil, E );
  end;
  P3DApplication.MainWindow.Free;
End.
