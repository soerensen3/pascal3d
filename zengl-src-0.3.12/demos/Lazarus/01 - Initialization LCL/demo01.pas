unit demo01;

{$I zglCustomConfig.cfg}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,

{$IFDEF LINUX}
  {$IFDEF LCLGTK2}
  GTK2, GDK2x, GTK2Proc,
  {$ENDIF}
{$ENDIF}

{$IFDEF MACOSX}
  CarbonPrivate,
{$ENDIF}

  {$IFDEF USE_ZENGL_STATIC}
  zgl_main,
  zgl_window,
  zgl_screen,
  zgl_timers,
  zgl_primitives_2d,
  zgl_utils
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Panel1: TPanel;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Panel1Resize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

  zglInited  : Boolean;
  zglResized : Boolean;

implementation

{$R *.lfm}

procedure Init;
begin
  // RU: Вертикальная синхронизация поможет избежать загрузки процессора.
  // EN: Vertical synchronization will decrease a CPU loading.
  scr_SetVSync( TRUE );

  // RU: Перед стартом необходимо настроить viewport.
  // EN: Before the start need to configure a viewport.
  wnd_SetPos( Form1.Panel1.Left, Form1.Panel1.Top );
  wnd_SetSize( Form1.Panel1.Width, Form1.Panel1.ClientHeight );

  Form1.BringToFront();
end;

procedure Draw;
begin
  // RU: Необходимо обновлять viewport как только изменились размеры контрола, куда был инициализирован ZenGL.
  // EN: Viewport should be updated as soon as size of control was changed.
  if zglResized Then
    begin
      zglResized := FALSE;
      wnd_SetPos( Form1.Panel1.Left, Form1.Panel1.Top );
      wnd_SetSize( Form1.Panel1.ClientWidth, Form1.Panel1.ClientHeight );
    end;

  pr2d_Rect( 10, 10, 800 - 30, 600 - 30, $FF0000, 255 );

  // RU: Т.к. ZenGL перехватывает "управление" нужно выполнять обработку интерфейса вручную.
  // EN: Because ZenGL intercepts "control" you need to call process of GUI manually.
  Application.ProcessMessages();
end;

procedure Timer;
begin
  Form1.Caption := '01 - Initialization [ FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) + ' ]';
end;

procedure UpdateDT( dt : Double );
begin
end;

{ TForm1 }

procedure TForm1.FormActivate(Sender: TObject);
{$IFDEF LINUX}
  var
    widget : PGtkWidget;
{$ENDIF}
begin
  if not zglInited Then
    begin
      zglInited := TRUE;
      {$IFNDEF USE_ZENGL_STATIC}
      zglLoad( libZenGL );
      {$ENDIF}

      zgl_Reg( SYS_LOAD, @Init );
      zgl_Reg( SYS_DRAW, @Draw );
      // RU: Стоит обратить внимание на название регистрируемой функции, т.к. Update является методом TForm.
      // EN: Take a look on name of function which will be registered, because Update is a method of TForm.
      zgl_Reg( SYS_UPDATE, @UpdateDT );

      wnd_ShowCursor( TRUE );

    {$IFDEF LINUX}
      {$IFDEF LCLGTK2}
      widget := GetFixedWidget( PGtkWidget( Panel1.Handle ) );
      gtk_widget_realize( widget );
      zgl_InitToHandle( GDK_WINDOW_XID( widget.window ) );
      {$ENDIF}
    {$ENDIF}

    {$IFDEF WINDOWS}
      zgl_InitToHandle( Panel1.Handle );
    {$ENDIF}

    {$IFDEF MACOSX}
      // RU: В MacOS X инициализироваться нужно в форму, даже если рисовать надо в другом контроле.
      // EN: For MacOS X initialization should be done into form, even if rendering will be into another control.
      zgl_InitToHandle( LongWord( TCarbonWindow( Form1.Handle ).Window ) );
    {$ENDIF}

      Application.Terminate();
    end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if zglInited Then
    begin
      zglInited := FALSE;
      zgl_Exit();
    end;
end;

procedure TForm1.Panel1Resize(Sender: TObject);
begin
  // RU: Установим флаг, что размер контрола изменился.
  // EN: Set a flag that size of control has been changed.
  zglResized := TRUE;
end;

end.

