unit p3dwindow;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, sdl2, dglOpenGL;

const
  TMouseEvent: array [ 0..3 ] of Word = ( SDL_MOUSEMOTION, SDL_MOUSEBUTTONDOWN,
                SDL_MOUSEBUTTONUP, SDL_MOUSEWHEEL );
  TJoystickEvent: array [ 0..6 ] of Word = ( SDL_JOYAXISMOTION,
                SDL_JOYBALLMOTION, SDL_JOYHATMOTION, SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP,
                SDL_JOYDEVICEADDED, SDL_JOYDEVICEREMOVED );
  TGameControllerEvent: array[ 0..5 ] of Word = ( SDL_CONTROLLERAXISMOTION,
                SDL_CONTROLLERBUTTONDOWN, SDL_CONTROLLERBUTTONUP, SDL_CONTROLLERDEVICEADDED,
                SDL_CONTROLLERDEVICEREMOVED, SDL_CONTROLLERDEVICEREMAPPED );
  TTouchEvent: array [ 0..2 ] of Word = ( SDL_FINGERDOWN, SDL_FINGERUP, SDL_FINGERMOTION );
  TGestureEvent: array [ 0..2 ] of Word = ( SDL_DOLLARGESTURE, SDL_DOLLARRECORD, SDL_MULTIGESTURE );

type

  { TSDLWindow }

  TSDLWindow = class;

  TWindowEvent            = procedure( Sender: TSDLWindow );
  TWindowEventKey         = procedure( Sender: TSDLWindow; Event: TSDL_KeyboardEvent );
  TWindowEventMouseButton = procedure( Sender: TSDLWindow; Event: TSDL_MouseButtonEvent );
  TWindowEventMouseMotion = procedure( Sender: TSDLWindow; Event: TSDL_MouseMotionEvent );
  TWindowEventMouseWheel  = procedure( Sender: TSDLWindow; Event: TSDL_MouseWheelEvent );

  TSDLWindow = class
    private
      fFullScreen: Boolean;
      FOnClose: TWindowEvent;
      fOnDeinit: TWindowEvent;
      fOnInit: TWindowEvent;
      fOnInput: TWindowEvent;
      FOnKey: TWindowEventKey;
      FOnMouseButton: TWindowEventMouseButton;
      FOnMouseMotion: TWindowEventMouseMotion;
      FOnMouseWheel: TWindowEventMouseWheel;
      fOnRender: TWindowEvent;
      FOnResize: TWindowEvent;
      fTitle: String;
      FVisible: Boolean;
      fWindow: PSDL_Window;
      context: TSDL_GLContext;
      renderer: PSDL_Renderer;
//      ReadingTextInput: Boolean;

      function GetHeight: Integer;
      function GetLeft: Integer;
      function GetTitle: String;
      function GetTop: Integer;
      function GetWidth: Integer;
      procedure SetFullScreen(AValue: Boolean);
      procedure SetHeight(AValue: Integer);
      procedure SetLeft(AValue: Integer);
      procedure SetTitle(AValue: String);
      procedure SetTop(AValue: Integer);
      procedure SetVisible(AValue: Boolean);
      procedure SetWidth(AValue: Integer);

    public
      procedure SetViewportOpenGL(w,h: Integer);
      procedure Render;

      constructor Create;
      constructor CreateFrom( Wnd: Pointer );

      procedure Show;
      procedure Hide;

      function GetRenderer: PSDL_Renderer; inline;

      property OnRender: TWindowEvent read fOnRender write fOnRender;
      property OnInit: TWindowEvent read fOnInit write fOnInit;
      property OnDeinit: TWindowEvent read fOnDeinit write fOnDeinit;
      property OnMouseMotion: TWindowEventMouseMotion read FOnMouseMotion write FOnMouseMotion;
      property OnMouseButton: TWindowEventMouseButton read FOnMouseButton write FOnMouseButton;
      property OnMouseWheel: TWindowEventMouseWheel read FOnMouseWheel write FOnMouseWheel;
      property OnKey: TWindowEventKey read FOnKey write FOnKey;
      property OnInput: TWindowEvent read fOnInput write fOnInput;
      property OnResize: TWindowEvent read FOnResize write FOnResize;
      property OnClose: TWindowEvent read FOnClose write FOnClose;

      property Title: String read GetTitle write SetTitle;
      property Width: Integer read GetWidth write SetWidth;
      property Height: Integer read GetHeight write SetHeight;
      property Left: Integer read GetLeft write SetLeft;
      property Top: Integer read GetTop write SetTop;
      property FullScreen: Boolean read fFullScreen write SetFullScreen;
      property Window: PSDL_Window read fWindow;
      property Visible: Boolean read FVisible write SetVisible;
  end;

  function ClientToScreen( Window: TSDLWindow; X, Y: Integer ): TPoint;
  function ScreenToClient( Window: TSDLWindow; X, Y: Integer ): TPoint;

var
  ActiveWindow: TSDLWindow = nil;
//var
  //MainWindow: TSDLWindow;

implementation
  uses p3dinput, p3dviewport;

function ClientToScreen(Window: TSDLWindow; X, Y: Integer): TPoint;
begin
  Result:= Point( Window.Left + X, Window.Top + Y );
end;

function ScreenToClient(Window: TSDLWindow; X, Y: Integer): TPoint;
begin
  Result:= Point( X - Window.Left, Y - Window.Top );
end;


{ TSDLWindow }

procedure TSDLWindow.SetTitle(AValue: String);
begin
  if FTitle=AValue then Exit;
  FTitle:= AValue;
  SDL_SetWindowTitle( window, PChar( AValue ));
end;

procedure TSDLWindow.SetHeight(AValue: Integer);
begin
  SDL_SetWindowSize( window, Width, AValue );
end;

procedure TSDLWindow.SetFullScreen(AValue: Boolean);
begin
  if fFullScreen=AValue then Exit;
  fFullScreen:=AValue;
  SDL_SetWindowFullscreen( window, Integer( fFullScreen ));
end;

function TSDLWindow.GetHeight: Integer;
begin
  SDL_GetWindowSize( Window, nil, @Result );
end;

function TSDLWindow.GetLeft: Integer;
begin
  SDL_GetWindowPosition( Window, @Result, nil );
end;

function TSDLWindow.GetTitle: String;
begin
  Result:= SDL_GetWindowTitle( Window );
end;

function TSDLWindow.GetTop: Integer;
begin
  SDL_GetWindowPosition( Window, nil, @Result );
end;

function TSDLWindow.GetWidth: Integer;
begin
  SDL_GetWindowSize( Window, @Result, nil );
end;

procedure TSDLWindow.SetLeft(AValue: Integer);
begin
  SDL_SetWindowPosition( window, AValue, Top );
end;

procedure TSDLWindow.SetTop(AValue: Integer);
begin
  SDL_SetWindowPosition( window, Left, AValue );
end;

procedure TSDLWindow.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  if ( Visible ) then
    Show
  else
    Hide;
end;

procedure TSDLWindow.SetWidth(AValue: Integer);
begin
  SDL_SetWindowSize( window, AValue, Width );
end;

procedure TSDLWindow.SetViewportOpenGL(w, h: Integer);
begin
  // Wird beim Ändern der Fenstergröße aufgerufen, passt den OpenGL-Viewport
  // an die neue Fenstergröße an.

  if w <= 0 then w := 1;
  if h <= 0 then h := 1;
  glViewport( 0, 0, w, h );
  p3dviewport.P3DViewports.WndHeight:= h;
end;


procedure TSDLWindow.Render;
begin
  ActiveWindow:= Self;
  if ( Assigned( OnRender )) then
    fOnRender( Self );
  SDL_GL_SwapWindow( window );
end;

constructor TSDLWindow.Create;
begin
  fWindow:= SDL_CreateWindow( 'Main Window', 100, 100, 640, 480,
                              SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE );

  // Fenster korrekt erstellt?
  if ( window = nil ) then
    raise Exception.Create( 'SDL-Fenster konnte nicht erstellt werden: ' + SDL_GetError );

  SDL_SetWindowData( fWindow, 'p3dwindow', Self );

  // Die gewünschte OpenGL-Version festlegen
  SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 3 );
  SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 3 );
  SDL_GL_SetAttribute( SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_CORE );
  // OpenGL-Kontext erstellen
  context := SDL_GL_CreateContext( window );
  // Kontext korrekt erstellt?
  if context = nil then
    begin
      raise Exception.Create('OpenGL-Kontext konnte nicht erstellt werden: ' + SDL_GetError);
    end;
  renderer:= SDL_CreateRenderer( window, -1, SDL_RENDERER_ACCELERATED );
  if ( renderer = nil ) then
    raise Exception.Create('SDL-Renderer konnte nicht erstellt werden: ' + SDL_GetError);
  FVisible:= True;

  if ( ActiveWindow = nil ) then
    ActiveWindow:= Self;
  SetViewportOpenGL( Width, Height );
end;

constructor TSDLWindow.CreateFrom(Wnd: Pointer);
begin
  fWindow:= SDL_CreateWindowFrom( Wnd );

  // Fenster korrekt erstellt?
  if ( window = nil ) then
    raise Exception.Create('SDL-Fenster konnte nicht erstellt werden: ' + SDL_GetError);

  SDL_SetWindowData( fWindow, 'p3dwindow', Self );
  WriteLn( SDL_GetWindowFlags( Window ));
  FVisible:= True;
end;

procedure TSDLWindow.Show;
begin
  SDL_ShowWindow( Window );
end;

procedure TSDLWindow.Hide;
begin
  SDL_HideWindow( Window );
end;

function TSDLWindow.GetRenderer: PSDL_Renderer;
begin
  Result:= renderer;
end;

end.

