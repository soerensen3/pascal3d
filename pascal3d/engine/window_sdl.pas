unit window_sdl;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, sdl2, dglOpenGL;

type

  { TSDLWindow }

  TSDLWindow = class;

  TWindowEvent = procedure( Sender: TSDLWindow );
  TWindowEventKey = procedure( Sender: TSDLWindow; Event: TSDL_KeyboardEvent );
  TWindowEventMouseButton = procedure( Sender: TSDLWindow; Event: TSDL_MouseButtonEvent );
  TWindowEventMouseMotion = procedure( Sender: TSDLWindow; Event: TSDL_MouseMotionEvent );
  TWindowEventMouseWheel = procedure( Sender: TSDLWindow; Event: TSDL_MouseWheelEvent );

  TSDLWindow = class
    private
      fFullScreen: Boolean;
      fHeight: Integer;
      fLeft: Integer;
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
      fRunning: Boolean;
      fTitle: String;
      fTop: Integer;
      fWidth: Integer;
      fWindow: PSDL_Window;
      context: TSDL_GLContext;
      renderer: PSDL_Renderer;

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
      procedure SetWidth(AValue: Integer);

    public
      procedure Setup_SDL2;
      procedure Setup_OpenGL;
      procedure SetViewportOpenGL(w,h: Integer);
      procedure HandleEvents_SDL;
      procedure Run;
      procedure Render;
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

      property Running: Boolean read fRunning write fRunning;
      property Title: String read GetTitle write SetTitle;
      property Width: Integer read GetWidth write SetWidth;
      property Height: Integer read GetHeight write SetHeight;
      property Left: Integer read GetLeft write SetLeft;
      property Top: Integer read GetTop write SetTop;
      property FullScreen: Boolean read fFullScreen write SetFullScreen;
      property Window: PSDL_Window read fWindow;
  end;

var
  MainWindow: TSDLWindow;

implementation


{ TSDLWindow }

procedure TSDLWindow.SetTitle(AValue: String);
begin
  if FTitle=AValue then Exit;
  FTitle:= AValue;
  SDL_SetWindowTitle( window, PChar( AValue ));
end;

procedure TSDLWindow.SetHeight(AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
  SDL_SetWindowSize( window, FWidth, FHeight );
end;

procedure TSDLWindow.SetFullScreen(AValue: Boolean);
begin
  if fFullScreen=AValue then Exit;
  fFullScreen:=AValue;
  SDL_SetWindowFullscreen( window, Integer( fFullScreen ));
end;

function TSDLWindow.GetHeight: Integer;
begin
  Result:= Window^.h;
end;

function TSDLWindow.GetLeft: Integer;
begin
  Result:= Window^.x;
end;

function TSDLWindow.GetTitle: String;
begin
  Result:= Window^.title;
end;

function TSDLWindow.GetTop: Integer;
begin
  Result:= Window^.y;
end;

function TSDLWindow.GetWidth: Integer;
begin
  Result:= Window^.w;
end;

procedure TSDLWindow.SetLeft(AValue: Integer);
begin
  if FLeft=AValue then Exit;
  FLeft:=AValue;
  SDL_SetWindowPosition( window, FLeft, FTop );
end;

procedure TSDLWindow.SetTop(AValue: Integer);
begin
  if FTop=AValue then Exit;
  FTop:=AValue;
  SDL_SetWindowPosition( window, FLeft, FTop );
end;

procedure TSDLWindow.SetWidth(AValue: Integer);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  SDL_SetWindowSize( window, FWidth, FHeight );
end;

procedure TSDLWindow.Setup_SDL2;
begin
  // Initialisieren des SDL-Videosubsystems
  if SDL_Init(SDL_INIT_VIDEO) > 0 then
  begin
    raise Exception.Create('Initializieren des SDL-Videosubsystems fehlgeschlagen: '
                           + SDL_GetError);
  end;
  // Ein Fenster an Desktopposition 100, 100 erstellen, Fenstergröße ist
  // 640 mal 480 und veränderbar, OpenGL-Unterstützung ist aktiviert
  fWindow:= SDL_CreateWindow('OpenGL 3.3 und SDL2', 100, 100, 640, 480,
                             SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE );
  // Fenster korrekt erstellt?
  if window = nil then
  begin
    raise Exception.Create('SDL-Fenster konnte nicht erstellt werden: ' + SDL_GetError);
  end;
  // Die gewünschte OpenGL-Version festlegen
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
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
end;

procedure TSDLWindow.Setup_OpenGL;
begin
  // Initialisieren von OpenGL mit den Funktionen aus der dglOpenGL.pas
  InitOpenGL;
  ReadExtensions;
  ReadImplementationProperties;

  // Einige OpenGL-Einstellungen, für dieses Beispiel nicht wirklich notwendig
  glClearColor(0.0, 0.5, 1.0, 1.0);
  glClearDepth(1.0);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
end;

procedure TSDLWindow.SetViewportOpenGL(w, h: Integer);
begin
  // Wird beim Ändern der Fenstergröße aufgerufen, passt den OpenGL-Viewport
  // an die neue Fenstergröße an.
//ToDo 1:  Nach Änderung der Fenstergröße sehen die Objekte stark verzerrt aus

  if w <= 0 then w := 1;
  if h <= 0 then h := 1;
  glViewport( 0, 0, w, h );
end;

procedure TSDLWindow.HandleEvents_SDL;
var
  event: TSDL_Event;
begin
  while SDL_PollEvent(@event) > 0 do
    case event.type_ of
      SDL_WINDOWEVENT:
        case event.window.event of
          SDL_WINDOWEVENT_SIZE_CHANGED:
            if event.window.windowID = SDL_GetWindowID(window) then
              begin
                SetViewportOpenGL(window^.w,window^.h);
                if ( Assigned( FOnResize )) then
                  FOnResize( Self );
              end;
          SDL_WINDOWEVENT_CLOSE:
            if event.window.windowID = SDL_GetWindowID(window) then
              begin
                Running:= false; // Hauptschleife soll beendet werden
                if ( Assigned( FOnClose )) then
                  FOnClose( Self );
              end;
        end;
      SDL_KEYDOWN, SDL_KEYUP:
        if ( Assigned( FOnKey )) then
          FOnKey( Self, event.key );
      SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
        if ( Assigned( FOnMouseButton )) then
          FOnMouseButton( Self, event.button );
      SDL_MOUSEMOTION:
        if ( Assigned( FOnMouseMotion )) then
          FOnMouseMotion( Self, event.motion );
      SDL_MOUSEWHEEL:
        if ( Assigned( FOnMouseWheel )) then
          FOnMouseWheel( Self, event.wheel );
    end;
end;

procedure TSDLWindow.Run;
begin
  try
    Setup_SDL2();                    // Fenster und Kontext erstellen
    Setup_OpenGL();                  // OpenGL initialisieren
    SetViewportOpenGL( 640,480 );    // Viewport setzen
    // Pfad zu den Shaderdateien ermitteln, diese liegen in einem Unterordner
    // "Shader" im Programmordner

    if ( not Assigned( MainWindow )) then
      MainWindow:= Self;

    if ( Assigned( OnInit )) then
      OnInit( Self );

    fRunning := true;
    while fRunning and (window <> nil) do
    begin                // Hauptschleife für Rendern und Ereignisbehandlung
      HandleEvents_SDL;
      if ( Assigned( fOnInput )) then
        fOnInput( Self );
      Render;
    end;

    // Resourcen freigeben
    if ( Assigned( OnDeinit )) then
      OnDeinit( Self );

    SDL_GL_DeleteContext(context);
    SDL_DestroyWindow(window);
    FWindow := nil;
  except
    on E: Exception do
      //Writeln(E.ClassName, ': ', E.Message);
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR, PAnsiChar('Fehler'),
                                                 PAnsiChar(E.Message), window);
  end;
end;

procedure TSDLWindow.Render;
begin
  if ( Assigned( OnRender )) then
    fOnRender( Self );
  SDL_GL_SwapWindow( window );
end;

function TSDLWindow.GetRenderer: PSDL_Renderer;
begin
  Result:= renderer;
end;

end.

