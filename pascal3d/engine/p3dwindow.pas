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
      ReadingTextInput: Boolean;

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

  function ClientToScreen( Window: TSDLWindow; X, Y: Integer ): TPoint;
  function ScreenToClient( Window: TSDLWindow; X, Y: Integer ): TPoint;


var
  MainWindow: TSDLWindow;

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
var
  tmp: Integer;
begin
  SDL_GetWindowPosition( Window, @Result, @tmp );
end;

function TSDLWindow.GetTitle: String;
begin
  Result:= Window^.title;
end;

function TSDLWindow.GetTop: Integer;
var
  tmp: Integer;
begin
  SDL_GetWindowPosition( Window, @tmp, @Result );
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
  fWindow:= SDL_CreateWindow('Main Window', 100, 100, 640, 480,
                             SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE );
  // Fenster korrekt erstellt?
  if window = nil then
  begin
    raise Exception.Create('SDL-Fenster konnte nicht erstellt werden: ' + SDL_GetError);
  end;
  // Die gewünschte OpenGL-Version festlegen
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MAJOR_VERSION, 3);
  SDL_GL_SetAttribute(SDL_GL_CONTEXT_MINOR_VERSION, 3);
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

  if w <= 0 then w := 1;
  if h <= 0 then h := 1;
  glViewport( 0, 0, w, h );
  p3dviewport.P3DViewports.WndHeight:= h;
end;



procedure TSDLWindow.HandleEvents_SDL;
  procedure debug_printeventname( E: TSDL_Event );
  begin
    case E.type_ of
      SDL_KEYDOWN: Write( 'SDL_KEYDOWN' );
      SDL_KEYUP: Write( 'SDL_KEYUP' );
      SDL_TEXTEDITING: Write( 'SDL_TEXTEDITING' );
      SDL_TEXTINPUT: Write( 'SDL_TEXTINPUT' );

      { Mouse events }
      SDL_MOUSEMOTION: Write( 'SDL_MOUSEMOTION' );
      SDL_MOUSEBUTTONDOWN: Write( 'SDL_MOUSEBUTTONDOWN' );
      SDL_MOUSEBUTTONUP: Write( 'SDL_MOUSEBUTTONUP' );
      SDL_MOUSEWHEEL: Write( 'SDL_MOUSEWHEEL' );

      { Joystick events }
      SDL_JOYAXISMOTION: Write( 'SDL_JOYAXISMOTION' );
      SDL_JOYBALLMOTION: Write( 'SDL_JOYBALLMOTION' );
      SDL_JOYHATMOTION: Write( 'SDL_JOYHATMOTION' );
      SDL_JOYBUTTONDOWN: Write( 'SDL_JOYBUTTONDOWN' );
      SDL_JOYBUTTONUP: Write( 'SDL_JOYBUTTONUP' );
      SDL_JOYDEVICEADDED: Write( 'SDL_JOYDEVICEADDED' );
      SDL_JOYDEVICEREMOVED: Write( 'SDL_JOYDEVICEREMOVED' );

      { Game controller events }
      SDL_CONTROLLERAXISMOTION: Write( 'SDL_CONTROLLERAXISMOTION' );
      SDL_CONTROLLERBUTTONDOWN: Write( 'SDL_CONTROLLERBUTTONDOWN' );
      SDL_CONTROLLERBUTTONUP: Write( 'SDL_CONTROLLERBUTTONUP' );
      SDL_CONTROLLERDEVICEADDED: Write( 'SDL_CONTROLLERDEVICEADDED' );
      SDL_CONTROLLERDEVICEREMOVED: Write( 'SDL_CONTROLLERDEVICEREMOVED' );
      SDL_CONTROLLERDEVICEREMAPPED: Write( 'SDL_CONTROLLERDEVICEREMAPPED' );

      { Touch events }
      SDL_FINGERDOWN: Write( 'SDL_FINGERDOWN');
      SDL_FINGERUP: Write( 'SDL_FINGERUP');
      SDL_FINGERMOTION: Write( 'SDL_FINGERMOTION');

      { Gesture events }
      SDL_DOLLARGESTURE: Write( 'SDL_DOLLARGESTURE');
      SDL_DOLLARRECORD: Write( 'SDL_DOLLARRECORD');
      SDL_MULTIGESTURE: Write( 'SDL_MULTIGESTURE');
    end;
  end;

  procedure debug_printmousemoveevent( E: TSDL_Event );
  begin
    WriteLn( 'windowID', E.motion.windowID );
    WriteLn( 'which', E.motion.which );
    WriteLn( 'state', E.motion.state );
    WriteLn( 'padding1', E.motion.padding1 );
    WriteLn( 'padding2', E.motion.padding2 );
    WriteLn( 'padding3', E.motion.padding3 );
    WriteLn( 'x', E.motion.x );
    WriteLn( 'y', E.motion.y );
    WriteLn( 'xrel', E.motion.xrel );
    WriteLn( 'yrel', E.motion.yrel );
  end;

  procedure debug_printevent( E: TSDL_Event );
  begin
    WriteLn( '--- UNHANDLED SDL-EVENT ---' );
    Write( '  Type: ' );
    debug_printeventname( E );
    WriteLn;
    Write( '  Timestamp: ',  E.common.timestamp );
    WriteLn;

    case E.type_ of
      SDL_MOUSEMOTION: debug_printmousemoveevent( E );
    end;
  end;
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
      SDL_MOUSEMOTION:
        begin
          InputManager.Mouse.X:= event.motion.x;
          InputManager.Mouse.Y:= event.motion.y;
          InputManager.Mouse.DX:= event.motion.xrel;
          InputManager.Mouse.DY:= event.motion.yrel;
        end;
      SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
        begin
          InputManager.Mouse.Buttons[ event.button.button - 1 ]:= not Boolean( event.type_ - SDL_MOUSEBUTTONDOWN );
          InputManager.Mouse.DButtons[ event.button.button - 1 ]:= True;
        end;
      SDL_KEYDOWN, SDL_KEYUP:
        begin
          InputManager.Keyboard.Keys[ event.key.keysym.scancode ]:= Boolean( event.key.state );
          InputManager.Keyboard.DKeys[ event.key.keysym.scancode ]:= True;
        end;
      SDL_TEXTINPUT:
        begin
          InputManager.Keyboard.InputText:= InputManager.Keyboard.InputText + event.text.text;
        end;
      SDL_RENDER_TARGETS_RESET:
        WriteLn( 'The render targets have been reset and their contents need to be updated!' ){;
      SDL_RENDER_DEVICE_RESET:
        WriteLn( 'The device has been reset and all textures need to be recreated' )}
      else
        debug_printevent( event );
      //SDL_KEYDOWN, SDL_KEYUP, SDL_TEXTEDITING, SDL_TEXTINPUT:
      //  InputManager.KeyboardEvent( Self, event.key );
      //SDL_MOUSEMOTION, SDL_MOUSEWHEEL:
      //  InputManager.KeyboardEvent( Self, event.key );
      //SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
      //  InputManager.KeyboardEvent( Self, event.key );
      //SDL_KEYDOWN, SDL_KEYUP:
      //  if ( Assigned( FOnKey )) then
      //    FOnKey( Self, event.key );
      {SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
        if ( Assigned( FOnMouseButton )) then
          FOnMouseButton( Self, event.button );
      SDL_MOUSEMOTION:
        if ( Assigned( FOnMouseMotion )) then
          FOnMouseMotion( Self, event.motion );
      SDL_MOUSEWHEEL:
        if ( Assigned( FOnMouseWheel )) then
          FOnMouseWheel( Self, event.wheel );}
    end;
end;

procedure TSDLWindow.Run;
begin
  try
    Setup_SDL2();                    // Fenster und Kontext erstellen
    Setup_OpenGL();                  // OpenGL initialisieren
    SetViewportOpenGL( 640,480 );    // Viewport setzen

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
      if ( InputManager.Keyboard.ReadingTextInput <> ReadingTextInput ) then
        begin
          if ( InputManager.Keyboard.ReadingTextInput ) then
            SDL_StartTextInput
          else
            SDL_StopTextInput;
          ReadingTextInput:= InputManager.Keyboard.ReadingTextInput;
        end;
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

