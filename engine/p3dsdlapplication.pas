unit p3dSDLApplication;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, sdl2, p3dwindow, p3dinput, dglOpenGL, p3dlogging;

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
  TP3DApplication = class;

  TSDLEvent = procedure( Sender: TP3DApplication );
  TSDLEventKey = procedure( Sender: TP3DApplication; Event: TSDL_KeyboardEvent );
  TSDLEventMouseButton = procedure( Sender: TP3DApplication; Event: TSDL_MouseButtonEvent );
  TSDLEventMouseMotion = procedure( Sender: TP3DApplication; Event: TSDL_MouseMotionEvent );
  TSDLEventMouseWheel = procedure( Sender: TP3DApplication; Event: TSDL_MouseWheelEvent );

  { TP3DApplication }

  TP3DApplication = class
    private
      FGLInitialized: Boolean;
      FMainWindow: TSDLWindow;
      fOnDeinit: TSDLEvent;
      fOnInit: TSDLEvent;
      fOnInput: TSDLEvent;
      FOnKey: TSDLEventKey;
      FOnMouseButton: TSDLEventMouseButton;
      FOnMouseMotion: TSDLEventMouseMotion;
      FOnMouseWheel: TSDLEventMouseWheel;
      FRunning: Boolean;
      FSDLInitialized: Boolean;
      ReadingTextInput: Boolean;
      procedure SetMainWindow(AValue: TSDLWindow);

    public
      constructor Create;

      procedure InitSDL;
      procedure InitGL;
      procedure HandleEvents_SDL;
      procedure Initialize; //calls InitSDL and InitGL
      procedure Run;

      property OnMouseMotion: TSDLEventMouseMotion read FOnMouseMotion write FOnMouseMotion;
      property OnMouseButton: TSDLEventMouseButton read FOnMouseButton write FOnMouseButton;
      property OnMouseWheel: TSDLEventMouseWheel read FOnMouseWheel write FOnMouseWheel;
      property OnKey: TSDLEventKey read FOnKey write FOnKey;
      property OnInput: TSDLEvent read fOnInput write fOnInput;
      property OnInit: TSDLEvent read fOnInit write fOnInit;
      property OnDeinit: TSDLEvent read fOnDeinit write fOnDeinit;
      property MainWindow: TSDLWindow read FMainWindow write SetMainWindow;
      property SDLInitialized: Boolean read FSDLInitialized write FSDLInitialized;
      property GLInitialized: Boolean read FGLInitialized write FGLInitialized;
      property Running: Boolean read FRunning write FRunning;
  end;

var
  P3DApplication: TP3DApplication = nil;

implementation

{ TP3DApplication }

procedure TP3DApplication.SetMainWindow(AValue: TSDLWindow);
begin
  P3DLog.LogInfo( Self, 'SetMainWindow').AttribStrings['function']:= 'SetMainWindow';         ;
  if FMainWindow=AValue then Exit;
  FMainWindow:=AValue;
  ActiveWindow:= AValue;
end;

constructor TP3DApplication.Create;
begin
  inherited;
  MainWindow:= nil;
end;

procedure TP3DApplication.InitSDL;
begin
  if ( SDL_Init( SDL_INIT_EVERYTHING ) > 0 ) then
    raise Exception.Create('Initialization of SDL2 failed: '
                           + SDL_GetError);
  SDLInitialized:= True;
end;

procedure TP3DApplication.InitGL;
begin
  P3DLog.LogInfo( Self, 'InitGL').AttribStrings['function']:= 'InitGL';
  if ( not InitOpenGL()) then
    raise Exception.Create('Initialization of OpenGL failed.');
  GLInitialized:= True;

  ReadImplementationProperties;
  ReadExtensions;

  // Some OpenGL configurations       ;
  glClearColor(0.0, 0.5, 1.0, 1.0);
  glClearDepth(1.0);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
end;

procedure TP3DApplication.HandleEvents_SDL;
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
      else
        Write( E.type_ );
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
  wnd: Pointer;
begin
  while SDL_PollEvent(@event) > 0 do
    case event.type_ of
      SDL_WINDOWEVENT:
        begin
          wnd:= SDL_GetWindowData( SDL_GetWindowFromID( event.window.windowID ), 'p3dwindow' );
          case event.window.event of
            SDL_WINDOWEVENT_SIZE_CHANGED:
              with ( TSDLWindow( wnd )) do
                begin
                  SetViewportOpenGL( event.window.data1, event.window.data2 );
                  if ( Assigned( OnResize )) then
                    OnResize( TSDLWindow( wnd ));
                end;
            SDL_WINDOWEVENT_CLOSE:
                begin
                  if ( TSDLWindow( wnd ) = MainWindow ) then
                    begin
                      Running:= false; // Hauptschleife soll beendet werden
                      TSDLWindow( wnd ).Hide;
                      if ( Assigned( TSDLWindow( wnd ).OnClose )) then
                        TSDLWindow( wnd ).OnClose( TSDLWindow( wnd ));
                    end;
                end;
          end;
        end;
      SDL_MOUSEMOTION:
        begin
          InputManager.Mouse.X:= event.motion.x;
          InputManager.Mouse.Y:= event.motion.y;
          InputManager.Mouse.DX:= event.motion.xrel;
          InputManager.Mouse.DY:= event.motion.yrel;
          if ( Assigned( FOnMouseMotion )) then
            FOnMouseMotion( Self, event.motion );
        end;
      SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
        begin
          InputManager.Mouse.Buttons[ event.button.button - 1 ]:= not Boolean( event.type_ - SDL_MOUSEBUTTONDOWN );
          InputManager.Mouse.DButtons[ event.button.button - 1 ]:= True;
          if ( Assigned( FOnMouseButton )) then
            FOnMouseButton( Self, event.button );
        end;
      SDL_MOUSEWHEEL:
        if ( Assigned( FOnMouseWheel )) then
          FOnMouseWheel( Self, event.wheel );
      SDL_KEYDOWN, SDL_KEYUP:
        begin
          InputManager.Keyboard.Keys[ event.key.keysym.scancode ]:= Boolean( event.key.state );
          InputManager.Keyboard.DKeys[ event.key.keysym.scancode ]:= True;
          if ( Assigned( FOnKey )) then
            FOnKey( Self, event.key );
        end;
      SDL_TEXTINPUT, SDL_TEXTEDITING:
        begin
          InputManager.Keyboard.InputText:= InputManager.Keyboard.InputText + event.text.text;
        end;
      SDL_RENDER_TARGETS_RESET:
        WriteLn( 'The render targets have been reset and their contents need to be updated!' );{;
      SDL_RENDER_DEVICE_RESET:
        WriteLn( 'The device has been reset and all textures need to be recreated' )}
      SDL_QUITEV:
        begin
          Running:= false; // Hauptschleife soll beendet werden
          MainWindow.Hide;
          if ( Assigned( MainWindow.OnClose )) then
            MainWindow.OnClose( MainWindow );
        end

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
  if ( Assigned( fOnInput )) then
    fOnInput( Self );
  if ( InputManager.Keyboard.ReadingTextInput <> ReadingTextInput ) then
    begin
      if ( InputManager.Keyboard.ReadingTextInput ) then
        SDL_StartTextInput
      else
        SDL_StopTextInput;
      ReadingTextInput:= InputManager.Keyboard.ReadingTextInput;
    end;
end;

procedure TP3DApplication.Initialize;
begin
  P3DLog.LogInfo( Self, 'initializing application' ).AttribStrings['function']:= 'Initialize';
  InitSDL;
  InitGL;
  if ( Assigned( OnInit )) then
    OnInit( Self );
end;

procedure TP3DApplication.Run;
begin
  try
    P3DLog.LogInfo( Self, 'Running application' ).AttribStrings['function']:= 'Run';
    Running := True;
    while Running do
      begin                // Mainloop
        HandleEvents_SDL;
        if ( Assigned( MainWindow )) then
          MainWindow.Render;
      end;
  except
    on E: Exception do
      P3DLog.LogException( Self, E.Message );
  end;
  P3DLog.LogInfo( Self, 'exit mainloop' ).AttribStrings['function']:= 'Run';
  // Resourcen freigeben
  if ( Assigned( OnDeinit )) then
    OnDeinit( Self );
end;

initialization
  P3DApplication:= TP3DApplication.Create;

finalization
  FreeAndNil( P3DApplication );

end.

