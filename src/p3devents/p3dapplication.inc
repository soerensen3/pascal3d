//p3dapplication.inc
//part of p3devents.pas

{$IFDEF INTERFACE}

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
      FActiveWindow: TP3DWindow;
      FGLInitialized: Boolean;
      FMainWindow: TP3DWindow;
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
      procedure SetActiveWindow(AValue: TP3DWindow);
      procedure SetMainWindow(AValue: TP3DWindow);

    public
      constructor Create;

      procedure InitSDL;
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
      property MainWindow: TP3DWindow read FMainWindow write SetMainWindow;
      property ActiveWindow: TP3DWindow read FActiveWindow write SetActiveWindow;
      property SDLInitialized: Boolean read FSDLInitialized write FSDLInitialized;
      property GLInitialized: Boolean read FGLInitialized write FGLInitialized;
      property Running: Boolean read FRunning write FRunning;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DApplication }

procedure TP3DApplication.SetMainWindow(AValue: TP3DWindow);
begin
  P3DLog.LogInfo( Self, 'SetMainWindow').AttribStrings['function']:= 'SetMainWindow';         ;
  if FMainWindow=AValue then Exit;
  FMainWindow:=AValue;
  ActiveWindow:= AValue;
end;

procedure TP3DApplication.SetActiveWindow(AValue: TP3DWindow);
begin
  if FActiveWindow=AValue then Exit;
  FActiveWindow:=AValue;
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
    {$IFDEF VERBOSE}
    WriteLn( 'windowID ', E.motion.windowID );
    WriteLn( 'which ', E.motion.which );
    WriteLn( 'state ', E.motion.state );
    WriteLn( 'padding1 ', E.motion.padding1 );
    WriteLn( 'padding2 ', E.motion.padding2 );
    WriteLn( 'padding3 ', E.motion.padding3 );
    WriteLn( 'x ', E.motion.x );
    WriteLn( 'y ', E.motion.y );
    WriteLn( 'xrel ', E.motion.xrel );
    WriteLn( 'yrel ', E.motion.yrel );
    {$ENDIF}
  end;

  procedure debug_printkeyevent( E: TSDL_Event );
  begin
    {$IFDEF VERBOSE}
    WriteLn( 'windowID ', E.key.windowID );
    WriteLn( 'keysym.scancode ', E.key.keysym.scancode );
    WriteLn( 'keysym.sym ', E.key.keysym.sym );
    WriteLn( 'keysym.unicode ', E.key.keysym.unicode );
    WriteLn( 'keysym._mod ', E.key.keysym._mod );
    WriteLn( 'state ', E.key.state );
    WriteLn( 'padding2 ', E.key.padding2 );
    WriteLn( 'padding3 ', E.key.padding3 );
    WriteLn( 'type ', E.key.type_ );
    WriteLn( 'repeat ', E.key._repeat );
    {$ENDIF}
  end;

  procedure debug_printevent( E: TSDL_Event );
  begin
    {$IFDEF VERBOSE}
    WriteLn( '--- UNHANDLED SDL-EVENT ---' );
    Write( '  Type: ' );
    debug_printeventname( E );
    WriteLn;
    Write( '  Timestamp: ',  E.common.timestamp );
    WriteLn;

    case E.type_ of
      SDL_MOUSEMOTION: debug_printmousemoveevent( E );
      SDL_KEYDOWN,SDL_KEYUP: debug_printkeyevent( E );
    end;
    {$ENDIF}
  end;
var
  event: TSDL_Event;
  wnd: Pointer;
begin
  while ( SDL_PollEvent( @event ) > 0 ) do
    case event.type_ of
      SDL_WINDOWEVENT:
        begin
          wnd:= SDL_GetWindowData( SDL_GetWindowFromID( event.window.windowID ), 'p3dwindow' );
          case event.window.event of
            SDL_WINDOWEVENT_SIZE_CHANGED:
              with ( TP3DWindow( wnd )) do
                begin
                  TP3DWindow( wnd ).UpdateViewport;
                  if ( Assigned( OnResize )) then
                    OnResize( TP3DWindow( wnd ));
                end;
            SDL_WINDOWEVENT_CLOSE:
                begin
                  if ( TP3DWindow( wnd ) = MainWindow ) then
                    begin
                      Running:= false; // Hauptschleife soll beendet werden
                      TP3DWindow( wnd ).Hide;
                      if ( Assigned( TP3DWindow( wnd ).OnClose )) then
                        TP3DWindow( wnd ).OnClose( TP3DWindow( wnd ));
                    end;
                end;
          end;
        end;
      SDL_MOUSEMOTION:
        begin
          P3DInput.Mouse.X:= event.motion.x;
          P3DInput.Mouse.Y:= event.motion.y;
          P3DInput.Mouse.DX:= event.motion.xrel;
          P3DInput.Mouse.DY:= event.motion.yrel;
          if ( Assigned( FOnMouseMotion )) then
            FOnMouseMotion( Self, event.motion );
        end;
      SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
        begin
          P3DInput.Mouse.Buttons[ event.button.button - 1 ]:= not Boolean( event.type_ - SDL_MOUSEBUTTONDOWN );
          P3DInput.Mouse.DButtons[ event.button.button - 1 ]:= True;
          if ( Assigned( FOnMouseButton )) then
            FOnMouseButton( Self, event.button );
        end;
      SDL_MOUSEWHEEL:
        if ( Assigned( FOnMouseWheel )) then
          FOnMouseWheel( Self, event.wheel );
      SDL_KEYDOWN, SDL_KEYUP:
        begin
          if ( event.key._repeat = 1 ) then
            continue;
          debug_printevent( event );
          P3DInput.Keyboard.Keys[ event.key.keysym.scancode ]:= Boolean( event.key.state );
          P3DInput.Keyboard.DKeys[ event.key.keysym.scancode ]:= True;
          if ( Assigned( FOnKey )) then
            FOnKey( Self, event.key );
        end;
      SDL_TEXTINPUT, SDL_TEXTEDITING:
        begin
          P3DInput.Keyboard.InputText:= P3DInput.Keyboard.InputText + event.text.text;
        end;
      SDL_RENDER_TARGETS_RESET:
        P3DLog.LogInfo( Self, 'The render targets have been reset and their contents need to be updated!' );{;
      SDL_RENDER_DEVICE_RESET:
        P3DLog.LogInfo( Self, 'The device has been reset and all textures need to be recreated' )}
      SDL_QUITEV:
        begin
          Running:= False; // Quit main loop
          MainWindow.Hide;
          if ( Assigned( MainWindow.OnClose )) then
            MainWindow.OnClose( MainWindow );
        end

      else
        debug_printevent( event );
    end;
  if ( Assigned( FOnInput )) then
    FOnInput( Self );
  if ( P3DInput.Keyboard.ReadingTextInput <> ReadingTextInput ) then
    begin
      if ( P3DInput.Keyboard.ReadingTextInput ) then
        SDL_StartTextInput
      else
        SDL_StopTextInput;
      ReadingTextInput:= P3DInput.Keyboard.ReadingTextInput;
    end;
end;

procedure TP3DApplication.Initialize;
begin
  P3DLog.LogInfo( Self, 'initializing application' ).AttribStrings['function']:= 'Initialize';
  if ( not SDLInitialized ) then
    InitSDL;
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
        P3DInput.NextCycle;
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

{$ENDIF}

