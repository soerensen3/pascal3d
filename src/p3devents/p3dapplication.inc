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

  TP3DApplication = class ( TPersistent )
    private
      FActiveWindow: TP3DWindow;
      FMainWindow: TP3DWindow;
      FOnDeinit: TSDLEvent;
      FOnInit: TSDLEvent;
      FOnInput: TSDLEvent;
      FOnKey: TSDLEventKey;
      FOnMouseButton: TSDLEventMouseButton;
      FOnMouseMotion: TSDLEventMouseMotion;
      FOnMouseWheel: TSDLEventMouseWheel;
      FRunning: Boolean;
      FSDLInitialized: Boolean;
      ReadingTextInput: Boolean;

      procedure SetActiveWindow(AValue: TP3DWindow);
      procedure SetMainWindow(AValue: TP3DWindow);

    protected
      procedure InitializeWindow( Sender: TP3DWindow ); virtual;
      procedure FinalizeWindow( Sender: TP3DWindow ); virtual;
      procedure ResizeWindow( Sender: TP3DWindow; Event: TSDL_WindowEvent ); virtual;
      procedure Render; virtual;
      procedure Events; virtual;
      procedure MouseMotion( Event: TSDL_MouseMotionEvent ); virtual;
      procedure MouseButton( Event: TSDL_MouseButtonEvent ); virtual;
      procedure MouseWheel( Event: TSDL_MouseWheelEvent ); virtual;
      procedure Keyboard( Event: TSDL_KeyboardEvent ); virtual;
      procedure TextInput( Event: TSDL_TextInputEvent ); virtual;
      procedure AfterRun; virtual;

    public
      constructor Create;
      destructor Destroy; override;

      procedure InitSDL;
      procedure Initialize; virtual; //calls InitSDL
      procedure Finalize; virtual;
      procedure Run;
      procedure LoadConfig( FileName: String );
      procedure LoadConfig( XML: TXMLDocument );
      procedure LoadConfigSection( FileName, NodeName: String );
      procedure LoadConfigSection( XML: TXMLDocument; DOMNode: TDOMElement );

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
      property Running: Boolean read FRunning write FRunning;
  end;

const
  SDL2P3DKeystate: array [ SDL_KEYDOWN..SDL_KEYUP ] of TP3DKeyState = ( ksPressed, ksReleased );

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

procedure TP3DApplication.InitializeWindow(Sender: TP3DWindow);
begin

end;


procedure TP3DApplication.FinalizeWindow(Sender: TP3DWindow);
begin
  if ( Sender = MainWindow ) then
    Running:= False;
end;

procedure TP3DApplication.ResizeWindow(Sender: TP3DWindow; Event: TSDL_WindowEvent);
var
  vp: TP3DViewport;
begin
  Sender.ResizeViewport( Event.data1, Event.data2 );

  if ( Assigned( Sender.OnResize )) then
    Sender.OnResize( Sender );
end;

procedure TP3DApplication.Render;
begin

end;

procedure TP3DApplication.Events;
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
  ScanCode: TSDL_ScanCode;
begin
  while ( SDL_PollEvent( @event ) > 0 ) do
    case event.type_ of
      SDL_WINDOWEVENT:
        begin
          wnd:= SDL_GetWindowData( SDL_GetWindowFromID( event.window.windowID ), 'p3dwindow' );
          case event.window.event of
            SDL_WINDOWEVENT_SIZE_CHANGED:
              ResizeWindow( TP3DWindow( wnd ), event.window );
            SDL_WINDOWEVENT_CLOSE:
              FinalizeWindow( TP3DWindow( wnd ));
          end;
        end;
      SDL_MOUSEMOTION:
        MouseMotion( event.motion );
      SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP:
        MouseButton( event.button );
      SDL_MOUSEWHEEL:
        MouseWheel( event.wheel );
      SDL_KEYDOWN, SDL_KEYUP:
        Keyboard( event.key );
      SDL_TEXTINPUT, SDL_TEXTEDITING:
        TextInput( event.text );
      SDL_RENDER_TARGETS_RESET:
        P3DLog.LogInfo( Self, 'The render targets have been reset and their contents need to be updated!' );{;
      SDL_RENDER_DEVICE_RESET:
        P3DLog.LogInfo( Self, 'The device has been reset and all textures need to be recreated' )}
      SDL_QUITEV:
        Running:= False;
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

procedure TP3DApplication.MouseMotion(Event: TSDL_MouseMotionEvent);
begin
  P3DInput.Mouse.X:= Event.x;
  P3DInput.Mouse.Y:= event.y;
  P3DInput.Mouse.DX:= Event.xrel;
  P3DInput.Mouse.DY:= Event.yrel;
  if ( Assigned( FOnMouseMotion )) then
    FOnMouseMotion( Self, Event );
end;

procedure TP3DApplication.MouseButton(Event: TSDL_MouseButtonEvent);
begin
  P3DInput.Mouse.Buttons[ Event.button - 1 ]:= not Boolean( Event.type_ - SDL_MOUSEBUTTONDOWN );
  P3DInput.Mouse.DButtons[ Event.button - 1 ]:= True;
  if ( Assigned( FOnMouseButton )) then
    FOnMouseButton( Self, Event );
end;

procedure TP3DApplication.MouseWheel(Event: TSDL_MouseWheelEvent);
begin
  P3DInput.Mouse.DWX:= Event.x;
  P3DInput.Mouse.DWY:= Event.y;
  if ( Assigned( FOnMouseWheel )) then
    FOnMouseWheel( Self, Event );
end;

procedure TP3DApplication.Keyboard(Event: TSDL_KeyboardEvent);
var
  ScanCode: TSDL_ScanCode;
begin
  if ( Event._repeat = 1 ) then
    exit;
  //debug_printevent( event );
  P3DInput.Keyboard.Keys[ Event.keysym.scancode ]:= Boolean( Event.state );
  P3DInput.Keyboard.DKeys[ Event.keysym.scancode ]:= SDL2P3DKeystate[ Event.type_ ];
  ScanCode:= SDL_GetScancodeFromName( SDL_GetKeyName( Event.keysym.sym ));
  P3DInput.Keyboard.KeysLayout[ ScanCode ]:= Boolean( Event.state );
  P3DInput.Keyboard.DKeysLayout[ ScanCode ]:= SDL2P3DKeystate[ Event.type_ ];
  if ( Assigned( FOnKey )) then
    FOnKey( Self, Event );
end;

procedure TP3DApplication.TextInput(Event: TSDL_TextInputEvent);
begin
  P3DInput.Keyboard.InputText:= P3DInput.Keyboard.InputText + Event.text;
end;

procedure TP3DApplication.AfterRun;
begin
  MainWindow.Hide;
  if ( Assigned( MainWindow.OnClose )) then
    MainWindow.OnClose( MainWindow );
end;

procedure TP3DApplication.SetActiveWindow(AValue: TP3DWindow);
begin
  if FActiveWindow=AValue then Exit;
  FActiveWindow:=AValue;
end;

constructor TP3DApplication.Create;
begin
  inherited;
  P3DUtilsInit;
  P3DEventsInit;
  MainWindow:= nil;
end;

destructor TP3DApplication.Destroy;
begin
  Finalize;
  SDL_Quit();
  if ( Assigned( MainWindow )) then
    MainWindow.Free;
  inherited Destroy;
end;

procedure TP3DApplication.InitSDL;
begin
  if ( SDL_Init( SDL_INIT_EVERYTHING ) > 0 ) then
    raise Exception.Create('Initialization of SDL2 failed: '
                           + SDL_GetError);
  SDLInitialized:= True;
end;

procedure TP3DApplication.Initialize;
begin
  P3DLog.LogInfo( Self, 'initializing application' ).AttribStrings['function']:= 'Initialize';
  if ( not SDLInitialized ) then
    InitSDL;
  if ( Assigned( OnInit )) then
    OnInit( Self );
end;

procedure TP3DApplication.Finalize;
begin

end;

procedure TP3DApplication.Run;
begin
  try
    P3DLog.LogInfo( Self, 'Running application' ).AttribStrings['function']:= 'Run';
    Running := True;
    while Running do
      begin                // Mainloop
        Events;
        Render;
        if ( Assigned( MainWindow )) then
          MainWindow.Render;
        P3DInput.NextCycle;
        SDL_Delay( 10 );
      end;
  except
    on E: Exception do
      P3DLog.LogException( Self, E.Message );
  end;
  AfterRun;
  P3DLog.LogInfo( Self, 'exit mainloop' ).AttribStrings[ 'function' ]:= 'Run';
  // Resourcen freigeben
  if ( Assigned( OnDeinit )) then
    OnDeinit( Self );
end;

procedure TP3DApplication.LoadConfig(FileName: String);
var
  F: TXMLDocument;
begin
  if ( FileExists( FileName )) then
    begin
      ReadXMLFile( F, FileName );
      LoadConfig( F );
      F.Free;
    end
  else
    raise Exception.Create( 'Could not load config! File not found: "' + FileName + '"' );
end;

procedure TP3DApplication.LoadConfig(XML: TXMLDocument);
var
  Child: TDOMElement;
begin
  Child:= TDOMElement( XML.DocumentElement.FirstChild );
  while ( Assigned( Child )) do
    begin
      LoadConfigSection( XML, Child );

      Child:= TDOMElement( Child.NextSibling );
    end;
end;

procedure TP3DApplication.LoadConfigSection(FileName, NodeName: String);
var
  F: TXMLDocument;
  Node: TDOMElement;
begin
  if ( FileExists( FileName )) then
    begin
      ReadXMLFile( F, FileName );
      Node:= TDOMElement( F.DocumentElement.FindNode( NodeName ));
      if ( Assigned( Node )) then
        LoadConfigSection( F, Node );
      F.Free;
    end
  else
    raise Exception.Create( 'Could not load config! File not found: "' + FileName + '"' );
end;

procedure TP3DApplication.LoadConfigSection(XML: TXMLDocument; DOMNode: TDOMElement);
var
  wnd: TP3DWindow;
begin
  if ( DOMNode.NodeName = TP3DWindow.DOMNodeName ) then
    begin
      wnd:= TP3DWindow.Create;
      wnd.LoadFromDOMNew( DOMNode );
    end
  else if ( DOMNode.NodeName = TP3DSearchPathContainer.DOMNodeName ) then
    P3DSearchPaths.LoadFromDOMNew( DOMNode )
  else if ( DOMNode.NodeName = TP3DLogger.DOMNodeName ) then
    P3DLog.LoadFromDOMNew( DOMNode )
  else if ( DOMNode.NodeName = TP3DShaderNodeLibrary.DOMNodeName ) then
    P3DShaderNodeLib.LoadFromDOMNew( DOMNode );
end;

{$ENDIF}

