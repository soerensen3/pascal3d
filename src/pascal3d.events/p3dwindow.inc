//p3dwindow.inc
//part of p3devents.pas

{$IFDEF INTERFACE}

const
  TP3DMouseEvent: array [ 0..3 ] of Word = ( SDL_MOUSEMOTION, SDL_MOUSEBUTTONDOWN,
                SDL_MOUSEBUTTONUP, SDL_MOUSEWHEEL );
  TP3DJoystickEvent: array [ 0..6 ] of Word = ( SDL_JOYAXISMOTION,
                SDL_JOYBALLMOTION, SDL_JOYHATMOTION, SDL_JOYBUTTONDOWN, SDL_JOYBUTTONUP,
                SDL_JOYDEVICEADDED, SDL_JOYDEVICEREMOVED );
  TP3DGameControllerEvent: array[ 0..5 ] of Word = ( SDL_CONTROLLERAXISMOTION,
                SDL_CONTROLLERBUTTONDOWN, SDL_CONTROLLERBUTTONUP, SDL_CONTROLLERDEVICEADDED,
                SDL_CONTROLLERDEVICEREMOVED, SDL_CONTROLLERDEVICEREMAPPED );
  TP3DTouchEvent: array [ 0..2 ] of Word = ( SDL_FINGERDOWN, SDL_FINGERUP, SDL_FINGERMOTION );
  TP3DGestureEvent: array [ 0..2 ] of Word = ( SDL_DOLLARGESTURE, SDL_DOLLARRECORD, SDL_MULTIGESTURE );

type

  { TP3DWindow }

  TP3DWindow = class;

  TP3DWindowEvent            = procedure( Sender: TP3DWindow );
  TP3DWindowEventKey         = procedure( Sender: TP3DWindow; Event: TSDL_KeyboardEvent );
  TP3DWindowEventMouseButton = procedure( Sender: TP3DWindow; Event: TSDL_MouseButtonEvent );
  TP3DWindowEventMouseMotion = procedure( Sender: TP3DWindow; Event: TSDL_MouseMotionEvent );
  TP3DWindowEventMouseWheel  = procedure( Sender: TP3DWindow; Event: TSDL_MouseWheelEvent );

  TP3DWindow = class ( TP3DInterfacedPersistent )
    protected
      FFullScreen: Boolean;
      FOnClose: TP3DWindowEvent;
      FOnInput: TP3DWindowEvent;
      FOnKey: TP3DWindowEventKey;
      FOnMouseButton: TP3DWindowEventMouseButton;
      FOnMouseMotion: TP3DWindowEventMouseMotion;
      FOnMouseWheel: TP3DWindowEventMouseWheel;
      fOnRender: TP3DWindowEvent;
      FOnResize: TP3DWindowEvent;
      FTitle: String;
      FVisible: Boolean;
      FWindow: PSDL_Window;
      FContext: TSDL_GLContext;
      FShowBorder: Boolean;

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
      procedure ResizeViewport(AWidth, AHeight: Integer);
      procedure SetShowBorder(AValue: Boolean);

    public
      destructor Destroy; override;

      procedure Render;

      constructor Create;
      constructor CreateFromJSONContext( AContext: TP3DJSONContext ); override;
      constructor CreateFrom( Wnd: Pointer );
      constructor CreateDummyWindow;

      procedure Show;
      procedure Hide;
      procedure UpdateViewport;
      class function DOMNodeName: String; override;

      property OnRender: TP3DWindowEvent read fOnRender write fOnRender;

      property OnMouseMotion: TP3DWindowEventMouseMotion read FOnMouseMotion write FOnMouseMotion;
      property OnMouseButton: TP3DWindowEventMouseButton read FOnMouseButton write FOnMouseButton;
      property OnMouseWheel: TP3DWindowEventMouseWheel read FOnMouseWheel write FOnMouseWheel;
      property OnKey: TP3DWindowEventKey read FOnKey write FOnKey;
      property OnInput: TP3DWindowEvent read fOnInput write fOnInput;
      property OnResize: TP3DWindowEvent read FOnResize write FOnResize;
      property OnClose: TP3DWindowEvent read FOnClose write FOnClose;

      property Window: PSDL_Window read FWindow;
      property Context: TSDL_GLContext read FContext;

    published
      property Title: String read GetTitle write SetTitle;
      property Width: Integer read GetWidth write SetWidth;
      property Height: Integer read GetHeight write SetHeight;
      property Left: Integer read GetLeft write SetLeft;
      property Top: Integer read GetTop write SetTop;
      property FullScreen: Boolean read FFullScreen write SetFullScreen;
      property Visible: Boolean read FVisible write SetVisible;
      property ShowBorder: Boolean read FShowBorder write SetShowBorder;
  end;

  TP3DPropertyAccessWindow = specialize gP3DPropertyAccessInterfacedPersistent < TP3DWindow >;
  TP3DWindowList = specialize gP3DInterfacedPersistentList < TP3DWindow >;
  TP3DPropertyAccessWindowList = specialize gP3DListPropertyAccessObject < TP3DWindowList, TP3DWindow, TP3DPropertyAccessWindow >;

  function P3DClientToScreen( Window: TP3DWindow; X, Y: Integer ): TPoint;
  function P3DScreenToClient( Window: TP3DWindow; X, Y: Integer ): TPoint;

{$ENDIF}

{$IFDEF IMPLEMENTATION}


function P3DClientToScreen(Window: TP3DWindow; X, Y: Integer): TPoint;
begin
  Result:= Point( Window.Left + X, Window.Top + Y );
end;

function P3DScreenToClient(Window: TP3DWindow; X, Y: Integer): TPoint;
begin
  Result:= Point( X - Window.Left, Y - Window.Top );
end;



{ TP3DWindow }

procedure TP3DWindow.SetTitle(AValue: String);
begin
  if FTitle=AValue then Exit;
  FTitle:= AValue;
  SDL_SetWindowTitle( window, PChar( AValue ));
end;

procedure TP3DWindow.SetHeight(AValue: Integer);
begin
  SDL_SetWindowSize( window, Width, AValue );
end;

procedure TP3DWindow.SetFullScreen(AValue: Boolean);
begin
  if fFullScreen=AValue then Exit;
  fFullScreen:=AValue;
  SDL_SetWindowFullscreen( window, Integer( fFullScreen ));
end;

procedure TP3DWindow.SetShowBorder( AValue: Boolean );
begin
  if FShowBorder=AValue then Exit;
  FShowBorder:=AValue;

  SDL_SetWindowBordered( Window, TSDL_Bool( FShowBorder ));
end;

function TP3DWindow.GetHeight: Integer;
begin
  SDL_GetWindowSize( Window, nil, @Result );
end;

function TP3DWindow.GetLeft: Integer;
begin
  SDL_GetWindowPosition( Window, @Result, nil );
end;

function TP3DWindow.GetTitle: String;
begin
  Result:= SDL_GetWindowTitle( Window );
end;

function TP3DWindow.GetTop: Integer;
begin
  SDL_GetWindowPosition( Window, nil, @Result );
end;

function TP3DWindow.GetWidth: Integer;
begin
  SDL_GetWindowSize( Window, @Result, nil );
end;

procedure TP3DWindow.SetLeft(AValue: Integer);
begin
  SDL_SetWindowPosition( window, AValue, Top );
end;

procedure TP3DWindow.SetTop(AValue: Integer);
begin
  SDL_SetWindowPosition( window, Left, AValue );
end;

procedure TP3DWindow.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  if ( Visible ) then
    Show
  else
    Hide;
end;

procedure TP3DWindow.SetWidth(AValue: Integer);
begin
  SDL_SetWindowSize( window, AValue, Width );
end;

procedure TP3DWindow.ResizeViewport( AWidth, AHeight: Integer);
begin
  if ( Assigned( P3DViewports )) then
    P3DViewports.Screen:= P3DViewport( 0, 0, AWidth, AHeight );
end;

procedure TP3DWindow.UpdateViewport;
begin
  ResizeViewport( Width, Height );
end;

class function TP3DWindow.DOMNodeName: String;
begin
  Result:= 'window';
end;

destructor TP3DWindow.Destroy;
begin
  SDL_DestroyWindow( Window );
  inherited Destroy;
end;


procedure TP3DWindow.Render;
begin
  //UpdateViewport; //TODO: REMOVE THIS
  P3DApplication.ActiveWindow:= Self;
  if ( Assigned( OnRender )) then
    fOnRender( Self );
  SDL_GL_SwapWindow( window );
end;

constructor TP3DWindow.Create;
begin
  inherited Create;
  if ( not P3DApplication.SDLInitialized ) then
    P3DApplication.InitSDL;
  FWindow:= SDL_CreateWindow( 'SDL WINDOW', 100, 100, 640, 480,
                              SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE );

  if ( not Assigned( window )) then
    raise Exception.Create( 'Could not create SDL Window: ' + SDL_GetError );

  SDL_SetWindowData( FWindow, 'p3dwindow', Self );

  // Choose GL-Version
  SDL_GL_SetAttribute( SDL_GL_CONTEXT_MAJOR_VERSION, 2 ); //}4 );
  SDL_GL_SetAttribute( SDL_GL_CONTEXT_MINOR_VERSION, 1 ); //}5 );
  SDL_GL_SetAttribute( SDL_GL_CONTEXT_PROFILE_MASK, SDL_GL_CONTEXT_PROFILE_COMPATIBILITY );
  // Create a Context
  FContext:= SDL_GL_CreateContext( Window );

  if ( not Assigned( Context )) then
    raise Exception.Create( 'Could not create an OpenGL Context: ' + SDL_GetError );
  FVisible:= True;

  if ( not Assigned( P3DApplication.MainWindow )) then
    P3DApplication.MainWindow:= Self;

  if ( not Assigned( P3DApplication.ActiveWindow )) then
    P3DApplication.ActiveWindow:= Self;

  SDL_GL_SetSwapInterval( 0 );

  P3DDumpGraphicsInfo;

  UpdateViewport;

  P3DApplication.InitializeWindow( Self );

  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Width', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Height', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Visible', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Left', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Height', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'ShowBorder', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'FullScreen', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Title', smAttribute ));
end;

constructor TP3DWindow.CreateFromJSONContext(AContext: TP3DJSONContext);
begin
  Create;
  LoadFromJSONContext( AContext );
end;

constructor TP3DWindow.CreateFrom(Wnd: Pointer);  //TODO: TP3DWindow.CreateFrom
begin                                             //incomplete
  raise Exception.Create( 'Not implemented due to unfixed bug in SDL2' );
  fWindow:= SDL_CreateWindowFrom( Wnd );

  // Window created successfully
  if ( window = nil ) then
    raise Exception.Create('Could not create an SDL Window: ' + SDL_GetError);

  SDL_SetWindowData( fWindow, 'p3dwindow', Self );

  // Create a Context
  FContext:= SDL_GL_CreateContext( Window );

  if ( not Assigned( Context )) then
    raise Exception.Create( 'Could not create an OpenGL Context: ' + SDL_GetError );
  FVisible:= True;

  if ( not Assigned( P3DApplication.ActiveWindow )) then
    P3DApplication.ActiveWindow:= Self;

  SDL_GL_SetSwapInterval( 0 );

  UpdateViewport;

  FVisible:= True;
  P3DApplication.InitializeWindow( Self );

  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Width', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Height', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Visible', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Left', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Height', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'ShowBorder', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'FullScreen', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Title', smAttribute ));
end;

constructor TP3DWindow.CreateDummyWindow;
begin
  Create;
  SDL_SetHint( SDL_HINT_VIDEO_WINDOW_SHARE_PIXEL_FORMAT, PChar( Format( '%p', [ Window ])));
  Hide;
end;

procedure TP3DWindow.Show;
begin
  SDL_ShowWindow( Window );
end;

procedure TP3DWindow.Hide;
begin
  SDL_HideWindow( Window );
end;




{$ENDIF}

