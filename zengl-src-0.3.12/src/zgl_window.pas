{
 *  Copyright (c) 2012 Andrey Kemka
 *
 *  This software is provided 'as-is', without any express or
 *  implied warranty. In no event will the authors be held
 *  liable for any damages arising from the use of this software.
 *
 *  Permission is granted to anyone to use this software for any purpose,
 *  including commercial applications, and to alter it and redistribute
 *  it freely, subject to the following restrictions:
 *
 *  1. The origin of this software must not be misrepresented;
 *     you must not claim that you wrote the original software.
 *     If you use this software in a product, an acknowledgment
 *     in the product documentation would be appreciated but
 *     is not required.
 *
 *  2. Altered source versions must be plainly marked as such,
 *     and must not be misrepresented as being the original software.
 *
 *  3. This notice may not be removed or altered from any
 *     source distribution.
}
unit zgl_window;

{$I zgl_config.cfg}
{$IFDEF iOS}
  {$modeswitch objectivec1}
{$ENDIF}

interface
{$IFDEF USE_X11}
  uses X, XLib, XUtil;
{$ENDIF}
{$IFDEF WINDOWS}
  uses Windows;
{$ENDIF}
{$IFDEF MACOSX}
  uses MacOSAll;
{$ENDIF}
{$IFDEF iOS}
  uses iPhoneAll, CGGeometry;
{$ENDIF}

function  wnd_Create( Width, Height : Integer ) : Boolean;
procedure wnd_Destroy;
procedure wnd_Update;

procedure wnd_SetCaption( const NewCaption : UTF8String );
procedure wnd_SetSize( Width, Height : Integer );
procedure wnd_SetPos( X, Y : Integer );
procedure wnd_ShowCursor( Show : Boolean );

var
  wndX          : Integer;
  wndY          : Integer;
  wndWidth      : Integer = 800;
  wndHeight     : Integer = 600;
  wndFullScreen : Boolean;
  wndCaption    : UTF8String;

  {$IFDEF USE_X11}
  wndHandle      : TWindow;
  wndRoot        : TWindow;
  wndClass       : TXClassHint;
  wndAttr        : TXSetWindowAttributes;
  wndTitle       : TXTextProperty;
  wndValueMask   : LongWord;
  wndDestroyAtom : TAtom;
  wndProtocols   : TAtom;
  {$ENDIF}
  {$IFDEF WINDOWS}
  wndFirst     : Boolean = TRUE; // Microsoft Sucks! :)
  wndHandle    : HWND;
  wndDC        : HDC;
  wndINST      : HINST;
  wndClass     : TWndClassExW;
  wndClassName : PWideChar = 'ZenGL';
  wndStyle     : LongWord;
  wndCpnSize   : Integer;
  wndBrdSizeX  : Integer;
  wndBrdSizeY  : Integer;
  wndCaptionW  : PWideChar;
  {$ENDIF}
  {$IFDEF MACOSX}
  wndHandle  : WindowRef;
  wndAttr    : WindowAttributes;
  wndEvents  : array[ 0..14 ] of EventTypeSpec;
  wndMouseIn : Boolean;
  {$ENDIF}
  {$IFDEF iOS}
  wndHandle   : UIWindow;
  wndViewCtrl : UIViewController;
  wndPortrait : Boolean;
  {$ENDIF}
  {$IFDEF ANDROID}
  wndHandle : Integer; // dummy
  {$ENDIF}

implementation
uses
  zgl_main,
  zgl_application,
  zgl_screen,
  {$IFNDEF USE_GLES}
  zgl_opengl,
  zgl_opengl_all,
  {$ELSE}
  zgl_opengles,
  zgl_opengles_all,
  {$ENDIF}
  zgl_render,
  zgl_utils;

{$IFNDEF FPC}
// Various versions of Delphi... sucks again
function LoadCursorW(hInstance: HINST; lpCursorName: PWideChar): HCURSOR; stdcall; external user32 name 'LoadCursorW';
{$ENDIF}

{$IFDEF USE_X11}
procedure wnd_SetHints( Initialized : Boolean = TRUE );
  var
    sizehints : TXSizeHints;
begin
  FillChar( sizehints, SizeOf( TXSizeHints ), 0 );
  if wndFullScreen and Initialized Then
    sizehints.flags    := PBaseSize or PWinGravity
  else
    sizehints.flags    := PPosition or PSize or PMinSize or PMaxSize;
  sizehints.x          := wndX;
  sizehints.y          := wndY;
  sizehints.width      := wndWidth;
  sizehints.height     := wndHeight;
  sizehints.min_width  := wndWidth;
  sizehints.max_width  := wndWidth;
  sizehints.min_height := wndHeight;
  sizehints.max_height := wndHeight;
  XSetWMNormalHints( scrDisplay, wndHandle, @sizehints );
end;
{$ENDIF}

procedure wnd_Select;
begin
  if appInitedToHandle Then exit;

{$IFDEF USE_X11}
  XMapWindow( scrDisplay, wndHandle );
{$ENDIF}
{$IFDEF WINDOWS}
  BringWindowToTop( wndHandle );
{$ENDIF}
{$IFDEF MACOSX}
  SelectWindow( wndHandle );
  ShowWindow( wndHandle );
  if wndFullScreen Then
    wnd_SetPos( 0, 0 );
{$ENDIF}
{$IFDEF iOS}
  wndHandle.makeKeyAndVisible();
{$ENDIF}
end;

function wnd_Create( Width, Height : Integer ) : Boolean;
  {$IFDEF MACOSX}
  var
    size   : MacOSAll.Rect;
    status : OSStatus;
  {$ENDIF}
begin
  Result := TRUE;
  if wndHandle <> {$IFNDEF DARWIN} 0 {$ELSE} nil {$ENDIF} Then exit;

  Result    := FALSE;
  wndX      := 0;
  wndY      := 0;
  wndWidth  := Width;
  wndHeight := Height;

  if ( not wndFullScreen ) and ( appFlags and WND_USE_AUTOCENTER > 0 ) Then
    begin
      wndX := ( zgl_Get( DESKTOP_WIDTH ) - wndWidth ) div 2;
      wndY := ( zgl_Get( DESKTOP_HEIGHT ) - wndHeight ) div 2;
    end;
{$IFDEF USE_X11}
  FillChar( wndAttr, SizeOf( wndAttr ), 0 );
  wndAttr.colormap   := XCreateColormap( scrDisplay, wndRoot, oglVisualInfo.visual, AllocNone );
  wndAttr.event_mask := ExposureMask or FocusChangeMask or ButtonPressMask or ButtonReleaseMask or PointerMotionMask or KeyPressMask or KeyReleaseMask or StructureNotifyMask;
  wndValueMask       := CWColormap or CWEventMask or CWOverrideRedirect or CWBorderPixel or CWBackPixel;
  wndHandle          := XCreateWindow( scrDisplay, wndRoot, wndX, wndY, wndWidth, wndHeight, 0, oglVisualInfo.depth, InputOutput, oglVisualInfo.visual, wndValueMask, @wndAttr );

  if wndHandle = 0 Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  wnd_Select();

  wndClass.res_name  := 'ZenGL';
  wndClass.res_class := 'ZenGL Class';
  XSetClassHint( scrDisplay, wndHandle, @wndClass );
  wnd_SetHints( FALSE );

  wndDestroyAtom := XInternAtom( scrDisplay, 'WM_DELETE_WINDOW', FALSE );
  wndProtocols   := XInternAtom( scrDisplay, 'WM_PROTOCOLS', FALSE );
  XSetWMProtocols( scrDisplay, wndHandle, @wndDestroyAtom, 1 );
{$ENDIF}
{$IFDEF WINDOWS}
  wndCpnSize  := GetSystemMetrics( SM_CYCAPTION  );
  wndBrdSizeX := GetSystemMetrics( SM_CXDLGFRAME );
  wndBrdSizeY := GetSystemMetrics( SM_CYDLGFRAME );

  with wndClass do
    begin
      cbSize        := SizeOf( TWndClassExW );
      style         := CS_DBLCLKS or CS_OWNDC;
      lpfnWndProc   := @app_ProcessMessages;
      cbClsExtra    := 0;
      cbWndExtra    := 0;
      hInstance     := wndINST;
      hIcon         := LoadIconW  ( wndINST, 'MAINICON' );
      hIconSm       := 0;
      hCursor       := LoadCursorW( wndINST, PWideChar( IDC_ARROW ) );
      lpszMenuName  := nil;
      hbrBackGround := GetStockObject( BLACK_BRUSH );
      lpszClassName := wndClassName;
    end;

  if RegisterClassExW( wndClass ) = 0 Then
    begin
      u_Error( 'Cannot register window class' );
      exit;
    end;

  if wndFullScreen Then
    wndStyle := WS_POPUP or WS_VISIBLE or WS_SYSMENU
  else
    wndStyle := WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;
  {$IFNDEF USE_GLES}
  if oglFormat = 0 Then
    wndHandle := CreateWindowExW( WS_EX_TOOLWINDOW, wndClassName, wndCaptionW, WS_POPUP, 0, 0, 0, 0, 0, 0, 0, nil )
  else
  {$ENDIF}
    wndHandle := CreateWindowExW( WS_EX_APPWINDOW or WS_EX_TOPMOST * Byte( wndFullScreen ), wndClassName, wndCaptionW, wndStyle, wndX, wndY,
                                  wndWidth  + ( wndBrdSizeX * 2 ) * Byte( not wndFullScreen ),
                                  wndHeight + ( wndBrdSizeY * 2 + wndCpnSize ) * Byte( not wndFullScreen ), 0, 0, wndINST, nil );

  if wndHandle = 0 Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  wndDC := GetDC( wndHandle );
  if wndDC = 0 Then
    begin
      u_Error( 'Cannot get device context' );
      exit;
    end;
  wnd_Select();
{$ENDIF}
{$IFDEF MACOSX}
  size.Left   := wndX;
  size.Top    := wndY;
  size.Right  := wndX + wndWidth;
  size.Bottom := wndY + wndHeight;
  wndAttr     := kWindowCloseBoxAttribute or kWindowCollapseBoxAttribute or kWindowStandardHandlerAttribute;// or kWindowCompositingAttribute;
  if wndFullScreen Then
    wndAttr := wndAttr or kWindowNoTitleBarAttribute;
  status      := CreateNewWindow( kDocumentWindowClass, wndAttr, size, wndHandle );

  if ( status <> noErr ) or ( wndHandle = nil ) Then
    begin
      u_Error( 'Cannot create window' );
      exit;
    end;

  // Window
  wndEvents[ 0 ].eventClass := kEventClassWindow;
  wndEvents[ 0 ].eventKind  := kEventWindowClosed;
  wndEvents[ 1 ].eventClass := kEventClassWindow;
  wndEvents[ 1 ].eventKind  := kEventWindowActivated;
  wndEvents[ 2 ].eventClass := kEventClassWindow;
  wndEvents[ 2 ].eventKind  := kEventWindowDeactivated;
  wndEvents[ 3 ].eventClass := kEventClassWindow;
  wndEvents[ 3 ].eventKind  := kEventWindowCollapsed;
  wndEvents[ 4 ].eventClass := kEventClassWindow;
  wndEvents[ 4 ].eventKind  := kEventWindowBoundsChanged;
  // Keyboard
  wndEvents[ 5 ].eventClass := kEventClassKeyboard;
  wndEvents[ 5 ].eventKind  := kEventRawKeyDown;
  wndEvents[ 6 ].eventClass := kEventClassKeyboard;
  wndEvents[ 6 ].eventKind  := kEventRawKeyUp;
  wndEvents[ 7 ].eventClass := kEventClassKeyboard;
  wndEvents[ 7 ].eventKind  := kEventRawKeyRepeat;
  wndEvents[ 8 ].eventClass := kEventClassKeyboard;
  wndEvents[ 8 ].eventKind  := kEventRawKeyModifiersChanged;
  // Mouse
  wndEvents[ 9 ].eventClass  := kEventClassMouse;
  wndEvents[ 9 ].eventKind   := kEventMouseMoved;
  wndEvents[ 10 ].eventClass := kEventClassMouse;
  wndEvents[ 10 ].eventKind  := kEventMouseDown;
  wndEvents[ 11 ].eventClass := kEventClassMouse;
  wndEvents[ 11 ].eventKind  := kEventMouseUp;
  wndEvents[ 12 ].eventClass := kEventClassMouse;
  wndEvents[ 12 ].eventKind  := kEventMouseWheelMoved;
  wndEvents[ 13 ].eventClass := kEventClassMouse;
  wndEvents[ 13 ].eventKind  := kEventMouseDragged;
  // Command
  wndEvents[ 14 ].eventClass := kEventClassCommand;
  wndEvents[ 14 ].eventKind  := kEventProcessCommand;
  InstallEventHandler( GetApplicationEventTarget, NewEventHandlerUPP( @app_ProcessMessages ), 15, @wndEvents[ 0 ], nil, nil );
  wnd_Select();
{$ENDIF}
{$IFDEF iOS}
  // always fullscreen
  wndFullScreen := TRUE;

  UIApplication.sharedApplication.setStatusBarHidden( wndFullScreen );
  wndHandle := UIWindow.alloc().initWithFrame( UIScreen.mainScreen.bounds );
  wndViewCtrl := zglCiOSViewController.alloc().init();

  wnd_Select();
{$ENDIF}
  Result := TRUE;
end;

procedure wnd_Destroy;
begin
{$IFDEF USE_X11}
  if not appInitedToHandle Then
    XDestroyWindow( scrDisplay, wndHandle );
  XSync( scrDisplay, X_FALSE );
{$ENDIF}
{$IFDEF WINDOWS}
  if ( wndDC > 0 ) and ( ReleaseDC( wndHandle, wndDC ) = 0 ) Then
    begin
      u_Error( 'Cannot release device context' );
      wndDC := 0;
    end;

  if wndFirst or ( not appInitedToHandle ) Then
    begin
      if ( wndHandle <> 0 ) and ( not DestroyWindow( wndHandle ) ) Then
        begin
          u_Error( 'Cannot destroy window' );
          wndHandle := 0;
        end;

      if not UnRegisterClassW( wndClassName, wndINST ) Then
        begin
          u_Error( 'Cannot unregister window class' );
          wndINST := 0;
        end;
    end;
{$ENDIF}
{$IFDEF MACOSX}
  if not appInitedToHandle Then
    ReleaseWindow( wndHandle );
{$ENDIF}
  wndHandle := {$IFNDEF DARWIN} 0 {$ELSE} nil {$ENDIF};
end;

procedure wnd_Update;
  {$IFDEF USE_X11}
  var
    event : TXEvent;
  {$ENDIF}
  {$IFDEF WINDOWS}
  var
    FullScreen : Boolean;
  {$ENDIF}
begin
  if appInitedToHandle Then exit;

{$IFDEF USE_X11}
  XSync( scrDisplay, X_TRUE );
  wnd_SetHints();

  FillChar( event, SizeOf( TXEvent ), 0 );
  event._type                := ClientMessage;
  event.xclient._type        := ClientMessage;
  event.xclient.send_event   := X_TRUE;
  event.xclient.window       := wndHandle;
  event.xclient.message_type := XInternAtom( scrDisplay, '_NET_WM_STATE', FALSE );
  event.xclient.format       := 32;
  event.xclient.data.l[ 0 ]  := Integer( wndFullScreen );
  event.xclient.data.l[ 1 ]  := XInternAtom( scrDisplay, '_NET_WM_STATE_FULLSCREEN', FALSE );
  XSendEvent( scrDisplay, wndRoot, False, SubstructureRedirectMask or SubstructureNotifyMask, @event );
{$ENDIF}
{$IFDEF WINDOWS}
  if appFocus Then
    FullScreen := wndFullScreen
  else
    begin
      FullScreen := FALSE;
      if ( wndWidth >= zgl_Get( DESKTOP_WIDTH ) ) and ( wndHeight >= zgl_Get( DESKTOP_HEIGHT ) ) Then
        begin
          ShowWindow( wndHandle, SW_MINIMIZE );
          exit;
        end;
    end;

  if FullScreen Then
    wndStyle := WS_POPUP or WS_VISIBLE or WS_SYSMENU
  else
    wndStyle := WS_CAPTION or WS_MINIMIZEBOX or WS_SYSMENU or WS_VISIBLE;

  SetWindowLongW( wndHandle, GWL_STYLE, LongInt(wndStyle) );
  SetWindowLongW( wndHandle, GWL_EXSTYLE, WS_EX_APPWINDOW or WS_EX_TOPMOST * Byte( FullScreen ) );
{$ENDIF}
{$IFDEF MACOSX}
  if wndFullScreen Then
    ChangeWindowAttributes( wndHandle, kWindowNoTitleBarAttribute, kWindowResizableAttribute )
  else
    ChangeWindowAttributes( wndHandle, kWindowResizableAttribute, kWindowNoTitleBarAttribute );
  // Apple and their magic driving me crazy...
  ChangeWindowAttributes( wndHandle, 0, kWindowResizableAttribute );

  aglSetCurrentContext( oglContext );
{$ENDIF}
{$IFDEF iOS}
  UIApplication.sharedApplication.setStatusBarHidden( wndFullScreen );
{$ENDIF}
  appWork := TRUE;
  wnd_SetCaption( wndCaption );

  if ( not wndFullScreen ) and ( appFlags and WND_USE_AUTOCENTER > 0 ) Then
    wnd_SetPos( ( zgl_Get( DESKTOP_WIDTH ) - wndWidth ) div 2, ( zgl_Get( DESKTOP_HEIGHT ) - wndHeight ) div 2 );
  wnd_SetSize( wndWidth, wndHeight );
end;

procedure wnd_SetCaption( const NewCaption : UTF8String );
  {$IFDEF USE_X11}
  var
    err : Integer;
    str : PAnsiChar;
  {$ENDIF}
  {$IFDEF WINDOWS}
  var
    len : Integer;
  {$ENDIF}
  {$IFDEF MACOSX}
  var
    str : CFStringRef;
  {$ENDIF}
begin
  if appInitedToHandle Then exit;

  wndCaption := utf8_Copy( NewCaption );
{$IFDEF USE_X11}
  if wndHandle <> 0 Then
    begin
      str := utf8_GetPAnsiChar( wndCaption );
      err := Xutf8TextListToTextProperty( scrDisplay, @str, 1, XUTF8StringStyle, @wndTitle );

      if err = 0 Then
        begin
          XSetWMName( scrDisplay, wndHandle, @wndTitle );
          XSetWMIconName( scrDisplay, wndHandle, @wndTitle );
        end;
      FreeMem( str );
      XFree( wndTitle.value );
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  if wndHandle <> 0 Then
    begin
      len := MultiByteToWideChar( CP_UTF8, 0, @wndCaption[ 1 ], Length( wndCaption ), nil, 0 );
      if Assigned( wndCaptionW ) Then
        FreeMem( wndCaptionW );
      GetMem( wndCaptionW, len * 2 + 2 );
      wndCaptionW[ len ] := #0;
      MultiByteToWideChar( CP_UTF8, 0, @wndCaption[ 1 ], Length( wndCaption ), wndCaptionW, len );

      SetWindowTextW( wndHandle, wndCaptionW );
    end;
{$ENDIF}
{$IFDEF MACOSX}
  if Assigned( wndHandle ) Then
    begin
      str := CFStringCreateWithPascalString( nil, wndCaption, kCFStringEncodingUTF8 );
      SetWindowTitleWithCFString( wndHandle, str );
      CFRelease( str );
      wnd_Select();
    end;
{$ENDIF}
end;

procedure wnd_SetSize( Width, Height : Integer );
begin
  wndWidth  := Width;
  wndHeight := Height;
{$IFDEF USE_X11}
  if ( not appInitedToHandle ) and ( wndHandle <> 0 ) Then
    begin
      wnd_SetHints();
      XResizeWindow( scrDisplay, wndHandle, wndWidth, wndHeight );
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  if not appInitedToHandle Then
    wnd_SetPos( wndX, wndY );
{$ENDIF}
{$IFDEF MACOSX}
  if Assigned( wndHandle ) Then
    begin
      if not appInitedToHandle Then
        begin
          SizeWindow( wndHandle, wndWidth, wndHeight, TRUE );
          aglUpdateContext( oglContext );
          wnd_Select();
        end else
          wnd_SetPos( wndX, wndY );
    end;
{$ENDIF}
{$IFDEF iOS}
  eglContext.renderbufferStorage_fromDrawable( GL_RENDERBUFFER, eglSurface );
{$ENDIF}
  oglWidth   := Width;
  oglHeight  := Height;
  oglTargetW := Width;
  oglTargetH := Height;
  if appFlags and CORRECT_RESOLUTION > 0 Then
    scr_CorrectResolution( scrResW, scrResH )
  else
    SetCurrentMode();
end;

procedure wnd_SetPos( X, Y : Integer );
  {$IFDEF MACOSX}
  var
    clipRgn : RgnHandle;
  {$ENDIF}
begin
  wndX := X;
  wndY := Y;

  if appInitedToHandle Then
    begin
      {$IFDEF MACOSX}
      clipRgn := NewRgn();
      SetRectRgn( clipRgn, X, Y, X + wndWidth, Y + wndHeight );
      aglSetInteger( oglContext, AGL_CLIP_REGION, clipRgn );
      aglEnable( oglContext, AGL_CLIP_REGION );
      DisposeRgn( clipRgn );
      {$ENDIF}
      exit;
    end;

{$IFDEF USE_X11}
  if wndHandle <> 0 Then
    if not wndFullScreen Then
      XMoveWindow( scrDisplay, wndHandle, X, Y )
    else
      XMoveWindow( scrDisplay, wndHandle, 0, 0 );
{$ENDIF}
{$IFDEF WINDOWS}
  if wndHandle <> 0 Then
    if ( not wndFullScreen ) or ( not appFocus ) Then
      SetWindowPos( wndHandle, HWND_NOTOPMOST, wndX, wndY, wndWidth + ( wndBrdSizeX * 2 ), wndHeight + ( wndBrdSizeY * 2 + wndCpnSize ), SWP_NOACTIVATE )
    else
      SetWindowPos( wndHandle, HWND_TOPMOST, 0, 0, wndWidth, wndHeight, SWP_NOACTIVATE );
{$ENDIF}
{$IFDEF MACOSX}
  if Assigned( wndHandle ) Then
    if not wndFullScreen Then
      MoveWindow( wndHandle, wndX, wndY, TRUE )
    else
      MoveWindow( wndHandle, 0, 0, TRUE );
{$ENDIF}
{$IFDEF iOS}
  wndX := 0;
  wndY := 0;
{$ENDIF}
end;

procedure wnd_ShowCursor( Show : Boolean );
{$IFDEF USE_X11}
  var
    mask   : TPixmap;
    xcolor : TXColor;
begin
  if appInitedToHandle Then exit;

  appShowCursor := Show;

  if wndHandle = 0 Then exit;
  if Show Then
    begin
      if appCursor <> None Then
        begin
          XFreeCursor( scrDisplay, appCursor );
          appCursor := None;
          XDefineCursor( scrDisplay, wndHandle, appCursor );
        end;
    end else
      begin
        mask := XCreatePixmap( scrDisplay, wndRoot, 1, 1, 1 );
        FillChar( xcolor, SizeOf( xcolor ), 0 );
        appCursor := XCreatePixmapCursor( scrDisplay, mask, mask, @xcolor, @xcolor, 0, 0 );
        XDefineCursor( scrDisplay, wndHandle, appCursor );
      end;
{$ENDIF}
{$IF DEFINED(WINDOWS) or DEFINED(MACOSX) or DEFINED(iOS) or DEFINED(ANDROID)}
begin
  if appInitedToHandle Then exit;

  appShowCursor := Show;
{$IFEND}
end;

initialization
  wndCaption := cs_ZenGL;

{$IFDEF WINDOWS}
finalization
  FreeMem( wndCaptionW );
{$ENDIF}

end.
