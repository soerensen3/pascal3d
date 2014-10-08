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
unit zgl_main;

{$I zgl_config.cfg}
{$IFDEF iOS}
  {$modeswitch objectivec1}
{$ENDIF}

interface
uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  {$IFDEF USE_X11}
  X, XRandr,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF MACOSX}
  MacOSAll,
  {$ENDIF}
  {$IFDEF iOS}
  iPhoneAll,
  {$ENDIF}
  zgl_types;

const
  cs_ZenGL    = 'ZenGL 0.3.12';
  cs_Date     = '2013.08.12';
  cv_major    = 0;
  cv_minor    = 3;
  cv_revision = 12;

  // zgl_Reg
  SYS_APP_INIT           = $000001;
  SYS_APP_LOOP           = $000002;
  SYS_LOAD               = $000003;
  SYS_DRAW               = $000004;
  SYS_UPDATE             = $000005;
  SYS_EXIT               = $000006;
  SYS_ACTIVATE           = $000007;
  SYS_CLOSE_QUERY        = $000008;
  {$IFDEF iOS}
  SYS_iOS_MEMORY_WARNING     = $000010;
  SYS_iOS_CHANGE_ORIENTATION = $000011;
  {$ENDIF}
  {$IFDEF ANDROID}
  SYS_ANDROID_RESTORE = $00000015;
  {$ENDIF}

  INPUT_MOUSE_MOVE       = $000020;
  INPUT_MOUSE_PRESS      = $000021;
  INPUT_MOUSE_RELEASE    = $000022;
  INPUT_MOUSE_WHEEL      = $000023;
  INPUT_KEY_PRESS        = $000030;
  INPUT_KEY_RELEASE      = $000031;
  INPUT_KEY_CHAR         = $000032;
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  INPUT_TOUCH_MOVE       = $000040;
  INPUT_TOUCH_PRESS      = $000041;
  INPUT_TOUCH_RELEASE    = $000042;
  {$IFEND}

  TEX_FORMAT_EXTENSION   = $000100;
  TEX_FORMAT_FILE_LOADER = $000101;
  TEX_FORMAT_MEM_LOADER  = $000102;
  TEX_CURRENT_EFFECT     = $000103;

  SND_FORMAT_EXTENSION   = $000110;
  SND_FORMAT_FILE_LOADER = $000111;
  SND_FORMAT_MEM_LOADER  = $000112;
  SND_FORMAT_DECODER     = $000113;

  VIDEO_FORMAT_DECODER   = $000130;

  // zgl_Get
  ZENGL_VERSION           = 1;
  ZENGL_VERSION_STRING    = 2;
  ZENGL_VERSION_DATE      = 3;

  DIRECTORY_APPLICATION   = 101;
  DIRECTORY_HOME          = 102;

  LOG_FILENAME            = 203;

  DESKTOP_WIDTH           = 300;
  DESKTOP_HEIGHT          = 301;
  RESOLUTION_LIST         = 302;

  WINDOW_HANDLE           = 400;
  WINDOW_X                = 401;
  WINDOW_Y                = 402;
  WINDOW_WIDTH            = 403;
  WINDOW_HEIGHT           = 404;

  GAPI_CONTEXT            = 500;
  GAPI_MAX_TEXTURE_SIZE   = 501;
  GAPI_MAX_TEXTURE_UNITS  = 502;
  GAPI_MAX_ANISOTROPY     = 503;
  GAPI_CAN_BLEND_SEPARATE = 504;
  GAPI_CAN_AUTOGEN_MIPMAP = 505;

  VIEWPORT_WIDTH          = 600;
  VIEWPORT_HEIGHT         = 601;
  VIEWPORT_OFFSET_X       = 602;
  VIEWPORT_OFFSET_Y       = 603;

  RENDER_FPS              = 700;
  RENDER_BATCHES_2D       = 701;
  RENDER_CURRENT_MODE     = 702;
  RENDER_CURRENT_TARGET   = 703;
  RENDER_VRAM_USED        = 704;

  MANAGER_TIMER           = 800;
  MANAGER_TEXTURE         = 801;
  MANAGER_FONT            = 802;
  MANAGER_RTARGET         = 803;
  MANAGER_SOUND           = 804;
  MANAGER_EMITTER2D       = 805;

  // zgl_Enable/zgl_Disable
  COLOR_BUFFER_CLEAR    = $000001;
  DEPTH_BUFFER          = $000002;
  DEPTH_BUFFER_CLEAR    = $000004;
  DEPTH_MASK            = $000008;
  STENCIL_BUFFER_CLEAR  = $000010;
  CORRECT_RESOLUTION    = $000020;
  CORRECT_WIDTH         = $000040;
  CORRECT_HEIGHT        = $000080;
  APP_USE_AUTOPAUSE     = $000100;
  APP_USE_LOG           = $000200;
  APP_USE_ENGLISH_INPUT = $000400;
  APP_USE_DT_CORRECTION = $000800;
  WND_USE_AUTOCENTER    = $001000;
  SND_CAN_PLAY          = $002000;
  SND_CAN_PLAY_FILE     = $004000;
  CLIP_INVISIBLE        = $008000;
  {$IFDEF iOS}
  SND_ALLOW_BACKGROUND_MUSIC = $100000;
  {$ENDIF}

procedure zgl_Init( FSAA : Byte = 0; StencilBits : Byte = 0 );
procedure zgl_InitToHandle( Handle : Ptr; FSAA : Byte = 0; StencilBits : Byte = 0 );
procedure zgl_Destroy;
procedure zgl_Exit;
procedure zgl_Reg( What : LongWord; UserData : Pointer );
function  zgl_Get( What : LongWord ) : Ptr;
procedure zgl_GetSysDir;
procedure zgl_GetMem( out Mem : Pointer; Size : LongWord );
procedure zgl_FreeMem( var Mem : Pointer );
procedure zgl_FreeStrList( var List : zglTStringList );
procedure zgl_Enable( What : LongWord );
procedure zgl_Disable( What : LongWord );

implementation
uses
  zgl_application,
  zgl_screen,
  zgl_window,
  {$IFNDEF USE_GLES}
  zgl_opengl,
  zgl_opengl_all,
  {$ELSE}
  zgl_opengles,
  zgl_opengles_all,
  {$ENDIF}
  {$IF DEFINED(LINUX) or DEFINED(WINDOWS) or DEFINED(iOS)}
  zgl_file,
  {$IFEND}
  zgl_timers,
  zgl_log,
  {$IF DEFINED(iOS) or DEFINED(ANDROID)}
  zgl_touch,
  {$IFEND}
  zgl_mouse,
  zgl_keyboard,
  zgl_render,
  zgl_render_2d,
  zgl_resources,
  zgl_textures,
  zgl_render_target,
  zgl_font,
  {$IFDEF USE_SENGINE}
  zgl_sengine_2d,
  {$ENDIF}
  {$IFDEF USE_PARTICLES}
  zgl_particles_2d,
  {$ENDIF}
  {$IFDEF USE_SOUND}
  zgl_sound,
  {$IFDEF USE_OGG}
  zgl_lib_ogg,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF USE_VIDEO}
  zgl_video,
  {$IFDEF USE_THEORA}
  zgl_lib_theora,
  {$ENDIF}
  {$ENDIF}
  zgl_utils;

procedure InitSoundVideo;
begin
  {$IFDEF USE_OGG}
  if not InitVorbis() Then
    {$IFNDEF USE_OGG_STATIC}
    log_Add( 'Ogg: Error while loading libraries: ' + libogg + ', ' + libvorbis + ', ' + libvorbisfile )
    {$ENDIF}
  else
    log_Add( 'Ogg: Initialized' );
  {$ENDIF}
  {$IFDEF USE_THEORA}
  if not InitTheora() Then
    {$IFNDEF USE_THEORA_STATIC}
    log_Add( 'Theora: Error while loading library: ' + libtheoradec )
    {$ENDIF}
  else
    log_Add( 'Theora: Initialized' );
  {$ENDIF}
end;

procedure zgl_Init( FSAA : Byte = 0; StencilBits : Byte = 0 );
begin
  oglFSAA    := FSAA;
  oglStencil := StencilBits;

  {$IFDEF iOS}
  if not appPoolInitialized Then
    begin
      appPoolInitialized := TRUE;
      app_InitPool();
      ExitCode := UIApplicationMain( argc, argv, nil, utf8_GetNSString( 'zglCAppDelegate' ) );
      app_FreePool();
      exit;
    end;
  {$ENDIF}

  zgl_GetSysDir();
  log_Init();

  appInitialized := TRUE;
  if not scr_Create() Then exit;
  if not gl_Create() Then exit;
  if not wnd_Create( wndWidth, wndHeight ) Then exit;
  if not gl_Initialize() Then exit;

  InitSoundVideo();

  wnd_ShowCursor( appShowCursor );
  wnd_SetCaption( wndCaption );
  appWork := TRUE;

  {$IF DEFINED(WINDOWS) or DEFINED(LINUX) or DEFINED(MACOSX)}
  if wndFullScreen Then
    scr_SetOptions( wndWidth, wndHeight, scrRefresh, wndFullScreen, scrVSync );
  {$IFEND}
  {$IFDEF iOS}
  key_BeginReadText( '' );
  key_EndReadText();
  scr_SetOptions( scrDesktopW, scrDesktopH, 0, TRUE, TRUE );
  {$ENDIF}

  app_PInit();
  {$IFDEF iOS}
  if ( UIDevice.currentDevice.systemVersion.floatValue() >= 3.1 ) Then
    begin
      scrDisplayLink := CADisplayLink.displayLinkWithTarget_selector( appDelegate, objcselector( 'MainLoop' ) );
      scrDisplayLink.setFrameInterval( 1 );
      scrDisplayLink.addToRunLoop_forMode( NSRunLoop.currentRunLoop(), NSDefaultRunLoopMode );
    end else
      NSTimer.scheduledTimerWithTimeInterval_target_selector_userInfo_repeats( 1 / 60, appDelegate, objcselector( 'MainLoop' ), nil, TRUE );
  exit;
  {$ENDIF}
  {$IFDEF ANDROID}
  exit;
  {$ENDIF}

  app_PLoop();
  zgl_Destroy();
end;

procedure zgl_InitToHandle( Handle : Ptr; FSAA : Byte = 0; StencilBits : Byte = 0 );
begin
  zgl_GetSysDir();
  log_Init();

  oglFSAA    := FSAA;
  oglStencil := StencilBits;

  appInitedToHandle := TRUE;
  if not scr_Create() Then exit;
  if not gl_Create() Then exit;
  {$IFDEF USE_X11}
  wndHandle := TWindow( Handle );
  {$ENDIF}
  {$IFDEF WINDOWS}
  wndHandle := HWND( Handle );
  wndDC     := GetDC( wndHandle );
  {$ENDIF}
  {$IFDEF MACOSX}
  wndHandle := WindowRef( Handle );
  {$ENDIF}
  if not gl_Initialize() Then exit;

  InitSoundVideo();

  wnd_ShowCursor( appShowCursor );
  wnd_SetCaption( wndCaption );
  appWork := TRUE;

  app_PInit();
  app_PLoop();
  zgl_Destroy();
end;

procedure zgl_Destroy;
  var
    i : Integer;
    p : Pointer;
begin
  if appWorkTime <> 0 Then
    log_Add( 'Average FPS: ' + u_IntToStr( Round( appFPSAll / appWorkTime ) ) );

  if Assigned( app_PExit ) Then
    app_PExit();
  res_Free();

  if managerTimer.Count <> 0 Then
    log_Add( 'Timers to free: ' + u_IntToStr( managerTimer.Count ) );
  while managerTimer.Count > 0 do
    begin
      p := managerTimer.First.next;
      timer_Del( zglPTimer( p ) );
    end;

  if managerFont.Count <> 0 Then
    log_Add( 'Fonts to free: ' + u_IntToStr( managerFont.Count ) );
  while managerFont.Count > 0 do
    begin
      p := managerFont.First.next;
      font_Del( zglPFont( p ) );
    end;

  if managerRTarget.Count <> 0 Then
    log_Add( 'Render Targets to free: ' + u_IntToStr( managerRTarget.Count ) );
  while managerRTarget.Count > 0 do
    begin
      p := managerRTarget.First.next;
      rtarget_Del( zglPRenderTarget( p ) );
    end;

  managerZeroTexture := nil;
  if managerTexture.Count.Items <> 0 Then
    log_Add( 'Textures to free: ' + u_IntToStr( managerTexture.Count.Items ) );
  while managerTexture.Count.Items > 0 do
    begin
      p := managerTexture.First.next;
      tex_Del( zglPTexture( p ) );
    end;

  {$IFDEF USE_SENGINE}
  sengine2d_Set( nil );
  sengine2d_ClearAll();
  {$ENDIF}

  {$IFDEF USE_PARTICLES}
  if managerEmitter2D.Count <> 0 Then
    log_Add( 'Emitters2D to free: ' + u_IntToStr( managerEmitter2D.Count ) );
  for i := 0 to managerEmitter2D.Count - 1 do
    emitter2d_Free( managerEmitter2D.List[ i ] );
  SetLength( managerEmitter2D.List, 0 );
  pengine2d_Set( nil );
  pengine2d_ClearAll();
  {$ENDIF}

  {$IFDEF USE_SOUND}
  snd_Free();
  {$ENDIF}

  {$IFDEF USE_VIDEO}
  if managerVideo.Count.Items <> 0 Then
    log_Add( 'Videos to free: ' + u_IntToStr( managerVideo.Count.Items ) );
  while managerVideo.Count.Items > 0 do
    begin
      p := managerVideo.First.next;
      video_Del( zglPVideoStream( p ) );
    end;
  {$ENDIF}

  scr_Destroy();
  gl_Destroy();
  wnd_Destroy();

  {$IFDEF USE_OGG}
  FreeVorbis();
  {$ENDIF}
  {$IFDEF USE_THEORA}
  FreeTheora();
  {$ENDIF}

  appInitialized := FALSE;

  log_Add( 'End' );
  log_Close();
end;

procedure zgl_Exit;
begin
  appWork := FALSE;
end;

procedure zgl_Reg( What : LongWord; UserData : Pointer );
  var
    i : Integer;
begin
  case What of
    // Callback
    SYS_APP_INIT:
      begin
        app_PInit := UserData;
        if not Assigned( UserData ) Then app_PInit := @app_Init;
      end;
    SYS_APP_LOOP:
      begin
        app_PLoop := UserData;
        if not Assigned( UserData ) Then app_PLoop := @app_MainLoop;
      end;
    SYS_LOAD:
      begin
        app_PLoad := UserData;
      end;
    SYS_DRAW:
      begin
        app_PDraw := UserData;
      end;
    SYS_UPDATE:
      begin
        app_PUpdate := UserData;
      end;
    SYS_EXIT:
      begin
        app_PExit := UserData;
      end;
    SYS_ACTIVATE:
      begin
        app_PActivate := UserData;
      end;
    SYS_CLOSE_QUERY:
      begin
        app_PCloseQuery := UserData;
        if not Assigned( app_PCloseQuery ) Then app_PCloseQuery := @app_CloseQuery;
      end;
    {$IFDEF iOS}
    SYS_iOS_MEMORY_WARNING:
      begin
        app_PMemoryWarn := UserData;
      end;
    SYS_iOS_CHANGE_ORIENTATION:
      begin
        app_POrientation := UserData;
      end;
    {$ENDIF}
    {$IFDEF ANDROID}
    SYS_ANDROID_RESTORE:
      begin
        app_PRestore := UserData;
      end;
    {$ENDIF}
    // Input events
    INPUT_MOUSE_MOVE:
      begin
        mouse_PMove := UserData;
      end;
    INPUT_MOUSE_PRESS:
      begin
        mouse_PPress := UserData;
      end;
    INPUT_MOUSE_RELEASE:
      begin
        mouse_PRelease := UserData;
      end;
    INPUT_MOUSE_WHEEL:
      begin
        mouse_PWheel := UserData;
      end;
    INPUT_KEY_PRESS:
      begin
        key_PPress := UserData;
      end;
    INPUT_KEY_RELEASE:
      begin
        key_PRelease := UserData;
      end;
    INPUT_KEY_CHAR:
      begin
        key_PInputChar := UserData;
      end;
    {$IF DEFINED(iOS) or DEFINED(ANDROID)}
    INPUT_TOUCH_MOVE:
      begin
        touch_PMove := UserData;
      end;
    INPUT_TOUCH_PRESS:
      begin
        touch_PPress := UserData;
      end;
    INPUT_TOUCH_RELEASE:
      begin
        touch_PRelease := UserData;
      end;
    {$IFEND}
    // Textures
    TEX_FORMAT_EXTENSION:
      begin
        SetLength( managerTexture.Formats, managerTexture.Count.Formats + 1 );
        managerTexture.Formats[ managerTexture.Count.Formats ].Extension := u_StrUp( PAnsiChar( UserData ) );
      end;
    TEX_FORMAT_FILE_LOADER:
      begin
        managerTexture.Formats[ managerTexture.Count.Formats ].FileLoader := UserData;
      end;
    TEX_FORMAT_MEM_LOADER:
      begin
        managerTexture.Formats[ managerTexture.Count.Formats ].MemLoader := UserData;
        INC( managerTexture.Count.Formats );
      end;
    TEX_CURRENT_EFFECT:
      begin
        tex_CalcCustomEffect := UserData;
      end;
    // Sound
    {$IFDEF USE_SOUND}
    SND_FORMAT_EXTENSION:
      begin
        SetLength( managerSound.Formats, managerSound.Count.Formats + 1 );
        managerSound.Formats[ managerSound.Count.Formats ].Extension := u_StrUp( PAnsiChar( UserData ) );
        managerSound.Formats[ managerSound.Count.Formats ].Decoder   := nil;
      end;
    SND_FORMAT_FILE_LOADER:
      begin
        managerSound.Formats[ managerSound.Count.Formats ].FileLoader := UserData;
      end;
    SND_FORMAT_MEM_LOADER:
      begin
        managerSound.Formats[  managerSound.Count.Formats ].MemLoader := UserData;
        INC( managerSound.Count.Formats );
      end;
    SND_FORMAT_DECODER:
      begin
        for i := 0 to managerSound.Count.Formats - 1 do
          if managerSound.Formats[ i ].Extension = zglPSoundDecoder( UserData ).Ext Then
            managerSound.Formats[ i ].Decoder := UserData;
      end;
    {$ENDIF}
    // Video
    {$IFDEF USE_VIDEO}
    VIDEO_FORMAT_DECODER:
      begin
        SetLength( managerVideo.Decoders, managerVideo.Count.Decoders + 1 );
        managerVideo.Decoders[ managerVideo.Count.Decoders ] := UserData;
        INC( managerVideo.Count.Decoders );
      end;
    {$ENDIF}
  end;
end;

function zgl_Get( What : LongWord ) : Ptr;
begin
  Result := 0;

  if ( not appGotSysDirs ) and ( ( What = DIRECTORY_APPLICATION ) or ( What = DIRECTORY_HOME ) ) Then
    zgl_GetSysDir();

  if ( not scrInitialized ) and ( ( What = DESKTOP_WIDTH ) or ( What = DESKTOP_HEIGHT ) or ( What = RESOLUTION_LIST ) ) Then
    scr_Init();

  case What of
    ZENGL_VERSION: Result := cv_major shl 16 + cv_minor shl 8 + cv_revision;
    ZENGL_VERSION_STRING: Result := Ptr( PAnsiChar( cs_ZenGL ) );
    ZENGL_VERSION_DATE: Result := Ptr( PAnsiChar( cs_Date ) );

    DIRECTORY_APPLICATION: Result := Ptr( PAnsiChar( appWorkDir ) );
    DIRECTORY_HOME: Result := Ptr( PAnsiChar( appHomeDir ) );

    LOG_FILENAME:
      if not appWork Then
        Result := Ptr( @logfile );

    DESKTOP_WIDTH:
    {$IFDEF USE_X11}
      Result := PXRRScreenSize( scrModeList + scrDesktop ).width;
    {$ENDIF}
    {$IFDEF WINDOWS}
      Result := scrDesktop.dmPelsWidth;
    {$ENDIF}
    {$IF DEFINED(DARWIN) or DEFINED(ANDROID)}
      Result := scrDesktopW;
    {$IFEND}
    DESKTOP_HEIGHT:
    {$IFDEF USE_X11}
      Result := PXRRScreenSize( scrModeList + scrDesktop ).height;
    {$ENDIF}
    {$IFDEF WINDOWS}
      Result := scrDesktop.dmPelsHeight;
    {$ENDIF}
    {$IF DEFINED(DARWIN) or DEFINED(ANDROID)}
      Result := scrDesktopH;
    {$IFEND}
    RESOLUTION_LIST: Result := Ptr( @scrResList );

    {$IFNDEF iOS}
    WINDOW_HANDLE: Result := Ptr( wndHandle );
    {$ENDIF}
    WINDOW_X: Result := Ptr( wndX );
    WINDOW_Y: Result := Ptr( wndY );
    WINDOW_WIDTH: Result := Ptr( wndWidth );
    WINDOW_HEIGHT: Result := Ptr( wndHeight );

    {$IFNDEF NO_EGL}
    GAPI_CONTEXT: Result := Ptr( oglContext );
    {$ENDIF}
    GAPI_MAX_TEXTURE_SIZE: Result := oglMaxTexSize;
    GAPI_MAX_TEXTURE_UNITS: Result := oglMaxTexUnits;
    GAPI_MAX_ANISOTROPY: Result := oglMaxAnisotropy;
    GAPI_CAN_BLEND_SEPARATE: Result := Ptr( oglSeparate );
    GAPI_CAN_AUTOGEN_MIPMAP: Result := Ptr( oglCanAutoMipMap );

    RENDER_FPS: Result := appFPS;
    RENDER_BATCHES_2D: Result := b2dBatches + 1;
    RENDER_CURRENT_MODE: Result := oglMode;
    RENDER_CURRENT_TARGET: Result := oglTarget;
    RENDER_VRAM_USED: Result := oglVRAMUsed;

    VIEWPORT_WIDTH: Result := oglWidth - scrSubCX;
    VIEWPORT_HEIGHT: Result := oglHeight - scrSubCY;
    VIEWPORT_OFFSET_X: Result := scrAddCX;
    VIEWPORT_OFFSET_Y: Result := scrAddCY;

    // Managers
    MANAGER_TIMER:     Result := Ptr( @managerTimer );
    MANAGER_TEXTURE:   Result := Ptr( @managerTexture );
    MANAGER_FONT:      Result := Ptr( @managerFont );
    MANAGER_RTARGET:   Result := Ptr( @managerRTarget );
    {$IFDEF USE_SOUND}
    MANAGER_SOUND:     Result := Ptr( @managerSound );
    {$ENDIF}
    {$IFDEF USE_PARTICLES}
    MANAGER_EMITTER2D: Result := Ptr( @managerEmitter2D );
    {$ENDIF}
  end;
end;

procedure zgl_GetSysDir;
{$IFDEF LINUX}
begin
  {$IFNDEF ANDROID}
  appWorkDir := './';
  appHomeDir := FpGetEnv( 'XDG_CONFIG_HOME' );
  if appHomeDir = '' Then
    appHomeDir := FpGetEnv( 'HOME' ) + '/.config/'
  else
    appHomeDir := appHomeDir + '/';
  // for some old distros
  if not file_Exists( appHomeDir ) Then
    file_MakeDir( appHomeDir );
  {$ENDIF}
{$ENDIF}
{$IFDEF WINDOWS}
  const
    APPDATA : PWideChar = 'APPDATA'; // workaround for Delphi 7
  var
    fn  : PWideChar;
    len : Integer;
begin
  wndINST := GetModuleHandle( nil );
  GetMem( fn, 32768 * 2 );
  GetModuleFileNameW( wndINST, fn, 32768 );
  len := WideCharToMultiByte( CP_UTF8, 0, fn, 32768, nil, 0, nil, nil );
  SetLength( appWorkDir, len );
  WideCharToMultiByte( CP_UTF8, 0, fn, 32768, @appWorkDir[ 1 ], len, nil, nil );
  appWorkDir := file_GetDirectory( appWorkDir );
  FreeMem( fn );

  GetMem( fn, 32768 * 2 );
  len := GetEnvironmentVariableW( APPDATA, fn, 32768 );
  len := WideCharToMultiByte( CP_UTF8, 0, fn, len, nil, 0, nil, nil );
  SetLength( appHomeDir, len + 1 );
  WideCharToMultiByte( CP_UTF8, 0, fn, 32768, @appHomeDir[ 1 ], len, nil, nil );
  appHomeDir[ len + 1 ] := '\';
  FreeMem( fn );
{$ENDIF}
{$IFDEF MACOSX}
  var
    appBundle   : CFBundleRef;
    appCFURLRef : CFURLRef;
    appCFString : CFStringRef;
    appPath     : array[ 0..8191 ] of AnsiChar;
begin
  appBundle   := CFBundleGetMainBundle();
  appCFURLRef := CFBundleCopyBundleURL( appBundle );
  appCFString := CFURLCopyFileSystemPath( appCFURLRef, kCFURLPOSIXPathStyle );
  CFStringGetFileSystemRepresentation( appCFString, @appPath[ 0 ], 8192 );
  appWorkDir  := appPath + '/';
  appHomeDir  := FpGetEnv( 'HOME' ) + '/Library/Preferences/';
{$ENDIF}
{$IFDEF iOS}
begin
  appWorkDir := file_GetDirectory( ParamStr( 0 ) );
  appHomeDir := FpGetEnv( 'HOME' ) + '/Documents/';
  if not file_Exists( appHomeDir ) Then
    file_MakeDir( appHomeDir );
{$ENDIF}
  appGotSysDirs := TRUE;
end;

procedure zgl_GetMem( out Mem : Pointer; Size : LongWord );
begin
  if Size > 0 Then
    begin
      GetMem( Mem, Size );
      FillChar( Mem^, Size, 0 );
    end else
      Mem := nil;
end;

procedure zgl_FreeMem( var Mem : Pointer );
begin
  FreeMem( Mem );
  Mem := nil;
end;

procedure zgl_FreeStrList( var List : zglTStringList );
  var
    i : Integer;
begin
  for i := 0 to List.Count - 1 do
    List.Items[ i ] := '';
  List.Count := 0;
  SetLength( List.Items, 0 );
end;

procedure zgl_Enable( What : LongWord );
begin
  appFlags := appFlags or What;

  if What and DEPTH_BUFFER > 0 Then
    glEnable( GL_DEPTH_TEST );

  if What and DEPTH_MASK > 0 Then
    glDepthMask( GL_TRUE );

  if What and CORRECT_RESOLUTION > 0 Then
    appFlags := appFlags or CORRECT_WIDTH or CORRECT_HEIGHT;

  if What and APP_USE_AUTOPAUSE > 0 Then
    appAutoPause := TRUE;

  if What and APP_USE_LOG > 0 Then
    appLog := TRUE;

  {$IFDEF USE_SOUND}
  if What and SND_CAN_PLAY > 0 Then
    sndCanPlay := TRUE;

  if What and SND_CAN_PLAY_FILE > 0 Then
    sndCanPlayFile := TRUE;
  {$ENDIF}

  if What and CLIP_INVISIBLE > 0 Then
    render2dClip := TRUE;

{$IFDEF iOS}
  if What and SND_ALLOW_BACKGROUND_MUSIC > 0 Then
    begin
      sndAllowBackgroundMusic := 1;
      if sndInitialized Then
        AudioSessionSetProperty( LongWord( kAudioSessionProperty_OverrideCategoryMixWithOthers ), SizeOf( sndAllowBackgroundMusic ), @sndAllowBackgroundMusic );
    end;
{$ENDIF}
end;

procedure zgl_Disable( What : LongWord );
begin
  if appFlags and What > 0 Then
    appFlags := appFlags xor What;

  if What and DEPTH_BUFFER > 0 Then
    glDisable( GL_DEPTH_TEST );

  if What and DEPTH_MASK > 0 Then
    glDepthMask( GL_FALSE );

  if What and CORRECT_RESOLUTION > 0 Then
    begin
      scrResCX := 1;
      scrResCY := 1;
      scrAddCX := 0;
      scrAddCY := 0;
      scrSubCX := 0;
      scrSubCY := 0;
    end;

  if What and APP_USE_AUTOPAUSE > 0 Then
    appAutoPause := FALSE;

  if What and APP_USE_LOG > 0 Then
    appLog := FALSE;

  {$IFDEF USE_SOUND}
  if What and SND_CAN_PLAY > 0 Then
    sndCanPlay := FALSE;

  if What and SND_CAN_PLAY_FILE > 0 Then
    sndCanPlayFile := FALSE;
  {$ENDIF}

  if What and CLIP_INVISIBLE > 0 Then
    render2dClip := FALSE;

{$IFDEF iOS}
  if What and SND_ALLOW_BACKGROUND_MUSIC > 0 Then
    begin
      sndAllowBackgroundMusic := 0;
      if sndInitialized Then
        AudioSessionSetProperty( LongWord( kAudioSessionProperty_OverrideCategoryMixWithOthers ), SizeOf( sndAllowBackgroundMusic ), @sndAllowBackgroundMusic );
    end;
{$ENDIF}
end;

end.
