{$IFDEF INTERFACE}
procedure OnMouseMotion( Sender: TP3DApplication; Event: TSDL_MouseMotionEvent );
procedure OnMouseButton( Sender: TP3DApplication; Event: TSDL_MouseButtonEvent );
procedure OnMouseWheel( Sender: TP3DApplication; Event: TSDL_MouseWheelEvent );
procedure OnKey( Sender: TP3DApplication; Event: TSDL_KeyboardEvent );
procedure OnInput( Sender: TP3DApplication );
procedure OnWndResize( Sender: TSDLWindow );
{$ENDIF}
{$IFDEF IMPLEMENTATION}
procedure OnMouseMotion(Sender: TP3DApplication; Event: TSDL_MouseMotionEvent);
begin //ROTATE CAMERA ON MOUSE MOTION
  maincamera.YawPitchRoll.X:= ( min( 180, max( 0, maincamera.YawPitchRoll.X - Event.yrel / 5 )));
  maincamera.YawPitchRoll.Z:= ( maincamera.YawPitchRoll.Z - Event.xrel / 5 );
end;

procedure OnMouseButton(Sender: TP3DApplication; Event: TSDL_MouseButtonEvent);
begin

end;

procedure OnMouseWheel(Sender: TP3DApplication; Event: TSDL_MouseWheelEvent);
begin

end;

procedure OnKey(Sender: TP3DApplication; Event: TSDL_KeyboardEvent);
begin
  if ( Event.type_ = SDL_KEYDOWN ) then
    begin
      if ( Event.keysym.scancode = SDL_SCANCODE_ESCAPE ) then //EXIT ON ESCPAPE KEY
        P3DApplication.Running:= False;
      if ( Event.keysym.scancode = SDL_SCANCODE_F5 ) then  //HOT KEYS FOR RELOADING SHADERS
        begin
          UnloadShader;
          LoadShader;
        end;
      if ( Event.keysym.scancode = SDL_SCANCODE_F6 ) then //HOT KEYS FOR RELOADING MODELS
        begin
          UnloadModels;
          LoadModels;
        end;
      if ( Event.keysym.scancode = SDL_SCANCODE_RETURN ) then //FULL SCREEN ON ALT+ENTER
        if ( Event.keysym._mod = 256 ) then  //ALT KEY
          P3DApplication.MainWindow.FullScreen:= not P3DApplication.MainWindow.FullScreen;

      if ( Event.keysym.scancode in [ SDL_SCANCODE_LCTRL, SDL_SCANCODE_RCTRL ]) then  //CATCH THE MOUSE WHEN CONTROL IS PRESSED
        begin
          CatchMouse:= not CatchMouse;
          SDL_SetRelativeMouseMode( TSDL_BOOL( CatchMouse ));
        end;
    end;
end;

procedure OnInput(Sender: TP3DApplication);

type
  KeyState = array of PUInt8;
var
  Keys: PUInt8;

  function KeyPressed( Key: Integer ): Boolean;
  begin
    Result:= ( Keys + LongInt( Key ))^ = 1;
  end;

const speed = 0.1;

begin
  // A SIMPLE FLY CAM
  Keys:= SDL_GetKeyboardState( nil );
  if ( KeyPressed( SDL_SCANCODE_S )) then
    maincamera.Position:= maincamera.Position - vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad - PI ), cos( - maincamera.YawPitchRoll.Z * deg2rad - PI ), 0 ) * speed;
  if ( KeyPressed( SDL_SCANCODE_W )) then
    maincamera.Position:= maincamera.Position - vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad ), cos( - maincamera.YawPitchRoll.Z * deg2rad ), 0 ) * speed;
  if ( KeyPressed( SDL_SCANCODE_A )) then
    maincamera.Position:= maincamera.Position - vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad - PI/2 ), cos( - maincamera.YawPitchRoll.Z * deg2rad - PI /2), 0 ) * speed;
  if ( KeyPressed( SDL_SCANCODE_D )) then
    maincamera.Position:= maincamera.Position - vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad + PI/2 ), cos( - maincamera.YawPitchRoll.Z * deg2rad + PI /2), 0 ) * speed;
end;

procedure OnWndResize( Sender: TSDLWindow );
var
  vp: TP3DViewport;
begin
  //IF YOU USE p3dgeometry YOU NEED TO DO
  //Setup2D( 0, 0, Sender.Width, Sender.Height );
  if ( P3DViewports.Count > 0 ) then
    begin
      vp:= P3DViewports.VP[ 0 ];
      vp.Width:= Sender.Width;
      vp.Height:= Sender.Height;
      P3DViewports.VP[ 0 ] := vp;
    end;
  //IF YOU HAVE GUI YOU NEED TO DO
  //GUIManager.UpdateExtents;
end;
{$ENDIF}
