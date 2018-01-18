{$IFDEF IMPLEMENTATION}

procedure __appclassname__.Events;
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
  inherited Events;

  {$IFDEF GUI}
  // Update the input of the GUI
  if ( not CatchMouse ) then
    P3DGUIManager.Input;
  {$ENDIF}


  // A basic fly cam
  Keys:= SDL_GetKeyboardState( nil );
  if ( KeyPressed( SDL_SCANCODE_W )) then
    CamObj.MoveAlongLocalAxis( vec3_Axis_NZ, speed );
  if ( KeyPressed( SDL_SCANCODE_S )) then
    CamObj.MoveAlongLocalAxis( vec3_Axis_PZ, speed );
  if ( KeyPressed( SDL_SCANCODE_A )) then
    CamObj.MoveAlongLocalAxis( vec3_Axis_NX, speed );
  if ( KeyPressed( SDL_SCANCODE_D )) then
    CamObj.MoveAlongLocalAxis( vec3_Axis_PX, speed );
end;

procedure __appclassname__.MouseMotion(Event: TSDL_MouseMotionEvent);
begin
  inherited MouseMotion( Event );

  // Rotate the camera with the mouse
  if ( CatchMouse ) then
    CamObj.Rotation:= vec3( min( 180, max( 0, camobj.Rotation.X - Event.yrel / 5 )), 0, camobj.Rotation.Z - Event.xrel / 5 );
end;

procedure __appclassname__.Keyboard(Event: TSDL_KeyboardEvent);
begin
  inherited Keyboard(Event);
  if ( Event.type_ = SDL_KEYDOWN ) then
    begin
      if ( Event.keysym.scancode = SDL_SCANCODE_ESCAPE ) then // Stop the program on ESC key
        P3DApplication.Running:= False;
      if ( Event.keysym.scancode = SDL_SCANCODE_RETURN ) then // Toggle fullscreen on ALT+ENTER
        if ( P3DInput.Keyboard.Keys[ P3DK_LALT ] or P3DInput.Keyboard.Keys[ P3DK_RALT ]) then
          // Or also possible
          //if ( Event.keysym._mod = 256 ) then
          P3DApplication.MainWindow.FullScreen:= not P3DApplication.MainWindow.FullScreen;

      if ( Event.keysym.scancode in [ SDL_SCANCODE_LCTRL, SDL_SCANCODE_RCTRL ]) then  // Toggle catching the cursor when control key is pressed
        begin
          CatchMouse:= not CatchMouse;
          SDL_SetRelativeMouseMode( TSDL_BOOL( CatchMouse ));
        end;
    end;
end;


{$ENDIF}
