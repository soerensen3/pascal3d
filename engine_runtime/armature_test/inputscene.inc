const
  move_speed: Single = 1.10;
  mouse_speed: Single = 0.2;

procedure OnMouseMotion( Sender: TP3DApplication; Event: TSDL_MouseMotionEvent );
begin
  CamYawPitchRoll.X:= ( min( 180, max( 0, CamYawPitchRoll.X - Event.yrel * mouse_speed )));
  CamYawPitchRoll.Z:= ( CamYawPitchRoll.Z - Event.xrel * mouse_speed );
end;

procedure OnMouseButton( Sender: TP3DApplication; Event: TSDL_MouseButtonEvent );
begin

end;

procedure OnMouseWheel( Sender: TP3DApplication; Event: TSDL_MouseWheelEvent );
begin

end;

procedure OnKey( Sender: TP3DApplication; Event: TSDL_KeyboardEvent );
begin
  if ( Boolean( Event._repeat )) then
    exit;
  if ( Event.type_ = SDL_KEYDOWN ) then
    begin
      if ( Event.keysym.scancode = SDL_SCANCODE_ESCAPE ) then
        P3DApplication.Running:= False;
      if ( Event.keysym.scancode = SDL_SCANCODE_F5 ) then
        begin
          UnloadShader;
          LoadShader;
          ReloadShader;
        end;
      if ( Event.keysym.scancode = SDL_SCANCODE_F6 ) then
        begin
          UnloadModels;
          LoadModels;
        end;
      if ( Event.keysym.scancode = SDL_SCANCODE_RETURN ) then
        if ( Event.keysym._mod = 256 ) then
          P3DApplication.MainWindow.FullScreen:= not P3DApplication.MainWindow.FullScreen;

      if ( Event.keysym.scancode in [ SDL_SCANCODE_LCTRL, SDL_SCANCODE_RCTRL ]) then
        begin
          CatchMouse:= not CatchMouse;
          WriteLn( 'Catch mouse: ', BoolToStr( CatchMouse, 'Yes', 'No' ));
          SDL_SetRelativeMouseMode( TSDL_BOOL( CatchMouse ));
        end;
    end;
end;

procedure OnInput( Sender: TP3DApplication );

type
  KeyState = array of PUInt8;
var
  Keys: PUInt8;

  function KeyPressed( Key: Integer ): Boolean;
  begin
    Result:= {P3DInput.Keyboard.Keys[ Key ];//}( Keys + LongInt( Key ))^ = 1;
  end;

var
  m: TMat4;
begin
  P3DGUIManager.Input;
  Keys:= SDL_GetKeyboardState( nil );
  m:= TP3DCamera( maincamera.Data ).View;
  mat4inverse( m, m );
  if ( KeyPressed( SDL_SCANCODE_S )) then
    CamPos:= CamPos + m.Row[ 2 ].xyz * move_speed;
  if ( KeyPressed( SDL_SCANCODE_A )) then
    CamPos:= CamPos - m.Row[ 0 ].xyz * move_speed;
  if ( KeyPressed( SDL_SCANCODE_W )) then
    CamPos:= CamPos - m.Row[ 2 ].xyz * move_speed;
  if ( KeyPressed( SDL_SCANCODE_D )) then
    CamPos:= CamPos + m.Row[ 0 ].xyz * move_speed;
  if ( KeyPressed( SDL_SCANCODE_SPACE )) then
    CamPos.Z:= CamPos.Z + move_speed;
  if ( KeyPressed( SDL_SCANCODE_LSHIFT )) then
    CamPos.Z:= CamPos.Z - move_speed;
end;

