procedure OnMouseMotion( Sender: TP3DApplication; Event: TSDL_MouseMotionEvent );
begin
  maincamera.YawPitchRoll.X:= ( min( 180, max( 0, maincamera.YawPitchRoll.X - Event.yrel / 5 )));
  maincamera.YawPitchRoll.Z:= ( maincamera.YawPitchRoll.Z - Event.xrel / 5 );
end;

procedure OnMouseButton( Sender: TP3DApplication; Event: TSDL_MouseButtonEvent );
begin

end;

procedure OnMouseWheel( Sender: TP3DApplication; Event: TSDL_MouseWheelEvent );
const speed = 0.2;
begin
  maincamera.Position:= maincamera.Position + vec3( 0, 0, Event.y * speed );
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
    Result:= ( Keys + LongInt( Key ))^ = 1;
  end;

const speed = 0.01;

begin
  Keys:= SDL_GetKeyboardState( nil );
  if ( KeyPressed( SDL_SCANCODE_S )) then
    maincamera.Position:= maincamera.Position + vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad - PI ), cos( - maincamera.YawPitchRoll.Z * deg2rad - PI ), 0 ) * speed;
  if ( KeyPressed( SDL_SCANCODE_W )) then
    maincamera.Position:= maincamera.Position + vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad ), cos( - maincamera.YawPitchRoll.Z * deg2rad ), 0 ) * speed;
  if ( KeyPressed( SDL_SCANCODE_A )) then
    maincamera.Position:= maincamera.Position + vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad - PI/2 ), cos( - maincamera.YawPitchRoll.Z * deg2rad - PI /2), 0 ) * speed;
  if ( KeyPressed( SDL_SCANCODE_D )) then
    maincamera.Position:= maincamera.Position + vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad + PI/2 ), cos( - maincamera.YawPitchRoll.Z * deg2rad + PI /2), 0 ) * speed;
  if ( KeyPressed( SDL_SCANCODE_SPACE )) then
    maincamera.Position.Z:= maincamera.Position.Z + speed;
  if ( KeyPressed( SDL_SCANCODE_LSHIFT )) then
    maincamera.Position.Z:= maincamera.Position.Z - speed;
end;

