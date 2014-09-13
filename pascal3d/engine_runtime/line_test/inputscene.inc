procedure OnMouseMotion( Sender: TSDLWindow; Event: TSDL_MouseMotionEvent );
begin
  maincamera.YawPitchRoll.X:= ( min( 180, max( 0, maincamera.YawPitchRoll.X - Event.yrel / 5 )));
  maincamera.YawPitchRoll.Z:= ( maincamera.YawPitchRoll.Z - Event.xrel / 5 );
end;

procedure OnMouseButton( Sender: TSDLWindow; Event: TSDL_MouseButtonEvent );
begin
  mainwnd.Title:= Format( 'X: %d, Y: %d', [ Event.x, Event.y ]);
  if (( Event.button = 1 ) AND ( Event.state = 1 )) then
    begin
      SetLength( points, Length( points ) + 1 );
      points[ high( points )]:= vec2( Event.x, Event.y );
    end;
end;

procedure OnMouseWheel( Sender: TSDLWindow; Event: TSDL_MouseWheelEvent );
const speed = 1;
begin
  maincamera.Position:= maincamera.Position + vec3( 0, 0, Event.y * speed );
end;

procedure OnKey( Sender: TSDLWindow; Event: TSDL_KeyboardEvent );
begin
  if ( Event.type_ = SDL_KEYDOWN ) then
    begin
      if ( Event.keysym.scancode = SDL_SCANCODE_ESCAPE ) then
        mainwnd.Running:= False;
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
          MainWindow.FullScreen:= not MainWindow.FullScreen;

      if ( Event.keysym.scancode in [ SDL_SCANCODE_LCTRL, SDL_SCANCODE_RCTRL ]) then
        begin
          CatchMouse:= not CatchMouse;
          SDL_SetRelativeMouseMode( TSDL_BOOL( CatchMouse ));
        end;
    end;
end;

procedure OnInput( Sender: TSDLWindow );

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

