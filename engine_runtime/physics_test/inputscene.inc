procedure OnMouseMotion( Sender: TSDLWindow; Event: TSDL_MouseMotionEvent );
begin
  character.YawPitchRoll.X:= ( min( 180, max( 0, character.YawPitchRoll.X - Event.yrel / 5 )));
  character.YawPitchRoll.Z:= ( character.YawPitchRoll.Z - Event.xrel / 5 );
end;

procedure OnMouseButton( Sender: TSDLWindow; Event: TSDL_MouseButtonEvent );
begin

end;

procedure OnMouseWheel( Sender: TSDLWindow; Event: TSDL_MouseWheelEvent );
const speed = 1;
begin
//  maincamera.Position:= maincamera.Position + vec3( 0, 0, Event.y * speed );
end;

procedure OnKey( Sender: TSDLWindow; Event: TSDL_KeyboardEvent );
var
  i: Integer;
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
      if ( Event.keysym.scancode = SDL_SCANCODE_SPACE ) then
        begin
          for i:= 0 to High( cubes ) do
            cubes[ i ].Body.SetPosition( Random( 50 ), Random( 50 ), 150 + Random( 50 ));
          for i:= 0 to High( spheres ) do
            spheres[ i ].Body.SetPosition( Random( 50 ), Random( 50 ), 150 + Random( 50 ));
          character.Body.SetPosition( Random( 50 ), Random( 50 ), 150 + Random( 50 ));
        end;
      if ( Event.keysym.scancode = SDL_SCANCODE_KP_1 ) then
        Dec( mrot );
      if ( Event.keysym.scancode = SDL_SCANCODE_KP_3 ) then
        Inc( mrot );
      if ( Event.keysym.scancode = SDL_SCANCODE_KP_8 ) then
        Inc( mshifty );
      if ( Event.keysym.scancode = SDL_SCANCODE_KP_2 ) then
        Dec( mshifty );
      if ( Event.keysym.scancode = SDL_SCANCODE_KP_6 ) then
        Inc( mshiftx );
      if ( Event.keysym.scancode = SDL_SCANCODE_KP_4 ) then
        Dec( mshiftx );
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

const speed = 10;

begin
  Keys:= SDL_GetKeyboardState( nil );
{  if ( KeyPressed( SDL_SCANCODE_S )) then
    character.AddForce( vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad - PI ), cos( - maincamera.YawPitchRoll.Z * deg2rad - PI ), 0 ) * speed );
  if ( KeyPressed( SDL_SCANCODE_W )) then
    character.AddForce( vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad ), cos( - maincamera.YawPitchRoll.Z * deg2rad ), 0 ) * speed );
  if ( KeyPressed( SDL_SCANCODE_A )) then
    character.AddForce( vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad - PI/2 ), cos( - maincamera.YawPitchRoll.Z * deg2rad - PI /2), 0 ) * speed );
  if ( KeyPressed( SDL_SCANCODE_D )) then
    character.AddForce( vec3( sin( - maincamera.YawPitchRoll.Z * deg2rad + PI/2 ), cos( - maincamera.YawPitchRoll.Z * deg2rad + PI /2), 0 ) * speed );}
  if ( KeyPressed( SDL_SCANCODE_S )) then
    character.WalkY( True );
  if ( KeyPressed( SDL_SCANCODE_W )) then
    character.WalkY();
  if ( KeyPressed( SDL_SCANCODE_A )) then
    character.WalkX( True );
  if ( KeyPressed( SDL_SCANCODE_D )) then
    character.WalkX();
end;

