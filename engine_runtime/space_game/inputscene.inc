procedure OnMouseMotion( Sender: TSDLWindow; Event: TSDL_MouseMotionEvent );
var
  x,y: Integer;
begin
  case LS of
    0:
      begin
        maincamera.YawPitchRoll.X:= ( min( 180, max( 0, maincamera.YawPitchRoll.X - Event.yrel / 5 )));
        maincamera.YawPitchRoll.Z:= ( maincamera.YawPitchRoll.Z - Event.xrel / 5 );
      end;
    1:
      begin
        Lxrot1:= Lxrot1 - Event.xrel / 1050;
        Lyrot1:= Lyrot1 - Event.yrel / 1050;
        LightSources[ 0 ].Position:= vec3( vec4( 0, 0, Lzoom1, 1 ) * mat4rotate( vec3_Axis_PY, Lxrot1 ) * mat4rotate( vec3_Axis_PX, Lyrot1 ));
      end;
    2:
      begin
        Lxrot2:= Lxrot2 - Event.xrel / 1050;
        Lyrot2:= Lyrot2 - Event.yrel / 1050;
        LightSources[ 1 ].Position:= vec3( vec4( 0, 0, Lzoom2, 1 ) * mat4rotate( vec3_Axis_PY, Lxrot2 ) * mat4rotate( vec3_Axis_PX, Lyrot2 ));
      end;
  end;
//  SDL_WarpMouseInWindow( mainwnd.Window, 0, 0 );
end;

procedure OnMouseButton( Sender: TSDLWindow; Event: TSDL_MouseButtonEvent );
begin

end;

procedure OnMouseWheel( Sender: TSDLWindow; Event: TSDL_MouseWheelEvent );
const
  speed = 5;
begin
  case LS of
    0: maincamera.Position:= maincamera.Position + vec3( 0, 0, Event.y * speed );
    1:
      begin
        LZoom1+= Event.y / 5;
        LightSources[ 0 ].Position:= vec3( vec4( 0, 0, Lzoom1, 1 ) * mat4rotate( vec3_Axis_PY, Lxrot1 ) * mat4rotate( vec3_Axis_PX, Lyrot1 ));
      end;
    2:
      begin
        LZoom2+= Event.y / 5;
        LightSources[ 1 ].Position:= vec3( vec4( 0, 0, Lzoom2, 1 ) * mat4rotate( vec3_Axis_PY, Lxrot2 ) * mat4rotate( vec3_Axis_PX, Lyrot2 ));
      end;
  end;
end;

procedure OnKey( Sender: TSDLWindow; Event: TSDL_KeyboardEvent );
begin
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
          SDL_SetRelativeMouseMode( TSDL_BOOL( CatchMouse ));
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
      if ( Event.keysym.scancode = SDL_SCANCODE_Z ) then
        renderFlags:= renderFlags >< [ rfWireFrame ];
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

const speed = 1;

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

  if ( KeyPressed( SDL_SCANCODE_1 )) then
    LS:= 0;
  if ( KeyPressed( SDL_SCANCODE_2 )) then
    LS:= 1;
  if ( KeyPressed( SDL_SCANCODE_3 )) then
    LS:= 2;
end;

