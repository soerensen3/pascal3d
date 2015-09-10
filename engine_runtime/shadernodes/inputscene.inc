procedure OnMouseMotion( Sender: TSDLWindow; Event: TSDL_MouseMotionEvent );
begin
  maincamera.YawPitchRoll.X:= ( min( 180, max( 0, maincamera.YawPitchRoll.X - Event.yrel / 5 )));
  maincamera.YawPitchRoll.Z:= ( maincamera.YawPitchRoll.Z - Event.xrel / 5 );
end;

procedure OnMouseButton( Sender: TSDLWindow; Event: TSDL_MouseButtonEvent );
begin

end;

procedure OnMouseWheel( Sender: TSDLWindow; Event: TSDL_MouseWheelEvent );
const speed = 1;
begin
  ;//maincamera.Position:= maincamera.Position + vec3( 0, 0, Event.y * speed );
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
          ActiveWindow.FullScreen:= not ActiveWindow.FullScreen;

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
  Mesh: TP3DMesh;

  function KeyPressed( Key: Integer ): Boolean;
  begin
    Result:= ( Keys + LongInt( Key ))^ = 1;
  end;

const speed = 0.1;

begin
  Keys:= SDL_GetKeyboardState( nil );
end;

{ TEventsHandler }

procedure TEventsHandler.CompileBtnClick(Sender: TP3DGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);
var
  frag: TP3DShaderNodeOutlineFragment;
  res: TP3DShaderCompiled;
  buffer: TP3DShaderBuffer;
begin
  for frag in shadernodeoutline.Fragments do
    begin
      res:= frag.Compile;
      for buffer in res.Buffers do
        WriteLn( 'result of node "', buffer.Name, '"' + #13#10, buffer.Code );
      res.Free;
    end;
  //guiscene.Shader:= shadernodeoutline;
end;

