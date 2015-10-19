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

procedure TEventsHandler.OptionsBtnClick(Sender: TP3DGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);
begin
  if ( TestPopupMenu.Visible ) then
    TestPopupMenu.Visible:= False
  else
    TestPopupMenu.PopUp( Sender.Left, Sender.Top + Sender.Height );
end;

procedure TEventsHandler.NodeDeleteClick(Sender: TObject);
var
  idx: LongInt;
begin
  if ( Sender is TP3DNodeControl ) then
    begin
      idx:= NodeTree.Nodes.IndexOf( TP3DNodeControl( Sender ).Node );
      if ( idx > -1 ) then
        NodeTree.Nodes.Delete( idx );
      TP3DNodeControl( Sender ).Node:= nil;
      TP3DNodeControl( Sender ).Sockets.Clear( True );
      //Sender.Free;
    end;
end;

procedure TEventsHandler.CompileItmClick(Sender: TP3DGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);
begin
  guiscene.UpdateShader;
end;

procedure TEventsHandler.AddItmClick(Sender: TP3DGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);

  procedure CreateNodeCtrl( ANode: TP3DNode );
  var
    guisno: TP3DNodeControl;
  begin
    guisno:= TP3DNodeControl.Create( ParentList, GUIManager );
    guisno.Node:= ANode;
    guisno.OnDelete:= @NodeDeleteClick;
  end;

var
  Node: TP3DShaderNode;
begin
  if (( Sender is TP3DMenuItem ) and ( Assigned( TP3DMenuItem( Sender ).UserData ))) then
    begin
      Node:= TP3DShaderNode( TP3DMenuItem( Sender ).UserData ).MakeCopy;
      Node.X:= X;
      Node.Y:= Y;
      NodeTree.Nodes.Add( Node );
      CreateNodeCtrl( Node );
    end;
end;

procedure TEventsHandler.DrawScene(Sender: TObject);
begin
  glActiveTexture( GL_TEXTURE0 );
  glEnable( GL_TEXTURE );
  glBindTexture( GL_TEXTURE_2D, tex1.fGLTexture );
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glActiveTexture( GL_TEXTURE1 );
  glEnable( GL_TEXTURE );
  glBindTexture( GL_TEXTURE_2D, tex2.fGLTexture );
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glActiveTexture( GL_TEXTURE2 );
  glEnable( GL_TEXTURE );
  glBindTexture( GL_TEXTURE_2D, tex3.fGLTexture );
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glActiveTexture( GL_TEXTURE3 );
  glEnable( GL_TEXTURE );
  glBindTexture( GL_TEXTURE_2D, tex4.fGLTexture );
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glActiveTexture( GL_TEXTURE0 );
end;

