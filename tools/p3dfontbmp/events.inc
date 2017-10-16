{$IFDEF IMPLEMENTATION}

procedure TP3DFontBitmap.Events;
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
end;


procedure TP3DFontBitmap.Keyboard(Event: TSDL_KeyboardEvent);
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
    end;
end;



{$ENDIF}
