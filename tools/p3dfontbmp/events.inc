{$IFDEF IMPLEMENTATION}

procedure TP3DFontBitmap.SetZoom(AValue: Single);
begin
  if FZoom=AValue then Exit;
  FZoom:= Min( 20, Max( 0.1, AValue ));
end;

procedure TP3DFontBitmap.SetOffSet(AValue: TVec2);
begin
  if FOffSet=AValue then Exit;
  FOffSet:= clamp( AValue, vec2( 0 ), vec2( 1 ));
end;

procedure TP3DFontBitmap.Events;
type
  KeyState = array of PUInt8;
var
  Keys: PUInt8;
  Ctrl: TP3DGraphicControl;

  function KeyPressed( Key: Integer ): Boolean;
  begin
    Result:= ( Keys + LongInt( Key ))^ = 1;
  end;

const speed = 0.1;

begin
  inherited Events;

  // Update the input of the GUI
  Ctrl:= P3DUI.Viewer.Controls.MouseRay( P3DUI.Viewer.MouseX, P3DUI.Viewer.MouseY, True );
  P3DUI.LastMouseOverCtrl:= Ctrl;
  P3DUI.Events;
  P3DUI.Input;
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

procedure TP3DFontBitmap.MouseWheel(Event: TSDL_MouseWheelEvent);
begin
  Zoom:= Zoom + Event.y / 3;
end;

procedure TP3DFontBitmap.MouseMotion(Event: TSDL_MouseMotionEvent);
begin
  if ( P3DInput.Mouse.Buttons[ 1 ]) then
    OffSet:= OffSet - ( vec2( P3DInput.Mouse.DX, P3DInput.Mouse.DY ) / Zoom / 10 );
  inherited MouseMotion(Event);
end;



{$ENDIF}
