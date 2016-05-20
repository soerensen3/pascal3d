procedure OnMouseButton( Sender: TP3DApplication; Event: TSDL_MouseButtonEvent );
begin

end;

procedure OnMouseMotion( Sender: TP3DApplication; Event: TSDL_MouseMotionEvent );
begin
end;

procedure OnMouseWheel( Sender: TP3DApplication; Event: TSDL_MouseWheelEvent );
begin
end;

procedure OnKey( Sender: TP3DApplication; Event: TSDL_KeyboardEvent );
begin
end;

procedure OnInput( Sender: TP3DApplication );
begin
  P3DGUIManager.Input;
end;
