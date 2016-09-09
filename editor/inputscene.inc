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
  procedure UpdateMaterials( Data: TP3DData );
  var
    i: Integer;
  begin
    for i:= 0 to Data.Materials.Count - 1 do
      if ( Data.Materials[ i ] is TP3DMaterialBase ) then
        ( Data.Materials[ i ] as TP3DMaterialBase ).BuildShader();
  end;

var
  i: Integer;
begin
  if ( Event.keysym.scancode = SDL_SCANCODE_F5 ) then
    begin
      P3DShaderNodeLib.Nodes.Clear();
      P3DShaderNodeLib.LoadLibraryPath( P3DSearchPaths.BaseDir + 'shaders/nodes/core/', '*.pmd' );;
      UpdateMaterials( P3DData );
      for i:= 0 to P3DData.Libraries.Count - 1 do
        UpdateMaterials( P3DData.Libraries[ i ]);
    end;
end;

procedure OnInput( Sender: TP3DApplication );
begin
  P3DGUIManager.Input;
end;
