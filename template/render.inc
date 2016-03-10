{$IFDEF INTERFACE}
procedure Render( Sender: TSDLWindow );
procedure DrawObjects( AScene: TP3DScene );
{$UNDEF INTERFACE}
{$ELSE}

procedure Render( Sender: TSDLWindow );
begin
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour and depth buffer
  mainscene.Render; //RENDER MAIN SCENE
  //DRAW GUI ON TOP AND UPDATE INPUT OF GUI
  //GUIManager.Input;
  //GUIManager.Render;
end;

procedure DrawObjects( AScene: TP3DScene );
begin

  //DRAW CODE HERE
end;


{$ENDIF}
