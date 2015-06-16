{$IFDEF INTERFACE}
procedure DrawObjects( Scene: tScene ); forward;
{$UNDEF INTERFACE}
{$ELSE}

procedure Render( Sender: TSDLWindow );
var
  p: TVec4;
begin
  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  mainscene.Render;
end;

procedure DrawObjects( Scene: tScene );
begin
  if ( not Assigned( ActShad )) then
    exit;


  TArmature( Mdl.Children[ 1 ]).CurrentFrame:= Round( SDL_GetTicks / 1000 );

  mainwnd.Title:= Format( 'Current Action: %d - Current frame: %d', [ TArmature( Mdl.Children[ 1 ]).CurrentAction, TArmature( Mdl.Children[ 1 ]).CurrentFrame ]);

  Mdl.Render( mat4( 1 ), [ rfDebugShowArmature ]);
  //DRAW CODE HERE
end;

procedure ResizeWnd( Sender: TSDLWindow );
begin

end;

{$ENDIF}
