{$IFDEF INTERFACE}
procedure DrawObjects( Scene: TP3DScene ); forward;
{$UNDEF INTERFACE}
{$ELSE}

procedure Render( Sender: TSDLWindow );
var
  p: TVec4;
  BBox: TP3DBoundingBox;
  Obj: Integer;
begin
  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  {TP3DRenderableObject( mainscene.Objects[ 3 ]).Matrix:= mat4rotate( vec3( 0.5, 0.5, 0 ).Normalize, ( SDL_GetTicks / 10000 )) * mat4translate( vec4( TP3DRenderableObject( mainscene.Objects[ 3 ]).Position, 1 ));

  BBox:= TP3DRenderableObject( mainscene.Objects[ 3 ]).CalcBoundingBox();

  Obj:= mainscene.Objects.FindByName( 'BBox' );
  if ( Obj >= 0 ) then
    with ( mainscene.Objects[ Obj ] as TP3DRenderableObject ) do
      Matrix:= mat4scale( vec4(( BBox.Max - BBox.Min ) / 2, 1 )) * mat4translate( vec4( BBox.Center, 1 )); }

  mainscene.Render;
end;

procedure DrawObjects( Scene: TP3DScene );
begin
  if ( not Assigned( ActShad )) then
    exit;


  //TArmature( Mdl.Children[ 1 ]).CurrentFrame:= Round( SDL_GetTicks / 1000 );

  //mainwnd.Title:= Format( 'Current Action: %d - Current frame: %d', [ TArmature( Mdl.Children[ 1 ]).CurrentAction, TArmature( Mdl.Children[ 1 ]).CurrentFrame ]);

//  n.Render({ Scene.view * Scene.proj, [ rfDebugShowArmature ]});
  //DRAW CODE HERE
end;

procedure ResizeWnd( Sender: TSDLWindow );
begin

end;

{$ENDIF}
