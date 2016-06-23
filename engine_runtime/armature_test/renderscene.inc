{$IFDEF INTERFACE}

{$UNDEF INTERFACE}
{$ELSE}

procedure Render( Sender: TP3DWindow );
var
  p: TVec4;
  BBox: TP3DBoundingBox;
  Obj, m: Integer;
  proj: TMat4;

const
  hsize = 4096;
  cell = 64;
  hcell = hsize/cell*2;
begin
  glEnable( GL_DEPTH_TEST );
  glClearColor(0.451, 0.404, 0.404, 1.0);                      // Set the Background colour of or scene
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );   // Clear the colour buffer

  maincamera.Position:= CamPos;
  maincamera.Rotation:= CamYawPitchRoll;
  skycamera.Rotation:= CamYawPitchRoll;


  TP3DCamera( skycamera.Data ).UpdateMatrices( skycamera );
  TP3DCamera( maincamera.Data ).UpdateMatrices( maincamera );

  skyboxscene.Render;
  glClear( GL_DEPTH_BUFFER_BIT );   // Clear the depth buffer

  {TP3DRenderableObject( mainscene.Objects[ 3 ]).Matrix:= mat4rotate( vec3( 0.5, 0.5, 0 ).Normalize, ( SDL_GetTicks / 10000 )) * mat4translate( vec4( TP3DRenderableObject( mainscene.Objects[ 3 ]).Position, 1 ));

  BBox:= TP3DRenderableObject( mainscene.Objects[ 3 ]).CalcBoundingBox();

  Obj:= mainscene.Objects.FindByName( 'BBox' );
  if ( Obj >= 0 ) then
    with ( mainscene.Objects[ Obj ] as TP3DRenderableObject ) do
      Matrix:= mat4scale( vec4(( BBox.Max - BBox.Min ) / 2, 1 )) * mat4translate( vec4( BBox.Center, 1 )); }

  if( P3DData.Libraries.Count > 0 ) then
    begin
      Obj:= P3DData.Libraries[ 0 ].Objects.FindByName( 'Grid' );
      if ( Obj >= 0 ) then
        with ( P3DData.Libraries[ 0 ].Objects[ Obj ] as TP3DActor ) do
          begin
            Position:= vec3( Int( CamPos.X / hcell ) * hcell, Int( CamPos.Y / hcell ) * hcell, 0 );
            Scale:= vec3( hsize, hsize, hsize );
            TP3DMesh( Data ).MaterialGroups[ 0 ].Material.Shader.Enable;
            glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'uvX' ), Round( Int( -CamPos.X / hcell )));
            glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'uvY' ), Round( Int( -CamPos.Y / hcell )));
            glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'height' ), -0.05 );
            glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'cellNumber' ), cell );
          end;
    end;
  //WriteLn( P3DShaderActive.DumpUniforms );
  P3DApplication.MainWindow.Title:= 'FPS: ' + FloatToStr( FPS ) + ' ' + CamPos.ToString() + ' <' + CamYawPitchRoll.ToString() + '>';
  mainscene.Render;

  glDisable( GL_DEPTH_TEST );

  P3DGUIManager.Render;
  Inc( FramesCounter );
  if ( FPSLastPoll + FPSUpdate <= SDL_GetTicks ) then
    begin
      FPS:= FramesCounter / ( SDL_GetTicks - FPSLastPoll ) * 1000;
      FramesCounter:= 0;
      FPSLastPoll:= SDL_GetTicks;
    end;
end;


procedure ResizeWnd( Sender: TP3DWindow );
begin

end;

{$ENDIF}
