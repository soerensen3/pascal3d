var
  mainscene: tScene;
  maincamera: tCamera;
  shadowscene: tScene;
  skyboxscene: tScene;

  pathshaders: String = '../../../shaders';
  pathmodels: String = '../../../models';
  pathfonts: String = '../../../fonts';

  WatchTime: Integer;

  Mdl: TP3DScene;
  terrain_collision: TP3DScene;
  Watch_Mdl: TFileWatch;
  lightbulb: TP3DScene;

  terrainshader: TShader;
  simpleshader: TShader;
  skyshader: TShader;
  Watch_simpleshader_V: TFileWatch;
  Watch_simpleshader_F: TFileWatch;
  shadowshader: TShader;
  renderFlags: TRenderFlags;

  ShadowRes: Integer;

  ShadowFB: TFrameBufferTex2D;

  LightSources: TLightList;

  LS: Integer;
  Lxrot1, Lyrot1, LZoom1: Single;
  Lxrot2, Lyrot2, LZoom2: Single;
  mrot: Integer;
  mshiftx: Integer;
  mshifty: Integer;


  skybox: TP3DScene;

  CatchMouse: Boolean = True;


function LoadShader: Boolean;
  procedure InitShader;
  begin
    LightSources.PassToActiveShader( vec3( 0 ), 8, Mat4Identity, [ liLightParams ]);
  end;
begin
  Result:= False;
  SetCurrentDir( pathshaders );

//  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'shader.vert' ), LoadShaderToText( 'shader.frag' ));
  terrainshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'landscape.vert' ), LoadShaderToText( 'landscape.frag' ));
  skyshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'skybox.vert' ), LoadShaderToText( 'skybox.frag' ));

//  shadowshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'parabol_shadow.vert' ), LoadShaderToText( 'parabol_shadow.frag' ));
  Watch_simpleshader_V:= TFileWatch.Create( pathshaders + '/landscape.vert' );
  Watch_simpleshader_F:= TFileWatch.Create( pathshaders + '/landscape.frag' );

  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' ));;
  if ( not Assigned( terrainshader )) then
    begin
      mainscene.Shader:= nil;
    end
  else
    begin
      terrainshader.Enable;
      InitShader;
      mainscene.Shader:= terrainshader;
    end;
  if ( Assigned( skyshader)) then
    begin
      skyboxscene.Shader:= skyshader;
    end;
    //  if ( not Assigned( shadowshader )) then
    //    exit;
    //  shadowshader.Enable;
    //  InitShader;
    //  shadowscene.Shader:= shadowshader;
end;

procedure UnloadShader;
begin
  skyshader.Free;
  simpleshader.Free;
  terrainshader.Free;
  Watch_simpleshader_F.Free;
  Watch_simpleshader_V.Free;
  shadowshader.Free;
end;

function LoadModels: Boolean;
var
  i: Integer;
begin
  Result:= False;
  SetCurrentDir( pathmodels );

  Mdl:= LoadModelFileFromFile( 'landscape.model' );
//  Mdl:= LoadModelFileFromFile( 'landscape_high.model' );
{  for i:= 0 to Mdl.Children.Count - 1 do
    if ( strutils.AnsiEndsStr( '_collision', Mdl.Children[ i ].Name )) then
      Mdl.Children[ i ].Visible:= False;}
  terrain_collision:= LoadModelFileFromFile( 'terrain_collision.model' );

  Watch_Mdl:= TFileWatch.Create( pathmodels + '/landscape.model' );

  lightbulb:= LoadModelFileFromFile( 'lightbulb.model' );

  skybox:= LoadModelFileFromFile( 'skybox.model' );
end;

procedure UnloadModels;
begin
  terrain_collision.Free;
  skybox.Free;
  Mdl.Free;
  lightbulb.Free;
  Watch_Mdl.Free;
end;

procedure LoadTextures;
begin

end;

procedure DeInit( Sender: TSDLWindow );
begin
  LightSources.Free;
  mainscene.Free;
  shadowscene.Free;
  maincamera.Free;
  ShadowFB.Free;

  UnloadShader;
  UnloadModels;
end;

procedure Init( Sender: TSDLWindow );
begin
  pathshaders:= ExtractFilePath( ParamStr( 0 )) + pathshaders;
  pathmodels:= ExtractFilePath( ParamStr( 0 )) + pathmodels;
  pathfonts:= ExtractFilePath( ParamStr( 0 )) + pathfonts;

  Sender.Width:= 800;
  Sender.Height:= 600;

  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) + '/../../../fonts' );
//  fntMain := font_LoadFromFile( 'Calibri-Regular-36pt.zfi' );
  mainscene:= tScene.Create;
  shadowscene:= tScene.Create;
  skyboxscene:= tScene.Create;
  maincamera:= tCamera.Create;
  maincamera.Position:= vec3( 4, -1.2+0.05, 10.5 );
  maincamera.YawPitchRoll.X:= 80;
  maincamera.far:= 1000;

  LightSources:= TLightList.Create;
  with ( LightSources[ LightSources.Add( TLight.Create )]) do
    begin
      Position:= vec3( 0 );
      Diffuse:= vec4( 1, 1, 1, 1 );
      Specular:= vec4( 1, 1, 1, 0.8 );
      LinearAttenuation:= 0.1;
    end;
  with ( LightSources[ LightSources.Add( TLight.Create )]) do
    begin
      Position:= vec3( 0 );
      Diffuse:= vec4( 1, 0.6, 0.3, 1 );
      Specular:= vec4( 1, 1, 1, 0.8 );
      LinearAttenuation:= 0.2;
    end;

  LoadModels;
  LoadTextures;
  LoadShader;

  mainscene.DrawObjects:= @DrawObjects;
  mainscene.Cam:= maincamera;

  shadowscene.DrawObjects:= @DrawShadowObjects;
  shadowscene.Cam:= maincamera;

  skyboxscene.DrawObjects:= @DrawSkyBoxObjects;
  skyboxscene.Cam:= maincamera;

  ShadowRes:= 4096;
  ShadowFB:= TFrameBufferTex2D.Create( 0, True, ShadowRes, ShadowRes, False );
  ShadowFB.BindFB();
  WriteLn( 'FB_STATUS: ', glCheckFramebufferStatus( GL_FRAMEBUFFER ));
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );

  SDL_SetRelativeMouseMode( SDL_TRUE );

  Lxrot1:= 1.22;
  Lyrot1:= -1.22;
  LZoom1:= 10;
  LightSources[ 0 ].Position:= vec3( vec4( 0, 0, Lzoom1, 1 ) * mat4rotate( vec3_Axis_PY, Lxrot1 ) * mat4rotate( vec3_Axis_PX, Lyrot1 ));
  Lxrot2:= 0.2;
  Lyrot2:= -1.5;
  LZoom2:= 10;
  LightSources[ 1 ].Position:= vec3( vec4( 0, 0, Lzoom2, 1 ) * mat4rotate( vec3_Axis_PY, Lxrot2 ) * mat4rotate( vec3_Axis_PX, Lyrot2 ));
  WatchTime:= GetTickCount;
end;
