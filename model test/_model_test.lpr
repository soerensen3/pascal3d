program model_test;

{$APPTYPE CONSOLE}

{.$DEFINE USE_ZENGL_STATIC}

{-$IFDEF WINDOWS}
  {$R *.res}
{-$ENDIF}

{.$DEFINE DEBUG_DEFERRED}

uses
  {.$IFDEF USE_ZENGL_STATIC}
{  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_mouse,
  zgl_keyboard,
  zgl_joystick,
  zgl_textures,
  zgl_sprite_2d,
  zgl_primitives_2d,
  zgl_font,
  zgl_text,
  zgl_textures_png,
  zgl_textures_tga,
  zgl_textures_jpg,
  zgl_math_2d,
  zgl_collision_2d,
  zgl_utils,
  zgl_render, zgl_types,
  zgl_render_target,}
  zglHeader,
  LCLIntf,
  dglOpenGL,
  sysutils, Classes,
  Interfaces,
  shaders,
  Math,
  Math3D,
  Model,
  shadernodes,
  vars
  ;

procedure LoadShader;
var
  DefaultShader: TBaseShader;

  F: TStringList;
begin
  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) +  '/../../../shaders' );
{  DefaultShader:= TBaseShader.Create;
  simpleshader:= CreateVertexAndFragmentShader( DefaultShader.GetVertexHeader + DefaultShader.GetVertexCode,
                                                DefaultShader.GetFragmentHeader + DefaultShader.GetFragmentCode );

  F:= TStringList.Create;
  F.Text:= DefaultShader.GetVertexHeader + DefaultShader.GetVertexCode;
//  F.SaveToFile( 'shader.vert');
  F.Text:= DefaultShader.GetFragmentHeader + DefaultShader.GetFragmentCode;
//  F.SaveToFile( 'shader.frag');
  F.Free;}
  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'shader.vert' ), LoadShaderToText( 'shader.frag' ));
  if ( not Assigned( simpleshader )) then
    zgl_Exit;

  simpleshader.Enable;
  glUniform1i( simpleshader.Uniforms.AddrByName( 'numLightSource' ), 2 );

//  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[0].ambient' ), 0.1, 0.1, 0.1, 1 );
  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[0].ambient' ), 0, 0, 0, 1 );
  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[0].diffuse' ), 1, 1, 1, 1 );
//  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[0].specular' ), 0, 0, 0, 0 );
  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[0].specular' ), 1, 1, 1, 0.8 );
  glUniform1f( simpleshader.Uniforms.AddrByName( 'LightSource[0].linearAttenuation' ), 0.1 );
  glUniform1f( simpleshader.Uniforms.AddrByName( 'LightSource[0].constantAttenuation' ), 1 );


//  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[1].ambient' ), 0.0, 0.0, 0.0, 1 );
  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[1].diffuse' ), 1, 0.0, 0.0, 1 );
  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[1].specular' ), 1, 0.0, 0.0, 0.8 );
  glUniform1f( simpleshader.Uniforms.AddrByName( 'LightSource[1].constantAttenuation' ), 1 );
  simpleshader.Disable;

//  DefaultShader.Free;
end;

procedure UnloadShader;
begin
  simpleshader.Free;
end;

procedure LoadModels;
begin
  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) +  '/../../../models' );

//  testTex:= tex_LoadFromFile( 'Kaisersemmel.jpg' );
//  Mdl:= LoadModelFileFromFile( 'landscape.model' );
//  Mdl:= LoadModelFileFromFile( 'spaceship_shark.model' );
//  Mdl:= LoadModelFileFromFile( 'character.model' );
  Mdl:= LoadModelFileFromFile( 'asteroid_baked.model' );
  lightbulb:= LoadModelFileFromFile( 'lightbulb.model' );;
//  Mdl.Children[ 0 ].testTex:= testTex^.ID;
//  WriteLn( Mdl.Debug );
end;

procedure UnloadModels;
begin
  Mdl.Free;
  lightbulb.Free;
end;

procedure LoadTextures;
begin

end;

procedure DeInit;
begin
  UnloadShader;
  UnloadModels;
end;

procedure Init;
begin
  InitOpenGL();
  ReadExtensions;

  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) + '/../../../fonts' );
  fntMain := font_LoadFromFile( 'Calibri-Regular-36pt.zfi' );

  LoadModels;
  LoadTextures;
  LoadShader;

  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  xrot:= 80;
  yrot:= 0;
  zoom:= 10;
  Lxrot:=80;
  Lyrot:= 0;
  LZoom:= 10;
end;

function MatMath3DToDGL( MatIn: TMat4 ): dglOpenGL.TMatrix4f;
begin
  Result[ 0, 0 ]:= MatIn._00;
  Result[ 0, 1 ]:= MatIn._01;
  Result[ 0, 2 ]:= MatIn._02;
  Result[ 0, 3 ]:= MatIn._03;

  Result[ 1, 0 ]:= MatIn._10;
  Result[ 1, 1 ]:= MatIn._11;
  Result[ 1, 2 ]:= MatIn._12;
  Result[ 1, 3 ]:= MatIn._13;

  Result[ 2, 0 ]:= MatIn._20;
  Result[ 2, 1 ]:= MatIn._21;
  Result[ 2, 2 ]:= MatIn._22;
  Result[ 2, 3 ]:= MatIn._23;

  Result[ 3, 0 ]:= MatIn._30;
  Result[ 3, 1 ]:= MatIn._31;
  Result[ 3, 2 ]:= MatIn._32;
  Result[ 3, 3 ]:= MatIn._33;
end;

function MatDGLtoMath3D( MatIn: dglOpenGL.TMatrix4f ): TMat4;
begin
  Result._00:= MatIn[ 0, 0 ];
  Result._01:= MatIn[ 0, 1 ];
  Result._02:= MatIn[ 0, 2 ];
  Result._03:= MatIn[ 0, 3 ];

  Result._10:= MatIn[ 1, 0 ];
  Result._11:= MatIn[ 1, 1 ];
  Result._12:= MatIn[ 1, 2 ];
  Result._13:= MatIn[ 1, 3 ];

  Result._20:= MatIn[ 2, 0 ];
  Result._21:= MatIn[ 2, 1 ];
  Result._22:= MatIn[ 2, 2 ];
  Result._23:= MatIn[ 2, 3 ];

  Result._30:= MatIn[ 3, 0 ];
  Result._31:= MatIn[ 3, 1 ];
  Result._32:= MatIn[ 3, 2 ];
  Result._33:= MatIn[ 3, 3 ];
end;

procedure Draw;
    procedure SetupMatrices;
    var
      zglMat: dglOpenGL.TMatrix4f;
    begin
      world:= Mat4Identity;

      view:= Mat4Identity;
      view*= Mat4Rot( vec3_Axis_PZ, deg2rad* yrot );
      view*= Mat4Rot( vec3_Axis_PX, deg2rad* xrot );
//      view3x3:= view;
      view:= view * MatrixTranslate( vec4( 0, 0, -Zoom, 1 ));
//      MatrixInverse( view, invview );
      proj:= MatrixPerspectiveFOVLH( deg2rad* 90, 4/3, 0.1, 100 );

      glMatrixMode( GL_PROJECTION );
      glLoadIdentity;
      glLoadMatrixf( @proj );

      glMatrixMode( GL_MODELVIEW );
      glLoadMatrixf( @view );

      CamPos:= Vec4( 0, 0, Zoom, 1 ) * view * world;
    end;

var
  halfVec: TVec4;
  lightpos: TVec3;
  lvpos: TVec4;
  eyeVec: TVec3;
begin
  glClearColor(0.2, 0.2, 0.5, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  glMatrixMode( GL_PROJECTION );
  glLoadIdentity();
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity();
  glMatrixMode( GL_PROJECTION );
  glEnable( GL_DEPTH_TEST );
  //gluPerspective( 90, 4/3, 0.1, 100 );
  SetupMatrices;

  simpleshader.Enable;

  ShaderSetParameter4fv( simpleshader.ShaderObj, 'world', world );
  ShaderSetParameter4fv( simpleshader.ShaderObj, 'view', view );
  ShaderSetParameter4fv( simpleshader.ShaderObj, 'proj', proj );
  eyeVec:= vec3( 0, 0, -1 );//( -vec3( CamPos )).Normalize;
//  glUniform4f( simpleshader.Uniforms.AddrByName( 'eyePos' ), CamPos.x, CamPos.y, CamPos.z, 0 );
  glUniform4f( simpleshader.Uniforms.AddrByName( 'eyeVec' ), eyeVec.x, eyeVec.y, eyeVec.z, 0 );

  lightpos:= vec3( vec4( 0, 0, Lzoom, 1 ) * Mat4Rot( vec3_Axis_PX, Lxrot ) * Mat4Rot( vec3_Axis_PZ, Lyrot ));
  //5* vec3( sin( GetTickCount / 10000 ), cos( GetTickCount / 10000 ), 0 );
  lvpos:= vec4( lightpos, 1 ) * view * world;
//  lvpos:= vec4( 0 );//CamPos;
  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[0].position' ), lvpos.x, lvpos.y, lvpos.z, 1 );

//  halfVec:= ( vec4( eyeVec, 0 ) - lvpos ).Normalize();
//  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[0].halfVector' ), halfVec.x, halfVec.y, halfVec.z, halfVec.w );

  lightbulb.Render( MatrixTranslate( vec4( lightpos, 1 )), view, proj );

  lightpos:= 5 * vec3( sin( GetTickCount / 10000 + PI ), cos( GetTickCount / 10000 + PI ), 0 );
  lvpos:= vec4( lightpos, 1 ) * view * world;
  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[1].position' ), lvpos.x, lvpos.y, lvpos.z, 1 );

//  halfVec:= ( vec3( lvpos ) + VecNormalize( -CamPos )) / 2;
//  glUniform4f( simpleshader.Uniforms.AddrByName( 'LightSource[1].halfVector' ), halfVec.x, halfVec.y, halfVec.z, 0 );

  lightbulb.Render( MatrixTranslate( vec4( lightpos, 1 )), view, proj );

  {
  glBegin(GL_TRIANGLES);
    glColor3f(1, 0, 0); glVertex3f(-1,-1, sin( GetTickCount / 100 ) * 15 );
    glColor3f(0, 0, 1); glVertex3f( 1,-1, sin( GetTickCount / 100 ) * 15 );
    glColor3f(0, 1, 0); glVertex3f( 0, 1, sin( GetTickCount / 100 ) * 15 );
  glEnd;}


  Mdl.Render( world, view, proj );
  //Mdl2.Draw;

  ShaderDisable;

  Set2DMode;
end;

procedure Input;
var
  cur: TPoint;
begin
  GetCursorPos( cur );
  if ( mouse_Down( M_BLEFT )) then
    begin
      xrot:= min( 180, max( 0, xrot - Round(( my - cur.y )) / 5 ));
      yrot:= yrot - Round(( mx - cur.x )) / 5;
    end;
  if ( mouse_Down( M_BRIGHT )) then
    begin
      Lxrot:= Lxrot - Round( my - cur.y ) / 50;
      Lyrot:= Lyrot - Round( mx - cur.x ) / 50;
    end;
  mx:= cur.x;
  my:= cur.y;

  if ( key_Down( K_CTRL )) then
    begin
      if ( mouse_Wheel( 0 )) then
        Lzoom:= Lzoom - 0.5;
      if ( mouse_Wheel( 1 )) then
        Lzoom:= Lzoom + 0.5;
    end
  else
    begin
      if ( mouse_Wheel( 0 )) then
        zoom:= zoom - 0.5;
      if ( mouse_Wheel( 1 )) then
        zoom:= zoom + 0.5;
    end;

  if key_Press( K_F5 ) Then
    begin
      UnloadShader;
      LoadShader;
    end;
  if key_Press( K_ESCAPE ) Then zgl_Exit();
  mouse_ClearState;
  key_ClearState;
end;

Begin
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  timer_Add( @Input, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );
  zgl_Reg( SYS_EXIT, @DeInit );

  wnd_SetCaption( 'model test' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.