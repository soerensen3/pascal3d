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
  Mdl:= LoadModelFileFromFile( 'spaceship_shark.model' );
//  Mdl.Children[ 0 ].testTex:= testTex^.ID;
//  WriteLn( Mdl.Debug );
end;

procedure UnloadModels;
begin
  Mdl.Free;
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

      CamPos:= vec3( Vec4( 0, 0, Zoom, 1 ) * view * world );
    end;

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
  mx:= cur.x;
  my:= cur.y;

  if ( mouse_Wheel( 0 )) then
    zoom:= zoom - 0.5;
  if ( mouse_Wheel( 1 )) then
    zoom:= zoom + 0.5;

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

  wnd_SetCaption( 'Baking Bread' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
