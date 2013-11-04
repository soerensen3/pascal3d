program demo03;

{$APPTYPE CONSOLE}

{$DEFINE USE_ZENGL_STATIC}

{$IFDEF WINDOWS}
  {$R *.res}
{$ENDIF}

{.$DEFINE DEBUG_DEFERRED}

uses
  {$IFDEF USE_ZENGL_STATIC}
  zgl_main,
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
  zgl_render,
  LCLIntf,
  zgl_opengl,
  zgl_opengl_all, pl_opengl,
  dglOpenGL,
  sysutils,
  Interfaces,
  tiles, resources, vars, shaders,
  Math,
  Math3D,
  wavefront, mechanics
//  model,
//  Tiles3D

//  GL,
//  GLU
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;


procedure Init;
var
  i: Integer;
  MapTile: TMapTile;
  Tile: TTile;

begin
  InitOpenGL();
  ReadExtensions;

  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) + '/../../../fonts' );
  fntMain := font_LoadFromFile( 'Calibri-Regular-36pt.zfi' );

  inputRect.X := 400 - 192;
  inputRect.Y := 300 - 100 - 32;
  inputRect.W := 384;
  inputRect.H := 96;

  // RU: Инициализируем обработку ввода джойстиков и получаем количество подключенных джойстиков.
  // EN: Initialize processing joystick input and get count of plugged joysticks.
  joyCount := joy_Init();

  zoom:= 1.0;
  pos.x:= 2;//400;
  pos.y:= 2;//-200;
  speed:= 4;

{  Randomize;
  for i:= 0 to 99 do
    begin
      MapTile.Index:= Random( 3 );
      TileMap.MapArray[ i ]:= MapTile;
    end;}
  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) + '/../../../model' );
//  mdl:= TModel.Create
  mdl:= LoadModelFromFile( 'volcanoe.obj' );
  water:= LoadModelFromFile( 'water.obj' );
  skymap:= tex_LoadFromFile( 'sky.png' );
  pointmap:= tex_LoadFromFile( 'pointsprites/point.png' );

  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) +  '/../../../shaders' );
  watershader:= CreateVertexAndFragmentShader( LoadShaderToText( 'water.vert' ), LoadShaderToText( 'water.frag' ));
  pplight:= CreateVertexAndFragmentShader( LoadShaderToText( 'pplight.vert' ), LoadShaderToText( 'pplight.frag' ));
  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' ));

  xrot:= 80;
  yrot:= 0;
  zoom:= 10;

  Point1:= Vector( 15, 5, 0 );
  Point2:= Vector( -10, 5, 10 );
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
end;

procedure Draw;
  procedure Draw3D;
  var
    WorldMatrix: dglOpenGL.TMatrix4f;
    CamPos: TVector;
    Mat: TMatrix4f;


    procedure DrawTex( Idx: GLuint; x1, y1, x2, y2: Real; const color: TVector );
    begin
      glActiveTexture( GL_TEXTURE0 );
      glBindTexture( GL_TEXTURE_2D, Idx );
      glColor3f(Color.x,Color.y,Color.z);
      glBegin(GL_QUADS);
      glTexCoord2f( 0, 0 );
      glVertex3f( x1, y2, 0.0);

      glTexCoord2f( 0, 1 );
      glVertex3f( x1, y1, 0.0);

      glTexCoord2f( 1, 1 );
      glVertex3f( x2, y1, 0.0);

      glTexCoord2f( 1, 0 );
      glVertex3f( x2, y2, 0.0);
      glEnd();
    end;



    function MatDGLtoMath3D( MatIn: dglOpenGL.TMatrix4f ): TMatrix4f;
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

{.$DEFINE MATTRAVERSE}
    function MatMath3DToDGL( MatIn: TMatrix4f ): dglOpenGL.TMatrix4f;
    begin
{$IFNDEF MATTRAVERSE}
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

      {$ELSE}
      Result[ 0, 0 ]:= MatIn._00;
      Result[ 1, 0 ]:= MatIn._01;
      Result[ 2, 0 ]:= MatIn._02;
      Result[ 3, 0 ]:= MatIn._03;

      Result[ 0, 1 ]:= MatIn._10;
      Result[ 1, 1 ]:= MatIn._11;
      Result[ 2, 1 ]:= MatIn._12;
      Result[ 3, 1 ]:= MatIn._13;

      Result[ 0, 2 ]:= MatIn._20;
      Result[ 1, 2 ]:= MatIn._21;
      Result[ 2, 2 ]:= MatIn._22;
      Result[ 3, 2 ]:= MatIn._23;

      Result[ 0, 3 ]:= MatIn._30;
      Result[ 1, 3 ]:= MatIn._31;
      Result[ 2, 3 ]:= MatIn._32;
      Result[ 3, 3 ]:= MatIn._33;
      {$ENDIF}
    end;

    procedure DrawTile3D( Mdl: TModel; X, Y, Z, Angle: Real; const S: Real = 1.0 );
    var
      _world, normalmatrix: TMatrix4f;
    begin
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix();

//      glLoadIdentity();

      glTranslatef(X,Z,Y);
      glRotated( Angle, 0, 1.0, 0.0 );
      glScalef( S, S, S );

//      _world:= MatDGLtoMath3D( WorldMatrix );
      _world:= world;
      _world*= MatrixScale( Vector( S, S, S ));
      _world*= MatrixRot( Vector( 0, 1, 0 ), Angle);
      _world*= MatrixTranslate( Vector( X, Y, Z ));

      ShaderSetParameter3f( ActiveShader, 'lightdir', SunDir.x, SunDir.y, SunDir.z );
      ShaderSetParameter4fv( ActiveShader, 'world', MatMath3DToDGL( _world ));
      ShaderSetParameter4fv( ActiveShader, 'view', MatMath3DToDGL( {MatrixTranspose(} view ));
      ShaderSetParameter4fv( ActiveShader, 'proj', MatMath3DToDGL( {MatrixTranspose(} proj ));
//      if ( MatrixInverse( _world* view, normalmatrix )) then
//        ShaderSetParameter4fv( ActiveShader, 'normalmatrix', MatMath3DToDGL( MatrixTranspose( normalmatrix )));


//      glGetFloatv(GL_MODELVIEW_MATRIX, @worldMatrix);
//      ShaderSetParameter4fv( Shad_lightning2, 'WorldMatrix', WorldMatrix );

      Mdl.Draw;

      glPopMatrix();
    end;
    procedure DrawPoint( P: TVector; Color: TVector; const rad: Single = 5.0 );
    var
      n: TVector;
      _p: TVector;
      _ph: Extended;
    begin
      ShaderEnable( simpleshader );
      glBindTexture( GL_TEXTURE_2D, pointmap^.ID );
      glEnable( GL_POINT_SPRITE );
      glTexEnvi( GL_POINT_SPRITE, GL_COORD_REPLACE, GL_TRUE );
      glPointSize(32.0);

      glBegin(GL_POINTS);
        glColor3f( Color.x, Color.y, Color.z );
        glVertex3f( P.x, P.y, P.z );
      glEnd();

      glDisable( GL_POINT_SPRITE );
      ShaderDisable;
{      n:= -P;
      n.Normalize;

      _p:= P*world*view*proj;
      _ph:= ( _p.x * proj._03 ) + ( _p.y * proj._13 ) + ( _p.z * proj._23 ) + proj._33;
      _p/= _ph;

      _p.x:= (((_p.x + 1.0) * 0.5) * 800) + 0;
      _p.y:= (((1.0 - _p.y) * 0.5) * 600) + 0;
      _p.z:= 0;//(vProjected.Z * (pMaxZ - pMinZ)) + pMinZ;

      if ( n.DotProduct( CamPos ) > 0 ) then
        DrawTex( 0, _p.x - rad, _p.y - rad, _p.x + rad, _p.y + rad, Color );}
    end;

  var
    XRes, YRes: Integer;
    light_diffuse: TVector;
    light_ambient: TVector;
    VecZero: TVector;
    VecOne: TVector;
    v1,v2,v3 : array [0..3] of Real;

    procedure RenderSky( matView, matWorld: TMatrix4f );
    begin
      ShaderEnable( simpleshader );
      ShaderSetParameter4fv( ActiveShader, 'world', MatMath3DToDGL( matWorld ));
      ShaderSetParameter4fv( ActiveShader, 'view', MatMath3DToDGL( matView ));

      glActiveTexture( GL_TEXTURE0 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, skymap^.ID );

      glColor3f( 1.0, 1.0, 1.0 );
      glBegin( GL_QUADS );
      glTexCoord1f( 0 );
      glVertex3f( -1, 1, -1 );
      glTexCoord1f( 0 );
      glVertex3f( 1, 1, -1 );
      glTexCoord1f( 1 );
      glVertex3f( 1, -0.1, -1 );
      glTexCoord1f( 1 );
      glVertex3f( -1, -0.1, -1 );

      glTexCoord1f( 0 );
      glVertex3f( -1, 1, 1 );
      glTexCoord1f( 0 );
      glVertex3f( 1, 1, 1 );
      glTexCoord1f( 1 );
      glVertex3f( 1, -0.1, 1 );
      glTexCoord1f( 1 );
      glVertex3f( -1, -0.1, 1 );

      glTexCoord1f( 0 );
      glVertex3f( -1, 1, -1 );
      glTexCoord1f( 0 );
      glVertex3f( -1, 1, 1 );
      glTexCoord1f( 1 );
      glVertex3f( -1, -0.1, 1 );
      glTexCoord1f( 1 );
      glVertex3f( -1, -0.1, -1 );

      glTexCoord1f( 0 );
      glVertex3f( 1, 1, -1 );
      glTexCoord1f( 0 );
      glVertex3f( 1, 1, 1 );
      glTexCoord1f( 1 );
      glVertex3f( 1, -0.1, 1 );
      glTexCoord1f( 1 );
      glVertex3f( 1, -0.1, -1 );

      glTexCoord1f( 0 );
      glVertex3f( 1, 1, -1 );
      glTexCoord1f( 0 );
      glVertex3f( 1, 1, 1 );
      glTexCoord1f( 0 );
      glVertex3f( -1, 1, 1 );
      glTexCoord1f( 0 );
      glVertex3f( -1, 1, -1 );

      glTexCoord1f( 1 );
      glVertex3f( 1,  -0.1, 1 );
      glVertex3f( 1,  -0.1, -1 );
      glVertex3f( -1, -0.1, -1 );
      glVertex3f( -1, -0.1, 1 );
      glEnd;

      ShaderDisable;
    end;

    procedure RenderWater( matView, matWorld: TMatrix4f );
    var
      matInv: TMatrix4f;
    begin
      glActiveTexture( GL_TEXTURE1 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, skymap^.ID );
      ShaderEnable( watershader );
      ShaderSetParameterf( watershader, 'worldTime', GetTickCount/2000 );

//      glGetFloatv(GL_MODELVIEW_MATRIX, @WorldMatrix);
//      Mat:= MatDGLtoMath3D( WorldMatrix );
      wnd_SetCaption( Format( 'CamPos = <%f,%f,%f> SunDir = <%f,%f,%f>', [CamPos.X, CamPos.Y, CamPos.Z, SunDir.X, SunDir.Y, SunDir.Z ]));
      ShaderSetParameteri( watershader, 'skymap', 1);
  //    glUniform1iARB(glSlang_GetUniLoc(ProgramObject, 'skymap'), 1);
//      ShaderSetParameter3f( watershader, 'cameraPosition', Cam.P.x, Cam.P.y, Cam.P.z );
      ShaderSetParameter3f( watershader, 'cameraPosition', CamPos.x, CamPos.y, CamPos.z );
      ShaderSetParameter3f( ActiveShader, 'sunDir', SunDir.x, SunDir.y, SunDir.z );

      DrawTile3D( water, 0, 0, 0, 0, 1000 );
      ShaderDisable;
      glActiveTexture( GL_TEXTURE1 );
      glDisable( GL_TEXTURE_2D );

      ShaderDisable;

      glActiveTexture( GL_TEXTURE1 );
      glDisable( GL_TEXTURE_2D );
      glActiveTexture( GL_TEXTURE0 );
      glEnable( GL_TEXTURE_2D );
    end;

    procedure RenderIsland( matView, matWorld: TMatrix4f );
    begin
      VecZero:= Vector( 0, 0, 0 );
      VecOne:= Vector( 1, 1, 1 );
      v1[ 0 ]:= SunDir.x;
      v1[ 1 ]:= SunDir.y;
      v1[ 2 ]:= SunDir.z;

      v2[ 0 ]:= 0;
      v2[ 1 ]:= 0;
      v2[ 2 ]:= 0;

      v3[ 0 ]:= 1;
      v3[ 1 ]:= 1;
      v3[ 2 ]:= 1;

      glMaterialfv(GL_FRONT, GL_SPECULAR,  @v2[ 0 ]);
      glMaterialfv(GL_FRONT, GL_SHININESS, @v2[ 0 ]);
      glMaterialfv(GL_FRONT, GL_AMBIENT,   @v2[ 0 ]);
      glMaterialfv(GL_FRONT, GL_DIFFUSE,   @v3[ 0 ]);

      light_ambient:= Vector( 0.5, 0.5, 0.5 );
      light_diffuse:= Vector( 0.8, 1, 1 );
      glLightfv(GL_LIGHT0, GL_AMBIENT,  @v3[ 0 ]);
      glLightfv(GL_LIGHT0, GL_DIFFUSE,  @v3[ 0 ]);

      glLightfv(GL_LIGHT0, GL_POSITION, @v1[ 0 ]);
  {    glMaterialfv(GL_FRONT, GL_SPECULAR,  VecZero.Ptr );
      glMaterialfv(GL_FRONT, GL_SHININESS, VecZero.Ptr );
      glMaterialfv(GL_FRONT, GL_AMBIENT,   VecZero.Ptr );
      glMaterialfv(GL_FRONT, GL_DIFFUSE,   VecOne.Ptr );

      light_ambient:= Vector( 0.5, 0.5, 0.5 );
      light_diffuse:= Vector( 0.8, 1, 1 );
      glLightfv(GL_LIGHT0, GL_AMBIENT,  light_ambient.Ptr);
      glLightfv(GL_LIGHT0, GL_DIFFUSE,  light_diffuse.Ptr);

      glLightfv(GL_LIGHT0, GL_POSITION, SunDir.Ptr);}
      glEnable( GL_LIGHT0 );

      ShaderEnable( pplight );

      world:= MatrixIdentity;
//      view:= MatrixTranslate( Cam.P );
//      view*= MatrixRot( Vector( 0.0, 1.0, 0.0 ), deg2rad* Cam.R.Y );
//      view*= MatrixRot( Vector( 1.0, 0.0, 0.0 ), deg2rad* Cam.R.X );
//      view*= MatrixRot( Vector( 0.0, 0.0, 1.0 ), deg2rad* Cam.R.Z );
{      view:= MatrixIdentity;
      view*= MatrixRot( Vector( 0.0, -1.0, 0.0 ), deg2rad* yrot );
      view*= MatrixRot( Vector( -1.0, 0.0, 0.0 ), deg2rad* xrot );
      view*= MatrixTranslate( Vector( 0, 0, -Zoom ));}
  //    MatrixInverse( view, view );
      ShaderSetParameter3f( pplight, 'lightdir', SunDir.x, SunDir.y, SunDir.z );
      ShaderSetParameter3f( pplight, 'lightdiff', 1, 1, 1 );

      DrawTile3D( mdl, 0, 0.0, 0, 0, 10 );
      ShaderDisable;
    end;

    procedure SetupMatrices;
    var
      zglProj: dglOpenGL.TMatrix4f;
    begin
      world:= MatrixIdentity;

      view:= MatrixIdentity;
      view*= MatrixRot( Vector( 0.0, 1.0, 0.0 ), deg2rad* yrot );
      view*= MatrixRot( Vector( 1.0, 0.0, 0.0 ), deg2rad* xrot );
      view3x3:= view;
      view*= MatrixTranslate( Vector( 0, 0, Zoom ));

      SunDir:= Vector(0,1,1).Normalize;//Vector( 0, 1, 0 ) * MatrixRot( Vector( 0, 0, 1 ), sin( GetTickCount / 3000 ) * PI / 2);

//      glGetFloatv( GL_PROJECTION_MATRIX, @zglProj );
      proj:= //MatDGLtoMath3D( zglProj );
            MatrixPerspectiveFOVLH( deg2rad* 90, 4/3, 0.01, 1000 );
      glMatrixMode( GL_PROJECTION );
//      glPushMatrix;
      zglProj:= MatMath3DToDGL( proj );
      glLoadMatrixf( @zglProj );
      glMatrixMode( GL_MODELVIEW );
      CamPos:= Vector( 0, 0, Zoom ) * view * world;
    end;

  begin
    glClearColor(0.0, 0.0, 0.0, 1.0);                      // Set the Background colour of or scene
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

    Set3DMode( 90 );

    SetupMatrices;

    glDisable(GL_DEPTH_TEST);

    RenderSky( view3x3, world );

    glEnable(GL_DEPTH_TEST);

//    glLoadIdentity;
//    glTranslatef(0, 0, -Zoom);
//    glRotatef(xrot,1.0,0.0,0.0);
//    glRotatef(yrot,0.0,1.0,0.0);

//    glScalef( Zoom, Zoom, Zoom );

//    glPushMatrix();
//    glLoadIdentity();

//    glColor4f(0.5,0.5,0.5,1.0);

//    TileMap.Draw;

    RenderIsland( view, world );
    RenderWater( view, world );
{    glActiveTexture( GL_TEXTURE0 );
    glEnable( GL_TEXTURE_2D );
    glBindTexture( GL_TEXTURE_1D, skymap^.ID );

    glColor3f( 0.0, 0.0, 1.0 );
    glBegin( GL_QUADS );
    glTexCoord1f( 0 );
    glVertex3f( 0, 0, 0 );
    glTexCoord1f( 0 );
    glVertex3f( 1, 0, 0 );
    glTexCoord1f( 1 );
    glVertex3f( 1, 1, 0 );
    glTexCoord1f( 1 );
    glVertex3f( 0, 1, 0 );
    glEnd;}
//    glPopMatrix();

//    glMatrixMode(GL_PROJECTION);
//    glPopMatrix();

    rtri := rtri + 0.1;
    rquad := rquad - 0.1;
//    Set2DMode;
    DrawPoint( Point1, Vector( 1, 0, 0 ));
    DrawPoint( Point2, Vector( 0, 1, 0 ));
    DrawPoint( SunDir, Vector( 0, 1, 1 ));

//  Check Textures
//    Set2DMode;
//    DrawTex( mdl.Materials[ 1 ].Diff_Map^.ID, 0, 0, mdl.Materials[ 1 ].Diff_Map^.Width / 2, mdl.Materials[ 1 ].Diff_Map^.Height / 2 );
  end;

begin
  Draw3D;
  Set2DMode;

end;

procedure Timer;
const speed = 0.1;

begin
  if lineAlpha > 5 Then
    DEC( lineAlpha, 10 )
  else
    lineAlpha := 255;

  // RU: Проверить нажата ли левая кнопка мыши в пределах inputRect и начать отслеживать ввод текста.
  // EN: Check if left mouse button was pressed inside inputRect and start to track text input.
  if ( mouse_Down( M_BLEFT )) then
    begin
      xrot:= min( 90, max( 0, xrot - Round(( my - mouse_Y )) / 5 ));
      yrot:= yrot - Round(( mx - mouse_X )) / 5;
//      offx:= offx - Round(( mx - mouse_X ));
//      offy:= offy - Round(( my - mouse_Y ));
    end;
{  Cam.R.X:= min( 90, max( -90, Cam.R.X + Round(( my - mouse_Y )) / 2 ));
  Cam.R.Y:= Cam.R.Y + Round(( mx - mouse_X )) / 2;}
  mx:= mouse_X;
  my:= mouse_Y;
  if ( mouseWheel[ M_WUP ]) then
    Zoom:= Min( 500, Zoom + 8.2 );
  if ( mouseWheel[ M_WDOWN ]) then
    Zoom:= Max( 0.2, Zoom - 8.2 );

  if ( keysDown[ K_A ]) then
    Cam.P+= Vector( view._00, view._10, view._20 ) * speed;
  if ( keysDown[ K_D ]) then
    Cam.P+= Vector( view._00, view._10, view._20 ) * ( -speed );
  if ( keysDown[ K_S ]) then
    Cam.P+= Vector( view._02, view._12, view._22 ) * ( -speed );
  if ( keysDown[ K_W ]) then
    Cam.P+= Vector( view._02, view._12, view._22 ) * speed;
  if ( keysDown[ K_SPACE ]) then
    Cam.P+= Vector( view._01, view._11, view._21 ) * speed;
  if ( keysDown[ K_SHIFT ]) then
    Cam.P+= Vector( view._01, view._11, view._21 ) * ( -speed );

  if key_Press( K_ESCAPE ) Then zgl_Exit();

  // RU: Обязательно очищаем состояния всех подсистем ввода.
  // EN: Necessarily clear all the states of input subsystems.
  mouse_ClearState();
  key_ClearState();
  joy_ClearState();
end;

Begin
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );

  wnd_SetCaption( '03 - Input' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
  TileSet.Free;
  TileMap.Free;
  mdl.Free;
End.
