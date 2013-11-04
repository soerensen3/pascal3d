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
  zgl_render, zgl_types,
  LCLIntf,
  dglOpenGL,
  sysutils,
  Interfaces,
  tiles,
  resources,
  vars,
  shaders,
  Math,
  Math3D,
  wavefront,
  mechanics, //empty
  fft,
  ocean,
  terrain
//  model,
//  Tiles3D

//  GL,
//  GLU
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;


procedure LoadShader;
begin
  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) +  '/../../../shaders' );
  watershader:= CreateVertexAndFragmentShader( LoadShaderToText( 'water.vert' ), LoadShaderToText( 'water.frag' ));
  pplight:= CreateVertexAndFragmentShader( LoadShaderToText( 'pplight.vert' ), LoadShaderToText( 'pplight.frag' ));
  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' ));
  oceanshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'ocean.vert' ), LoadShaderToText( 'ocean.frag' ));
  shadersky:= CreateVertexAndFragmentShader( LoadShaderToText( 'skybox.vert' ), LoadShaderToText( 'skybox.frag' ));
  terrainshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'terrain.vert' ), LoadShaderToText( 'terrain.frag' ));
  if ( Assigned( oceanobj )) then
    oceanobj.updateShader( oceanshader );
  if ( Assigned( terrainobj )) then
    terrainobj.updateShader( terrainshader );
end;

procedure UnloadShader;
begin
  DeleteShader( watershader );
  DeleteShader( pplight );
  DeleteShader( simpleshader );
  DeleteShader( oceanshader );
  DeleteShader( terrainshader );
  DeleteShader( shadersky );
end;

procedure DeInit;
begin
  UnloadShader;
  oceanobj.Free;
  terrainobj.Free;
  TileSet.Free;
  TileMap.Free;
  mdl.Free;
end;

procedure Init;
var
  i: Integer;
  MapTile: TMapTile;
  Tile: TTile;

procedure LoadCubeMapFaces( tex: array of String);
var
  pData: PByteArray;
  i: Integer;
  ext: String;
  w, h,
  format: Word;
  dir: Integer;
begin
//  tex_GetData( cubemap, zgl_types.PByteArray( pData ));
  glEnable( GL_TEXTURE_CUBE_MAP );
  glGenTextures( 1, @sky[ 0 ]);
  glBindTexture( GL_TEXTURE_CUBE_MAP, sky[ 0 ]);

  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_CUBE_MAP, GL_TEXTURE_WRAP_R, GL_CLAMP_TO_EDGE);

  for dir:= 0 to 5 do
    begin
      ext:= Copy( ExtractFileExt( tex[ dir ]), 2, 255 );
      for i := managerTexture.Count.Formats - 1 downto 0 do
        if ( UpperCase( ext ) = managerTexture.Formats[ i ].Extension ) then
           managerTexture.Formats[ i ].FileLoader( tex[ dir ], zgl_types.PByteArray( pData ), w, h, format );

      glTexImage2D(GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT + dir,
        0,                  //level
        GL_RGBA,            //internal format
        w,                  //width
        w,                  //height
        0,                  //border
        GL_RGBA,             //format
        GL_UNSIGNED_BYTE,   //type
        pData ); // pixel data
    end;
end;

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
//  mdl:= LoadModelFromFile( 'volcanoe.obj' );
  water:= LoadModelFromFile( 'water.obj' );
  skymap:= tex_LoadFromFile( 'sky.png' );
  foammap:= tex_LoadFromFile( 'foam.png' );
  pointmap:= tex_LoadFromFile( 'pointsprites/point.png' );
  wavemap:= tex_LoadFromFile( 'wavemap.png' );
  terrainheight:= tex_LoadFromFile( 'island.png' );

  LoadCubeMapFaces([ 'skybox/right.png', 'skybox/left.png',
                     'skybox/up.png', 'skybox/down.png',
                     'skybox/back.png', 'skybox/front.png' ]);


  waveheight:= 50;
  waveampl:= 10;
  winddir:= Vector( 1, 1, 1 ).Normalize;

  terrainobj:= TTerrain.Create;
  terrainobj.LoadHeightMap( terrainheight, 15 );

  LoadShader;

  xrot:= 80;
  yrot:= 0;
  zoom:= 10;

  Point1:= Vector( 15, 5, 0 );
  Point2:= Vector( -10, 5, 10 );
  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
  oceanobj:= TOcean.Create( 64, 0.0005, Vector( 0, 8, 0 ), 128, oceanshader, False );
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
      glActiveTexture( GL_TEXTURE0 );
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
    waterscale: Extended;

    procedure RenderSky( matView, matWorld: TMatrix4f );
    var
      vert: GLuint;
    begin
      ShaderEnable( shadersky );
      ShaderSetParameter4fv( ActiveShader, 'world', MatMath3DToDGL( matWorld ));
      ShaderSetParameter4fv( ActiveShader, 'view', MatMath3DToDGL( matView ));
      ShaderSetParameter4fv( ActiveShader, 'proj', MatMath3DToDGL( proj ));

      ShaderSetParameter4fv( shadersky, 'ModelViewMatrix', MatMath3DToDGL( view3x3 ));

      glActiveTexture( GL_TEXTURE0 );
      glEnable(GL_TEXTURE_CUBE_MAP);

      glColor3f( 1.0, 1.0, 1.0 );

      glBindTexture( GL_TEXTURE_CUBE_MAP, sky[ 0 ]);

      glDisable(GL_DEPTH_TEST);
      glDisable(GL_LIGHTING);
      vert:= glGetAttribLocation( shadersky, 'vertex' );

      glBegin(GL_QUADS);
          glVertexAttrib3f( vert, -1.0, -1.0, 0.0 );
          glVertexAttrib3f( vert, 1.0, -1.0, 0.0 );
          glVertexAttrib3f( vert, 1.0,  1.0, 0.0 );
          glVertexAttrib3f( vert, -1.0,  1.0, 0.0 );
      glEnd();

      glDisable( GL_TEXTURE_CUBE_MAP );
      glEnable( GL_TEXTURE_2D );

      ShaderDisable;
    end;

    procedure RenderWater( matView, matWorld: TMatrix4f );
    var
      matInv: TMatrix4f;
    begin
      glActiveTexture( GL_TEXTURE1 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, skymap^.ID );
      glActiveTexture( GL_TEXTURE2 );
      glEnable( GL_TEXTURE_2D );
      glBindTexture( GL_TEXTURE_2D, foammap^.ID );
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
      ShaderEnable( watershader );
      ShaderSetParameterf( watershader, 'worldTime', GetTickCount/1000 );

//      glGetFloatv(GL_MODELVIEW_MATRIX, @WorldMatrix);
//      Mat:= MatDGLtoMath3D( WorldMatrix );
      wnd_SetCaption( Format( 'CamPos = <%f,%f,%f> SunDir = <%f,%f,%f>', [CamPos.X, CamPos.Y, CamPos.Z, SunDir.X, SunDir.Y, SunDir.Z ]));
      ShaderSetParameteri( watershader, 'skymap', 1);
      ShaderSetParameteri( watershader, 'foammap', 2);

  //    glUniform1iARB(glSlang_GetUniLoc(ProgramObject, 'skymap'), 1);
      ShaderSetParameter3f( watershader, 'cameraPosition', Cam.P.x, Cam.P.y, Cam.P.z );
      ShaderSetParameter4fv( ActiveShader, 'world', MatMath3DToDGL( matWorld ));
      ShaderSetParameter4fv( ActiveShader, 'view', MatMath3DToDGL( matView ));
      ShaderSetParameter4fv( ActiveShader, 'proj', MatMath3DToDGL( proj ));

//      ShaderSetParameter3f( watershader, 'cameraPosition', CamPos.x, CamPos.y, CamPos.z );
      ShaderSetParameter3f( ActiveShader, 'sunDir', SunDir.x, SunDir.y, SunDir.z );
      ShaderSetParameter3f( ActiveShader, 'windDir', winddir.x, winddir.y, winddir.z );
      ShaderSetParameterf( ActiveShader, 'waveHeight', waveheight );
      ShaderSetParameterf( ActiveShader, 'waveAmpl', waveampl );

      water.Draw;
//      DrawTile3D( water, 0, 0, 0, 0, 100 );
      ShaderDisable;
      glActiveTexture( GL_TEXTURE1 );
      glDisable( GL_TEXTURE_2D );
      glActiveTexture( GL_TEXTURE2 );
      glDisable( GL_TEXTURE_2D );

      ShaderDisable;

      glActiveTexture( GL_TEXTURE1 );
      glDisable( GL_TEXTURE_2D );
      glActiveTexture( GL_TEXTURE0 );
      glEnable( GL_TEXTURE_2D );
    end;

    procedure RenderOcean( matView, matWorld: TMatrix4f );
    begin
      glActiveTexture( GL_TEXTURE0 );
      glEnable( GL_TEXTURE_2D );
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
      glBindTexture( GL_TEXTURE_2D, wavemap^.ID );

      glActiveTexture( GL_TEXTURE1 );
      glEnable( GL_TEXTURE_2D );
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
      glBindTexture( GL_TEXTURE_2D, foammap^.ID );

      glActiveTexture( GL_TEXTURE2 );
      glEnable(GL_TEXTURE_CUBE_MAP);

      glColor3f( 1.0, 1.0, 1.0 );

      glBindTexture( GL_TEXTURE_CUBE_MAP, sky[ 0 ]);

//      glDisable( GL_TEXTURE_2D );
//      ShaderEnable( simpleshader );
      ShaderEnable( oceanshader );
      ShaderSetParameterf( oceanshader, 'worldTime', GetTickCount/1000 );
      ShaderSetParameteri( oceanshader, 'watermap', 0 );
      ShaderSetParameteri( oceanshader, 'foammap', 1 );
      ShaderSetParameteri( oceanshader, 'skymap', 2 );

      world:= MatrixIdentity;

{      ShaderSetParameter3f( ActiveShader, 'lightdir', SunDir.x, SunDir.y, SunDir.z );
      ShaderSetParameter4fv( ActiveShader, 'world', MatMath3DToDGL( world ));
      ShaderSetParameter4fv( ActiveShader, 'view', MatMath3DToDGL( {MatrixTranspose(} view ));
      ShaderSetParameter4fv( ActiveShader, 'proj', MatMath3DToDGL( {MatrixTranspose(} proj ));

      ShaderSetParameter3f( pplight, 'lightdiff', 1, 1, 1 );}

      oceanobj.render( GetTickCount/1000, SunDir, proj, view, MatrixIdentity, CamPos );

      ShaderDisable;
    end;

    procedure RenderTerrain( matView, matWorld: TMatrix4f );
    begin
      glActiveTexture( GL_TEXTURE0 );
      glEnable( GL_TEXTURE_2D );
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
      glBindTexture( GL_TEXTURE_2D, terrainheight^.ID );

      glColor3f( 1.0, 1.0, 1.0 );

      ShaderEnable( terrainshader );
      ShaderSetParameteri( oceanshader, 'surface', 0 );

      world:= MatrixIdentity;

      terrainobj.render( proj, matView, matWorld );

      ShaderDisable;
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

      DrawTile3D( mdl, 0, 0.0, 0, 0, 500 );
      ShaderDisable;
    end;

    procedure SetupMatrices;
    var
      zglProj: dglOpenGL.TMatrix4f;
    begin
      world:= MatrixIdentity;

      view:= MatrixIdentity;
//      view*= MatrixRot( Vector( 0.0, 1.0, 0.0 ), deg2rad* yrot );
//      view*= MatrixRot( Vector( 1.0, 0.0, 0.0 ), deg2rad* xrot );
      view*= MatrixRot( Vector( 0.0, 1.0, 0.0 ), deg2rad* -Cam.R.y );
      view*= MatrixRot( Vector( 1.0, 0.0, 0.0 ), deg2rad* -Cam.R.x );
      view3x3:= view;
      view:= MatrixTranslate( Cam.P ) * view;//Vector( 0, 0, Zoom ));

      SunDir:= Vector(-1000,100,-1000);//Vector( 0, 1, 0 ) * MatrixRot( Vector( 0, 0, 1 ), sin( GetTickCount / 3000 ) * PI / 2);

//      glGetFloatv( GL_PROJECTION_MATRIX, @zglProj );
      proj:= //MatDGLtoMath3D( zglProj );
            MatrixPerspectiveFOVLH( deg2rad* 90, 4/3, 0.5, 10000 );
      glMatrixMode( GL_PROJECTION );
//      glPushMatrix;
      zglProj:= MatMath3DToDGL( proj );
      glLoadMatrixf( @zglProj );
      glMatrixMode( GL_MODELVIEW );
      zglProj:= MatMath3DToDGL( view3x3 );
      glLoadMatrixf( @zglProj );
      CamPos:= Cam.P; //Vector( 0, 0, Zoom ) * view * world;
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
    glEnable( GL_BLEND );
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );
    RenderTerrain( view, MatrixTranslate( Vector( 0, -2, 0 )) * MatrixScale( Vector( 30, 30, 30 )));
    oceanobj.wind:= Vector( cos( windangle )* windstrength, sin( windangle ) * windstrength, 0 );
    waterscale:= CamPos.Y / 4;
    RenderWater( view3x3, MatrixScale( Vector( 100 * waterscale, 1, 100 * waterscale )) * MatrixTranslate( Vector( 0, CamPos.Y, 0 )));// * sqr( CamPos.Y )));
    RenderOcean( view, world );//MatrixScale( Vector( 0.1, 0.1, 0.1 )));
    //RenderIsland( view, world );
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
    //DrawPoint( Point1, Vector( 1, 0, 0 ));
//    DrawPoint( Point2, Vector( 0, 1, 0 ));
//    DrawPoint( SunDir, Vector( 0, 1, 1 ));
        glActiveTexture( GL_TEXTURE0 );

//  Check Textures
//    Set2DMode;
//    DrawTex( mdl.Materials[ 1 ].Diff_Map^.ID, 0, 0, mdl.Materials[ 1 ].Diff_Map^.Width / 2, mdl.Materials[ 1 ].Diff_Map^.Height / 2 );
  end;
var
  cl: Cardinal;

begin
  Draw3D;
  Set2DMode;

  glEnable( GL_TEXTURE_2D );
  cl:= $FFFF0000;
  text_DrawEx( fntMain, 0, 0, 0.5, 0.0, Format( 'Choppiness = %G', [ oceanobj.Choppiness ]), 200 + 55 * Ord( varSel = 0 ));
  text_DrawEx( fntMain, 0, 40, 0.5, 0.0, Format( 'Foam Amount = %G', [ oceanobj.FoamAmount ]), 200 + 55 * Ord( varSel = 1 ));
  text_DrawEx( fntMain, 0, 80, 0.5, 0.0, Format( 'Foam Factor = %G', [ oceanobj.FoamFactor ]), 200 + 55 * Ord( varSel = 2 ));
  text_DrawEx( fntMain, 0, 120, 0.5, 0.0, Format( 'Foam Threshhold = %G', [ oceanobj.FoamThreshh ]), 200 + 55 * Ord( varSel = 3 ));
  text_DrawEx( fntMain, 0, 160, 0.5, 0.0, Format( 'Wind Angle = %d', [ windangle ]), 200 + 55 * Ord( varSel = 4 ));
  text_DrawEx( fntMain, 0, 200, 0.5, 0.0, Format( 'Wind Strength = %G', [ windstrength ]), 200 + 55 * Ord( varSel = 5 ));
end;

procedure Timer;
const speed = 1;

begin
  if lineAlpha > 5 Then
    DEC( lineAlpha, 10 )
  else
    lineAlpha := 255;

  // RU: Проверить нажата ли левая кнопка мыши в пределах inputRect и начать отслеживать ввод текста.
  // EN: Check if left mouse button was pressed inside inputRect and start to track text input.
{  if ( mouse_Down( M_BLEFT )) then
    begin
      xrot:= min( 90, max( 0, xrot - Round(( my - mouse_Y )) / 5 ));
      yrot:= yrot - Round(( mx - mouse_X )) / 5;
//      offx:= offx - Round(( mx - mouse_X ));
//      offy:= offy - Round(( my - mouse_Y ));
    end;}
  Cam.R.X:= min( 90, max( -90, Cam.R.X + Round(( my - mouse_Y )) / 2 ));
  Cam.R.Y:= Cam.R.Y + Round(( mx - mouse_X )) / 2;
  mx:= mouse_X;
  my:= mouse_Y;
{  if ( mouseWheel[ M_WUP ] or keysDown[ K_UP ]) then
    Zoom:= Min( 5000, Zoom + 80.2 );
  if ( mouseWheel[ M_WDOWN ]  or keysDown[ K_DOWN ]) then
    Zoom:= Max( 0.2, Zoom - 80.2 );}

  if ( keysDown[ K_A ]) then
    Cam.P+= Vector( view._00, view._10, view._20 ) * speed;
  if ( keysDown[ K_D ]) then
    Cam.P+= Vector( view._00, view._10, view._20 ) * ( -speed );
  if ( keysDown[ K_S ]) then
    Cam.P+= Vector( view._02, view._12, view._22 ) * speed;
  if ( keysDown[ K_W ]) then
    Cam.P+= Vector( view._02, view._12, view._22 ) * ( - speed );
  if ( keysDown[ K_SPACE ]) then
    Cam.P+= Vector( view._01, view._11, view._21 ) * ( -speed );
  if ( keysDown[ K_SHIFT ]) then
    Cam.P+= Vector( view._01, view._11, view._21 ) * speed;

  if ( key_Press( K_SPACE )) then
    oceanobj.Paused:= not oceanobj.Paused;

  if ( key_Press( K_PAGEUP )) then
    Dec( varSel );
  if ( key_Press( K_PAGEDOWN )) then
    Inc( varSel );

  if ( varSel > 5 ) then
    varSel:= 0;
  if ( varSel < 0 ) then
    varSel:= 5;
  if ( key_Press( K_KP_ADD )) then
    case ( varSel ) of
      0: oceanobj.Choppiness:= oceanobj.Choppiness + 0.1;
      1: oceanobj.FoamAmount:= oceanobj.FoamAmount + 0.0001;
      2: oceanobj.FoamFactor:= oceanobj.FoamFactor + 0.001;
      3: oceanobj.FoamThreshh:= oceanobj.FoamThreshh + 0.1;
      4: windangle:= windangle + 5;
      5: windstrength:= windstrength + 0.2;
    end;
  if ( key_Press( K_KP_SUB )) then
    case ( varSel ) of
      0: oceanobj.Choppiness:= oceanobj.Choppiness - 0.1;
      1: oceanobj.FoamAmount:= oceanobj.FoamAmount - 0.0001;
      2: oceanobj.FoamFactor:= oceanobj.FoamFactor - 0.001;
      3: oceanobj.FoamThreshh:= oceanobj.FoamThreshh - 0.1;
      4: windangle:= windangle - 5;
      5: windstrength:= windstrength - 0.2;
    end;

  if ( windangle >= 360 ) then
    windangle:= windangle mod 360;
  if ( windangle < 0 ) then
     windangle:= windangle mod 360;

  if ( key_Press( K_F5 )) then
    begin
      UnloadShader;
      LoadShader;
    end;

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
  zgl_Reg( SYS_EXIT, @DeInit );

  wnd_SetCaption( '03 - Input' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
