program baking_bread;

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
  zgl_render_target,
  LCLIntf,
  dglOpenGL,
  sysutils,
  Interfaces,
  vars,
  shaders,
  Math,
  Math3D,
  wavefront
  {$ELSE}
  zglHeader
  {$ENDIF}
  ;

function intersectRayTriangle( orig, dir, v0, v1, v2: TVector; out baryPosition: TVector ): Boolean;
var
  e1, e2: TVector;
  p: TVector;
  a: Float;
  f: Extended;
  s: TVector;
  q: TVector;
begin
  Result:= False;

  e1:= v1 - v0;
  e2:= v2 - v0;

  p:= VecCrossProduct( dir, e2 );

  a:= VecDotProduct( e1, p );

  if ( a < epsilon ) then
    exit;
  f:= 1 / a;

  s:= orig - v0;
  baryPosition.x:= f * VecDotProduct( s, p );
  if( baryPosition.x < 0 ) then
    exit;
  if( baryPosition.x > 1 ) then
    exit;

  q:= VecCrossProduct( s, e1 );
  baryPosition.y:= f * VecDotProduct( dir, q );

  if( baryPosition.y < 0.0 ) then
    exit;

  if( baryPosition.y + baryPosition.x > 1 ) then
    exit;

  baryPosition.z:= f * VecDotProduct( e2, q );

  Result:= baryPosition.z >= 0.0;
end;

function IntersectRayMdl( RayOrig, RayDir: TVector; Mdl: TModel; m: TMatrix4f; out BaryPos: TVector ): Boolean;
  function TestPoly( n: Integer ): Boolean;
  var
    i: Integer;
    v1: TVector;
    v0: TVector;
    v2: TVector;
  begin
    Result:= False;
    v0:= VecTransform( Mdl.Vertices[ Mdl.Faces[ n ].v[ 0 ] - 1 ], m );
    for i:= 1 to ( Length( Mdl.Faces[ n ].v ) - 2 ) do
      begin
        v1:= VecTransform( Mdl.Vertices[ Mdl.Faces[ n ].v[ i ] - 1], m );
        v2:= VecTransform( Mdl.Vertices[ Mdl.Faces[ n ].v[ i + 1 ] - 1 ], m );
        if ( intersectRayTriangle( RayOrig, RayDir, v0, v1, v2, BaryPos )) then
          begin
            Result:= True;
            break;
          end;
      end;
  end;

var
  i: Integer;
begin
  Result:= False;

  for i:= 0 to high( Mdl.Faces ) do
    if ( TestPoly( i )) then
      begin
        Result:= True;
        break;
      end;
end;

procedure LoadShader;
begin
  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) +  '/../../../shaders' );
  pplight:= CreateVertexAndFragmentShader( LoadShaderToText( 'pplight.vert' ), LoadShaderToText( 'pplight.frag' ));
  breadshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'bread.vert' ), LoadShaderToText( 'bread.frag' ));
  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' ));
  shadowshader_pass1:= CreateVertexAndFragmentShader( LoadShaderToText( 'shadow.vert' ), LoadShaderToText( 'shadow.frag' ));
end;

procedure UnloadShader;
begin
  DeleteShader( pplight );
  DeleteShader( simpleshader );
  DeleteShader( breadshader );
  DeleteShader( shadowshader_pass1 );
end;

procedure LoadModels;
begin
  bread:= LoadModelFromFile( 'bread.obj' );
  bread.CalcBoundingBox;

  roll:= LoadModelFromFile( 'roll.obj' );
  roll.CalcBoundingBox;

  kitchen:= LoadModelFromFile( 'kitchen.obj' );;
  kitchen.CalcBoundingBox;

  oven:= LoadModelFromFile( 'oven.obj' );
  oven.CalcBoundingBox;

  bowl:= LoadModelFromFile( 'bowl.obj' );
  bowl.CalcBoundingBox;

  dough:= LoadModelFromFile( 'dough.obj' );
  dough.CalcBoundingBox;
end;

procedure UnloadModels;
begin
  bread.Free;
  kitchen.Free;
  oven.Free;
  dough.Free;
  bowl.Free;
end;

procedure LoadTextures;
begin
  doughmap:= tex_LoadFromFile( 'dough.png' );

  titlemap:= tex_LoadFromFile( 'Baking Bread logo.png' );;
end;

procedure DeInit;
begin
  UnloadShader;
  UnloadModels;
end;

procedure Init;
var
  i: Integer;
  xres: Ptr;
  yres: Ptr;

begin
  InitOpenGL();
  ReadExtensions;

  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) + '/../../../fonts' );
  fntMain := font_LoadFromFile( 'Calibri-Regular-36pt.zfi' );

  joyCount := joy_Init();

  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) + '/../../../content' );

  LoadModels;
  LoadTextures;
  LoadShader;

  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  Cam.P:= Vector( 0, 5, 8 );
  Cam.R:= Vector( -40, 0, 0 );
  lightDir:= Vector( -5, 5, 0 );

{  SetLength( breads, 5 );
  for i:= 0 to High( breads ) do
    begin
      breads[ i ].P:= Vector(( i - Length( breads ) / 2 ) * 2 - 7, 0, 0 );
      breads[ i ].time:= 0;
    end;}

//  xres:= zgl_Get( VIEWPORT_WIDTH );
//  yres:= zgl_Get( VIEWPORT_HEIGHT );
  ShadowRT:= rtarget_Add( tex_CreateZero( 512, 512 ), RT_FULL_SCREEN {or RT_USE_DEPTH });
end;

function MatMath3DToDGL( MatIn: TMatrix4f ): dglOpenGL.TMatrix4f;
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

procedure Draw;
  procedure Draw3D;
  var
    WorldMatrix: dglOpenGL.TMatrix4f;
    CamPos: TVector;
    Mat: TMatrix4f;
    i: Integer;
    mdl: TModel;


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


    procedure draw_bbox( Mdl: TModel; Off: TVector );
    var
      vertices: array [0..8*3-1] of GLfloat = (
        -0.5, -0.5, -0.5,
         0.5, -0.5, -0.5,
         0.5,  0.5, -0.5,
        -0.5,  0.5, -0.5,
        -0.5, -0.5,  0.5,
         0.5, -0.5,  0.5,
         0.5,  0.5,  0.5,
        -0.5,  0.5,  0.5
      );
      vbo_vertices: GLuint;
      elements: array [0..4*4-1] of GLushort = (
        0, 1, 2, 3,
        4, 5, 6, 7,
        0, 4, 1, 5,
        2, 6, 3, 7
      );
      ibo_elements: GLuint ;
      size: TVector;
      transform: dglOpenGL.TMatrix4f;
      i: Integer;
    begin
      // Cube 1x1x1, centered on origin
{      glGenBuffers( 1, @vbo_vertices );
      glBindBuffer( GL_ARRAY_BUFFER, vbo_vertices );
      glBufferData( GL_ARRAY_BUFFER, sizeof( vertices ), @vertices, GL_STATIC_DRAW );
      glBindBuffer( GL_ARRAY_BUFFER, 0 );

      glGenBuffers( 1, @ibo_elements );
      glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, ibo_elements );
      glBufferData( GL_ELEMENT_ARRAY_BUFFER, sizeof( elements ), @elements, GL_STATIC_DRAW );
      glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, 0 );}

      with ( Mdl.BoundingBox ) do
        begin
          size:= Vector( max.x-min.x, max.y-min.y, max.z-min.z );
          transform:= MatMath3DToDGL( MatrixTranslate( ctr + Off ) * MatrixScale( size ));
        end;

      glMatrixMode( GL_MODELVIEW );

//      glLoadMatrixf( @transform );

      glColor3f( 0, 0, 0 );
      glBegin( GL_LINE_LOOP );
        for i:= 0 to 3 do
          glVertex3f( vertices[ i * 3 ], vertices[ i * 3 + 1 ], vertices[ i * 3 + 2 ]);
      glEnd;
      glBegin( GL_LINE_LOOP );
        for i:= 4 to 7 do
          glVertex3f( vertices[ i * 3 ], vertices[ i * 3 + 1 ], vertices[ i * 3 + 2 ]);
      glEnd;
      glBegin( GL_LINE );
        for i:= 8 to High( elements ) do
          glVertex3f( vertices[ i * 3 ], vertices[ i * 3 + 1 ], vertices[ i * 3 + 2 ]);
      glEnd;

{      glBindBuffer( GL_ARRAY_BUFFER, vbo_vertices );
      glEnableVertexAttribArray( attribute_v_coord );
      glVertexAttribPointer(
        0, // attribute
        3,                  // number of elements per vertex, here (x,y,z,w)
        GL_FLOAT,           // the type of each element
        GL_FALSE,           // take our values as-is
        0,                  // no extra data between each position
        0                   // offset of first element
      );}

{      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo_elements);
      glDrawElements(GL_LINE_LOOP, 4, GL_UNSIGNED_SHORT, 0);
      glDrawElements(GL_LINE_LOOP, 4, GL_UNSIGNED_SHORT, (GLvoid*)(4*sizeof(GLushort)));
      glDrawElements(GL_LINES, 8, GL_UNSIGNED_SHORT, (GLvoid*)(8*sizeof(GLushort)));
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

      glDisableVertexAttribArray(attribute_v_coord);
      glBindBuffer(GL_ARRAY_BUFFER, 0);

      glDeleteBuffers(1, &vbo_vertices);
      glDeleteBuffers(1, &ibo_elements);}
    end;


    procedure DrawObject( Mdl: TModel; X, Y, Z, Angle: Real; const S: Real = 1.0 );
    var
      _world, normalmatrix: TMatrix4f;
    begin
      _world:= world;
      _world*= MatrixScale( Vector( S, S, S ));
      _world*= MatrixRot( Vector( 0, 1, 0 ), Angle);
      _world*= MatrixTranslate( Vector( X, Y, Z ));

      ShaderSetParameter4fv( ActiveShader, 'normalmatrix', MatMath3DToDGL( {MatrixTranspose(} view3x3 ));
      ShaderSetParameter4fv( ActiveShader, 'world', MatMath3DToDGL( _world ));

      if ( Mdl = oven ) then
        ovenMat:= _world;
      Mdl.Draw;
//      draw_bbox( Mdl, Vector( X, Y, Z ));
    end;

    procedure DrawBreads;
    var
      i: Integer;
      mdl: TModel;
    begin
      ShaderSetParameterf( ActiveShader, 'ready', 0 );
      if ( breadSel = -3 ) then
        DrawObject( dough, -12, 3, 4, 0, doughSize );
      for i:= 0 to high( breads ) do
        if ( i <> breadInOven ) then
          begin
            case ( breads[ i ].btype ) of
              btBread: mdl:= bread;
              btRoll: mdl:= roll;
            end;
            ShaderSetParameterf( ActiveShader, 'ready', breads[ i ].time );
            DrawObject( mdl, breads[ i ].P.X, breads[ i ].P.Y, breads[ i ].P.Z, PI / 2, 1 );
          end;
    end;

    procedure DrawShadow( Light: TVector; LookAt: TVector );
    var
      i: Integer;
      _view: TMatrix4f;
      _world: TMatrix4f;
      scaleandbias: TMatrix4f;
      _proj: TMatrix4f;
    begin
      rtarget_Set( ShadowRT );
      glClearColor(0.0, 0.0, 0.0, 0.0);                      // Set the Background colour of or scene
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer
      ShaderEnable( shadowshader_pass1 );
      _view:= {MatrixScale( Vector( 0.4, 0.4, 0.4 )) * }MatrixLookAtRH( Light, LookAt, Vector( 0, 0, 1 ));
      _proj:= MatrixPerspectiveFOVRH( PI * 0.9, 1, 0.1, 1000 );
      ShaderSetParameter4fv( ActiveShader, 'view', MatMath3DToDGL( {MatrixTranspose(} _view ));
      ShaderSetParameter4fv( ActiveShader, 'proj', MatMath3DToDGL( {MatrixTranspose(} _proj ));
      ShaderSetParameter4fv( ActiveShader, 'world', MatMath3DToDGL( {MatrixTranspose(} MatrixIdentity ));

      DrawBreads;
      ShaderDisable;
      rtarget_Set( Nil );
      scaleandbias:= MatrixScale( Vector( 0.5, 0.5, 0.5 ));
      scaleandbias._30:= 0.5;
      scaleandbias._31:= 0.5;
      scaleandbias._32:= 0.5;
      scaleandbias._33:= 1;
      matShadow:= _view * _proj * scaleandbias;
    end;

{
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
}
    procedure SetupMatrices;
    var
      zglMat: dglOpenGL.TMatrix4f;
    begin
      world:= MatrixIdentity;

//      Cam.R:= Vector( 45, GetTickCount / 100, 0 );
      view:= MatrixIdentity;
//      view*= MatrixRot( Vector( 0.0, 1.0, 0.0 ), deg2rad* yrot );
//      view*= MatrixRot( Vector( 1.0, 0.0, 0.0 ), deg2rad* xrot );
      view*= MatrixRot( Vector( 0.0, 1.0, 0.0 ), deg2rad* Cam.R.y );
      view*= MatrixRot( Vector( 1.0, 0.0, 0.0 ), deg2rad* Cam.R.x );
      view3x3:= view;
      view:= MatrixTranslate( -Cam.P ) * view;//Vector( 0, 0, Zoom ));
      MatrixInverse( view, invview );

//      lightDir:= {Vector(-0.5,0.7,-0.3).Normalize;//}5*Vector( 1, 0, 0 ) * MatrixRot( Vector( 0, 0, 1 ), sin( GetTickCount / 3000 ) * PI / 2) + Vector( 0, 0, 5 );

//      glGetFloatv( GL_PROJECTION_MATRIX, @zglProj );
      proj:= //MatDGLtoMath3D( zglProj );
            MatrixPerspectiveFOVRH( deg2rad* 90, 4/3, 0.5, 10000 );
      glMatrixMode( GL_PROJECTION );
//      glPushMatrix;
      zglMat:= MatMath3DToDGL( proj );
      glLoadMatrixf( @zglMat );
      glMatrixMode( GL_MODELVIEW );
      zglMat:= MatMath3DToDGL( view );
      glLoadMatrixf( @zglMat );
      CamPos:= Cam.P; //Vector( 0, 0, Zoom ) * view * world;
    end;

  begin
    glCullFace( GL_BACK );
    glEnable( GL_CULL_FACE );

    glClearColor(0.0, 0.0, 0.0, 1.0);                      // Set the Background colour of or scene
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

    Set3DMode( 90 );

    SetupMatrices;

    glEnable( GL_BLEND );
    glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );


    DrawShadow( lightDir, Vector( -5, 0, 0 ));

    ShaderEnable( pplight );

    ShaderSetParameter3f( ActiveShader, 'lightdir', lightDir.x, lightDir.y, lightDir.z );
    ShaderSetParameter4fv( ActiveShader, 'view', MatMath3DToDGL( view ));
    ShaderSetParameter4fv( ActiveShader, 'proj', MatMath3DToDGL( proj ));
    ShaderSetParameter4fv( ActiveShader, 'texproj', MatMath3DToDGL( matShadow ));
    ShaderSetParameter4fv( ActiveShader, 'invview', MatMath3DToDGL( invview ));

    glActiveTexture( GL_TEXTURE1 );
    glEnable( GL_TEXTURE_2D );
    glBindTexture( GL_TEXTURE_2D, ShadowRT^.Surface^.ID );

    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER );

    ShaderSetParameteri( ActiveShader, 'Texture0', 0 );
    ShaderSetParameteri( ActiveShader, 'shadow', 1 );

    ShaderSetParameteri( ActiveShader, 'highlight', 0 );
    DrawObject( kitchen, 0, 0, 0, 0, 2 );

    ShaderSetParameteri( ActiveShader, 'highlight', Ord( ovenHighLight ));
    DrawObject( oven, 0, 0, 0, 0, 2 );
    ShaderSetParameteri( ActiveShader, 'highlight', Ord( bowlHighLight ));
    DrawObject( bowl, 0, 0, 0, 0, 2 );

//    DrawObject( oven, 2, 3, -8, PI / 2, 1 );

    //DRAW BREADS

    ShaderEnable( breadshader );


    ShaderSetParameter3f( ActiveShader, 'lightdir', lightDir.x, lightDir.y, lightDir.z );
    ShaderSetParameter4fv( ActiveShader, 'view', MatMath3DToDGL( {MatrixTranspose(} view ));
    ShaderSetParameter4fv( ActiveShader, 'proj', MatMath3DToDGL( {MatrixTranspose(} proj ));

    glActiveTexture( GL_TEXTURE1 );
    glEnable( GL_TEXTURE_2D );
    glBindTexture( GL_TEXTURE_2D, doughmap^.ID );

    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
    glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );

    ShaderSetParameteri( ActiveShader, 'Texture0', 0 );
    ShaderSetParameteri( ActiveShader, 'dough', 1 );
    {
    if ( breadSel = -3 ) then
      begin
        ShaderSetParameterf( ActiveShader, 'ready', 0 );
        DrawObject( dough, -12, 3, 4, 0, doughSize );
      end;}
    DrawBreads;
    {
    for i:= 0 to high( breads ) do
      begin
        ShaderSetParameterf( ActiveShader, 'ready', breads[ i ].time );
        if ( i = breadInOven ) then

        else
          DrawObject( bread, breads[ i ].P.X, breads[ i ].P.Y, breads[ i ].P.Z, PI/2, 1 );
      end;}
    if ( breadInOven >= 0 ) then
      begin
        case ( breads[ breadInOven ].btype ) of
          btBread: mdl:= bread;
          btRoll: mdl:= roll;
        end;
        ShaderSetParameterf( ActiveShader, 'ready', breads[ breadInOven ].time );
        DrawObject( mdl, 12.3, 0.8, 1, 0, 1 );
      end;
    glActiveTexture( GL_TEXTURE1 );
    glDisable( GL_TEXTURE_2D );
    glActiveTexture( GL_TEXTURE0 );

    //END DRAW BREADS
    ShaderDisable;

    if ( titleAlpha > 0 ) then
        begin
          ShaderEnable( simpleshader );

          ShaderSetParameter4fv( ActiveShader, 'world', MatMath3DToDGL( MatrixIdentity ));
          ShaderSetParameter4fv( ActiveShader, 'view', MatMath3DToDGL( MatrixIdentity ));
          ShaderSetParameter4fv( ActiveShader, 'proj', MatMath3DToDGL( MatrixIdentity ));

          glActiveTexture( GL_TEXTURE0 );
          glBindTexture( GL_TEXTURE_2D, titlemap^.ID );

          glBegin(GL_QUADS);
          glColor4f( 1, 1, 1, titleAlpha );
          glTexCoord2f( 0, 1 );
          glVertex3f( -1, 1, 0.0);

          glTexCoord2f( 0, 0 );
          glVertex3f( -1, -1, 0.0);

          glTexCoord2f( 1, 0 );
          glVertex3f( 1, -1, 0.0);

          glTexCoord2f( 1, 1 );
          glVertex3f( 1, 1, 0.0);
          glEnd();

          ShaderDisable;
        end;



    Set2DMode;

    glCullFace( GL_BACK );
    glDisable( GL_CULL_FACE );
  end;
var
  cl: Cardinal;

begin
  Draw3D;
  Set2DMode;

  glActiveTexture( GL_TEXTURE0 );
  glEnable( GL_TEXTURE_2D );

//  text_DrawEx( fntMain, 0, 0, 0.5, 0.0, Format( 'Cam.P = <%G,%G,%G>', [ Cam.P.X, Cam.P.Y, Cam.P.Z ]));
//  text_DrawEx( fntMain, 0, 25, 0.5, 0.0, Format( 'Cam.R = <%G,%G,%G>', [ Cam.R.X, Cam.R.Y, Cam.R.Z ]));
//  text_DrawEx( fntMain, 0, 50, 0.5, 0.0, Format( 'Light = <%G,%G,%G>', [ lightDir.X, lightDir.Y, lightDir.Z ]));
  text_DrawEx( fntMain, 0, 0, 0.5, 0.0, Format( 'Score = %d', [ Score ]));
  text_DrawEx( fntMain, 0, 100, 1, 0.0, Msg, Min( 255, MsgAlpha ));
  text_DrawEx( fntMain, 0, 200, 1, 0.0, MsgEnd, Min( 255, MsgAlpha ), $00FF00 );

end;

procedure FinishBread( n: Integer );
var
  AddScore: Integer;
  MsgTxt: String;
begin
  AddScore:= Round( cos( PI * Min( 0.5, Max( -0.5, ( breads[ n ].time - 1 ) * 2 ))) * 100 ) * 10;

  if ( AddScore < 500 ) then
    MsgTxt:= 'Too early or to late!'
  else if ( AddScore < 800 ) then
    MsgTxt:= 'Getting closer...'
  else if ( AddScore < 900 ) then
    MsgTxt:= 'Not bad! '
  else if ( AddScore < 950 ) then
    MsgTxt:= 'Really close!'
  else
    MsgTxt:= 'Perfect!';

  Msg:= Format( MsgTxt + ' Score = %d (%Gs)', [ AddScore, Round( breads[ n ].time * 100 ) / 10 ]);
  Score:= Score + AddScore;
  MsgAlpha:= 512;
  breads[ n ].finished:= True;
end;

function NewBread: Integer;
begin
  Result:= Length( breads );
  SetLength( breads, Length( breads ) + 1 );
  breads[ Result ].finished:= False;
  breads[ Result ].time:= 0;
end;

procedure Timer;
const speed = 1;
var
  xres, yres: Integer;
  vdir: TVector;
  rayOrig: TVector;
  zero_n: Extended;
  breadcnt: Integer;
  i: Integer;
  hit: TVector;

  procedure TestBreads( RayOrig, RayDir: TVector );
  var
    m: TMatrix4f;

    i: Integer;
    hit: TVector;
  begin
    breadSel:= -2;
    for i:= 0 to High( breads ) do
      begin
        if ( breads[ i ].finished ) then
          Continue;
        m:= MatrixRot( Vector( 0, 1, 0 ), PI / 2 ) * MatrixTranslate( breads[ i ].P );
        if ( IntersectRayMdl( RayOrig, RayDir, bread, m, hit )) then
//        if ( TestBread( i )) then
          begin
            breadSel:= i;
            break;
//            breads[ i ].selected:= not breads[ i ].selected;
          end
      end;
  end;


begin
//  Cam.R.X:= min( 90, max( -90, Cam.R.X + Round(( my - mouse_Y )) / 2 ));
//  Cam.R.Y:= Cam.R.Y + Round(( mx - mouse_X )) / 2;
{  if ( mouseDown[ 1 ]) then
    begin
      lightDir.X:= lightDir.X + ( my - mouse_Y ) / 2;
      lightDir.Z:= lightDir.Z + ( mx - mouse_X ) / 2;
    end;}
  xres:= zgl_Get( VIEWPORT_WIDTH );
  yres:= zgl_Get( VIEWPORT_HEIGHT );
  rayOrig:= VecUnproject( Vector( mouse_X, mouse_Y, 0 ), MatrixIdentity, view, proj, xres, yres, vdir );

  if ( mouseDown[ 0 ]) then
    begin
      if ( breadSel = -1 ) then
        begin
          if ( ovenHighLight ) then
            begin
              breadSel:= breadInOven;
              breadInOven:= -1;
              FinishBread( breadSel );
              ovenHighLight:= False;
            end
          else
            begin
              zero_n:= - rayOrig.y / vdir.Y;
              dMove:= rayOrig + zero_n * vdir;
              TestBreads( rayOrig, vdir )
            end;
        end
      else if ( breadSel = -2 ) then
        begin
          zero_n:= - rayOrig.y / vdir.Y;
          dOff:= dMove - ( rayOrig + zero_n * vdir );
        end
      else if ( breadSel = -3 ) then
        begin
          if ( IntersectRayMdl( rayOrig, vdir, bowl, MatrixScale( Vector( 2, 2, 2 )), hit )) then
            doughSize:= doughSize + ( GetTickCount - timeGone ) / 5000
          else
            begin
              bowlHighLight:= False;
              breadSel:= NewBread;
              if ( doughSize < 0.2 ) then
                breads[ breadSel ].btype:= btRoll
              else
                breads[ breadSel ].btype:= btBread;
            end;
        end
      else
        begin
          zero_n:= - rayOrig.y / vdir.Y;
          breads[ breadSel ].P:= rayOrig + zero_n * vdir;
          breads[ breadSel ].P.Z:= Min( Max( -1, breads[ breadSel ].P.Z ), 4.5 );

          breads[ breadSel ].P.X:= Max( -19, Min( 19, breads[ breadSel ].P.X ));
          if ( not breads[ breadSel ].finished ) then
            ovenHighLight:= IntersectRayMdl( rayOrig, vdir, oven, ovenMat, hit );
        end;
    end
  else
    begin
      if ( ovenHighLight AND ( breadSel > -1 )) then
        breadInOven:= breadSel;
      ovenHighLight:= False;
      breadSel:= -1;
      bowlHighLight:= IntersectRayMdl( rayOrig, vdir, bowl, MatrixScale( Vector( 2, 2, 2 )), hit );
      if ( bowlHighLight ) then
        begin
          breadSel:= -3;
          doughSize:= 0.0;
        end;
      if ( breadInOven > -1 ) then
        ovenHighLight:= IntersectRayMdl( rayOrig, vdir, oven, ovenMat, hit );
    end;

  if ( breadInOven >= 0 ) then
    breads[ breadInOven ].time:= Min( breads[ breadInOven ].time + ( GetTickCount - timeGone ) / 10000, 2 );

//  breadcnt:= 0;
//  for i:= 0 to High( breads ) do
//    breadcnt:= breadcnt + Ord( breads[ i ].finished );
//  if ( breadcnt > High( breads ) ) then
//    MsgEnd:= Format( 'The End.. Overall Score = %d', [ Score ]);;

  MsgAlpha:= Max( 0, Round( MsgAlpha - (( GetTickCount - timeGone ) / 100 )));
  if ( timeGone > 0 ) then
    titleAlpha:= Max( 0, titleAlpha - (( GetTickCount - timeGone ) / 1000 ));
  dOff*= 0.9;
  Cam.P.X:= Min( 16, Max( -16, Cam.P.X + dOff.X ));

//  breads[ 0 ].P:= VecUnproject( Vector( mouse_X, mouse_Y, 0 ), MatrixIdentity, view, proj, xres, yres, vdir );
//  breads[ 0 ].P+= vdir * 2;
  mx:= mouse_X;
  my:= mouse_Y;

{  if ( keysDown[ K_A ]) then
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
    Cam.P+= Vector( view._01, view._11, view._21 ) * speed;}

  if ( key_Press( K_F5 )) then
    begin
      UnloadShader;
      LoadShader;
    end;

  if key_Press( K_ESCAPE ) Then zgl_Exit();

  mouse_ClearState();
  key_ClearState();
  joy_ClearState();
  timeGone:= GetTickCount;
end;

Begin
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  timer_Add( @Timer, 16 );

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );
  zgl_Reg( SYS_EXIT, @DeInit );

  wnd_SetCaption( 'Baking Bread' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.
