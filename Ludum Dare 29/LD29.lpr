program LD29;

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
  strutils,
  zglHeader,
  LCLIntf,
  dglOpenGL,
  sysutils,
  Classes,
  Interfaces, //crashes in linux without
  shaders,
  Math,
  Math3D,
  Model,
  shadernodes,
  vars,
  framebuffer,
  lighting, primitves2d
  ;

var
  Triangle: array[ 0..2 ] of TVec3;

function col2d_PointInTriangle( P : TVec2; P1, P2, P3 : TVec2 ) : Boolean;
var
  o1 : Integer;
  o2 : Integer;
  o3 : Integer;
begin
  o1 := m_Orientation( P.X, P.Y, P1.x, P1.y, P2.x, P2.y );
  o2 := m_Orientation( P.X, P.Y, P2.x, P2.y, P3.x, P3.y );

  if ( o1 * o2 ) <> -1 Then
    begin
      o3 := m_Orientation( P.X, P.Y, P3.x, P3.y, P1.x, P1.y );
      if ( o1 = o3 ) or ( o3 = 0 ) Then
        Result := TRUE
      else
        if o1 = 0 Then
          Result := ( o2 * o3 ) >= 0
        else
          if o2 = 0 Then
            Result := ( o1 * o3 ) >= 0
          else
            Result := FALSE;
    end else
      Result := FALSE;
end;

function col2d_PointInCircle( P : TVec2; Circle: TVec3 ) : Boolean;
begin
  Result := sqr( Circle.X - P.X ) + sqr( Circle.Y - P.Y ) <= sqr( Circle.Z );
end;

function col2d_LineVsCircle( P1, P2: TVec2; Circle : TVec3 ) : Boolean;
  var
    p_1, p_2  : TVec2;
    dx, dy  : Single;
    a, b, c : Single;
begin
  p_1[ 0 ] := P1.X - Circle.X;
  p_1[ 1 ] := P1.Y - Circle.Y;
  p_2[ 0 ] := P2.X - Circle.X;
  p_2[ 1 ] := P2.Y - Circle.Y;

  dx := p_2[ 0 ] - p_1[ 0 ];
  dy := p_2[ 1 ] - p_1[ 1 ];

  a := sqr( dx ) + sqr( dy );
  b := ( p_1[ 0 ] * dx + p_1[ 1 ] * dy ) * 2;
  c := sqr( p_1[ 0 ] ) + sqr( p_1[ 1 ] ) - sqr( Circle.Z );

  if -b < 0 Then
    Result := c < 0
  else
    if -b < a * 2 Then
      Result := a * c * 4 - sqr( b )  < 0
    else
      Result := a + b + c < 0;
end;


function col2d_CircleInTriangle( Circle: TVec3; P1, P2, P3 : TVec2 ) : Boolean;
begin
  Result:= col2d_PointInTriangle( vec2( Circle.X, Circle.Y ), P1, P2, P3 )
           {AND NOT ( col2d_PointInCircle( P1, Circle )
                  OR col2d_PointInCircle( P2, Circle )
                  OR col2d_PointInCircle( P3, Circle ))
           AND NOT ( col2d_LineVsCircle( P1, P2, Circle )
                  OR col2d_LineVsCircle( P1, P3, Circle )
                  OR col2d_LineVsCircle( P2, P3, Circle ))};
end;

function LoadShader: Boolean;
var
  DefaultShader: TBaseShader;

  F: TStringList;
  diffuse: TVec4;
  specular: TVec4;
  pFloat: PGLfloat;
  i: Integer;

  procedure InitShader;
  begin
//    LightSource.PassToActiveShader( vec3( 0 ), 8, Mat4Identity, [ liLightParams ]);
  end;

begin
  Result:= False;
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
//  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'mat_Material.vert' ), LoadShaderToText( 'mat_Material.frag' ));
  if ( not Assigned( simpleshader )) then
    begin
      //zgl_Exit;
      exit;
    end;
//  shadowshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'shadow.vert' ), LoadShaderToText( 'shadow.frag' ));
  shadowshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'parabol_shadow.vert' ), LoadShaderToText( 'parabol_shadow.frag' ));

  if ( not Assigned( shadowshader )) then
    begin
      //zgl_Exit;
      exit;
    end;

//  depthshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'depthbuffer.vert' ), LoadShaderToText( 'depthbuffer.frag' ));
//  depthblurshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'depthblur.vert' ), LoadShaderToText( 'depthblur.frag' ));
//  HBlur:= CreateVertexAndFragmentShader( LoadShaderToText( 'HBlur.vert' ), LoadShaderToText( 'Blur.frag' ));
//  VBlur:= CreateVertexAndFragmentShader( LoadShaderToText( 'VBlur.vert' ), LoadShaderToText( 'Blur.frag' ));

  simpleshader.Enable;
  InitShader;

//  shadowshader.Enable;
//  InitShader;

//  shadowshader.Disable;
  Result:= True;
end;

procedure UnloadShader;
begin
  FreeAndNil( simpleshader );
//  FreeAndNil( shadowshader );
//  FreeAndNil( depthshader );
//  FreeAndNil( depthblurshader );
//  FreeAndNil( HBlur );
//  FreeAndNil( VBlur );
end;

function LoadModels: Boolean;
var
  i: Integer;
begin
  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) +  '/../../../models' );
end;

procedure UnloadModels;
begin
  Mdl.Free;
  lightbulb.Free;
  Mdlsphere.Free;
end;

procedure LoadTextures;
begin
  cursorTex:= tex_LoadFromFile( 'mouse.png' );
end;

procedure DeInit;
begin
  ShadowFB.Free;

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
{
//  GL_FRAMEBUFFER_COMPLETE;
  ShadowRes:= 4096;
  ShadowFB:= TFrameBufferTex2D.Create( 0, True, ShadowRes, ShadowRes, False );
//  ShadowFB:= TFrameBufferTex2D.Create( 1, False, ShadowRes, ShadowRes, False );
  ShadowFB.BindFB();
  WriteLn( 'FB_STATUS: ', glCheckFramebufferStatus( GL_FRAMEBUFFER ));
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );}
end;

procedure Draw;
    procedure SetupMatrices;
    var
      zglMat: dglOpenGL.TMatrix4f;
      _view: TMat4;
    begin
      world:= Mat4Identity;

      view:= Mat4Identity;
      view:= view * MatrixTranslate( vec4( -sphere.X, -sphere.Y, -1, 1 ));
      _view:= Mat4Rot( vec3_Axis_PZ, deg2rad* yrot );
      _view*= Mat4Rot( vec3_Axis_PX, deg2rad* xrot );
      view*= _view;
      view3x3:= mat3( _view );
//      view:= view * MatrixTranslate( vec4( 0, 0, -Zoom, 1 ));
//      MatrixInverse( view, invview );
      proj:= MatrixPerspectiveFOVLH( deg2rad* 90, 4/3, 0.1, 100 );

      glMatrixMode( GL_PROJECTION );
      glLoadIdentity;
      glLoadMatrixf( @proj );

      glMatrixMode( GL_MODELVIEW );
      glLoadMatrixf( @view );

      CamPos:= vec4( 0, 0, 0, 1) * view * world;//Vec4( 0, 0, Zoom, 1 ) * view * world;
    end;

    procedure RenderScene( RenderMode: TRenderFlags );
    var
      halfVec: TVec4;
      lightpos: TVec3;
      lvpos: TVec4;
      eyeVec: TVec3;
      shadow_view: TMat4;
      pass: Integer;
      i: Integer;
    begin
      if ( Assigned( ActShad )) then
        begin
          eyeVec:= vec3( 0, 0, -1 );//( -vec3( CamPos )).Normalize;
        //  glUniform4f( ActShad.Uniforms.AddrByName( 'eyePos' ), CamPos.x, CamPos.y, CamPos.z, 0 );
          glUniform4f( ActShad.Uniforms.AddrByName( 'eyeVec' ), eyeVec.x, eyeVec.y, eyeVec.z, 2 );

          ShaderSetParameter4fv( ActShad.ShaderObj, 'world', world );
          ShaderSetParameter4fv( ActShad.ShaderObj, 'view', view );
          ShaderSetParameter4fv( ActShad.ShaderObj, 'proj', proj );
        end;
    end;

var
  XRes, YRes: Integer;

begin
  if ( not ( Assigned( shadowshader ) AND Assigned( simpleshader ))) then
    exit;
//  glClearColor(0.2, 0.2, 0.5, 1.0);                      // Set the Background colour of or scene
  glClearColor(0.01, 0.02, 0.05, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  glMatrixMode( GL_PROJECTION );
  glLoadIdentity();
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity();
  glMatrixMode( GL_PROJECTION );
  glEnable( GL_DEPTH_TEST );
  //gluPerspective( 90, 4/3, 0.1, 100 );
  SetupMatrices;

//  shadowshader.Enable;
//  simpleshader.Enable;
//  ShadowFB.BindFB();
//  ShadowFB.BindRB();
  glCullFace( GL_FRONT );
//  glDrawBuffer( GL_NONE );
//  glReadBuffer( GL_NONE );
  glDisable( GL_BLEND );

  glViewport ( 0, 0, ShadowRes, ShadowRes );
//  ShadowFB.BindAndClear();

//  RenderScene([ rfShadowMap ]);

  glCullFace( GL_NONE );

//  glDrawBuffer( GL_BACK );
//  glReadBuffer( GL_BACK );

//  shadowshader.Disable;

  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;

  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;

  glEnable( GL_BLEND );
  XRes:= Integer( zgl_Get( VIEWPORT_WIDTH ));
  YRes:= Integer( zgl_Get( VIEWPORT_HEIGHT ));
  glViewport ( 0, 0, XRes, YRes );

  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
  glBindRenderbuffer( GL_RENDERBUFFER, 0 );

  simpleshader.Enable;

//  glActiveTexture( GL_TEXTURE2 );
//  glBindTexture( GL_TEXTURE_2D, {ShadowFB.Textures[ 0 ]);//}ShadowFB.TexDepth );
//  glUniform1i( ActShad.Uniforms.AddrByName( 'shadow' ), 2 );

  RenderScene([]);

  ShaderDisable;

  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;

  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;

{  if ( Assigned( depthshader )) then
    begin
      glViewport( 0, 0, 200, 200 );
      depthshader.Enable;

      RenderDepthQuad;

      depthshader.Disable;
      glViewport ( 0, 0, XRes, YRes );
    end;}
  Set2DMode;

  ssprite2d_Draw( cursorTex,  mouse_X()-cursorTex^.Width / 8, mouse_Y()-cursorTex^.Height / 8, cursorTex^.Width / 4, cursorTex^.Height / 4, 0 );
  text_Draw( fntMain, 0, 0, Format( 'Light[0].Rotation=X=%f,Y=%f', [ Lxrot1, Lyrot1 ]));
  text_Draw( fntMain, 0, 40, Format( 'Light[1].Rotation=X=%f,Y=%f', [ Lxrot2, Lyrot2 ]));
  text_Draw( fntMain, 0, 80, Format( 'mouse=dX=%s,dY=%s', [ IntToStr( mouse_DX() ), IntToStr( mouse_DY() )]));
end;

function CheckSphereCollision( sph: TVec4; direction: TVec3; out HitPoint: TVec3 ): Boolean;
  function intersectRayTriangle( orig, dir, v0, v1, v2: TVec3; out baryPosition: TVec3 ): Boolean;
  var
    e1, e2: TVec3;
    p: TVec3;
    a: Float;
    f: Extended;
    s: TVec3;
    q: TVec3;
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

  function CollideMdl( Model: TModel ): Boolean;
  var
    face: Integer;
    vert: Integer;
    v0, v1, v2: TVec3;
    index: Integer;
    idxcnt: Integer;
  begin
    Result:= False;
    idxcnt:= Length( Model.Indices );
    for index:= 0 to idxcnt div 3 - 1 do
      begin
        v0:= vec3( vec4( Model.Positions[ Model.Indices[ index * 3 + 0 ]], 1 ) * Model.Matrix * world );
        v1:= vec3( vec4( Model.Positions[ Model.Indices[ index * 3 + 1 ]], 1 ) * Model.Matrix * world );
        v2:= vec3( vec4( Model.Positions[ Model.Indices[ index * 3 + 2 ]], 1 ) * Model.Matrix * world );
        if ( {intersectRayTriangle( vec3( sphere ), direction, v0, v1, v2, HitPoint )) then//}col2d_CircleInTriangle( vec3( sph.x + direction.x, sph.y + direction.y, sph.w ), vec2( v0.x, v0.y ), vec2( v1.x, v1.y ), vec2( v2.x, v2.y ))) then
          begin
            Result:= True;
            if ( Assigned( ActShad )) then
              glUniform4f( ActShad.Uniforms.AddrByName( 'mat_diffuse' ), 1, 1, 1, 1 );

//            if ( not intersectRayTriangle( vec3( sph.x, sph.y, 10 ), vec3( 0, 0, -1 ), v0, v1, v2, HitPoint )) then
//              HitPoint:= vec3( sph );
            Triangle[ 0 ]:= v0;
            Triangle[ 1 ]:= v1;
            Triangle[ 2 ]:= v2;
            Break;
          end;
      end;
    {
    for face:= 0 to high( Model.Faces ) do
      begin
        v0:= vec3( vec4( Model.Positions[ Model.Faces[ Face ].verts[ 0 ].v ], 1 ) * Model.Matrix );
        for vert:= 1 to Length( Model.Faces[ face ].verts ) - 2 do
          begin
            v1:= vec3( vec4( Model.Positions[ Model.Faces[ face ].verts[ vert ].v ], 1 ) * Model.Matrix );
            v2:= vec3( vec4( Model.Positions[ Model.Faces[ face ].verts[ vert + 1 ].v ], 1 ) * Model.Matrix );
            if ( col2d_CircleInTriangle( vec3( sph.x, sph.y, sph.w ), vec2( v0.x, v0.y ), vec2( v1.x, v1.y ), vec2( v2.x, v2.y ))) then
              begin
                Result:= True;
                glUniform4f( ActShad.Uniforms.AddrByName( 'mat_diffuse' ), 1, 1, 1, 1 );
                Triangle[ 0 ]:= v0;
                Triangle[ 1 ]:= v1;
                Triangle[ 2 ]:= v2;
                Break;
              end;
          end;
        if ( Result ) then
          break;
      end;}
  end;

var
  i: Integer;
begin
  Result:= False;
  for i:= 0 to Mdl.Children.Count - 1 do
    if ( strutils.AnsiEndsStr( '_collision', Mdl.Children[ i ].Name )) then
      if ( CollideMdl( Mdl.Children[ i ])) then
        begin
          Result:= True;
          break;
        end;
end;

procedure Input;
var
  cur, cur2: TPoint;
  newsphere: TVec4;
  tmpsphere: TVec3;
  m, m2: TVec2;
  direction: TVec3;
begin
  GetCursorPos( cur );
//  mouse_Lock();
  cur2.x:= zgl_Get( WINDOW_X ) + zgl_Get( WINDOW_WIDTH ) div 2;
  cur2.y:= zgl_Get( WINDOW_Y ) + zgl_Get( WINDOW_HEIGHT ) div 2;
//  cur.X:= mouse_DX();
//  cur.Y:= mouse_DY();

//  if ( mouse_Down( M_BLEFT )) then
  m:= vec2(( cur2.x - cur.x ), ( cur2.y - cur.y ));
  m *= Max( 0, m.GetDist - 50 ) / 1000;
  m2:= vec2( mx - cur.x, my - cur.y ).Normalize;
  if ( mouse_Down( M_BRIGHT )) then
    begin
      if ( LS = 0 ) then
        begin
          Lxrot1:= Lxrot1 - m2.x / 50;
          Lyrot1:= Lyrot1 - m2.y / 50;
        end
      else
        begin
          Lxrot2:= Lxrot2 - m2.x / 50;
          Lyrot2:= Lyrot2 - m2.y / 50;
        end
    end
  else
    begin
      xrot:= ( min( 180, max( 0, xrot + m.y / 5 )));//Round(( my - cur.y )) / 5 ));
      yrot:= ( yrot + m.x / 5 );//Round(( mx - cur.x )) / 5;
    end;

//  if ( key_Down( K_TAB )) then
//    sphcoll:= not sphcoll;

  if key_Press( K_F5 ) Then
    begin
      UnloadShader;
      LoadShader;
    end;
  if key_Press( K_F6 ) Then
    begin
      UnloadModels;
      LoadModels;
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

  wnd_ShowCursor( False );
  wnd_SetCaption( 'model test' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.