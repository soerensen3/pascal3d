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
  strutils,
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
  lighting,
  zglHeader
  ;

var
  Triangle: array[ 0..2 ] of TVec3;
  m: TVec2;

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
    LightSource.PassToActiveShader( vec3( 0 ), 8, Mat4Identity, [ liLightParams ]);
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
  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'simple.vert' ), LoadShaderToText( 'simple.frag' ));
//  simpleshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'shader.vert' ), LoadShaderToText( 'shader.frag' ));
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

  depthshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'depthbuffer.vert' ), LoadShaderToText( 'depthbuffer.frag' ));
//  depthblurshader:= CreateVertexAndFragmentShader( LoadShaderToText( 'depthblur.vert' ), LoadShaderToText( 'depthblur.frag' ));
  HBlur:= CreateVertexAndFragmentShader( LoadShaderToText( 'HBlur.vert' ), LoadShaderToText( 'Blur.frag' ));
  VBlur:= CreateVertexAndFragmentShader( LoadShaderToText( 'VBlur.vert' ), LoadShaderToText( 'Blur.frag' ));

  simpleshader.Enable;
  InitShader;

  shadowshader.Enable;
  InitShader;

  shadowshader.Disable;
  Result:= True;

  for i:= 0 to Mdlsphere.Materials.Count - 1 do
    begin
      Mdlsphere.Materials[ i ].Shader:= simpleshader;
      Mdlsphere.Materials[ i ].ShaderShadow:= shadowshader;
    end;

  for i:= 0 to Mdl.Materials.Count - 1 do
    begin
      Mdl.Materials[ i ].Shader:= simpleshader;
      Mdl.Materials[ i ].ShaderShadow:= shadowshader;
    end;
  for i:= 0 to lightbulb.Materials.Count - 1 do
    begin
      lightbulb.Materials[ i ].Shader:= simpleshader;
      lightbulb.Materials[ i ].ShaderShadow:= shadowshader;
    end;

//  DefaultShader.Free;
end;

procedure UnloadShader;
begin
  FreeAndNil( simpleshader );
  FreeAndNil( shadowshader );
  FreeAndNil( depthshader );
//  FreeAndNil( depthblurshader );
  FreeAndNil( HBlur );
  FreeAndNil( VBlur );
end;

function LoadModels: Boolean;
var
  i: Integer;
begin
  SetCurrentDir( ExtractFilePath( ParamStr( 0 )) +  '/../../../models' );

  //testTex:= tex_LoadFromFile( 'Kaisersemmel.jpg' );
  //specTex:= tex_LoadFromFile( 'spec.png' );
  Mdlsphere:= LoadModelFileFromFile( 'sphere.model' );
//  Mdl:= LoadModelFileFromFile( 'collision_test.model' );
  Mdl:= LoadModelFileFromFile( 'moon.model' );
//  Mdl:= LoadModelFileFromFile( 'landscape.model' );
//  Mdl:= LoadModelFileFromFile( 'spaceship_shark.model' );
//  Mdl:= LoadModelFileFromFile( 'character.model' );
//  Mdl:= LoadModelFileFromFile( 'valley_test.model' );
//  Mdl:= LoadModelFileFromFile( 'detective.model' );
  lightbulb:= LoadModelFileFromFile( 'lightbulb.model' );;

  for i:= 0 to Mdl.Children.Count - 1 do
    if ( strutils.AnsiEndsStr( '_collision', Mdl.Children[ i ].Name )) then
      Mdl.Children[ i ].Visible:= False;

  Result:= True;
//  Mdl.Children[ 0 ].testTex:= testTex^.ID;
//  WriteLn( Mdl.Debug );
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

  sphere:= vec4( 0, 4, -1.2+0.05, 0.05 );
  LightSource:= TLightList.Create;
  with ( LightSource[ LightSource.Add( TLight.Create )]) do
    begin
      Position:= vec3( 0 );
      Diffuse:= vec4( 1, 1, 1, 1 );
      Specular:= vec4( 1, 1, 1, 0.8 );
      LinearAttenuation:= 0.1;
    end;
  with ( LightSource[ LightSource.Add( TLight.Create )]) do
    begin
      Position:= vec3( 0 );
      Diffuse:= vec4( 1, 0.6, 0.3, 1 );
      Specular:= vec4( 1, 1, 1, 0.8 );
      LinearAttenuation:= 0.2;
    end;

  LoadModels;
  LoadTextures;
  LoadShader;

  glEnable( GL_BLEND );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA );

  xrot:= 80;
  yrot:= 0;
  zoom:= 10;
  Lxrot1:= 1.22;
  Lyrot1:= -1.22;
  LZoom1:= 10;
  Lxrot2:= 0.2;
  Lyrot2:= -1.5;
  LZoom2:= 10;

//  GL_FRAMEBUFFER_COMPLETE;
  ShadowRes:= 4096;
  ShadowFB:= TFrameBufferTex2D.Create( 0, True, ShadowRes, ShadowRes, False );
//  ShadowFB:= TFrameBufferTex2D.Create( 1, False, ShadowRes, ShadowRes, False );
  ShadowFB.BindFB();
  WriteLn( 'FB_STATUS: ', glCheckFramebufferStatus( GL_FRAMEBUFFER ));
  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
end;
procedure Input; forward;
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

          LightSource[ 0 ].Position:= vec3( vec4( 0, 0, Lzoom1, 1 ) * Mat4Rot( vec3_Axis_PX, Lxrot1 ) * Mat4Rot( vec3_Axis_PZ, Lyrot1 ));

          ShaderSetParameter4fv( ActShad.ShaderObj, 'world', world );
          ShaderSetParameter4fv( ActShad.ShaderObj, 'view', view );
          ShaderSetParameter4fv( ActShad.ShaderObj, 'proj', proj );

          LightSource[ 1 ].Position:= vec3( vec4( 0, 0, Lzoom2, 1 ) * Mat4Rot( vec3_Axis_PX, Lxrot2 ) * Mat4Rot( vec3_Axis_PZ, Lyrot2 ));

        //  halfVec:= ( vec3( lvpos ) + VecNormalize( -CamPos )) / 2;
          LightSource.PassToActiveShader( vec3( CamPos ), 8, view * world, [ liPosition ]);

          if not ( rfShadowMap in RenderMode ) then
            for i:= 0 to LightSource.Count - 1 do
              lightbulb.Render( MatrixTranslate( vec4( LightSource[ i ].Position, 1 )), view, proj, RenderMode );

          if ( rfShadowMap in RenderMode ) then
            for i:= 0 to LightSource.Count - 1 do
              begin
                ShaderSetParameteri( ActShad.ShaderObj, 'light', i );
                for pass:= 0 to 1 do
                  begin
                    glViewPort( Round( ( ShadowRes / 4 ) * pass + ShadowRes / 2 * ( i div 4)),
                                Round( ShadowRes / 4 *( i mod 4 )),
                                ShadowRes div 4, ShadowRes div 4 );
                    ShaderSetParameteri( ActShad.ShaderObj, 'renderpass', pass );

                    Mdlsphere.Render( MatrixScale( vec4( vec3( sphere.W ), 1 )) * MatrixTranslate( vec4( vec3( sphere ), 1 )), view, proj, RenderMode );
                    Mdl.Render( world, view, proj, RenderMode );
                  end;
              end
          else
            begin
              if ( sphcoll ) then
                Mdlsphere.Materials[ 0 ].Diff:= Vec3( 1, 0, 0 )
              else
                Mdlsphere.Materials[ 0 ].Diff:= Vec3( 0, 1, 0 );
              Mdlsphere.Render( MatrixScale( vec4( vec3( sphere.W ), 1 )) * MatrixTranslate( vec4( vec3( sphere ), 1 )), view, proj, RenderMode );
//              for i:= 0 to Mdl.Children.Count - 1 do
//                if ( strutils.AnsiEndsStr( '_collision', Mdl.Children[ i ].Name )) then
//                  Mdl.Children[ i ].Render( world, view, proj, RenderMode + [ rfWireFrame ]);
              Mdl.Render( world, view, proj, RenderMode );
{              if ( sphcoll ) then
                begin
                  glBegin( GL_LINE_LOOP );
                  glVertex3f( Triangle[ 0 ].x, Triangle[ 0 ].y, Triangle[ 0 ].z );
                  glVertex3f( Triangle[ 1 ].x, Triangle[ 1 ].y, Triangle[ 1 ].z );
                  glVertex3f( Triangle[ 2 ].x, Triangle[ 2 ].y, Triangle[ 2 ].z );
                  glEnd;
                end;}
            end;
        end;
    end;

    procedure RenderDepthQuad;
    begin
      glActiveTexture( GL_TEXTURE0 );
      glBindTexture( GL_TEXTURE_2D, {ShadowFB.Textures[ 0 ]);//}ShadowFB.TexDepth );
{      glActiveTexture( GL_TEXTURE1 );
      glBindTexture( GL_TEXTURE_2D, specTex^.ID );
      glUniform1i( depthshader.Uniforms.AddrByName( 'specSampler' ), 1 );
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);}

    //  glBindTexture( GL_TEXTURE_2D, testTex^.ID );
      glEnable( GL_TEXTURE_2D );
      glBegin( GL_QUADS );
      glColor4f( 1, 1, 1, 1 );

      glTexCoord2f( 0.0, 1.0 );
      glVertex3f( -1,  1, 0 );

      glTexCoord2f( 1.0, 1.0 );
      glVertex3f(  1,  1, 0 );

      glTexCoord2f( 1.0, 0.0 );
      glVertex3f(  1, -1, 0 );

      glTexCoord2f( 0.0, 0.0 );
      glVertex3f( -1, -1, 0 );
      glEnd();
    end;

var
  XRes, YRes: Integer;

begin
  //Input;
  if ( not ( Assigned( shadowshader ) AND Assigned( simpleshader ))) then
    exit;
  glClearColor(0.2, 0.2, 0.5, 1.0);                      // Set the Background colour of or scene
//  glClearColor(0.01, 0.02, 0.05, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  glMatrixMode( GL_PROJECTION );
  glLoadIdentity();
  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity();
  glMatrixMode( GL_PROJECTION );
  glEnable( GL_DEPTH_TEST );
  //gluPerspective( 90, 4/3, 0.1, 100 );
  SetupMatrices;

  shadowshader.Enable;
//  simpleshader.Enable;
//  ShadowFB.BindFB();
//  ShadowFB.BindRB();
  glCullFace( GL_FRONT );
//  glDrawBuffer( GL_NONE );
//  glReadBuffer( GL_NONE );
  glDisable( GL_BLEND );

  glViewport ( 0, 0, ShadowRes, ShadowRes );
  ShadowFB.BindAndClear();

  RenderScene([ rfShadowMap ]);

  glCullFace( GL_NONE );

//  glDrawBuffer( GL_BACK );
//  glReadBuffer( GL_BACK );

  shadowshader.Disable;

  glMatrixMode( GL_PROJECTION );
  glLoadIdentity;

  glMatrixMode( GL_MODELVIEW );
  glLoadIdentity;
{
//  if ( Assigned( depthblurshader )) then
  if ( Assigned( HBlur ) and Assigned( VBlur )) then
    begin
//      dephtblurshader.Enable;

//      ShaderSetParameteri( depthblurshader.ShaderObj, 'bluramount', 4 );
//      ShaderSetParameterf( depthblurshader.ShaderObj, 'blurdiameter', 0.0005 );
//      ShaderSetParameteri( depthblurshader.ShaderObj, 'direction', 0 );

//      glUniform1i( depthshader.Uniforms.AddrByName( 'bluramount' ), 4 );
//      glUniform1f( depthshader.Uniforms.AddrByName( 'blurdiameter' ), 0.01 );

      glViewport( 0, 0, ShadowRes, ShadowRes );

      HBlur.Enable;
      RenderDepthQuad;

//      ShaderSetParameteri( depthblurshader.ShaderObj, 'direction', 1 );

      VBlur.Enable;
      RenderDepthQuad;

//      depthblurshader.Disable;
    end;
}
  glEnable( GL_BLEND );
  XRes:= Integer( zgl_Get( VIEWPORT_WIDTH ));
  YRes:= Integer( zgl_Get( VIEWPORT_HEIGHT ));
  glViewport ( 0, 0, XRes, YRes );

  glBindFramebuffer( GL_FRAMEBUFFER, 0 );
  glBindRenderbuffer( GL_RENDERBUFFER, 0 );

  simpleshader.Enable;

  glActiveTexture( GL_TEXTURE2 );
  glBindTexture( GL_TEXTURE_2D, {ShadowFB.Textures[ 0 ]);//}ShadowFB.TexDepth );
  glUniform1i( ActShad.Uniforms.AddrByName( 'shadow' ), 2 );

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

//  ssprite2d_Draw( cursorTex,  mouse_X()-cursorTex^.Width / 8, mouse_Y()-cursorTex^.Height / 8, cursorTex^.Width / 4, cursorTex^.Height / 4, 0 );
  text_Draw( fntMain, 0, 0, Format( 'FPS = %d', [ zgl_Get( RENDER_FPS )]));
  text_Draw( fntMain, 0, 40, Format( 'Light[0].Rotation=X=%f,Y=%f', [ Lxrot1, Lyrot1 ]));
  text_Draw( fntMain, 0, 80, Format( 'Light[1].Rotation=X=%f,Y=%f', [ Lxrot2, Lyrot2 ]));
  text_Draw( fntMain, 0, 120, Format( 'mouse=dX=%d,dY=%d', [ mouse_DX(), mouse_DY()]));
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
  {m, }m2: TVec2;
  direction: TVec3;
  scx, scy: Integer;
begin
  xrot:= ( min( 180, max( 0, xrot - mouse_DY() / 5 )));//Round(( my - cur.y )) / 5 ));
  yrot:= ( yrot - mouse_DX() / 5 );//Round(( mx - cur.x )) / 5
  {
//  GetCursorPos( cur );
  cur.x:= Mouse.GetMouseX;
  cur.y:= Mouse.GetMouseY;
  scx:= 800;//zgl_Get( WINDOW_X ) + zgl_Get( WINDOW_WIDTH ) div 2;
  scy:= 600;//zgl_Get( WINDOW_Y ) + zgl_Get( WINDOW_HEIGHT ) div 2;
//  scx:= zgl_Get( DESKTOP_WIDTH ) div 2;
//  scy:= zgl_Get( DESKTOP_HEIGHT ) div 2;
  cur2:= Point( scx - cur.x, scy - cur.y );
//  SetCursorPos( scx, scy );
  Mouse.SetMouseXY( scx, scy );

//  mouse_Lock();
//  cur.X:= mouse_DX();
//  cur.Y:= mouse_DY();          mouse_Lock();

//  if ( mouse_Down( M_BLEFT )) then
  if ( key_Press( K_1 )) then
    LS:= 0;
  if ( key_Press( K_2 )) then
    LS:= 1;
//  m:= vec2(( cur2.x - cur.x ), ( cur2.y - cur.y ));
//  m *= Max( 0, m.GetDist - 50 ) / 1000;
//  m2:= vec2( mx - cur.x, my - cur.y ).Normalize;
  m:= vec2( cur2.x, cur2.y );
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

  newsphere:= sphere;
  if ( key_Down( K_W )) then
    newsphere+= vec4( sin( - yrot * deg2rad ), cos( - yrot * deg2rad ), 0, 0 ) * 0.01;
  if ( key_Down( K_S )) then
    newsphere+= vec4( sin( - yrot * deg2rad - PI ), cos( - yrot * deg2rad - PI ), 0, 0 ) * 0.01;
  if ( key_Down( K_A )) then
    newsphere+= vec4( sin( - yrot * deg2rad - PI/2 ), cos( - yrot * deg2rad - PI /2), 0, 0 ) * 0.01;
  if ( key_Down( K_D )) then
    newsphere+= vec4( sin( - yrot * deg2rad + PI/2 ), cos( - yrot * deg2rad + PI /2), 0, 0 ) * 0.01;

  tmpsphere:= vec3( 0, 0, 0 );
  direction:= vec3( newsphere - sphere );
  sphcoll:= CheckSphereCollision( sphere, direction, tmpsphere );
  newsphere:= vec4( newsphere.x, newsphere.y, tmpsphere.z, sphere.W );
  if ( not sphcoll ) then
    sphere:= newsphere;
  //col2d_CircleInTriangle( vec3( sphere.X, sphere.Y, sphere.W ), vec2( -5, -5 ), vec2( 5, -5 ), vec2( -5, 5 ));
  //col2d_PointInTriangle( vec2( sphere.X, sphere.Y ), vec2( -5, -5 ), vec2( 5, -5 ), vec2( -5, 5 ));

  if ( key_Down( K_CTRL )) then
    begin
      if ( LS = 0 ) then
        begin
          if ( mouse_Wheel( 0 ) or key_Down( K_KP_SUB )) then
            Lzoom1:= Lzoom1 - 0.05;
          if ( mouse_Wheel( 1 ) or key_Down( K_KP_ADD )) then
            Lzoom1:= Lzoom1 + 0.05;
        end
      else
        begin
          if ( mouse_Wheel( 0 ) or key_Down( K_KP_SUB )) then
            Lzoom2:= Lzoom2 - 0.05;
          if ( mouse_Wheel( 1 ) or key_Down( K_KP_ADD )) then
            Lzoom2:= Lzoom2 + 0.05;
        end;
    end
  else
    begin
      if ( mouse_Wheel( 0 ) or key_Down( K_KP_SUB )) then
        zoom:= zoom - 0.05;
      if ( mouse_Wheel( 1 ) or key_Down( K_KP_ADD )) then
        zoom:= zoom + 0.05;
    end;

  if key_Press( K_F5 ) Then
    begin
      UnloadShader;
      LoadShader;
    end;
  if key_Press( K_F6 ) Then
    begin
      UnloadModels;
      LoadModels;
    end;}
  if key_Press( K_ESCAPE ) Then zgl_Exit();
  mouse_ClearState;
  key_ClearState;
  mouse_Lock();
end;

Begin
  {$IFNDEF USE_ZENGL_STATIC}
  if not zglLoad( libZenGL ) Then exit;
  {$ENDIF}

  zgl_Reg( SYS_LOAD, @Init );
  zgl_Reg( SYS_DRAW, @Draw );
  zgl_Reg( SYS_EXIT, @DeInit );
  timer_Add( @model_test.Input, 16 );

//  wnd_ShowCursor( False );
  wnd_SetCaption( 'model test' );

  wnd_ShowCursor( TRUE );

  scr_SetOptions( 800, 600, REFRESH_MAXIMUM, FALSE, FALSE );

  zgl_Init();
End.