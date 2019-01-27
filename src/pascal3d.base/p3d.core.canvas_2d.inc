{$IFDEF INTERFACE}


{ TP3DBrush }
type

  { TP3DCanvas2D }

  TP3DCanvas2D = class ( TP3DCanvas )
    private
      FBrush: TP3DBrush;
      FPen: TP3DPen;
      FProj: TMat4;
      FScreenAligned: Boolean;
      FScroll: TVec2;
      FView: TMat4;
      FZoom: Float;

      procedure SetScreenAligned(AValue: Boolean);
      procedure SetScroll(AValue: TVec2);
      procedure UpdateLockedArea; override;
      procedure UpdateProj;
      procedure UpdateParams;
      procedure PenChange( Sender: TObject );

    public
      constructor Create(AOwner: TObject);
      destructor Destroy; override;

      function PrepareForDrawing(Mat: TP3DMaterial): Boolean;

      procedure RenderLine( p1, p2: TVec2; Color: TP3DColorRGBA );
      procedure RenderLine( p1, p2: TVec2; Color1, Color2: TP3DColorRGBA );
      procedure RenderLines( points: array of TVec2; Color: TP3DColorRGBA );
      procedure RenderLines( points: TVec2List; Color: TP3DColorRGBA );
      procedure RenderLineRect( p1, p2: TVec2; c1, c2, c3, c4: TP3DColorRGBA );
      procedure RenderLineRect( p1, p2: TVec2; Color: TP3DColorRGBA );
      procedure RenderRect( p1, p2: TVec2; c1, c2, c3, c4: TP3DColorRGBA );
      procedure RenderRect( p1, p2: TVec2; Color: TP3DColorRGBA );
      procedure RenderCircle(c: TVec2; r: Single; num_segments: Integer; Color: TP3DColorRGBA );
      procedure RenderRoundRect(p1, p2: TVec2; r: Float; num_segments: Integer; Color: TP3DColorRGBA);
      procedure RenderArc(c: TVec2; r: Single; startangle, endangle: Float;
        num_segments: Integer; Color: TP3DColorRGBA);
      procedure RenderLineCircle( c: TVec2; r: Single; num_segments: Integer; Color: TP3DColorRGBA );
      procedure RenderQuad( p1, p2, p3, p4: TVec2; c1, c2, c3, c4: TP3DColorRGBA );
      procedure RenderQuad( p1, p2, p3, p4: TVec2; Color: TP3DColorRGBA );
      procedure RenderText( Text: TP3DText; P: TVec2 );
      procedure RenderText( Text: String; P: TVec2; var CacheObj: TP3DText );
      procedure RenderText( Text: TP3DTextBmp; P: TVec2 );
      procedure RenderText( Text: String; P: TVec2; var CacheObj: TP3DTextBmp );
      procedure RenderTextShadow( Text: TP3DTextBmp; P: TVec2; blur: Single; Color: TP3DColorRGBA );
      procedure RenderTextShadow( Text: String; P: TVec2; blur: Single; Color: TP3DColorRGBA; var CacheObj: TP3DTextBmp );
      procedure RenderLetter( Text: TP3DText; Idx: Integer; P: TVec2 );
      procedure RenderBezier( p1, p2, p3, p4: TVec2; num_segments: Integer; Color: TP3DColorRGBA );
      procedure RenderRectShadow(p1, p2: TVec2; blur: Single; AColor: TP3DColorRGBA);
      procedure RenderRectShadowInner( p1, p2: TVec2; off, blur: Single; AColor: TP3DColorRGBA );
      function GetValidText( Text: String; var CacheObj: TP3DText ): TP3DText; inline;
      function GetValidText( Text: String; var CacheObj: TP3DText; AFont: TP3DFont ): TP3DText;
      function GetValidText( Text: String; var CacheObj: TP3DTextBmp; AFont: TP3DFont ): TP3DTextBmp;
      function GetValidText( Text: String; var CacheObj: TP3DTextBmp ): TP3DTextBmp; inline;
      function CalcAlignedTextRect( Text: TP3DText; R: TP3DRect; HAlignment: TP3DHorizontalAlignment; VAlignment: TP3DVerticalAlignment ): TVec2;
      function CalcAlignedTextRect( Text: TP3DTextBmp; R: TP3DRect; HAlignment: TP3DHorizontalAlignment; VAlignment: TP3DVerticalAlignment ): TVec2;
      procedure PassToShader( Mat: TP3DMaterial );

      function Lock: Integer; override;
      procedure Unlock(); override;

      property Proj: TMat4 read FProj write FProj;
      property View: TMat4 read FView;
      property Scroll: TVec2 read FScroll write SetScroll;
      property ScreenAligned: Boolean read FScreenAligned write SetScreenAligned;
      property Brush: TP3DBrush read FBrush;
      property Pen: TP3DPen read FPen;
      property Zoom: Float read FZoom write FZoom;
  end;

//function CreateRectMesh: TP3DMesh;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DCanvas2D }

procedure TP3DCanvas2D.RenderLine(p1, p2: TVec2; Color: TP3DColorRGBA);
var
  LineDir: TVec2;
begin
  if ( not PrepareForDrawing( Material )) then exit;

  LineDir:= ( p2 - p1 ).Normalize * Pen.Width / 2;
  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A ); cle( Self );
  glBegin( GL_TRIANGLE_STRIP );
  glVertexAttrib2f( P3DAttribPosition, p1.X - LineDir.y, p1.Y + LineDir.x );
  glVertexAttrib2f( P3DAttribPosition, p2.X - LineDir.y, p2.Y + LineDir.x );
  glVertexAttrib2f( P3DAttribPosition, p1.X + LineDir.y, p1.Y - LineDir.x );
  glVertexAttrib2f( P3DAttribPosition, p2.X + LineDir.y, p2.Y - LineDir.x );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas2D.RenderLine(p1, p2: TVec2; Color1, Color2: TP3DColorRGBA);
var
  LineDir: TVec2;
begin
  if ( not PrepareForDrawing( Material )) then exit;

  LineDir:= ( p2 - p1 ).Normalize * Pen.Width / 2;

  glBegin( GL_TRIANGLE_STRIP );
  glVertexAttrib4f( P3DAttribColor, Color1.R, Color1.G, Color1.B, Color1.A );
  glVertexAttrib2f( P3DAttribPosition, p1.X - LineDir.y, p1.Y + LineDir.x );
  glVertexAttrib2f( P3DAttribPosition, p2.X - LineDir.y, p2.Y + LineDir.x );
  glVertexAttrib4f( P3DAttribColor, Color2.R, Color2.G, Color2.B, Color2.A );
  glVertexAttrib2f( P3DAttribPosition, p1.X + LineDir.y, p1.Y - LineDir.x );
  glVertexAttrib2f( P3DAttribPosition, p2.X + LineDir.y, p2.Y - LineDir.x );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas2D.RenderLines(points: array of TVec2; Color: TP3DColorRGBA);
var
  i: Integer;
begin
  if ( not PrepareForDrawing( Material )) then exit;

  glBegin( GL_LINE_STRIP );
  for i:= low( points ) to high( points ) do
    begin
      glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
      glVertexAttrib2f( P3DAttribPosition, points[ i ].X, points[ i ].Y );
    end;
  glEnd(); cle( Self );
end;

procedure TP3DCanvas2D.RenderLines(points: TVec2List; Color: TP3DColorRGBA);
var
  i: Integer;
begin
  if ( not PrepareForDrawing( Material )) then exit;

  glBegin( GL_LINE_STRIP );
  for i:= 0 to points.Count - 1 do
    begin
      glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
      glVertexAttrib2f( P3DAttribPosition, points[ i ].X, points[ i ].Y );
    end;
  glEnd(); cle( Self );
end;

procedure TP3DCanvas2D.RenderLineRect(p1, p2: TVec2; c1, c2, c3, c4: TP3DColorRGBA);
begin
  PrepareForDrawing( Material );
  //TODO: Finish RenderLineRect function for Canvas
end;

procedure TP3DCanvas2D.RenderLineRect(p1, p2: TVec2; Color: TP3DColorRGBA);
begin
  RenderLines([ p1, vec2( p2.x, p1.y ), p2, vec2( p1.x, p2.y ), p1 ], Color );
end;

procedure TP3DCanvas2D.RenderRect(p1, p2: TVec2; c1, c2, c3, c4: TP3DColorRGBA);
begin
  if ( not PrepareForDrawing( Material )) then exit;

  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib4f( P3DAttribColor, c1.R, c1.G, c1.B, c1.A );
  glVertexAttrib2f( P3DAttribPosition, p1.X, p1.Y );
  glVertexAttrib4f( P3DAttribColor, c2.R, c2.G, c2.B, c2.A );
  glVertexAttrib2f( P3DAttribPosition, p2.X, p1.Y );
  glVertexAttrib4f( P3DAttribColor, c3.R, c3.G, c3.B, c3.A );
  glVertexAttrib2f( P3DAttribPosition, p2.X, p2.Y );
  glVertexAttrib4f( P3DAttribColor, c4.R, c4.G, c4.B, c4.A );
  glVertexAttrib2f( P3DAttribPosition, p1.X, p2.Y );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas2D.RenderRect(p1, p2: TVec2; Color: TP3DColorRGBA);
begin
  if ( not PrepareForDrawing( Material )) then exit;

  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
  glVertexAttrib2f( P3DAttribPosition, p1.X, p1.Y );
  glVertexAttrib2f( P3DAttribPosition, p2.X, p1.Y );
  glVertexAttrib2f( P3DAttribPosition, p2.X, p2.Y );
  glVertexAttrib2f( P3DAttribPosition, p1.X, p2.Y );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas2D.RenderCircle(c: TVec2; r: Single; num_segments: Integer;
  Color: TP3DColorRGBA);
var
  theta, co, si, t: Single;
  p: TVec2;
  i: Integer;
begin
  if ( not PrepareForDrawing( Material )) then exit;

  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A ); cle( Self );
  theta:= 2 * PI / num_segments;
  co:= cos( theta );//precalculate the sine and cosine
  si:= sin( theta );

  p:= vec2( r, 0 );//we start at angle = 0

  glBegin(GL_TRIANGLE_FAN);
  glVertexAttrib2f( P3DAttribPosition, c.x, c.y );//start with center
  for i:= 0 to num_segments do
    begin
      glVertexAttrib2f( P3DAttribPosition, p.x + c.x, p.y + c.y);//output vertex

		//apply the rotation matrix
      t:= p.x;
      p:= vec2( co * p.x - si * p.y, si * t + co * p.y );
    end;
  glEnd(); cle( Self );
end;

procedure TP3DCanvas2D.RenderRoundRect(p1, p2: TVec2; r: Float; num_segments: Integer; Color: TP3DColorRGBA);
begin
  RenderArc( p1 + r, r, 180, 270, num_segments, Color );
  RenderArc( vec2( p2.x - r, p1.y + r ), r, 270, 360, num_segments, Color );
  RenderArc( p2 - r, r, 0, 90, num_segments, Color );
  RenderArc( vec2( p1.x + r, p2.y - r ), r, 90, 180, num_segments, Color );
  RenderRect( vec2( p1.x + r, p1.y ), vec2( p2.x - r, p2.y ), Color );
  RenderRect( vec2( p1.x, p1.y + r ), vec2( p1.x + r, p2.y - r ), Color );
  RenderRect( vec2( p2.x - r, p1.y + r ), vec2( p2.x, p2.y - r ), Color );
end;

procedure TP3DCanvas2D.RenderArc(c: TVec2; r: Single; startangle, endangle: Float;
  num_segments: Integer; Color: TP3DColorRGBA);
var
  theta, co, si, t: Single;
  p: TVec2;
  i: Integer;
  num_segments_draw: Integer;
  sa, ea: Float;
begin
  if ( not PrepareForDrawing( Material )) then exit;

  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A ); cle( Self );
  theta:= 2 * PI / num_segments;

  sa:= Min( startangle, endangle );
  ea:= Max( startangle, endangle );
  co:= cos( sa * deg2rad );
  si:= sin( sa * deg2rad );
  p:= vec2( co * r, si * r );
  //we start at the start angle

  co:= cos( theta );//precalculate the sine and cosine
  si:= sin( theta );

  num_segments:= num_segments div 4 * 4;

  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib2f( P3DAttribPosition, c.x, c.y );//start with center
  glVertexAttrib2f( P3DAttribPosition, p.x + c.x, p.y + c.y );//add first point
  num_segments_draw:= round( System.int(( ea - sa ) * deg2rad / theta ));

  for i:= 0 to num_segments_draw do
    begin
      glVertexAttrib2f( P3DAttribPosition, p.x + c.x, p.y + c.y );//output vertex

		//apply the rotation matrix
      t:= p.x;
      p:= vec2( co * p.x - si * p.y, si * t + co * p.y );
    end;

  co:= cos( ea * deg2rad );
  si:= sin( ea * deg2rad );
  p:= vec2( co * r, si * r );

  glVertexAttrib2f( P3DAttribPosition, p.x + c.x, p.y + c.y );//add last point

  glEnd(); cle( Self );
end;

procedure TP3DCanvas2D.RenderLineCircle(c: TVec2; r: Single;
  num_segments: Integer; Color: TP3DColorRGBA);
var
  theta, co, si, t: Single;
  p: TVec2;
  i: Integer;
  VBO: TP3DVec2BufferGL;
begin
  if ( not PrepareForDrawing( Material )) then exit;

  VBO:= TP3DVec2BufferGL.Create;

  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A ); cle( Self );
  theta:= 2 * PI / num_segments;
  co:= cos( theta );//precalculate the sine and cosine
  si:= sin( theta );

  p:= vec2( r, 0 );//we start at angle = 0

  for i:= 0 to num_segments - 1 do
    begin
      VBO.Add( vec2( p.x + c.x, p.y + c.y ));//output vertex

		//apply the rotation matrix
      t:= p.x;
      p:= vec2( co * p.x - si * p.y, si * t + co * p.y );
    end;
  VBO.PushData;
  VBO.SetAttribArray( P3DAttribPosition );
  glDrawArrays( GL_LINE_LOOP, 0, VBO.Count ); cle( Self );
  VBO.UnsetAttribArray();
  VBO.Free;
end;

procedure TP3DCanvas2D.RenderQuad(p1, p2, p3, p4: TVec2; c1, c2, c3, c4: TP3DColorRGBA);
begin
  if ( not PrepareForDrawing( Material )) then exit;

  glBegin( GL_TRIANGLE_STRIP );
  glVertexAttrib4f( P3DAttribColor, c1.R, c1.G, c1.B, c1.A );
  glVertexAttrib2f( P3DAttribPosition, p1.x, p1.Y );
  glVertexAttrib4f( P3DAttribColor, c2.R, c2.G, c2.B, c2.A );
  glVertexAttrib2f( P3DAttribPosition, p2.X, p2.Y );
  glVertexAttrib4f( P3DAttribColor, c3.R, c3.G, c3.B, c3.A );
  glVertexAttrib2f( P3DAttribPosition, p3.X, p3.Y );
  glVertexAttrib4f( P3DAttribColor, c4.R, c4.G, c4.B, c4.A );
  glVertexAttrib2f( P3DAttribPosition, p4.X, p4.Y );
  glEnd();
  cle( Self );
end;

procedure TP3DCanvas2D.RenderQuad(p1, p2, p3, p4: TVec2; Color: TP3DColorRGBA);
begin
  if ( not PrepareForDrawing( Material )) then exit;

  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib2f( P3DAttribPosition, p1.x, p1.Y );
  glVertexAttrib2f( P3DAttribPosition, p2.X, p2.Y );
  glVertexAttrib2f( P3DAttribPosition, p3.X, p3.Y );
  glVertexAttrib2f( P3DAttribPosition, p4.X, p4.Y );
  glEnd();
  cle( Self );
end;

procedure TP3DCanvas2D.RenderText(Text: TP3DText; P: TVec2);
begin
  if ( not PrepareForDrawing( Material )) then exit;
  if ( Assigned( Text )) then
    Text.Render( P, ScreenColor * Font.Color, Proj );
end;

procedure TP3DCanvas2D.RenderText( Text: String; P: TVec2; var CacheObj: TP3DText );
begin
  if ( Text = '' ) then
    exit;

  GetValidText( Text, CacheObj );
  RenderText( CacheObj, P );
end;

procedure TP3DCanvas2D.RenderText(Text: TP3DTextBmp; P: TVec2);
var
  decl: TP3DShaderDecl;
begin
  if ( not PrepareForDrawing( MaterialText )) then exit;
  if ( not Assigned( Text )) then exit;

  if ( P3DMaterialActive.Shader.Uniforms.FindByName( 'sdf_basealpha', decl )) then
    decl.AsFloat:= 1.0;
  if ( P3DMaterialActive.Shader.Uniforms.FindByName( 'sdf_smoothing', decl )) then
    decl.AsFloat:= 1/256;
  if ( P3DMaterialActive.Shader.Uniforms.FindByName( 'sdf_bias', decl )) then
    decl.AsFloat:= -0.02;

  {P3DFontManagerBmp.TextModifier.Reset;
  P3DFontManagerBmp.TextModifier.TextAlpha:= 1.0;}
  Text.Render( P, ScreenColor * Font.Color, View );
end;

procedure TP3DCanvas2D.RenderText(Text: String; P: TVec2;
  var CacheObj: TP3DTextBmp);
begin
  if ( Text = '' ) then
    exit;

  GetValidText( Text, CacheObj );
  RenderText( CacheObj, P );
end;

procedure TP3DCanvas2D.RenderTextShadow(Text: TP3DTextBmp; P: TVec2; blur: Single; Color: TP3DColorRGBA);
var
  decl: TP3DShaderDecl;
begin
  if ( not PrepareForDrawing( MaterialText )) then exit;
  if ( not Assigned( Text )) then exit;

  if ( P3DMaterialActive.Shader.Uniforms.FindByName( 'sdf_basealpha', decl )) then
    decl.AsFloat:= ScreenColor.a * Color.A;
  if ( P3DMaterialActive.Shader.Uniforms.FindByName( 'sdf_smoothing', decl )) then
    decl.AsFloat:= min( 0.4, blur );
  if ( P3DMaterialActive.Shader.Uniforms.FindByName( 'sdf_bias', decl )) then
    decl.AsFloat:= -0.02;

  Text.Render( P, ScreenColor * Color, View );
end;

procedure TP3DCanvas2D.RenderTextShadow(Text: String; P: TVec2; blur: Single; Color: TP3DColorRGBA; var CacheObj: TP3DTextBmp);
begin
  if ( Text = '' ) then
    exit;

  GetValidText( Text, CacheObj );
  RenderTextShadow( CacheObj, P, blur, Color );
end;

procedure TP3DCanvas2D.RenderLetter(Text: TP3DText; Idx: Integer; P: TVec2);
begin
  exit;
  if ( not PrepareForDrawing( MaterialText )) then exit;
  if ( Assigned( Text )) then
    Text.RenderLetter( P, Idx, ScreenColor * Font.Color, Proj );
end;

procedure TP3DCanvas2D.RenderBezier(p1, p2, p3, p4: TVec2;
  num_segments: Integer; Color: TP3DColorRGBA);
  function bezier( t, P0, P1, P2, P3: Single ): Float;
  begin
    // Cubic bezier Curve
    Result:= (power((1-t), 3.0) * P0) +
      (3 * power((1-t),2) * t * P1) +
      (3 * (1-t) * t * t * P2) +
      (power(t, 3) * P3);
  end;

var
  i: Integer;
begin
  if ( num_segments <= 0 ) then
    exit;
  if ( not Assigned( Material )) then
    exit;

  PassToShader( Material );

  glBegin( GL_LINE_STRIP );
  for i:= 0 to num_segments do
    begin
      glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
      glVertexAttrib2f( P3DAttribPosition, bezier( i / num_segments, p1.x, p2.x, p3.x, p4.x ), bezier( i / num_segments, p1.y, p2.y, p3.y, p4.y ));
    end;
  glEnd(); cle( Self );
end;

procedure TP3DCanvas2D.RenderRectShadow(p1, p2: TVec2; blur: Single;
  AColor: TP3DColorRGBA);
var
  c0: TP3DColorRGBA;
begin
  c0:= vec4( AColor.xyz, 0 );

  RenderQuad(
    vec2( p1.x - blur, p1.y - blur ),
    vec2( p1.x - blur, p2.y + blur ),
    vec2( p1.x, p1.y ),
    vec2( p1.x, p2.y ),
    c0, c0, AColor, AColor );
  RenderQuad(
    vec2( p1.x - blur, p1.y - blur ),
    vec2( p1.x, p1.y ),
    vec2( p2.x + blur, p1.y - blur ),
    vec2( p2.x, p1.y ),
    c0, AColor, c0, AColor );
  RenderQuad(
    vec2( p1.x, p2.y ),
    vec2( p1.x - blur, p2.y + blur ),
    vec2( p2.x, p2.y ),
    vec2( p2.x + blur, p2.y + blur ),
    AColor, c0, AColor, c0 );
  RenderQuad(
    vec2( p2.x, p1.y ),
    vec2( p2.x, p2.y ),
    vec2( p2.x + blur, p1.y - blur ),
    vec2( p2.x + blur, p2.y + blur ),
    AColor, AColor, c0, c0 );

  RenderRect( p1, p2, AColor );
end;

procedure TP3DCanvas2D.RenderRectShadowInner(p1, p2: TVec2; off, blur: Single;
  AColor: TP3DColorRGBA);
var
  c0: TP3DColorRGBA;
begin
  c0:= vec4( 0, 0, 0, 0 );

  RenderQuad(
    vec2( p1.x + off, p1.y + off ),
    vec2( p1.x + off + blur, p1.y + off + blur ),
    vec2( p1.x + off, p2.y ),
    vec2( p1.x + off + blur, p2.y ),
    AColor, c0, AColor, c0 );
  RenderQuad(
    vec2( p1.x + off, p1.y + off ),
    vec2( p2.x, p1.y + off ),
    vec2( p1.x + off + blur, p1.y + off + blur ),
    vec2( p2.x, p1.y + off + blur ),
    AColor, AColor, c0, c0 );

  RenderRect( p1, vec2( p1.X + off, p2.Y ), AColor );
  RenderRect( vec2( p1.X + off, p1.Y ), vec2( p2.X, p1.Y + off ), AColor );
end;

function TP3DCanvas2D.GetValidText(Text: String;
  var CacheObj: TP3DText): TP3DText;
begin
  Result:= GetValidText( Text, CacheObj, P3DFontManager[ Font.Name, Font.Size ]);
end;

function TP3DCanvas2D.GetValidText(Text: String;
  var CacheObj: TP3DText; AFont: TP3DFont): TP3DText;
begin
  if ( Text = '' ) then
    Text:= ' ';

  if (( not Assigned( CacheObj )) or ( CacheObj.Text <> Text ) or ( CacheObj.Font <> AFont )) then
    begin
      FreeAndNil( CacheObj );
      CacheObj:= p3dTextSimple( Text, AFont );
    end;
  Result:= CacheObj;
end;

function TP3DCanvas2D.GetValidText(Text: String; var CacheObj: TP3DTextBmp;
  AFont: TP3DFont): TP3DTextBmp;
begin
  if ( Text = '' ) then
    Text:= ' ';

  if (( not Assigned( CacheObj )) or ( CacheObj.Text <> Text )) then
    begin
      FreeAndNil( CacheObj );
      CacheObj:= TP3DTextBmp.CreateSimple( Text, P3DFontManagerBmp.FontsByP3DFont[ AFont ], AFont.Size );
    end;
  Result:= CacheObj;
end;

function TP3DCanvas2D.GetValidText(Text: String; var CacheObj: TP3DTextBmp ): TP3DTextBmp;
begin
  Result:= GetValidText( Text, CacheObj, P3DFontManager[ Font.Name, Font.Size ]);
end;

function TP3DCanvas2D.CalcAlignedTextRect(Text: TP3DText; R: TP3DRect; HAlignment: TP3DHorizontalAlignment; VAlignment: TP3DVerticalAlignment): TVec2;
begin
  case HAlignment of
    haLeft: Result.x:= R.Left;
    haRight: Result.x:= R.Left + R.Width - Text.Width;
    haCenter: Result.x:= R.Left + ( R.Width - Text.Width ) / 2;
  end;
  case VAlignment of
    vaTop: Result.y:= R.Top;
    vaBottom: Result.y:= R.Top + R.Height - Text.Height;
    vaCenter: Result.y:= R.Top + ( R.Height - Text.Height ) / 2;
  end;
end;

function TP3DCanvas2D.CalcAlignedTextRect(Text: TP3DTextBmp; R: TP3DRect; HAlignment: TP3DHorizontalAlignment; VAlignment: TP3DVerticalAlignment): TVec2;
begin
  case HAlignment of
    haLeft: Result.x:= R.Left;
    haRight: Result.x:= R.Left + R.Width - Text.Width;
    haCenter: Result.x:= R.Left + ( R.Width - Text.Width ) / 2;
  end;
  case VAlignment of
    vaTop: Result.y:= R.Top;
    vaBottom: Result.y:= R.Top + R.Height - Text.Height;
    vaCenter: Result.y:= R.Top + ( R.Height - Text.Height ) / 2;
  end;
end;

procedure TP3DCanvas2D.PassToShader(Mat: TP3DMaterial);
var
  Uniform: TP3DShaderDecl;
  u: TP3DNodeClone;
begin
  if ( Assigned( Mat )) then
    begin
      if ( Assigned( Mat.Shader )) then
        begin
          if ( Mat.Shader <> P3DShaderActive ) then
            Mat.Shader.Enable;
          if ( Mat.Shader.Uniforms.FindByName( 'proj', Uniform )) then
            Uniform.AsMat4:= Proj;
          if ( Mat.Shader.Uniforms.FindByName( 'view', Uniform )) then
            Uniform.AsMat4:= View;
          if ( Mat.Shader.Uniforms.FindByName( 'world', Uniform )) then
            Uniform.AsMat4:= Mat4Identity;
          if ( Mat is TP3DMaterialNode ) then
            begin
              u:= TP3DMaterialNode( Mat ).FindUniform( 'ClippingRect' );
              if ( Assigned( u )) then
                u.FindInput( 'Value' ).ValueProp.AsString:= vec4( TopLeft, BottomRight ).ToString();
            end
          else if ( Mat.Shader.Uniforms.FindByName( 'ClippingRect', Uniform )) then
            Uniform.AsVec4:= vec4( vec2( 0 ) - TopLeft, vec2( 0 ) - BottomRight );
        end;
      Mat.PassToShader();
      //WriteLn( Mat.Shader.DumpUniforms );
    end;
end;

function TP3DCanvas2D.Lock: Integer;
begin
  Result:=0;
  UpdateProj;
  UpdateParams;
end;

procedure TP3DCanvas2D.Unlock();
begin

end;

procedure TP3DCanvas2D.UpdateLockedArea;
begin

end;

procedure TP3DCanvas2D.SetScreenAligned(AValue: Boolean);
begin
  if FScreenAligned=AValue then Exit;
  FScreenAligned:=AValue;
  UpdateProj;
end;

procedure TP3DCanvas2D.SetScroll(AValue: TVec2);
begin
  if FScroll=AValue then Exit;
  FScroll:=AValue;
  UpdateProj;
end;

procedure TP3DCanvas2D.UpdateProj;
var
  _TopLeft, _WidthHeight: TVec2;
  u: TP3DNodeClone;
begin
  if ( ScreenAligned ) then
    _TopLeft:= FScroll
  else
    _TopLeft:= FScroll + TopLeft {+ vec2( P3DViewports.Peek.Left, P3DViewports.Peek.Top + P3DViewports.Peek.Height - P3DViewports.Screen.Height )};

  _WidthHeight:= vec2( P3DViewports.Peek.Width, P3DViewports.Peek.Height );
  Proj:=  mat4orthoRH( 0, _WidthHeight.X, 0, _WidthHeight.Y, 0, 1 );
  FView:= mat4translate( vec3( _TopLeft, 0 )) * mat4( Zoom );
  PassToShader( MaterialText );
  PassToShader( MaterialTexture );
  PassToShader( Material );
  //WriteLn( Material.Shader.DumpUniforms );
end;

procedure TP3DCanvas2D.UpdateParams;
begin
  glLineWidth( Pen.Width ); cle( Self );
  if ( Pen.Style = psSolid ) then
    glDisable( GL_LINE_STIPPLE )
  else
    glEnable( GL_LINE_STIPPLE );
  cle( Self );

  case Pen.Style of
    psStippled1: begin glLineStipple( 1, $AAAA ); cle( Self ) end;
    psStippled2: begin glLineStipple( 2, $AAAA ); cle( Self ) end;
    psStippled3: begin glLineStipple( 3, $AAAA ); cle( Self ) end;
    psStippled4: begin glLineStipple( 4, $AAAA ); cle( Self ) end;
    psStippled5: begin glLineStipple( 1, $FF ); cle( Self ) end;
    psStippled6: begin glLineStipple( 2, $FF ); cle( Self ) end;
  end;
end;

procedure TP3DCanvas2D.PenChange(Sender: TObject);
begin
  UpdateParams;
end;

function TP3DCanvas2D.PrepareForDrawing( Mat: TP3DMaterial ): Boolean;
begin
  Result:= False;
  {$IFDEF CHECKFORERRORS}CheckDrawingAllowed;{$ENDIF}
  if( Assigned( Mat )) then
    begin
      Result:= True;
      Mat.PassToShader();
      //PassToShader( Mat );
    end;
end;

constructor TP3DCanvas2D.Create(AOwner: TObject);
begin
  inherited Create( AOwner );
  FPen:= TP3DPen.Create( vec4( 0, 0, 0, 1 ), 1.0, psSolid );
  FPen.OnChange:= @PenChange;
  FBrush:= TP3DBrush.Create( vec4( 1 ));
  FProj:= Mat4Identity;
  FView:= Mat4Identity;
  FZoom:= 1.0;
end;

destructor TP3DCanvas2D.Destroy;
begin
  Pen.Free;
  Brush.Free;
  inherited Destroy;
end;


{$ENDIF}
