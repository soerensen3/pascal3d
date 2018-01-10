{$IFDEF INTERFACE}


{ TP3DBrush }
type
  TP3DBrush = class
    Color: TP3DColorRGBA;
    constructor Create(AColor: TP3DColorRGBA);
  end;

  TP3DPenStyle = ( psSolid, psStippled1, psStippled2, psStippled3, psStippled4, psStippled5, psStippled6 );

  TP3DHorizontalAlignment = (
    haLeft,
    haCenter,
    haRight);

  TP3DVerticalAlignment = (
    vaTop,
    vaCenter,
    vaBottom);


  { TP3DPen }

  TP3DPen = class
    private
      FColor: TP3DColorRGBA;
      FOnChange: TNotifyEvent;
      FWidth: Single;
      FStyle: TP3DPenStyle;

      procedure SetStyle(AValue: TP3DPenStyle);
      procedure SetWidth(AValue: Single);

    public
      constructor Create( AColor: TP3DColorRGBA; AWidth: Single; AStyle: TP3DPenStyle );

      property Width: Single read FWidth write SetWidth;
      property Color: TP3DColorRGBA read FColor write FColor;
      property Style: TP3DPenStyle read FStyle write SetStyle;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  { TP3DCanvas2D }

  TP3DCanvas2D = class ( TP3DCanvas )
    private
      FBrush: TP3DBrush;
      FPen: TP3DPen;
      FProj: TMat4;
      FScreenAligned: Boolean;
      FScroll: TVec2;
      FWorld: TMat4;

      procedure SetScreenAligned(AValue: Boolean);
      procedure SetScroll(AValue: TVec2);
      procedure UpdateLockedArea; override;
      procedure UpdateProj;
      procedure UpdateParams;
      procedure PenChange( Sender: TObject );

    public
      constructor Create(AOwner: TPersistent);
      destructor Destroy; override;

      function PrepareForDrawing: Boolean;

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
      procedure PassToShader;

      function Lock: Integer; override;

      property Proj: TMat4 read FProj write FProj;
      property Scroll: TVec2 read FScroll write SetScroll;
      property ScreenAligned: Boolean read FScreenAligned write SetScreenAligned;
      property Brush: TP3DBrush read FBrush;
      property Pen: TP3DPen read FPen;
      property World: TMat4 read FWorld write FWorld;
  end;

  function P3DRect( Left, Top, Width, Height: Float ): TP3DRect;
  function P3DRect(TopLeft, WidthHeight: TVec2): TP3DRect;
  function P3DRectEx(Left, Top, Right, Bottom: Float): TP3DRect;
  function P3DRectEx( TopLeft, BottomRight: TVec2 ): TP3DRect;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DCanvas2D }

procedure TP3DCanvas2D.RenderLine(p1, p2: TVec2; Color: TP3DColorRGBA);
var
  LineDir: TVec2;
begin
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;

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
  PrepareForDrawing
  //TODO: Finish RenderLineRect function for Canvas
end;

procedure TP3DCanvas2D.RenderLineRect(p1, p2: TVec2; Color: TP3DColorRGBA);
begin
  RenderLines([ p1, vec2( p2.x, p1.y ), p2, vec2( p1.x, p2.y ), p1 ], Color );
end;

procedure TP3DCanvas2D.RenderRect(p1, p2: TVec2; c1, c2, c3, c4: TP3DColorRGBA);
begin
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;

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
  if ( not PrepareForDrawing ) then exit;
  if ( Assigned( Text )) then
    Text.Render( P, ScreenColor * Font.Color, World * Proj );
end;

procedure TP3DCanvas2D.RenderText( Text: String; P: TVec2; var CacheObj: TP3DText );
begin
  if ( Text = '' ) then
    exit;

  GetValidText( Text, CacheObj );
  RenderText( CacheObj, P );
end;

procedure TP3DCanvas2D.RenderText(Text: TP3DTextBmp; P: TVec2);
begin
  if ( not PrepareForDrawing ) then exit;

  P3DFontManagerBmp.TextModifier.Reset;
  P3DFontManagerBmp.TextModifier.TextAlpha:= 1.0;
  if ( Assigned( Text )) then
    Text.Render( P, ScreenColor * Font.Color, World * Proj );
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
begin
  if ( not PrepareForDrawing ) then exit;
  if ( not Assigned( Text )) then exit;

  P3DFontManagerBmp.TextModifier.Reset;
  P3DFontManagerBmp.TextModifier.TextSmoothing:= blur;
  P3DFontManagerBmp.TextModifier.TextBias:= -0.05;
  P3DFontManagerBmp.TextModifier.TextAlpha:= ScreenColor.a * Color.a;
  Text.Render( P, ScreenColor * Color, World * Proj );
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
  if ( not PrepareForDrawing ) then exit;
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

  PassToShader();

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

procedure TP3DCanvas2D.PassToShader;
var
  Uniform: TP3DShaderDecl;
begin
  if ( Assigned( Material )) then
    begin
      Material.PassToShader();
      if ( Assigned( Material.Shader )) then
        begin
          if ( Material.Shader.Uniforms.FindByName( 'proj', Uniform )) then
            Uniform.AsMat4:= world * proj;
          if ( Material.Shader.Uniforms.FindByName( 'view', Uniform )) then
            Uniform.AsMat4:= Mat4Identity;
          if ( Material.Shader.Uniforms.FindByName( 'world', Uniform )) then
            Uniform.AsMat4:= Mat4Identity;
        end;
    end;
end;

function TP3DCanvas2D.Lock: Integer;
begin
  Result:=inherited Lock;
  UpdateProj;
  UpdateParams;
end;

procedure TP3DCanvas2D.UpdateLockedArea;
begin
  inherited UpdateLockedArea;
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
  NewProj: TMat4;
  Uniform: TP3DShaderDecl;
begin
  if ( ScreenAligned ) then
    NewProj:= mat4orthoRH( FScroll.x + Left, FScroll.x + Left + Width, FScroll.y +Top, FScroll.y + Top + Height )
  else
    NewProj:= mat4orthoRH( FScroll.x, FScroll.x + Width, FScroll.y, FScroll.y + Height );
  if ( Assigned( Material ) and Assigned( Material.Shader )) then
    begin
      if ( Material.Shader <> P3DShaderActive ) then
        Material.Shader.Enable;
      FProj:= NewProj;
      if ( Material.Shader.Uniforms.FindByName( 'proj', Uniform )) then
        Uniform.AsMat4:= proj;
    end;
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

function TP3DCanvas2D.PrepareForDrawing: Boolean;
begin
  Result:= False;
  {$IFDEF CHECKFORERRORS}CheckDrawingAllowed;{$ENDIF}
  if( Assigned( Material )) then
    begin
      Result:= True;
      PassToShader();
    end;
end;

constructor TP3DCanvas2D.Create(AOwner: TPersistent);
begin
  inherited Create( AOwner );
  FPen:= TP3DPen.Create( vec4( 0, 0, 0, 1 ), 1.0, psSolid );
  FPen.OnChange:= @PenChange;
  FBrush:= TP3DBrush.Create( vec4( 1 ));
  World:= Mat4Identity;
end;

destructor TP3DCanvas2D.Destroy;
begin
  Pen.Free;
  Brush.Free;
  inherited Destroy;
end;

{ TP3DRect }

function TP3DRect.GetBottomRight: TVec2;
begin
  Result:= TopLeft + vec2( Width, Height );
end;

function TP3DRect.GetHeightI: Integer;
begin
  Result:= round( Height );
end;

function TP3DRect.GetLeftI: Integer;
begin
  Result:= round( Left );
end;

function TP3DRect.GetBottom: Float;
begin
  Result:= TopLeft.y + Height;
end;

function TP3DRect.GetBottomI: Integer;
begin
  Result:= round( Bottom );
end;

function TP3DRect.GetRight: Float;
begin
  Result:= TopLeft.x + Width;
end;

function TP3DRect.GetRightI: Integer;
begin
  Result:= round( Right );
end;

function TP3DRect.GetTopI: Integer;
begin
  Result:= round( Top );
end;

function TP3DRect.GetTopLeft: TVec2;
begin
  Result:= vec2( Left, Top );
end;

function TP3DRect.GetWidthHeight: TVec2;
begin
  Result:= vec2( Width, Height );
end;

function TP3DRect.GetWidthI: Integer;
begin
  Result:= round( Width );
end;

procedure TP3DRect.SetBottom(AValue: Float);
begin
  Height:= Max( 0, AValue - Top );
end;


procedure TP3DRect.SetBottomRight(AValue: TVec2);
begin
  Width:= Max( 0, AValue.x - Left );
  Height:= Max( 0, AValue.y - Top );
end;


procedure TP3DRect.SetRight(AValue: Float);
begin
  Width:= Max( 0, AValue - Left );
end;


procedure TP3DRect.SetTopLeft(AValue: TVec2);
begin
  Left:= AValue.X;
  Top:= AValue.Y;
end;

procedure TP3DRect.SetWidthHeight(AValue: TVec2);
begin
  Width:= AValue.X;
  Height:= AValue.Y;
end;


function TP3DRect.PtInRect(P: TVec2): Boolean;
begin
  Result:= ( P.x >= Left ) and ( P.x < Right )
       and ( P.Y >= Top )  and ( P.y < Bottom );
end;

function TP3DRect.RectCollision(R: TP3DRect): Boolean;
begin
  Result:= ( Left <= R.Right ) and
           ( Right >= R.Left ) and
           ( Top <= R.Bottom ) and
           ( Bottom >= R.Top );
end;

function TP3DRect.Clip(R: TP3DRect): TP3DRect;
begin
  Result.TopLeft:= Max( vec2( 0 ), Max( R.TopLeft, TopLeft ));
  Result.BottomRight:= Max( Result.TopLeft, Min( R.BottomRight, BottomRight ));
end;

function TP3DRect.Inset(ATopLeft, ABottomRight: TVec2): TP3DRect;
begin
  Result.TopLeft:= TopLeft + ATopLeft;
  Result.BottomRight:= BottomRight - ABottomRight;
end;

function TP3DRect.ToString: String;
begin
  Result:= TopLeft.ToString() + ', ' + BottomRight.ToString();
end;

operator+(R: TP3DRect; v: TVec2): TP3DRect;
begin
  Result:= P3DRect( R.TopLeft + v, R.WidthHeight );
end;

function P3DRect( Left, Top, Width, Height: Float ): TP3DRect;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Width:= Max( 0, Width );
  Result.Height:= Max( 0, Height );
end;

function P3DRect(TopLeft, WidthHeight: TVec2): TP3DRect;
begin
  Result.TopLeft:= TopLeft;
  Result.WidthHeight:= WidthHeight;
end;

function P3DRectEx(Left, Top, Right, Bottom: Float): TP3DRect;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Right:= Right;
  Result.Bottom:= Bottom;
end;

function P3DRectEx(TopLeft, BottomRight: TVec2): TP3DRect;
begin
  Result.TopLeft:= TopLeft;
  Result.BottomRight:= BottomRight;
end;

{$ENDIF}
