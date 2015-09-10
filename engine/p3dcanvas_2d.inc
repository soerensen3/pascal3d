{$IFDEF INTERFACE}


{ TP3DBrush }

TP3DBrush = class
  Color: TVec4;
  constructor Create(AColor: TVec4);
end;

TP3DPenStyle = ( psSolid, psStippled1, psStippled2, psStippled3, psStippled4, psStippled5, psStippled6 );

{ TP3DPen }

TP3DPen = class
  private
    FColor: TVec4;
    FOnChange: TNotifyEvent;
    FWidth: Single;
    FStyle: TP3DPenStyle;

    procedure SetStyle(AValue: TP3DPenStyle);
    procedure SetWidth(AValue: Single);

  public
    constructor Create( AColor: TVec4; AWidth: Single; AStyle: TP3DPenStyle );

    property Width: Single read FWidth write SetWidth;
    property Color: TVec4 read FColor write FColor;
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

    procedure SetScreenAligned(AValue: Boolean);
    procedure SetScroll(AValue: TVec2);
    procedure UpdateLockedArea; override;
    procedure UpdateProj;
    procedure UpdateParams;
    procedure PenChange( Sender: TObject );

  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;

    procedure RenderLine( p1, p2: TVec2; Color: TVec4 );
    procedure RenderLine( p1, p2: TVec2; Color1, Color2: TVec4 );
    procedure RenderLines( points: array of TVec2; Color: TVec4 );
    procedure RenderLines( points: TVec2List; Color: TVec4 );
    procedure RenderLineRect( p1, p2: TVec2; c1, c2, c3, c4: TVec4 );
    procedure RenderLineRect( p1, p2: TVec2; Color: TVec4 );
    procedure RenderRect( p1, p2: TVec2; c1, c2, c3, c4: TVec4 );
    procedure RenderRect( p1, p2: TVec2; Color: TVec4 );
    procedure RenderCircle(c: TVec2; r: Single; num_segments: Integer; Color: TVec4 );
    procedure RenderLineCircle( c: TVec2; r: Single; num_segments: Integer; Color: TVec4 );
    procedure RenderQuad( p1, p2, p3, p4: TVec2; c1, c2, c3, c4: TVec4 );
    procedure RenderQuad( p1, p2, p3, p4: TVec2; Color: TVec4 );
    procedure RenderText( Text: TP3DText; P: TVec2 );
    procedure RenderBezier( p1, p2, p3, p4: TVec2; num_segments: Integer; Color: TVec4 );
    procedure RenderRectShadow(p1, p2: TVec2; blur: Single; AColor: TVec4);

    function Lock: Integer; override;

    property Proj: TMat4 read FProj;
    property Scroll: TVec2 read FScroll write SetScroll;
    property ScreenAligned: Boolean read FScreenAligned write SetScreenAligned;
    property Brush: TP3DBrush read FBrush;
    property Pen: TP3DPen read FPen;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DCanvas2D }

procedure TP3DCanvas2D.RenderLine(p1, p2: TVec2; Color: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
  glBegin( GL_LINES );
  glVertexAttrib2f( vt, p1.X, p1.Y );
  glVertex2f( p2.X, p2.Y );
  glEnd();
end;

procedure TP3DCanvas2D.RenderLine(p1, p2: TVec2; Color1, Color2: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glBegin( GL_LINES );
  glVertexAttrib4f( cl, Color1.R, Color1.G, Color1.B, Color1.A );
  glVertexAttrib2f( vt, p1.X, p1.Y );
  glVertexAttrib4f( cl, Color2.R, Color2.G, Color2.B, Color2.A );
  glVertexAttrib2f( vt, p2.X, p2.Y );
  glEnd();
end;

procedure TP3DCanvas2D.RenderLines(points: array of TVec2; Color: TVec4);
var
  cl: GLint;
  vt: GLint;
  i: Integer;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glBegin( GL_LINE_STRIP );
  for i:= low( points ) to high( points ) do
    begin
      glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
      glVertexAttrib2f( vt, points[ i ].X, points[ i ].Y );
    end;
  glEnd();
end;

procedure TP3DCanvas2D.RenderLines(points: TVec2List; Color: TVec4);
var
  cl: GLint;
  vt: GLint;
  i: Integer;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glBegin( GL_LINE_STRIP );
  for i:= 0 to points.Count - 1 do
    begin
      glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
      glVertexAttrib2f( vt, points[ i ].X, points[ i ].Y );
    end;
  glEnd();
end;

procedure TP3DCanvas2D.RenderLineRect(p1, p2: TVec2; c1, c2, c3, c4: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );
  //TODO: Finish RenderLineRect function for Canvas
end;

procedure TP3DCanvas2D.RenderLineRect(p1, p2: TVec2; Color: TVec4);
begin
  RenderLines([ p1, vec2( p2.x, p1.y ), p2, vec2( p1.x, p2.y ), p1 ], Color );
end;

procedure TP3DCanvas2D.RenderRect(p1, p2: TVec2; c1, c2, c3, c4: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib4f( cl, c1.R, c1.G, c1.B, c1.A );
  glVertexAttrib2f( vt, p1.X, p1.Y );
  glVertexAttrib4f( cl, c2.R, c2.G, c2.B, c2.A );
  glVertexAttrib2f( vt, p2.X, p1.Y );
  glVertexAttrib4f( cl, c3.R, c3.G, c3.B, c3.A );
  glVertexAttrib2f( vt, p2.X, p2.Y );
  glVertexAttrib4f( cl, c4.R, c4.G, c4.B, c4.A );
  glVertexAttrib2f( vt, p1.X, p2.Y );
  glEnd();
end;

procedure TP3DCanvas2D.RenderRect(p1, p2: TVec2; Color: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib2f( vt, p1.X, p1.Y );
  glVertexAttrib2f( vt, p2.X, p1.Y );
  glVertexAttrib2f( vt, p2.X, p2.Y );
  glVertexAttrib2f( vt, p1.X, p2.Y );
  glEnd();
end;

procedure TP3DCanvas2D.RenderCircle(c: TVec2; r: Single; num_segments: Integer;
  Color: TVec4);
var
  cl: GLint;
  vt: GLint;

  theta, co, si, t: Single;
  p: TVec2;
  i: Integer;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
  theta:= 2 * PI / num_segments;
  co:= cos( theta );//precalculate the sine and cosine
  si:= sin( theta );

  p:= vec2( r, 0 );//we start at angle = 0

  glBegin(GL_TRIANGLE_FAN);
  glVertexAttrib2f( vt, c.x, c.y );//start with center
  for i:= 0 to num_segments do
    begin
      glVertexAttrib2f( vt, p.x + c.x, p.y + c.y);//output vertex

		//apply the rotation matrix
      t:= p.x;
      p:= vec2( co * p.x - si * p.y, si * t + co * p.y );
    end;
  glEnd();
end;

procedure TP3DCanvas2D.RenderLineCircle(c: TVec2; r: Single;
  num_segments: Integer; Color: TVec4);
var
  cl: GLint;
  vt: GLint;

  theta, co, si, t: Single;
  p: TVec2;
  i: Integer;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
  theta:= 2 * PI / num_segments;
  co:= cos( theta );//precalculate the sine and cosine
  si:= sin( theta );

  p:= vec2( r, 0 );//we start at angle = 0

  glBegin(GL_LINE_LOOP);
  for i:= 0 to num_segments - 1 do
    begin
      glVertexAttrib2f( vt, p.x + c.x, p.y + c.y);//output vertex

		//apply the rotation matrix
      t:= p.x;
      p:= vec2( co * p.x - si * p.y, si * t + co * p.y );
    end;
  glEnd();
end;

procedure TP3DCanvas2D.RenderQuad(p1, p2, p3, p4: TVec2; c1, c2, c3, c4: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glBegin( GL_TRIANGLE_STRIP );
  glVertexAttrib4f( cl, c1.R, c1.G, c1.B, c1.A );
  glVertexAttrib2f( vt, p1.x, p1.Y );
  glVertexAttrib4f( cl, c2.R, c2.G, c2.B, c2.A );
  glVertexAttrib2f( vt, p2.X, p2.Y );
  glVertexAttrib4f( cl, c3.R, c3.G, c3.B, c3.A );
  glVertexAttrib2f( vt, p3.X, p3.Y );
  glVertexAttrib4f( cl, c4.R, c4.G, c4.B, c4.A );
  glVertexAttrib2f( vt, p4.X, p4.Y );
  glEnd();
end;

procedure TP3DCanvas2D.RenderQuad(p1, p2, p3, p4: TVec2; Color: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib2f( vt, p1.x, p1.Y );
  glVertexAttrib2f( vt, p2.X, p2.Y );
  glVertexAttrib2f( vt, p3.X, p3.Y );
  glVertexAttrib2f( vt, p4.X, p4.Y );
  glEnd();
end;

procedure TP3DCanvas2D.RenderText(Text: TP3DText; P: TVec2);
begin
  Text.Render( P, ScreenColor * Font.Color, Proj );
end;

procedure TP3DCanvas2D.RenderBezier(p1, p2, p3, p4: TVec2;
  num_segments: Integer; Color: TVec4);
  function bezier( t, P0, P1, P2, P3: Single ): Float;
  begin
    // Cubic bezier Curve
    Result:= (power((1-t), 3.0) * P0) +
      (3 * power((1-t),2) * t * P1) +
      (3 * (1-t) * t * t * P2) +
      (power(t, 3) * P3);
  end;

var
  cl: GLint;
  vt: GLint;
  i: Integer;
begin
  if ( num_segments <= 0 ) then
    exit;
  if ( not Assigned( ActShad )) then
    Exit;
  cl:= ActShad.Attributes.AddrByName( 'color' );
  vt:= ActShad.Attributes.AddrByName( 'vertex' );

  glBegin( GL_LINE_STRIP );
  for i:= 0 to num_segments do
    begin
      glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
      glVertexAttrib2f( vt, bezier( i / num_segments, p1.x, p2.x, p3.x, p4.x ), bezier( i / num_segments, p1.y, p2.y, p3.y, p4.y ));
    end;
  glEnd();
end;

procedure TP3DCanvas2D.RenderRectShadow(p1, p2: TVec2; blur: Single;
  AColor: TVec4);
var
  c0: TVec4;
begin
  c0:= vec4( 0, 0, 0, 0 );

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

function TP3DCanvas2D.Lock: Integer;
begin
  Result:=inherited Lock;
  UpdateProj;
  UpdateParams;
end;

procedure TP3DCanvas2D.UpdateLockedArea;
begin
  inherited UpdateLockedArea;
  UpdateProj;
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
  n: Integer;
  NewProj: TMat4;
begin
  if ( ScreenAligned ) then
    NewProj:= mat4orthoRH( FScroll.x + Left, FScroll.x + Left + Width, FScroll.y +Top, FScroll.y + Top + Height )
  else
    NewProj:= mat4orthoRH( FScroll.x, FScroll.x + Width, FScroll.y, FScroll.y + Height );
  if ({( NewProj <> FProj ) AND }( Assigned( ActShad ))) then
    begin
      FProj:= NewProj;
      n:= ActShad.Uniforms.FindByName( 'proj' );
      if ( n >= 0 ) then
        ActShad.Uniforms[ n ].AsMat4:= Proj;
    end;
end;

procedure TP3DCanvas2D.UpdateParams;
begin
  glLineWidth( Pen.Width );
  if ( Pen.Style = psSolid ) then
    glDisable( GL_LINE_STIPPLE )
  else
    glEnable( GL_LINE_STIPPLE );

  case Pen.Style of
    psStippled1: glLineStipple( 1, $AAAA );
    psStippled2: glLineStipple( 2, $AAAA );
    psStippled3: glLineStipple( 3, $AAAA );
    psStippled4: glLineStipple( 4, $AAAA );
    psStippled5: glLineStipple( 1, $FF );
    psStippled6: glLineStipple( 2, $FF );
  end;
end;

procedure TP3DCanvas2D.PenChange(Sender: TObject);
begin
  UpdateParams;
end;

constructor TP3DCanvas2D.Create(AOwner: TPersistent);
begin
  inherited;
  FPen:= TP3DPen.Create( vec4( 0, 0, 0, 1 ), 1.0, psSolid );
  FPen.OnChange:= @PenChange;
  FBrush:= TP3DBrush.Create( vec4( 1 ));
end;

destructor TP3DCanvas2D.Destroy;
begin
  inherited Destroy;
end;

{$ENDIF}
