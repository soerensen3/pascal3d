{$IFDEF INTERFACE}

{ TP3DCanvas2D }

TP3DCanvas2D = class ( TP3DCanvas )
  private
    FProj: TMat4;
    FScreenAligned: Boolean;
    FScroll: TVec2;

    procedure SetScreenAligned(AValue: Boolean);
    procedure SetScroll(AValue: TVec2);
    procedure UpdateLockedArea; override;
    procedure UpdateProj;

  public
    procedure RenderLine( p1, p2: TVec2; Color: TVec4 );
    procedure RenderLine( p1, p2: TVec2; Color1, Color2: TVec4 );
    procedure RenderLines( points: array of TVec2; Color: TVec4 );
    procedure RenderLines( points: TVec2List; Color: TVec4 );
    procedure RenderLineRect( p1, p2: TVec2; c1, c2, c3, c4: TVec4 );
    procedure RenderLineRect( p1, p2: TVec2; Color: TVec4 );
    procedure RenderRect( p1, p2: TVec2; c1, c2, c3, c4: TVec4 );
    procedure RenderRect( p1, p2: TVec2; Color: TVec4 );
    procedure RenderQuad( p1, p2, p3, p4: TVec2; c1, c2, c3, c4: TVec4 );
    procedure RenderQuad( p1, p2, p3, p4: TVec2; Color: TVec4 );
    procedure RenderText( Text: TP3DText; P: TVec2 );

    function Lock: Integer; override;

    property Proj: TMat4 read FProj;
    property Scroll: TVec2 read FScroll write SetScroll;
    property ScreenAligned: Boolean read FScreenAligned write SetScreenAligned;
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

function TP3DCanvas2D.Lock: Integer;
begin
  Result:=inherited Lock;
  UpdateProj;
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

{$ENDIF}
