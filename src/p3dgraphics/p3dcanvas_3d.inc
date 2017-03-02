{$IFDEF INTERFACE}

type

  { TP3DCanvas3D }

  TP3DCanvas3D = class ( TP3DCanvas )
    private
      FBrush: TP3DBrush;
      FPen: TP3DPen;
      FProj: TMat4;
      FView: TMat4;

      procedure UpdateParams;
      procedure PenChange( Sender: TObject );

    public
      constructor Create(AOwner: TPersistent);
      destructor Destroy; override;

      function PrepareForDrawing: Boolean;

      procedure RenderLine( p1, p2: TVec3; Color: TVec4 );
      procedure RenderLine(p1, p2: TVec3; Color1, Color2: TVec4);
      procedure RenderLines( points: array of TVec3; Color: TVec4 );
      procedure RenderLines( points: TVec3List; Color: TVec4 );
      procedure RenderLineRect( p1, p2: TVec3; c1, c2, c3, c4: TVec4 );
      procedure RenderLineRect( p1, p2: TVec3; Color: TVec4 );
      procedure RenderRect(p1, p2: TVec3; c1, c2, c3, c4: TVec4);
      procedure RenderRect(p1, p2: TVec3; Color: TVec4);
      {procedure RenderCircle(c: TVec2; r: Single; num_segments: Integer; Color: TVec4 );
      procedure RenderRoundRect(p1, p2: TVec2; r: Float; num_segments: Integer;
        Color: TVec4);
      procedure RenderArc(c: TVec2; r: Single; startangle, endangle: Float;
        num_segments: Integer; Color: TVec4);
      procedure RenderLineCircle( c: TVec2; r: Single; num_segments: Integer; Color: TVec4 );
      procedure RenderQuad( p1, p2, p3, p4: TVec2; c1, c2, c3, c4: TVec4 );
      procedure RenderQuad( p1, p2, p3, p4: TVec2; Color: TVec4 );
      procedure RenderText( Text: TP3DText; P: TVec2 );
      procedure RenderText( Text: String; P: TVec2; var CacheObj: TP3DText );
      procedure RenderLetter( Text: TP3DText; Idx: Integer; P: TVec2 );
      procedure RenderBezier( p1, p2, p3, p4: TVec2; num_segments: Integer; Color: TVec4 );
      procedure RenderRectShadow(p1, p2: TVec2; blur: Single; AColor: TVec4);
      procedure RenderRectShadowInner( p1, p2: TVec2; off, blur: Single; AColor: TVec4 );
      function GetValidText( Text: String; var CacheObj: TP3DText ): TP3DText; inline;
      function GetValidText( Text: String; var CacheObj: TP3DText; AFont: TP3DFont ): TP3DText;}
      procedure PassToShader;

      function Lock: Integer; override;

      property Proj: TMat4 read FProj write FProj;
      property View: TMat4 read FView write FView;
      property Brush: TP3DBrush read FBrush;
      property Pen: TP3DPen read FPen;
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DCanvas3D }

procedure TP3DCanvas3D.UpdateParams;
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

procedure TP3DCanvas3D.PenChange(Sender: TObject);
begin
  UpdateParams;
end;

constructor TP3DCanvas3D.Create(AOwner: TPersistent);
begin
  inherited Create( AOwner );
  FPen:= TP3DPen.Create( vec4( 0, 0, 0, 1 ), 1.0, psSolid );
  FPen.OnChange:= @PenChange;
  FBrush:= TP3DBrush.Create( vec4( 1 ));
end;

destructor TP3DCanvas3D.Destroy;
begin
  Pen.Free;
  Brush.Free;
  inherited Destroy;
end;

function TP3DCanvas3D.PrepareForDrawing: Boolean;
begin
  Result:= False;
  {$IFDEF CHECKFORERRORS}CheckDrawingAllowed;{$ENDIF}
  if( Assigned( Material )) then
    begin
      Result:= True;
      PassToShader();
    end;
end;

procedure TP3DCanvas3D.RenderLine(p1, p2: TVec3; Color: TVec4);
begin
  if ( not PrepareForDrawing ) then exit;

  glBegin( GL_LINES );
  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
  glVertexAttrib3f( P3DAttribPosition, p1.X, p1.Y, p1.Z );
  glVertexAttrib3f( P3DAttribPosition, p2.X, p2.Y, p2.Z );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas3D.RenderLine(p1, p2: TVec3; Color1, Color2: TVec4);
begin
  if ( not PrepareForDrawing ) then exit;

  glBegin( GL_LINES );
  glVertexAttrib4f( P3DAttribColor, Color1.R, Color1.G, Color1.B, Color1.A );
  glVertexAttrib3f( P3DAttribPosition, p1.X, p1.Y, p1.Z );
  glVertexAttrib4f( P3DAttribColor, Color2.R, Color2.G, Color2.B, Color2.A );
  glVertexAttrib3f( P3DAttribPosition, p2.X, p2.Y, p2.Z );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas3D.RenderLines(points: array of TVec3; Color: TVec4);
var
  i: Integer;
begin
  if ( not PrepareForDrawing ) then exit;

  glBegin( GL_LINE_STRIP );
  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
  for i:= low( points ) to high( points ) do
    glVertexAttrib3f( P3DAttribPosition, points[ i ].X, points[ i ].Y, points[ i ].Z );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas3D.RenderLines(points: TVec3List; Color: TVec4);
var
  i: Integer;
begin
  if ( not PrepareForDrawing ) then exit;

  glBegin( GL_LINE_STRIP );
  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
  for i:= 0 to points.Count - 1 do
    glVertexAttrib3f( P3DAttribPosition, points[ i ].X, points[ i ].Y, points[ i ].Z );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas3D.RenderLineRect(p1, p2: TVec3; c1, c2, c3, c4: TVec4);
begin

end;

procedure TP3DCanvas3D.RenderLineRect(p1, p2: TVec3; Color: TVec4);
begin
  RenderLines([ p1, vec3( p2.x, p1.y, p1.z ), p2, vec3( p1.x, p2.y, p2.z ), p1 ], Color );
end;

procedure TP3DCanvas3D.RenderRect(p1, p2: TVec3; c1, c2, c3, c4: TVec4);
begin
  if ( not PrepareForDrawing ) then exit;

  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib4f( P3DAttribColor, c1.R, c1.G, c1.B, c1.A );
  glVertexAttrib3f( P3DAttribPosition, p1.X, p1.Y, p1.Z );
  glVertexAttrib4f( P3DAttribColor, c2.R, c2.G, c2.B, c2.A );
  glVertexAttrib3f( P3DAttribPosition, p2.X, p1.Y, p1.Z );
  glVertexAttrib4f( P3DAttribColor, c3.R, c3.G, c3.B, c3.A );
  glVertexAttrib3f( P3DAttribPosition, p2.X, p2.Y, p2.Z );
  glVertexAttrib4f( P3DAttribColor, c4.R, c4.G, c4.B, c4.A );
  glVertexAttrib3f( P3DAttribPosition, p1.X, p2.Y, p2.Z );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas3D.RenderRect( p1, p2: TVec3; Color: TVec4 );
begin
  if ( not PrepareForDrawing ) then exit;
  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A ); cle( Self );
  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib3f( P3DAttribPosition, p1.X, p1.Y, p1.Z );
  glVertexAttrib3f( P3DAttribPosition, p2.X, p1.Y, p1.Z );
  glVertexAttrib3f( P3DAttribPosition, p2.X, p2.Y, p2.Z );
  glVertexAttrib3f( P3DAttribPosition, p1.X, p2.Y, p2.Z );
  glEnd(); cle( Self );
end;

procedure TP3DCanvas3D.PassToShader;
var
  Uniform: TP3DShaderDecl;
begin
  if ( Assigned( Material )) then
    begin
      Material.PassToShader();
      if ( Assigned( Material.Shader )) then
        begin
          if ( Material.Shader.Uniforms.FindByName( 'proj', Uniform )) then
            Uniform.AsMat4:= Proj;
          if ( Material.Shader.Uniforms.FindByName( 'view', Uniform )) then
            Uniform.AsMat4:= View;
          if ( Material.Shader.Uniforms.FindByName( 'world', Uniform )) then
            Uniform.AsMat4:= Mat4Identity;
        end;
    end;
end;

function TP3DCanvas3D.Lock: Integer;
begin
  Result:= inherited Lock;
end;


{$ENDIF}
