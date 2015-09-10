unit p3dgeometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, p3dMath, p3dshaders, dglOpenGL;

procedure RenderLine2D( p1, p2: TVec2; Color: TVec4; const Thickness: Single = 1 );
procedure RenderLine2D( p1, p2: TVec2; Color1, Color2: TVec4; const Thickness: Single = 1 );
procedure RenderLines2D( points: array of TVec2; Color: TVec4; const Thickness: Single = 1 );
procedure RenderLines2D( points: TVec2List; Color: TVec4; const Thickness: Single = 1 );
procedure RenderLineRect2D(p1, p2: TVec2; c1, c2, c3, c4: TVec4; const Thickness: Single);
procedure RenderLineRect2D(p1, p2: TVec2; Color: TVec4; const Thickness: Single);
procedure RenderRect2D(p1, p2: TVec2; c1, c2, c3, c4: TVec4);
procedure RenderRect2D(p1, p2: TVec2; Color: TVec4);
procedure RenderQuad2D(p1, p2, p3, p4: TVec2; c1, c2, c3, c4: TVec4);
procedure RenderQuad2D(p1, p2, p3, p4: TVec2; Color: TVec4);
procedure RenderLineCircle2D( C: TVec2; R: Single; num_segments: Integer; Color: TVec4 );
procedure RenderCircle2D( C: TVec2; R: Single; num_segments: Integer; Color: TVec4 );

procedure RenderLine3D( p1, p2: TVec3; Color: TVec4; const Thickness: Single = 1 );
procedure RenderLines3D( points: array of TVec3; Color: TVec4; const Thickness: Single = 1 );
procedure RenderLines3D( points: TVec3List; Color: TVec4; const Thickness: Single = 1 );

procedure Setup2D( Left, Top, Width, Height: Single );
procedure Setup3D( Mat: TMat4 );

procedure InitGeom( ALineShader2D: TShader; ALineShader3D: TShader );

var
  LineShader2D: TShader;
  LineShader3D: TShader;
  proj2D: TMat4;

implementation

procedure SegmentLine2D( p1, p2: TVec2; Thickness: Single ); inline;
var
  Dir: TVec2;
  vt: GLint;
begin
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );
  Dir:= ( p2 - p1 ).Normalize;                                    //Dir2 = vec2( -y, x )
  glVertexAttrib2f( vt, p1.X + ( - Dir.y - Dir.X ) * Thickness, p1.Y + (   Dir.x - Dir.Y ) * Thickness );
  glVertexAttrib2f( vt, p2.X + ( - Dir.y + Dir.X ) * Thickness, p2.Y + (   Dir.x + Dir.Y ) * Thickness );
  glVertexAttrib2f( vt, p1.X + (   Dir.y - Dir.X ) * Thickness, p1.Y + ( - Dir.x - Dir.Y ) * Thickness );
  glVertexAttrib2f( vt, p2.X + (   Dir.y + Dir.X ) * Thickness, p2.Y + ( - Dir.x + Dir.Y ) * Thickness );
end;

procedure SegmentLine2D( p1, p2: TVec2; c1, c2: TVec4; Thickness: Single ); inline;
var
  Dir: TVec2;
  vt, cl: GLint;
begin
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );
  cl:= LineShader2D.Attributes.AddrByName( 'color' );
  Dir:= ( p2 - p1 ).Normalize;                                    //Dir2 = vec2( -y, x )
  glVertexAttrib4f( cl, c1.R, c1.G, c1.B, c1.A );
  glVertexAttrib2f( vt, p1.X + ( - Dir.y - Dir.X ) * Thickness, p1.Y + (   Dir.x - Dir.Y ) * Thickness );
  glVertexAttrib2f( vt, p2.X + ( - Dir.y + Dir.X ) * Thickness, p2.Y + (   Dir.x + Dir.Y ) * Thickness );
  glVertexAttrib4f( cl, c2.R, c2.G, c2.B, c2.A );
  glVertexAttrib2f( vt, p1.X + (   Dir.y - Dir.X ) * Thickness, p1.Y + ( - Dir.x - Dir.Y ) * Thickness );
  glVertexAttrib2f( vt, p2.X + (   Dir.y + Dir.X ) * Thickness, p2.Y + ( - Dir.x + Dir.Y ) * Thickness );
end;

procedure RenderLine2D( p1, p2: TVec2; Color: TVec4; const Thickness: Single );
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( LineShader2D )) then
    Exit;
  LineShader2D.Enable;
  cl:= LineShader2D.Attributes.AddrByName( 'color' );
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );
  if ( Thickness = 1 ) then
    begin
      glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
      glBegin( GL_LINES );
      glVertexAttrib2f( vt, p1.X, p1.Y );
      glVertex2f( p2.X, p2.Y );
      glEnd();
    end
  else
    begin
      glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
      glBegin( GL_TRIANGLE_STRIP );
      SegmentLine2D( p1, p2, Thickness );
      glEnd();
    end;
end;

procedure RenderLine2D( p1, p2: TVec2; Color1, Color2: TVec4;
  const Thickness: Single );
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( LineShader2D )) then
    Exit;
  LineShader2D.Enable;
  cl:= LineShader2D.Attributes.AddrByName( 'color' );
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );
  if ( Thickness = 1 ) then
    begin
      glBegin( GL_LINES );
      glVertexAttrib4f( cl, Color1.R, Color1.G, Color1.B, Color1.A );
      glVertexAttrib2f( vt, p1.X, p1.Y );
      glVertexAttrib4f( cl, Color2.R, Color2.G, Color2.B, Color2.A );
      glVertexAttrib2f( vt, p2.X, p2.Y );
      glEnd();
    end
  else
    begin
      glBegin( GL_TRIANGLE_STRIP );
      SegmentLine2D( p1, p2, Color1, Color2, Thickness );
      glEnd();
    end;
end;

procedure RenderLines2D( points: array of TVec2; Color: TVec4;
  const Thickness: Single );
var
  Dir: TVec2;
  i: Integer;
  vt: GLint;
  cl: GLint;
begin
  if ( Length( points ) < 2 ) then
    exit;
  if ( not Assigned( LineShader2D )) then
    Exit;
  LineShader2D.Enable;

  cl:= LineShader2D.Attributes.AddrByName( 'color' );
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );
  if ( Thickness = 1 ) then
    begin
      glBegin( GL_LINE_STRIP );
      for i:= low( points ) to high( points ) do
        begin
          glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
          glVertexAttrib2f( vt, points[ i ].X, points[ i ].Y );
        end;
      glEnd();
    end
  else
    begin
      glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
      glBegin( GL_TRIANGLE_STRIP );
      for i:= low( points ) + 1 to high( points ) do
        SegmentLine2D( points[ i - 1 ], points[ i ], Thickness );
      glEnd();
    end;
end;


procedure RenderLines2D( points: array of TVec2; Colors: array of TVec4;
  const Thickness: Single );
var
  Dir: TVec2;
  i: Integer;
  vt: GLint;
  cl: GLint;
begin
  if ( Length( points ) < 2 ) then
    exit;
  if ( not Assigned( LineShader2D )) then
    Exit;
  LineShader2D.Enable;

  cl:= LineShader2D.Attributes.AddrByName( 'color' );
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );
  if ( Thickness = 1 ) then
    begin
      glBegin( GL_LINE_STRIP );
      for i:= low( points ) to high( points ) do
        begin
          glVertexAttrib4f( cl, Colors[ i ].R, Colors[ i ].G, Colors[ i ].B, Colors[ i ].A );
          glVertexAttrib2f( vt, points[ i ].X, points[ i ].Y );
        end;
      glEnd();
    end
  else
    begin
      glBegin( GL_TRIANGLE_STRIP );
      for i:= low( points ) + 1 to high( points ) do
        SegmentLine2D( points[ i - 1 ], points[ i ], Colors[ i - 1 ], Colors[ i ], Thickness );
      glEnd();
    end;
end;

procedure RenderLines2D(points: TVec2List; Color: TVec4; const Thickness: Single );
begin

end;


procedure RenderRect2D(p1, p2: TVec2; Color: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( LineShader2D )) then
    Exit;
  LineShader2D.Enable;

  cl:= LineShader2D.Attributes.AddrByName( 'color' );
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );

  glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib2f( vt, p1.X, p1.Y );
  glVertexAttrib2f( vt, p2.X, p1.Y );
  glVertexAttrib2f( vt, p2.X, p2.Y );
  glVertexAttrib2f( vt, p1.X, p2.Y );
  glEnd();
end;


procedure RenderQuad2D(p1, p2, p3, p4: TVec2; c1, c2, c3, c4: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( LineShader2D )) then
    Exit;
  LineShader2D.Enable;

  cl:= LineShader2D.Attributes.AddrByName( 'color' );
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );

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

procedure RenderQuad2D(p1, p2, p3, p4: TVec2; Color: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( LineShader2D )) then
    Exit;
  LineShader2D.Enable;

  cl:= LineShader2D.Attributes.AddrByName( 'color' );
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );

  glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
  glBegin( GL_TRIANGLE_FAN );
  glVertexAttrib2f( vt, p1.x, p1.Y );
  glVertexAttrib2f( vt, p2.X, p2.Y );
  glVertexAttrib2f( vt, p3.X, p3.Y );
  glVertexAttrib2f( vt, p4.X, p4.Y );
  glEnd();
end;

procedure RenderRect2D(p1, p2: TVec2; c1, c2, c3, c4: TVec4);
var
  cl: GLint;
  vt: GLint;
begin
  if ( not Assigned( LineShader2D )) then
    Exit;
  LineShader2D.Enable;

  cl:= LineShader2D.Attributes.AddrByName( 'color' );
  vt:= LineShader2D.Attributes.AddrByName( 'vertex' );

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

procedure RenderLineRect2D(p1, p2: TVec2; Color: TVec4; const Thickness: Single);
begin
  RenderLines2D([ p1, vec2( p2.x, p1.y ), p2, vec2( p1.x, p2.y ), p1 ], Color, Thickness );
end;

procedure RenderLineRect2D(p1, p2: TVec2; c1, c2, c3, c4: TVec4; const Thickness: Single);
begin
  RenderLines2D([ p1, vec2( p2.x, p1.y ), p2, vec2( p1.x, p2.y ), p1 ], [ c1, c2, c3, c4, c1 ], Thickness );
end;

{
procedure SegmentLine3D( p1, p2: TVec3; Thickness: Single ); inline;
var
  Dir: TVec2;
  vt: GLint;
begin
  vt:= LineShader.Attributes.AddrByName( 'vertex' );
  Dir:= ( p2 - p1 ).Normalize;                                    //Dir2 = vec2( -y, x )
  glVertexAttrib3f( vt, p1.X + ( - Dir.y - Dir.X ) * Thickness, p1.Y + (   Dir.x - Dir.Y ) * Thickness );
  glVertexAttrib3f( vt, p2.X + ( - Dir.y + Dir.X ) * Thickness, p2.Y + (   Dir.x + Dir.Y ) * Thickness );
  glVertexAttrib3f( vt, p1.X + (   Dir.y - Dir.X ) * Thickness, p1.Y + ( - Dir.x - Dir.Y ) * Thickness );
  glVertexAttrib3f( vt, p2.X + (   Dir.y + Dir.X ) * Thickness, p2.Y + ( - Dir.x + Dir.Y ) * Thickness );
end;}

procedure RenderLine3D(p1, p2: TVec3; Color: TVec4; const Thickness: Single);
begin

end;

procedure RenderLines3D(points: array of TVec3; Color: TVec4;
  const Thickness: Single);
var
  P: TVec3;
  i: Integer;
  vt: GLint;
  cl: GLint;
begin
  if ( Length( points ) < 2 ) then
    exit;
  if ( not Assigned( LineShader3D )) then
    Exit;
  LineShader3D.Enable;

  cl:= LineShader3D.Attributes.AddrByName( 'color' );
  vt:= LineShader3D.Attributes.AddrByName( 'vertex' );
  if ( Thickness = 1 ) then
    begin
      glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
      glBegin( GL_LINE_STRIP );
      for P in points do
        glVertexAttrib3f( vt, P.X, P.Y, P.Z );
      glEnd();
    end
  else
    begin

    end;
end;

procedure RenderLines3D( points: TVec3List; Color: TVec4; const Thickness: Single );
begin

end;

procedure RenderLineCircle2D(C: TVec2; R: Single; num_segments: Integer;
  Color: TVec4);
var
  theta, co, si, t: Single;
  p: TVec2;
  i: Integer;
begin
  theta:= 2 * PI / num_segments;
  co:= cos( theta );//precalculate the sine and cosine
  si:= sin( theta );

  p:= vec2( r, 0 );//we start at angle = 0

  glBegin(GL_LINE_LOOP);
  for i:= 0 to num_segments - 1 do
    begin
      glVertex2f( p.x + c.x, p.y + c.y);//output vertex

		//apply the rotation matrix
      t:= p.x;
      p:= vec2( co * p.x - si * p.y, si * t + co * p.y );
    end;
  glEnd();
end;

procedure RenderCircle2D( C: TVec2; R: Single; num_segments: Integer; Color: TVec4 );
var
  theta, co, si, t: Single;
  p: TVec2;
  i: Integer;
begin
  theta:= 2 * PI / num_segments;
  co:= cos( theta );//precalculate the sine and cosine
  si:= sin( theta );

  p:= vec2( r, 0 );//we start at angle = 0

  glBegin(GL_TRIANGLE_FAN);
  glVertex2f( c.x, c.y );//start with center
  for i:= 0 to num_segments do
    begin
      glVertex2f( p.x + c.x, p.y + c.y);//output vertex

		//apply the rotation matrix
      t:= p.x;
      p:= vec2( co * p.x - si * p.y, si * t + co * p.y );
    end;
  glEnd();
end;

procedure Setup2D(Left, Top, Width, Height: Single);
begin
  if ( not Assigned( LineShader2D )) then
    Exit;

  proj2D:= {mat4translate( vec4( -Left, Top, 0, 1 )) * }mat4orthoRH( Left, Left + Width, Top, Top + Height );// * mat4translate( vec4( 0, 1, 0, 1 ));

  LineShader2D.Enable;
  glUniformMatrix4fv( LineShader2D.Uniforms.AddrByName( 'mat' ), 1, False, @proj2D );
end;

procedure Setup3D(Mat: TMat4);
begin
  LineShader3D.Enable;
  glUniformMatrix4fv( LineShader2D.Uniforms.AddrByName( 'mat' ), 1, False, @Mat );
end;

procedure InitGeom( ALineShader2D: TShader; ALineShader3D: TShader );
begin
  LineShader2D:= ALineShader2D;
  LineShader3D:= ALineShader3D;
end;

end.

