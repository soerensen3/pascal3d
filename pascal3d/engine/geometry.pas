unit geometry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math3D, shaders, dglOpenGL;

procedure RenderLine2D( p1, p2: TVec2; Color: TVec4; const Thickness: Single = 1 );
procedure RenderLines2D( points: array of TVec2; Color: TVec4; const Thickness: Single = 1 );
procedure RenderLines2D( points: TVec2List; Color: TVec4; const Thickness: Single = 1 );

procedure RenderLine3D( p1, p2: TVec3; Color: TVec4; const Thickness: Single = 1 );
procedure RenderLines3D( points: array of TVec3; Color: TVec4; const Thickness: Single = 1 );
procedure RenderLines3D( points: TVec3List; Color: TVec4; const Thickness: Single = 1 );

procedure Setup2D( Width, Height: Single );
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

procedure RenderLines2D(points: array of TVec2; Color: TVec4;
  const Thickness: Single);
var
  Dir: TVec2;
  P: TVec2;
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
      glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
      glBegin( GL_LINE_STRIP );
      for P in points do
        glVertexAttrib2f( vt, P.X, P.Y );
      glEnd();
    end
  else
    begin
      glVertexAttrib4f( cl, Color.R, Color.G, Color.B, Color.A );
      glBegin( GL_TRIANGLE_STRIP );
      for i:= 1 to high( points ) do
        SegmentLine2D( points[ i - 1 ], points[ i ], Thickness );
      glEnd();
    end;
end;

procedure RenderLines2D(points: TVec2List; Color: TVec4; const Thickness: Single );
begin

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

procedure Setup2D( Width, Height: Single );
begin
  if ( not Assigned( LineShader2D )) then
    Exit;

  proj2D:= mat4orthoRH( Width, -Height, -1, 1 ) * mat4translate( vec4( -1, 1, 0, 1 ));

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

