unit p3dprimitves2d;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, dglOpenGL, zglHeader, Math3D;

  procedure RenderRect( Left, Top, Width, Height: Float; Color: DWORD );



  const
      TRectangle: array [ 0..3 ] of TVec2 = (( FCOORD: ( 0, 0 )), ( FCOORD: ( 1, 0 )),
                                             ( FCOORD: ( 0, 1 )), ( FCOORD: ( 1, 1 )));

implementation



  procedure RenderRect(Left, Top, Width, Height: Float; Color: DWORD);
  var
    XRes: Integer;
  begin
    XRes:= zgl_Get( ;
    glViewport( Left, Top, Width, Height );



  end;

end.

