program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  window_sdl,
  texture_sdl,
  dglOpenGL
  { you can add units after this };

var
  Tex: TSDLSurface;

procedure Render( Window: TSDLWindow );
var
  X: Integer;
  Y: Integer;
  Width: Integer;
  Height: Integer;
begin
  glClear( GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT );

  glBindTexture(GL_TEXTURE_2D, Tex.fGLTexture );
  glEnable( GL_TEXTURE_2D );
  // For Ortho mode, of course
  X:= -1;
  Y:= -1;
  Width:= 2;
  Height:= 2;

  glBegin( GL_TRIANGLES ); // Wir wollen ein Dreieck zeichnen

    glColor3f(1,0,0);      // Ab jetzt werden alle gezeichneten Punkte rot
    glVertex3f(400,100,0); // Der erste Eckpunkt ist mittig und 100 Pixel
    glTexCoord2f(0.5, 0);
                           // vom oberen Rand entfernt

    glColor3f(0,1,0);      // Ab jetzt werden alle gezeichneten Punkte grün
    glVertex3f(750,500,0); // Der zweite Eckpunkt ist 50 Pixel vom rechten
    glTexCoord2f(1, 1);
                           // und 100 Pixel vom unteren Rand entfernt

    glColor3f(0,0,1);      // Ab jetzt werden alle gezeichneten Punkte blau
    glVertex3f(50,500,0);  // Der dritte Eckpunkt ist 50 Pixel vom linken
    glTexCoord2f(0, 1);
                           // und 100 Pixel vom unteren Rand entfernt

  glEnd(); // Wir sind fertig mit dem Zeichnen
end;

procedure Init( Window: TSDLWindow );
begin
  Tex:= TSDLSurface.Create( '/home/johannes/hpscan002.png' );

  glMatrixMode( GL_PROJECTION ); // Stack für Projektionsmatrix als
                                 // aktiven Matrixstack setzen
  glLoadIdentity();              // Identitätsmatrix auf den Stack laden

  // Eine orthogonale Projektionsmatrix zum Stack
  // dazu multiplizieren.
  glOrtho( 0, 800, 600, 0, -1, 1 );
end;

procedure Deinit( Window: TSDLWindow );
begin
  Tex.Free;
end;

var
  Wnd: TSDLWindow;

begin
  Wnd:= TSDLWindow.Create;

  Wnd.OnInit:= @Init;
  Wnd.OnDeinit:= @Deinit;
  Wnd.OnRender:= @Render;

  Wnd.Run;
  Wnd.Free;
end.

