unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  p3d.math,
  p3d.events,
  p3d.core;

type

  { TMyApplication }

  TMyApplication = class ( TP3DApplication )
    private
      FTriangleList: TP3DPlotListLayer;

    public
      procedure Initialize; override; // Override the initialize procedure to
                                      // allow custom initializations
      procedure Finalize; override;
      procedure Render; override;
      procedure SetupTriangle;
  end;


var
  MyApplication: TMyApplication = nil;

implementation

{ TMyApplication }

procedure TMyApplication.Initialize;
begin
  LogTitle:= 'triangle';
  log_file_open( 'triangle.html' );

  inherited Initialize;
  TP3DWindow.Create( Windows ); // Create the main window. Note that this is freed
                                // automatically when the application terminates
  MainWindow.Title:= 'Rendering a triangle'; // Set the title
  //MainWindow.Width:= 800; // Set the resolution
  //MainWindow.Height:= 600;
  // Uncomment if you want full screen mode
  //MainWindow.FullScreen:= True;

  P3DShaderNodeLib.LoadLibraryPath( '../../../shaders/' );

  SetupTriangle;
end;

procedure TMyApplication.Finalize;
begin
  FreeAndNil( FTriangleList );
  inherited Finalize;
end;

procedure TMyApplication.Render;
begin
  inherited Render;
  FTriangleList.Execute;
end;

procedure TMyApplication.SetupTriangle;
begin
  FTriangleList:=
    layers([
      command_clear([ cfColor, cfDepth ], Grey500 ), // Clear the background and depth buffer

      geom_polygon([ vec3( -1,-1, 0 ), vec3( 1, -1, 0 ), vec3( 0, 1, 0 )],
      // draws a triangle from the passed coordinates
        settings([
          //attrib( P3DAttribColor, Red500 )
          // sets the color attribute for the whole triangle
          attrib( P3DAttribColor, [ Red500, Green500, Blue500 ])
          // sets the color attribute for each vertex
        ])
      )
    ]);
end;

end.

