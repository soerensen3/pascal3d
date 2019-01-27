unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  sdl2,

  p3d.math,
  p3d.utils,
  p3d.events,
  p3d.core;

type

  { TMyApplication }

  TMyApplication = class ( TP3DApplication )
    private
      FTriangleList: TP3DPlotListLayer;
      FView: TP3DTransform;
      FProj: TP3DTransform;

    public
      procedure Initialize; override; // Override the initialize procedure to
                                      // allow custom initializations
      procedure Finalize; override;
      procedure Render; override;
      procedure SetupTriangle;
      procedure SetupProj;
      procedure ResizeWindow(Sender: TP3DWindow; Event: TSDL_WindowEvent); override;

      property View: TP3DTransform read FView;
      property Proj: TP3DTransform read FProj;
  end;


var
  MyApplication: TMyApplication = nil;

implementation

{ TMyApplication }

procedure TMyApplication.Initialize;
var
  Prop: TP3DStreamableContainer;
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

  Prop:= TP3DStreamableContainer.Create( 'View', TP3DTransform );
  Properties.Add( Prop );
  FView:= TP3DTransform.Create( Prop );

  Prop:= TP3DStreamableContainer.Create( 'Proj', TP3DTransform );
  Properties.Add( Prop );
  FProj:= TP3DTransform.Create( Prop );

  SetupTriangle;
  SetupProj;
end;

procedure TMyApplication.Finalize;
begin
  FreeAndNil( FTriangleList );
  inherited Finalize;
end;

procedure TMyApplication.Render;
begin
  inherited Render;
  View.Matrix:= mat4rotate( vec3_Axis_PZ, GetTickCount64 / 1000 ) * mat4scale( vec4( vec2( 0.5 ), 1, 1 ));
  FTriangleList.Execute;
end;

procedure TMyApplication.SetupTriangle;
var
  h: Float;
begin
  h:= sqrt( 3 );
  FTriangleList:=
    layers([
      command_clear([ cfColor, cfDepth ], Grey500 ), // Clear the background and depth buffer
      geom_polygon([ vec3( -1,-1/3*h, 0 ), vec3( 1, -1/3*h, 0 ), vec3( 0, 2/3*h, 0 )],
      // draws a triangle from the passed coordinates
        settings([
          TP3DPlotCommandUniformTransform.Create( 'proj', FProj ),
          TP3DPlotCommandUniformTransformView.Create( FView ),
          //attrib( P3DAttribColor, Red500 )
          // sets the color attribute for the whole triangle
          attrib( P3DAttribColor, [ Red500, Green500, Blue500 ])
          // sets the color attribute for each vertex
        ])
      )
    ]);
end;

procedure TMyApplication.SetupProj;
var
  Aspect: Extended;
begin
  Aspect:= P3DViewports.Screen.Width / P3DViewports.Screen.Height;
  FProj.Scale:= vec3( 1/Aspect, 1, 1 );
end;

procedure TMyApplication.ResizeWindow(Sender: TP3DWindow; Event: TSDL_WindowEvent);
begin
  inherited ResizeWindow(Sender, Event);
  SetupProj;
end;

end.

