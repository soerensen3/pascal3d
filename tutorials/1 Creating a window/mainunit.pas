unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  p3d.events,
  p3d.core;

type

  { TMyApplication }

  TMyApplication = class ( TP3DApplication )
    public
      procedure Initialize; override; // Override the initialize procedure to
                                      // allow custom initializations
  end;


var
  MyApplication: TMyApplication = nil;

implementation

{ TMyApplication }

procedure TMyApplication.Initialize;
begin
  inherited Initialize;
  TP3DWindow.Create( Windows ); // Create the main window. Note that this is freed
                                // automatically when the application terminates
  MainWindow.Title:= 'My first Pascal3D application'; // Set the title
  MainWindow.Width:= 800; // Set the resolution
  MainWindow.Height:= 600;
  // Uncomment if you want full screen mode
  //MainWindow.FullScreen:= True;
end;

end.

