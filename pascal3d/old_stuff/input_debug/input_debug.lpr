program input_debug;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  SDL2, window_sdl, input;

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure PeekInput;
    procedure PeekInputMouse;
    procedure CreateWnd;
    procedure DestroyWnd;
    procedure ListDevices;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  CreateWnd;
  { add your program here }
  ListDevices;
  PeekInput;
  DestroyWnd;
  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure Render(Sender: TSDLWindow);
begin
  MainWindow.Title:= Format( 'Mouse X: %d Y: %d DX: %d DY: %d [%d,%d,%d,%d,%d], D[%d,%d,%d,%d,%d]', [ InputManager.Mouse.X, InputManager.Mouse.Y,
                                                                  InputManager.Mouse.DX, InputManager.Mouse.DY,
                                                                  Ord( InputManager.Mouse.Buttons[ 0 ]), Ord( InputManager.Mouse.Buttons[ 1 ]), Ord( InputManager.Mouse.Buttons[ 2 ]),
                                                                  Ord( InputManager.Mouse.Buttons[ 3 ]), Ord( InputManager.Mouse.Buttons[ 4 ]),
                                                                  Ord( InputManager.Mouse.DButtons[ 0 ]), Ord( InputManager.Mouse.DButtons[ 1 ]), Ord( InputManager.Mouse.DButtons[ 2 ]),
                                                                  Ord( InputManager.Mouse.DButtons[ 3 ]), Ord( InputManager.Mouse.DButtons[ 4 ])]);
  InputManager.NextCycle;
end;

procedure TMyApplication.PeekInput;
begin
  MainWindow.Run;
end;

procedure TMyApplication.PeekInputMouse;
begin

end;

procedure TMyApplication.CreateWnd;
begin
  window_sdl.MainWindow:= TSDLWindow.Create;
  MainWindow.OnRender:= @Render;
end;

procedure TMyApplication.DestroyWnd;
begin
  window_sdl.MainWindow.Running:= False;
  window_sdl.MainWindow.Free;
end;

procedure TMyApplication.ListDevices;
var
  Keys: Integer;
begin
  WriteLn( '--- Touch Devices ---' );
  WriteLn( 'Num Touch Devices: ', SDL2.SDL_GetNumTouchDevices());
  WriteLn( '--- Joystick/Gamepad Devices ---' );
  WriteLn( 'Num Joystick/Gamepad Devices: ', SDL2.SDL_NumJoysticks());
  WriteLn( '--- Haptics ---' );
  WriteLn( 'Num Haptics: ', SDL2.SDL_NumHaptics());
  WriteLn( '--- Keyboard ---' );
  SDL_GetKeyboardState( @Keys );
  WriteLn( 'Num Keys: ', Keys );
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

