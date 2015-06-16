unit p3dinput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

{$INCLUDE p3dinput_scancodes.inc}
type
  TInputDevice = class
    procedure FlushDelta; virtual; abstract;
  end;

  { TMouseDevice }

  TMouseDevice = class( TInputDevice )
    private
      FDX, FDY: Integer;
      FX, FY: Integer;
      FButtons: array [ 0..4 ] of Boolean;
      FDButtons: array [ 0..4 ] of Boolean;

      function GetButtons( Index: Integer ): Boolean;
      function GetDButtons( Index: Integer ): Boolean;
      procedure SetButtons( Index: Integer ; AValue: Boolean);
      procedure SetDButtons( Index: Integer ; AValue: Boolean);

    public
      procedure FlushDelta; override;

      //Delta of cursor x position
      property DX: Integer read FDX write FDX;
      //Delta of cursor x position
      property DY: Integer read FDY write FDY;
      //Cursor x position on screen, not window.
      property X: Integer read FX write FX;
      //Cursor y position on screen, not window.
      property Y: Integer read FY write FY;
      //Button State
      property Buttons[ Index: Integer ]: Boolean read GetButtons write SetButtons;
      //Delta Button State
      property DButtons[ Index: Integer ]: Boolean read GetDButtons write SetDButtons;
  end;

  { TKeyboardDevice }

  TKeyboardDevice = class( TInputDevice )
    private
      FKeys: array [ 0..511 ] of Boolean;
      FDKeys: array [ 0..511 ] of Boolean;
      FReadingTextInput: Boolean;
      FInputText: String;

      procedure SetReadingTextInput(AValue: Boolean);
      function GetDKeys( Index: Integer ): Boolean;
      function GetKeys( Index: Integer ): Boolean;
      procedure SetDKeys( Index: Integer ; AValue: Boolean);
      procedure SetKeys( Index: Integer ; AValue: Boolean);

    public
      procedure FlushDelta; override;

      property Keys[ Index: Integer ]: Boolean read GetKeys write SetKeys;
      property DKeys[ Index: Integer ]: Boolean read GetDKeys write SetDKeys;
      property ReadingTextInput: Boolean read FReadingTextInput write SetReadingTextInput;
      property InputText: String read FInputText write FInputText;
  end;

  { TInputEvent }

  TP3DInputEvent = class
    private
      FTimeStamp: Cardinal;

    published
      property TimeStamp: Cardinal read FTimeStamp write FTimeStamp;
  end;

  { TMouseMotionEvent }

  TP3DMouseMotionEvent = class( TP3DInputEvent )
    private
      FDX: Integer;
      FDY: Integer;
      FX: Integer;
      FY: Integer;

    published
      property DX: Integer read FDX write FDX;
      property DY: Integer read FDY write FDY;
      property X: Integer read FX write FX;
      property Y: Integer read FY write FY;
  end;

  TP3DMouseButton = ( p3dmbLeft = 0, p3dmbRight = 1, p3dmbMiddle = 2 );
  TP3DInputState = ( p3disReleased = 0, p3disPressed = 1 );

  { TP3DMouseButtonEvent }

  TP3DMouseButtonEvent = class( TP3DInputEvent )
    private
      FButton: TP3DMouseButton;
      FState: TP3DInputState;
      FX: Integer;
      FY: Integer;

    published
      property X: Integer read FX write FX;
      property Y: Integer read FY write FY;
      property Button: TP3DMouseButton read FButton write FButton;
      property State: TP3DInputState read FState write FState;
  end;

  { TP3DKeyboardEvent }

  TP3DKeyboardEvent = class ( TP3DInputEvent )
    private
      FState: TP3DInputState;

    published
      property State: TP3DInputState read FState write FState;
      //property Key:
  end;

  { TInputManager }

  TInputManager = class
    private
      FKeyboard: TKeyboardDevice;
      FMouse: TMouseDevice;

    public
      constructor Create;
      destructor Destroy; override;

      procedure NextCycle;

    published
      property Mouse: TMouseDevice read FMouse write FMouse;
      property Keyboard: TKeyboardDevice read FKeyboard write FKeyboard;
  end;

  var InputManager: TInputManager;

implementation

{ TKeyboardDevice }

function TKeyboardDevice.GetKeys( Index: Integer ): Boolean;
begin
  Result:= FKeys[ Index ];
end;

function TKeyboardDevice.GetDKeys( Index: Integer ): Boolean;
begin
  Result:= FDKeys[ Index ];
end;

procedure TKeyboardDevice.SetDKeys( Index: Integer ; AValue: Boolean);
begin
  FDKeys[ Index ]:= AValue;
end;

procedure TKeyboardDevice.SetKeys( Index: Integer ; AValue: Boolean);
begin
  FKeys[ Index ]:= AValue;
end;

procedure TKeyboardDevice.FlushDelta;
begin
  FillByte( FDKeys, SizeOf( FDKeys ), 0 );
  FInputText:= '';
end;

{ TInputManager }

procedure TKeyboardDevice.SetReadingTextInput(AValue: Boolean);
begin
  if FReadingTextInput=AValue then Exit;
  FReadingTextInput:=AValue;
  FInputText:= '';
end;

constructor TInputManager.Create;
begin
  FMouse:= TMouseDevice.Create;
  FKeyboard:= TKeyboardDevice.Create;
end;

destructor TInputManager.Destroy;
begin
  FMouse.Free;
  FKeyboard.Free;
  inherited Destroy;
end;

procedure TInputManager.NextCycle;
begin
  Mouse.FlushDelta;
  Keyboard.FlushDelta;
end;

{ TMouseDevice }

function TMouseDevice.GetButtons( Index: Integer ): Boolean;
begin
  Result:= FButtons[ Index ];
end;

function TMouseDevice.GetDButtons( Index: Integer ): Boolean;
begin
  Result:= FDButtons[ Index ];
end;

procedure TMouseDevice.SetButtons( Index: Integer ; AValue: Boolean);
begin
  FButtons[ Index ]:= AValue;
end;

procedure TMouseDevice.SetDButtons( Index: Integer ; AValue: Boolean);
begin
  FDButtons[ Index ]:= AValue;
end;

procedure TMouseDevice.FlushDelta;
begin
  DX:= 0; DY:= 0;
  FillByte( FDButtons, SizeOf( FDButtons ), 0 );
end;

{ TInputManager }



initialization
  InputManager:= TInputManager.Create;

finalization
  InputManager.Free;


end.

