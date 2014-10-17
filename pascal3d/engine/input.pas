unit input;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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

      function GetDKeys( Index: Integer ): Boolean;
      function GetKeys( Index: Integer ): Boolean;
      procedure SetDKeys( Index: Integer ; AValue: Boolean);
      procedure SetKeys( Index: Integer ; AValue: Boolean);

    public
      procedure FlushDelta; override;
      property Keys[ Index: Integer ]: Boolean read GetKeys write SetKeys;
      property DKeys[ Index: Integer ]: Boolean read GetDKeys write SetDKeys;
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
end;

{ TInputManager }

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
