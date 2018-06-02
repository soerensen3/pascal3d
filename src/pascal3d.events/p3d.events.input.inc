//p3dinput.inc
//part of p3devents.pas

{$IFDEF INTERFACE}

{$INCLUDE p3d.events.input_scancodes.inc}
type
  TP3DInputDevice = class
    procedure FlushDelta; virtual; abstract;
  end;

  { TP3DMouseDevice }

  TP3DMouseDevice = class( TP3DInputDevice )
    private
      FDWX: Integer;
      FDWY: Integer;
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
      //Delta of mouse wheel X
      property DWX: Integer read FDWX write FDWX;
      property DWY: Integer read FDWY write FDWY;
      //Cursor x position on screen, not window.
      property X: Integer read FX write FX;
      //Cursor y position on screen, not window.
      property Y: Integer read FY write FY;
      //Button State
      property Buttons[ Index: Integer ]: Boolean read GetButtons write SetButtons;
      //Delta Button State
      property DButtons[ Index: Integer ]: Boolean read GetDButtons write SetDButtons;
  end;

  TP3DKeyState = ( ksUnchanged, ksPressed, ksReleased );

  { TP3DKeyboardDevice }

  TP3DKeyboardDevice = class( TP3DInputDevice )
    private
      FKeys: array [ 0..511 ] of Boolean;
      FDKeys: array [ 0..511 ] of TP3DKeyState;
      FKeysLayout: array [ 0..511 ] of Boolean;
      FDKeysLayout: array [ 0..511 ] of TP3DKeyState;
      FReadingTextInput: Boolean;
      FInputText: String;

      function GetDKeysLayout( Index: Integer ): TP3DKeyState;
      function GetKeysLayout( Index: Integer ): Boolean;
      procedure SetDKeysLayout( Index: Integer ; AValue: TP3DKeyState);
      procedure SetKeysLayout( Index: Integer ; AValue: Boolean);
      procedure SetReadingTextInput(AValue: Boolean);
      function GetDKeys( Index: Integer ): TP3DKeyState;
      function GetKeys( Index: Integer ): Boolean;
      procedure SetDKeys( Index: Integer ; AValue: TP3DKeyState);
      procedure SetKeys( Index: Integer ; AValue: Boolean);

    public
      procedure FlushDelta; override;

      property Keys[ Index: Integer ]: Boolean read GetKeys write SetKeys;
      property DKeys[ Index: Integer ]: TP3DKeyState read GetDKeys write SetDKeys;
      property KeysLayout[ Index: Integer ]: Boolean read GetKeysLayout write SetKeysLayout;
      property DKeysLayout[ Index: Integer ]: TP3DKeyState read GetDKeysLayout write SetDKeysLayout;
      property ReadingTextInput: Boolean read FReadingTextInput write SetReadingTextInput;
      property InputText: String read FInputText write FInputText;
  end;

  // Key
  // - pressed
  // - state: unchanged, released, pressed

  { TInputEvent }

type
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

  { TP3DInputManager }

  TP3DInputManager = class
    private
      FKeyboard: TP3DKeyboardDevice;
      FMouse: TP3DMouseDevice;

    public
      constructor Create;
      destructor Destroy; override;

      procedure NextCycle;

    published
      property Mouse: TP3DMouseDevice read FMouse write FMouse;
      property Keyboard: TP3DKeyboardDevice read FKeyboard write FKeyboard;
  end;

  function P3DGetKeyByScancode( ScanCode: LongInt ): LongInt;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DKeyboardDevice }

function TP3DKeyboardDevice.GetKeys(Index: Integer): Boolean;
begin
  Result:= FKeys[ Index ];
end;

function TP3DKeyboardDevice.GetDKeys(Index: Integer): TP3DKeyState;
begin
  Result:= FDKeys[ Index ];
end;

procedure TP3DKeyboardDevice.SetDKeys(Index: Integer; AValue: TP3DKeyState);
begin
  FDKeys[ Index ]:= AValue;
end;

procedure TP3DKeyboardDevice.SetKeys( Index: Integer ; AValue: Boolean);
begin
  FKeys[ Index ]:= AValue;
end;

procedure TP3DKeyboardDevice.FlushDelta;
begin
  FillByte( FDKeys, SizeOf( FDKeys ), 0 );
  FillByte( FDKeysLayout, SizeOf( FDKeysLayout ), 0 );
  FInputText:= '';
end;

{ TP3DInputManager }

procedure TP3DKeyboardDevice.SetReadingTextInput(AValue: Boolean);
begin
  if FReadingTextInput=AValue then Exit;
  FReadingTextInput:=AValue;
  FInputText:= '';
end;

function TP3DKeyboardDevice.GetDKeysLayout( Index: Integer ): TP3DKeyState;
begin
  Result:= FDKeysLayout[ Index ];
end;

function TP3DKeyboardDevice.GetKeysLayout( Index: Integer ): Boolean;
begin
  Result:= FKeysLayout[ Index ];
end;

procedure TP3DKeyboardDevice.SetDKeysLayout( Index: Integer ; AValue: TP3DKeyState);
begin
  FDKeysLayout[ Index ]:= AValue;
end;

procedure TP3DKeyboardDevice.SetKeysLayout( Index: Integer ; AValue: Boolean);
begin
  FKeysLayout[ Index ]:= AValue;
end;


constructor TP3DInputManager.Create;
begin
  FMouse:= TP3DMouseDevice.Create;
  FKeyboard:= TP3DKeyboardDevice.Create;
end;

destructor TP3DInputManager.Destroy;
begin
  FMouse.Free;
  FKeyboard.Free;
  inherited Destroy;
end;

procedure TP3DInputManager.NextCycle;
begin
  Mouse.FlushDelta;
  Keyboard.FlushDelta;
end;

{ TP3DMouseDevice }

function TP3DMouseDevice.GetButtons( Index: Integer ): Boolean;
begin
  Result:= FButtons[ Index ];
end;

function TP3DMouseDevice.GetDButtons( Index: Integer ): Boolean;
begin
  Result:= FDButtons[ Index ];
end;

procedure TP3DMouseDevice.SetButtons( Index: Integer ; AValue: Boolean);
begin
  FButtons[ Index ]:= AValue;
end;

procedure TP3DMouseDevice.SetDButtons( Index: Integer ; AValue: Boolean);
begin
  FDButtons[ Index ]:= AValue;
end;

procedure TP3DMouseDevice.FlushDelta;
begin
  DX:= 0; DY:= 0; DWX:= 0; DWY:= 0;
  FillByte( FDButtons, SizeOf( FDButtons ), 0 );
end;


function P3DGetKeyByScancode( ScanCode: LongInt ): LongInt;
begin
  Result:= SDL_GetKeyFromScancode( ScanCode );
end;

{$ENDIF}

