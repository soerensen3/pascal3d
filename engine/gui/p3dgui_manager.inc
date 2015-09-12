{$IFDEF INTERFACE}

{ TP3DGUIManager }

TP3DGUIManager = class
  private
    FCanvas: TP3DCanvas2D;
    FControls: TP3DControlList;
    FWindow: TSDLWindow;

    function GetMouseX: Integer;
    function GetMouseY: Integer;
    procedure SetMouseX( const Value: Integer );
    procedure SetMouseY( const Value: Integer );

  public
    constructor Create;
    destructor Destroy; override;

    procedure Render;
    procedure Input;
    procedure UpdateExtents;

  published
    property MouseX: Integer read GetMouseX write SetMouseX;
    property MouseY: Integer read GetMouseY write SetMouseY;
    property Window: TSDLWindow read FWindow write FWindow;
    property ScreenCanvas: TP3DCanvas2D read FCanvas write FCanvas;
    property Controls: TP3DControlList read FControls write FControls;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DGUIManager }

constructor TP3DGUIManager.Create();
begin
  inherited Create;

//  DefaultFont:= FontManager[ FontManager.Add( '..\Fonts\Arial.fnt' )];
  Controls:= TP3DControlList.Create( nil, Self );
  ScreenCanvas:= TP3DCanvas2D.Create( nil );
  UpdateExtents;
end;

destructor TP3DGUIManager.Destroy;
begin
  ScreenCanvas.Free;
  Controls.Clear( True );
  Controls.Free;
  inherited;
end;

function TP3DGUIManager.GetMouseX: Integer;
var
  cpos: TPoint;
begin
  Result:= InputManager.Mouse.X;
end;

function TP3DGUIManager.GetMouseY: Integer;
var
  cpos: TPoint;
begin
  Result:= InputManager.Mouse.Y;
end;

procedure TP3DGUIManager.Input;
var
  Ctrl: TP3DGraphicControl;
begin
  Ctrl:= Controls.Input;
  //if (( Assigned( Ctrl )) AND ( gcisMouseOverOccl in Ctrl.InputState )) then
  //  Ctrl.InputState:= ( Ctrl.InputState + [ gcisMouseOver ]) - [ gcisMouseOverOccl ];
end;

procedure TP3DGUIManager.Render;
begin
  Controls.Render( vec4( 1 ), vec2( 0 ));
end;

procedure TP3DGUIManager.SetMouseX( const Value: Integer );
var
  cpos: TPoint;
begin
  InputManager.Mouse.X:= ActiveWindow.Left + Value;
end;

procedure TP3DGUIManager.SetMouseY(const Value: Integer);
var
  cpos: TPoint;
begin
  InputManager.Mouse.Y:= ActiveWindow.Top + Value
end;

procedure TP3DGUIManager.UpdateExtents;
begin
  if ( P3DViewports.Count > 0 ) then
    begin
      ScreenCanvas.Left:= P3DViewports.VP[ 0 ].Left;
      ScreenCanvas.Top:= P3DViewports.VP[ 0 ].Top;
      ScreenCanvas.Width:= P3DViewports.VP[ 0 ].Width;
      ScreenCanvas.Height:= P3DViewports.VP[ 0 ].Height;
    end;
  Controls.Realign;
end;

{$ENDIF}
