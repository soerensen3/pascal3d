{$IFDEF INTERFACE}

TP3DCursor = ( curArrow = 'A', curHourGlass = 'B', curHResize = 'C', curVResize = 'D', curMoveLeftRight = 'E', curMoveTopBottom = 'F', curNodeEdit = 'G' );

{ TP3DGUIManager }

TP3DGUIManager = class
  private
    FCanvas: TP3DCanvas2D;
    FControls: TP3DControlList;
    FCursor: TP3DCursor;
    FFocusedControl: TP3DGraphicControl;
    FShowCursor: Boolean;
    FWindow: TP3DWindow;
    FCursorTxt: TP3DText;

    function GetMouseX: Integer;
    function GetMouseY: Integer;
    procedure SetCursor(AValue: TP3DCursor);
    procedure SetFocusedControl(AValue: TP3DGraphicControl);
    procedure SetMouseX( const Value: Integer );
    procedure SetMouseY( const Value: Integer );
    procedure CtrlDeleteNotifier( Ctrl: TP3DGraphicControl );
    procedure SetShowCursor(AValue: Boolean);
    procedure SetWindow(AValue: TP3DWindow);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Render;
    procedure Input;
    procedure UpdateExtents;

  published
    property MouseX: Integer read GetMouseX write SetMouseX;
    property MouseY: Integer read GetMouseY write SetMouseY;
    property Window: TP3DWindow read FWindow write SetWindow;
    property ScreenCanvas: TP3DCanvas2D read FCanvas write FCanvas;
    property Controls: TP3DControlList read FControls write FControls;
    property Cursor: TP3DCursor read FCursor write SetCursor;
    property ShowCursor: Boolean read FShowCursor write SetShowCursor;
    property FocusedControl: TP3DGraphicControl read FFocusedControl write SetFocusedControl;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DGUIManager }

constructor TP3DGUIManager.Create();
begin
  inherited Create;

  //P3DFontManager.DefaultFontsPath:= '../../../fonts';
  Controls:= TP3DControlList.Create( nil, Self );
  ScreenCanvas:= TP3DCanvas2D.Create( nil );
  Cursor:= curArrow;
  UpdateExtents;
end;

destructor TP3DGUIManager.Destroy;
begin
  ScreenCanvas.Free;
  FCursorTxt.Free;
  Controls.Clear( True );
  Controls.Free;
  inherited;
end;

function TP3DGUIManager.GetMouseX: Integer;
begin
  Result:= P3DInput.Mouse.X;
end;

function TP3DGUIManager.GetMouseY: Integer;
begin
  Result:= P3DInput.Mouse.Y;
end;

procedure TP3DGUIManager.SetCursor(AValue: TP3DCursor);
begin
  FCursor:=AValue;
  if ( Assigned( FCursorTxt )) then
    FreeAndNil( FCursorTxt );
  if ( ShowCursor ) then
    FCursorTxt:= p3dTextSimple( Char( FCursor ), P3DFontManager[ 'Pascal3D-Cursors', 16 ]);
end;

procedure TP3DGUIManager.SetFocusedControl(AValue: TP3DGraphicControl);
begin
  if FFocusedControl=AValue then Exit;

  FFocusedControl:=AValue;
end;

procedure TP3DGUIManager.Input;
begin
  Controls.Input;
end;

procedure TP3DGUIManager.Render;
begin
  Controls.Render( vec4( 1 ), vec2( 0 ));
  if ( ShowCursor and Assigned( FCursorTxt )) then
    begin
      ScreenCanvas.Lock;
      ScreenCanvas.Font.Color:= vec4( 1 );
      ScreenCanvas.RenderText( FCursorTxt, vec2( MouseX-1, MouseY-1 ));
      ScreenCanvas.RenderText( FCursorTxt, vec2( MouseX-1, MouseY+1 ));
      ScreenCanvas.RenderText( FCursorTxt, vec2( MouseX+1, MouseY-1 ));
      ScreenCanvas.RenderText( FCursorTxt, vec2( MouseX+1, MouseY+1 ));
      ScreenCanvas.Font.Color:= vec4( 0 );
      ScreenCanvas.RenderText( FCursorTxt, vec2( MouseX, MouseY ));
      ScreenCanvas.Unlock();
    end;
end;

procedure TP3DGUIManager.SetMouseX( const Value: Integer );
begin
  P3DInput.Mouse.X:= P3DApplication.ActiveWindow.Left + Value;
end;

procedure TP3DGUIManager.SetMouseY(const Value: Integer);
begin
  P3DInput.Mouse.Y:= P3DApplication.ActiveWindow.Top + Value
end;

procedure TP3DGUIManager.CtrlDeleteNotifier(Ctrl: TP3DGraphicControl);
begin
  if ( DragDropSrc = Ctrl ) then
    DragDropSrc:= nil;
  if ( LastMouseOverCtrl = Ctrl ) then
    LastMouseOverCtrl:= nil;
  if ( LastMouseDownCtrl[ 0 ] = Ctrl ) then
    LastMouseDownCtrl[ 0 ]:= nil;
  if ( LastMouseDownCtrl[ 1 ] = Ctrl ) then
    LastMouseDownCtrl[ 1 ]:= nil;
  if ( LastMouseDownCtrl[ 2 ] = Ctrl ) then
    LastMouseDownCtrl[ 2 ]:= nil;
end;

procedure TP3DGUIManager.SetShowCursor(AValue: Boolean);
begin
  if FShowCursor=AValue then Exit;
  FShowCursor:=AValue;
  SetCursor( Cursor );
end;

procedure TP3DGUIManager.SetWindow(AValue: TP3DWindow);
begin
  if FWindow=AValue then Exit;
  FWindow:=AValue;
  if ( Assigned( Window )) then
    begin
      //UpdateExtents;
      ScreenCanvas.Left:= 0;
      ScreenCanvas.Top:= 0;
      ScreenCanvas.Width:= Window.Width;
      ScreenCanvas.Height:= Window.Height;
    end;
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
