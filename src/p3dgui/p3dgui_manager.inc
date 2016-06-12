{$IFDEF INTERFACE}

TP3DCursor = ( curArrow = 'A', curHourGlass = 'B', curHResize = 'C', curVResize = 'D', curMoveLeftRight = 'E', curMoveTopBottom = 'F', curNodeEdit = 'G', curDragDrop = 'H' );

{ TP3DGUIManager }

TP3DGUIManager = class
  private
    FCanvas: TP3DCanvas2D;
    FControls: TP3DControlList;
    FCursor: TP3DCursor;
    FFocusedControl: TP3DGraphicControl;
    FScrollSpeed: Single;
    FShowCursor: Boolean;
    FCursorOverrideTxt: TP3DText;
    FCursorOverride: TP3DCursor;
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
    procedure SetCursorOverride(AValue: TP3DCursor);

    property CursorOverride: TP3DCursor read FCUrsorOverride write SetCursorOverride;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Render;
    procedure Input;
    procedure UpdateExtents;

  published
    property MouseX: Integer read GetMouseX write SetMouseX;
    property MouseY: Integer read GetMouseY write SetMouseY;
    property ScrollSpeed: Single read FScrollSpeed write FScrollSpeed;
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

  ScrollSpeed:= -50;
  Controls:= TP3DControlList.Create( nil );
  ScreenCanvas:= TP3DCanvas2D.Create( nil );
  Cursor:= curArrow;
  CursorOverride:= curArrow;
  if ( Assigned( P3DApplication ) and Assigned( P3DApplication.MainWindow )) then
    Window:= P3DApplication.MainWindow;
end;

destructor TP3DGUIManager.Destroy;
begin
  ScreenCanvas.Free;
  FCursorTxt.Free;
  FCursorOverrideTxt.Free;
  Controls.Clear( True );
  Controls.Free;
  inherited;
end;

procedure TP3DGUIManager.SetCursorOverride(AValue: TP3DCursor);
begin
  if FCUrsorOverride=AValue then Exit;
  FCUrsorOverride:=AValue;
  FCursorOverrideTxt:= p3dTextSimple( Char( FCUrsorOverride ), P3DFontManager[ 'Pascal3D-Cursors', 16 ]);
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
var
  DrawCursor: TP3DText;
begin
  glDisable( GL_DEPTH_TEST );
  if (( ScreenCanvas.Width <> P3DViewports[ 0 ].Width ) or
      ( ScreenCanvas.Height <> P3DViewports[ 0 ].Height )) then
    UpdateExtents;
  Controls.Render( vec4( 1 ), vec2( 0 ));

  if ( ShowCursor ) then
    begin
      if ( DragDropSrc <> nil ) then
        begin
          CursorOverride:= curDragDrop;
          DrawCursor:= FCursorOverrideTxt;
        end
      else
        DrawCursor:= FCursorTxt;

      if ( not Assigned( DrawCursor )) then
        exit;
      ScreenCanvas.Lock;
      ScreenCanvas.Font.Color:= vec4( 1 );
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX-1, MouseY-1 ));
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX-1, MouseY+1 ));
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX+1, MouseY-1 ));
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX+1, MouseY+1 ));
      ScreenCanvas.Font.Color:= vec4( 0 );
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX, MouseY ));
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
      ScreenCanvas.Left:= 0;
      ScreenCanvas.Top:= 0;
      ScreenCanvas.Width:= Window.Width;
      ScreenCanvas.Height:= Window.Height;
      UpdateExtents;
    end;
end;

procedure TP3DGUIManager.UpdateExtents;
begin
  if ( not Assigned( P3DViewports )) then
    exit;

  if ( P3DViewports.Count = 0 ) then
    if ( Assigned( Window )) then
      Window.UpdateViewport
    else
      exit;

  ScreenCanvas.Left:= P3DViewports.VP[ 0 ].Left;
  ScreenCanvas.Top:= P3DViewports.VP[ 0 ].Top;
  ScreenCanvas.Width:= P3DViewports.VP[ 0 ].Width;
  ScreenCanvas.Height:= P3DViewports.VP[ 0 ].Height;

  Controls.Realign;
end;

{$ENDIF}
