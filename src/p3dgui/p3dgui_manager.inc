{$IFDEF INTERFACE}
type

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
    FCursorOverrideTxt: TP3DTextBmp;
    FCursorOverride: TP3DCursor;
    FWindow: TP3DWindow;
    FCursorTxt: TP3DTextBmp;
    {$IFDEF DEBUG_GUI}
    DebugText: TP3DTextBmp;
    DebugTextPos: TVec2;
    {$ENDIF}

    function GetMouseX: Integer;
    function GetMouseY: Integer;
    procedure SetCursor(AValue: TP3DCursor);
    procedure SetFocusedControl(AValue: TP3DGraphicControl);
    procedure SetMouseX( const Value: Integer );
    procedure SetMouseY( const Value: Integer );
    procedure CtrlDeleteNotifier( Ctrl: TP3DGraphicControl );
    procedure SetShowCursor(AValue: Boolean);
    procedure SetCursorOverride(AValue: TP3DCursor);

    property CursorOverride: TP3DCursor read FCUrsorOverride write SetCursorOverride;

  public
    LastMouseDownCtrl: array[ 0..2 ] of TP3DGraphicControl;
    LastMouseOverCtrl: TP3DGraphicControl;

    constructor Create;
    destructor Destroy; override;

    procedure Render;
    procedure Input;
    procedure UpdateExtents;

  published
    property MouseX: Integer read GetMouseX write SetMouseX;
    property MouseY: Integer read GetMouseY write SetMouseY;
    property ScrollSpeed: Single read FScrollSpeed write FScrollSpeed;
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
  ScreenCanvas.Font.Size:= 48;
  UpdateExtents;
  Cursor:= curArrow;
  CursorOverride:= curArrow;
end;

destructor TP3DGUIManager.Destroy;
begin
  Controls.Clear( True );
  Controls.Free;
  ScreenCanvas.Free;
  FCursorTxt.Free;
  FCursorOverrideTxt.Free;
  inherited;
end;

procedure TP3DGUIManager.SetCursorOverride(AValue: TP3DCursor);
begin
  if FCUrsorOverride=AValue then Exit;
  FCUrsorOverride:=AValue;
  if ( Assigned( FCursorOverrideTxt )) then
    FreeAndNil( FCursorOverrideTxt );
  FCursorOverrideTxt:= TP3DTextBmp.CreateSimple( Char( FCursorOverride ), P3DFontManagerBmp.Fonts[ 'Pascal3D-Cursors' ], 16 );
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
    FCursorTxt:= TP3DTextBmp.CreateSimple( Char( FCursor ), P3DFontManagerBmp.Fonts[ 'Pascal3D-Cursors' ], 24 );
end;

procedure TP3DGUIManager.SetFocusedControl(AValue: TP3DGraphicControl);
var
  OldCtrl: TP3DGraphicControl;
begin
  if FFocusedControl=AValue then Exit;

  OldCtrl:= FFocusedControl; //Delayed event handler prevents deadlock
  FFocusedControl:= AValue;

  If ( Assigned( OldCtrl ) and P3DData.IsValid( OldCtrl )) then
    OldCtrl.SetFocused( False );
end;

procedure TP3DGUIManager.Input;
begin
  if ( Controls.NeedsUpdate ) then
    Controls.UpdateControls;
  Controls.Input;
end;

procedure TP3DGUIManager.Render;
var
  DrawCursor: TP3DTextBmp;
begin
  glDisable( GL_DEPTH_TEST );
  if (( ScreenCanvas.Width <> P3DViewports.Screen.Width ) or
      ( ScreenCanvas.Height <> P3DViewports.Screen.Height )) then
    UpdateExtents;
  Controls.Render();

  {$IFDEF DEBUG_GUI}
  if ( Assigned( DebugText )) then
    begin
      P3DGUIManager.ScreenCanvas.Lock;
      P3DGUIManager.ScreenCanvas.RenderRect( DebugTextPos, DebugTextPos + vec2( DebugText.Width, DebugText.Height ), vec4( 1, 1, 1, 0.5 ));
      ScreenCanvas.RenderText( DebugText, DebugTextPos );
      P3DGUIManager.ScreenCanvas.Unlock();
    end;

  {$ENDIF}

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

      ScreenCanvas.RenderTextShadow( DrawCursor, vec2( MouseX, MouseY ) + 2, 10, Black.Fade( 0.7 ));
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

procedure TP3DGUIManager.UpdateExtents;
begin
  if ( not Assigned( P3DViewports )) then
    exit;

  ScreenCanvas.Left:= P3DViewports.Screen.Left;
  ScreenCanvas.Top:= P3DViewports.Screen.Top;
  ScreenCanvas.Width:= P3DViewports.Screen.Width;
  ScreenCanvas.Height:= P3DViewports.Screen.Height;

  Controls.ClipRect:= ScreenCanvas.Rect;
  Controls.PageRect:= ScreenCanvas.Rect;
end;

{$ENDIF}
