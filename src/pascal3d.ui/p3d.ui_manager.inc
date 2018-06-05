{$IFDEF INTERFACE}
type

TP3DUIViewer = class;
TP3DUIViewerStack = class;

{ TP3DUI }
TP3DUI = class ( TP3DJSONRoot )
  private
    FCanvas: TP3DCanvas2D;
    FCursor: TP3DCursor;
    FFocusedControl: TP3DStreamablePointer;
    FShowCursor: Boolean;
    FCursorOverrideTxt: TP3DTextBmp;
    FCursorOverride: TP3DCursor;
    FCursorTxt: TP3DTextBmp;
    FLastMouseDownCtrl: array[ 0..2 ] of TP3DStreamablePointer;
    FLastMouseOverCtrl: TP3DStreamablePointer;
    FDragDropSrc: TP3DStreamablePointer;
    FViewer: TP3DStreamableContainer;
    FViewerStack: TP3DUIViewerStack;
    {$IFDEF DEBUG_GUI}
    DebugText: TP3DTextBmp;
    DebugTextPos: TVec2;
    {$ENDIF}

    function GetDragDropSrc: TP3DGraphicControl;
    function GetFocusedControl: TP3DGraphicControl;
    function GetLastMouseDownCtrl( AIndex: Integer ): TP3DGraphicControl;
    function GetLastMouseOverCtrl: TP3DGraphicControl;
    function GetScreenCanvas: TP3DCanvas2D;
    function GetViewer: TP3DUIViewer;
    procedure SetCursor(AValue: TP3DCursor);
    procedure SetDragDropSrc(AValue: TP3DGraphicControl);
    procedure SetFocusedControl(AValue: TP3DGraphicControl);
    procedure SetLastMouseDownCtrl( AIndex: Integer ; AValue: TP3DGraphicControl);
    procedure SetLastMouseOverCtrl(AValue: TP3DGraphicControl);
    procedure CtrlDeleteNotifier( Ctrl: TP3DGraphicControl );
    procedure SetShowCursor(AValue: Boolean);
    procedure SetCursorOverride(AValue: TP3DCursor);
    procedure SetViewer(AValue: TP3DUIViewer);

    property CursorOverride: TP3DCursor read FCUrsorOverride write SetCursorOverride;

  public
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
    destructor Destroy; override;

    procedure Render;
    procedure Input;
    procedure Logic;
    procedure Events;
    procedure UpdateExtents;

    property LastMouseDownCtrl[ AIndex: Integer ]: TP3DGraphicControl read GetLastMouseDownCtrl write SetLastMouseDownCtrl;
    property LastMouseOverCtrl: TP3DGraphicControl read GetLastMouseOverCtrl write SetLastMouseOverCtrl;

  published
    property ScreenCanvas: TP3DCanvas2D read GetScreenCanvas;
    property Viewer: TP3DUIViewer read GetViewer write SetViewer;
    property Cursor: TP3DCursor read FCursor write SetCursor;
    property ShowCursor: Boolean read FShowCursor write SetShowCursor;
    property FocusedControl: TP3DGraphicControl read GetFocusedControl write SetFocusedControl;
    property DragDropSrc: TP3DGraphicControl read GetDragDropSrc write SetDragDropSrc;
    property ViewerStack: TP3DUIViewerStack read FViewerStack;

    //property Transform: TP3DTransform read GetTransform;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DUI }

constructor TP3DUI.Create(const AOwner: IP3DStreamableContainer);
var
  i: Integer;
begin
  inherited Create;
  FViewerStack:= TP3DUIViewerStack.Create( 'ViewerStack' );
  Properties.Add( FViewerStack );

  FViewer:= TP3DStreamableContainer.Create( 'Viewer', TP3DUIViewer );
  Properties.Add( FViewer );
  TP3DUIViewer.Create( FViewer );
  Viewer.AutoViewRect:= True;

  FFocusedControl:= TP3DStreamablePointer.Create( 'FocusedControl', TP3DGraphicControl );
  Properties.Add( FFocusedControl );

  FLastMouseOverCtrl:= TP3DStreamablePointer.Create( 'LastMouseOverCtrl', TP3DGraphicControl );
  Properties.Add( FLastMouseOverCtrl );

  for i:= 0 to 2 do
    begin
      FLastMouseDownCtrl[ i ]:= TP3DStreamablePointer.Create( 'LastMouseDownCtrl[' + IntToStr( i ) + ']', TP3DGraphicControl );
      Properties.Add( FLastMouseDownCtrl[ i ]);
      FLastMouseDownCtrl[ i ].AsValue:= nil;
    end;

  FDragDropSrc:= TP3DStreamablePointer.Create( 'DragDropSrc', TP3DGraphicControl );
  Properties.Add( FDragDropSrc );

  {ScreenCanvas:= TP3DCanvas2D.Create( nil );
  ScreenCanvas.Font.Size:= 48;
  UpdateExtents;}
  Cursor:= curArrow;
  CursorOverride:= curArrow;
end;

destructor TP3DUI.Destroy;
begin
  //ScreenCanvas.Free;
  FCursorTxt.Free;
  FCursorOverrideTxt.Free;
  inherited;
end;

procedure TP3DUI.SetCursorOverride(AValue: TP3DCursor);
begin
  if FCUrsorOverride=AValue then Exit;
  FCUrsorOverride:=AValue;
  if ( Assigned( FCursorOverrideTxt )) then
    FreeAndNil( FCursorOverrideTxt );
  FCursorOverrideTxt:= TP3DTextBmp.CreateSimple( Char( FCursorOverride ), P3DFontManagerBmp.Fonts[ 'Pascal3D-Cursors' ], 16 );
end;

procedure TP3DUI.SetViewer(AValue: TP3DUIViewer);
begin
  FViewer.AsValue:= AValue;
end;

function TP3DUI.GetFocusedControl: TP3DGraphicControl;
begin
  Result:= TP3DGraphicControl( FFocusedControl.AsValue );
end;

function TP3DUI.GetDragDropSrc: TP3DGraphicControl;
begin
  Result:= TP3DGraphicControl( FDragDropSrc.AsValue );
end;

function TP3DUI.GetLastMouseDownCtrl( AIndex: Integer ): TP3DGraphicControl;
begin
  Result:= TP3DGraphicControl( FLastMouseDownCtrl[ AIndex ].AsValue );
end;

function TP3DUI.GetLastMouseOverCtrl: TP3DGraphicControl;
begin
  Result:= TP3DGraphicControl( FLastMouseOverCtrl.AsValue );
end;

function TP3DUI.GetScreenCanvas: TP3DCanvas2D;
begin
  Result:= Viewer.ScreenCanvas;
end;

function TP3DUI.GetViewer: TP3DUIViewer;
begin
  Result:= TP3DUIViewer( FViewer.AsValue );
end;

procedure TP3DUI.SetCursor(AValue: TP3DCursor);
begin
  FCursor:=AValue;
  if ( Assigned( FCursorTxt )) then
    FreeAndNil( FCursorTxt );
  if ( ShowCursor ) then
    FCursorTxt:= TP3DTextBmp.CreateSimple( Char( FCursor ), P3DFontManagerBmp.Fonts[ 'Pascal3D-Cursors' ], 24 );
end;

procedure TP3DUI.SetDragDropSrc(AValue: TP3DGraphicControl);
begin
  FDragDropSrc.AsValue:= AValue;
end;

procedure TP3DUI.SetFocusedControl(AValue: TP3DGraphicControl);
var
  OldCtrl: TP3DGraphicControl;
begin
  if ( FFocusedControl.AsValue = AValue ) then Exit;

  OldCtrl:= FocusedControl; //Delayed event handler prevents deadlock
  FFocusedControl.AsValue:= AValue;

  If ( Assigned( OldCtrl ){ and P3DDataBlockCache.IsValid( OldCtrl )}) then
    OldCtrl.SetFocused( False );
end;

procedure TP3DUI.SetLastMouseDownCtrl( AIndex: Integer ; AValue: TP3DGraphicControl);
begin
  FLastMouseDownCtrl[ AIndex ].AsValue:= AValue;
end;

procedure TP3DUI.SetLastMouseOverCtrl(AValue: TP3DGraphicControl);
begin
  if ( AValue = LastMouseOverCtrl ) then
    exit;
  if ( Assigned( LastMouseOverCtrl )) then
    LastMouseOverCtrl.InputState:= LastMouseOverCtrl.InputState - [ gcisMouseOver ];
  FLastMouseOverCtrl.AsValue:= AValue;
  if ( Assigned( AValue )) then
    AValue.InputState:= AValue.InputState + [ gcisMouseOver ];
end;

procedure TP3DUI.Input;
begin
  Viewer.Input;
end;

procedure TP3DUI.Logic;
begin
  Viewer.Controls.Logic();
  UpdateExtents;
end;

procedure TP3DUI.Events;

  function BtnInpState( Idx: Integer ): TP3DGCInputFlags;
  begin
    Result:= [];
    case Idx of
      0: Result:= [ gcisMouseBtn1Down ];
      1: Result:= [ gcisMouseBtn2Down ];
      2: Result:= [ gcisMouseBtn3Down ];
    end;
  end;

var
  i: Integer;
  Accept: Boolean;
begin
  if ( not Assigned( LastMouseOverCtrl ) and ( Cursor <> curArrow )) then
    Cursor:= curArrow
  else if (( Assigned( LastMouseOverCtrl )) and ( LastMouseOverCtrl.Cursor <> Cursor )) then
    Cursor:= LastMouseOverCtrl.Cursor;

  for i:= 0 to 2 do
    if ( P3DInput.Mouse.DButtons[ i ]) then
      if ( P3DInput.Mouse.Buttons[ i ]) then // mouse button pressed
        begin
          LastMouseDownCtrl[ i ]:= LastMouseOverCtrl;
          if ( Assigned( LastMouseDownCtrl[ i ]){ and P3DDataBlockCache.IsValid( P3DUI.LastMouseDownCtrl[ i ])}) then
            LastMouseDownCtrl[ i ].InputState:= LastMouseDownCtrl[ i ].InputState + BtnInpState( i );
          FocusedControl:= LastMouseOverCtrl;
        end
      else                    // mouse button released
        begin
          if ( Assigned( LastMouseDownCtrl[ i ]){ and P3DDataBlockCache.IsValid( P3DUI.LastMouseDownCtrl[ i ])}) then
            LastMouseDownCtrl[ i ].InputState:= LastMouseDownCtrl[ i ].InputState - BtnInpState( i );
          // Drag and Drop release
          if (( DragDropSrc <> nil )) then
            begin
              Accept:= False;
              if ( Assigned( LastMouseOverCtrl ){ and P3DDataBlockCache.IsValid( P3DUI.LastMouseOverCtrl )}) then
                LastMouseOverCtrl.DragDrop( LastMouseOverCtrl, DragDropSrc, LastMouseOverCtrl.UIViewer.MouseX, LastMouseOverCtrl.UIViewer.MouseY, Accept );
              if ( not Accept ) then
                DragDropSrc.DragCancel( DragDropSrc, DragDropSrc.UIViewer.MouseX, DragDropSrc.UIViewer.MouseY );
              DragDropSrc:= nil;
            end;
          LastMouseDownCtrl[ i ]:= nil;
        end;
end;

procedure TP3DUI.Render;
var
  DrawCursor: TP3DTextBmp;
  i: Integer;
begin
  glDisable( GL_DEPTH_TEST ); cle( Self );

  Viewer.Render;

  {if ( ShowCursor ) then
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

      ScreenCanvas.RenderTextShadow( DrawCursor, vec2( MouseX, MouseY ) + 2, 0, Black.Fade( 0.5 ));
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX-1, MouseY-1 ));
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX-1, MouseY+1 ));
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX+1, MouseY-1 ));
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX+1, MouseY+1 ));
      ScreenCanvas.Font.Color:= vec4( 0 );
      ScreenCanvas.RenderText( DrawCursor, vec2( MouseX, MouseY ));
      ScreenCanvas.Unlock();
    end;}

    {$IFDEF DEBUG_GUI}
    ScreenCanvas.Lock;
    ScreenCanvas.Font.Size:= 12;
    ScreenCanvas.Font.Color:= Red300;
    ScreenCanvas.Font.Color:= Black;
    if ( Assigned( LastMouseOverCtrl )) then
      with ( LastMouseOverCtrl ) do
        begin
          //UI.ScreenCanvas.RenderRect( vec2( ClippedRect.Left + ClientRect.Left, ClippedRect.Top + ClientRect.Top ), vec2( Canvas.Left + ClientRect.Left + ClientRect.Right, Canvas.Top + ClientRect.Top + ClientRect.Bottom ), vec4( 1, 1, 1, 0.2 ));
          ScreenCanvas.RenderRect( ClippedRect.TopLeft, ClippedRect.BottomRight, vec4( 0, 0, 0, 0.5 ));

          ScreenCanvas.Font.Size:= 24;
          ScreenCanvas.GetValidText( Format( '%s @[%dx%d] wh[%dx%d]', [ LastMouseOverCtrl.Name, LastMouseOverCtrl.Rect.LeftI, LastMouseOverCtrl.Rect.TopI, LastMouseOverCtrl.Rect.WidthI, LastMouseOverCtrl.Rect.HeightI ]), DebugText );
          DebugTextPos:= vec2( ClippedRect.Left, ClippedRect.Top + ClippedRect.Height - 20 );
          ScreenCanvas.RenderText( DebugText, DebugTextPos );
          ScreenCanvas.RenderLineRect( Controls.PageRect.TopLeft, Controls.PageRect.BottomRight, Red500.Fade( 0.5 ));
          //ScreenCanvas.GetValidText( IntToStr( ParentCtrl.IndexOf( LastMouseOverCtrl )), DebugText );
          //ScreenCanvas.RenderText( DebugText, ClippedRect.TopLeft );

          //UI.ScreenCanvas.RenderText( txt, p );
          //txt.Free;
        end;
    for i:= 0 to 2 do
      if ( Assigned( LastMouseDownCtrl[ i ])) then
        with ( LastMouseDownCtrl[ i ]) do
          P3DUI.ScreenCanvas.RenderRect( ClippedRect.TopLeft, ClippedRect.BottomRight, vec4( Ord( i = 0 ), Ord( i = 1 ), Ord( i = 2 ), 0.2 ));
    ScreenCanvas.Unlock();
    {$ENDIF}
end;

procedure TP3DUI.CtrlDeleteNotifier(Ctrl: TP3DGraphicControl);
begin

end;

procedure TP3DUI.SetShowCursor(AValue: Boolean);
begin
  if FShowCursor=AValue then Exit;
  FShowCursor:=AValue;
  SetCursor( Cursor );
end;

procedure TP3DUI.UpdateExtents;
begin
  //Viewer.UpdateExtents;
  //ScreenCanvas.WidthHeight:= vec2( P3DViewports.Screen.Width, P3DViewports.Screen.Height );
end;


{$ENDIF}
