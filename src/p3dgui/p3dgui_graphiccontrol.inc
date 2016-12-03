{$IFDEF INTERFACE}


TP3DGraphicControlClass = class of TP3DGraphicControl;

{ TP3DGraphicControl }
                            // 1                  2                4               8              16
TP3DGCInputState = ( gcisEmpty, gcisMouseBtn1Down, gcisMouseBtn2Down, gcisMouseBtn3Down, gcisMouseOver {, gcisMouseOverOccl} );
TP3DGCInputFlags = set of TP3DGCInputState;
// 1 = M1
// 2 = M2
// 3 = M1 + M2
// 4 = M3
// 5 = M1 + M3
// 6 = M2 + M3
// 7 = M1 + M2 + M3
// 8 = MO
// 9 = MO + M1
// 10 = MO + M2
// 11 = MO + M1 + M2
// 12 = MO + M3
// 13 = MO + M1 + M3
// 14 = MO + M2 + M3
// 15 = MO + M1 + M2 + M3
//...

TP3DNotifyEvent = procedure ( Sender: TP3DGraphicControl ) of object;
TP3DGraphicControl = class ( TP3DDataBlock )
  private
    FMarginBR: TVec2;
    FMarginTL: TVec2;
    FHideChildren: Boolean;

    function GetLeftTop: TVec2;
    procedure SetMarginBR(AValue: TVec2);
    procedure SetMarginTL(AValue: TVec2);
    procedure SetLeftTop(AValue: TVec2);

  protected
    FAlign: TP3DControlAlign;
    FCanvas: TP3DCanvas2D;
    FClientRect: TP3DRect;
    FControls: TP3DControlList;
    FEnabled: Boolean;
    FInputState: TP3DGCInputFlags;
    FLastClick: Integer;
    FLeft: Float;
    FManager: TP3DGUIManager;
    FNoMargin: Boolean;
    FOnDragDrop: TP3DGUIDragDrop;
    FOnDragDropCancel: TP3DGUIMouseEvent;
    FOnDraw: TP3DGUIDraw;
    FOnKeyDown: TNotifyEvent;
    FOnMouseClick: TP3DGUIMouseClick;
    FOnMouseDown: TP3DGUIMouseClick;
    FOnMouseEnter: TP3DGUIMouseEvent;
    FOnMouseLeave: TP3DGUIMouseEvent;
    FOnMouseMove: TP3DGUIMouseEvent;
    FOnMouseUp: TP3DGUIMouseClick;
    FOnParentRealign: TNotifyEvent;
    FOwnerCtrlBuf: TP3DControlList;
    FTag: Integer;
    FTop: Float;
    FVisible: Boolean;
    FWidth: Float;
    FCursor: TP3DCursor;
    FHeight: Float;
    FMaxHeight: Float;
    FMaxWidth: Float;
    FMinHeight: Float;
    FMinWidth: Float;

    //Various getters and setters

    function GetFocused: Boolean; virtual;
    procedure SetFocused( AValue: Boolean ); virtual;
    procedure SetCursor( AValue: TP3DCursor );
    function GetParent: TP3DGraphicControl;
    function GetScroll: TVec2;
    procedure SetAlign( const Value: TP3DControlAlign ); virtual;
    procedure SetHeight( const Value: Float ); virtual;
    procedure SetInputState(AValue: TP3DGCInputFlags); virtual;
    procedure SetLeft( Value: Float ); virtual;
    procedure SetParent( Value: TP3DGraphicControl);
    procedure SetScroll(AValue: TVec2);
    procedure SetTop( Value: Float ); virtual;
    procedure SetVisible( Value: Boolean ); virtual;
    procedure SetWidth( Value: Float ); virtual;

    function MouseRay( X, Y: Integer ): Boolean; virtual; //Check if a ray at the given Pixel is inside of the control (usually the client rect).
                                                          //Can be overridden for to change this behaviour
    procedure UpdateExtents; virtual; //Update both client and canvas extents
    procedure UpdateCanvasArea; virtual; //Update canvas draw area
    procedure UpdateClientRect; virtual; //Recalculate the client rect
    procedure DragCancel( Sender: TP3DGraphicControl; X, Y: Integer ); //Cancel dragging.
    procedure DragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean ); //This procedure is called internally when the mouse is released on the control during dragging. It can be overriden to make this control a drag target.
    procedure Realign( var ClientArea: TP3DRect ); virtual; //Realign the control. Is called from TP3DControlList when parent changes
    procedure Paint( BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect ); virtual; //Prepare the canvas for drawing and call draw procedure.

    property HideChildren: Boolean read FHideChildren write FHideChildren; //Do not children's render procedure

  public
    constructor Create( const AData: TP3DData = nil; const AParent: TP3DGraphicControl = nil ); virtual;
    constructor Create( const AData: TP3DData = nil ); override;
    procedure FastDestroy; override;

    procedure BeginDrag(); //Start drag and drop with control as drag source
    procedure Draw; virtual;
    procedure KeyboardAction(); virtual;
    procedure KeyDown; virtual;
    procedure MouseAction( X, Y: Integer; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean ); virtual;
    procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
    procedure MouseDblClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
    procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
    procedure MouseEnter( X, Y: Integer ); virtual;
    procedure MouseLeave( X, Y: Integer ); virtual;
    procedure MouseMove( X, Y: Integer ); virtual;
    procedure MouseUp( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
    procedure MouseWheel( delta: Integer ); virtual;

    procedure BringToFront; //Bring control to the front layer in control list
    procedure OneLayerDown; //Move control one layer down in control list.
    procedure OneLayerUp; //Move control one layer up in control list.
    procedure SendToBack; //Send control to the back
    procedure SetMargin( left, top, right, bottom: Float ); //Set the control's margin at once.

    class function IsFocusControl(): Boolean; virtual;
    class function HasShadow(): Boolean; virtual;

    property MarginTL: TVec2 read FMarginTL write SetMarginTL;
    property MarginBR: TVec2 read FMarginBR write SetMarginBR;
    property Canvas: TP3DCanvas2D read FCanvas write FCanvas;
    property ClientRect: TP3DRect read FClientRect;
    property Controls: TP3DControlList read FControls write FControls;
    property InputState: TP3DGCInputFlags read FInputState write SetInputState;
    property ParentCtrl: TP3DControlList read FOwnerCtrlBuf write FOwnerCtrlBuf;
    property Scroll: TVec2 read GetScroll write SetScroll;
    property LeftTop: TVec2 read GetLeftTop write SetLeftTop;

  published
    property Align: TP3DControlAlign read FAlign write SetAlign;
    property Enabled: Boolean read FEnabled write FEnabled;
    property NoMargin: Boolean read FNoMargin write FNoMargin;
    property Parent: TP3DGraphicControl read GetParent write SetParent;
    property Tag: Integer read FTag write FTag;
    property Visible: Boolean read FVisible write SetVisible;
    property Focused: Boolean read GetFocused write SetFocused;

    property Left: Float read FLeft write SetLeft;
    property Top: Float read FTop write SetTop;
    property Width: Float read FWidth write SetWidth;
    property Height: Float read FHeight write SetHeight;

    property MinWidth: Float read FMinWidth write FMinWidth;
    property MinHeight: Float read FMinHeight write FMinHeight;
    property MaxWidth: Float read FMaxWidth write FMaxWidth;
    property MaxHeight: Float read FMaxHeight write FMaxHeight;

    property OnDragDrop: TP3DGUIDragDrop read FOnDragDrop write FOnDragDrop;
    property OnDragDropCancel: TP3DGUIMouseEvent read FOnDragDropCancel write FOnDragDropCancel;
    property OnDraw: TP3DGUIDraw read FOnDraw write FOnDraw;
    property OnKeyDown: TNotifyEvent read FOnKeyDown write FOnKeyDown;
    property OnMouseClick: TP3DGUIMouseClick read FOnMouseClick write FOnMouseClick;
    property OnMouseDown: TP3DGUIMouseClick read FOnMouseDown write FOnMouseDown;
    property OnMouseEnter: TP3DGUIMouseEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TP3DGUIMouseEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseMove: TP3DGUIMouseEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TP3DGUIMouseClick read FOnMouseUp write FOnMouseUp;
    property OnParentRealign: TNotifyEvent read FOnParentRealign write FOnParentRealign;
    property Cursor: TP3DCursor read FCursor write SetCursor;
end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DGraphicControl }

constructor TP3DGraphicControl.Create(const AData: TP3DData;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AData );
  Controls:= TP3DControlList.Create( Self );
  FCanvas:= TP3DCanvas2D.Create( Self );
  if ( Assigned( AParent )) then
    ParentCtrl:= AParent.Controls
  else
    ParentCtrl:= P3DGUIManager.Controls;
  ParentCtrl.Add( Self );

  Visible:= True;
  Enabled:= True;
  Scroll.X:= 0;
  Scroll.Y:= 0;
  Width:= 50;
  Height:= 50;
  Cursor:= curArrow;

  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Left' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Top' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Width' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Height' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Left' ));

  Properties.Add( TP3DPropertyAccessVec2.CreateGetSet( 'LeftTop', @GetLeftTop, @SetLeftTop ));
  Properties.Add( TP3DPropertyAccessVec2.CreateGetSet( 'Scroll', @GetScroll, @SetScroll ));
  //TODO: Add rest of graphiccontrol properties

end;

constructor TP3DGraphicControl.Create(const AData: TP3DData);
begin
  Create( AData, nil );
end;

procedure TP3DGraphicControl.FastDestroy;
begin
  P3DGUIManager.CtrlDeleteNotifier( Self );
  ParentCtrl.Remove( Self );
  Controls.Free;
  Canvas.Free;
  inherited;
end;

procedure TP3DGraphicControl.Draw;
begin
  if ( Assigned( FOnDraw )) then
    FOnDraw( Self, Canvas.Left, Canvas.Top, Canvas.Width, Canvas.Height );
end;

procedure TP3DGraphicControl.SetMarginBR(AValue: TVec2);
begin
  if FMarginBR=AValue then Exit;
  FMarginBR:=AValue;
  UpdateExtents;
  Controls.Realign;
end;

function TP3DGraphicControl.GetLeftTop: TVec2;
begin
  Result:= vec2( Left, Top );
end;

procedure TP3DGraphicControl.SetMarginTL(AValue: TVec2);
begin
  if FMarginTL=AValue then Exit;
  FMarginTL:=AValue;
  UpdateExtents;
  Controls.Realign;
end;

procedure TP3DGraphicControl.SetLeftTop(AValue: TVec2);
begin
  FLeft:= AValue.x;
  Top:= AValue.y;
end;

procedure TP3DGraphicControl.SetMargin( left, top, right, bottom: Float );
begin
  FMarginTL:= vec2( left,  top );
  FMarginBR:= vec2( right, bottom );
  UpdateExtents;
  Controls.Realign;
end;

function TP3DGraphicControl.GetFocused: Boolean;
begin
  Result:= Self = P3DGUIManager.FocusedControl;
end;

procedure TP3DGraphicControl.SetFocused(AValue: Boolean);
begin
  if ( AValue ) then
    P3DGUIManager.FocusedControl:= Self
  else if ( P3DGUIManager.FocusedControl = Self ) then
    P3DGUIManager.FocusedControl:= nil;
end;

procedure TP3DGraphicControl.SetCursor(AValue: TP3DCursor);
begin
  if FCursor=AValue then Exit;
  FCursor:=AValue;
end;

function TP3DGraphicControl.GetParent: TP3DGraphicControl;
begin
  Result:= ParentCtrl.Parent;
end;

procedure TP3DGraphicControl.KeyDown;
begin
  if ( Assigned( FOnKeyDown )) then
    FOnKeyDown( Self );
  Controls.Input;
end;

procedure TP3DGraphicControl.MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  if (( X >= 0 ) and ( Y >= 0 ) and ( X <= Canvas.Width ) and ( Y <= Canvas.Height )) then
    if ( Assigned( FOnMouseClick )) then
      FOnMouseClick( Self, mb1, mb2, mb3, X, Y );
  if ( FLastClick >= SDL_GetTicks ) then
    MouseDblClick( mb1, mb2, mb3, X, Y );
  FLastClick:= SDL_GetTicks + 500;
end;

procedure TP3DGraphicControl.MouseDblClick( mb1, mb2, mb3: Boolean; X,
  Y: Integer );
begin

end;

procedure TP3DGraphicControl.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if ( Assigned( FOnMouseDown )) then
    FOnMouseDown( Self, mb1, mb2, mb3, X, Y );
end;

procedure TP3DGraphicControl.MouseEnter( X, Y: Integer );
begin
  if ( Assigned( FOnMouseEnter )) then
    FOnMouseEnter( Self, X, Y );
end;

procedure TP3DGraphicControl.MouseLeave( X, Y: Integer );
begin
  if ( Assigned( FOnMouseLeave )) then
    FOnMouseLeave( Self, X, Y );
end;

procedure TP3DGraphicControl.MouseAction(X, Y: Integer; mb1, mb2, mb3, dmb1,
  dmb2, dmb3: Boolean);
var
  cx: Integer;
  cy: Integer;
  Ctrl: TP3DGraphicControl;
begin
  cx:= X - Canvas.Left;
  cy:= Y - Canvas.Top;
  if (( P3DInput.Mouse.DX <> 0 ) or ( P3DInput.Mouse.DY <> 0 )) then
    MouseMove( cx, cy );
  if ( P3DInput.Mouse.DZ <> 0 ) then
    MouseWheel( P3DInput.Mouse.DZ );
  for Ctrl in Controls do
    Ctrl.MouseAction( X, Y, mb1, mb2, mb3, dmb1, dmb2, dmb3 );
end;

procedure TP3DGraphicControl.MouseMove( X, Y: Integer );
  function Button0Released: Boolean; inline;
  begin
    Result:= P3DInput.Mouse.DButtons[ 0 ] and ( not P3DInput.Mouse.Buttons[ 0 ]);
  end;

begin
  {TODO: Only call this if mouse is in client rect.}
  if ( Assigned( FOnMouseMove )) then
    FOnMouseMove( Self, X, Y );
end;

procedure TP3DGraphicControl.MouseUp(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if ( Assigned( FOnMouseUp )) then
    FOnMouseUp( Self, mb1, mb2, mb3, X, Y );
  if ( gcisMouseOver in InputState ) then //ONLY FIRE EVENT IF THE BUTTON IS RELEASED INSIDE OF THE CONTROL
    MouseClick( mb1, mb2, mb3, X, Y );
end;

procedure TP3DGraphicControl.MouseWheel(delta: Integer);
begin

end;

procedure TP3DGraphicControl.KeyboardAction;
var
  Ctrl: TP3DGraphicControl;
begin
  for Ctrl in Controls do
    Ctrl.KeyboardAction();
end;

procedure TP3DGraphicControl.OneLayerDown;
begin
  FOwnerCtrlBuf.OneLayerDown( FOwnerCtrlBuf.IndexOf( Self ));
end;

procedure TP3DGraphicControl.OneLayerUp;
begin
  FOwnerCtrlBuf.OneLayerUp( FOwnerCtrlBuf.IndexOf( Self ));
end;

procedure TP3DGraphicControl.BringToFront;
begin
  FOwnerCtrlBuf.BringToFront( FOwnerCtrlBuf.IndexOf( Self ));
end;

procedure TP3DGraphicControl.SendToBack;
begin
  FOwnerCtrlBuf.SendToBack( FOwnerCtrlBuf.IndexOf( Self ));
end;

class function TP3DGraphicControl.IsFocusControl: Boolean;
begin
  Result:= False;
end;

class function TP3DGraphicControl.HasShadow: Boolean;
begin
  Result:= False;
end;


procedure TP3DGraphicControl.Realign(var ClientArea: TP3DRect);
begin
  case Align of //TODO: Fix Bug that Controls positioned at the ClientRect should have a zero position value not the R.TopLeft
    alClient:
      begin
        FLeft:= ClientArea.Left;
        FTop:= ClientArea.Top;
        FWidth:= ClientArea.Right - ClientArea.Left;
        FHeight:= ClientArea.Bottom - ClientArea.Top;
        //FPageSize:= max( FPageSize, vec2( Left + Width, Top + Height ));
      end;
    alLeft:
      begin
        FLeft:= ClientArea.Left;
        FHeight:= ClientArea.Height;
        ClientArea.Left:= ClientArea.Left + Width;
        ClientArea.Width:= ClientArea.Width - Width;
      end;
    alRight:
      begin
        FLeft:= ClientArea.Left + ClientArea.Width - Width;
        FTop:= ClientArea.Top;
        FHeight:= ClientArea.Height;
        ClientArea.Right:= ClientArea.Right - Width;
      end;
    alTop:
      begin
        FTop:= ClientArea.Top;
        FLeft:= ClientArea.Left;
        FWidth:= ClientArea.Width;
        ClientArea.Top:= ClientArea.Top + Height;
        ClientArea.Height:= ClientArea.Height - Height;
      end;
    alBottom:
      begin
        FLeft:= ClientArea.Left;
        FWidth:= ClientArea.Width;
        FTop:= ClientArea.Bottom - Height;
        ClientArea.Height:= ClientArea.Height - Height;
      end;
  end;

  //ParentCtrl.FPageSize:= max( ParentCtrl.FPageSize, vec2( Left + Width, Top + Height ));
end;

function TP3DGraphicControl.GetScroll: TVec2;
begin
  Result:= Canvas.Scroll;
end;

procedure TP3DGraphicControl.SetInputState(AValue: TP3DGCInputFlags);
var
  mx: Integer;
  my: Integer;
  DIS: TP3DGCInputFlags;
  OldInputState: TP3DGCInputFlags;

  function FlagSet( Flag: TP3DGCInputState ): Boolean;
  begin
    Result:= Flag in ( AValue * DIS );
  end;

  function FlagWasSet( Flag: TP3DGCInputState ): Boolean;
  begin
    Result:= Flag in ( OldInputState * DIS );
  end;

  function MDwn: Boolean;
  begin
    Result:= ( DIS * AValue ) * [ gcisMouseBtn1Down, gcisMouseBtn2Down, gcisMouseBtn3Down ] <> [];
  end;

  function MUp(): Boolean;
  begin
    Result:= ( DIS * OldInputState ) * [ gcisMouseBtn1Down, gcisMouseBtn2Down, gcisMouseBtn3Down ] <> [];
  end;

begin
  if FInputState=AValue then Exit;

  mx:= P3DGUIManager.MouseX - Canvas.Left;
  my:= P3DGUIManager.MouseY - Canvas.Top;

  DIS:= AValue >< InputState;

  OldInputState:= InputState;
  FInputState:=AValue;

  if ( gcisMouseOver in DIS ) then
    if ( gcisMouseOver in AValue ) then
      MouseEnter( mx, my )
    else
      MouseLeave( mx, my );
  if ([ gcisMouseBtn1Down, gcisMouseBtn2Down, gcisMouseBtn3Down ] * DIS <> []) then
    begin
      if ( MDwn()) then
        MouseDown( FlagSet( gcisMouseBtn1Down ), FlagSet( gcisMouseBtn2Down ), FlagSet( gcisMouseBtn3Down ), mx, my );
      if ( MUp()) then
        MouseUp( FlagWasSet( gcisMouseBtn1Down ), FlagWasSet( gcisMouseBtn2Down ), FlagWasSet( gcisMouseBtn3Down ), mx, my );
    end;
end;

procedure TP3DGraphicControl.SetScroll(AValue: TVec2);
begin
  Canvas.Scroll:= AValue;
end;

procedure TP3DGraphicControl.Paint(BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect);
var
  OldScroll: TVec2;
  {$IFDEF DEBUG_GUI}
  i: Integer;
  txt: TP3DTextBmp;
  {$ENDIF}
  p: TVec2;
  R: TP3DRect;
begin
  R:= P3DRect( Canvas.Left, Canvas.Top, Canvas.Width, Canvas.Height ).Clip( ClipRect );
  if ( R.Width * R.Height = 0 ) then
    exit;

  if ( HasShadow()) then
    begin
      p:= vec2( Canvas.Left + 2, Canvas.Top + 2 );
      P3DGUIManager.ScreenCanvas.Lock;
      P3DGUIManager.ScreenCanvas.RenderRectShadow( p, p + vec2( Canvas.Width, Canvas.Height ), 5, vec4( 0, 0, 0, 0.1 ));
      P3DGUIManager.ScreenCanvas.Unlock();
    end;

  Canvas.Lock;
  OldScroll:= Scroll;
  Canvas.Scroll:= ScrollAcc + Scroll + vec2( R.Left, R.Top ) - vec2( Canvas.Left, Canvas.Top );
  Canvas.Width:= R.WidthI;
  Canvas.Height:= R.HeightI;

  //Canvas.ScreenColor:= vec4( 1 * Ord( gcisMouseBtn1Down in InputState ), 1 * Ord( gcisMouseBtn2Down in InputState ), 1 * Ord( gcisMouseBtn3Down in InputState ), 0.25 + 0.75 * Ord( gcisMouseOver in InputState ) + 0.25 * Ord( gcisMouseOverOccl in InputState ));

  Draw;

  Canvas.Unlock();
  Canvas.Scroll:= OldScroll;
  if ( not HideChildren ) then
    Controls.Render( BaseColor, ScrollAcc + Scroll, R );
  {$IFDEF DEBUG_GUI}
  if ( Self = LastMouseOverCtrl ) then
    begin
      P3DGUIManager.ScreenCanvas.Lock;
      //P3DGUIManager.ScreenCanvas.RenderRect( vec2( Canvas.Left + ClientRect.Left, Canvas.Top + ClientRect.Top ), vec2( Canvas.Left + ClientRect.Left + ClientRect.Right, Canvas.Top + ClientRect.Top + ClientRect.Bottom ), vec4( 1, 1, 1, 0.2 ));
      P3DGUIManager.ScreenCanvas.RenderRect( vec2( Canvas.Left, Canvas.Top ), vec2( Canvas.Left + Canvas.Width, Canvas.Top + Canvas.Height ), vec4( 0, 0, 0, 0.5 ));

      P3DGUIManager.ScreenCanvas.GetValidText( Format( '%s [%dx%d]', [ LastMouseOverCtrl.Name, round( LastMouseOverCtrl.Width ), round( LastMouseOverCtrl.Height )]), P3DGUIManager.DebugText );
      P3DGUIManager.DebugTextPos:= vec2( Canvas.Left, Canvas.Top + Canvas.Height - 20 );
      //P3DGUIManager.ScreenCanvas.RenderText( txt, p );
      //txt.Free;
      P3DGUIManager.ScreenCanvas.Unlock();
    end;
  for i:= 0 to 2 do
    if ( Self = LastMouseDownCtrl[ i ]) then
      begin
        P3DGUIManager.ScreenCanvas.Lock;
        P3DGUIManager.ScreenCanvas.RenderRect( vec2( Canvas.Left, Canvas.Top ), vec2( Canvas.Left + Canvas.Width, Canvas.Top + Canvas.Height ), vec4( Ord( i = 0 ), Ord( i = 1 ), Ord( i = 2 ), 0.2 ));
        P3DGUIManager.ScreenCanvas.Unlock();
      end;
  {$ENDIF}
end;


procedure TP3DGraphicControl.SetAlign( const Value: TP3DControlAlign );
begin
  FAlign:= Value;
  if ( Visible ) then
    ParentCtrl.Realign;
end;

procedure TP3DGraphicControl.SetHeight(const Value: Float);
var
  Val: Float;
begin
  if ( Align in [ alClient, alLeft, alRight ]) then
    exit;

  Val:= Max( 1, Value );
  if ( FMinHeight > 0 ) then
    Val:= Max( FMinHeight, Val );
  if ( FMaxHeight > 0 ) then
    Val:= Min( FMaxHeight, Val );
  FHeight:= Val;

  if ( Align <> alNone ) then
    ParentCtrl.Realign
  else
    Controls.Realign;
  ParentCtrl.FPageSize:= max( ParentCtrl.FPageSize, vec2( Left + Width, Top + Height ));
end;

procedure TP3DGraphicControl.SetLeft( Value: Float );
begin
  if ( Align = alNone ) then
    begin
      FLeft:= Value;
      UpdateExtents;
      Controls.Realign; //Or Update Canvas of clients
    end;
end;

procedure TP3DGraphicControl.SetParent( Value: TP3DGraphicControl );
begin
  if ( Value = Parent ) then
    exit;
  FOwnerCtrlBuf.Delete( Self );
  if ( Value = nil ) then
    exit;
  Value.Controls.Add( Self );
  FOwnerCtrlBuf:= Value.Controls;
  Parent.Controls.Realign;
end;


procedure TP3DGraphicControl.SetTop( Value: Float );
begin
  if ( Align = alNone ) then
    begin
      FTop:= Value;
      UpdateExtents;
      Controls.Realign; //Or Update Canvas of clients
    end;
end;

procedure TP3DGraphicControl.SetVisible( Value: Boolean );
begin
  if ( FVisible <> Value ) then
    begin
      FVisible:= Value;
      if ( Align <> alNone ) then
        FOwnerCtrlBuf.Realign;
    end;
end;

procedure TP3DGraphicControl.SetWidth( Value: Float );
var
  Val: Float;
begin
  if ( Align in [ alClient, alTop, alBottom ]) then
    exit;

  Val:= Max( 1, Value );
  if ( FMinWidth > 0 ) then
    Val:= Max( FMinWidth, Val );
  if ( FMaxWidth > 0 ) then
    Val:= Min( FMaxWidth, Val );
  FWidth:= Val;

  if ( Align <> alNone ) then
    ParentCtrl.Realign
  else
    Controls.Realign;
end;


procedure TP3DGraphicControl.UpdateClientRect;
begin
  FClientRect:= P3DRect( MarginTL.X, MarginTL.Y, Width - MarginTL.X - MarginBR.X, Height - MarginTL.Y - MarginBR.Y );
end;

function TP3DGraphicControl.MouseRay(X, Y: Integer): Boolean;
begin
 Result:= PtInRect( Rect( Canvas.Left, Canvas.Top, Canvas.Left + Canvas.Width, Canvas.Top + Canvas.Height ), Point( X, Y ))
end;

procedure TP3DGraphicControl.UpdateExtents;
begin
  UpdateClientRect;
  UpdateCanvasArea;
end;

procedure TP3DGraphicControl.UpdateCanvasArea;
begin
  if ( Assigned( Parent )) then
    begin
      if ( NoMargin ) then
        begin
          Canvas.Left:= Round( Left + Parent.Canvas.Left );
          Canvas.Top:= Round( Top + Parent.Canvas.Top );
          Canvas.Width:= Round( Min( Parent.Width - Left, Width ));
          Canvas.Height:= Round( Min( Parent.Height - Top, Height ));
        end
      else
        begin
          Canvas.Left:= Round( Left + Parent.Canvas.Left + Parent.ClientRect.Left );
          Canvas.Top:= Round( Top + Parent.Canvas.Top + Parent.ClientRect.Top );
          Canvas.Width:= Round( Min( Parent.ClientRect.Right - Left, Width ));
          Canvas.Height:= Round( Min( Parent.ClientRect.Bottom - Top, Height ));
        end;
    end
  else
    begin
      Canvas.Left:= Round( Left );
      Canvas.Top:= Round( Top );
      Canvas.Width:= Round( Min( P3DGUIManager.ScreenCanvas.Width - Left, Width ));
      Canvas.Height:= Round( Min( P3DGUIManager.ScreenCanvas.Height - Top, Height ));
    end;
end;

procedure TP3DGraphicControl.BeginDrag;
begin
  DragDropSrc:= Self;
end;

procedure TP3DGraphicControl.DragDrop(Sender, Source: TP3DGraphicControl; X,
  Y: Integer; var Accept: Boolean);
begin
  if ( Assigned( FOnDragDrop )) then
    FOnDragDrop( Sender, Source, X, Y, Accept );
end;

procedure TP3DGraphicControl.DragCancel(Sender: TP3DGraphicControl; X,
  Y: Integer);
begin
  if ( Assigned( FOnDragDropCancel )) then
    FOnDragDropCancel( Sender, X, Y );
end;

{$ENDIF}
