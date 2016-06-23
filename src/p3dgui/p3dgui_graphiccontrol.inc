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
TP3DGraphicControl = class ( TP3DObject )
  private
    FBoundsBR: TVec2;
    FBoundsTL: TVec2;
    FHideChildren: Boolean;

    procedure SetBoundsBR(AValue: TVec2);
    procedure SetBoundsTL(AValue: TVec2);

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
    FNoBounds: Boolean;
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

    function GetFocused: Boolean; virtual;
    procedure SetFocused(AValue: Boolean); virtual;
    procedure SetCursor(AValue: TP3DCursor);
    function GetParent: TP3DGraphicControl;
    function GetScroll: TVec2;
    function MouseRay( X, Y: Integer ): Boolean; virtual;
    procedure DragCancel( Sender: TP3DGraphicControl; X, Y: Integer );
    procedure DragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean );
    procedure Realign; virtual;
    procedure Render(BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect); virtual;
    procedure SetAlign( const Value: TP3DControlAlign ); virtual;
    procedure SetHeight( const Value: Float ); virtual;
    procedure SetInputState(AValue: TP3DGCInputFlags); virtual;
    procedure SetLeft( const Value: Float ); virtual;
    procedure SetParent(const Value: TP3DGraphicControl);
    procedure SetScroll(AValue: TVec2);
    procedure SetTop( const Value: Float ); virtual;
    procedure SetVisible( const Value: Boolean ); virtual;
    procedure SetWidth( const Value: Float ); virtual;
    procedure UpdateClientRect;

    property HideChildren: Boolean read FHideChildren write FHideChildren;

  public
    constructor Create(const AOwner: TP3DObjectList = nil; const AParent: TP3DGraphicControl=nil);
    destructor Destroy; override;

    procedure BeginDrag();
    procedure BringToFront;
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
    procedure OneLayerDown;
    procedure OneLayerUp;
    procedure SendToBack;
    procedure SetBounds(left, top, right, bottom: Float);

    class function IsFocusControl(): Boolean; virtual;
    class function HasShadow(): Boolean; virtual;

    property BoundsTL: TVec2 read FBoundsTL write SetBoundsTL;
    property BoundsBR: TVec2 read FBoundsBR write SetBoundsBR;
    property Canvas: TP3DCanvas2D read FCanvas write FCanvas;
    property ClientRect: TP3DRect read FClientRect;
    property Controls: TP3DControlList read FControls write FControls;
    property InputState: TP3DGCInputFlags read FInputState write SetInputState;
    property ParentCtrl: TP3DControlList read FOwnerCtrlBuf write FOwnerCtrlBuf;
    property Scroll: TVec2 read GetScroll write SetScroll;

  published
    property Align: TP3DControlAlign read FAlign write SetAlign;
    property Enabled: Boolean read FEnabled write FEnabled;
    property NoBounds: Boolean read FNoBounds write FNoBounds;
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

constructor TP3DGraphicControl.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AOwner );
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
end;

destructor TP3DGraphicControl.Destroy;
begin
  P3DGUIManager.CtrlDeleteNotifier( Self );
  ParentCtrl.Delete( Self );
  Controls.Free;
  Canvas.Free;
  inherited;
end;

procedure TP3DGraphicControl.Draw;
begin
  if ( Assigned( FOnDraw )) then
    FOnDraw( Self, Canvas.Left, Canvas.Top, Canvas.Width, Canvas.Height );
end;

procedure TP3DGraphicControl.SetBoundsBR(AValue: TVec2);
begin
  if FBoundsBR=AValue then Exit;
  FBoundsBR:=AValue;
  Realign;
  Controls.Realign;
end;

procedure TP3DGraphicControl.SetBoundsTL(AValue: TVec2);
begin
  if FBoundsTL=AValue then Exit;
  FBoundsTL:=AValue;
  Realign;
  Controls.Realign;
end;

procedure TP3DGraphicControl.SetBounds( left, top, right, bottom: Float );
begin
  FBoundsTL:= vec2( left,  top );
  FBoundsBR:= vec2( right, bottom );
  Realign;
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


procedure TP3DGraphicControl.Realign;
begin
  if ( Assigned( Parent )) then
    begin
      if ( NoBounds ) then
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
  UpdateClientRect;
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

procedure TP3DGraphicControl.Render(BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect );
var
  OldScroll: TVec2;
  {$IFDEF DEBUG_GUI}
  i: Integer;
  txt: TP3DText;
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
      P3DGUIManager.ScreenCanvas.RenderLineRect( vec2( Canvas.Left + ClientRect.Left, Canvas.Top + ClientRect.Top ), vec2( Canvas.Left + ClientRect.Left + ClientRect.Right, Canvas.Top + ClientRect.Top + ClientRect.Bottom ), vec4( 0, 1, 0, 1 ));
      P3DGUIManager.ScreenCanvas.RenderLineRect( vec2( Canvas.Left, Canvas.Top ), vec2( Canvas.Left + Canvas.Width, Canvas.Top + Canvas.Height ), vec4( 1, 0, 0, 1 ));

      txt:= p3dTextSimple( LastMouseOverCtrl.Name, P3DFontManager.Fonts[ Canvas.Font.Name, Canvas.Font.Size ]);
      p:= vec2( Canvas.Left, Canvas.Top + Canvas.Height + 5 );
      P3DGUIManager.ScreenCanvas.RenderRect( p, p + vec2( txt.Width, txt.Height ), vec4( 1, 1, 1, 0.5 ));
      P3DGUIManager.ScreenCanvas.RenderText( txt, p );
      txt.Free;
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
end;

procedure TP3DGraphicControl.SetLeft(const Value: Float);
begin
  if ( Align = alNone ) then
    begin
      FLeft:= Value;
      Realign;
      Controls.Realign; //Or Update Canvas of clients
    end;
end;

procedure TP3DGraphicControl.SetParent( const Value: TP3DGraphicControl );
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


procedure TP3DGraphicControl.SetTop(const Value: Float);
begin
  if ( Align = alNone ) then
    begin
      FTop:= Value;
      Realign;
      Controls.Realign; //Or Update Canvas of clients
    end;
end;

procedure TP3DGraphicControl.SetVisible( const Value: Boolean );
begin
  if ( FVisible <> Value ) then
    begin
      FVisible:= Value;
      if ( Align <> alNone ) then
        FOwnerCtrlBuf.Realign;
    end;
end;

procedure TP3DGraphicControl.SetWidth(const Value: Float);
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
  FClientRect:= P3DRect( BoundsTL.X, BoundsTL.Y, Width - BoundsTL.X - BoundsBR.X, Height - BoundsTL.Y - BoundsBR.Y );
end;

function TP3DGraphicControl.MouseRay(X, Y: Integer): Boolean;
begin
 Result:= PtInRect( Rect( Canvas.Left, Canvas.Top, Canvas.Left + Canvas.Width, Canvas.Top + Canvas.Height ), Point( X, Y ))
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
