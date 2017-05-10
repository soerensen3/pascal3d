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
    FMargin: TVec4;
    FHideChildren: Boolean;
    FNeedsUpdate: Boolean;
    FPointerTag: Pointer;
    FRect: TP3DRect;

    function GetBottomRight: TVec2; virtual;
    function GetHeight: Single; virtual;
    function GetLeft: Single; virtual;
    function GetPageModeX: TP3DPageMode;
    function GetPageModeY: TP3DPageMode;
    function GetPageSize: TVec2;
    function GetTop: Single; virtual;
    function GetTopLeft: TVec2; virtual;
    function GetWidth: Single; virtual;
    function GetWidthHeight: TVec2; virtual;
    procedure SetBottomRight(AValue: TVec2); virtual;
    procedure SetHeight(AValue: Single); virtual;
    procedure SetLeft(AValue: Single); virtual;
    procedure SetPageModeX(AValue: TP3DPageMode);
    procedure SetPageModeY(AValue: TP3DPageMode);
    procedure SetTop(AValue: Single); virtual;
    procedure SetMargin(AValue: TVec4); virtual;
    procedure SetTopLeft(AValue: TVec2); virtual;
    procedure SetWidth(AValue: Single); virtual;
    procedure SetWidthHeight(AValue: TVec2); virtual;

  protected
    FAlign: TP3DControlAlign;
    FCanvas: TP3DCanvas2D;
    FClientRect: TP3DRect;
    FControls: TP3DControlList;
    FEnabled: Boolean;
    FInputState: TP3DGCInputFlags;
    FLastClick: Integer;
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
    FVisible: Boolean;
    FCursor: TP3DCursor;
    FMaxHeight: Float;
    FMaxWidth: Float;
    FMinHeight: Float;
    FMinWidth: Float;
    FBaseCanvasRect: TP3DRect;
    //Various getters and setters

    function GetFocused: Boolean; virtual;
    procedure SetFocused( AValue: Boolean ); virtual;
    procedure SetCursor( AValue: TP3DCursor );
    function GetParent: TP3DGraphicControl;
    function GetScroll: TVec2;
    procedure SetAlign( const AValue: TP3DControlAlign ); virtual;
    procedure SetInputState(AValue: TP3DGCInputFlags); virtual;
    procedure SetParent( Value: TP3DGraphicControl);
    procedure SetScroll(AValue: TVec2);
    procedure SetVisible( Value: Boolean ); virtual;

    function MouseRay( X, Y: Integer ): Boolean; virtual; //Check if a ray at the given Pixel is inside of the control (usually the client rect).
                                                          //Can be overridden for to change this behaviour
    procedure DragCancel( Sender: TP3DGraphicControl; X, Y: Integer ); virtual; //Cancel dragging.
    procedure DragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean ); virtual; //This procedure is called internally when the mouse is released on the control during dragging. It can be overriden to make this control a drag target.
    procedure Realign(); virtual; //Realign the control. Is called from TP3DControlList when parent changes
    procedure Paint(); virtual; //Prepare the canvas for drawing and call draw procedure.

    {procedure CalculateDimensions( Origin: TVec2; var R: TP3DRect );
    function CalculatePosition( AMaxSize: TVec2 ): TVec2;
    function CalculatePageSize(): TVec2;
    function CalculateWidthHeight( AMaxSize: TVec2 ): TVec2;
    function CalculateCanvasRect(): TP3DRect;}

    property HideChildren: Boolean read FHideChildren write FHideChildren; //Do not call children's render procedure

  public
    constructor Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl); virtual;
    constructor Create( const AData: TP3DLibrary = nil ); override;
    procedure FastDestroy; override;

    procedure BeginDrag(); virtual; //Start drag and drop with control as drag source
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
    procedure MouseWheel( deltax: Integer; deltay: Integer ); virtual;

    procedure BringToFront; //Bring control to the front layer in control list
    procedure OneLayerDown; //Move control one layer down in control list.
    procedure OneLayerUp; //Move control one layer up in control list.
    procedure SendToBack; //Send control to the back

    class function IsFocusControl(): Boolean; virtual;
    class function HasShadow(): Boolean; virtual;

    property Rect: TP3DRect read FRect write FRect;
    property TopLeft: TVec2 read GetTopLeft write SetTopLeft;
    property BottomRight: TVec2 read GetBottomRight write SetBottomRight;
    property WidthHeight: TVec2 read GetWidthHeight write SetWidthHeight;
    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
    property Left: Single read GetLeft write SetLeft;
    property Top: Single read GetTop write SetTop;
    property PageSize: TVec2 read GetPageSize;

    property PageModeX: TP3DPageMode read GetPageModeX write SetPageModeX;
    property PageModeY: TP3DPageMode read GetPageModeY write SetPageModeY;
    property Margin: TVec4 read FMargin write SetMargin;
    property Canvas: TP3DCanvas2D read FCanvas write FCanvas;
    property ClientRect: TP3DRect read FClientRect;
    property Controls: TP3DControlList read FControls write FControls;
    property InputState: TP3DGCInputFlags read FInputState write SetInputState;
    property ParentCtrl: TP3DControlList read FOwnerCtrlBuf write FOwnerCtrlBuf;
    property Scroll: TVec2 read GetScroll write SetScroll;
    property PointerTag: Pointer read FPointerTag write FPointerTag;

  published
    property Align: TP3DControlAlign read FAlign write SetAlign;
    property Enabled: Boolean read FEnabled write FEnabled;
    property NoMargin: Boolean read FNoMargin write FNoMargin;
    property Parent: TP3DGraphicControl read GetParent write SetParent;
    property Tag: Integer read FTag write FTag;
    property Visible: Boolean read FVisible write SetVisible;
    property Focused: Boolean read GetFocused write SetFocused;

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
    property NeedsUpdate: Boolean read FNeedsUpdate write FNeedsUpdate;
end;

TP3DPropertyAccessGraphicControl = specialize gP3DPropertyAccessDataBlock < TP3DGraphicControl >;
TP3DPropertyAccessGraphicControlList = specialize gP3DListPropertyAccess < TP3DControlList, TP3DGraphicControl, TP3DPropertyAccessGraphicControl >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DGraphicControl }

constructor TP3DGraphicControl.Create(const AData: TP3DLibrary;
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
  WidthHeight:= vec2( 50 );

  Cursor:= curArrow;

  Properties.Add( [ TP3DPropertyAccessRTTI.Create( Self, 'Align', smAttribute ),
                    TP3DPropertyAccessVec2.CreateGetSet( 'TopLeft', @GetTopLeft, @SetTopLeft ),
                    TP3DPropertyAccessRTTI.Create( Self, 'MinWidth', smAttribute ),
                    TP3DPropertyAccessRTTI.Create( Self, 'MaxWidth', smAttribute ),
                    TP3DPropertyAccessRTTI.Create( Self, 'MinHeight', smAttribute ),
                    TP3DPropertyAccessRTTI.Create( Self, 'MaxHeight', smAttribute )],
                    'Position and Size' );

  Properties.Add( [ TP3DPropertyAccessVec4.CreateFieldSet( 'Margin', @FMargin, @SetMargin, smAttribute ),
                    TP3DPropertyAccessRTTI.Create( Self, 'NoMargin', smAttribute )],
                    'Margin' );

  Properties.Add( [
                    TP3DPropertyAccessRTTI.Create( Self, 'Enabled', smAttribute ),
                    TP3DPropertyAccessRTTI.Create( Self, 'Visible', smAttribute ),
                    TP3DPropertyAccessRTTI.Create( Self, 'Focused', smAttribute ),
                    TP3DPropertyAccessRTTI.Create( Self, 'Cursor', smAttribute )],
                    'Behavior' );

  Properties.Add( [ TP3DPropertyAccessGraphicControl.CreateGetSet( 'Parent', @GetParent, @SetParent, smAttribute ),
                    TP3DPropertyAccessVec2.CreateGetSet( 'Scroll', @GetScroll, @SetScroll ),
                    TP3DPropertyAccessRTTI.Create( Self, 'Tag', smAttribute )],
                    'Other' );
  //Properties.Add( TP3DPropertyAccesslRTTI.Create( Self, 'ClientRect', smAttribute )); //cannot be published
  Properties.Add( TP3DPropertyAccessGraphicControlList.CreateField( 'Controls', @FControls, smAttribute )); //cannot be published
  //property Controls: TP3DControlList read FControls write FControls;
end;

constructor TP3DGraphicControl.Create(const AData: TP3DLibrary);
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

procedure TP3DGraphicControl.SetMargin(AValue: TVec4);
begin
  if FMargin=AValue then Exit;
  FMargin:=AValue;
  NeedsUpdate:= True;
end;

function TP3DGraphicControl.GetBottomRight: TVec2;
begin
  Result:= Rect.BottomRight;
end;

function TP3DGraphicControl.GetHeight: Single;
begin
  Result:= Rect.Height;
end;

function TP3DGraphicControl.GetLeft: Single;
begin
  Result:= Rect.Left;
end;

function TP3DGraphicControl.GetPageModeX: TP3DPageMode;
begin
  Result:= Controls.PageModeX;
end;

function TP3DGraphicControl.GetPageModeY: TP3DPageMode;
begin
  Result:= Controls.PageModeY;
end;

function TP3DGraphicControl.GetPageSize: TVec2;
begin
  Result:= Controls.PageSize;
end;

function TP3DGraphicControl.GetTop: Single;
begin
  Result:= Rect.Top;
end;

function TP3DGraphicControl.GetTopLeft: TVec2;
begin
  Result:= Rect.TopLeft;
end;

function TP3DGraphicControl.GetWidth: Single;
begin
  Result:= Rect.Width;
end;

function TP3DGraphicControl.GetWidthHeight: TVec2;
begin
  Result:= Rect.WidthHeight;
end;

procedure TP3DGraphicControl.SetBottomRight(AValue: TVec2);
begin
  Rect.BottomRight:= AValue;
  NeedsUpdate:= True;
end;

procedure TP3DGraphicControl.SetHeight(AValue: Single);
begin
  Rect.Height:= AValue;
  NeedsUpdate:= True;
end;

procedure TP3DGraphicControl.SetLeft(AValue: Single);
begin
  Rect.Left:= AValue;
  NeedsUpdate:= True;
end;

procedure TP3DGraphicControl.SetPageModeX(AValue: TP3DPageMode);
begin
  Controls.PageModeX:= AValue;
end;

procedure TP3DGraphicControl.SetPageModeY(AValue: TP3DPageMode);
begin
  Controls.PageModeY:= AValue;
end;

procedure TP3DGraphicControl.SetTopLeft(AValue: TVec2);
begin
  Rect.TopLeft:= AValue;
  NeedsUpdate:= True;
end;

procedure TP3DGraphicControl.SetWidth(AValue: Single);
begin
  Rect.Width:= AValue;
  NeedsUpdate:= True;
end;

procedure TP3DGraphicControl.SetWidthHeight(AValue: TVec2);
begin
  Rect.WidthHeight:= AValue;
  NeedsUpdate:= True;
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
  //Controls.Input;
end;

procedure TP3DGraphicControl.MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  if (( X >= 0 ) and ( Y >= 0 ) and ( X <= Canvas.Width ) and ( Y <= Canvas.Height )) then
    if ( Assigned( FOnMouseClick )) then
      FOnMouseClick( Self, mb1, mb2, mb3, X, Y );
  if ( not Assigned( Self )) then
    exit;
  if ( FLastClick >= SDL_GetTicks ) then
    MouseDblClick( mb1, mb2, mb3, X, Y );
  if ( Assigned( Self )) then
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
  if (( P3DInput.Mouse.DWX <> 0 ) or ( P3DInput.Mouse.DWY <> 0 )) then
    MouseWheel( P3DInput.Mouse.DWX, P3DInput.Mouse.DWY );
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

procedure TP3DGraphicControl.MouseWheel(deltax: Integer; deltay: Integer);
begin

end;

procedure TP3DGraphicControl.KeyboardAction;
var
  Ctrl: TP3DGraphicControl;
begin
  KeyDown;
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


function TP3DGraphicControl.GetScroll: TVec2;
begin
  Result:= Controls.PageScroll;
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
  Controls.PageScroll:= AValue;
  Controls.UpdateControls;
end;

procedure TP3DGraphicControl.SetTop(AValue: Single);
begin
  Rect.Top:= AValue;
  NeedsUpdate:= True;
end;



procedure TP3DGraphicControl.Paint();
var
  OldScroll: TVec2;
  {$IFDEF DEBUG_GUI}
  i: Integer;
  txt: TP3DTextBmp;
  {$ENDIF}
  p: TVec2;
begin
  if ( Canvas.Width * Canvas.Height = 0 ) then
    exit;

  if ( HasShadow()) then
    begin
      p:= Canvas.TopLeft + 2;
      P3DGUIManager.ScreenCanvas.Lock;
      P3DGUIManager.ScreenCanvas.RenderRectShadow( p, p + Canvas.WidthHeight, 10, vec4( 0, 0, 0, 0.05 ));
      P3DGUIManager.ScreenCanvas.Unlock();
    end;

  OldScroll:= Canvas.Scroll;
  //Canvas.Scroll:= Canvas.Scroll + ScrollAcc;
  //Canvas.Rect:= R;
//  Canvas.Scroll:= - Controls.Origin + Canvas.TopLeft + ClientRect.TopLeft;
  Canvas.Scroll:= Controls.FinalScroll;
  Canvas.Lock;

  //Canvas.ScreenColor:= vec4( 1 * Ord( gcisMouseBtn1Down in InputState ), 1 * Ord( gcisMouseBtn2Down in InputState ), 1 * Ord( gcisMouseBtn3Down in InputState ), 0.25 + 0.75 * Ord( gcisMouseOver in InputState ) + 0.25 * Ord( gcisMouseOverOccl in InputState ));

  Draw;

  Canvas.Unlock();
  Canvas.Scroll:= OldScroll;
  if ( not HideChildren ) then
    Controls.Render();
  {$IFDEF DEBUG_GUI}
  P3DGUIManager.ScreenCanvas.Lock;
  P3DGUIManager.ScreenCanvas.Font.Size:= 12;
  P3DGUIManager.ScreenCanvas.Font.Color:= Red300;
  P3DGUIManager.ScreenCanvas.GetValidText( IntToStr( ParentCtrl.IndexOf( Self )), P3DGUIManager.DebugText );
  P3DGUIManager.ScreenCanvas.RenderText( P3DGUIManager.DebugText, vec2( Canvas.Left, Canvas.Top ));
  P3DGUIManager.ScreenCanvas.Font.Color:= Black;
  if ( Self = P3DGUIManager.LastMouseOverCtrl ) then
    begin
      //P3DGUIManager.ScreenCanvas.RenderRect( vec2( Canvas.Left + ClientRect.Left, Canvas.Top + ClientRect.Top ), vec2( Canvas.Left + ClientRect.Left + ClientRect.Right, Canvas.Top + ClientRect.Top + ClientRect.Bottom ), vec4( 1, 1, 1, 0.2 ));
      P3DGUIManager.ScreenCanvas.RenderRect( vec2( Canvas.Left, Canvas.Top ), vec2( Canvas.Left + Canvas.Width, Canvas.Top + Canvas.Height ), vec4( 0, 0, 0, 0.5 ));

      P3DGUIManager.ScreenCanvas.Font.Size:= 24;
      P3DGUIManager.ScreenCanvas.GetValidText( Format( '%s [%dx%d]', [ P3DGUIManager.LastMouseOverCtrl.Name, round( P3DGUIManager.LastMouseOverCtrl.Width ), round( P3DGUIManager.LastMouseOverCtrl.Height )]), P3DGUIManager.DebugText );
      P3DGUIManager.DebugTextPos:= vec2( Canvas.Left, Canvas.Top + Canvas.Height - 20 );
      P3DGUIManager.ScreenCanvas.RenderLineRect( Controls.PageRect.TopLeft, Controls.PageRect.BottomRight, Red500.Fade( 0.5 ));

      //P3DGUIManager.ScreenCanvas.RenderText( txt, p );
      //txt.Free;
    end;
  for i:= 0 to 2 do
    if ( Self = P3DGUIManager.LastMouseDownCtrl[ i ]) then
      begin
        P3DGUIManager.ScreenCanvas.RenderRect( vec2( Canvas.Left, Canvas.Top ), vec2( Canvas.Left + Canvas.Width, Canvas.Top + Canvas.Height ), vec4( Ord( i = 0 ), Ord( i = 1 ), Ord( i = 2 ), 0.2 ));
      end;
  P3DGUIManager.ScreenCanvas.Unlock();
  {$ENDIF}
end;

{#1
case Align of
  alLeft:
    begin
      R.Left:= R.Left + Canvas.Width;
      R.Width:= R.Width - Canvas.Width;
    end;
  alRight:
    begin
      R.Right:= R.Right - Canvas.Width;
    end;
  alTop:
    begin
      R.Top:= R.Top + Canvas.Height;
      R.Height:= R.Height - Canvas.Height;
    end;
  alBottom:
    begin
      R.Height:= R.Height - Canvas.Height;
    end;
end;}

procedure TP3DGraphicControl.SetAlign( const AValue: TP3DControlAlign );
begin
  if FAlign=AValue then Exit;
  FAlign:=AValue;
  if ( Visible ) then
    NeedsUpdate:= True;
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
  NeedsUpdate:= True;
end;


procedure TP3DGraphicControl.SetVisible( Value: Boolean );
begin
  if ( FVisible <> Value ) then
    begin
      FVisible:= Value;
      //if ( Align <> alNone ) then
        NeedsUpdate:= True;
    end;
end;

function TP3DGraphicControl.MouseRay(X, Y: Integer): Boolean;
begin
 Result:= Canvas.Rect.PtInRect( vec2( X, Y ));
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

procedure TP3DGraphicControl.Realign();
begin

end;

procedure TP3DGraphicControl.DragCancel(Sender: TP3DGraphicControl; X,
  Y: Integer);
begin
  if ( Assigned( FOnDragDropCancel )) then
    FOnDragDropCancel( Sender, X, Y );
end;

{$ENDIF}
