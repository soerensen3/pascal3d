{$IFDEF INTERFACE}


TP3DGraphicControlClass = class of TP3DGraphicControl;

{ TP3DGraphicControl }
                            // 1                  2                4               8              16
TP3DGCInputState = ( gcisEmpty, gcisMouseBtn1Down, gcisMouseBtn2Down, gcisMouseBtn3Down, gcisMouseOver {, gcisMouseOverOccl} );
TP3DGCInputFlags = set of TP3DGCInputState;
TP3DCursor = ( curArrow = 'A', curHourGlass = 'B', curHResize = 'C', curVResize = 'D', curMoveLeftRight = 'E', curMoveTopBottom = 'F', curNodeEdit = 'G', curDragDrop = 'H' );

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
TP3DUI = class;
TP3DUIViewerBase = class;

TP3DGraphicControlUpdateRequest = ( urNone, urClippedRect, urChildren, urParentCtrls );

TP3DGraphicControl = class ( TP3DNamedStreamable )
  private
    FClippedRect: TP3DRect;
    FClippedRectScreen: TP3DRect;
    FMargin: TVec4;
    FHideChildren: Boolean;
    FPointerTag: Pointer;
    FRect: TP3DRect;
    FUpdateRequested: TP3DGraphicControlUpdateRequest;

    function GetBottomRight: TVec2; virtual;
    function GetHeight: Single; virtual;
    function GetLeft: Single; virtual;
    function GetPageModeX: TP3DPageMode;
    function GetPageModeY: TP3DPageMode;
    function GetPageSize: TVec2;
    function GetParentCtrl: TP3DGraphicControlContainerList;
    function GetTop: Single; virtual;
    function GetTopLeft: TVec2; virtual;
    function GetWidth: Single; virtual;
    function GetWidthHeight: TVec2; virtual;
    procedure SetBottomRight(AValue: TVec2); virtual;
    procedure SetHeight(AValue: Single); virtual;
    procedure SetLeft(AValue: Single); virtual;
    procedure SetPageModeX(AValue: TP3DPageMode);
    procedure SetPageModeY(AValue: TP3DPageMode);
    procedure SetParentCtrl(AValue: TP3DGraphicControlContainerList);
    procedure SetRect(AValue: TP3DRect);
    procedure SetTop(AValue: Single); virtual;
    procedure SetMargin(AValue: TVec4); virtual;
    procedure SetTopLeft(AValue: TVec2); virtual;
    procedure SetWidth(AValue: Single); virtual;
    procedure SetWidthHeight(AValue: TVec2); virtual;

  protected
    FAlign: TP3DControlAlign;
    FCanvas: TP3DCanvas2D;
    FClientRect: TP3DRect;
    FControls: TP3DGraphicControlContainerList;
    FEnabled: Boolean;
    FInputState: TP3DGCInputFlags;
    FLastClick: Integer;
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
    FOnMouseWheel: TP3DGUIMouseEvent;
    FOnMouseUp: TP3DGUIMouseClick;
    FOnParentRealign: TNotifyEvent;
    FTag: Integer;
    FVisible: Boolean;
    FCursor: TP3DCursor;
    FMaxHeight: Float;
    FMaxWidth: Float;
    FMinHeight: Float;
    FMinWidth: Float;
    FBaseClippedRect: TP3DRect;
    FParent: TP3DStreamablePointer;
    FUIViewer: TP3DStreamablePointer;
    //Various getters and setters

    function GetUIViewer: TP3DUIViewerBase;
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
    procedure RequestUpdate( AUpdateRequest: TP3DGraphicControlUpdateRequest );

    procedure DragCancel( Sender: TP3DGraphicControl; X, Y: Float ); virtual; //Cancel dragging.
    procedure DragDrop( Sender, Source: TP3DGraphicControl; X, Y: Float; var Accept: Boolean ); virtual; //This procedure is called internally when the mouse is released on the control during dragging. It can be overriden to make this control a drag target.
    procedure Realign(); virtual; //Realign the control. Is called from TP3DControlList when parent changes
    procedure Paint(); virtual; //Prepare the canvas for drawing and call draw procedure.
    procedure Logic(); virtual;
    procedure TriggerMouseAction;

    {procedure CalculateDimensions( Origin: TVec2; var R: TP3DRect );
    function CalculatePosition( AMaxSize: TVec2 ): TVec2;
    function CalculatePageSize(): TVec2;
    function CalculateWidthHeight( AMaxSize: TVec2 ): TVec2;
    function CalculateClippedRect(): TP3DRect;}

  public
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
    destructor Destroy; override;

    function MouseRay( X, Y: Float ): Boolean; virtual;   //Check if a ray at the given Pixel is inside of the control (usually the client rect).
                                                          //Can be overridden for to change this behaviour
    function ChildrenMouseRay( X, Y: Float; const Recurse: Boolean = True ): TP3DGraphicControl; virtual;   //Check if a ray at the given Pixel is inside of the control (usually the client rect).
                                                          //Can be overridden for to change this behaviour
    procedure BeginDrag(); virtual; //Start drag and drop with control as drag source
    procedure Draw; virtual;
    procedure KeyDown; virtual;
    procedure KeyboardAction(); virtual;
    procedure MouseAction( X, Y: Float; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean ); virtual;
    procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Float ); virtual;
    procedure MouseDblClick( mb1, mb2, mb3: Boolean; X, Y: Float ); virtual;
    procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Float ); virtual;
    procedure MouseEnter( X, Y: Float ); virtual;
    procedure MouseLeave( X, Y: Float ); virtual;
    procedure MouseMove( X, Y: Float ); virtual;
    procedure MouseUp( mb1, mb2, mb3: Boolean; X, Y: Float ); virtual;
    procedure MouseWheel( deltax: Integer; deltay: Integer ); virtual;

    procedure BringToFront; //Bring control to the front layer in control list
    procedure OneLayerDown; //Move control one layer down in control list.
    procedure OneLayerUp; //Move control one layer up in control list.
    procedure SendToBack; //Send control to the back

    class function IsFocusControl(): Boolean; virtual;
    class function HasShadow(): Boolean; virtual;

    property Rect: TP3DRect read FRect write SetRect;
    property ClippedRect: TP3DRect read FClippedRect write FClippedRect;
    property ClippedRectScreen: TP3DRect read FClippedRectScreen write FClippedRectScreen;
    property TopLeft: TVec2 read GetTopLeft write SetTopLeft;
    property BottomRight: TVec2 read GetBottomRight write SetBottomRight;
    property WidthHeight: TVec2 read GetWidthHeight write SetWidthHeight;
    property Width: Single read GetWidth write SetWidth;
    property Height: Single read GetHeight write SetHeight;
    property Left: Single read GetLeft write SetLeft;
    property Top: Single read GetTop write SetTop;
    property PageSize: TVec2 read GetPageSize;
    property HideChildren: Boolean read FHideChildren write FHideChildren; //Do not call children's render procedure

    property Margin: TVec4 read FMargin write SetMargin;
    property Canvas: TP3DCanvas2D read FCanvas write FCanvas;
    property ClientRect: TP3DRect read FClientRect;
    property Controls: TP3DGraphicControlContainerList read FControls write FControls;
    property InputState: TP3DGCInputFlags read FInputState write SetInputState;
    property ParentCtrl: TP3DGraphicControlContainerList read GetParentCtrl write SetParentCtrl;
    property Scroll: TVec2 read GetScroll write SetScroll;
    property PointerTag: Pointer read FPointerTag write FPointerTag;
    property UIViewer: TP3DUIViewerBase read GetUIViewer;

  published
    property Align: TP3DControlAlign read FAlign write SetAlign;
    property Enabled: Boolean read FEnabled write FEnabled;
    property NoMargin: Boolean read FNoMargin write FNoMargin;
    property Parent: TP3DGraphicControl read GetParent write SetParent;
    property Tag: Integer read FTag write FTag;
    property Visible: Boolean read FVisible write SetVisible;
    property Focused: Boolean read GetFocused write SetFocused;
    property PageModeX: TP3DPageMode read GetPageModeX write SetPageModeX;
    property PageModeY: TP3DPageMode read GetPageModeY write SetPageModeY;

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
    property OnMouseWheel: TP3DGUIMouseEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseUp: TP3DGUIMouseClick read FOnMouseUp write FOnMouseUp;
    property OnParentRealign: TNotifyEvent read FOnParentRealign write FOnParentRealign;
    property Cursor: TP3DCursor read FCursor write SetCursor;
    property UpdateRequested: TP3DGraphicControlUpdateRequest read FUpdateRequested write FUpdateRequested;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DGraphicControl }

constructor TP3DGraphicControl.Create(const AOwner: IP3DStreamableContainer);
var
  _Owner: IP3DStreamableContainer;
  a: TObject;
begin
  if ( Assigned( AOwner )) then
    _Owner:= AOwner
  else
    _Owner:= P3DUI.Viewer.Controls;
  FUIViewer:= TP3DStreamablePointer.Create( 'UIViewer', TP3DUIViewerBase );
  FParent:= TP3DStreamablePointer.Create( 'Parent', TP3DGraphicControl );
  inherited Create( _Owner );
  FControls:= TP3DGraphicControlContainerList.Create( 'Controls' );
  if ( _Owner.GetContainer is TP3DGraphicControlContainerList ) then begin
    FUIViewer.AsValue:= TP3DGraphicControlContainerList( _Owner.GetContainer ).UIViewer;
    FControls.FUIViewer.AsValue:= UIViewer;
  end;
  Properties.Add([ FUIViewer, FControls, FParent ]);
  FCanvas:= TP3DCanvas2D.Create( Self );

  Visible:= True;
  Enabled:= True;
  Scroll.X:= 0;
  Scroll.Y:= 0;
  WidthHeight:= vec2( 50 );

  Cursor:= curArrow;

  Properties.Add( [ TP3DPropAccessRTTI.Create( Self, 'Align', smText ),
                    TP3DPropAccessVec2.CreateGetSet( 'TopLeft', @GetTopLeft, @SetTopLeft ),
                    TP3DPropAccessVec2.CreateGetSet( 'WidthHeight', @GetWidthHeight, @SetWidthHeight ),
                    TP3DPropAccessRTTI.Create( Self, 'MinWidth', smText ),
                    TP3DPropAccessRTTI.Create( Self, 'MaxWidth', smText ),
                    TP3DPropAccessRTTI.Create( Self, 'MinHeight', smText ),
                    TP3DPropAccessRTTI.Create( Self, 'MaxHeight', smText ),
                    TP3DPropAccessRTTI.Create( Self, 'PageModeX', smText ),
                    TP3DPropAccessRTTI.Create( Self, 'PageModeY', smText )],
                    'Position and Size' );

  Properties.Add( [ TP3DPropAccessVec4.CreateFieldSet( 'Margin', @FMargin, @SetMargin, smText ),
                    TP3DPropAccessRTTI.Create( Self, 'NoMargin', smText )],
                    'Margin' );

  Properties.Add( [
                    TP3DPropAccessRTTI.Create( Self, 'Enabled', smText ),
                    TP3DPropAccessRTTI.Create( Self, 'Visible', smText ),
                    TP3DPropAccessRTTI.Create( Self, 'Focused', smText ),
                    TP3DPropAccessRTTI.Create( Self, 'Cursor', smText )],
                    'Behavior' );

  Properties.Add( [ {TP3DPropAccessStreamable.CreateGetSet( 'Parent', @GetParent, @SetParent ),}
                    TP3DPropAccessVec2.CreateGetSet( 'Scroll', @GetScroll, @SetScroll ),
                    TP3DPropAccessRTTI.Create( Self, 'Tag', smText )],
                    'Other' );

  //Properties.Add( TP3DPropAccesslRTTI.Create( Self, 'ClientRect', smText )); //cannot be published
  //property Controls: TP3DControlList read FControls write FControls;
end;

destructor TP3DGraphicControl.Destroy;
begin
  Canvas.Free;
  inherited;
end;

procedure TP3DGraphicControl.Draw;
begin
  if ( Assigned( FOnDraw )) then
    FOnDraw( Self, ClippedRect.Left, ClippedRect.Top, ClippedRect.Width, ClippedRect.Height );
end;

procedure TP3DGraphicControl.SetMargin(AValue: TVec4);
begin
  if FMargin=AValue then Exit;
  FMargin:=AValue;
  ParentCtrl.UpdateControls;
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

function TP3DGraphicControl.GetParentCtrl: TP3DGraphicControlContainerList;
begin
  if ( Assigned( Parent )) then
    Result:= Parent.Controls
  else if ( UIViewer is TP3DUIViewer ) then
    Result:= TP3DUIViewer( UIViewer ).Controls;
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
  Rect:= P3DRectEx( Rect.TopLeft, AValue );
  //NeedsUpdate:= True;
end;

procedure TP3DGraphicControl.SetHeight(AValue: Single);
begin
  Rect:= P3DRect( Rect.Left, Rect.Top, Rect.Width, AValue );
end;

procedure TP3DGraphicControl.SetLeft(AValue: Single);
begin
  Rect:= P3DRect( AValue, Rect.Top, Rect.Width, Rect.Height );
  //NeedsUpdate:= True;
end;

procedure TP3DGraphicControl.SetPageModeX(AValue: TP3DPageMode);
begin
  Controls.PageModeX:= AValue;
  RequestUpdate( urChildren );
end;

procedure TP3DGraphicControl.SetPageModeY(AValue: TP3DPageMode);
begin
  Controls.PageModeY:= AValue;
  RequestUpdate( urChildren );
end;

procedure TP3DGraphicControl.SetParentCtrl(AValue: TP3DGraphicControlContainerList);
begin
  Owner:= AValue;
end;

procedure TP3DGraphicControl.SetRect(AValue: TP3DRect);
var
  OldRect: TP3DRect;
  Control: TP3DGraphicControl;
begin
  if FRect=AValue then Exit;

  OldRect:= FRect;

  FRect:= AValue;

  if ( OldRect.WidthHeight <> Rect.WidthHeight ) then
    RequestUpdate( urParentCtrls )
  else if ( OldRect.TopLeft <> Rect.TopLeft ) then
    RequestUpdate( urChildren )
  Else
    Exit;

  RequestUpdate( urClippedRect );
end;

procedure TP3DGraphicControl.SetTopLeft(AValue: TVec2);
begin
  Rect:= P3DRect( AValue, Rect.WidthHeight );
end;

procedure TP3DGraphicControl.RequestUpdate(AUpdateRequest: TP3DGraphicControlUpdateRequest);
begin
  if ( UpdateRequested >= AUpdateRequest ) then
    exit;
  FUpdateRequested:= AUpdateRequest;
end;

procedure TP3DGraphicControl.SetWidth(AValue: Single);
begin
  Rect:= P3DRect( Rect.Left, Rect.Top, AValue, Rect.Height );
end;

procedure TP3DGraphicControl.SetWidthHeight(AValue: TVec2);
begin
    Rect:= P3DRect( Rect.TopLeft, AValue );
end;

function TP3DGraphicControl.GetUIViewer: TP3DUIViewerBase;
begin
  Result:= TP3DUIViewerBase( FUIViewer.AsValue );
end;

function TP3DGraphicControl.GetFocused: Boolean;
begin
  if ( Assigned( P3DUI )) then
    Result:= Self = P3DUI.FocusedControl
  else
    Result:= False;
end;

procedure TP3DGraphicControl.SetFocused(AValue: Boolean);
begin
  if ( AValue ) then
    P3DUI.FocusedControl:= Self
  else if ( P3DUI.FocusedControl = Self ) then
    P3DUI.FocusedControl:= nil;
end;

procedure TP3DGraphicControl.SetCursor(AValue: TP3DCursor);
begin
  if FCursor=AValue then Exit;
  FCursor:=AValue;
end;

function TP3DGraphicControl.GetParent: TP3DGraphicControl;
begin
  Result:= TP3DGraphicControl( FParent.AsValue );
end;

procedure TP3DGraphicControl.KeyDown;
begin
  if ( Assigned( FOnKeyDown )) then
    FOnKeyDown( Self );
  //Controls.Input;
end;

procedure TP3DGraphicControl.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if (( X >= 0 ) and ( Y >= 0 ) and ( X <= ClippedRect.Width ) and ( Y <= ClippedRect.Height )) then
    if ( Assigned( FOnMouseClick )) then
      FOnMouseClick( Self, mb1, mb2, mb3, X, Y );
  if ( not Assigned( Self )) then
    exit;
  if ( FLastClick >= SDL_GetTicks ) then
    MouseDblClick( mb1, mb2, mb3, X, Y );
  if ( Assigned( Self )) then
    FLastClick:= SDL_GetTicks + 500;
end;

procedure TP3DGraphicControl.MouseDblClick(mb1, mb2, mb3: Boolean; X, Y: Float);
begin

end;

procedure TP3DGraphicControl.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if ( Assigned( FOnMouseDown )) then
    FOnMouseDown( Self, mb1, mb2, mb3, X, Y );
end;

procedure TP3DGraphicControl.MouseEnter(X, Y: Float);
begin
  if ( Assigned( FOnMouseEnter )) then
    FOnMouseEnter( Self, X, Y );
end;

procedure TP3DGraphicControl.MouseLeave(X, Y: Float);
begin
  if ( Assigned( FOnMouseLeave )) then
    FOnMouseLeave( Self, X, Y );
end;

procedure TP3DGraphicControl.MouseAction(X, Y: Float; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean);
var
  Ctrl: TP3DGraphicControl;
begin
  if (( UIViewer.MouseDX <> 0 ) or ( UIViewer.MouseDY <> 0 )) then
    MouseMove( X, Y );
  if (( P3DInput.Mouse.DWX <> 0 ) or ( P3DInput.Mouse.DWY <> 0 )) then
    MouseWheel( P3DInput.Mouse.DWX, P3DInput.Mouse.DWY );
  for Ctrl in Controls do
    Ctrl.TriggerMouseAction;
end;

procedure TP3DGraphicControl.MouseMove(X, Y: Float);
  function Button0Released: Boolean; inline;
  begin
    Result:= P3DInput.Mouse.DButtons[ 0 ] and ( not P3DInput.Mouse.Buttons[ 0 ]);
  end;

begin
  {TODO: Only call this if mouse is in client rect.}
  if ( Assigned( FOnMouseMove )) then
    FOnMouseMove( Self, X, Y );
end;

procedure TP3DGraphicControl.MouseUp(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if ( Assigned( FOnMouseUp )) then
    FOnMouseUp( Self, mb1, mb2, mb3, X, Y );
  if ( gcisMouseOver in InputState ) then //ONLY FIRE EVENT IF THE BUTTON IS RELEASED INSIDE OF THE CONTROL
    MouseClick( mb1, mb2, mb3, X, Y );
end;

procedure TP3DGraphicControl.MouseWheel(deltax: Integer; deltay: Integer);
begin
  if ( Assigned( FOnMouseWheel )) then
    FOnMouseWheel( Self, deltax, deltay );
end;

procedure TP3DGraphicControl.KeyboardAction();
var
  Ctrl: TP3DGraphicControl;
begin
  KeyDown;
  for Ctrl in Controls do
    Ctrl.KeyboardAction();
end;

procedure TP3DGraphicControl.OneLayerDown;
begin
  ParentCtrl.OneLayerDown( ParentCtrl.IndexOf( Self ));
end;

procedure TP3DGraphicControl.OneLayerUp;
begin
  ParentCtrl.OneLayerUp( ParentCtrl.IndexOf( Self ));
end;

procedure TP3DGraphicControl.BringToFront;
begin
  ParentCtrl.BringToFront( ParentCtrl.Find( Self ));
end;

procedure TP3DGraphicControl.SendToBack;
begin
  ParentCtrl.SendToBack( ParentCtrl.Find( Self ));
end;

class function TP3DGraphicControl.IsFocusControl(): Boolean;
begin
  Result:= False;
end;

class function TP3DGraphicControl.HasShadow(): Boolean;
begin
  Result:= False;
end;


function TP3DGraphicControl.GetScroll: TVec2;
begin
  Result:= Controls.PageScroll;
end;

procedure TP3DGraphicControl.SetInputState(AValue: TP3DGCInputFlags);
var
  mx: Float;
  my: Float;
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

  mx:= UIViewer.MouseX - ClippedRect.Left;
  my:= UIViewer.MouseY - ClippedRect.Top;

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
end;

procedure TP3DGraphicControl.SetTop(AValue: Single);
begin
  Rect:= P3DRect( Rect.Left, AValue, Rect.Width, Rect.Height );
  //NeedsUpdate:= True;
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
  if ( ClippedRect.Width * ClippedRect.Height = 0 ) then
    exit;

  if ( not Assigned( UIViewer )) then
    exit;

  ClippedRectScreen:= ClippedRect;
  ClippedRectScreen:= ClippedRectScreen * UIViewer.Zoom - UIViewer.Position;
  if ( Assigned( Parent ) and ( Parent.UIViewer = UIViewer )) then
    ClippedRectScreen:= ClippedRectScreen.Clip( Parent.ClippedRectScreen );
//  else
//    ClippedRectScreen:= ClippedRectScreen.Clip( UIViewer.ViewRect );

  Canvas.Rect:= ClippedRectScreen;
  Canvas.Zoom:= UIViewer.Zoom;

  if ( HasShadow()) then
    begin
      p:= ClippedRect.TopLeft + 2;
      UIViewer.ScreenCanvas.Lock;
      UIViewer.ScreenCanvas.RenderRectShadow( p, p + ClippedRect.WidthHeight, 10, vec4( 0, 0, 0, 0.05 ));
      UIViewer.ScreenCanvas.Unlock();
    end;

  OldScroll:= Canvas.Scroll;

  Canvas.Scroll:= -( Controls.FinalScroll + ( 1 / UIViewer.Zoom ) * ( Canvas.Rect.TopLeft - ClippedRectScreen.TopLeft ));
  Canvas.Lock;
  //if ( Assigned( Parent )) then
  //  R:= R.Clip( Parent.ClippedRect );

  //Canvas.ScreenColor:= vec4( 1 * Ord( gcisMouseBtn1Down in InputState ), 1 * Ord( gcisMouseBtn2Down in InputState ), 1 * Ord( gcisMouseBtn3Down in InputState ), 0.25 + 0.75 * Ord( gcisMouseOver in InputState ) + 0.25 * Ord( gcisMouseOverOccl in InputState ));

  Draw;

  Canvas.Unlock();
  Canvas.Scroll:= OldScroll;
  if ( not HideChildren ) then
    Controls.Render();
end;

procedure TP3DGraphicControl.Logic();
begin
  case UpdateRequested of
    urClippedRect: ParentCtrl.UpdateClippedRect( Self, ParentCtrl.ClipRect );
    urChildren: Controls.UpdateControls;
    urParentCtrls: ParentCtrl.UpdateControls;
  end;
  Controls.Logic();
end;

procedure TP3DGraphicControl.TriggerMouseAction;
begin
  with P3DInput.Mouse do
    MouseAction( UIViewer.MouseX - ClippedRect.Left, UIViewer.MouseY - ClippedRect.Top, Buttons[ 0 ], Buttons[ 1 ], Buttons[ 2 ], DButtons[ 0 ], DButtons[ 1 ], DButtons[ 2 ]);
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
    RequestUpdate( urParentCtrls );
end;

procedure TP3DGraphicControl.SetParent( Value: TP3DGraphicControl );
begin
  if ( Assigned( Value )) then
    Owner:= Value.Controls
  else if ( UIViewer is TP3DUIViewer ) then
    Owner:= TP3DUIViewer( UIViewer ).Controls;
  RequestUpdate( urParentCtrls );
end;


procedure TP3DGraphicControl.SetVisible( Value: Boolean );
begin
  if ( FVisible <> Value ) then
    begin
      FVisible:= Value;
      //if ( Align <> alNone ) then
      RequestUpdate( urParentCtrls );
    end;
end;

function TP3DGraphicControl.MouseRay(X, Y: Float): Boolean;
begin
  Result:= ClippedRect.PtInRect( vec2( X, Y ));
end;

function TP3DGraphicControl.ChildrenMouseRay(X, Y: Float; const Recurse: Boolean): TP3DGraphicControl;
begin
  Result:= Controls.MouseRay( X, Y, Recurse );
end;

procedure TP3DGraphicControl.BeginDrag();
begin
  P3DUI.DragDropSrc:= Self;
end;

procedure TP3DGraphicControl.DragDrop(Sender, Source: TP3DGraphicControl; X, Y: Float; var Accept: Boolean);
begin
  if ( Assigned( FOnDragDrop )) then
    FOnDragDrop( Sender, Source, X, Y, Accept );
end;

procedure TP3DGraphicControl.Realign();
begin
  FUpdateRequested:= urNone;
end;

procedure TP3DGraphicControl.DragCancel(Sender: TP3DGraphicControl; X, Y: Float);
begin
  if ( Assigned( FOnDragDropCancel )) then
    FOnDragDropCancel( Sender, X, Y );
end;

{$ENDIF}
