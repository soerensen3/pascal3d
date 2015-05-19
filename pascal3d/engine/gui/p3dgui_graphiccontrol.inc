{$IFDEF INTERFACE}
TP3DGraphicControlClass = class of TP3DGraphicControl;

{ TP3DGraphicControl }

TP3DGCInputState = ( gcisMouseBtn1Down, gcisMouseBtn2Down, gcisMouseBtn3Down, gcisMouseOver, gcisMouseOverOccl );
TP3DGCInputFlags = set of TP3DGCInputState;
TP3DGraphicControl = class ( TBaseObject )
  private
    FInputState: TP3DGCInputFlags;

    function GetScroll: TVec2;
    procedure SetInputState(AValue: TP3DGCInputFlags);
    procedure SetScroll(AValue: TVec2);

  protected
    FLeft,
    FTop,
    FWidth,
    FHeight: Integer;
    //FIndex: Integer;
    FOwnerCtrlBuf: TControlBuffer;
    FControls: TControlBuffer;

    FVisible,
    FEnabled: Boolean;
    FOnDraw: TGUIDrawProc;
    FOnMouseDown: TGUIMouseClick;
    FOnMouseUp: TGUIMouseClick;
    FOnMouseClick: TGUIMouseClick;
    FOnMouseMove: TGUIMouseMove;
    FOnMouseEnter: TGUIHover;
    FOnMouseLeave: TGUIHover;
    FOnKeyDown: TNotifyEvent;
    FAlign: TControlAlign;
    FNoBounds: Boolean;
    FMinWidth,
    FMinHeight,
    FMaxWidth,
    FMaxHeight: DWord;
    FLastClick: Integer;
    FOnParentRealign: TNotifyEvent;
    FTag: Integer;
    //FScreenColor: TVec4;
    FClientRect: TRect;
    FBoundsLeft: Integer;
    FBoundsTop: Integer;
    FBoundsBottom: Integer;
    FBoundsRight: Integer;
    FCanvas: TP3DCanvas2D;

    procedure Render( BaseColor: TVec4; ScrollAcc: TVec2 ); virtual;
    procedure SetAlign( const Value: TControlAlign ); virtual;
    procedure SetHeight( const Value: Integer ); virtual;
    procedure SetLeft( const Value: Integer ); virtual;
    procedure SetTop( const Value: Integer ); virtual;
    procedure SetWidth( const Value: Integer ); virtual;
    procedure SetVisible( const Value: Boolean ); virtual;
    procedure SetBounds( Index: Integer; const Value: Integer);
    procedure Realign; virtual;
    function GetParent: TP3DGraphicControl;
    procedure SetParent(const Value: TP3DGraphicControl);
    procedure UpdateClientRect;

  public
    Manager: TGUIManager;

    constructor Create(AOwner: TObjectList; AManager: TGUIManager;
      const AParent: TP3DGraphicControl=nil);
    destructor Destroy; override;

    procedure Draw; virtual;
    procedure MouseMove( X, Y: Integer ); virtual;
    procedure MouseEnter( X, Y: Integer ); virtual;
    procedure MouseLeave( X, Y: Integer ); virtual;
    function MouseAction( X, Y: Integer; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean ): TP3DGraphicControl; virtual;
    function MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ): TP3DGraphicControl; virtual;
    function MouseUp( mb1, mb2, mb3: Boolean; X, Y: Integer ): TP3DGraphicControl; virtual;
    procedure KeyboardAction(); virtual;
    procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
    procedure MouseDblClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
    procedure KeyDown; virtual;
    procedure BringToFront;
    procedure SendToBack;
    procedure OneLayerUp;
    procedure OneLayerDown;

    property ParentCtrl: TControlBuffer read FOwnerCtrlBuf write FOwnerCtrlBuf;
    property Controls: TControlBuffer read FControls write FControls;
    property Canvas: TP3DCanvas2D read FCanvas write FCanvas;
    property ClientRect: TRect read FClientRect;
    property InputState: TP3DGCInputFlags read FInputState write SetInputState;
    property Scroll: TVec2 read GetScroll write SetScroll;

  published
    property BoundsLeft: Integer index 0 read FBoundsLeft write SetBounds;
    property BoundsTop: Integer index 1 read FBoundsTop write SetBounds;
    property BoundsRight: Integer index 2 read FBoundsRight write SetBounds;
    property BoundsBottom: Integer index 3 read FBoundsBottom write SetBounds;
    property Left: Integer read FLeft write SetLeft;
    property Top: Integer read FTop write SetTop;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property MinWidth: DWord read FMinWidth write FMinWidth;
    property MinHeight: DWord read FMinHeight write FMinHeight;
    property MaxWidth: DWord read FMaxWidth write FMaxWidth;
    property MaxHeight: DWord read FMaxHeight write FMaxHeight;
//        property Index: Integer read FIndex write FIndex;
    property Visible: Boolean read FVisible write SetVisible;
    property Enabled: Boolean read FEnabled write FEnabled;
    property OnDraw: TGUIDrawProc read FOnDraw write FOnDraw;
    property OnMouseDown: TGUIMouseClick read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TGUIMouseClick read FOnMouseUp write FOnMouseUp;
    property OnMouseClick: TGUIMouseClick read FOnMouseClick write FOnMouseClick;
    property OnMouseMove: TGUIMouseMove read FOnMouseMove write FOnMouseMove;
    property OnMouseEnter: TGUIHover read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TGUIHover read FOnMouseLeave write FOnMouseLeave;
    property OnKeyDown: TNotifyEvent read FOnKeyDown write FOnKeyDown;
    property Align: TControlAlign read FAlign write SetAlign;
//        property Skin: TTexture read FSkin write SetSkin;
//        property ParentSkin: Boolean read FParentSkin write SetParentSkin;
    property NoBounds: Boolean read FNoBounds write FNoBounds;
    property OnParentRealign: TNotifyEvent read FOnParentRealign write FOnParentRealign;
    property Tag: Integer read FTag write FTag;
    property Parent: TP3DGraphicControl read GetParent write SetParent;
end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DGraphicControl }

constructor TP3DGraphicControl.Create( AOwner: TObjectList; AManager: TGUIManager; const AParent: TP3DGraphicControl = nil );
begin
  inherited Create( AOwner );
  Manager:= AManager;
  Controls:= TControlBuffer.Create( Self, Manager );
  FCanvas:= TP3DCanvas2D.Create( Self );
  if ( Assigned( AParent )) then
    ParentCtrl:= AParent.Controls
  else
    ParentCtrl:= AManager.Controls;
  ParentCtrl.Add( Self );

  Visible:= True;
  Enabled:= True;
  Scroll.X:= 0;
  Scroll.Y:= 0;
  Width:= 50;
  Height:= 50;
end;

destructor TP3DGraphicControl.Destroy;
begin
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

function TP3DGraphicControl.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer
  ): TP3DGraphicControl;
begin
  {
  if ( mb1 ) then
    InputState:= InputState + [ gcisMouseBtn1Down ];
  if ( mb2 ) then
    InputState:= InputState + [ gcisMouseBtn2Down ];
  if ( mb3 ) then
    InputState:= InputState + [ gcisMouseBtn3Down ];}
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

function TP3DGraphicControl.MouseAction(X, Y: Integer; mb1, mb2, mb3, dmb1,
  dmb2, dmb3: Boolean): TP3DGraphicControl;
var
  cx: Integer;
  cy: Integer;
  mdown_test: Boolean;
  mup_test: Boolean;

  procedure ReleaseButtons;
  begin
    cx:= X - Canvas.Left;
    cy:= Y - Canvas.Top;
    if ( gcisMouseBtn1Down in InputState ) then
      MouseUp( True, False, False, cx, cy );
    if ( gcisMouseBtn2Down in InputState ) then
      MouseUp( False, True, False, cx, cy );
    if ( gcisMouseBtn3Down in InputState ) then
      MouseUp( False, False, True, cx, cy );
  end;

begin
  Result:= nil;
  cx:= X - Canvas.Left;
  cy:= Y - Canvas.Top;
  if (( InputManager.Mouse.DX <> 0 ) or ( InputManager.Mouse.DY <> 0 )) then
    MouseMove( cx, cy );
  {
  if ( PtInRect( Rect( Canvas.Left, Canvas.Top, Canvas.Left + Canvas.Width, Canvas.Top + Canvas.Height ), Point( X, Y ))) then
    begin
      Result:= Controls.Input;
      if ( not Assigned( Result )) then
        begin
          InputState:= InputState + [ gcisMouseOver ];
          //InputState:= InputState + [ gcisMouseOverOccl ];
          mdown_test:= ( mb1 AND dmb1 ) OR ( mb2 AND dmb2 ) OR ( mb3 AND dmb3 );
          mup_test:= (( not mb1 ) AND dmb1 ) OR (( not mb2 ) AND dmb2 ) OR (( not mb3 ) AND dmb3 );
          if ( mdown_test ) then
            MouseDown(( mb1 AND dmb1 ), ( mb2 AND dmb2 ), ( mb3 AND dmb3 ), cx, cy );
          if ( mup_test ) then
            MouseUp((( not mb1 ) AND dmb1 ), (( not mb2 ) AND dmb2 ), (( not mb3 ) AND dmb3 ), cx, cy );
          Result:= Self;
        end
      else
        begin
          InputState:= InputState - [ gcisMouseOver ];
          InputState:= InputState + [ gcisMouseOverOccl ];
          ReleaseButtons;
        end;
    end
  else
    begin
      InputState:= InputState - [ gcisMouseOver, gcisMouseOverOccl ];
      ReleaseButtons;
    end;}
end;

procedure TP3DGraphicControl.MouseMove( X, Y: Integer );
begin

  {TODO: Only call this if mouse is in client rect.
  if ( Assigned( FOnMouseMove )) then
    FOnMouseMove( Self, X, Y );}
end;

function TP3DGraphicControl.MouseUp(mb1, mb2, mb3: Boolean; X, Y: Integer
  ): TP3DGraphicControl;
begin
  {if ( mb1 ) then
    InputState:= InputState - [ gcisMouseBtn1Down ];
  if ( mb2 ) then
    InputState:= InputState - [ gcisMouseBtn2Down ];
  if ( mb3 ) then
    InputState:= InputState - [ gcisMouseBtn3Down ];
  if ( Assigned( FOnMouseUp )) then
    FOnMouseUp( Self, mb1, mb2, mb3, X, Y );}
  MouseClick( mb1, mb2, mb3, X, Y );
end;

procedure TP3DGraphicControl.KeyboardAction;
begin

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


procedure TP3DGraphicControl.Realign;
begin
  if ( Assigned( Parent )) then
    begin
      if ( NoBounds ) then
        begin
          Canvas.Left:= Left + Parent.Canvas.Left;
          Canvas.Top:= Top + Parent.Canvas.Top;
          Canvas.Width:= Min( Parent.Width - Left, Width );
          Canvas.Height:= Min( Parent.Height - Top, Height );
        end
      else
        begin
          Canvas.Left:= Left + Parent.Canvas.Left + Parent.ClientRect.Left;
          Canvas.Top:= Top + Parent.Canvas.Top + Parent.ClientRect.Top;
          Canvas.Width:= Min( Parent.ClientRect.Right - Left, Width );
          Canvas.Height:= Min( Parent.ClientRect.Bottom - Top, Height );
        end;
    end
  else
    begin
      Canvas.Left:= Left;
      Canvas.Top:= Top;
      Canvas.Width:= Min( MainWindow.Width - Left, Width ); //TODO: Support for multiple windows
      Canvas.Height:= Min( MainWindow.Height - Top, Height );
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

  function FlagSet( Flag: TP3DGCInputState ): Boolean;
  begin
    Result:= Flag in ( AValue * DIS );
  end;

  function MDwn: Boolean;
  begin
    Result:= ( DIS * AValue ) * [ gcisMouseBtn1Down, gcisMouseBtn2Down, gcisMouseBtn3Down ] <> [];
  end;

  function MUp(): Boolean;
  begin
    Result:= ( DIS * InputState ) * [ gcisMouseBtn1Down, gcisMouseBtn2Down, gcisMouseBtn3Down ] <> [];
  end;

begin
  if FInputState=AValue then Exit;
  FInputState:=AValue;

  mx:= GUIManager.MouseX - Canvas.Left;
  my:= GUIManager.MouseY - Canvas.Top;

  DIS:= AValue * InputState;

  if ( gcisMouseOver in DIS ) then
    if ( gcisMouseOver in AValue ) then
      MouseEnter( mx, my )
    else
      MouseLeave( mx, my );
  if ([ gcisMouseBtn1Down, gcisMouseBtn2Down, gcisMouseBtn3Down ] * DIS <> []) then
    begin
      if ( MUp()) then
        MouseDown( FlagSet( gcisMouseBtn1Down ), FlagSet( gcisMouseBtn2Down ), FlagSet( gcisMouseBtn3Down ), mx, my );
      if ( MDwn()) then
        MouseUp( not FlagSet( gcisMouseBtn1Down ), not FlagSet( gcisMouseBtn2Down ), not FlagSet( gcisMouseBtn3Down ), mx, my );
    end;
end;

procedure TP3DGraphicControl.SetScroll(AValue: TVec2);
begin
  Canvas.Scroll:= AValue;
end;

procedure TP3DGraphicControl.Render(BaseColor: TVec4; ScrollAcc: TVec2);
var
  OldScroll: TVec2;
begin
  Canvas.Lock;
  OldScroll:= Scroll;
  Canvas.Scroll:= ScrollAcc + Scroll;

  Canvas.ScreenColor:= vec4( 1 * Ord( gcisMouseBtn1Down in InputState ), 1 * Ord( gcisMouseBtn2Down in InputState ), 1 * Ord( gcisMouseBtn3Down in InputState ), 0.25 + 0.75 * Ord( gcisMouseOver in InputState ) + 0.25 * Ord( gcisMouseOverOccl in InputState ));

  Draw;

  Canvas.Unlock();
  Canvas.Scroll:= OldScroll;
  Controls.Render( BaseColor, ScrollAcc + Scroll );
end;


procedure TP3DGraphicControl.SetAlign( const Value: TControlAlign );
begin
  FAlign:= Value;
  ParentCtrl.Realign;
end;

procedure TP3DGraphicControl.SetHeight( const Value: Integer );
var
  Val: Integer;
begin
  Val:= Max( 1, Value );
  if ( FMinHeight > 0 ) then
    Val:= Max( FMinHeight, Val );
  if ( FMaxHeight > 0 ) then
    Val:= Min( FMaxHeight, Val );
  FHeight:= Val;
  ParentCtrl.Realign;
  Controls.Realign;
end;

procedure TP3DGraphicControl.SetLeft( const Value: Integer );
begin
  FLeft:= Value;
  ParentCtrl.Realign;
  Controls.Realign;
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


procedure TP3DGraphicControl.SetTop( const Value: Integer );
begin
  FTop:= Value;
  ParentCtrl.Realign;
  Controls.Realign;
end;

procedure TP3DGraphicControl.SetVisible( const Value: Boolean );
begin
  if ( FVisible <> Value ) then
    begin
      FVisible:= Value;
      FOwnerCtrlBuf.Realign;
    end;
end;

procedure TP3DGraphicControl.SetWidth( const Value: Integer );
var
  Val: Integer;
begin
  Val:= Max( 1, Value );
  if ( FMinWidth > 0 ) then
    Val:= Max( FMinWidth, Val );
  if ( FMaxWidth > 0 ) then
    Val:= Min( FMaxWidth, Val );
  FWidth:= Val;
  ParentCtrl.Realign;
  Controls.Realign;
end;

{$ENDIF}
