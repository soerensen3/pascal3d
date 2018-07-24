{$IFDEF INTERFACE}

{ TP3DGraphicControlContainerList }

TP3DGraphicControlPointerList = specialize gP3DStreamablePointerList < TP3DGraphicControl >;

TP3DGraphicControlContainerList = class( specialize gP3DNamedStreamableContainerList < TP3DGraphicControl > )
  protected
    FClipRect: TP3DRect;
    FFinalScroll: TVec2;
    FPageModeX: TP3DPageMode;
    FPageModeY: TP3DPageMode;
    FPageRect: TP3DRect;
    FPageScroll: TVec2;
    FPageSize: TVec2;
    FParent: TP3DGraphicControl;
    FUIViewer: TP3DStreamablePointer;

    //Various getters and setters

    function GetUIViewer: TP3DUIViewerBase;
    function GetCanScrollX: Boolean;
    function GetCanScrollY: Boolean;
    function GetPageScrollPixel: TVec2;
    procedure SetPageModeX(AValue: TP3DPageMode);
    procedure SetPageModeY(AValue: TP3DPageMode);
    procedure SetPageRect(AValue: TP3DRect);
    procedure SetPageScroll(AValue: TVec2);
    procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;

  public
    constructor Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
    destructor Destroy; override;

    procedure UpdateControls;
    procedure UpdateControlRect( Control: TP3DGraphicControl; var TargetRect: TP3DRect );
    procedure UpdateClippedRect( Control: TP3DGraphicControl; TargetRect: TP3DRect );
    function UpdatePageSize: TVec2;
    function NeedsUpdate: Boolean;

    procedure Render();
    procedure Input();
    procedure Logic();

    function MouseRay(X, Y: Float; const Recurse: Boolean): TP3DGraphicControl;
    procedure KeyboardAction();
    procedure MouseAction;

    //procedure MoveTo( Index: Integer; Controls: TP3DGraphicControlContainerList );
    procedure BringToFront( Index: Integer );
    procedure SendToBack( Index: Integer );
    procedure OneLayerUp( Index: Integer );
    procedure OneLayerDown( Index: Integer );
    //function IndexOf( Name: String ): Integer; overload;
    function ChildOf( AControl: TP3DGraphicControl ): Boolean;

    property Parent: TP3DGraphicControl read FParent;
    property PageRect: TP3DRect read FPageRect write SetPageRect;
    property ClipRect: TP3DRect read FClipRect write FClipRect;
    property PageModeX: TP3DPageMode read FPageModeX write SetPageModeX;
    property PageModeY: TP3DPageMode read FPageModeY write SetPageModeY;
    property PageScroll: TVec2 read FPageScroll write SetPageScroll;
    property PageScrollPixel: TVec2 read GetPageScrollPixel;
    property FinalScroll: TVec2 read FFinalScroll;
    property PageSize: TVec2 read FPageSize;
    property CanScrollX: Boolean read GetCanScrollX;
    property CanScrollY: Boolean read GetCanScrollY;
    property UIViewer: TP3DUIViewerBase read GetUIViewer;

    //property Origin: TVec2 read FOrigin;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DGraphicControlContainerList }

{
procedure TP3DGraphicControlContainerList.MoveTo(Index: Integer; Controls: TP3DGraphicControlContainerList);
begin
  Controls.Add( Items[ Index ]);
  inherited Delete( Index );
end;
}
{
procedure TP3DGraphicControlContainerList.Realign;
var
  R: TP3DRect;
  Control: TP3DGraphicControl;

  // Boundary = Left/Right/Top/Bottom Boundary?
  // ControlPos = Position to be updated
  // BoundaryLength = The length of the boundary
  // BoundaryShift =

  procedure UpdateControlAndBoundary( var Boundary: Float; var ControlPos: Float; var ControlLength: Float; BoundaryLength, BoundaryShift: Float ); inline;
  begin
    if ( BoundaryShift < 0 ) then
      ControlPos:= Boundary + BoundaryShift
    else
      ControlPos:= Boundary - R.Left;
    ControlLength:= BoundaryLength;
    Boundary+= BoundaryShift;
  end;

begin
  if ( Parent <> nil ) then
    begin
      Parent.UpdateExtents;
      R:= Parent.ClientRect;
      R.Left:= 0;
      R.Top:= 0; //Left/Top start already at the Client Rect
    end
  else
    with ( P3DViewports.Screen ) do
      R:= P3DRect( 0, 0, Width, Height );
  FPageSize:= vec2( 0 );
  for Control in Self do
    if ( P3DDataBlockCache.IsValid( Control ) and ( Control.Visible ) and ( Control.Align <> alClient )) then
      begin
        Control.Realign( R );
        Control.Controls.Realign;
      end;

  for Control in Self do
    if (( Control.Visible ) and ( Control.Align = alClient )) then
      begin
        Control.Realign( R );
        Control.Controls.Realign;
      end
end;
}

procedure TP3DGraphicControlContainerList.SetPageModeX(AValue: TP3DPageMode);
begin
  if ( FPageModeX = AValue ) then Exit;
  FPageModeX:= AValue;
end;

function TP3DGraphicControlContainerList.GetPageScrollPixel: TVec2;
begin
  Result:= PageScroll * max( PageSize - PageRect.WidthHeight, 0 ); //* vec2( Ord( PageModeX = pmScroll ), Ord( PageModeY = pmScroll ));
end;

function TP3DGraphicControlContainerList.GetUIViewer: TP3DUIViewerBase;
begin
  Result:= TP3DUIViewerBase( FUIViewer.AsValue );
end;

function TP3DGraphicControlContainerList.GetCanScrollX: Boolean;
begin
  Result:= PageSize.x > PageRect.Width;
end;

function TP3DGraphicControlContainerList.GetCanScrollY: Boolean;
begin
  Result:= PageSize.y > PageRect.Height;
end;

procedure TP3DGraphicControlContainerList.SetPageModeY(AValue: TP3DPageMode);
begin
  if ( FPageModeY = AValue ) then Exit;
  FPageModeY:= AValue;
end;

procedure TP3DGraphicControlContainerList.SetPageRect(AValue: TP3DRect);
begin
  if ( FPageRect = AValue ) then
    exit;
  FPageRect:=AValue;
  UpdateControls;
end;

procedure TP3DGraphicControlContainerList.SetPageScroll(AValue: TVec2);
begin
  if FPageScroll=AValue then Exit;
  FPageScroll:= clamp( AValue, vec2( 0.0 ), vec2( Ord( CanScrollX ), Ord( CanScrollY )));
  UpdateControls;
  //PageScroll:= PageScroll * vec2( Ord( PageModeX = pmScroll ), Ord( PageModeY = pmScroll ));
end;

procedure TP3DGraphicControlContainerList.Notify(ASender: TObject; AItem: TP3DStreamable;
  AAction: TP3DListOnChangeAction);
begin
  inherited Notify(ASender, AItem, AAction);
  if (( AAction = actAdd ) and ( Context.GetInstance is TP3DGraphicControl )) then
    begin
      TP3DGraphicControl( AItem ).FParent.AsValue:= TP3DGraphicControl( Context.GetInstance );
      TP3DGraphicControl( AItem ).RequestUpdate( urParentCtrls );
    end
  else if ( Assigned( AItem )) then begin
    TP3DGraphicControl( AItem ).FParent.AsValue:= nil;
    if ( Context.GetInstance is TP3DGraphicControl ) then
      TP3DGraphicControl( AItem ).RequestUpdate( urParentCtrls );
  end;
end;

constructor TP3DGraphicControlContainerList.Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create(AName, AStoreMode);
  FUIViewer:= TP3DStreamablePointer.Create( 'UIViewer', TP3DUIViewerBase );
  FUIViewer.Root:= Root;
end;

destructor TP3DGraphicControlContainerList.Destroy;
begin
  FUIViewer.Free;
  inherited Destroy;
end;

procedure TP3DGraphicControlContainerList.UpdateControls;
var
  Control: TP3DGraphicControl;
  Rect: TP3DRect;
begin
  Rect:= PageRect;
  Rect.TopLeft:= vec2( 0 );

  FPageSize:= UpdatePageSize;

  for Control in Self do //alLeft, alTop, alRight, alBottom
    if (( Control.Visible ) and ( Control.Align in [ alLeft, alTop, alRight, alBottom, alNone ])) then
      UpdateControlRect( Control, Rect );

  {for Control in Self do //alLeft, alTop
    if (( Control.Visible ) and ( Control.Align in [ alLeft, alTop ])) then
      UpdateControlRect( Control, Rect );

  for Control in Self.Reversed() do //alRight, alBottom;
    if (( Control.Visible ) and ( Control.Align in [ alRight, alBottom ])) then
      UpdateControlRect( Control, Rect );}

  for Control in Self do //alClient
    if (( Control.Visible ) and ( Control.Align in [ alClient ])) then
      UpdateControlRect( Control, Rect );
  //FPageSize:= UpdatePageSize;

  for Control in Self do
    if ( Control.Visible ) then
      begin
        UpdateClippedRect( Control, ClipRect );
        Control.Realign();
        Control.FUpdateRequested:= urNone;
      end;
end;

procedure TP3DGraphicControlContainerList.UpdateControlRect(Control: TP3DGraphicControl;
  var TargetRect: TP3DRect);
var
  Rect: TP3DRect;
begin
  Control.Controls.FPageSize:= Control.Controls.UpdatePageSize;
  Rect.TopLeft:= Control.TopLeft;// + FinalScroll;
  Rect.WidthHeight:= Control.WidthHeight;

  if ( Control.PageModeX = pmExpand ) then   // Autosize for x and y
    Rect.Width:= Control.PageSize.X + Control.Margin.X + Control.Margin.Z;
  if ( Control.PageModeY = pmExpand ) then
    Rect.Height:= Control.PageSize.Y + Control.Margin.Y + Control.Margin.W;

  if ( Control.Align in [ alTop, alBottom, alClient ]) then
    Rect.Width:= TargetRect.Width; // change width to client rect's size

  if ( Control.Align in [ alLeft, alRight, alClient ]) then
    Rect.Height:= TargetRect.Height; // change height to client rect's size

  if ( Control.MaxWidth > 0 ) then
    Rect.Width:= min( Control.MaxWidth, Rect.Width );
  if ( Control.MaxHeight > 0 ) then
    Rect.Height:= min( Control.MaxHeight, Rect.Height );

  if ( Control.Align in [ alLeft, alTop, alBottom, alClient ]) then
    Rect.Left:= TargetRect.Left
  else if ( Control.Align in [ alRight ]) then
    Rect.Left:= TargetRect.Right - Rect.Width;

  if ( Control.Align in [ alLeft, alTop, alRight, alClient ]) then
    Rect.Top:= TargetRect.Top
  else if ( Control.Align in [ alBottom ]) then
    Rect.Top:= TargetRect.Bottom - Rect.Height;

  Control.FRect:= Rect;

  TargetRect:= TargetRect.Inset( vec2( Ord( Control.Align = alLeft ),  Ord( Control.Align = alTop ))    * Rect.WidthHeight,
                                 vec2( Ord( Control.Align = alRight ), Ord( Control.Align = alBottom )) * Rect.WidthHeight ); //#1
  Control.FClientRect:= P3DRect( 0, 0, Rect.Width, Rect.Height ).Inset( Control.Margin.XY, Control.Margin.ZW ); //#1
end;

procedure TP3DGraphicControlContainerList.UpdateClippedRect(Control: TP3DGraphicControl;
  TargetRect: TP3DRect);
var
  ControlRect, CtrlPageRect: TP3DRect;
  ratio: Double;
  Origin: TVec2;
begin
  ControlRect:= Control.Rect; // initialize control rect
  if (( PageSize.X > 0 ) and ( PageModeX = pmStretch )) then  // Stretch x of control to pagerect width / initial width of all controls
    begin
      ratio:= PageRect.Width / PageSize.X;
      ControlRect.Left:= ControlRect.Left * ratio;
      ControlRect.Width:= ControlRect.Width * ratio;
    end;
  if (( PageSize.Y > 0 ) and ( PageModeY = pmStretch )) then  // Stretch y of control to pagerect height / initial height of all controls
    begin
      ratio:= PageRect.Height / PageSize.Y;
      ControlRect.Top:= ControlRect.Top * ratio;
      ControlRect.Height:= ControlRect.Height * ratio;
    end;
  //if ( Control.UIViewer = UIViewer ) then
  ControlRect.TopLeft:= ControlRect.TopLeft + PageRect.TopLeft - GetPageScrollPixel;// + FFinalScroll; // calculate unclipped screen rect
  Origin:= ControlRect.TopLeft; // store origin for later use
  CtrlPageRect:= P3DRectEx( Origin + Control.Margin.XY, ControlRect.BottomRight - Control.Margin.ZW ); // calculate unclipped screen page rect for controls' childs

  if ( Assigned( Parent )) then
    ControlRect:= ControlRect.Clip( TargetRect ); // clip the control to the target rect of the parent to get the controls canvas rect
  Control.ClippedRect:= ControlRect; // <-- assign the final canvas rect

  // assign the canvas scroll if origin is out of target rect
  Control.Controls.FFinalScroll:= abs( ControlRect.TopLeft - Origin );
  if ( Assigned( Parent )) then
    Control.Controls.ClipRect:= CtrlPageRect.Clip( TargetRect ) // <-- assign clipped page rect
  else
    Control.Controls.ClipRect:= CtrlPageRect;
  if ( Control.Controls.UIViewer <> UIViewer ) then begin
    CtrlPageRect.TopLeft:= vec2( 0 );
  end;

  Control.Controls.PageRect:= CtrlPageRect; // <-- assign page rect
  {if ( Control.Controls.UIViewer <> UIViewer ) then begin
    Control.Controls.ClipRect.TopLeft:= vec2( 0 );
    Control.Controls.PageRect.TopLeft:= vec2( 0 );
  end;}
  Control.FUpdateRequested:= urNone;
end;

function TP3DGraphicControlContainerList.UpdatePageSize: TVec2;
var
  Control: TP3DGraphicControl;
  P: TVec2;
begin
  Result:= vec2( 0 );
  for Control in Self do
    if (( Control.Visible ) and ( Control.Align <> alClient )) then
      begin
        P:= max( Result, Control.BottomRight );
        if ( Control.Align in [ alNone, alLeft, alRight ]) then
          Result.X:= P.X;
        if ( Control.Align in [ alNone, alTop, alBottom ]) then
          Result.Y:= P.Y;
      end;
end;

function TP3DGraphicControlContainerList.NeedsUpdate: Boolean;
var
  Control: TP3DGraphicControl;
begin
  Result:= False;
  for Control in Self do
    if ( Control.NeedsUpdate or Control.Controls.NeedsUpdate ) then
      begin
        Result:= True;
        break;
      end;
end;

procedure TP3DGraphicControlContainerList.Render();
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( AsValues[ i ].Visible ) then
      AsValues[ i ].Paint;
end;

procedure TP3DGraphicControlContainerList.BringToFront( Index: Integer );
var
  Item: TP3DPropertyAccess;
begin
  if ( not InRange( Index, 0, Count-2 )) then
    exit;
  Item:= Items[ Index ];
  FItems.Extract( Item );
  FItems.Add( Item );
  if ( TP3DGraphicControl( TP3DStreamableContainer( Item ).AsValue ).Align in [ alLeft, alRight, alTop, alBottom ]) then
    TP3DGraphicControl( TP3DStreamableContainer( Item ).AsValue ).NeedsUpdate:= True;
end;

procedure TP3DGraphicControlContainerList.OneLayerDown( Index: Integer );
var
  Item: TP3DPropertyAccess;
begin
  if ( Index <= 0 ) then
    exit;
  Item:= Items[ Index ];
  FItems.Extract( Item );
  FItems.Insert( Index - 1, Item );
  if ( TP3DGraphicControl( TP3DStreamableContainer( Item ).AsValue ).Align in [ alLeft, alRight, alTop, alBottom ]) then
    TP3DGraphicControl( TP3DStreamableContainer( Item ).AsValue ).NeedsUpdate:= True;
end;

function TP3DGraphicControlContainerList.ChildOf(AControl: TP3DGraphicControl): Boolean;
var
  Control: TP3DGraphicControl;
begin
  Result:= False;
  for Control in Self do
    if (( Control = AControl ) or ( Control.Controls.ChildOf( AControl ))) then
      begin
        Result:= True;
        break;
      end;
end;

procedure TP3DGraphicControlContainerList.OneLayerUp( Index: Integer );
var
  Item: TP3DPropertyAccess;
begin
  if ( not InRange( Index, 0, Count-2 )) then
    exit;
  Item:= Items[ Index ];
  FItems.Extract( Item );
  FItems.Insert( Index, Item );
  if ( TP3DGraphicControl( TP3DStreamableContainer( Item ).AsValue ).Align in [ alLeft, alRight, alTop, alBottom ]) then
    TP3DGraphicControl( TP3DStreamableContainer( Item ).AsValue ).NeedsUpdate:= True;
end;

procedure TP3DGraphicControlContainerList.SendToBack( Index: Integer );
var
  Item: TP3DPropertyAccess;
begin
  if ( not InRange( Index, 1, Count-1 )) then
    exit;
  Item:= Items[ Index ];
  FItems.Extract( Item );
  FItems.Insert( 0, Item );
  if ( TP3DGraphicControl( TP3DStreamableContainer( Item ).AsValue ).Align in [ alLeft, alRight, alTop, alBottom ]) then
    TP3DGraphicControl( TP3DStreamableContainer( Item ).AsValue ).NeedsUpdate:= True;
end;

//Calls the keyboard and mouse input for all the controls in the list.
procedure TP3DGraphicControlContainerList.Input();
var
  Control: TP3DGraphicControl;

begin
  for Control in Self do
    if ( Control.Enabled and Control.Visible ) then
      begin
        //Handle Mouse
        Control.TriggerMouseAction;
        //Handle Keyboard
        Control.KeyboardAction();
      end;
end;

procedure TP3DGraphicControlContainerList.Logic();
var
  i: Integer;
begin
  //if ( NeedsUpdate ) then
  //  UpdateControls;
  for i:= 0 to Count - 1 do
    AsValues[ i ].Logic;
end;

function TP3DGraphicControlContainerList.MouseRay( X, Y: Float; const Recurse: Boolean ): TP3DGraphicControl;
var
  i: Integer;
begin
  Result:= nil;
  for i:= Count - 1 downto 0 do // The control on top of the stack is rendered last so this will be the control in front layer
    begin
      //WriteLn( AsValues[ i ].Name, ' TopLeft: ', AsValues[ i ].ClippedRect.TopLeft.ToString(), 'BottomRight: ', AsValues[ i ].ClippedRect.BottomRight.ToString());
    if ( AsValues[ i ].Visible and AsValues[ i ].Enabled ) then
      begin
        Result:= nil;

        if ( not AsValues[ i ].MouseRay( X, Y )) then
          continue;

        if ( Recurse ) then
          Result:= AsValues[ i ].ChildrenMouseRay( X, Y, Recurse );
        if ( not Assigned( Result )) then
          Result:= AsValues[ i ];

        break;
      end;

    end;
end;

procedure TP3DGraphicControlContainerList.KeyboardAction();
var
  Control: TP3DGraphicControl;

begin
  for Control in Self do
    if ( Control.Enabled and Control.Visible ) then
      Control.KeyboardAction();
end;

procedure TP3DGraphicControlContainerList.MouseAction;
var
  Control: TP3DGraphicControl;

begin
  for Control in Self do
    if ( Control.Enabled and Control.Visible ) then
      Control.TriggerMouseAction;
end;

{$ENDIF}
