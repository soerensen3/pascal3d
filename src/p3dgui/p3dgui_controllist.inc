{$IFDEF INTERFACE}

{ TP3DControlList }

TP3DControlList = class( specialize gP3DCustomObjectList < TP3DGraphicControl > )
  private
    FClipRect: TP3DRect;
    FFinalScroll: TVec2;
    FPageModeX: TP3DPageMode;
    FPageModeY: TP3DPageMode;
    FPageRect: TP3DRect;
    FPageScroll: TVec2;
    FPageSize: TVec2;
    FParent: TP3DGraphicControl;
    function GetCanScrollX: Boolean;
    function GetCanScrollY: Boolean;
    function GetPageScrollPixel: TVec2;
    procedure SetPageModeX(AValue: TP3DPageMode);
    procedure SetPageModeY(AValue: TP3DPageMode);
    procedure SetPageRect(AValue: TP3DRect);
    procedure SetPageScroll(AValue: TVec2);

  public
    procedure UpdateControls;
    procedure UpdateControlRect( Control: TP3DGraphicControl; var TargetRect: TP3DRect );
    procedure UpdateCanvasRect(Control: TP3DGraphicControl; TargetRect: TP3DRect);
    function UpdatePageSize: TVec2;
    function NeedsUpdate: Boolean;

    procedure Render();
    function Input: TP3DGraphicControl;

    function MouseRay(X, Y: Integer; const Recurse: Boolean): TP3DGraphicControl;
    procedure Clear( const FreeObjects: Boolean = False ); override;
    procedure Delete( Itm: TP3DGraphicControl ); overload;
    procedure MoveTo( Index: Integer; Controls: TP3DControlList );
    procedure BringToFront( Index: Integer );
    procedure SendToBack( Index: Integer );
    procedure OneLayerUp( Index: Integer );
    procedure OneLayerDown( Index: Integer );
    function IndexOf( Name: String ): Integer; overload;
    function ChildOf( AControl: TP3DGraphicControl ): Boolean;

    constructor Create( AParent: TP3DGraphicControl );
    destructor Destroy; override;

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
    //property Origin: TVec2 read FOrigin;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DControlList }


procedure TP3DControlList.Clear(const FreeObjects: Boolean);
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    if ( FreeObjects ) then
      Items[ i ].Free
    else
      begin
        Items[ i ].Controls:= P3DGUIManager.Controls;
        P3DGUIManager.Controls.Add( Items[ i ]);
      end;
  Count:= 0;
end;

procedure TP3DControlList.Delete(Itm: TP3DGraphicControl);
begin
  Delete( IndexOf( Itm ));
end;

constructor TP3DControlList.Create(AParent: TP3DGraphicControl);
begin
  inherited Create;
  FParent:= AParent;
end;

destructor TP3DControlList.Destroy;
begin
  Clear( True );
  inherited;
end;

procedure TP3DControlList.MoveTo(Index: Integer; Controls: TP3DControlList);
begin
  Controls.Add( Items[ Index ]);
  inherited Delete( Index );
end;

{
procedure TP3DControlList.Realign;
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

procedure TP3DControlList.SetPageModeX(AValue: TP3DPageMode);
begin
  if ( FPageModeX = AValue ) then Exit;
  FPageModeX:= AValue;
end;

function TP3DControlList.GetPageScrollPixel: TVec2;
begin
  Result:= PageScroll * max( PageSize - PageRect.WidthHeight, 0 ); //* vec2( Ord( PageModeX = pmScroll ), Ord( PageModeY = pmScroll ));
end;

function TP3DControlList.GetCanScrollX: Boolean;
begin
  Result:= PageSize.x > PageRect.Width;
end;

function TP3DControlList.GetCanScrollY: Boolean;
begin
  Result:= PageSize.y > PageRect.Height;
end;

procedure TP3DControlList.SetPageModeY(AValue: TP3DPageMode);
begin
  if ( FPageModeY = AValue ) then Exit;
  FPageModeY:= AValue;
end;

procedure TP3DControlList.SetPageRect(AValue: TP3DRect);
begin
  FPageRect:=AValue;
  UpdateControls;
end;

procedure TP3DControlList.SetPageScroll(AValue: TVec2);
begin
  if FPageScroll=AValue then Exit;
  FPageScroll:= clamp( AValue, vec2( 0.0 ), vec2( Ord( CanScrollX ), Ord( CanScrollY )));
  UpdateControls;
  //PageScroll:= PageScroll * vec2( Ord( PageModeX = pmScroll ), Ord( PageModeY = pmScroll ));
end;

procedure TP3DControlList.UpdateControls;
var
  Control: TP3DGraphicControl;
  Rect: TP3DRect;
begin
  Rect:= PageRect;
  Rect.TopLeft:= vec2( 0 );

  FPageSize:= UpdatePageSize; //workaround

  for Control in Self do //alLeft, alTop
    if (( Control.Visible ) and ( Control.Align in [ alLeft, alTop, alRight, alBottom, alNone ])) then
      UpdateControlRect( Control, Rect );

  {for Control in Self do //alLeft, alTop
    if (( Control.Visible ) and ( Control.Align in [ alLeft, alTop ])) then
      UpdateControlRect( Control, Rect );

  for Control in Self.Reversed() do //alLeft, alBottom;
    if (( Control.Visible ) and ( Control.Align in [ alRight, alBottom ])) then
      UpdateControlRect( Control, Rect );}

  for Control in Self do //alClient
    if (( Control.Visible ) and ( Control.Align in [ alClient ])) then
      UpdateControlRect( Control, Rect );
  FPageSize:= UpdatePageSize;

  for Control in Self do
    if ( Control.Visible ) then
      begin
        UpdateCanvasRect( Control, ClipRect );
        Control.Realign();
        Control.FNeedsUpdate:= False;
      end;
end;

procedure TP3DControlList.UpdateControlRect(Control: TP3DGraphicControl;
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

procedure TP3DControlList.UpdateCanvasRect(Control: TP3DGraphicControl;
  TargetRect: TP3DRect);
var
  ControlRect, CtrlPageRect: TP3DRect;
  ratio: Double;
  Origin: TVec2;
begin
  ControlRect:= Control.Rect;
  if (( PageSize.X > 0 ) and ( PageModeX = pmStretch )) then
    begin
      ratio:= PageRect.Width / PageSize.X;
      ControlRect.Left:= ControlRect.Left * ratio;
      ControlRect.Width:= ControlRect.Width * ratio;
    end;
  if (( PageSize.Y > 0 ) and ( PageModeY = pmStretch )) then
    begin
      ratio:= PageRect.Height / PageSize.Y;
      ControlRect.Top:= ControlRect.Top * ratio;
      ControlRect.Height:= ControlRect.Height * ratio;
    end;
  ControlRect.TopLeft:= ControlRect.TopLeft + PageRect.TopLeft - GetPageScrollPixel;
  Origin:= ControlRect.TopLeft;
  CtrlPageRect:= P3DRectEx( Origin + Control.Margin.XY, ControlRect.BottomRight - Control.Margin.ZW );
  ControlRect:= ControlRect.Clip( TargetRect );
  Control.Canvas.Rect:= ControlRect;
  Control.Controls.FFinalScroll:= Control.Canvas.TopLeft - Origin;
  Control.Controls.ClipRect:= CtrlPageRect.Clip( TargetRect );
  Control.Controls.PageRect:= CtrlPageRect;
  //ControlRect.TopLeft:= CanvRect.TopLeft + Control.Margin.XY;
end;

function TP3DControlList.UpdatePageSize: TVec2;
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

function TP3DControlList.NeedsUpdate: Boolean;
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

procedure TP3DControlList.Render();
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Visible ) then
      Items[ i ].Paint;
end;

procedure TP3DControlList.BringToFront( Index: Integer );
var
  Item: TP3DGraphicControl;
begin
  if ( Index + 1 = Count ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( Count, Item );
  if ( Item.Align in [ alLeft, alRight, alTop, alBottom ]) then
    Item.NeedsUpdate:= True;
end;

procedure TP3DControlList.OneLayerDown( Index: Integer );
var
  Item: TP3DGraphicControl;
begin
  if ( Index = 0 ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( Index - 1, Item );
  if ( Item.Align in [ alLeft, alRight, alTop, alBottom ]) then
    Item.NeedsUpdate:= True;
end;

function TP3DControlList.IndexOf(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = Name ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DControlList.ChildOf(AControl: TP3DGraphicControl): Boolean;
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

procedure TP3DControlList.OneLayerUp( Index: Integer );
var
  Item: TP3DGraphicControl;
begin
  if ( Index + 1 = Count ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( Index, Item );
  if ( Item.Align in [ alLeft, alRight, alTop, alBottom ]) then
    Item.NeedsUpdate:= True;
end;

procedure TP3DControlList.SendToBack( Index: Integer );
var
  Item: TP3DGraphicControl;
begin
  if ( Index = 0 ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( 0, Item );
  if ( Item.Align in [ alLeft, alRight, alTop, alBottom ]) then
    Item.NeedsUpdate:= True;
end;

//Calls the keyboard and mouse input for all the controls in the list.
function TP3DControlList.Input: TP3DGraphicControl;
var
  Control: TP3DGraphicControl;
  mx, my: Integer;
  i: Integer;
  Accept: Boolean;

  function BtnInpState( Idx: Integer ): TP3DGCInputFlags;
  begin
    Result:= [];
    case Idx of
      0: Result:= [ gcisMouseBtn1Down ];
      1: Result:= [ gcisMouseBtn2Down ];
      2: Result:= [ gcisMouseBtn3Down ];
  end;
  end;

begin
  mx:= P3DGUIManager.MouseX;
  my:= P3DGUIManager.MouseY;

  Result:= MouseRay( mx, my, True );
  if ( Result <> P3DGUIManager.LastMouseOverCtrl ) then
    begin
      if ( Assigned( P3DGUIManager.LastMouseOverCtrl )) then
        P3DGUIManager.LastMouseOverCtrl.InputState:= P3DGUIManager.LastMouseOverCtrl.InputState - [ gcisMouseOver ];
      if ( Assigned( Result )) then
        Result.InputState:= Result.InputState + [ gcisMouseOver ];
      P3DGUIManager.LastMouseOverCtrl:= Result;
    end;
  if ( not Assigned( Result ) and ( P3DGUIManager.Cursor <> curArrow )) then
    P3DGUIManager.Cursor:= curArrow
  else if (( Assigned( Result )) and ( Result.Cursor <> P3DGUIManager.Cursor )) then
    P3DGUIManager.Cursor:= Result.Cursor;


  for i:= 0 to 2 do
    with P3DInput.Mouse do
      if ( DButtons[ i ]) then
        if ( Buttons[ i ]) then //pushed
          begin
            P3DGUIManager.LastMouseDownCtrl[ i ]:= P3DGUIManager.LastMouseOverCtrl;
            if ( Assigned( P3DGUIManager.LastMouseDownCtrl[ i ]) and P3DDataBlockCache.IsValid( P3DGUIManager.LastMouseDownCtrl[ i ])) then
              P3DGUIManager.LastMouseDownCtrl[ i ].InputState:= P3DGUIManager.LastMouseDownCtrl[ i ].InputState + BtnInpState( i );
            P3DGUIManager.FocusedControl:= P3DGUIManager.LastMouseOverCtrl;
          end
        else                    //released
          begin
            if ( Assigned( P3DGUIManager.LastMouseDownCtrl[ i ]) and P3DDataBlockCache.IsValid( P3DGUIManager.LastMouseDownCtrl[ i ])) then
              P3DGUIManager.LastMouseDownCtrl[ i ].InputState:= P3DGUIManager.LastMouseDownCtrl[ i ].InputState - BtnInpState( i );
            // Drag and Drop release
            if (( DragDropSrc <> nil )) then
              begin
                Accept:= False;
                if ( Assigned( P3DGUIManager.LastMouseOverCtrl ) and P3DDataBlockCache.IsValid( P3DGUIManager.LastMouseOverCtrl )) then
                  P3DGUIManager.LastMouseOverCtrl.DragDrop( P3DGUIManager.LastMouseOverCtrl, DragDropSrc, X, Y, Accept );
                if ( not Accept ) then
                  DragDropSrc.DragCancel( DragDropSrc, X, Y );
                DragDropSrc:= nil;
              end;
            P3DGUIManager.LastMouseDownCtrl[ i ]:= nil;
          end;

  for Control in Self do
    if ( Control.Enabled and Control.Visible ) then
      begin
        //Handle Mouse

        with P3DInput.Mouse do
          Control.MouseAction( mx, my, Buttons[ 0 ], Buttons[ 1 ], Buttons[ 2 ], DButtons[ 0 ], DButtons[ 1 ], DButtons[ 2 ]);
        Control.KeyboardAction();
      end;
end;

function TP3DControlList.MouseRay( X, Y: Integer; const Recurse: Boolean ): TP3DGraphicControl;
var
  i: Integer;
begin
  Result:= nil;
  for i:= Count - 1 downto 0 do // The Control on top of the stack is rendered last so this will be the control on top
    if ( Items[ i ].Visible and Items[ i ].Enabled and Items[ i ].Canvas.Rect.PtInRect( vec2( X, Y ))) then
      begin
        if ( Recurse ) then
          Result:= Items[ i ].Controls.MouseRay( X, Y, Recurse );

        if ( not Assigned( Result )) then
          if ( Items[ i ].MouseRay( X, Y )) then
            Result:= Items[ i ]
          else
            continue;
        break;
      end;
end;

{$ENDIF}
