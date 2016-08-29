{$IFDEF INTERFACE}

TP3DCustomControlList = specialize gP3DCustomObjectList < TP3DGraphicControl >;

{ TP3DControlList }

TP3DControlList = class( TP3DCustomControlList )
  private
    FPageSize: TVec2;
    FParent: TP3DGraphicControl;

  public
    procedure Realign;
    procedure Render(BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect);
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
    property PageSize: TVec2 read FPageSize;
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
  Controls.Add( GetItems( Index ));
  inherited Delete( Index );
end;

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
      Parent.Realign;
      R:= Parent.ClientRect;
      R.Left:= 0;
      R.Top:= 0; //Left/Top start already at the Client Rect
    end
  else if P3DViewports.Count > 0 then
    with ( P3DViewports[ 0 ]) do
      R:= P3DRect( 0, 0, Width, Height )
  else //NO VIEWPORTS AND NO PARENT
    exit;
  FPageSize:= vec2( 0 );
  for Control in Self do
    if ( Control.Visible ) then
      begin
        case Control.Align of //TODO: Fix Bug that Controls positioned at the ClientRect should have a zero position value not the R.TopLeft
          alClient: continue;
          alLeft:
            begin
              Control.FLeft:= R.Left;
              Control.FHeight:= R.Height;
              R.Left:= R.Left + Control.Width;
              R.Width:= R.Width - Control.Width;
            end;
          alRight:
            begin
              Control.FLeft:= R.Left + R.Width - Control.Width;
              Control.FTop:= R.Top;
              Control.FHeight:= R.Height;
              R.Right:= R.Right - Control.Width;
            end;
          alTop:
            begin
              Control.FTop:= R.Top;
              Control.FLeft:= R.Left;
              Control.FWidth:= R.Width;
              R.Top:= R.Top + Control.Height;
              R.Height:= R.Height - Control.Height;
            end;
          alBottom:
            begin
              Control.FLeft:= R.Left;
              Control.FWidth:= R.Width;
              Control.FTop:= R.Bottom - Control.Height;
              R.Height:= R.Height - Control.Height;
            end;
          //UpdateControlAndBoundary( R.Left, Control.FLeft, Control.FHeight, R.Bottom - R.Top, Control.Width );
          //alRight: UpdateControlAndBoundary( R.FRight, Control.FLeft, Control.FHeight, R.Bottom - R.Top, -Control.Width );
          //alTop: UpdateControlAndBoundary( R.Top, Control.FTop, Control.FWidth, R.Right - R.Left, Control.Height );
          //alBottom: UpdateControlAndBoundary( R.Bottom, Control.FTop, Control.FWidth, R.Right - R.Left, -Control.Height );
        end;
        Control.Realign;
        Control.Controls.Realign;
      end;

  for Control in Self do
    if (( Control.Visible ) and ( Control.Align = alClient )) then
      begin
        Control.FLeft:= R.Left;
        Control.FTop:= R.Top;
        Control.FWidth:= R.Right - R.Left;
        Control.FHeight:= R.Bottom - R.Top;
        Control.Realign;
        Control.Controls.Realign;
        //FPageSize:= max( FPageSize, vec2( Control.Left + Control.Width, Control.Top + Control.Height ));
      end;
end;

procedure TP3DControlList.Render(BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect);
  procedure RenderIfInRect( Item: TP3DGraphicControl; Scroll: TVec2 );
  var
    R: TP3DRect;
  begin
    if ( Assigned( Parent )) then
      R:= Parent.ClientRect
    else
      R:= P3DRect( 0, 0, P3DGUIManager.ScreenCanvas.Width, P3DGUIManager.ScreenCanvas.Height );
    //if ( R.RectCollision( P3DRect( Item.Left + Scroll.x, Item.Top + Scroll.y, Item.Width, Item.Height ))) then
      Item.Render( BaseColor, Scroll, ClipRect );
  end;

var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Visible ) then
      if ( Items[ i ].Align = alNone ) then
        RenderIfInRect( Items[ i ], ScrollAcc )
      else
        RenderIfInRect( Items[ i ], vec2( 0 ));
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
    Realign;
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
    Realign;
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
    Realign;
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
    Realign;
end;

//Calls the keyboard and mouse input for all the controls in the list.
//The cursor position (?) is ignored for now and should be handled by the
//Control itself.
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
  if ( Result <> LastMouseOverCtrl ) then
    begin
      if ( Assigned( LastMouseOverCtrl )) then
        LastMouseOverCtrl.InputState:= LastMouseOverCtrl.InputState - [ gcisMouseOver ];
      if ( Assigned( Result )) then
        Result.InputState:= Result.InputState + [ gcisMouseOver ];
      LastMouseOverCtrl:= Result;
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
            LastMouseDownCtrl[ i ]:= LastMouseOverCtrl;
            if ( Assigned( LastMouseDownCtrl[ i ])) then
              LastMouseDownCtrl[ i ].InputState:= LastMouseDownCtrl[ i ].InputState + BtnInpState( i );
            P3DGUIManager.FocusedControl:= LastMouseOverCtrl;
          end
        else                    //released
          begin
            if ( Assigned( LastMouseDownCtrl[ i ])) then
              LastMouseDownCtrl[ i ].InputState:= LastMouseDownCtrl[ i ].InputState - BtnInpState( i );
            // Drag and Drop release
            if (( DragDropSrc <> nil )) then
              begin
                Accept:= False;
                if ( Assigned( LastMouseOverCtrl )) then
                  LastMouseOverCtrl.DragDrop( LastMouseOverCtrl, DragDropSrc, X, Y, Accept );
                if ( not Accept ) then
                  DragDropSrc.DragCancel( DragDropSrc, X, Y );
                DragDropSrc:= nil;
              end;
            LastMouseDownCtrl[ i ]:= nil;
          end;

  for Control in Self do
    begin
      //Handle Mouse

      with P3DInput.Mouse do
        Control.MouseAction( mx, my, Buttons[ 0 ], Buttons[ 1 ], Buttons[ 2 ], DButtons[ 0 ], DButtons[ 1 ], DButtons[ 2 ]);
      Control.KeyboardAction();
    end;
end;

function TP3DControlList.MouseRay( X, Y: Integer; const Recurse: Boolean ): TP3DGraphicControl;
  function InControlRect( Ctrl: TP3DGraphicControl ): Boolean;
  begin
    with ( Ctrl ) do
      Result:= PtInRect( Rect( Canvas.Left, Canvas.Top, Canvas.Left + Canvas.Width, Canvas.Top + Canvas.Height ), Point( X, Y ))
  end;

var
  i: Integer;
begin
  Result:= nil;
  for i:= Count - 1 downto 0 do // The Control on top of the stack is rendered last so this will be the control on top
    if ( Items[ i ].Visible and InControlRect( Items[ i ])) then
      begin
        if ( Recurse ) then
          Result:= Items[ i ].Controls.MouseRay( X, Y, Recurse );

        if ( Assigned( Result )) then
          break;
        if ( Items[ i ].MouseRay( X, Y )) then
          begin
            Result:= Items[ i ];
            break;
          end;
      end;
end;

{$ENDIF}
