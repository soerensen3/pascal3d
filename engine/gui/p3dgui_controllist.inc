{$IFDEF INTERFACE}

TP3DCustomControlList = specialize TP3DCustomObjectList < TP3DGraphicControl >;

{ TP3DControlList }

TP3DControlList = class( TP3DCustomControlList )
  private
    FManager: TP3DGUIManager;
    FParent: TP3DGraphicControl;

  public
    procedure Realign;
    procedure Render(base: TVec4; ScrollAcc: TVec2);
    function Input: TP3DGraphicControl;
    function MouseRay(X, Y: Integer; const Recurse: Boolean): TP3DGraphicControl;
    procedure Clear( const FreeObjects: Boolean = False ); override;
    procedure Delete( Itm: TP3DGraphicControl ); overload;
    procedure MoveTo( Index: Integer; Controls: TP3DControlList );
    procedure BringToFront( Index: Integer );
    procedure SendToBack( Index: Integer );
    procedure OneLayerUp( Index: Integer );
    procedure OneLayerDown( Index: Integer );

    constructor Create( AParent: TP3DGraphicControl; AManager: TP3DGUIManager );
    destructor Destroy; override;

    property Parent: TP3DGraphicControl read FParent;
    property Manager: TP3DGUIManager read FManager write FManager;
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
        Items[ i ].Controls:= Manager.Controls;
        Manager.Controls.Add( Items[ i ]);
      end;
  Count:= 0;
end;

procedure TP3DControlList.Delete(Itm: TP3DGraphicControl);
begin
  Delete( IndexOf( Itm ));
end;

constructor TP3DControlList.Create(AParent: TP3DGraphicControl;
  AManager: TP3DGUIManager);
begin
  inherited Create;
  FParent:= AParent;
  Manager:= AManager;
end;

destructor TP3DControlList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TP3DControlList.MoveTo(Index: Integer; Controls: TP3DControlList);
begin
  Controls.Add( GetItems( Index ));
  inherited Delete( Index );
end;

procedure TP3DControlList.Realign;
var
  R: TRect;
  Control: TP3DGraphicControl;

  // Boundary = Left/Right/Top/Bottom Boundary?
  // ControlPos = Position to be updated
  // BoundaryLength = The length of the boundary
  // BoundaryShift =

  procedure UpdateControlAndBoundary( var Boundary: Integer; var ControlPos: Integer; var ControlLength: Integer; BoundaryLength, BoundaryShift: Integer ); inline;
  begin
    if ( BoundaryShift < 0 ) then
      ControlPos:= Boundary + BoundaryShift
    else
      ControlPos:= Boundary;
    ControlLength:= BoundaryLength;
    Boundary+= BoundaryShift;
  end;

begin
  if ( Parent <> nil ) then
    begin
      Parent.UpdateClientRect;
      R:= Parent.ClientRect;
      R.Right+= R.Left;
      R.Bottom+= R.Top;
    end
  else
    with ( Manager.ScreenCanvas ) do
      R:= Rect( Left, Top, Width + Left, Height + Top );
  for Control in Self do
    if ( Control.Visible ) then
      begin
        case Control.Align of
          alClient: continue;
          alLeft: UpdateControlAndBoundary( R.Left, Control.FLeft, Control.FHeight, R.Bottom - R.Top, Control.Width );
          alRight: UpdateControlAndBoundary( R.Right, Control.FLeft, Control.FHeight, R.Bottom - R.Top, -Control.Width );
          alTop: UpdateControlAndBoundary( R.Top, Control.FTop, Control.FWidth, R.Right - R.Left, Control.Height );
          alBottom: UpdateControlAndBoundary( R.Bottom, Control.FTop, Control.FWidth, R.Right - R.Left, -Control.Height );
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
      end;

end;

procedure TP3DControlList.Render(base: TVec4; ScrollAcc: TVec2);
var
  I: Integer;
begin
  for I:= 0 to Count - 1 do
    if ( Items[ I ].Visible ) then
      Items[ I ].Render( base, ScrollAcc );
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

  function BtnInpState( Idx: Integer ): TP3DGCInputFlags; // Hack for the use in an array
  begin
    Result:= TP3DGCInputFlags([ TP3DInputState( Integer( gcisMouseBtn1Down ) shl Idx )]);
  end;

begin
  mx:= GUIManager.MouseX;
  my:= GUIManager.MouseY;

  Result:= MouseRay( mx, my, True );
  if ( Result <> LastMouseOverCtrl ) then
    begin
      if ( Assigned( LastMouseOverCtrl )) then
        LastMouseOverCtrl.InputState:= LastMouseOverCtrl.InputState - [ gcisMouseOver ];
      if ( Assigned( Result )) then
        Result.InputState:= Result.InputState + [ gcisMouseOver ];
      LastMouseOverCtrl:= Result;
    end;

  for i:= 0 to 2 do
    with InputManager.Mouse do
      if ( DButtons[ i ]) then
        if ( Buttons[ i ]) then //pushed
          begin
            LastMouseDownCtrl[ i ]:= LastMouseOverCtrl;
            if ( Assigned( LastMouseDownCtrl[ i ])) then
              LastMouseDownCtrl[ i ].InputState:= LastMouseDownCtrl[ i ].InputState + BtnInpState( i );
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
                  begin
                    p3dwindow.ActiveWindow.Title:= DragDropSrc.Name + '->' + LastMouseOverCtrl.Name;
                    LastMouseOverCtrl.DragDrop( LastMouseOverCtrl, DragDropSrc, X, Y, Accept );
                  end;
                if ( not Accept ) then
                  DragDropSrc.DragCancel( DragDropSrc, X, Y );
                DragDropSrc:= nil;
              end;
            LastMouseDownCtrl[ i ]:= nil;
          end;

  for Control in Self do
    begin
      //Handle Mouse

      with InputManager.Mouse do
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
