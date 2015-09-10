// ##################################
// # <----------------------------- #
// #       Revelation Engine        #
// # -----------------------------> #
// ##################################
// #                                #
// # P3D Interface                  #
// ##################################

unit p3dgui;

interface
  uses
    Classes,
    SysUtils,
    Contnrs,
    math,
    clipbrd,
//    GlobalVars,
    SDL2,
    p3dgenerics,
    p3dgeometry,
    p3dwindow,
    p3dobjects,
    p3dinput,
    p3dbmpfont,
    p3dMath,
    p3dviewport,
    p3dcanvas;

  type
    TP3DGraphicControl = class;
//    TP3DFocusControl = class;
    TControlBuffer = class;

    TGUIDrawProc = procedure( Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Single ) of Object;
    TGUIMouseClick = procedure( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer ) of Object;
    TGUIHover = procedure( Sender: TP3DGraphicControl; X, Y: Integer ) of Object;
    TGUIMouseMove = procedure( Sender: TP3DGraphicControl; X, Y: Integer ) of Object;
    TGUIDragDropProc = procedure( Sender: TP3DGraphicControl; Source: TP3DGraphicControl; X, Y: Integer ) of Object;


    TControlAlign = ( alNone, alLeft, alRight, alClient, alTop, alBottom );


    { TGUIManager }

    TGUIManager = class
      private
        FCanvas: TP3DCanvas2D;
        FControls: TControlBuffer;
        FWindow: TSDLWindow;

        function GetMouseX: Integer;
        function GetMouseY: Integer;
        procedure SetMouseX( const Value: Integer );
        procedure SetMouseY( const Value: Integer );

      public
        constructor Create;
        destructor Destroy; override;

        procedure Render;
        procedure Input;
        procedure UpdateExtents;

      published
        property MouseX: Integer read GetMouseX write SetMouseX;
        property MouseY: Integer read GetMouseY write SetMouseY;
        property Window: TSDLWindow read FWindow write FWindow;
        property ScreenCanvas: TP3DCanvas2D read FCanvas write FCanvas;
        property Controls: TControlBuffer read FControls write FControls;
    end;

    TCustomControlList = specialize TP3DCustomObjectList < TP3DGraphicControl >;

    { TControlBuffer }

    TControlBuffer = class( TCustomControlList )
    private
      FLastMouseInputCtrl: TP3DGraphicControl;
      protected
        Manager: TGUIManager;
        FParent: TP3DGraphicControl;

      public
        procedure Realign;
        procedure Render(base: TVec4; ScrollAcc: TVec2);
        function Input: TP3DGraphicControl;
        function MouseRay( X,Y: Integer; const Recurse: Boolean = True; UpdateInpState: Boolean = False ): TP3DGraphicControl;
        procedure Clear( const FreeObjects: Boolean = False ); override;
        procedure Delete( Itm: TP3DGraphicControl ); overload;
        procedure MoveTo( Index: Integer; Controls: TControlBuffer );
        procedure BringToFront( Index: Integer );
        procedure SendToBack( Index: Integer );
        procedure OneLayerUp( Index: Integer );
        procedure OneLayerDown( Index: Integer );

        constructor Create( AParent: TP3DGraphicControl; AManager: TGUIManager );
        destructor Destroy; override;

        property Parent: TP3DGraphicControl read FParent;
        property LastMouseInputCtrl: TP3DGraphicControl read FLastMouseInputCtrl write FLastMouseInputCtrl;
    end;


  {$DEFINE INTERFACE}
  {.$INCLUDE p3dgui_canvas.inc}
  {$INCLUDE p3dgui_graphiccontrol.inc}

  {$UNDEF INTERFACE}

var
  GUIManager: TGUIManager;

implementation

uses
  StrUtils,
  Types;

var
  DragDropSrc: TP3DGraphicControl;


{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgui_graphiccontrol.inc}
{.$INCLUDE p3dgui_canvas.inc}


{ TControlBuffer }


procedure TControlBuffer.Clear(const FreeObjects: Boolean);
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

procedure TControlBuffer.Delete(Itm: TP3DGraphicControl);
begin
  Delete( IndexOf( Itm ));
end;

constructor TControlBuffer.Create(AParent: TP3DGraphicControl;
  AManager: TGUIManager);
begin
  inherited Create;
  FParent:= AParent;
  Manager:= AManager;
end;

destructor TControlBuffer.Destroy;
begin
  Clear;
  inherited;
end;

procedure TControlBuffer.MoveTo(Index: Integer; Controls: TControlBuffer);
begin
  Controls.Add( GetItems( Index ));
  inherited Delete( Index );
end;

procedure TControlBuffer.Realign;
var
  I: Integer;
  BoundsTL: TPoint;
  BoundsBR: TPoint;
  _BoundsTL: TPoint;
  _BoundsBR: TPoint;
  Dim: TPoint;
  _Dim: TPoint;
begin
  if ( Parent <> nil ) then
    begin
      Dim.x:= Round( TP3DGraphicControl( Parent ).FWidth ) -
        TP3DGraphicControl( Parent ).BoundsLeft -
        TP3DGraphicControl( Parent ).BoundsRight;
      Dim.y:= Round( TP3DGraphicControl( Parent ).FHeight ) -
        TP3DGraphicControl( Parent ).BoundsTop -
        TP3DGraphicControl( Parent ).BoundsBottom;

//      BoundsTL.x:= TP3DGraphicControl( Parent ).FBound1.x;
//      BoundsTL.y:= TP3DGraphicControl( Parent ).FBound1.y;

      _Dim.x:= Round( TP3DGraphicControl( Parent ).FWidth );
      _Dim.y:= Round( TP3DGraphicControl( Parent ).FHeight );
      FillByte( _BoundsTL, SizeOf( TPoint ), 0 );
      FillByte( BoundsTL, SizeOf( TPoint ), 0 );
    end
  else
    begin
//      Dim.x:= Engine.DisplayDriver.CurSwapChain.Displ.XRes; TODO: REPAIR
//      Dim.y:= Engine.DisplayDriver.CurSwapChain.Displ.YRes;
      _Dim:= Dim;
      FillByte( BoundsTL, SizeOf( TPoint ), 0 );
    end;
  BoundsBR.x:= Dim.x;
  BoundsBR.y:= Dim.y;
  _BoundsBR.x:= _Dim.x;
  _BoundsBR.y:= _Dim.y;

  for I:= 0 to Count - 1 do
    if ( Items[ I ].Align <> alNone ) then
      with Items[ I ] do
        if ( Visible ) then
          begin
            if ( FNoBounds ) then
              begin
                case Align of
                  alLeft:
                    begin
                      FLeft:= _BoundsTL.x;
                      FTop:= _BoundsTL.y;
                      FHeight:= _BoundsBR.y - _BoundsTL.y;
                      Inc( _BoundsTL.x, Round( FWidth ));
                      Realign;
                      if ( Assigned( OnParentRealign )) then
                        OnParentRealign( Self );
                    end;
                  alRight:
                    begin
                      Dec( _BoundsBR.x, Round( FWidth ));
                      FLeft:= _BoundsBR.x;
                      FTop:= _BoundsTL.y;
                      FHeight:= _BoundsBR.y - _BoundsTL.y;
                      Realign;
                      if ( Assigned( OnParentRealign )) then
                        OnParentRealign( Self );
                    end;
                  alClient:
                    Realign;
                  alTop:
                    begin
                      FTop:= _BoundsTL.y;
                      FLeft:= _BoundsTL.x;
                      FWidth:= _BoundsBR.x - _BoundsTL.x;
                      Inc( _BoundsTL.y, Round( FHeight ));
                      Realign;
                      if ( Assigned( OnParentRealign )) then
                        OnParentRealign( Self );
                    end;
                  alBottom:
                    begin
                      Dec( _BoundsBR.y, Round( FHeight ));
                      FTop:= _BoundsBR.y;
                      FLeft:= _BoundsTL.x;
                      FWidth:= _BoundsBR.x - _BoundsTL.x;
                      Realign;
                      if ( Assigned( OnParentRealign )) then
                        OnParentRealign( Self );
                    end;
                end;
                if ( _BoundsTL.x > _BoundsBR.x ) then
                  _BoundsTL.x:= _BoundsBR.x;
                if ( _BoundsTL.y > _BoundsBR.y ) then
                  _BoundsTL.y:= _BoundsBR.y;
              end
            else
              begin
                case Align of
                  alLeft:
                    begin
                      FLeft:= BoundsTL.x;
                      FTop:= BoundsTL.y;
                      FHeight:= BoundsBR.y - BoundsTL.y;
                      Inc( BoundsTL.x, Round( FWidth ));
                      Realign;
                      if ( Assigned( OnParentRealign )) then
                        OnParentRealign( Self );
                    end;
                  alRight:
                    begin
                      Dec( BoundsBR.x, Round( FWidth ));
                      FLeft:= BoundsBR.x;
                      FTop:= BoundsTL.y;
                      FHeight:= BoundsBR.y - BoundsTL.y;
                      Realign;
                      if ( Assigned( OnParentRealign )) then
                        OnParentRealign( Self );
                    end;
                  alClient, alNone:
                    Realign;
                  alTop:
                    begin
                      FTop:= BoundsTL.y;
                      FLeft:= BoundsTL.x;
                      FWidth:= BoundsBR.x - BoundsTL.x;
                      Inc( BoundsTL.y, Round( FHeight ));
                      Realign;
                      if ( Assigned( OnParentRealign )) then
                        OnParentRealign( Self );
                    end;
                  alBottom:
                    begin
                      Dec( BoundsBR.y, Round( FHeight ));
                      FTop:= BoundsBR.y;
                      FLeft:= BoundsTL.x;
                      FWidth:= BoundsBR.x - BoundsTL.x;
                      Realign;
                      if ( Assigned( OnParentRealign )) then
                        OnParentRealign( Self );
                    end;
                end;
                if ( BoundsTL.x > BoundsBR.x ) then
                  BoundsTL.x:= BoundsBR.x;
                if ( BoundsTL.y > BoundsBR.y ) then
                  BoundsTL.y:= BoundsBR.y;
              end;
        end;
  for I:= 0 to Count - 1 do
    begin
      if ( Items[ I ].Align = alClient ) then
        with Items[ I ] do
          if ( FNoBounds ) then
            begin
              FLeft:= _BoundsTL.x;
              FTop:= _BoundsTL.y;
              FWidth:= _BoundsBR.x - _BoundsTL.x;
              FHeight:= _BoundsBR.y - _BoundsTL.y;
              Realign;
              if ( Assigned( OnParentRealign )) then
                OnParentRealign( Self );
            end
          else
            begin
              FLeft:= BoundsTL.x;
              FTop:= BoundsTL.y;
              FWidth:= BoundsBR.x - BoundsTL.x;
              FHeight:= BoundsBR.y - BoundsTL.y;
              Realign;
              if ( Assigned( OnParentRealign )) then
                OnParentRealign( Self );
            end;
      Items[ I ].Realign;
      Items[ I ].Controls.Realign;
    end;
end;

procedure TControlBuffer.Render(base: TVec4; ScrollAcc: TVec2);
var
  I: Integer;
begin
  for I:= 0 to Count - 1 do
    if ( Items[ I ].Visible ) then
      Items[ I ].Render( base, ScrollAcc );
end;

procedure TControlBuffer.BringToFront( Index: Integer );
var
  Item: TP3DGraphicControl;
begin
  if ( Index + 1 = Count ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( Count, Item );
end;

procedure TControlBuffer.OneLayerDown( Index: Integer );
var
  Item: TP3DGraphicControl;
begin
  if ( Index = 0 ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( Index - 1, Item );
end;

procedure TControlBuffer.OneLayerUp( Index: Integer );
var
  Item: TP3DGraphicControl;
begin
  if ( Index + 1 = Count ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( Index, Item );
end;

procedure TControlBuffer.SendToBack( Index: Integer );
var
  Item: TP3DGraphicControl;
begin
  if ( Index = 0 ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( 0, Item );
end;

//Calls the keyboard and mouse input for all the controls in the list.
//The cursor position is ignored for now and should be handled by the
//Control itself.
function TControlBuffer.Input: TP3DGraphicControl;

var
  Control: TP3DGraphicControl;
  mx, my: Integer;
begin
  mx:= GUIManager.MouseX;
  my:= GUIManager.MouseY;

  Result:= MouseRay( mx, my, True, True );
  if ( Assigned( Result )) then
    with InputManager.Mouse do
      Result.MouseAction( mx, my, Buttons[ 0 ], Buttons[ 1 ], Buttons[ 2 ], DButtons[ 0 ], DButtons[ 1 ], DButtons[ 2 ]);

  for Control in Self do
    begin
      //Handle Mouse
      if ( Control <> Result ) then //We already called that before
        with InputManager.Mouse do
          begin
            Control.MouseAction( mx, my, Buttons[ 0 ], Buttons[ 1 ], Buttons[ 2 ], DButtons[ 0 ], DButtons[ 1 ], DButtons[ 2 ]);
            Control.Controls.Input;
          end;
      Control.KeyboardAction();
    end;
end;

function TControlBuffer.MouseRay( X, Y: Integer; const Recurse: Boolean;
  UpdateInpState: Boolean ): TP3DGraphicControl;

  function SetFlag( Flag: TP3DGCInputState; Yes: Boolean ): TP3DGCInputFlags;
  begin
    if ( Yes ) then
      Result:= [ Flag ]
    else
      Result:= [];
  end;

  function SetIS( MOver: TP3DGCInputState ): TP3DGCInputFlags;
  begin //ONLY FIRE EVENT IF BUTTON IS PRESSED IN THE CONTROLS CLIENT RECT
    Result:= [ Mover ] + SetFlag( gcisMouseBtn1Down, InputManager.Mouse.Buttons[ 0 ] and InputManager.Mouse.DButtons[ 0 ])
                       + SetFlag( gcisMouseBtn2Down, InputManager.Mouse.Buttons[ 1 ] and InputManager.Mouse.DButtons[ 1 ])
                       + SetFlag( gcisMouseBtn3Down, InputManager.Mouse.Buttons[ 2 ] and InputManager.Mouse.DButtons[ 2 ]);
  end;

  function NegIS(): TP3DGCInputFlags;
  begin //NEGATIVE BUTTONS NEEDED TO FIRE MOUSE UP EVENT DELAYED EVEN IF THE BUTTON IS RELEASED OUTSIDE OF THE CONTROL
    Result:=   SetFlag( gcisMouseBtn1Down, not InputManager.Mouse.Buttons[ 0 ])
             + SetFlag( gcisMouseBtn2Down, not InputManager.Mouse.Buttons[ 1 ])
             + SetFlag( gcisMouseBtn3Down, not InputManager.Mouse.Buttons[ 2 ])
  end;

var
  i: Integer;
begin
  Result:= nil;
  for i:= Count - 1 downto 0 do
    if ( Items[ i ].Visible and Items[ i ].MouseRay( X, Y )) then
      begin
        if ( not Assigned( Result )) then
          begin
            if ( Recurse ) then
              Result:= Items[ i ].Controls.MouseRay( X, Y, Recurse, UpdateInpState );
            if ( UpdateInpState ) then
              begin
                if ( not Assigned( Result )) then
                  begin
                    Result:= Items[ i ];
                    Items[ i ].InputState:= SetIS( gcisMouseOver )
                  end
                else
                  Items[ i ].InputState:= SetIS( gcisMouseOverOccl );
              end
            else
              Break;
          end
        else
          Result.InputState:= SetIS( gcisMouseOverOccl );
      end
    else
      Items[ i ].InputState:= Items[ i ].InputState - NegIS() - [ gcisMouseOverOccl, gcisMouseOver ];
end;


{ TGUIManager }

constructor TGUIManager.Create();
begin
  inherited Create;

//  DefaultFont:= FontManager[ FontManager.Add( '..\Fonts\Arial.fnt' )];
  Controls:= TControlBuffer.Create( nil, Self );
  ScreenCanvas:= TP3DCanvas2D.Create( nil );
  UpdateExtents;
end;

destructor TGUIManager.Destroy;
begin
  ScreenCanvas.Free;
  Controls.Clear( True );
  Controls.Free;
  inherited;
end;

function TGUIManager.GetMouseX: Integer;
var
  cpos: TPoint;
begin
  Result:= InputManager.Mouse.X;
end;

function TGUIManager.GetMouseY: Integer;
var
  cpos: TPoint;
begin
  Result:= InputManager.Mouse.Y;
end;

procedure TGUIManager.Input;
var
  Ctrl: TP3DGraphicControl;
begin
  Ctrl:= Controls.Input;
  if (( Assigned( Ctrl )) AND ( gcisMouseOverOccl in Ctrl.InputState )) then
    Ctrl.InputState:= ( Ctrl.InputState + [ gcisMouseOver ]) - [ gcisMouseOverOccl ];
end;

procedure TGUIManager.Render;
begin
  Controls.Render( vec4( 1 ), vec2( 0 ));
end;

procedure TGUIManager.SetMouseX( const Value: Integer );
var
  cpos: TPoint;
begin
  InputManager.Mouse.X:= ActiveWindow.Left + Value;
end;

procedure TGUIManager.SetMouseY(const Value: Integer);
var
  cpos: TPoint;
begin
  InputManager.Mouse.Y:= ActiveWindow.Top + Value
end;

procedure TGUIManager.UpdateExtents;
begin
  if ( P3DViewports.Count > 0 ) then
    begin
      ScreenCanvas.Left:= P3DViewports.VP[ 0 ].Left;
      ScreenCanvas.Top:= P3DViewports.VP[ 0 ].Top;
      ScreenCanvas.Width:= P3DViewports.VP[ 0 ].Width;
      ScreenCanvas.Height:= P3DViewports.VP[ 0 ].Height;
    end;
  Controls.Realign;
end;



initialization
  GUIManager:= TGUIManager.Create;

finalization
  GUIManager.Free;


end.

