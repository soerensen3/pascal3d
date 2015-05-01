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
    TGUIMouseMove = procedure( Sender: TP3DGraphicControl; IsOver: Boolean; X, Y: Integer ) of Object;

    TControlAlign = ( alNone, alLeft, alRight, alClient, alTop, alBottom );


    { TGUIManager }

    TGUIManager = class
      private
        FWindow: TSDLWindow;

        function GetMouseX: Integer;
        function GetMouseY: Integer;
        procedure SetMouseX( const Value: Integer );
        procedure SetMouseY( const Value: Integer );

      public
        Controls: TControlBuffer;

        constructor Create;
        destructor Destroy; override;
        procedure Render;
        procedure Input;

      published
        property MouseX: Integer read GetMouseX write SetMouseX;
        property MouseY: Integer read GetMouseY write SetMouseY;
        property Window: TSDLWindow read FWindow write FWindow;
    end;

    {$MACRO ON}
    {$DEFINE TCustomList:= TCustomControlList}
    {$DEFINE TCustomListEnumerator:= TControlEnumerator}
    {$DEFINE TCustomItem:= TP3DGraphicControl}
    {$DEFINE INTERFACE}
    {$INCLUDE p3dcustomlist.inc}

    { TControlBuffer }

    TControlBuffer = class( TCustomControlList )
      protected
        Manager: TGUIManager;
        FParent: TP3DGraphicControl;

      public
        procedure Realign;
        procedure Render(base: TVec4; ScrollAcc: TVec2);
        function Input: TP3DGraphicControl;
        constructor Create( AParent: TP3DGraphicControl; AManager: TGUIManager );
        destructor Destroy; override;
        procedure Clear( DestroyObjects: Boolean = False ); reintroduce;
        procedure Delete( Itm: TP3DGraphicControl ); overload;
        procedure MoveTo( Index: Integer; Controls: TControlBuffer );
        procedure BringToFront( Index: Integer );
        procedure SendToBack( Index: Integer );
        procedure OneLayerUp( Index: Integer );
        procedure OneLayerDown( Index: Integer );

        property Parent: TP3DGraphicControl read FParent;
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


{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgui_graphiccontrol.inc}
{.$INCLUDE p3dgui_canvas.inc}

{$MACRO ON}
{$DEFINE TCustomList:= TCustomControlList}
{$DEFINE TCustomListEnumerator:= TControlEnumerator}
{$DEFINE TCustomItem:= TP3DGraphicControl}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{ TControlBuffer }


procedure TControlBuffer.Clear(DestroyObjects: Boolean);
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    if ( DestroyObjects ) then
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
  //Engine.DisplayDriver.TCAlphaArg1[ 0 ]:= caTEXTURE;
  //Engine.DisplayDriver.TCAlphaArg2[ 0 ]:= caDIFFUSE;
  //Engine.DisplayDriver.TCAlphaOp[ 0 ]:= coMODULATE;

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

  Result:= nil;

  for Control in Self do
    begin
      //Handle Mouse
      with InputManager.Mouse do
        Result:= Control.MouseAction( mx, my, Buttons[ 0 ], Buttons[ 1 ], Buttons[ 2 ], DButtons[ 0 ], DButtons[ 1 ], DButtons[ 2 ]);
      Control.KeyboardAction();
    end;
end;

procedure TP3DGraphicControl.UpdateClientRect;
begin
  FClientRect:= Rect( BoundsLeft, BoundsTop, Width - BoundsRight - BoundsLeft, Height - BoundsBottom - BoundsTop );
end;

procedure TP3DGraphicControl.SetBounds( Index: Integer; const Value: Integer );
begin
  case ( Index ) of
    0: FBoundsLeft := Value;
    1: FBoundsTop := Value;
    2: FBoundsRight := Value;
    3: FBoundsBottom := Value;
  end;
  Realign;
  Controls.Realign;
end;

{ TGUIManager }

constructor TGUIManager.Create();
begin
  inherited Create;

//  DefaultFont:= FontManager[ FontManager.Add( '..\Fonts\Arial.fnt' )];
  Controls:= TControlBuffer.Create( nil, Self );
end;

destructor TGUIManager.Destroy;
begin
  Controls.Clear( True );
  Controls.Free;
  inherited;
end;

function TGUIManager.GetMouseX: Integer;
var
  cpos: TPoint;
begin
  Result:= InputManager.Mouse.X;
  {
  if ( Engine.Wnd <> nil ) then
    cpos:= Engine.Wnd.Cursor
  else
    begin
      GetCursorPos( cpos );
      ScreenToClient( Engine.DisplayDriver.CurSwapChain.Handle, cpos );
    end;
  Result:= cpos.x;}
end;

function TGUIManager.GetMouseY: Integer;
var
  cpos: TPoint;
begin
  Result:= InputManager.Mouse.Y;
{  if ( Engine.Wnd <> nil ) then
    cpos:= Engine.Wnd.Cursor
  else
    begin
      GetCursorPos( cpos );
      ScreenToClient( Engine.DisplayDriver.CurSwapChain.Handle, cpos );
    end;
  Result:= cpos.y;}
end;

procedure TGUIManager.Input;
var
  Control: Pointer;
begin
  Controls.Input;
end;

procedure TGUIManager.Render;
begin
  Controls.Render( vec4( 1 ), vec2(0));
end;

procedure TGUIManager.SetMouseX( const Value: Integer );
var
  cpos: TPoint;
begin
  InputManager.Mouse.X:= MainWindow.Left + Value;
{  if ( Engine.Wnd <> nil ) then
    cpos:= Engine.Wnd.Cursor
  else
    GetCursorPos( cpos );

  ClientToScreen( Engine.DisplayDriver.CurSwapChain.Handle, cpos );
  cpos.Y:= Value;

  SetCursorPos( cpos.x, cpos.y );}
end;

procedure TGUIManager.SetMouseY(const Value: Integer);
var
  cpos: TPoint;
begin
  InputManager.Mouse.Y:= MainWindow.Top + Value
  {if ( Engine.Wnd <> nil ) then
    cpos:= Engine.Wnd.Cursor
  else
    GetCursorPos( cpos );

  ClientToScreen( Engine.DisplayDriver.CurSwapChain.Handle, cpos );
  cpos.Y:= Value;

  SetCursorPos( cpos.x, cpos.y );}
end;



initialization
  GUIManager:= TGUIManager.Create;

finalization
  GUIManager.Free;


end.

