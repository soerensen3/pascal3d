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
    strutils,
    SDL2,
    LazFileUtils,
    p3dutils,
    p3devents,
    p3dgraphics,
    p3dMath;

  type
    TP3DGraphicControl = class;
    TP3DControlList = class;

    TP3DGUIDraw       = procedure( Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Single ) of Object;
    TP3DGUIMouseClick = procedure( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer ) of Object;
    TP3DGUIMouseEvent = procedure( Sender: TP3DGraphicControl; X, Y: Integer ) of Object;
    TP3DGUIDragDrop   = procedure( Sender: TP3DGraphicControl; Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean ) of Object;


    TP3DControlAlign = ( alNone, alLeft, alRight, alClient, alTop, alBottom );

    { TP3DRect }

    TP3DRect = object
      private
        FHeight: Float;
        FTop: Float;
        FLeft: Float;
        FWidth: Float;

        function GetBottom: Float;
        function GetBottomRight: TVec2;
        function GetRight: Float;
        function GetTopLeft: TVec2;
        procedure SetBottom(AValue: Float);
        procedure SetBottomRight(AValue: TVec2);
        procedure SetRight(AValue: Float);
        procedure SetTopLeft(AValue: TVec2);

      public
        function PtInRect( P: TVec2 ): Boolean;

        property TopLeft: TVec2 read GetTopLeft write SetTopLeft;
        property BottomRight: TVec2 read GetBottomRight write SetBottomRight;
        property Left: Float read FLeft write FLeft;
        property Top: Float read FTop write FTop;
        property Width: Float read FWidth write FWidth;
        property Height: Float read FHeight write FHeight;
        property Bottom: Float read GetBottom write SetBottom;
        property Right: Float read GetRight write SetRight;
    end;

  {$DEFINE INTERFACE}
  {$INCLUDE p3dgui_manager.inc}
  {$INCLUDE p3dgui_controllist.inc}
  {$INCLUDE p3dgui_graphiccontrol.inc}

  {$INCLUDE p3dgui_buttons.inc}
  {$INCLUDE p3dgui_stdctrls.inc}
  {$INCLUDE p3dgui_commonctrls.inc}

  {$UNDEF INTERFACE}

  function P3DRect( Left, Top, Width, Height: Float ): TP3DRect;
  function P3DRectEx(Left, Top, Right, Bottom: Float): TP3DRect;
  function P3DRectEx( TopLeft, BottomRight: TVec2 ): TP3DRect;

var
  P3DGUIManager: TP3DGUIManager;
  DragDropSrc: TP3DGraphicControl;

procedure P3DGUIInit;
procedure P3DGUIFinish;

implementation

uses
  Types;

var
  LastMouseOverCtrl: TP3DGraphicControl;
  LastMouseDownCtrl: array[ 0..2 ] of TP3DGraphicControl;

{ TP3DRect }

function TP3DRect.GetBottomRight: TVec2;
begin
  Result:= TopLeft + vec2( Width, Height );
end;

function TP3DRect.GetBottom: Float;
begin
  Result:= TopLeft.y + Height;
end;

function TP3DRect.GetRight: Float;
begin
  Result:= TopLeft.x + Width;
end;

function TP3DRect.GetTopLeft: TVec2;
begin
  Result:= vec2( Left, Top );
end;

procedure TP3DRect.SetBottom(AValue: Float);
begin
  Height:= Max( 0, AValue - Top );
end;

procedure TP3DRect.SetBottomRight(AValue: TVec2);
begin
  Width:= Max( 0, AValue.x - Left );
  Height:= Max( 0, AValue.y - Top );
end;

procedure TP3DRect.SetRight(AValue: Float);
begin
  Width:= Max( 0, AValue - Left );
end;

procedure TP3DRect.SetTopLeft(AValue: TVec2);
begin
  Left:= AValue.X;
  Top:= AValue.Y;
end;

function TP3DRect.PtInRect(P: TVec2): Boolean;
begin
  Result:= ( P.x >= Left ) and ( P.x < Right )
       and ( P.Y >= Top )  and ( P.y < Bottom );
end;

function P3DRect( Left, Top, Width, Height: Float ): TP3DRect;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Width:= Width;
  Result.Height:= Height;
end;

function P3DRectEx(Left, Top, Right, Bottom: Float): TP3DRect;
begin
  Result.Left:= Left;
  Result.Top:= Top;
  Result.Right:= Right;
  Result.Bottom:= Bottom;
end;

function P3DRectEx(TopLeft, BottomRight: TVec2): TP3DRect;
begin
  Result.TopLeft:= TopLeft;
  Result.BottomRight:= BottomRight;
end;


{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgui_manager.inc}
{$INCLUDE p3dgui_controllist.inc}
{$INCLUDE p3dgui_graphiccontrol.inc}

{$INCLUDE p3dgui_buttons.inc}
{$INCLUDE p3dgui_stdctrls.inc}
{$INCLUDE p3dgui_commonctrls.inc}


procedure P3DGUIInit;
begin
  if ( not Assigned( P3DGUIManager )) then
    P3DGUIManager:= TP3DGUIManager.Create;
  LastMouseOverCtrl:= nil;
  LastMouseDownCtrl[ 0 ]:= nil;
  LastMouseDownCtrl[ 1 ]:= nil;
  LastMouseDownCtrl[ 2 ]:= nil;
end;

procedure P3DGUIFinish;
begin
  if ( Assigned( P3DGUIManager )) then
    FreeAndNil( P3DGUIManager );
end;

finalization
  P3DUtilsFinish;



end.

