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
    p3dviewport;

  type
    TP3DGraphicControl = class;
//    TP3DFocusControl = class;
    TControlBuffer = class;

    TGUIDrawProc = procedure( Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Single ) of Object;
    TGUIMouseClick = procedure( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer ) of Object;
    TGUIHover = procedure( Sender: TP3DGraphicControl; X, Y: Integer ) of Object;
    TGUIMouseMove = procedure( Sender: TP3DGraphicControl; IsOver: Boolean; X, Y: Integer ) of Object;

    TControlAlign = ( alNone, alLeft, alRight, alClient, alTop, alBottom );

    { TP3DCanvasFont }

    TP3DCanvasFont = class ( TPersistent )
      private
        FFontColor: TVec4;
        FFontName: String;
        FFontSize: Integer;
        FOnChange: TNotifyEvent;

        procedure SetFontColor(AValue: TVec4);
        procedure SetFontName(AValue: String);
        procedure SetFontSize(AValue: Integer);

      public
        procedure Assign( Font: TP3DCanvasFont );
        constructor Create;

        property Color: TVec4 read FFontColor write SetFontColor; //Object properties can't be published for some reason

      published
        property Name: String read FFontName write SetFontName;
        property Size: Integer read FFontSize write SetFontSize;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
    end;

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
        procedure Render( base: TVec4);
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

    { TP3DCanvas }

    TP3DCanvas = class( TPersistent )
      private
        FOwner: TP3DGraphicControl;
        FFont: TP3DCanvasFont;
//        FPen: TPen;
//        FBrush: TBrush;
        FOnChange: TNotifyEvent;
        FOnChanging: TNotifyEvent;
//        FTexture: TTexture;
//        procedure CreateBrush;
//        procedure CreateFont;
//        procedure CreatePen;
//        procedure BrushChanged(ABrush: TObject);
        function GetClipRect: TRect;
//        function GetPixel(X, Y: Integer): TColor;
//        procedure FontChanged(AFont: TObject);
//        procedure PenChanged(APen: TObject);
//        procedure SetBrush(Value: TBrush);
//        procedure SetFont( Value: TBitmapFont );
//        procedure SetPen(Value: TPen);
//        procedure SetPixel(X, Y: Integer; Value: TColor);

      protected
        procedure Changed; virtual;
        procedure Changing; virtual;

      public
        constructor Create( AOwner: TP3DGraphicControl );
        destructor Destroy; override;

        procedure Render3DFrame( pt0, pt1: TVec2; c0, c1, c2, c3: TVec4; EdgeDarken: Float );
        procedure RenderRect( pt0, pt1: TVec2; c0, c1, c2, c3: TVec4 );
        procedure RenderShadeFrame( pt0, pt1: TVec2; w: Single; c0, c1: TVec4 );
        procedure RenderShade( pt0, pt1: TVec2; w: Single; c0, c1: TVec4 );
        procedure RenderLineRect( pt0, pt1: TVec2; c0, c1, c2, c3: TVec4 );
        procedure RenderLine( pt0, pt1: TVec2; c0, c1: TVec4 );
        procedure RenderLineRect( pt0, pt1: TVec2; c: TVec4 );
        procedure RenderLine( pt0, pt1: TVec2; c: TVec4 );
        procedure RenderRect( pt0, pt1: TVec2; c: TVec4 );

        procedure RenderText( Text: TP3DText; P: TVec2 );
        procedure RenderTextAdv( Text: String; P: TPoint );
        //function TextWidth( Text: String ): Integer;
        //function TextWidthAdv( Text: String ): Integer;

{        procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
        procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap;
          const Source: TRect; Color: TColor);
        procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
        procedure CopyRect(const Dest: TRect; Canvas: TP3DCanvas;
          const Source: TRect);
        procedure Draw(X, Y: Integer; Graphic: TGraphic);
        procedure DrawFocusRect(const Rect: TRect);
        procedure Ellipse(X1, Y1, X2, Y2: Integer); overload;
        procedure Ellipse(const Rect: TRect); overload;
        procedure FillRect(const Rect: TRect);
        procedure FloodFill(X, Y: Integer; Color: TColor; FillStyle: TFillStyle);
        procedure FrameRect(const Rect: TRect);
        function HandleAllocated: Boolean;
        procedure LineTo(X, Y: Integer);
        procedure Lock;
        procedure MoveTo(X, Y: Integer);
        procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
        procedure Polygon(const Points: array of TPoint);
        procedure Polyline(const Points: array of TPoint);
        procedure PolyBezier(const Points: array of TPoint);
        procedure PolyBezierTo(const Points: array of TPoint);
        procedure Rectangle(X1, Y1, X2, Y2: Integer); overload;
        procedure Rectangle(const Rect: TRect); overload;
        procedure Refresh;
        procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
        procedure StretchDraw(const Rect: TRect; Graphic: TGraphic);
        function TextExtent(const Text: string): TSize;
        function TextHeight(const Text: string): Integer;
        procedure TextOut(X, Y: Integer; const Text: string);
        procedure TextRect(Rect: TRect; X, Y: Integer; const Text: string);
        function TextWidth(const Text: string): Integer;
        function TryLock: Boolean;
        procedure Unlock;}

        property ClipRect: TRect read GetClipRect;
//        property Pixels[X, Y: Integer]: TColor read GetPixel write SetPixel;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
        property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;

      published
//        property Brush: TBrush read FBrush write SetBrush;
        property Font: TP3DCanvasFont read FFont write FFont;
//        property Texture: TTexture read FTexture write FTexture;
//        property Pen: TPen read FPen write SetPen;
    end;

  {$DEFINE INTERFACE}
  {$INCLUDE p3dgui_graphiccontrol.inc}

  {$UNDEF INTERFACE}

var
  GUIManager: TGUIManager;

implementation

uses
  StrUtils,
  Types;

{ TP3DCanvasFont }

procedure TP3DCanvasFont.SetFontName(AValue: String);
begin
  if FFontName=AValue then Exit;
  FFontName:=AValue;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

procedure TP3DCanvasFont.SetFontColor(AValue: TVec4);
begin
  if FFontColor=AValue then Exit;
  FFontColor:=AValue;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

procedure TP3DCanvasFont.SetFontSize(AValue: Integer);
begin
  if FFontSize=AValue then Exit;
  FFontSize:=AValue;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

procedure TP3DCanvasFont.Assign(Font: TP3DCanvasFont);
begin
  if ( Assigned( Font )) then
    begin
      Name:= Font.Name;
      Size:= Font.Size;
      Color:= Font.Color;
    end;
end;

constructor TP3DCanvasFont.Create;
begin
  inherited;
  Size:= 16;
  Color:= vec4( 0, 0, 0, 1 );
  Name:= 'Deja Vu Sans';
end;


{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgui_graphiccontrol.inc}

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

procedure TControlBuffer.Render(base: TVec4);
var
  I: Integer;
begin
  //Engine.DisplayDriver.TCAlphaArg1[ 0 ]:= caTEXTURE;
  //Engine.DisplayDriver.TCAlphaArg2[ 0 ]:= caDIFFUSE;
  //Engine.DisplayDriver.TCAlphaOp[ 0 ]:= coMODULATE;

  for I:= 0 to Count - 1 do
    if ( Items[ I ].Visible ) then
      Items[ I ].Render( base );
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



procedure TP3DCanvas.Render3DFrame(pt0, pt1: TVec2; c0, c1, c2, c3: TVec4;
  EdgeDarken: Float);
var
  Col: TVec4;
  c0b,
  c1b,
  c1d,
  c2d,
  c3d,
  c3b: TVec4;

begin
  c0b:= ( c0 + EdgeDarken ).Normalize;
  c1b:= ( c1 + EdgeDarken ).Normalize;
  c1d:= ( c1 - EdgeDarken ).Normalize;
  c2d:= ( c2 - EdgeDarken ).Normalize;
  c3b:= ( c3 + EdgeDarken ).Normalize;
  c3d:= ( c3 - EdgeDarken ).Normalize;

  RenderLineRect2D( pt0, pt1, c0, c1, c2, c3, 1.0 );
end;

procedure TP3DCanvas.RenderLine(pt0, pt1: TVec2; c0, c1: TVec4);
begin
  c0:= FOwner.FScreenColor * c0;
  c1:= FOwner.FScreenColor * c1;
  RenderLine2D( vec2( pt0.x + FOwner.FScreenLeft, pt0.y + FOwner.FScreenTop ),
    vec2( pt1.x + FOwner.FScreenLeft, pt1.y + FOwner.FScreenTop ), c0, c1, 1.0 );
end;

procedure TP3DCanvas.RenderLineRect(pt0, pt1: TVec2; c: TVec4);
begin
  c:= FOwner.FScreenColor * c;
  RenderLineRect2D( vec2( pt0.x + FOwner.FScreenLeft, pt0.y + FOwner.FScreenTop ),
    vec2( pt1.x + FOwner.FScreenLeft, pt1.y + FOwner.FScreenTop ), c, 1.0 );
end;

procedure TP3DCanvas.RenderLine(pt0, pt1: TVec2; c: TVec4);
begin
  c:= FOwner.FScreenColor * c;
  RenderLine2D( vec2( pt0.x + FOwner.FScreenLeft, pt0.y + FOwner.FScreenTop ),
    vec2( pt1.x + FOwner.FScreenLeft, pt1.y + FOwner.FScreenTop ), c, 1.0 );
end;

procedure TP3DCanvas.RenderRect(pt0, pt1: TVec2; c: TVec4);
begin
  c:= FOwner.FScreenColor * c;
  RenderRect2D( vec2( pt0.x + FOwner.FScreenLeft, pt0.y + FOwner.FScreenTop ),
                vec2( pt1.x + FOwner.FScreenLeft, pt1.y + FOwner.FScreenTop ), c );
end;

procedure TP3DCanvas.RenderLineRect(pt0, pt1: TVec2; c0, c1, c2, c3: TVec4);
begin
  c0:= FOwner.FScreenColor * c0;
  c1:= FOwner.FScreenColor * c1;
  c2:= FOwner.FScreenColor * c2;
  c3:= FOwner.FScreenColor * c3;
  RenderLineRect2D( vec2( pt0.x + FOwner.FScreenLeft, pt0.y + FOwner.FScreenTop ),
    vec2( pt1.x + FOwner.FScreenLeft, pt1.y + FOwner.FScreenTop ), c0, c1, c2, c3, 1.0 );
end;

procedure TP3DCanvas.RenderRect(pt0, pt1: TVec2; c0, c1, c2, c3: TVec4);
begin
  c0:= FOwner.FScreenColor * c0;
  c1:= FOwner.FScreenColor * c1;
  c2:= FOwner.FScreenColor * c2;
  c3:= FOwner.FScreenColor * c3;
  RenderRect2D( vec2( pt0.x + FOwner.FScreenLeft, pt0.y + FOwner.FScreenTop ),
                vec2( pt1.x + FOwner.FScreenLeft, pt1.y + FOwner.FScreenTop ), c0, c1, c2, c3 );
end;

procedure TP3DCanvas.RenderText( Text: TP3DText; P: TVec2 );
begin
  P.x:= P.x + FOwner.FScreenLeft;
  P.y:= P.y + FOwner.FScreenTop;

  Text.Render( P, FOwner.FScreenColor * Font.Color );
end;

procedure TP3DCanvas.RenderTextAdv( Text: String; P: TPoint );
//var
//  f: TBitmapFont;
begin
{  P.x:= P.x + FOwner.FScreenLeft;
  P.y:= P.y + FOwner.FScreenTop;
  f:= GetFontObject;
  f.DrawTextAdv( Text, P, Font.Size, ColorMultiplyW( FOwner.FScreenColor, Font.Color ));}
end;

procedure TP3DCanvas.RenderShade(pt0, pt1: TVec2; w: Single; c0, c1: TVec4);
var
  halfw: Single;
  //vp,
  //Oldvp: TViewport;
begin
  halfw:= w / 2;

{  vp.MinZ:= 0.0;
  vp.MaxZ:= 1.0;

  vp.X:= Round( pt0 - halfw + FOwner.FScreenLeft );
  vp.Y:= Round( pt1 - halfw + FOwner.FScreenTop );
  vp.Width:= Round( pt2 - pt0 + 2*w + halfw );
  vp.Height:= Round( pt3 - pt1 + 2*w + halfw );

  Oldvp:= FOwner.Engine.DisplayDriver.GetViewport;
  FOwner.Engine.DisplayDriver.SetViewport( vp );}

  RenderShadeFrame( pt0 + halfw, pt1 - halfw, w, c0, c1 );
  RenderRect( pt0 + halfw, pt1 - halfw, c1, c1, c1, c1 );
  //FOwner.Engine.DisplayDriver.SetViewport( Oldvp );
end;

procedure TP3DCanvas.RenderShadeFrame(pt0, pt1: TVec2; w: Single; c0, c1: TVec4
  );
begin
  c0:= FOwner.FScreenColor * c0;
  c1:= FOwner.FScreenColor * c1;

  with ( FOwner ) do
    begin
      RenderQuad2D(
        vec2( pt0.x + FScreenLeft - w, pt0.y + FScreenTop - w ),
        vec2( pt0.x + FScreenLeft - w, pt1.y + FScreenTop + w ),
        vec2( pt0.x + FScreenLeft, pt0.y + FScreenTop ),
        vec2( pt0.x + FScreenLeft, pt1.y + FScreenTop ),
        c0, c0, c1, c1 );
      RenderQuad2D(
        vec2( pt0.x + FScreenLeft - w, pt0.y + FScreenTop - w ),
        vec2( pt0.x + FScreenLeft, pt0.y + FScreenTop ),
        vec2( pt1.x + FScreenLeft + w, pt0.y + FScreenTop - w ),
        vec2( pt1.x + FScreenLeft, pt0.y + FScreenTop ),
        c0, c1, c0, c1 );
      RenderQuad2D(
        vec2( pt0.x + FScreenLeft, pt1.y + FScreenTop ),
        vec2( pt0.x + FScreenLeft - w, pt1.y + FScreenTop + w ),
        vec2( pt1.x + FScreenLeft, pt1.y + FScreenTop ),
        vec2( pt1.x + FScreenLeft + w, pt1.y + FScreenTop + w ),
        c1, c0, c1, c0 );
      RenderQuad2D(
        vec2( pt1.x + FScreenLeft, pt0.y + FScreenTop ),
        vec2( pt1.x + FScreenLeft, pt1.y + FScreenTop ),
        vec2( pt1.x + FScreenLeft + w, pt0.y + FScreenTop - w ),
        vec2( pt1.x + FScreenLeft + w, pt1.y + FScreenTop + w ),
        c1, c1, c0, c0 );
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
  Controls.Render( vec4( 1 ));
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

{ TP3DCanvas }

procedure TP3DCanvas.Changed;
begin

end;

procedure TP3DCanvas.Changing;
begin

end;

constructor TP3DCanvas.Create( AOwner: TP3DGraphicControl );
begin
  inherited Create;
  FFont:= TP3DCanvasFont.Create;
  FOwner:= AOwner;
end;

destructor TP3DCanvas.Destroy;
begin
  FOwner:= nil;
  FFont.Free;
  inherited;
end;

function TP3DCanvas.GetClipRect: TRect;
begin
  Result:= Rect( FOwner.FScreenLeft, FOwner.FScreenTop, FOwner.FScreenLeft + FOwner.FScreenWidth, FOwner.FScreenTop + FOwner.FScreenHeight );
end;

{procedure TP3DCanvas.SetFont( Value: TBitmapFont );
begin

end;}
{
function TP3DCanvas.GetFontObject: TBitmapFont;
var
  n: Integer;
begin
  n:= FOwner.Manager.FontManager.GetFontByName( Font.Name );
  if ( n < 0 ) then
    Result:= FOwner.Manager.DefaultFont
  else
    Result:= FOwner.Manager.FontManager[ n ];
end;

function TP3DCanvas.TextWidth(Text: String ): Integer;
var
  f: TBitmapFont;
begin
  f:= GetFontObject;
  Result:= f.TextWidth( Text, Font.Size );
end;

function TP3DCanvas.TextWidthAdv(Text: String ): Integer;
var
  f: TBitmapFont;
begin
  f:= GetFontObject;
  Result:= f.TextWidthAdv( Text, Font.Size );
end;
}

initialization
  GUIManager:= TGUIManager.Create;

finalization
  GUIManager.Free;


end.

