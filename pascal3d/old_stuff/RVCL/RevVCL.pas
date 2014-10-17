// ##################################
// # <----------------------------- #
// #       Revelation Engine        #
// # -----------------------------> #
// ##################################
// #                                #
// # RVCL: CONTAINS REV VCL         #
// ##################################

unit RevVCL;

interface
  uses
    Classes,
    SysUtils,
    Contnrs,
    clipbrd,
//    GlobalVars,
    Math3d;

  type
    TRevGraphicControl = class;
    TRevFocusControl = class;
    TControlBuffer = class;

    TVCLDrawProc = procedure( Sender: TRevGraphicControl; OffSetX, OffSetY, _Width, _Height: Single ) of Object;
    TVCLMouseClick = procedure( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer ) of Object;
    TVCLHover = procedure( Sender: TRevGraphicControl; X, Y: Integer ) of Object;
    TVCLMouseMove = procedure( Sender: TRevGraphicControl; IsOver: Boolean; X, Y: Integer ) of Object;

    TControlAlign = ( alNone, alLeft, alRight, alClient, alTop, alBottom );

    TVCLManager = class
      private
        FFocus: TRevFocusControl;
        _CurInputCtrl: TRevGraphicControl;
        _CurMOver: TRevGraphicControl;
        _MUp,
        _MDown: Boolean;
        _c: TPoint;
        TestKey: array[ 0..255 ] of Boolean;
        LastKey: Integer;

        function GetMouseX: Integer;
        function GetMouseY: Integer;
        procedure SetMouseX(const Value: Integer);
        procedure SetMouseY(const Value: Integer);

      public
        _Activeform: TRevGraphicControl;
        Controls: TControlBuffer;

        constructor Create;
        destructor Destroy; override;
        procedure Render;
        procedure Input;

      published
        property Focus: TRevFocusControl read FFocus write FFocus;
        property MouseX: Integer read GetMouseX write SetMouseX;
        property MouseY: Integer read GetMouseY write SetMouseY;
    end;

    { TControlBuffer }

    TControlBuffer = class( TList )
      protected
        Manager: TVCLManager;
        FParent: TRevGraphicControl;

        function Get( Index: Integer ): TRevGraphicControl;
        procedure Put( Index: Integer; Item: TRevGraphicControl );

      public
        procedure Realign;
        procedure Render;
        procedure Input;
        constructor Create( AParent: TRevGraphicControl; AManager: TVCLManager );
        destructor Destroy; override;
        procedure Clear( DestroyObjects: Boolean = False ); reintroduce;
        procedure Delete( Index: Integer ); overload;
        procedure Delete( Item: TRevGraphicControl ); overload;
        procedure MoveTo( Index: Integer; Controls: TControlBuffer );
        function Add( Item: TRevGraphicControl ): Integer;
        function IndexOf( Item: TRevGraphicControl ): Integer;
        procedure BringToFront( Index: Integer );
        procedure SendToBack( Index: Integer );
        procedure OneLayerUp( Index: Integer );
        procedure OneLayerDown( Index: Integer );

        property Items[ Index: Integer ]: TRevGraphicControl read Get write Put; default;
        property Parent: TRevGraphicControl read FParent;
    end;

    TFont = class( TPersistent )
      private
        FSize: Integer;
        FName: String;
        FColor: Cardinal;

      published
        property Size: Integer read FSize write FSize;
        property Color: Cardinal read FColor write FColor;
        property Name: String read FName write FName;
    end;

    TRevCanvas = class( TPersistent )
      private
        FOwner: TRevGraphicControl;
        FFont: TFont;
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
        constructor Create( AOwner: TRevGraphicControl );
        destructor Destroy; override;

        procedure Render3DFrame( pt0, pt1, pt2, pt3: Single; c0, c1, c2, c3: DWord; EdgeDarken: Integer );
        procedure RenderRect( pt0, pt1, pt2, pt3: Single; c0, c1, c2, c3: DWord );
        procedure RenderShadeFrame( pt0, pt1, pt2, pt3, w: Single; c0, c1: DWord );
        procedure RenderShade( pt0, pt1, pt2, pt3, w: Single; c0, c1: DWord );
        procedure RenderLineRect( pt0, pt1, pt2, pt3: Single; c0, c1, c2, c3: DWord );
        procedure RenderLine( pt0, pt1, pt2, pt3: Single; c0, c1: DWord );
        procedure RenderText( Text: String; P: TPoint );
        procedure RenderTextAdv( Text: String; P: TPoint );
        function TextWidth( Text: String ): Integer;
        function TextWidthAdv( Text: String ): Integer;

{        procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
        procedure BrushCopy(const Dest: TRect; Bitmap: TBitmap;
          const Source: TRect; Color: TColor);
        procedure Chord(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
        procedure CopyRect(const Dest: TRect; Canvas: TRevCanvas;
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
        property Font: TFont read FFont write FFont;
//        property Texture: TTexture read FTexture write FTexture;
//        property Pen: TPen read FPen write SetPen;
    end;

    TRevGraphicControlClass = class of TRevGraphicControl;
    TRevGraphicControl = class
      protected
        FLeft,
        FTop,
        FWidth,
        FHeight: Integer;
        FIndex: Integer;
        FOwnerCtrlBuf: TControlBuffer;
        FControls: TControlBuffer;
        FScreenLeft: Integer;
        FScreenTop: Integer;
        FScreenWidth: Integer;
        FScreenHeight: Integer;
        FScrollX: Integer;
        FScrollY: Integer;
        FVisible,
        FEnabled: Boolean;
        FOnDraw: TVCLDrawProc;
        FOnMouseDown: TVCLMouseClick;
        FOnMouseUp: TVCLMouseClick;
        FOnMouseClick: TVCLMouseClick;
        FOnMouseMove: TVCLMouseMove;
        FOnMouseEnter: TVCLHover;
        FOnMouseLeave: TVCLHover;
        FOnKeyDown: TNotifyEvent;
        FAlign: TControlAlign;
        FNoBound: Boolean;
        FMinWidth,
        FMinHeight,
        FMaxWidth,
        FMaxHeight: DWord;
        FLastClick: Integer;
        FOnParentRealign: TNotifyEvent;
        FTag: Integer;
        FBaseColor: Cardinal;
        FScreenColor: Cardinal;
        FClientRect: TRect;
        FBoundLeft: Integer;
        FBoundTop: Integer;
        FBoundBottom: Integer;
        FBoundRight: Integer;
        FCanvas: TRevCanvas;

//        FSkin: TTexture;
        FParentSkin: Boolean;

        procedure Render( Color: Cardinal ); virtual;
        procedure UpdateInput( OffSetX, OffSetY, _Width, _Height: Single ); virtual;
        procedure SetAlign( const Value: TControlAlign ); virtual;
        procedure SetHeight( const Value: Integer ); virtual;
        procedure SetLeft( const Value: Integer ); virtual;
        procedure SetTop( const Value: Integer ); virtual;
        procedure SetWidth( const Value: Integer ); virtual;
        procedure SetVisible( const Value: Boolean ); virtual;
        procedure SetParentSkin( const Value: Boolean );
//        procedure SetSkin( const Value: TTexture );
        procedure SetBound(const Index, Value: Integer);
        procedure Realign; virtual;
        function GetParent: TRevGraphicControl;
        procedure SetParent(const Value: TRevGraphicControl);
        procedure UpdateClientRect;

      public
        Manager: TVCLManager;
//        Bound1: TPoint;
//        Bound2: TPoint;
        WasDown: Boolean;
        WasOver: Boolean;

        constructor Create( AOwner: TRevGraphicControl; AManager: TVCLManager; const AParent: TRevGraphicControl = nil );
        destructor Destroy; override;

        procedure Draw; virtual;
        procedure MouseMove( IsOver: Boolean; X, Y: Integer ); virtual;
        procedure MouseEnter( X, Y: Integer ); virtual;
        procedure MouseLeave( X, Y: Integer ); virtual;
        procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
        procedure MouseUp( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
        procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
        procedure MouseDblClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); virtual;
        procedure KeyDown( Loop: Boolean ); virtual;
        procedure BringToFront;
        procedure SendToBack;
        procedure OneLayerUp;
        procedure OneLayerDown;

        property ParentCtrl: TControlBuffer read FOwnerCtrlBuf write FOwnerCtrlBuf;
        property Controls: TControlBuffer read FControls write FControls;
        property Canvas: TRevCanvas read FCanvas write FCanvas;
        property ClientRect: TRect read FClientRect;

      published
        property BoundLeft: Integer index 0 read FBoundLeft write SetBound;
        property BoundTop: Integer index 1 read FBoundTop write SetBound;
        property BoundRight: Integer index 2 read FBoundRight write SetBound;
        property BoundBottom: Integer index 3 read FBoundBottom write SetBound;
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
        property OnDraw: TVCLDrawProc read FOnDraw write FOnDraw;
        property OnMouseDown: TVCLMouseClick read FOnMouseDown write FOnMouseDown;
        property OnMouseUp: TVCLMouseClick read FOnMouseUp write FOnMouseUp;
        property OnMouseClick: TVCLMouseClick read FOnMouseClick write FOnMouseClick;
        property OnMouseMove: TVCLMouseMove read FOnMouseMove write FOnMouseMove;
        property OnMouseEnter: TVCLHover read FOnMouseEnter write FOnMouseEnter;
        property OnMouseLeave: TVCLHover read FOnMouseLeave write FOnMouseLeave;
        property OnKeyDown: TNotifyEvent read FOnKeyDown write FOnKeyDown;
        property Align: TControlAlign read FAlign write SetAlign;
//        property Skin: TTexture read FSkin write SetSkin;
//        property ParentSkin: Boolean read FParentSkin write SetParentSkin;
        property NoBound: Boolean read FNoBound write FNoBound;
        property OnParentRealign: TNotifyEvent read FOnParentRealign write FOnParentRealign;
        property Tag: Integer read FTag write FTag;
        property Parent: TRevGraphicControl read GetParent write SetParent;
        property BaseColor: Cardinal read FBaseColor write FBaseColor;
    end;

    TRevFocusControl = class( TRevGraphicControl )
      protected
        procedure SetFocus( const Value: Boolean ); virtual;
        function GetFocus: Boolean; virtual;
        procedure UpdateInput( OffSetX, OffSetY, _Width, _Height: Single ); override;

      public
        procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
        procedure KeyDown( Loop: Boolean ); override;

      published
        property HasFocus: Boolean read GetFocus write SetFocus;
    end;

implementation

uses
  StrUtils,
  {RForms, }Types;

{ TControlBuffer }

function TControlBuffer.Add( Item: TRevGraphicControl ): Integer;
begin
  Result:= inherited Add( Item );
//  Item.Index:= IndexOf( Item );
end;

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

procedure TControlBuffer.Delete( Index: Integer );
begin
  inherited Delete( Index );
end;

constructor TControlBuffer.Create(AParent: TRevGraphicControl;
  AManager: TVCLManager);
begin
  inherited Create;
  FParent:= AParent;
  Manager:= AManager;
end;

procedure TControlBuffer.Delete( Item: TRevGraphicControl );
begin
  Delete( IndexOf( Item ));
end;

destructor TControlBuffer.Destroy;
begin
  Clear;
  inherited;
end;

function TControlBuffer.Get( Index: Integer ): TRevGraphicControl;
begin
  Result:= TRevGraphicControl( inherited Get( Index ));
end;

procedure TControlBuffer.MoveTo(Index: Integer; Controls: TControlBuffer);
begin
  Controls.Add( Get( Index ));
  inherited Delete( Index );
end;

procedure TControlBuffer.Put( Index: Integer; Item: TRevGraphicControl );
begin
  inherited Put( Index, Item );
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
      Dim.x:= Round( TRevGraphicControl( Parent ).FWidth ) -
        TRevGraphicControl( Parent ).BoundLeft -
        TRevGraphicControl( Parent ).BoundRight;
      Dim.y:= Round( TRevGraphicControl( Parent ).FHeight ) -
        TRevGraphicControl( Parent ).BoundTop -
        TRevGraphicControl( Parent ).BoundBottom;

//      BoundsTL.x:= TRevGraphicControl( Parent ).FBound1.x;
//      BoundsTL.y:= TRevGraphicControl( Parent ).FBound1.y;

      _Dim.x:= Round( TRevGraphicControl( Parent ).FWidth );
      _Dim.y:= Round( TRevGraphicControl( Parent ).FHeight );
      FillByte( _BoundsTL, SizeOf( TPoint ), 0 );
      FillByte( BoundsTL, SizeOf( TPoint ), 0 );
    end
  else
    begin
//      Dim.x:= Engine.DisplayDriver.CurSwapChain.Displ.XRes; TODO: REPAIR
//      Dim.y:= Engine.DisplayDriver.CurSwapChain.Displ.YRes;
      _Dim:= Dim;
      ZeroMemory( @BoundsTL, SizeOf( TPoint ));
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
            if ( FNoBound ) then
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
          if ( FNoBound ) then
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

procedure TControlBuffer.Render;
var
  I: Integer;
begin
  Engine.DisplayDriver.TCAlphaArg1[ 0 ]:= caTEXTURE;
  Engine.DisplayDriver.TCAlphaArg2[ 0 ]:= caDIFFUSE;
  Engine.DisplayDriver.TCAlphaOp[ 0 ]:= coMODULATE;

  for I:= 0 to Count - 1 do
    if ( Items[ I ].Visible ) then
      Items[ I ].Render( $FFFFFFFF );
end;

procedure TControlBuffer.BringToFront( Index: Integer );
var
  Item: TRevGraphicControl;
begin
  if ( Index + 1 = Count ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( Count, Item );
end;

function TControlBuffer.IndexOf( Item: TRevGraphicControl ): Integer;
begin
  Result:= inherited IndexOf( Item );
end;

procedure TControlBuffer.OneLayerDown( Index: Integer );
var
  Item: TRevGraphicControl;
begin
  if ( Index = 0 ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( Index - 1, Item );
end;

procedure TControlBuffer.OneLayerUp( Index: Integer );
var
  Item: TRevGraphicControl;
begin
  if ( Index + 1 = Count ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( Index, Item );
end;

procedure TControlBuffer.SendToBack( Index: Integer );
var
  Item: TRevGraphicControl;
begin
  if ( Index = 0 ) then
    exit;
  Item:= Items[ Index ];
  Delete( Items[ Index ]);
  Insert( 0, Item );
end;

procedure TControlBuffer.Input;
var
  i: Integer;

  kchanged: Boolean;
  knormal: Boolean;
  nextk: Boolean;
begin
  kchanged:= not CompareMem( @Manager.TestKey, @Manager.GameInput.KeysChanged, SizeOf( Manager.TestKey ));
  knormal:=  not CompareMem( @Manager.TestKey, @Manager.GameInput.Keys, SizeOf( Manager.TestKey ));
  nextk:= Manager.LastKey <= GetTickCount;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Visible and Items[ i ].Enabled ) then
      if ( kchanged ) then
        begin
          Manager.LastKey:= GetTickCount + 500;
          Items[ i ].KeyDown( False );
        end
      else if ( nextk and knormal ) then
        begin
          Items[ i ].KeyDown( True );
          Manager.LastKey:= GetTickCount + 100;
        end;
end;

{ TRevGraphicControl }

constructor TRevGraphicControl.Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
begin
  inherited Create( AOwner, AEngine );
  Manager:= _Manager;
  Controls:= TControlBuffer.Create( Engine, Self, Manager );
  FCanvas:= TRevCanvas.Create( Self );
  if ( Assigned( Parent )) then
    ParentCtrl:= Parent.Controls
  else
    ParentCtrl:= _Manager.Controls;
  ParentCtrl.Add( Self );

  Visible:= True;
  Enabled:= True;
  FScrollX:= 0;
  FScrollY:= 0;
  Width:= 50;
  Height:= 50;
  FBaseColor:= $FFFFFFFF;
  FParentSkin:= True;
end;

destructor TRevGraphicControl.Destroy;
begin
  ParentCtrl.Delete( Self );
  Controls.Free;
  Canvas.Free;
  inherited;
end;

procedure TRevGraphicControl.Draw;
begin
  if ( Assigned( FOnDraw )) then
    FOnDraw( Self, FScreenLeft, FScreenTop, FScreenWidth, FScreenHeight );
end;

function TRevGraphicControl.GetParent: TRevGraphicControl;
begin
  Result:= ParentCtrl.Parent;
end;

procedure TRevGraphicControl.KeyDown;
begin
  if ( Assigned( FOnKeyDown )) then
    FOnKeyDown( Self );
  Controls.Input;
end;

procedure TRevGraphicControl.MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  if (( X >= 0 ) and ( Y >= 0 ) and ( X <= FScreenWidth ) and ( Y <= FScreenHeight )) then
    if ( Assigned( FOnMouseClick )) then
      FOnMouseClick( Self, mb1, mb2, mb3, X, Y );
  if ( FLastClick >= GetTickCount ) then
    MouseDblClick( mb1, mb2, mb3, X, Y );
  FLastClick:= GetTickCount + 500;
end;

procedure TRevGraphicControl.MouseDblClick( mb1, mb2, mb3: Boolean; X,
  Y: Integer );
begin

end;

procedure TRevGraphicControl.MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer );
  procedure GetOwnerform( Ctrl: TBaseObject );
  begin
    if ( Ctrl is TRevGraphicControl ) then
      begin
        if ( Ctrl is TRevForm ) then
          TRevForm( Ctrl ).Active:= True
        else
          GetOwnerform( TRevGraphicControl( Ctrl ).FOwnerCtrlBuf.Parent );
      end
    else
      Manager._Activeform:= nil;
  end;
begin
  GetOwnerform( FOwnerCtrlBuf.Parent );
  if ( Assigned( FOnMouseDown )) then
    FOnMouseDown( Self, mb1, mb2, mb3, X, Y );
end;

procedure TRevGraphicControl.MouseEnter( X, Y: Integer );
begin
  if ( Assigned( FOnMouseEnter )) then
    FOnMouseEnter( Self, X, Y );
end;

procedure TRevGraphicControl.MouseLeave( X, Y: Integer );
begin
  if ( Assigned( FOnMouseLeave )) then
    FOnMouseLeave( Self, X, Y );
end;

procedure TRevGraphicControl.MouseMove( IsOver: Boolean; X, Y: Integer );
begin
  if ( Assigned( FOnMouseMove )) then
    FOnMouseMove( Self, IsOver, X, Y );
end;

procedure TRevGraphicControl.MouseUp( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  if ( Assigned( FOnMouseUp )) then
    FOnMouseUp( Self, mb1, mb2, mb3, X, Y );
  MouseClick( mb1, mb2, mb3, X, Y );
end;

procedure TRevGraphicControl.OneLayerDown;
begin
  FOwnerCtrlBuf.OneLayerDown( FOwnerCtrlBuf.IndexOf( Self ));
end;

procedure TRevGraphicControl.OneLayerUp;
begin
  FOwnerCtrlBuf.OneLayerUp( FOwnerCtrlBuf.IndexOf( Self ));
end;

procedure TRevGraphicControl.BringToFront;
begin
  FOwnerCtrlBuf.BringToFront( FOwnerCtrlBuf.IndexOf( Self ));
end;

procedure TRevGraphicControl.SendToBack;
begin
  FOwnerCtrlBuf.SendToBack( FOwnerCtrlBuf.IndexOf( Self ));
end;


procedure TRevGraphicControl.Realign;
begin
  if ( Assigned( Parent )) then
    begin
      if ( NoBound ) then
        begin
          FScreenLeft:= Left + Parent.FScreenLeft;
          FScreenTop:= Top + Parent.FScreenTop;
          FScreenWidth:= Min( Parent.Width - Left, Width );
          FScreenHeight:= Min( Parent.Height - Top, Height );
        end
      else
        begin
          FScreenLeft:= Left + Parent.FScreenLeft + Parent.ClientRect.Left;
          FScreenTop:= Top + Parent.FScreenTop + Parent.ClientRect.Top;
          FScreenWidth:= Min( Parent.ClientRect.Right - Left, Width );
          FScreenHeight:= Min( Parent.ClientRect.Bottom - Top, Height );
        end;
    end
  else
    begin
      FScreenLeft:= Left;
      FScreenTop:= Top;
      FScreenWidth:= Min( Engine.DisplayDriver.CurSwapChain.XRes - Left, Width );
      FScreenHeight:= Min( Engine.DisplayDriver.CurSwapChain.YRes - Top, Height );
    end;
  UpdateClientRect;
end;

procedure TRevGraphicControl.Render( Color: Cardinal );
var
  I,
  FX,
  FY,
  BX,
  BY,
  BH,
  BW,
  W,
  H: Integer;
  OldView,
  View: TViewport;
begin
  {
  if (( not FNoBound ) and ( OwnerCtrlBuf.Parent is TRevGraphicControl )) then
    begin
      FX:= Round( OffSetX + Left + TRevGraphicControl( FOwnerCtrlBuf.Parent ).FScrollX );
      FY:= Round( OffSetY + Top + TRevGraphicControl( FOwnerCtrlBuf.Parent ).FScrollY );
      W:= Round( Min( Width, _Width - Left - TRevGraphicControl( FOwnerCtrlBuf.Parent ).FScrollX ));
      H:= Round( Min( Height, _Height - Top - TRevGraphicControl( FOwnerCtrlBuf.Parent ).FScrollY ));
    end
  else
    begin
      FX:= Round( OffSetX + Left );
      FY:= Round( OffSetY + Top );
      W:= Round( Min( Width, _Width - Left ));
      H:= Round( Min( Height, _Height - Top ));
    end;

  View.X:= Max( 0, FX );
  View.Y:= Max( 0, FY );
  View.Width:= W;
  View.Height:= H;
  View.MinZ:=  0.0;
  View.MaxZ:=  1.0;

  if ( FX < 0 ) then
    View.Width:= View.Width + FX;
  if ( FY < 0 ) then
    View.Height:= View.Height + FY;

  if ( View.X < OffSetX ) then
    begin
      View.Width:= View.Width - ( Round( OffSetX ) - View.X );
      View.X:= Round( OffSetX );
    end;

  if ( View.Y < OffSetY ) then
    begin
      View.Height:= View.Height - ( Round( OffSetY ) - View.Y );
      View.Y:= Round( OffSetY );
    end;

  if ( View.Width > _Width ) then
    begin
      View.Width:= Round( _Width );
    end;

  if ( View.Height > _Height ) then
    begin
      View.Height:= Round( _Height );
    end;

  if ( W * H > 0 ) then
    begin
{      FScreenLeft:= FX;
      FScreenTop:= FY;
      FScreenWidth:= W;
      FScreenHeight:= H;}
{
      BX:= FX + BoundLeft;
      BY:= FY + BoundTop;
      BW:= Min( Round( Width ) - BoundRight - BoundLeft, W - BoundLeft );
      BH:= Min( Round( Height ) - BoundBottom - BoundTop, H - BoundTop );}

      View.X:= FScreenLeft;
      View.Y:= FScreenTop;
      View.Width:= FScreenWidth;
      View.Height:= FScreenHeight;
      View.MinZ:= 0.0;
      View.MaxZ:= 1.0;
      OldView:= Engine.DisplayDriver.GetViewport();
      Engine.DisplayDriver.SetViewport( View );

      Color:= ColorMultiplyW( Color, BaseColor );
      FScreenColor:= Color;

      Draw;

      View.X:= FScreenLeft + BoundRight;
      View.Y:= FScreenTop + BoundTop;
      View.Width:= FScreenWidth - BoundRight - BoundLeft;
      View.Height:= FScreenHeight - BoundBottom - BoundTop;
      View.MinZ:= 0.0;
      View.MaxZ:= 1.0;

      Engine.DisplayDriver.SetViewport( View );

      for I:= 0 to Controls.Count - 1 do
        if ( Controls[ I ].Visible ) then
          Controls[ I ].Render( Color );

      Manager.Engine.DisplayDriver.SetViewport( OldView );

{      with ( Engine.Painter ) do
        begin
          RenderLine2D(
            Vector( View.X, View.Y, 0 ),
            Vector( View.X + View.Width, View.Y, 0 ),
            $FFFF0000 );

          RenderLine2D(
            Vector( View.X, View.Y + View.Height, 0 ),
            Vector( View.X + View.Width, View.Y + View.Height, 0 ),
            $FFFF0000 );

          RenderLine2D(
            Vector( View.X, View.Y, 0 ),
            Vector( View.X, View.Y + View.Height, 0 ),
            $FFFF0000 );

          RenderLine2D(
            Vector( View.X + View.Width, View.Y, 0 ),
            Vector( View.X + View.Width, View.Y + View.Height, 0 ),
            $FFFF0000 );

          RenderLine2D(
            VectorZero,
            Vector( View.X, View.Y, 0 ),
            $FFFF0000 );
        end;
    end;}}
end;

procedure TRevCanvas.Render3DFrame( pt0, pt1, pt2, pt3: Single; c0, c1,
  c2, c3: DWord; EdgeDarken: Integer );
var
  Col: TColor;
  c0b,
  c1b,
  c1d,
  c2d,
  c3d,
  c3b: DWord;
  
begin
  Col.Col:= c0;
  Col.r:= Min( 255, Max( 0, Col.r + EdgeDarken ));
  Col.g:= Min( 255, Max( 0, Col.g + EdgeDarken ));
  Col.b:= Min( 255, Max( 0, Col.b + EdgeDarken ));
  c0b:= Col.Col;

  Col.Col:= c1;
  Col.r:= Min( 255, Max( 0, Col.r + EdgeDarken ));
  Col.g:= Min( 255, Max( 0, Col.g + EdgeDarken ));
  Col.b:= Min( 255, Max( 0, Col.b + EdgeDarken ));
  c1b:= Col.Col;

  Col.Col:= c1;
  Col.r:= Min( 255, Max( 0, Col.r - EdgeDarken ));
  Col.g:= Min( 255, Max( 0, Col.g - EdgeDarken ));
  Col.b:= Min( 255, Max( 0, Col.b - EdgeDarken ));
  c1d:= Col.Col;

  Col.Col:= c2;
  Col.r:= Min( 255, Max( 0, Col.r - EdgeDarken ));
  Col.g:= Min( 255, Max( 0, Col.g - EdgeDarken ));
  Col.b:= Min( 255, Max( 0, Col.b - EdgeDarken ));
  c2d:= Col.Col;

  Col.Col:= c3;
  Col.r:= Min( 255, Max( 0, Col.r + EdgeDarken ));
  Col.g:= Min( 255, Max( 0, Col.g + EdgeDarken ));
  Col.b:= Min( 255, Max( 0, Col.b + EdgeDarken ));
  c3b:= Col.Col;

  Col.Col:= c3;
  Col.r:= Min( 255, Max( 0, Col.r - EdgeDarken ));
  Col.g:= Min( 255, Max( 0, Col.g - EdgeDarken ));
  Col.b:= Min( 255, Max( 0, Col.b - EdgeDarken ));
  c3d:= Col.Col;

  RenderLine(
    pt0,
    pt1,
    pt2,
    pt1, c0b, c1b );

  RenderLine(
    pt0,
    pt3,
    pt2,
    pt3, c3d, c2d );

  RenderLine(
    pt0,
    pt1,
    pt0,
    pt3, c0b, c3b );

  RenderLine(
    pt2,
    pt1,
    pt2,
    pt3, c1d, c2d );
end;

procedure TRevCanvas.RenderLine( pt0, pt1, pt2, pt3: Single; c0, c1: DWord );
{  function VecS( x, y: Single ): TVector;
  begin
    Result:= Vector( x / Width * Engine.Wnd.Width, y / Height * Engine.Wnd.Height, 0.0 );
  end;}
begin
  if ( FOwner.Engine.Painter <> nil ) then
    FOwner.Engine.Painter.RenderLine2D( Vector( pt0 + FOwner.FScreenLeft, pt1 + FOwner.FScreenTop, 0.0 ),
      Vector( pt2 + FOwner.FScreenLeft, pt3 + FOwner.FScreenTop, 0.0 ), ColorMultiplyW( FOwner.FScreenColor, c0 ));
end;

procedure TRevCanvas.RenderLineRect( pt0, pt1, pt2, pt3: Single; c0,
  c1, c2, c3: DWord );
begin
  RenderLine(
    pt0,
    pt1,
    pt2,
    pt1, c0, c1 );

  RenderLine(
    pt0,
    pt3,
    pt2,
    pt3, c3, c2 );

  RenderLine(
    pt0,
    pt1,
    pt0,
    pt3, c0, c3 );

  RenderLine(
    pt2,
    pt1,
    pt2,
    pt3, c1, c2 );
end;

procedure TRevCanvas.RenderRect( pt0, pt1, pt2, pt3: Single; c0, c1,
  c2, c3: DWord );
var
  TexInfo: pTextureInfo;
begin
  if ( FOwner.Engine.Painter = nil ) then
    exit;
  if ( FOwner.Skin <> nil ) then
    TexInfo:= @FOwner.Skin.TexInfo
  else
    TexInfo:= nil;

  FOwner.Engine.Painter.RenderRect2DEx( Vector( pt0 + FOwner.FScreenLeft, pt1 + FOwner.FScreenTop, 0.0 ),
    Vector( pt2 + FOwner.FScreenLeft, pt3 + FOwner.FScreenTop, 0.0 ),
    ColorMultiplyW( FOwner.FScreenColor, c0 ), ColorMultiplyW( FOwner.FScreenColor, c1 ),
    ColorMultiplyW( FOwner.FScreenColor, c2 ), ColorMultiplyW( FOwner.FScreenColor, c3 ),
    0, 0, 1, {( pt3 - pt1 ) / 3} 1, TexInfo );
end;

procedure TRevCanvas.RenderText( Text: String; P: TPoint );
var
  f: TBitmapFont;
begin
  P.x:= P.x + FOwner.FScreenLeft;
  P.y:= P.y + FOwner.FScreenTop;
  f:= GetFontObject;
  f.DrawText( Text, P, Font.Size, ColorMultiplyW( FOwner.FScreenColor, Font.Color ));
end;

procedure TRevCanvas.RenderTextAdv( Text: String; P: TPoint );
var
  f: TBitmapFont;
begin
  P.x:= P.x + FOwner.FScreenLeft;
  P.y:= P.y + FOwner.FScreenTop;
  f:= GetFontObject;
  f.DrawTextAdv( Text, P, Font.Size, ColorMultiplyW( FOwner.FScreenColor, Font.Color ));
end;


procedure TRevGraphicControl.SetAlign( const Value: TControlAlign );
begin
  FAlign:= Value;
  ParentCtrl.Realign;
end;

procedure TRevGraphicControl.SetHeight( const Value: Integer );
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

procedure TRevGraphicControl.SetLeft( const Value: Integer );
begin
  FLeft:= Value;
  ParentCtrl.Realign;
  Controls.Realign;
end;

procedure TRevGraphicControl.SetParent( const Value: TRevGraphicControl );
begin
  if ( Value = Parent ) then
    exit;
  FOwnerCtrlBuf.Delete( Self );
  if ( Value = nil ) then
    exit;
  Value.Controls.Add( Self );
  FOwnerCtrlBuf:= Value.Controls;
end;

procedure TRevGraphicControl.SetParentSkin( const Value: Boolean );
begin
  FParentSkin:= Value;
  if ( Value ) then
    if ( ParentCtrl <> nil ) then
      if ( ParentCtrl.Parent <> nil ) then
        Skin:= ParentCtrl.Parent.Skin;
end;

procedure TRevGraphicControl.SetSkin( const Value: TTexture );
var
  I: Integer;
begin
  FSkin:= Value;
  for I:= 0 to Controls.Count - 1 do
    if ( Controls[ I ].ParentSkin ) then
      Controls[ I ].Skin:= Value;
end;

procedure TRevGraphicControl.SetTop( const Value: Integer );
begin
  FTop:= Value;
  ParentCtrl.Realign;
  Controls.Realign;
end;

procedure TRevGraphicControl.SetVisible( const Value: Boolean );
begin
  if ( FVisible <> Value ) then
    begin
      FVisible:= Value;
      FOwnerCtrlBuf.Realign;
    end;
end;

procedure TRevGraphicControl.SetWidth( const Value: Integer );
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

procedure TRevGraphicControl.UpdateInput( OffSetX, OffSetY, _Width,
  _Height: Single );
var
  I: Integer;
  OldView,
  Pos: TVector;
  Scal: TVector;
  MOver: Boolean;
  MDown: Boolean;
  MChanged: Boolean;
  BX,
  BY,
  W,
  H: Integer;
  CX, CY: Integer;
  CursorPos: TPoint;
begin
  if ( not ( Visible and Enabled )) then
    exit;

{  if (( not FNoBound ) and ( FOwnerCtrlBuf.Parent is TRevGraphicControl )) then
    begin
      Pos.x:= Round( OffSetX + Left + TRevGraphicControl( FOwnerCtrlBuf.Parent ).FScrollX );
      Pos.y:= Round( OffSetY + Top + TRevGraphicControl( FOwnerCtrlBuf.Parent ).FScrollY );
      Scal.X:= Max( 0, Round( Min( Width, _Width - TRevGraphicControl( FOwnerCtrlBuf.Parent ).FScrollX - Pos.x )));
      Scal.Y:= Max( 0, Round( Min( Height, _Height - TRevGraphicControl( FOwnerCtrlBuf.Parent ).FScrollY - Pos.y )));
    end
  else
    begin
      Pos.x:= OffSetX + Left;
      Pos.y:= OffSetY + Top;
      Scal.x:= Round( Min( Width, _Width - Pos.x ));
      Scal.y:= Round( Min( Height, _Height - Pos.y ));
    end;

{  Pos.X:= Round( OffSetX + Left );
  Pos.Y:= Round( OffSetY + Top );
  Scal.X:= Round( Min( Width, _Width ));
  Scal.Y:= Round( Min( Height, _Height ));}}
  Pos.x:= FScreenLeft;
  Pos.y:= FScreenTop;
  Scal.x:= FScreenWidth;
  Scal.y:= FScreenHeight;
  if (( Scal.x * Scal.y > 0 )) then
    begin
      GetCursorPos( CursorPos );
      ScreenToClient( Engine.DisplayDriver.CurSwapChain.Displ.Handle, CursorPos );
      MOver:= ( CursorPos.x >= Pos.X ) and ( CursorPos.x <= Scal.X + Pos.x ) and
              ( CursorPos.y >= Pos.Y ) and ( CursorPos.y <= Scal.Y + Pos.y );
      with ( Manager.GameInput ) do
        MDown:= ( MBtns[ 0 ] or MBtns[ 1 ] or MBtns[ 2 ]);

      with ( Manager.GameInput ) do
        MChanged:= ( MBtnsChanged[ 0 ] or MBtnsChanged[ 1 ] or MBtnsChanged[ 2 ]);

      CX:= Round( CursorPos.x - Pos.X );
      CY:= Round( CursorPos.y - Pos.Y );

      BX:= Round( Pos.x + BoundLeft );
      BY:= Round( Pos.y + BoundTop );
      W:= Round( Scal.x - BoundLeft + Pos.x );
      H:= Round( Scal.y - BoundLeft + Pos.y );


      with ( Manager.GameInput ) do
        begin
          if ( MPosChanged ) then
            begin
              MouseMove( WasOver, cx, cy );
            end;

          if ( MOver ) then
            begin
              Manager._CurMOver:= Self;

              if ( MDown ) then
                if ( MChanged ) then
                  begin
                    Manager._c:= Point( CX, CY );
                    Manager._MDown:= True;
                    Manager._CurInputCtrl:= Self;
                  end;
            end
          else
            if ( Manager._CurMOver = Self ) then
              Manager._CurMOver:= nil;

          if ( not MDown ) then
            if ( MChanged ) then
              if ( WasDown ) then
                begin
                  Manager._c:= Point( CX, CY );
                  Manager._MUp:= True;
                  Manager._CurInputCtrl:= Self;
                end;
        end;

    //  for I:= 0 to Controls.Count - 1 do
    //    Controls[ I ].UpdateInput( Pos.X, Pos.Y, Scal.X, Scal.Y );
      for I:= 0 to Controls.Count - 1 do
        if ( Controls[ I ].Enabled and Controls[ I ].Visible ) then
          if ( Controls[ I ].FNoBound ) then
            Controls[ I ].UpdateInput( Pos.x, Pos.y, Scal.x + Pos.x, Scal.y + Pos.y )
          else
            Controls[ I ].UpdateInput( BX, BY, W, H );

{      if ( MOver ) then
        begin
          if ( not WasOver ) then
            WasOver:= True;
        end
      else
        if ( WasOver ) then
          WasOver:= False;}
    end;
end;

procedure TRevCanvas.RenderShade( pt0, pt1, pt2, pt3, w: Single; c0,
  c1: DWord );
var
  halfw: Single;
  vp,
  Oldvp: TViewport;
begin
  halfw:= w / 2;

  vp.MinZ:= 0.0;
  vp.MaxZ:= 1.0;

  vp.X:= Round( pt0 - halfw + FOwner.FScreenLeft );
  vp.Y:= Round( pt1 - halfw + FOwner.FScreenTop );
  vp.Width:= Round( pt2 - pt0 + 2*w + halfw );
  vp.Height:= Round( pt3 - pt1 + 2*w + halfw );

  Oldvp:= FOwner.Engine.DisplayDriver.GetViewport;
  FOwner.Engine.DisplayDriver.SetViewport( vp );

  RenderShadeFrame( pt0 + halfw, pt1 + halfw, pt2 - halfw, pt3 - halfw, w, c0, c1 );
  RenderRect( pt0 + halfw, pt1 + halfw, pt2 - halfw, pt3 - halfw, c1, c1, c1, c1 );
  FOwner.Engine.DisplayDriver.SetViewport( Oldvp );
end;

procedure TRevCanvas.RenderShadeFrame( pt0, pt1, pt2, pt3, w: Single;
  c0, c1: DWord );
begin
  c0:= ColorMultiplyW( FOwner.FScreenColor, c0 );
  c1:= ColorMultiplyW( FOwner.FScreenColor, c1 );

  with ( FOwner ) do
    begin
      Engine.Painter.RenderQuad2DEx(
        Vector( pt0 + FScreenLeft - w, pt1 + FScreenTop - w, 0.0 ),
        Vector( pt0 + FScreenLeft - w, pt3 + FScreenTop + w, 0.0 ),
        Vector( pt0 + FScreenLeft, pt1 + FScreenTop, 0.0 ),
        Vector( pt0 + FScreenLeft, pt3 + FScreenTop, 0.0 ),
        c0, c0, c1, c1, 0, 0, 0, 0, nil );
      Engine.Painter.RenderQuad2DEx(
        Vector( pt0 + FScreenLeft - w, pt1 + FScreenTop - w, 0.0 ),
        Vector( pt0 + FScreenLeft, pt1 + FScreenTop, 0.0 ),
        Vector( pt2 + FScreenLeft + w, pt1 + FScreenTop - w, 0.0 ),
        Vector( pt2 + FScreenLeft, pt1 + FScreenTop, 0.0 ),
        c0, c1, c0, c1, 0, 0, 0, 0, nil );
      Engine.Painter.RenderQuad2DEx(
        Vector( pt0 + FScreenLeft, pt3 + FScreenTop, 0.0 ),
        Vector( pt0 + FScreenLeft - w, pt3 + FScreenTop + w, 0.0 ),
        Vector( pt2 + FScreenLeft, pt3 + FScreenTop, 0.0 ),
        Vector( pt2 + FScreenLeft + w, pt3 + FScreenTop + w, 0.0 ),
        c1, c0, c1, c0, 0, 0, 0, 0, nil );
      Engine.Painter.RenderQuad2DEx(
        Vector( pt2 + FScreenLeft, pt1 + FScreenTop, 0.0 ),
        Vector( pt2 + FScreenLeft, pt3 + FScreenTop, 0.0 ),
        Vector( pt2 + FScreenLeft + w, pt1 + FScreenTop - w, 0.0 ),
        Vector( pt2 + FScreenLeft + w, pt3 + FScreenTop + w, 0.0 ),
        c1, c1, c0, c0, 0, 0, 0, 0, nil );
    end;
end;

procedure TRevGraphicControl.UpdateClientRect;
begin
  FClientRect:= Rect( BoundLeft, BoundTop, Width - BoundRight - BoundLeft, Height - BoundBottom - BoundTop );
end;

procedure TRevGraphicControl.SetBound( const Index, Value: Integer );
begin
  case ( Index ) of
    0: FBoundLeft := Value;
    1: FBoundTop := Value;
    2: FBoundRight := Value;
    3: FBoundBottom := Value;
  end;
  Realign;
  Controls.Realign;
end;

{ TRevFocusControl }

procedure TRevFocusControl.SetFocus( const Value: Boolean );
begin
  if ( Manager.FFocus <> nil ) then
    if ( Manager.FFocus <> Self ) then
      Manager.FFocus.SetFocus( False );
  if ( Value ) then
    Manager.FFocus:= Self
  else
    Manager.FFocus:= nil;
end;

procedure TRevFocusControl.MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  inherited;
  if ( Enabled ) then
    HasFocus:= True;
end;

function TRevFocusControl.GetFocus: Boolean;
begin
  Result:= Manager.FFocus = Self;
end;

procedure TRevFocusControl.UpdateInput( OffSetX, OffSetY, _Width,
  _Height: Single );
begin
  inherited;
{  if (( Manager.GameInput.MBtnsChanged[ 0 ] and Manager.GameInput.MBtnsChanged[ 0 ]) or
      ( Manager.GameInput.MBtnsChanged[ 1 ] and Manager.GameInput.MBtnsChanged[ 1 ]) or
      ( Manager.GameInput.MBtnsChanged[ 2 ] and Manager.GameInput.MBtnsChanged[ 2 ])) then
    if (( FScreenLeft < Manager._c.x ) or
        ( FScreenLeft + FScreenWidth > Manager._c.x ) or
        ( FScreenTop < Manager._c.y ) or
        ( FScreenTop + FScreenHeight > Manager._c.y )) then
      HasFocus:= False;}
end;

procedure TRevFocusControl.KeyDown(Loop: Boolean);
begin
  if ( GetFocus and Assigned( FOnKeyDown )) then
    FOnKeyDown( Self );
  Controls.Input;
end;

{ TVCLManager }

constructor TVCLManager.Create( AOwner: TBaseObject; AEngine: TEngine; AInput: TInput; AFontManager: TFontManager );
begin
  inherited Create( AOwner, AEngine );
  FGameInput:= AInput;
  FFontManager:= AFontManager;
//  TBitmapFont.Create( Self, Engine, 'Ari, FontManager );
  FontManager.TextureBuffer.Add( '..\Textures\Scanlines.bmp' );
  DefaultFont:= FontManager[ FontManager.Add( '..\Fonts\Arial.fnt' )];
  Controls:= TControlBuffer.Create( Engine, nil, Self );
  LastKey:= 0;
  ZeroMemory( @TestKey, SizeOf( TestKey ));
end;

destructor TVCLManager.Destroy;
begin
  Controls.Clear( True );
  Controls.Free;
  inherited;
end;

function TVCLManager.GetMouseX: Integer;
var
  cpos: TPoint;
begin
  if ( Engine.Wnd <> nil ) then
    cpos:= Engine.Wnd.Cursor
  else
    begin
      GetCursorPos( cpos );
      ScreenToClient( Engine.DisplayDriver.CurSwapChain.Handle, cpos );
    end;
  Result:= cpos.x;
end;

function TVCLManager.GetMouseY: Integer;
var
  cpos: TPoint;
begin
  if ( Engine.Wnd <> nil ) then
    cpos:= Engine.Wnd.Cursor
  else
    begin
      GetCursorPos( cpos );
      ScreenToClient( Engine.DisplayDriver.CurSwapChain.Handle, cpos );
    end;
  Result:= cpos.y;
end;

procedure TVCLManager.Input;
var
  I: Integer;
  OldMOver: TRevGraphicControl;

begin
  OldMOver:= _CurMOver;

  for I:= 0 to Controls.Count - 1 do
    Controls[ I ].UpdateInput( 0, 0, Engine.DisplayDriver.CurSwapChain.Displ.XRes, Engine.DisplayDriver.CurSwapChain.Displ.YRes );

  if ( OldMOver <> _CurMOver ) then
    begin
      if ( OldMOver <> nil ) then
        begin
          OldMOver.WasOver:= False;
          OldMOver.MouseLeave( _c.x, _c.y );
        end;
      if ( _CurMOver <> nil ) then
        begin
          _CurMOver.WasOver:= True;
          _CurMOver.MouseEnter( _c.x, _c.y );
        end;
    end;

{  if ( FFocus <> nil ) then
    begin
      if ( not CompareMem( @TestKey, @GameInput.KeysChanged, SizeOf( TestKey ))) then
        begin
          LastKey:= GetTickCount + 500;
          FFocus.KeyDown( False );
        end
      else if ( LastKey <= GetTickCount ) then
        if ( not CompareMem( @TestKey, @GameInput.Keys, SizeOf( TestKey ))) then
          begin
            FFocus.KeyDown( True );
            LastKey:= GetTickCount + 100;
          end;
    end;}
  Controls.Input;

  if ( _CurInputCtrl <> nil ) then
    begin
      if ( _MUp ) then
        begin
          _CurInputCtrl.WasDown:= False;
          _CurInputCtrl.MouseUp( GameInput.MBtnsChanged[ 0 ],
            GameInput.MBtnsChanged[ 1 ], GameInput.MBtnsChanged[ 2 ], _c.x, _c.y );
        end
      else if ( _MDown ) then
        begin
          _CurInputCtrl.WasDown:= True;
          _CurInputCtrl.MouseDown( GameInput.MBtnsChanged[ 0 ],
            GameInput.MBtnsChanged[ 1 ], GameInput.MBtnsChanged[ 2 ], _c.x, _c.y );
        end;
      _MUp:= False;
      _MDown:= False;
      _CurInputCtrl:= nil;
    end
  else
    if ( _Activeform <> nil ) then
      if (( GameInput.MBtnsChanged[ 0 ] and GameInput.MBtns[ 0 ]) or
          ( GameInput.MBtnsChanged[ 1 ] and GameInput.MBtns[ 1 ]) or
          ( GameInput.MBtnsChanged[ 2 ] and GameInput.MBtns[ 2 ])) then
      TRevForm( _Activeform ).Active:= False;
end;

procedure TVCLManager.Render;
begin
  Controls.Render;
end;

procedure TVCLManager.SetMouseX( const Value: Integer );
var
  cpos: TPoint;
begin
  if ( Engine.Wnd <> nil ) then
    cpos:= Engine.Wnd.Cursor
  else
    GetCursorPos( cpos );

  ClientToScreen( Engine.DisplayDriver.CurSwapChain.Handle, cpos );
  cpos.Y:= Value;

  SetCursorPos( cpos.x, cpos.y );
end;

procedure TVCLManager.SetMouseY(const Value: Integer);
var
  cpos: TPoint;
begin
  if ( Engine.Wnd <> nil ) then
    cpos:= Engine.Wnd.Cursor
  else
    GetCursorPos( cpos );

  ClientToScreen( Engine.DisplayDriver.CurSwapChain.Handle, cpos );
  cpos.Y:= Value;

  SetCursorPos( cpos.x, cpos.y );
end;

{ TRevCanvas }

procedure TRevCanvas.Changed;
begin

end;

procedure TRevCanvas.Changing;
begin

end;

constructor TRevCanvas.Create( AOwner: TRevGraphicControl );
begin
  inherited Create;
  FFont:= TFont.Create;
  FOwner:= AOwner;
end;

destructor TRevCanvas.Destroy;
begin
  FOwner:= nil;
  FFont.Free;
  inherited;
end;

function TRevCanvas.GetClipRect: TRect;
begin
  Result:= Rect( FOwner.FScreenLeft, FOwner.FScreenTop, FOwner.FScreenLeft + FOwner.FScreenWidth, FOwner.FScreenTop + FOwner.FScreenHeight );
end;

{procedure TRevCanvas.SetFont( Value: TBitmapFont );
begin

end;}

function TRevCanvas.GetFontObject: TBitmapFont;
var
  n: Integer;
begin
  n:= FOwner.Manager.FontManager.GetFontByName( Font.Name );
  if ( n < 0 ) then
    Result:= FOwner.Manager.DefaultFont
  else
    Result:= FOwner.Manager.FontManager[ n ];
end;

function TRevCanvas.TextWidth(Text: String ): Integer;
var
  f: TBitmapFont;
begin
  f:= GetFontObject;
  Result:= f.TextWidth( Text, Font.Size );
end;

function TRevCanvas.TextWidthAdv(Text: String ): Integer;
var
  f: TBitmapFont;
begin
  f:= GetFontObject;
  Result:= f.TextWidthAdv( Text, Font.Size );
end;

initialization
  Rev_RegisterClasses([ TRevGraphicControl, TRevFocusControl, TVCLManager ]);

end.
