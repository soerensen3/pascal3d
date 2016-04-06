{$IFDEF INTERFACE}

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

{$ENDIF}
{$IFDEF IMPLEMENTATION}

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
  RenderLine2D( vec2( pt0.x { + FOwner.FScreenLeft}, pt0.y {+ FOwner.FScreenTop} ),
    vec2( pt1.x {+ FOwner.FScreenLeft}, pt1.y {+ FOwner.FScreenTop} ), c0, c1, 1.0 );
end;

procedure TP3DCanvas.RenderLineRect(pt0, pt1: TVec2; c: TVec4);
begin
  c:= FOwner.FScreenColor * c;
  RenderLineRect2D( vec2( pt0.x {+ FOwner.FScreenLeft}, pt0.y {+ FOwner.FScreenTop} ),
    vec2( pt1.x {+ FOwner.FScreenLeft}, pt1.y {+ FOwner.FScreenTop} ), c, 1.0 );
end;

procedure TP3DCanvas.RenderLine(pt0, pt1: TVec2; c: TVec4);
begin
  c:= FOwner.FScreenColor * c;
  RenderLine2D( vec2( pt0.x {+ FOwner.FScreenLeft}, pt0.y {+ FOwner.FScreenTop} ),
    vec2( pt1.x {+ FOwner.FScreenLeft}, pt1.y {+ FOwner.FScreenTop} ), c, 1.0 );
end;

procedure TP3DCanvas.RenderRect(pt0, pt1: TVec2; c: TVec4);
begin
  c:= FOwner.FScreenColor * c;
  RenderRect2D( vec2( pt0.x {+ FOwner.FScreenLeft}, pt0.y {+ FOwner.FScreenTop} ),
                vec2( pt1.x {+ FOwner.FScreenLeft}, pt1.y {+ FOwner.FScreenTop} ), c );
end;

procedure TP3DCanvas.RenderLineRect(pt0, pt1: TVec2; c0, c1, c2, c3: TVec4);
begin
  c0:= FOwner.FScreenColor * c0;
  c1:= FOwner.FScreenColor * c1;
  c2:= FOwner.FScreenColor * c2;
  c3:= FOwner.FScreenColor * c3;
  RenderLineRect2D( vec2( pt0.x {+ FOwner.FScreenLeft}, pt0.y {+ FOwner.FScreenTop} ),
    vec2( pt1.x {+ FOwner.FScreenLeft}, pt1.y {+ FOwner.FScreenTop} ), c0, c1, c2, c3, 1.0 );
end;

procedure TP3DCanvas.RenderRect(pt0, pt1: TVec2; c0, c1, c2, c3: TVec4);
begin
  c0:= FOwner.FScreenColor * c0;
  c1:= FOwner.FScreenColor * c1;
  c2:= FOwner.FScreenColor * c2;
  c3:= FOwner.FScreenColor * c3;
  RenderRect2D( vec2( pt0.x {+ FOwner.FScreenLeft}, pt0.y {+ FOwner.FScreenTop} ),
                vec2( pt1.x {+ FOwner.FScreenLeft}, pt1.y {+ FOwner.FScreenTop} ), c0, c1, c2, c3 );
end;

procedure TP3DCanvas.RenderText( Text: TP3DText; P: TVec2 );
begin
  P.x:= P.x {+ FOwner.FScreenLeft};
  P.y:= P.y {+ FOwner.FScreenTop};

  Text.Render( P, FOwner.FScreenColor * Font.Color );
end;

procedure TP3DCanvas.RenderTextAdv( Text: String; P: TPoint );
//var
//  f: TBitmapFont;
begin
{  P.x:= P.x + {FOwner.FScreenLeft};
  P.y:= P.y + {FOwner.FScreenTop};
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

  vp.X:= Round( pt0 - halfw + {FOwner.FScreenLeft} );
  vp.Y:= Round( pt1 - halfw + {FOwner.FScreenTop} );
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

  //with ( FOwner ) do
    begin
      RenderQuad2D(
        vec2( pt0.x {+ FScreenLeft} - w, pt0.y {+ FScreenTop} - w ),
        vec2( pt0.x {+ FScreenLeft} - w, pt1.y {+ FScreenTop} + w ),
        vec2( pt0.x {+ FScreenLeft}, pt0.y {+ FScreenTop} ),
        vec2( pt0.x {+ FScreenLeft}, pt1.y {+ FScreenTop} ),
        c0, c0, c1, c1 );
      RenderQuad2D(
        vec2( pt0.x {+ FScreenLeft} - w, pt0.y {+ FScreenTop} - w ),
        vec2( pt0.x {+ FScreenLeft}, pt0.y {+ FScreenTop} ),
        vec2( pt1.x {+ FScreenLeft} + w, pt0.y {+ FScreenTop} - w ),
        vec2( pt1.x {+ FScreenLeft}, pt0.y {+ FScreenTop} ),
        c0, c1, c0, c1 );
      RenderQuad2D(
        vec2( pt0.x {+ FScreenLeft}, pt1.y {+ FScreenTop} ),
        vec2( pt0.x {+ FScreenLeft} - w, pt1.y {+ FScreenTop} + w ),
        vec2( pt1.x {+ FScreenLeft}, pt1.y {+ FScreenTop} ),
        vec2( pt1.x {+ FScreenLeft} + w, pt1.y {+ FScreenTop} + w ),
        c1, c0, c1, c0 );
      RenderQuad2D(
        vec2( pt1.x {+ FScreenLeft}, pt0.y {+ FScreenTop} ),
        vec2( pt1.x {+ FScreenLeft}, pt1.y {+ FScreenTop} ),
        vec2( pt1.x {+ FScreenLeft} + w, pt0.y {+ FScreenTop} - w ),
        vec2( pt1.x {+ FScreenLeft} + w, pt1.y {+ FScreenTop} + w ),
        c1, c1, c0, c0 );
    end;
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
  Result:= Rect( 0, 0, FOwner.FScreenWidth, FOwner.FScreenHeight );
  //Result:= Rect( FOwner.FScreenLeft, FOwner.FScreenTop, FOwner.FScreenLeft + FOwner.FScreenWidth, FOwner.FScreenTop + FOwner.FScreenHeight );
end;

{$ENDIF}
