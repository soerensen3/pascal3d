//p3dcanvas.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}
type

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

  { TP3DCanvas }

  TP3DCanvas = class ( TPersistent )
    private
      FFont: TP3DCanvasFont;
      FLockID: Integer;
      FLeft: Integer;
      FMaterial: TP3DMaterialBase;
      FOffScreenCanvas: Boolean;
      FOwner: TPersistent;
      FScreenColor: TVec4;
      FTop: Integer;
      FWidth: Integer;
      FHeight: Integer;

      procedure SetExtent(AIndex: Integer; AValue: Integer);
      procedure UpdateLockedArea(); virtual;
      procedure CheckDrawingAllowed;

    public
      function Lock(): Integer; virtual;
      procedure Unlock();
      constructor Create( AOwner: TPersistent );
      destructor Destroy; override;

      property Left: Integer index 0 read FLeft write SetExtent;
      property Top: Integer index 1 read FTop write SetExtent;
      property Width: Integer index 2 read FWidth write SetExtent;
      property Height: Integer index 3 read FHeight write SetExtent;
      property LockID: Integer read FLockID write FLockID;
      property Owner: TPersistent read FOwner write FOwner;
      property Font: TP3DCanvasFont read FFont write FFont; //TODO: OnChange event for font will fire redraw of texts in components
      property ScreenColor: TVec4 read FScreenColor write FScreenColor;
      property Material: TP3DMaterialBase read FMaterial;
      property OffScreenCanvas: Boolean read FOffScreenCanvas write FOffScreenCanvas;
  end;

  {$INCLUDE p3dcanvas_2d.inc}
  {$INCLUDE p3dcanvas_3d.inc}


{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure InitDefaultMat;
begin
  if ( not Assigned( P3DCanvasMaterialDefault )) then
    begin
      P3DCanvasMaterialDefault:= TP3DMaterialBase.Create();
      P3DCanvasMaterialDefault.Unlit:= True;
      P3DCanvasMaterialDefault.UseVertexColor:= True;
    end;
end;

{ TP3DPen }

procedure TP3DPen.SetWidth(AValue: Single);
begin
  if FWidth=AValue then Exit;
  FWidth:=AValue;
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

procedure TP3DPen.SetStyle(AValue: TP3DPenStyle);
begin
  if FStyle=AValue then Exit;
  FStyle:=AValue;
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

constructor TP3DPen.Create(AColor: TVec4; AWidth: Single; AStyle: TP3DPenStyle);
begin
  inherited Create;
  Color:= AColor;
  Width:= AWidth;
  Style:= AStyle;
end;

{ TP3DBrush }

constructor TP3DBrush.Create(AColor: TVec4);
begin
  inherited Create;
  Color:= AColor;
end;

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
  Size:= 18;
  Color:= vec4( 0, 0, 0, 1 );
  Name:= 'OpenSans-Regular';//'RobotoCondensed-Light';
end;


{ TP3DCanvas }

procedure TP3DCanvas.SetExtent(AIndex: Integer; AValue: Integer);
begin
  case ( AIndex ) of
    0: FLeft:= AValue;
    1: FTop:= AValue;
    2: FWidth:= Max( 1, AValue );
    3: FHeight:= Max( 1, AValue );
  end;
  if ( LockID >= 0 ) then
    UpdateLockedArea();
end;

procedure TP3DCanvas.UpdateLockedArea;
var
  VP: TP3DViewport;
begin
  if ( LockID < 0 ) then
    raise Exception.Create( 'TP3DCanvas.Unlock: Failed to update canvas. The canvas is not currently locked!' );
  if ( LockID = P3DViewports.Count - 1 ) then
    begin
      P3DViewports.Pop;
      LockID:= P3DViewports.Push( Left, P3DViewports[ 0 ].Height - Top - Height, Width, Height );
    end
  else
    begin
      VP.Left:= Left;
      VP.Top:= Top;
      VP.Width:= Width;
      VP.Height:= P3DViewports[ 0 ].Height - Top - Height;
      P3DViewports.VP[ LockID ]:= VP;
    end;
end;

procedure TP3DCanvas.CheckDrawingAllowed;
begin
  if ( LockID < 0 ) then
    raise Exception.Create( 'TP3DCanvas.CheckDrawingAllowed: The canvas has to be locked before drawing!' );
  if ( not Assigned( Material )) then
    raise Exception.Create( 'TP3DCanvas.CheckDrawingAllowed: The canvas material is currently unassigned!' );
end;

function TP3DCanvas.Lock: Integer;
begin
  if ( LockID >= 0 ) then
    raise Exception.Create( 'TP3DCanvas.Lock: Failed to lock canvas. The canvas is already locked!' );
  LockID:= P3DViewports.Push( Left, P3DViewports[ 0 ].Height - Top - Height, Width, Height );
  Result:= LockID;
end;

procedure TP3DCanvas.Unlock;
begin
  //P3DViewports.DebugShowVP( vec4( 1, 0, 0, 0.1 ));
  if ( LockID <> P3DViewports.Count - 1 ) then
    raise Exception.Create( 'TP3DCanvas.Unlock: Failed to unlock canvas. The canvas is not on top of the stack!' );
  P3DViewports.Pop();
  LockID:= -1;
end;

constructor TP3DCanvas.Create(AOwner: TPersistent);
begin
  inherited Create;
  FFont:= TP3DCanvasFont.Create;
  FWidth:= 1;
  FHeight:= 1;
  FLockID:= -1;
  FScreenColor:= vec4( 1 );
  FOwner:= AOwner;
  if ( not Assigned( P3DCanvasMaterialDefault )) then
    InitDefaultMat;
  FMaterial:= P3DCanvasMaterialDefault;
end;

destructor TP3DCanvas.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

{$INCLUDE p3dcanvas_2d.inc}
{$INCLUDE p3dcanvas_3d.inc}

{$ENDIF}

