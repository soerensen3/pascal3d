//p3dcanvas.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}
type

  { TP3DCanvasFont }

  TP3DCanvasFont = class ( TPersistent )
    private
      FFontColor: TP3DColorRGBA;
      FFontName: String;
      FFontSize: Integer;
      FOnChange: TNotifyEvent;

      procedure SetFontColor(AValue: TP3DColorRGBA);
      procedure SetFontName(AValue: String);
      procedure SetFontSize(AValue: Integer);

    public
      procedure Assign( Font: TP3DCanvasFont ); reintroduce;
      constructor Create;

      property Color: TP3DColorRGBA read FFontColor write SetFontColor; //Object properties can't be published for some reason

    published
      property Name: String read FFontName write SetFontName;
      property Size: Integer read FFontSize write SetFontSize;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TP3DBrush = class
    Color: TP3DColorRGBA;
    constructor Create(AColor: TP3DColorRGBA);
  end;

  TP3DPenStyle = ( psSolid, psStippled1, psStippled2, psStippled3, psStippled4, psStippled5, psStippled6 );

  TP3DHorizontalAlignment = (
    haLeft,
    haCenter,
    haRight);

  TP3DVerticalAlignment = (
    vaTop,
    vaCenter,
    vaBottom);


  { TP3DPen }

  TP3DPen = class
    private
      FColor: TP3DColorRGBA;
      FOnChange: TNotifyEvent;
      FWidth: Single;
      FStyle: TP3DPenStyle;

      procedure SetStyle(AValue: TP3DPenStyle);
      procedure SetWidth(AValue: Single);

    public
      constructor Create( AColor: TP3DColorRGBA; AWidth: Single; AStyle: TP3DPenStyle );

      property Width: Single read FWidth write SetWidth;
      property Color: TP3DColorRGBA read FColor write FColor;
      property Style: TP3DPenStyle read FStyle write SetStyle;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { TP3DCanvas }

  TP3DCanvas = class ( TPersistent )
  private
    FMaterialTextureManMip: TP3DMaterial;
    protected
      FFont: TP3DCanvasFont;
      FLockID: Integer;
      FMaterial: TP3DMaterial;
      FOffScreenCanvas: Boolean;
      FOwner: TObject;
      FRect: TP3DRect;
      FScreenColor: TVec4;
      FMaterialText: TP3DMaterial;
      FMaterialTexture: TP3DMaterial;

      function GetBottomRight: TVec2;
      function GetExtent(AIndex: Integer): Integer;
      function GetTopLeft: TVec2;
      function GetWidthHeight: TVec2;
      procedure SetBottomRight(AValue: TVec2);
      procedure SetExtent(AIndex: Integer; AValue: Integer);
      procedure SetTopLeft(AValue: TVec2);
      procedure SetWidthHeight(AValue: TVec2);
      procedure UpdateLockedArea(); virtual;
      procedure CheckDrawingAllowed;

    public
      function Lock(): Integer; virtual;
      procedure Unlock(); virtual;
      constructor Create( AOwner: TObject );
      destructor Destroy; override;

      property Rect: TP3DRect read FRect write FRect;
      property Left: Integer index 0 read GetExtent write SetExtent;
      property Top: Integer index 1 read GetExtent write SetExtent;
      property Width: Integer index 2 read GetExtent write SetExtent;
      property Height: Integer index 3 read GetExtent write SetExtent;
      property TopLeft: TVec2 read GetTopLeft write SetTopLeft;
      property BottomRight: TVec2 read GetBottomRight write SetBottomRight;
      property WidthHeight: TVec2 read GetWidthHeight write SetWidthHeight;
      property LockID: Integer read FLockID write FLockID;
      property Owner: TObject read FOwner write FOwner;
      property Font: TP3DCanvasFont read FFont write FFont; //TODO: OnChange event for font should fire redraw of texts in components
      property ScreenColor: TVec4 read FScreenColor write FScreenColor;
      property Material: TP3DMaterial read FMaterial write FMaterial;
      property MaterialText: TP3DMaterial read FMaterialText write FMaterialText;
      property MaterialTexture: TP3DMaterial read FMaterialTexture write FMaterialTexture;
      property MaterialTextureManMip: TP3DMaterial read FMaterialTextureManMip write FMaterialTextureManMip;
      property OffScreenCanvas: Boolean read FOffScreenCanvas write FOffScreenCanvas;
  end;

  {$INCLUDE p3d.core.canvas_2d.inc}
  {$INCLUDE p3d.core.canvas_3d.inc}


{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure InitDefaultMat;
  function CreateCanvasMaterial: TP3DMaterialNode;
  var
    ClipRect: TP3DNodeClone;
  begin
    Result:= TP3DMaterialNode.CreateBlock();
    Result.Name:= 'P3DCanvasMaterialDefault';
    Result.MakeRootNode( 'output_canvas' );
    with ( Result.RootNode.AsJSON ) do begin
      WriteLn( AsJSON );
      Free;
    end;
    ClipRect:= Result.Uniforms.AddCopy( 'uniform_vec4' );
    ClipRect.FindInput( 'UnName' ).ValueProp.AsString:= 'ClippingRect';
    ClipRect.FindOutput( 'Vector' ).Connected:= Result.RootNode.FindInput( 'ClipRect' );
    Result.GeometryNode.FindOutput( 'Color' ).Connected:= Result.RootNode.FindInput( 'Color' );
  end;
begin
  if ( not Assigned( P3DCanvasMaterialDefault )) then
    P3DCanvasMaterialDefault:= CreateCanvasMaterial;
     {begin
      P3DCanvasMaterialDefault:= TP3DMaterialBase.CreateBlock();
      P3DCanvasMaterialDefault.Name:= 'CanvasDefaultMat';
      P3DCanvasMaterialDefault.Unlit:= True;
      P3DCanvasMaterialDefault.UseVertexColor:= True;
    end;}
end;

procedure InitTextMat;
  function CreateCanvasMaterial: TP3DMaterialNode;
  var
    ClipRect, SDF, TexCoords: TP3DNodeClone;
  begin
    Result:= TP3DMaterialNode.CreateBlock();
    Result.Name:= 'P3DCanvasMaterialText';
    Result.MakeRootNode( 'output_canvas' );
    ClipRect:= Result.Uniforms.AddCopy( 'uniform_vec4' );
    ClipRect.FindInput( 'UnName' ).ValueProp.AsString:= 'ClippingRect';
    ClipRect.FindOutput( 'Vector' ).Connected:= Result.RootNode.FindInput( 'ClipRect' );
    Result.Maps.AddCopy( 'texture2D' ); //TP3DMaterialMap.Create( Result.Maps );
    TexCoords:= Result.NodeTree.AddCopy( 'texcoord' );
    SDF:= Result.NodeTree.AddCopy( 'signed_distance_field_compatibility' );
    SDF.FindInput( 'texture' ).Connected:= Result.Maps[ 0 ].FindOutput( 'Texture' );
    SDF.FindInput( 'uv' ).Connected:= TexCoords.FindOutput( 'UV' );
    SDF.FindInput( 'color' ).Connected:= Result.GeometryNode.FindOutput( 'Color' );
    SDF.FindOutput( 'Color' ).Connected:= Result.RootNode.FindInput( 'Color' );
  end;
begin
  if ( not Assigned( P3DCanvasMaterialText )) then
    P3DCanvasMaterialText:= CreateCanvasMaterial;
end;

procedure InitTextureMat;
  function CreateCanvasMaterial: TP3DMaterialNode;
  var
    ClipRect, TexCoords, _mix: TP3DNodeClone;
  begin
    Result:= TP3DMaterialNode.CreateBlock();
    Result.Name:= 'P3DCanvasMaterialTexture';
    Result.MakeRootNode( 'output_canvas' );
    ClipRect:= Result.Uniforms.AddCopy( 'uniform_vec4' );
    ClipRect.FindInput( 'UnName' ).ValueProp.AsString:= 'ClippingRect';
    ClipRect.FindOutput( 'Vector' ).Connected:= Result.RootNode.FindInput( 'ClipRect' );
    Result.Maps.AddCopy( 'texture2D' );
    //TP3DMaterialMap.Create( Result.Maps );
    TexCoords:= Result.NodeTree.AddCopy( 'texcoord' );
    _mix:= Result.NodeTree.AddCopy( 'mix_simple' );
    _mix.FindInput( 'Color1' ).Connected:= Result.Maps[ 0 ].FindOutput( 'Color' );
    _mix.FindInput( 'Color2' ).Connected:= Result.GeometryNode.FindOutput( 'Color' );
    Result.Maps[ 0 ].FindInput( 'UV' ).Connected:= TexCoords.FindOutput( 'UV' );

    Result.RootNode.FindInput( 'Color' ).Connected:= _mix.FindOutput( 'mul' );
  end;
begin
  if ( not Assigned( P3DCanvasMaterialTexture )) then
    P3DCanvasMaterialTexture:= CreateCanvasMaterial;
end;


procedure InitTextureManualMipMat;
  function CreateCanvasMaterial: TP3DMaterialNode;
  var
    ClipRect, TexCoords, _mix, uniform: TP3DNodeClone;
  begin
    Result:= TP3DMaterialNode.CreateBlock();
    Result.Name:= 'P3DCanvasMaterialManMipTexture';
    Result.MakeRootNode( 'output_canvas' );
    ClipRect:= Result.Uniforms.AddCopy( 'uniform_vec4' );
    ClipRect.FindInput( 'UnName' ).ValueProp.AsString:= 'ClippingRect';
    ClipRect.FindOutput( 'Vector' ).Connected:= Result.RootNode.FindInput( 'ClipRect' );
    Result.Maps.AddCopy( 'texture2DLod' );
    uniform:= Result.Uniforms.AddCopy( 'uniform_float' );
    uniform.FindInput( 'UnName' ).ValueProp.AsString:= 'MipLevel';
    Result.Maps[ 0 ].FindInput( 'Level' ).Connected:= uniform.FindOutput( 'Value' );
    //TP3DMaterialMap.Create( Result.Maps );
    TexCoords:= Result.NodeTree.AddCopy( 'texcoord' );
    _mix:= Result.NodeTree.AddCopy( 'mix_simple' );
    _mix.FindInput( 'Color1' ).Connected:= Result.Maps[ 0 ].FindOutput( 'Color' );
    _mix.FindInput( 'Color2' ).Connected:= Result.GeometryNode.FindOutput( 'Color' );
    Result.Maps[ 0 ].FindInput( 'UV' ).Connected:= TexCoords.FindOutput( 'UV' );

    Result.RootNode.FindInput( 'Color' ).Connected:= _mix.FindOutput( 'mul' );
  end;
begin
  if ( not Assigned( P3DCanvasMaterialManMipTexture )) then
    P3DCanvasMaterialManMipTexture:= CreateCanvasMaterial;
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

constructor TP3DPen.Create(AColor: TP3DColorRGBA; AWidth: Single; AStyle: TP3DPenStyle);
begin
  inherited Create;
  Color:= AColor;
  Width:= AWidth;
  Style:= AStyle;
end;

{ TP3DBrush }

constructor TP3DBrush.Create(AColor: TP3DColorRGBA);
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

procedure TP3DCanvasFont.SetFontColor(AValue: TP3DColorRGBA);
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
  Name:= 'Cabin-Regular';//'OpenSans-Regular';//}'DejaVuSans';//'RobotoCondensed-Light';
end;


{ TP3DCanvas }

procedure TP3DCanvas.SetExtent(AIndex: Integer; AValue: Integer);
begin
  case ( AIndex ) of
    0: Rect.Left:= AValue;
    1: Rect.Top:= AValue;
    2: Rect.Width:= Max( 1, AValue );
    3: Rect.Height:= Max( 1, AValue );
  end;
  if ( LockID >= 0 ) then
    UpdateLockedArea();
end;

function TP3DCanvas.GetBottomRight: TVec2;
begin
  Result:= Rect.BottomRight;
end;

function TP3DCanvas.GetExtent(AIndex: Integer): Integer;
begin
  case ( AIndex ) of
    0: Result:= Rect.LeftI;
    1: Result:= Rect.TopI;
    2: Result:= Rect.WidthI;
    3: Result:= Rect.HeightI;
  end;
end;

function TP3DCanvas.GetTopLeft: TVec2;
begin
  Result:= Rect.TopLeft;
end;

function TP3DCanvas.GetWidthHeight: TVec2;
begin
  Result:= Rect.WidthHeight;
end;

procedure TP3DCanvas.SetBottomRight(AValue: TVec2);
begin
  Rect.BottomRight:= AValue;
  if ( LockID >= 0 ) then
    UpdateLockedArea();
end;

procedure TP3DCanvas.SetTopLeft(AValue: TVec2);
begin
  Rect.TopLeft:= AValue;
  if ( LockID >= 0 ) then
    UpdateLockedArea();
end;

procedure TP3DCanvas.SetWidthHeight(AValue: TVec2);
begin
  Rect.WidthHeight:= AValue;
  if ( LockID >= 0 ) then
    UpdateLockedArea();
end;

procedure TP3DCanvas.UpdateLockedArea();
var
  VP: TP3DViewport;
begin
  if ( LockID < 0 ) then
    raise Exception.Create( 'TP3DCanvas.UpdateLockedArea: Failed to update canvas. The canvas is not currently locked!' );
  if ( LockID = P3DViewports.Count - 1 ) then
    P3DViewports[ LockID ]:= P3DViewport( Left, P3DViewports.Screen.Height - Top - Height, Width, Height )
  else
    begin
      VP.Left:= Left;
      VP.Top:= Top;
      VP.Width:= Width;
      VP.Height:= P3DViewports.Screen.Height - Top - Height;
      P3DViewports.VP[ LockID ]:= VP;
    end;
end;

procedure TP3DCanvas.CheckDrawingAllowed;
begin
  //if ( LockID < 0 ) then
  //  raise Exception.Create( 'TP3DCanvas.CheckDrawingAllowed: The canvas has to be locked before drawing!' );
  if ( not Assigned( Material )) then
    raise Exception.Create( 'TP3DCanvas.CheckDrawingAllowed: The canvas material is currently unassigned!' );
end;

function TP3DCanvas.Lock(): Integer;
begin
  if ( LockID >= 0 ) then
    raise Exception.Create( 'TP3DCanvas.Lock: Failed to lock canvas. The canvas is already locked!' );
  LockID:= P3DViewports.Push( Left, P3DViewports.Screen.Height - Top - Height, Width, Height );
  Result:= LockID;
end;

procedure TP3DCanvas.Unlock();
begin
  //P3DViewports.DebugShowVP( vec4( 1, 0, 0, 0.1 ));
  if ( LockID <> P3DViewports.Count - 1 ) then
    raise Exception.Create( 'TP3DCanvas.Unlock: Failed to unlock canvas. The canvas is not on top of the stack!' );
  P3DViewports.Pop();
  LockID:= -1;
end;

constructor TP3DCanvas.Create(AOwner: TObject);
begin
  inherited Create;
  FFont:= TP3DCanvasFont.Create;
  Rect.Width:= 1;
  Rect.Height:= 1;
  FLockID:= -1;
  FScreenColor:= vec4( 1 );
  FOwner:= AOwner;
  InitDefaultMat;
  InitTextMat;
  InitTextureMat;
  InitTextureManualMipMat;
  FMaterial:= P3DCanvasMaterialDefault;
  FMaterialText:= P3DCanvasMaterialText;
  FMaterialTexture:= P3DCanvasMaterialTexture;
  FMaterialTextureManMip:= P3DCanvasMaterialManMipTexture;
end;

destructor TP3DCanvas.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

{$INCLUDE p3d.core.canvas_2d.inc}
{$INCLUDE p3d.core.canvas_3d.inc}

{$ENDIF}

