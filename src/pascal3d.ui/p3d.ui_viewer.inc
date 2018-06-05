{$IFDEF INTERFACE}
type

{ TP3DUIViewerBase }
TP3DUIViewerBase = class ( TP3DStreamable )
  private
    FAutoViewRect: Boolean;
    FCanvas: TP3DCanvas2D;
    FControls: TP3DPropertyPointer;
    FPosition: TVec2;
    FScrollSpeed: Single;
    FViewRect: TP3DRect;
    FZoom: Single;

    {$IFDEF DEBUG_GUI}
    DebugText: TP3DTextBmp;
    DebugTextPos: TVec2;
    {$ENDIF}

    function GetControls: TP3DGraphicControlContainerList;
    function GetMouse: TVec2;
    function GetMouseD: TVec2;
    function GetMouseDX: Float;
    function GetMouseDY: Float;
    function GetMouseX: Float;
    function GetMouseY: Float;
    procedure SetAutoViewRect(AValue: Boolean);
    procedure SetControls(AValue: TP3DGraphicControlContainerList);
    procedure SetMouse(AValue: TVec2);
    procedure SetMouseX( const Value: Float );
    procedure SetMouseY( const Value: Float );
    procedure CtrlDeleteNotifier( Ctrl: TP3DGraphicControl );
    procedure SetPosition(AValue: TVec2);
    procedure SetZoom(AValue: Single);

  public
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
    destructor Destroy; override;

    procedure PrepareRender;
    procedure Activate;
    procedure Deactivate;
    procedure UpdateExtents; virtual;
    function TransformCoord( P: TVec2 ): TVec2; inline;

    property Mouse: TVec2 read GetMouse write SetMouse;
    property MouseD: TVec2 read GetMouseD;
    property Zoom: Single read FZoom write SetZoom;
    property Position: TVec2 read FPosition write SetPosition;
    property ViewRect: TP3DRect read FViewRect write FViewRect;

  published
    property MouseX: Float read GetMouseX write SetMouseX;
    property MouseY: Float read GetMouseY write SetMouseY;
    property MouseDX: Float read GetMouseDX;
    property MouseDY: Float read GetMouseDY;
    property ScrollSpeed: Single read FScrollSpeed write FScrollSpeed;
    property ScreenCanvas: TP3DCanvas2D read FCanvas write FCanvas;

    property AutoViewRect: Boolean read FAutoViewRect write SetAutoViewRect;
    property Controls: TP3DGraphicControlContainerList read GetControls write SetControls;
end;

TP3DUIViewer = class ( TP3DUIViewerBase )
  protected
    FViewerControls: TP3DGraphicControlContainerList;

  public
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

    procedure Render;
    procedure Input;
    procedure UpdateExtents; override;
end;

{ TP3DUIViewerBaseStack }

TP3DUIViewerStack = class( specialize gP3DStreamablePointerList < TP3DUIViewerBase >)
  public
    function Push( Viewer: TP3DUIViewerBase ): Integer;
    function Pop: TP3DUIViewerBase;
    function Peek: TP3DUIViewerBase;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DUIViewerBase }

constructor TP3DUIViewerBase.Create(const AOwner: IP3DStreamableContainer);
var
  i: Integer;
begin
  inherited Create( AOwner );
  FControls:= TP3DPropertyPointer.Create;

  ScrollSpeed:= -50;

  ScreenCanvas:= TP3DCanvas2D.Create( nil );
  ScreenCanvas.Font.Size:= 48;
  UpdateExtents;
  Zoom:= 1.0;
end;

destructor TP3DUIViewerBase.Destroy;
begin
  FControls.Free;
  ScreenCanvas.Free;
  inherited;
end;

procedure TP3DUIViewerBase.PrepareRender;
begin
  glDisable( GL_DEPTH_TEST ); cle( Self );
  //P3DViewports.Push( Controls.ClipRect );
  {if (( ScreenCanvas.Width <> P3DViewports.Screen.Width ) or
      ( ScreenCanvas.Height <> P3DViewports.Screen.Height )) then}
  //if (( ScreenCanvas.Width <> Controls.ClipRect.WidthI ) or
  //   ( ScreenCanvas.Height <> Controls.ClipRect.HeightI )) then
  UpdateExtents;
  //P3DViewports.Push( ScreenCanvas.Left, P3DViewports.Screen.Height - ScreenCanvas.Top - ScreenCanvas.Height, ScreenCanvas.Width, ScreenCanvas.Height );
  P3DViewports.Push( ViewRect.LeftI, P3DViewports.Screen.Height - ViewRect.TopI - ViewRect.HeightI, ViewRect.WidthI, ViewRect.HeightI );
end;

procedure TP3DUIViewerBase.Activate;
begin
  P3DUI.ViewerStack.Push( Self );
end;

procedure TP3DUIViewerBase.Deactivate;
begin
  if ( P3DUI.ViewerStack.Peek = Self ) then
    P3DUI.ViewerStack.Pop;
end;

procedure TP3DUIViewerBase.SetZoom(AValue: Single);
begin
  if FZoom=AValue then Exit;
  FZoom:=AValue;
  ScreenCanvas.Zoom:= Zoom;
end;

function TP3DUIViewerBase.GetMouseX: Float;
begin
  Result:= Mouse.X;
end;

function TP3DUIViewerBase.GetMouse: TVec2;
begin
  Result:= ( 1 / Zoom ) * ( vec2( P3DInput.Mouse.X, P3DInput.Mouse.Y ) + Position - ViewRect.TopLeft );
end;

function TP3DUIViewerBase.GetControls: TP3DGraphicControlContainerList;
begin
  Result:= TP3DGraphicControlContainerList( FControls.Prop );
end;

function TP3DUIViewerBase.GetMouseD: TVec2;
begin
  Result:= ( 1 / Zoom ) * vec2( P3DInput.Mouse.DX, P3DInput.Mouse.DY );
end;

function TP3DUIViewerBase.GetMouseDX: Float;
begin
  Result:= MouseD.X;
end;

function TP3DUIViewerBase.GetMouseDY: Float;
begin
  Result:= MouseD.Y;
end;

function TP3DUIViewerBase.GetMouseY: Float;
begin
  Result:= Mouse.Y;
end;

procedure TP3DUIViewerBase.SetAutoViewRect(AValue: Boolean);
begin
  if FAutoViewRect=AValue then Exit;
  FAutoViewRect:=AValue;
  UpdateExtents;
end;

procedure TP3DUIViewerBase.SetControls(AValue: TP3DGraphicControlContainerList);
begin
  if FControls.Prop=AValue then Exit;
  FControls.Prop:= AValue;
  if ( Assigned( Controls )) then
    Controls.FUIViewer.AsValue:= Self;
end;

procedure TP3DUIViewerBase.SetMouse(AValue: TVec2);
begin

end;

procedure TP3DUIViewerBase.SetMouseX(const Value: Float);
begin
  P3DInput.Mouse.X:= P3DApplication.ActiveWindow.Left + round( Value );
end;

procedure TP3DUIViewerBase.SetMouseY(const Value: Float);
begin
  P3DInput.Mouse.Y:= P3DApplication.ActiveWindow.Top + round( Value )
end;

procedure TP3DUIViewerBase.CtrlDeleteNotifier(Ctrl: TP3DGraphicControl);
begin

end;

procedure TP3DUIViewerBase.SetPosition(AValue: TVec2);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  ScreenCanvas.Scroll:= -Position;
end;

procedure TP3DUIViewerBase.UpdateExtents;
begin
  if ( not Assigned( P3DViewports )) then
    exit;

  {ScreenCanvas.Left:= P3DViewports.Screen.Left;
  ScreenCanvas.Top:= P3DViewports.Screen.Top;
  ScreenCanvas.Width:= P3DViewports.Screen.Width;
  ScreenCanvas.Height:= P3DViewports.Screen.Height;}
  if ( AutoViewRect ) then
    ViewRect:= P3DRect( 0, 0, P3DViewports.Peek.Width, P3DViewports.Peek.Height );
  ScreenCanvas.TopLeft:= vec2( 0 );
  ScreenCanvas.WidthHeight:= ViewRect.WidthHeight;
end;

function TP3DUIViewerBase.TransformCoord(P: TVec2): TVec2;
begin
  Result:= ( 1 / Zoom ) * ( P + Position - ViewRect.TopLeft );
end;

{ TP3DUIViewerBaseStack }

function TP3DUIViewerStack.Push(Viewer: TP3DUIViewerBase): Integer;
begin
  Result:= Add( Viewer );
  P3DUIViewerActive:= Viewer;
end;

function TP3DUIViewerStack.Pop: TP3DUIViewerBase;
var
  _Last: Integer;
begin
  _Last:= Count - 1;
  if ( _Last >= 0 ) then
    begin
      Result:= AsValues[ _Last ];
      Delete( _Last );
    end
  else
    Result:= nil;
  if ( _Last - 1 >= 0 ) then
    P3DUIViewerActive:= AsValues[ _Last - 1 ]
  else
    P3DUIViewerActive:= P3DUI.Viewer;
end;

function TP3DUIViewerStack.Peek: TP3DUIViewerBase;
var
  _Last: Integer;
begin
  _Last:= Count - 1;
  if ( _Last >= 0 ) then
    Result:= AsValues[ _Last ]
  else
    Result:= P3DUI.Viewer;
end;

{ TP3DUIViewer }

constructor TP3DUIViewer.Create(const AOwner: IP3DStreamableContainer);
begin
  FViewerControls:= TP3DGraphicControlContainerList.Create( 'Controls' );
  inherited Create(AOwner);
  Controls:= FViewerControls;
  Properties.Add( FViewerControls );
end;

procedure TP3DUIViewer.Input;
begin
  Activate;
  Controls.Input;
  Controls.Logic;
  Deactivate;
end;

procedure TP3DUIViewer.UpdateExtents;
begin
  inherited UpdateExtents;
  if ( Assigned( Controls )) then begin
    Controls.ClipRect:= ScreenCanvas.Rect - Position;
    Controls.PageRect:= P3DRect( vec2( 0 ), ScreenCanvas.Rect.WidthHeight );
  end;
end;

procedure TP3DUIViewer.Render;
var
  DrawCursor: TP3DTextBmp;
begin
  Activate;

  PrepareRender;

  Controls.Render();

  {$IFDEF DEBUG_GUI}
  if ( Assigned( DebugText )) then
    begin
      ScreenCanvas.Lock;
      ScreenCanvas.RenderRect( DebugTextPos, DebugTextPos + vec2( DebugText.Width, DebugText.Height ), vec4( 1, 1, 1, 0.5 ));
      ScreenCanvas.RenderText( DebugText, DebugTextPos );
      ScreenCanvas.Unlock();
    end;

  {$ENDIF}

  P3DViewports.Pop;
  Deactivate;
end;


{$ENDIF}
