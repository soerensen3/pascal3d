unit p3dcanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  p3dMath,
  p3dviewport,
  p3dshaders,
  p3dbmpfont,
  math,
  dglOpenGL;

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
      FOwner: TPersistent;
      FScreenColor: TVec4;
      FTop: Integer;
      FWidth: Integer;
      FHeight: Integer;

      procedure SetExtent(AIndex: Integer; AValue: Integer);
      procedure UpdateLockedArea(); virtual;

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
      property Font: TP3DCanvasFont read FFont write FFont;
      property ScreenColor: TVec4 read FScreenColor write FScreenColor;
  end;

  {$DEFINE INTERFACE}
  {$INCLUDE p3dcanvas_2d.inc}
  {$UNDEF INTERFACE}

implementation

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
{$INCLUDE p3dcanvas_2d.inc}
{$UNDEF IMPLEMENTATION}


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
      P3DViewports.Push( Left, Top, Width, Height );
    end
  else
    begin
      VP.Left:= Left;
      VP.Top:= Top;
      VP.Width:= Width;
      VP.Height:= Height;
      P3DViewports.VP[ LockID ]:= VP;
    end;
end;

function TP3DCanvas.Lock: Integer;
begin
  if ( LockID >= 0 ) then
    raise Exception.Create( 'TP3DCanvas.Lock: Failed to lock canvas. The canvas is already locked!' );
  LockID:= P3DViewports.Push( Left, Top, Width, Height );
end;

procedure TP3DCanvas.Unlock;
begin
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
end;

destructor TP3DCanvas.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

end.

