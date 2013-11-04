unit compo;

{$mode objfpc}{$H+}
{$DEFINE USE_ZENGL_STATIC}

interface

uses
  Classes, SysUtils, fgl, GL,

  {$IFDEF USE_ZENGL_STATIC}
  // RU: При использовании статической компиляции необходимо подключать модули ZenGL содержащие необходимый функционал.
  // EN: Using static compilation needs to use ZenGL units with needed functionality.
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_utils,
  zgl_mouse,
  zgl_font,
  zgl_text,
  zgl_primitives_2d,
  zgl_sprite_2d,
  zgl_textures
  {$ELSE}
  // RU: Используя ZenGL в качестве библиотеки(so, dll или dylib) нужен всего один заголовочный файл.
  // EN: Using ZenGL as a shared library(so, dll or dylib) needs only one header.
  zglHeader
  {$ENDIF}
  ;

type

  { TGraphicControl }

  TGraphicControl = class;

  TOnMouseEvent = procedure ( Sender: TGraphicControl; X,Y: Integer ) of object;
  TGraphicControl = class ( TPersistent )
    private
      FOnClick: TOnMouseEvent;
      FOnMove: TOnMouseEvent;
    public
      X,Y,W,H: Integer;
    published
      procedure Draw; virtual;
      function MouseMove( MX, MY: Integer ): Boolean; virtual;
      function MouseClick( MX, MY: Integer ): Boolean; virtual;

      property OnClick: TOnMouseEvent read FOnClick write FOnClick;
      property OnMove: TOnMouseEvent read FOnMove write FOnMove;
  end;

  { TButton }

  TButton = class ( TGraphicControl )
    public
      Caption: String;

      procedure Draw; override;
  end;

  { TImage }

  TImage = class ( TGraphicControl )
  private
    FImg: zglPTexture;
    FRotation: Real;
    procedure SetImg(AValue: zglPTexture);
    procedure SetRotation(AValue: Real);

  public
    property Img: zglPTexture read FImg write SetImg;

  published
    procedure Draw; override;
    property Rotation: Real read FRotation write SetRotation;
  end;

  { TSelection }

  TSelection = class ( TGraphicControl )
  private
    FObj: TImage;
    procedure SetObj(AValue: TImage);
  public
    procedure Draw; override;
    function MouseMove(MX, MY: Integer): Boolean; override;
  published
    property Obj: TImage read FObj write SetObj;
  end;

  TGraphicList = specialize TFPGList <TGraphicControl>;

  { TScreen }

  TScreen = class ( TGraphicControl )
  private
    FItems: TGraphicList;
    FSelection: TSelection;
  public
    procedure Draw; override;

    constructor Create;
    destructor Destroy; override;

    function MouseClick(MX, MY: Integer): Boolean; override;
    function MouseMove(MX, MY: Integer): Boolean; override;

  published
    property Items: TGraphicList read FItems write FItems;
    property Selection: TSelection read FSelection write FSelection;
  end;

  TListBox = class;
  TListBoxGetCountFunc = function( Sender: TListBox ): Integer of object;
  TListBoxGetItemFunc = function( Sender: TListBox; Index: Integer ): String of object;

  { TListBox }

  TListBox = class( TGraphicControl )
    private
      FCaption: String;
      FGetCount: TListBoxGetCountFunc;
      FGetItm: TListBoxGetItemFunc;
      FSelection: Integer;

    public
      procedure Draw; override;
      function MouseClick(MX, MY: Integer): Boolean; override;
      function MouseMove(MX, MY: Integer): Boolean; override;

      property GetCount: TListBoxGetCountFunc read FGetCount write FGetCount;
      property GetItem: TListBoxGetItemFunc read FGetItm write FGetItm;

    published
      property Caption: String read FCaption write FCaption;
      property Selection: Integer read FSelection write FSelection;
  end;

  procedure DrawList( List: TGraphicList );
  function MouseList( List: TGraphicList; const Click: Boolean = True; const Move: Boolean = True; const offx: Integer = 0; const offy: Integer = 0 ): Boolean;
  procedure FreeList( List: TGraphicList );
  function PtInRect( X, Y, RX, RY, RW, RH: Integer ): Boolean;

implementation

uses vars;

procedure DrawList(List: TGraphicList);
var
  i: Integer;
begin
  for i:= 0 to List.Count - 1 do
    List[ i ].Draw;
end;

function MouseList( List: TGraphicList; const Click: Boolean = True; const Move: Boolean = True; const offx: Integer = 0; const offy: Integer = 0 ): Boolean;
var
  i: Integer;
  stop: Boolean;
begin
  Result:= False;
  stop:= False;
  for i:= 0 to List.Count - 1 do
    if ( PtInRect( mouse_X() + offx, mouse_Y() + offy, List[ i ].X, List[ i ].Y, List[ i ].W, List[ i ].H )) then
      begin
        if ( Move ) then
          stop:= List[ i ].MouseMove( mouse_X() - List[ i ].X + offx, mouse_Y() - List[ i ].Y + offy );
        if ( Click AND mouse_Click( 0 )) then
          stop:= stop or List[ i ].MouseClick( mouse_X() - List[ i ].X + offx, mouse_Y() - List[ i ].Y + offy );
        if ( stop ) then
          break;
      end;
  Result:= stop;
end;


procedure FreeList(List: TGraphicList);
var
  i: Integer;
begin
  for i:= 0 to List.Count - 1 do
    List[ i ].Free;
  List.Free;
end;

function PtInRect(X, Y, RX, RY, RW, RH: Integer): Boolean;
begin
  Result:= (( X > RX ) AND ( X < RX + RW ) AND ( Y > RY ) AND ( Y < RY + RH ));
end;

{ TListBox }

procedure TListBox.Draw;
var
  n, i: Integer;
begin
  pr2d_Rect( X, Y, W, H, $336699, 255, PR2D_FILL );
//  pr2d_Rect( X + 5, W - 10, 30, H - 60, $447AB0, 255, PR2D_FILL );
//  text_DrawEx( fntMain, X + 8, Y + 6, 0.4, 0, Caption, 255, $FFFFFF );

  if ( Assigned( GetCount ) AND Assigned( GetItem )) then
    begin
      n:= GetCount( Self );
      for i:= 0 to n - 1 do
        begin
          if ( Selection = i ) then
            pr2d_Rect( X + 5, Y + ( i + 1 ) * 30, W - 10, 30, $A1B9D3, 255, PR2D_FILL );
          text_DrawEx( fntMain, X + 8, Y + 36 + i * 30, 0.4, 0, GetItem( Self, i ), 255, $FFFFFF );
        end;
    end;
end;

function TListBox.MouseClick(MX, MY: Integer): Boolean;
begin
  Result:= inherited MouseClick(MX, MY);
  Selection:= MY div 30 - 1;
end;

function TListBox.MouseMove(MX, MY: Integer): Boolean;
begin
  Result:= inherited MouseMove(MX, MY);
end;

{ TScreen }

procedure TScreen.Draw;
begin
  glMatrixMode( GL_PROJECTION_MATRIX );
  glPushMatrix;
  glTranslatef( -X, -Y, 0 );
  pr2d_Rect( X, Y, W, H, $FFFFFF, 255, PR2D_FILL );

  if ( Assigned( Items )) then
    DrawList( Items );

  Selection.Draw;

  glPopMatrix;
end;

constructor TScreen.Create;
begin
  inherited;
  FSelection:= TSelection.Create;
end;

destructor TScreen.Destroy;
begin
  FSelection.Free;
  inherited Destroy;
end;

function TScreen.MouseClick(MX, MY: Integer): Boolean;
begin
  if ( Assigned( FItems )) then
    Result:= MouseList( Items, True, False, x, y );
end;

function TScreen.MouseMove(MX, MY: Integer): Boolean;
begin
  if ( Assigned( FItems )) then
    Result:= MouseList( Items, False, True, x, y );
  if ({( not Result ) AND} ( mouse_Down( 1 ))) then
    begin
      X+= mouse_X() - vars.mx;
      Y+= mouse_Y() - vars.my;
    end;
end;

{ TSelection }

procedure TSelection.SetObj(AValue: TImage);
begin
  if FObj=AValue then Exit;
  FObj:=AValue;
  if ( Assigned( FObj )) then
    begin
      x:= FObj.x;
      y:= FObj.y;
      w:= FObj.w;
      h:= FObj.h;
    end;
end;

procedure TSelection.Draw;
begin
  if ( Assigned( FObj )) then
    pr2d_Rect( X, Y, W, H, $FF0000, 255 );
end;

function TSelection.MouseMove(MX, MY: Integer): Boolean;
begin
  Result:= False;
end;

{ TImage }

procedure TImage.SetRotation(AValue: Real);
begin
  if ( FRotation=AValue ) then Exit;
  FRotation:=AValue;
  W:= Round( cos( FRotation ) * Img^.Width );
  H:= Round( sin( FRotation ) * Img^.Height );
end;

procedure TImage.SetImg(AValue: zglPTexture);
begin
  if FImg=AValue then Exit;
  FImg:=AValue;
  if ( Assigned( FImg )) then
    begin
      W:= FImg^.Width;
      H:= FImg^.Height;
    end;
end;

procedure TImage.Draw;
begin
  if ( Assigned( Img )) then
    ssprite2d_Draw( Img, X, Y, Img^.Width, Img^.Height, Rotation );
end;

{ TButton }

procedure TButton.Draw;
begin
  inherited Draw;
  pr2d_Rect( X, Y, W, H, $336699, 255, PR2D_FILL );
  pr2d_Rect( X, Y, W, H, $447AB0, 255 );
  text_DrawEx( fntMain, X, Y, 0.4, 0, Caption );
end;

{ TGraphicControl }

procedure TGraphicControl.Draw;
begin
  //
end;

function TGraphicControl.MouseMove(MX, MY: Integer): Boolean;
begin
  if ( Assigned( FOnMove )) then
    FOnMove( Self, MX, MY );
  Result:= True;
end;

function TGraphicControl.MouseClick(MX, MY: Integer): Boolean;
begin
  writeln( 'Mouse Event: ', Self.ClassName );
  if ( Assigned( FOnClick )) then
    FOnClick( Self, MX, MY );
  Result:= True;
end;

end.

