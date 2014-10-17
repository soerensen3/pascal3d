unit RForms;

interface
  uses
    Windows,
    Math,
    RVCL,
    RButtons,
    RUtils,
    RTypes,
    Types;
  type
    TRevForm = class( TRevGraphicControl )
      private
        FTitleBarSize: Integer;
        CaptionBar: TRevGraphicControl;
        FGradColor1,
        FGradColor2,
        FGradColor1Act,
        FGradColor2Act: Cardinal;
        FFontColorAct: Cardinal;
        FFontColor: Cardinal;
        FOldPos: TPoint;
        FWasResizing: Boolean;
        FResMode: Integer;
        FCaption: String;
        FCloseBtn: TRevButton;
        FMaxBtn: TRevButton;
        FShowTitle: Boolean;
        FOldWidth: Integer;
        FOldHeight: Integer;
        FOldLeft: Integer;
        FOldTop: Integer;

        function GetAlpha: Byte;
        procedure SetAlpha( const Value: Byte );
        procedure SetTitleBarSize(const Value: Integer);
        procedure _Draw( Sender: TRevGraphicControl; OffSetX, OffSetY, _Width, _Height: Single );
        procedure Click( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
        procedure MouseMove( Sender: TRevGraphicControl; IsOver: Boolean; X, Y: Integer );
        procedure MMov( Sender: TRevGraphicControl; IsOver: Boolean; X, Y: Integer );
        function IsActive: Boolean;
        procedure SetActive( const Value: Boolean );
        procedure Close( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
        procedure Maximize( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
        procedure SetShowTitle( const Value: Boolean );

      public
        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
        destructor Destroy; override;

      published
        property GradColor1: DWord read FGradColor1 write FGradColor1;
        property GradColor2: DWord read FGradColor2 write FGradColor2;
        property GradColor1Act: DWord read FGradColor1Act write FGradColor1Act;
        property GradColor2Act: DWord read FGradColor2Act write FGradColor2Act;
        property FontColorAct: DWord read FFontColorAct write FFontColorAct;
        property FontColor: DWord read FFontColor write FFontColor;
        property Active: Boolean read IsActive write SetActive;
        property Caption: String read FCaption write FCaption;
        property ShowTitle: Boolean read FShowTitle write SetShowTitle;
        property Alpha: Byte read GetAlpha write SetAlpha;
        property TitleBarSize: Integer read FTitleBarSize Write SetTitleBarSize;
    end;

implementation

{ TForm }

procedure TRevForm.Click( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X,
  Y: Integer );
begin
  Active:= True;
end;

constructor TRevForm.Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
begin
  inherited;
  FGradColor1:= Color( 128, 128, 128, 127 ).Col;
  FGradColor2:= Color( 192, 192, 192, 127 ).Col;
  FGradColor1Act:= Color( 0, 36, 104, 127 ).Col;
  FGradColor2Act:= Color( 168, 204, 240, 127 ).Col;
  FFontColorAct:= Color( 255, 255, 255, 127 ).Col;
  FFontColor:= Color( 0, 0, 255, 127 ).Col;

  Width:= 150;
  Height:= 100;

  CaptionBar:= TRevGraphicControl.Create( Self, AEngine, _Manager, Self );
  CaptionBar.OnDraw:= _Draw;
  CaptionBar.Align:= alTop;
  OnMouseClick:= Click;
  OnDraw:= _Draw;
  CaptionBar.OnMouseMove:= MouseMove;
  CaptionBar.OnMouseClick:= Click;
  CaptionBar.NoBound:= True;
  CaptionBar.BoundLeft:= 2;
  CaptionBar.BoundTop:= 2;
  CaptionBar.BoundRight:= 2;
  CaptionBar.BoundBottom:= 2;
  CaptionBar.Canvas.Font.Name:= 'Arial';
  FCloseBtn:= TRevButton.Create( Self, AEngine, Manager, CaptionBar );
  FCloseBtn.Align:= alRight;
  FCloseBtn.Caption:= 'r';
  FCloseBtn.FontName:= 'Marlett';
  FCloseBtn.OnMouseClick:= Close;
  FMaxBtn:= TRevButton.Create( Self, AEngine, Manager, CaptionBar );
  FMaxBtn.Align:= alRight;
  FMaxBtn.Caption:= '1';
  FMaxBtn.FontName:= 'Marlett';
  TitleBarSize:= 15;
  FMaxBtn.OnMouseClick:= Maximize;
  FMinWidth:= 80;
  FMinHeight:= 80;
  OnMouseMove:= MMov;
//  Bound1:= Point( 5, TitleBarSize + 5 );
  BoundLeft:= 5;
  BoundRight:= 5;
  BoundBottom:= 5;
  Caption:= Name;
  FShowTitle:= True;
end;

destructor TRevForm.Destroy;
begin
//  FCloseBtn.Free;
//  CaptionBar.Free;
  inherited;
end;

function TRevForm.IsActive: Boolean;
begin
  Result:= Manager._ActiveForm = Self;
end;

procedure TRevForm.MouseMove( Sender: TRevGraphicControl; IsOver: Boolean; X,
  Y: Integer );
var
  CPos: TPoint;
begin
  if ( FResMode = 0 ) then
    if ( Sender = CaptionBar ) then
      begin
        if ( Manager.Engine.Wnd <> nil ) then
          CPos:= Manager.Engine.Wnd.Cursor
        else
          begin
            GetCursorPos( CPos );
            ScreenToClient( Manager.Engine.DisplayDriver.CurSwapChain.Handle, CPos );
          end;
        if ( Sender.WasDown ) then
          begin
            Left:= Left + CPos.x - FOldPos.x;
            Top:= Top + CPos.y - FOldPos.y;
          end;
        FOldPos.x:= CPos.x;
        FOldPos.y:= CPos.y;
      end;
end;

procedure TRevForm._Draw( Sender: TRevGraphicControl; OffSetX, OffSetY, _Width, _Height: Single );
var
  FntC,
  Col1,
  Col2: DWord;
  ShadePos: TVector;
  w: Single;
begin
  if ( Sender = Self ) then
    begin
      Col1:= FGradColor1Act;
      Col2:= FGradColor2Act;
{      ShadePos.X:= Engine.Wnd.Cursor.X - ( FScreenLeft + FScreenWidth / 2 );
      ShadePos.Y:= Engine.Wnd.Cursor.Y - ( FScreenTop + FScreenHeight / 2 );
      w:= sqrt( sqr( ShadePos.x ) + sqr( ShadePos.y ));
      ShadePos:= Vector( - ShadePos.x, - ShadePos.y, 0.0 );}
      w:= 10 + FOwnerCtrlBuf.IndexOf( Self ) * 3;
      ShadePos:= Vector( 10, 10, 0 );
      Canvas.RenderShade( ShadePos.X, ShadePos.Y, FScreenWidth + ShadePos.X, FScreenHeight + ShadePos.Y, w,
        $00000000, $40000000 );

      Canvas.RenderRect( 0, 0, Width, Height,
        Brightness( Col1, 64 ), Brightness( Col2, 64 ), Brightness( Col1, -64 ), Brightness( Col2, -64 ));
      Canvas.RenderRect( BoundLeft, BoundTop,
        Width - BoundRight - 1, Height - BoundBottom - 1,
        Brightness( Col1, 64 ), Brightness( Col2, 64 ), Brightness( Col1, -64 ), Brightness( Col2, -64 ));
      Canvas.Render3DFrame( 0, 0, Width - 1, Height - 1,
        Brightness( Col1, 64 ), Brightness( Col2, 64 ), Brightness( Col1, -64 ), Brightness( Col2, -64 ), 64 );
      Canvas.Render3DFrame( BoundLeft, BoundTop, Width - BoundRight - 1, Height - BoundBottom - 1,
        Brightness( Col1, 64 ), Brightness( Col2, 64 ), Brightness( Col1, -64 ), Brightness( Col2, -64 ), -64 );
    end
  else if ( Sender = CaptionBar ) then
    begin
      if ( Active ) then
        begin
          Col1:= FGradColor1Act;
          Col2:= FGradColor2Act;
          FntC:= FFontColorAct;
        end
      else
        begin
          Col1:= FGradColor1;
          Col2:= FGradColor2;
          FntC:= FFontColor;
        end;

      CaptionBar.Canvas.RenderRect( 0, 0, Sender.Width, Sender.Height div 2,
        Brightness( Col1, 64 ), Brightness( Col2, 64 ), Brightness( Col1, 0 ), Brightness( Col2, 0 ));
      CaptionBar.Canvas.RenderRect( 0, Sender.Height div 2, Sender.Width, Sender.Height,
        Brightness( Col1, -64 ), Brightness( Col2, -64 ), Brightness( Col1, -32 ), Brightness( Col2, -32 ));
      CaptionBar.Canvas.Font.Size:= Round( CaptionBar.Height );
      CaptionBar.Canvas.Font.Color:= FntC;
      CaptionBar.Canvas.RenderText( Caption, Point( 5, 0 ));
    end;
end;

procedure TRevForm.SetActive( const Value: Boolean );
begin
  if ( Value ) then
    Manager._ActiveForm:= Self
  else
    Manager._ActiveForm:= nil;
  Manager.Focus:= nil;
  if ( Value ) then
    BringToFront;
end;

procedure TRevForm.MMov( Sender: TRevGraphicControl; IsOver: Boolean; X,
  Y: Integer );
var
  hC: hCursor;
  OldResMode: Integer;
begin
  OldResMode:= FResMode;

  if ( Manager.GameInput.MBtns[ 0 ]) then
    begin
      if ( FResMode > 0 ) then
        case FResMode of
          1:
            begin
              Left:= Left + X;
              Top:= Top + Y;
              Width:= Width - X;
              Height:= Height - Y;
            end;
          2:
            begin
              Left:= Left + X;
              Height:= Max( 0, Y );
              Width:= Width - X;
            end;
          3:
            begin
              Left:= Left + X;
              Width:= Width - X;
            end;
          4:
            begin
              Width:= Max( 0, X );
              Top:= Top + Y;
              Height:= Height - Y;
            end;
          5:
            begin
              Width:= Max( 0, X );
              Height:= Max( 0, Y );
            end;
          6:
            begin
              Width:= Max( 0, X );
            end;
          7:
            begin
              Top:= Top + Y;
              Height:= Height - Y;
            end;
          8:
            begin
              Height:= Max( 0, Y );
            end;
        end;
      if ( Manager.Focus <> nil ) then
        Manager.Focus:= nil;
      exit;
    end;

//  if ( not PtInRect( Rect( X - 5, Y - 5, Round( Width ) + 5, Round( Height ) + 5 ), Point( X, Y ))) then
  if (( not ( WasOver or CaptionBar.WasOver )) and ( not WasDown )) then
    FResMode:= 0
  else if (( X >= -5 ) and ( X <= 5 )) then
    begin
      if (( Y >= -5 ) and ( Y <= 5 )) then
        FResMode:= 1
      else if (( Y >= Height - 5 ) and ( Y <= Height + 5 )) then
        FResMode:= 2
      else
        FResMode:= 3;
    end
  else if (( X >= Width - 5 ) and ( X <= Width + 5 )) then
    begin
      if (( Y >= -5 ) and ( Y <= 5 )) then
        FResMode:= 4
      else if (( Y >= Height - 5 ) and ( Y <= Height + 5 )) then
        FResMode:= 5
      else
        FResMode:= 6;
    end
  else if (( Y >= -5 ) and ( Y <= 5 )) then
    FResMode:= 7
  else if (( Y >= Height - 5 ) and ( Y <= Height + 5 )) then
    FResMode:= 8
  else
    FResMode:= 0;

  case ( Align ) of
    alLeft:
      begin
        if ( FResMode <> 6 ) then
          FResMode:= 0;
      end;
    alRight:
      begin
        if ( FResMode <> 3 ) then
          FResMode:= 0;
      end;
    alTop:
      begin
        if ( FResMode <> 8 ) then
          FResMode:= 0;
      end;
    alBottom:
      begin
        if ( FResMode <> 7 ) then
          FResMode:= 0;
      end;
    alClient: FResMode:= 0;
  end;

  if ( OldResMode <> FResMode ) then
    begin
      case ( FResMode ) of
        0: hC:= LoadCursor( 0, IDC_ARROW );
        1: hC:= LoadCursor( 0, IDC_SIZENWSE );
        2: hC:= LoadCursor( 0, IDC_SIZENESW );
        3: hC:= LoadCursor( 0, IDC_SIZEWE );
        4: hC:= LoadCursor( 0, IDC_SIZENESW );
        5: hC:= LoadCursor( 0, IDC_SIZENWSE );
        6: hC:= LoadCursor( 0, IDC_SIZEWE );
        7: hC:= LoadCursor( 0, IDC_SIZENS );
        8: hC:= LoadCursor( 0, IDC_SIZENS );
      end;
      SetCursor( hC );
    end;
end;

procedure TRevForm.Close( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  Visible:= False;
end;

procedure TRevForm.SetShowTitle( const Value: Boolean );
begin
  FShowTitle:= Value;
  if ( FShowTitle ) then
    begin
      BoundTop:= 20;
      CaptionBar.Visible:= True;
    end
  else
    begin
      BoundTop:= 5;
      CaptionBar.Visible:= False;
    end;
end;

function TRevForm.GetAlpha: Byte;
begin
  Result:= Byte( FBaseColor shr 24 );
end;

procedure TRevForm.SetAlpha( const Value: Byte );
var
  pB: pByte;
begin
  pB:= Pointer( Integer( @FBaseColor ) + 3 );
  pB^:= Value;
end;

procedure TRevForm.SetTitleBarSize( const Value: Integer );
begin
  FTitleBarSize:= Value;
  CaptionBar.Height:= FTitleBarSize;
  FMaxBtn.Width:= FTitleBarSize;
  FMaxBtn.FontSize:= TitleBarSize - 5;
  FCloseBtn.Width:= FTitleBarSize;
  FCloseBtn.FontSize:= TitleBarSize - 5;
  BoundTop:= TitleBarSize + 5;
end;

procedure TRevForm.Maximize( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean;
  X, Y: Integer );
begin
  if ( Align = alClient ) then
    begin
      Align:= alNone;
      Width:= FOldWidth;
      Height:= FOldHeight;
      Left:= FOldLeft;
      Top:= FOldTop;
      FMaxBtn.Caption:= '1';
    end
  else
    begin
      FOldWidth:= Width;
      FOldHeight:= Height;
      FOldLeft:= Left;
      FOldTop:= Top;
      Align:= alClient;
      FMaxBtn.Caption:= '2';      
    end;
end;

initialization
  Rev_RegisterClass( TRevForm );

end.