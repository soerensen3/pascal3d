unit RScrollbars;

interface
  uses
    Core,
    RVCL,
    RTypes,
    SysUtils,
    RUtils,
    RButtons,
    Math,
    Classes;

  type
    TScrollBarKind = ( sbHorizontal, sbVertical );
    TRevScrollBar = class( TRevGraphicControl )
      protected
        FKind: TScrollBarKind;
        FPercentage: Single;
        FPageSize: Single;
        FColor: Cardinal;
        FOnChange: TNotifyEvent;
        FMin: Single;
        FMax: Single;
        FFlat: Boolean;

        procedure SetWidth( const Value: Integer ); override;
        procedure SetHeight( const Value: Integer ); override;
        procedure SetKind( const Value: TScrollBarKind );
        procedure Arrange;
        procedure SetPercentage( const Value: Single );
        procedure MDown( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
        procedure SetPageSize( const Value: Single );
        procedure Realign; override;
        procedure MMove( Sender: TRevGraphicControl; IsOver: Boolean; X, Y: Integer );
        procedure SetColor( const Value: Cardinal );
        function GetPosition: Single;
        procedure SetPosition( const Value: Single );

      protected
        Slider: TRevButton;
        ButtonInc: TRevButton;
        ButtonDec: TRevButton;

        procedure SetAlign( const Value: TControlAlign ); override;

      public
        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
        procedure Draw; override;

      published
        property Kind: TScrollBarKind read FKind write SetKind;
        property Percentage: Single read FPercentage write SetPercentage;
        property PageSize: Single read FPageSize write SetPageSize;
        property Color: Cardinal read FColor write SetColor;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
        property Position: Single read GetPosition write SetPosition;
        property Min: Single read FMin write FMin;
        property Max: Single read FMax write FMax;
        property Flat: Boolean read FFlat write FFlat;
    end;

implementation

{ TScrollBar }

procedure TRevScrollBar.Arrange;
begin
  if (( ButtonInc = nil ) or ( ButtonDec = nil ) or ( Slider = nil )) then
    exit;

  case ( Kind ) of
    sbHorizontal:
      begin
        FHeight:= 20;
        ButtonDec.Align:= alLeft;
        ButtonDec.Width:= Height;
        ButtonDec.Caption:= '3';
        ButtonDec.ButtonStyle:= bsGlowHorizontal;
        ButtonInc.Align:= alRight;
        ButtonInc.Width:= Height;
        ButtonInc.Caption:= '4';
        ButtonInc.ButtonStyle:= bsGlowHorizontal;
        Slider.Width:= Math.Max( Height, Round(( Width - ButtonInc.Width - ButtonDec.Width ) * PageSize ));
        Slider.Height:= Height;
        Slider.Top:= 0;
        Slider.Left:= Round( ButtonDec.Width + Math.Max( 0.0, ( Width - ButtonInc.Width - ButtonDec.Width - Slider.Width )) * Percentage );
        Slider.ButtonStyle:= bsGlowHorizontal;
      end;
    sbVertical:
      begin
        FWidth:= 20;
        ButtonDec.Align:= alTop;
        ButtonDec.Height:= Width;
        ButtonDec.Caption:= '5';
        ButtonDec.ButtonStyle:= bsGlowVertical;
        ButtonInc.Align:= alBottom;
        ButtonInc.Height:= Width;
        ButtonInc.Caption:= '6';
        ButtonInc.ButtonStyle:= bsGlowVertical;
        Slider.Width:= Width;
        Slider.Height:= Math.Max( Width, Round(( Height - ButtonInc.Height - ButtonDec.Height ) * PageSize ));
        Slider.Left:= 0;
        Slider.Top:= Round( ButtonDec.Height + Math.Max( 0.0, ( Height - ButtonInc.Height - ButtonDec.Height - Slider.Height )) * Percentage );
        Slider.ButtonStyle:= bsGlowVertical;
      end;
  end;
end;

constructor TRevScrollBar.Create( AOwner: TBaseObject; AEngine: TEngine;
  _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
begin
  inherited;
  PageSize:= 0.5;
  Percentage:= 0.0;
  Slider:= TRevButton.Create( Self, AEngine, Manager, Self );
  Slider.OnMouseDown:= MDown;
  Slider.OnMouseUp:= MDown;
  Slider.OnMouseMove:= MMove;
  Slider.Tag:= -1;
  ButtonInc:= TRevButton.Create( Self, AEngine, Manager, Self );
  ButtonInc.OnMouseDown:= MDown;
  ButtonInc.FontName:= 'Marlett';
  ButtonDec:= TRevButton.Create( Self, AEngine, Manager, Self );
  ButtonDec.OnMouseDown:= MDown;
  ButtonDec.FontName:= 'Marlett';
  Kind:= sbVertical;
  Color:= $80003088;
  FMax:= 1.0;
  FMin:= 0.0;
end;

procedure TRevScrollBar.Draw;
var
  vp,
  Oldvp: TViewport;
  hx, hw,
  rx, ry: Integer;

  procedure ShowProgressHint;
  begin
    vp.MinZ:= 0.0;
    vp.MaxZ:= 1.0;

    vp.X:= 0;
    vp.Y:= 0;
    vp.Width:= Engine.DisplayDriver.Device.Displ.XRes;
    vp.Height:= Engine.DisplayDriver.Device.Displ.YRes;

    Oldvp:= Engine.DisplayDriver.GetViewport;
    Engine.DisplayDriver.SetViewport( vp );

    hw:= Manager.DefaultFont.TextWidth( FormatFloat( '0.00', Position ), 16 );
    case ( FKind ) of
      sbHorizontal:
        begin
          hx:= Round( Math.Max( 0, Math.Min( Manager.MouseX - ( FScreenLeft ), Slider.Width )));
          Canvas.RenderLineRect( hx - hw / 2 + Slider.Left - 2, -20, hx + hw / 2 + Slider.Left + 2, 0, $FF000000, $FF000000, $FF000000, $FF000000 );
          Canvas.RenderRect( hx - hw / 2 + Slider.Left, -20 - 2, hx + hw / 2 + Slider.Left + 2, 0, $AAFFFF80, $AAFFFF80, $AAFFFF80, $AAFFFF80 );
          Canvas.Font.Size:= 16;
          Canvas.Font.Color:= $FF000000;
          Canvas.RenderText( FormatFloat( '0.00', Position ), Point( Round( hx - hw / 2 + Slider.Left ), - 18 ));
        end;
      sbVertical:
        begin
          hx:= Round( Math.Max( Slider.Top, Math.Min( Manager.MouseY - ( FScreenTop ), Slider.Top + Slider.Height )));
          Canvas.RenderLineRect(( Width - hw ) / 2 - 2, hx -20, ( Width + hw ) / 2 + 2, hx, $FF000000, $FF000000, $FF000000, $FF000000 );
          Canvas.RenderRect(( Width - hw ) / 2 - 2, hx -20, ( Width + hw ) / 2 + 2, hx, $AAFFFF80, $AAFFFF80, $AAFFFF80, $AAFFFF80 );
          Canvas.Font.Size:= 16;
          Canvas.Font.Color:= $FF000000;
          Canvas.RenderText( FormatFloat( '0.00', Position ), Point( Round(( Width - hw ) / 2 ), hx - 18 ));
        end;
    end;

    Engine.DisplayDriver.SetViewport( Oldvp );
  end;
begin
  inherited;
  if ( Flat ) then
    begin
      Canvas.Render3DFrame( 0, 0, Width - 1, Height - 1, Color, Color, Color, Color, -50 );
      Canvas.RenderRect( 0, 0, Width - 1, Height - 1, Color, Color, Color, Color );
    end
  else
    begin
      if ( Kind = sbHorizontal ) then
        begin
          Canvas.RenderRect( 0, 0, Width, Height div 2,
            Brightness( Color, 64 ), Brightness( Color, 64 ), Brightness( Color, 0 ), Brightness( Color, 0 ));
          Canvas.RenderRect( 0, Height div 2, Width, Height,
            Brightness( Color, -64 ), Brightness( Color, -64 ), Brightness( Color, -32 ), Brightness( Color, -32 ));
        end
      else
        begin
          Canvas.RenderRect( 0, 0, Width div 2, Height,
            Brightness( Color, 64 ), Brightness( Color, 0 ), Brightness( Color, 64 ), Brightness( Color, 0 ));
          Canvas.RenderRect( Width div 2, 0, Width, Height,
            Brightness( Color, -64 ), Brightness( Color, -32 ), Brightness( Color, -64 ), Brightness( Color, -32 ));
        end;
    end;
  if ( Slider.WasDown ) then
    ShowProgressHint;
end;

function TRevScrollBar.GetPosition: Single;
begin
  Result:= Percentage * ( Max - Min ) + Min;
end;

procedure TRevScrollBar.MDown( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean;
  X, Y: Integer );
begin
  if ( Sender = ButtonInc ) then
    Percentage:= Percentage + 0.1 * ( 1 - PageSize );
  if ( Sender = ButtonDec ) then
    Percentage:= Percentage - 0.1 * ( 1 - PageSize );
  if ( Sender = Slider ) then
    if ( mb1 ) then
      begin
        if ( Kind = sbHorizontal ) then
          Sender.Tag:= X
        else
          Sender.Tag:= Y;
      end
    else
      Sender.Tag:= -1;
end;

procedure TRevScrollBar.MMove( Sender: TRevGraphicControl; IsOver: Boolean; X, Y: Integer );
begin
  if ( Sender.WasDown ) then
    if ( Kind = sbHorizontal ) then
      begin
        Sender.Left:= Math.Max( ButtonDec.Width, Math.Min( Width - ButtonInc.Width - Slider.Width, Sender.Left + X - Sender.Tag ));
        Percentage:= ( Slider.Left - ButtonDec.Width ) / ( Width - ButtonDec.Width - ButtonInc.Width - Slider.Width );
      end
    else
      begin
        Sender.Top:= Math.Max( ButtonDec.Height, Math.Min( Height - ButtonInc.Height - Slider.Height, Sender.Top + Y - Sender.Tag ));
        Percentage:= ( Slider.Top - ButtonDec.Height ) / ( Height - ButtonDec.Height - ButtonInc.Height - Slider.Height );
      end;
end;

procedure TRevScrollBar.Realign;
begin
  inherited;
  Arrange;
end;

procedure TRevScrollBar.SetAlign( const Value: TControlAlign );
begin
  inherited;
  case ( Align ) of
    alNone, alClient:;
    alLeft, alRight: Kind:= sbVertical;
    alTop, alBottom: Kind:= sbHorizontal;
  end;
  Arrange;
end;

procedure TRevScrollBar.SetColor( const Value: Cardinal );
begin
  FColor:= Value;
  If ( ButtonInc <> nil ) then
    ButtonInc.Color:= Color;
  If ( ButtonDec <> nil ) then
    ButtonDec.Color:= Color;
  If ( Slider <> nil ) then
    Slider.Color:= Color;
end;

procedure TRevScrollBar.SetHeight( const Value: Integer );
begin
  inherited;
  Arrange;
end;

procedure TRevScrollBar.SetKind( const Value: TScrollBarKind );
begin
  if ( FKind = Value ) then
    exit;

  FKind:= Value;
  case ( Kind ) of
    sbHorizontal: Align:= alBottom;
    sbVertical: Align:= alRight;
  end;
end;

procedure TRevScrollBar.SetPageSize( const Value: Single );
begin
  FPageSize:= Math.Min( 1.0, Math.Max( 0.0001, Value ));
  Arrange
end;

procedure TRevScrollBar.SetPercentage( const Value: Single );
begin
  if ( PageSize = 1.0 ) then
    FPercentage:= 0.0
  else
    FPercentage:= Math.Min( 1.0, Math.Max( 0.0, Value ));
  Arrange;
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

procedure TRevScrollBar.SetPosition( const Value: Single );
begin
  Percentage:= ( Value - Min ) / Max;
end;

procedure TRevScrollBar.SetWidth( const Value: Integer );
begin
  inherited;
  Arrange;
end;

initialization
  Rev_RegisterClass( TRevScrollBar );


end.
