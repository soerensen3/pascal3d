//p3dgui_forms.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

type

  { TP3DForm }

  TP3DForm = class( TP3DGroupBox )
    private
      FMouseDownPos: TVec2;
    public
      class function HasShadow: Boolean; override;

    private
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure MouseMove(X, Y: Integer); override;
  end;

  TP3DModalResult = ( mrCancel, mrOk );

  { TP3DModalWindow }

  TP3DModalWindow = class( TP3DForm )
    private
      FModalResult: TP3DModalResult;
      FOverlay: TP3DGraphicControl;

      procedure SetModalResult(AValue: TP3DModalResult);

    public
      procedure ShowModal;

      constructor Create(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
      procedure FastDestroy; override;
      procedure DrawOverlay( Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Single );

      property Overlay: TP3DGraphicControl read FOverlay write FOverlay;
      property ModalResult: TP3DModalResult read FModalResult write SetModalResult;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DForm }

class function TP3DForm.HasShadow: Boolean;
begin
  Result:= True;
end;

procedure TP3DForm.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if ( mb1 and ( gcisMouseOver in InputState )) then
    begin
      BringToFront;
      FMouseDownPos:= vec2( P3DInput.Mouse.X, P3DInput.Mouse.Y ) - LeftTop;
    end;
end;

procedure TP3DForm.MouseMove(X, Y: Integer);
begin
  inherited MouseMove( X, Y );
  if ( gcisMouseBtn1Down in InputState ) then
    begin
      Left:= P3DInput.Mouse.X - FMouseDownPos.X;
      Top:= P3DInput.Mouse.Y - FMouseDownPos.Y;
    end;
end;

{ TP3DModalWindow }

procedure TP3DModalWindow.SetModalResult(AValue: TP3DModalResult);
begin
  if FModalResult=AValue then Exit;
  FModalResult:=AValue;
  Visible:= False;
end;

procedure TP3DModalWindow.ShowModal;
begin
  Overlay.Visible:= True;
  Overlay.BringToFront;
  BringToFront;
end;

constructor TP3DModalWindow.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  Overlay:= TP3DGraphicControl.Create();
  Overlay.OnDraw:= @DrawOverlay;
  Overlay.Visible:= False;
  Overlay.Align:= alClient;
end;

procedure TP3DModalWindow.FastDestroy;
begin
  Overlay.Free;
  inherited FastDestroy;
end;

procedure TP3DModalWindow.DrawOverlay(Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Single);
begin
  Sender.Canvas.RenderRect( vec2( 0 ), vec2( Sender.Width, Sender.Height ), vec4( Primary900.XYZ, 0.6 ));
end;


{$ENDIF}

