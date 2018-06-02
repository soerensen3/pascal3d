{ p3d.ui_toolbar.inc }

{$IFDEF INTERFACE}
type

  { TP3DToolbar }

  TP3DToobarButtonClick = procedure ( AButtonIdx: Integer; AButtonCaption: String ) of object;

  TP3DToolbar = class ( TP3DPanel )
    protected
      FFont: TP3DCanvasFont;
      FOnButtonClick: TP3DToobarButtonClick;
      FButtons: TStringList;

      function AddButton: TP3DButton; virtual;
      procedure ButtonsChange( Sender: TObject );
      procedure ButtonClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      property OnButtonClick: TP3DToobarButtonClick read FOnButtonClick write FOnButtonClick;

    published
      property Buttons: TStringList read FButtons write FButtons;
      property Font: TP3DCanvasFont read FFont;
  end;

  { TP3DTabControl }

  TP3DTabControl = class ( TP3DToolbar )
    function AddButton: TP3DButton; override;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DToolbar }

function TP3DToolbar.AddButton: TP3DButton;
begin
  Result:= TP3DButton.Create( Controls );
  Result.Font.Assign( Font );
  Result.Align:= alLeft;
  Result.OnMouseClick:= @ButtonClick;
end;

procedure TP3DToolbar.ButtonsChange(Sender: TObject);
var
  i: Integer;
begin
  Controls.Clear;
  for i:= 0 to Buttons.Count - 1 do
    with ( AddButton ) do
      begin
        Caption:= Buttons[ i ];
        Width:= TextWidth + 10;
      end;
end;

procedure TP3DToolbar.ButtonClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if ( Assigned( FOnButtonClick )) then
    FOnButtonClick( Controls.IndexOf( Sender ), TP3DButton( Sender ).Caption );
end;

constructor TP3DToolbar.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Buttons:= TStringList.Create;
  Buttons.OnChange:= @ButtonsChange;
  FFont:= TP3DCanvasFont.Create;
  Font.OnChange:= @ButtonsChange;
end;

destructor TP3DToolbar.Destroy;
begin
  Font.Free;
  Buttons.Free;
  inherited Destroy;
end;

{ TP3DTabControl }

function TP3DTabControl.AddButton: TP3DButton;
begin
  Result:= TP3DGroupButton.Create( Controls );
  Result.Font.Assign( Font );
  Result.Align:= alLeft;
  Result.OnMouseClick:= @ButtonClick;
  TP3DGroupButton( Result ).Group:= 'Tools';
end;

{$ENDIF}

