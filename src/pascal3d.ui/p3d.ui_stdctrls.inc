//p3dgui_stdctrls.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

  { TP3DEdit }

  TP3DEdit = class ( TP3DButton )
    private
      FCursorRight: Boolean;
      FPageScrollX: Single;
      FSel1,
      FSel2: Integer;
      FTPos: TVec2;
      function GetPageScrollXPixel: Float;
      function GetSelLength(): Integer;
      function GetSelStart(): Integer;
      procedure SetPageScrollX(AValue: Single);
      procedure SetSel1(AValue: Integer);
      procedure SetSel2(AValue: Integer);
      procedure SetSelLength(AValue: Integer);
      procedure SetSelStart(AValue: Integer);
      procedure UpdateScroll;
      function GetPixelScrollPosition( X: Float ): Float;

    protected
      procedure SetCaption(AValue: String); override;
      procedure SetFocused(AValue: Boolean); override;

      property Caption;
      procedure DeleteLeftOfCursor();
      procedure DeleteRightOfCursor();
      procedure Insert( S: String );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Draw(); override;
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Float ); override;
      procedure MouseMove( X, Y: Float ); override;
      function CursorToSelPos( X: Float ): Cardinal;
      procedure KeyboardAction; override;

    published
      property Text: String read FCaption write SetCaption;
      property SelStart: Integer read GetSelStart write SetSelStart;
      property SelLength: Integer read GetSelLength write SetSelLength;
      property Sel1: Integer read FSel1 write SetSel1;
      property Sel2: Integer read FSel2 write SetSel2;
      property CursorRight: Boolean read FCursorRight write FCursorRight;
      property PageScrollX: Single read FPageScrollX write SetPageScrollX;
      property PageScrollXPixel: Float read GetPageScrollXPixel;
  end;

  { TP3DLabel }

  TP3DLabel = class ( TP3DGraphicControl )
    private
      FAutoSize: Boolean;
      FCaption: String;
      FCaptionTxt: TP3DTextBmp;
      FFont: TP3DCanvasFont;
      FHAlignment: TP3DHorizontalAlignment;
      FVAlignment: TP3DVerticalAlignment;

      procedure SetAutoSize(AValue: Boolean);
      procedure SetCaption( AValue: String ); virtual;
      procedure Resize;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;

    published
      property Caption: String read FCaption write SetCaption;
      property AutoSize: Boolean read FAutoSize write SetAutoSize;
      property Font: TP3DCanvasFont read FFont write FFont;
      property HAlignment: TP3DHorizontalAlignment read FHAlignment write FHAlignment;
      property VAlignment: TP3DVerticalAlignment read FVAlignment write FVAlignment;
  end;

  { TP3DGroupBox }

  TP3DGroupBox = class( TP3DGraphicControl )
    protected
      FBorderColor: TP3DColorRGBA;
      FBorderless: Boolean;
      FCanClose: Boolean;
      FCanCollapse: Boolean;
      FCaption: String;
      FCaptionTxt: TP3DTextBmp;
      FCollapsed: Boolean;
      FColor: TP3DColorPaletteMaterialDesign;
      FFont: TP3DCanvasFont;
      FOnClose: TP3DNotifyEvent;
      FStoredHeight: Float;
      FCollapseSymbolTxt: TP3DTextBmp;
      FCollapseSymbol: Char;
      FCloseSymbolTxt: TP3DTextBmp;
      FCloseSymbol: Char;

      procedure SetBordeless(AValue: Boolean);
      procedure SetCanCollapse(AValue: Boolean);
      procedure SetCaption(AValue: String);
      procedure SetCollapsed(AValue: Boolean);
      procedure SetColor(AValue: TP3DColorPaletteMaterialDesign); virtual;
      procedure SetHeight(AValue: Float); override;
      procedure UpdateCollapsed;
      class function HasShadow: Boolean; override;
      procedure Close; virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y:Float); override;
      procedure Draw(); override;
      class function IsFocusControl: Boolean; override;

      property Caption: String read FCaption write SetCaption;
      property Font: TP3DCanvasFont read FFont write FFont;
      property Color: TP3DColorPaletteMaterialDesign read FColor write SetColor;
      property BorderColor: TP3DColorRGBA read FBorderColor write FBorderColor;
      property CanCollapse: Boolean read FCanCollapse write SetCanCollapse;
      property CanClose: Boolean read FCanClose write FCanClose;
      property Collapsed: Boolean read FCollapsed write SetCollapsed;
      property Borderless: Boolean read FBorderless write SetBordeless;
      property OnClose: TP3DNotifyEvent read FOnClose write FOnClose;
  end;

  { TP3DPanel }

  TP3DPanel = class ( TP3DGraphicControl )
    protected
      FColor: TP3DColorRGBA;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Draw; override;

      property Color: TP3DColorRGBA read FColor write FColor;
  end;

  { TP3DCustomValueEdit }

  TP3DCustomValueEdit = class ( TP3DEdit )
    private
      FFixedDigits: Integer;
      FPrecision: Integer;
      FStep: Single;
      FTransmission: Single;
      FTyping: Boolean;
      FCursorMoved: Boolean;
      FValueNameText: TP3DTextBmp;
      FValueText: TP3DTextBmp;
      FValueCached: Single;

    protected
      function GetValue: Single; virtual;
      procedure SetValue(AValue: Single); virtual;

      procedure SetTyping(AValue: Boolean);
      procedure SetValueName(AValue: AnsiString); virtual;
      function GetValueName: AnsiString; virtual;

      procedure UpdateValueText(AValue: Single);
      procedure UpdateNameText(AValue: String);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure MouseMove(X, Y: Float); override;
      procedure MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float); override;
      procedure MouseUp(mb1, mb2, mb3: Boolean; X, Y: Float); override;
      procedure KeyboardAction; override;

    published
      property ValueName: AnsiString read GetValueName write SetValueName;
      property Value: Single read GetValue write SetValue;
      property Typing: Boolean read FTyping write SetTyping;
      property Step: Single read FStep write FStep;
      property Transmission: Single read FTransmission write FTransmission;
      property Precision: Integer read FPrecision write FPrecision;
      property FixedDigits: Integer read FFixedDigits write FFixedDigits;
  end;

  { TP3DEventValueEdit }

  TP3DEventValueEdit = class ( TP3DCustomValueEdit )
    private type
      TGetValue = function ( Sender: TP3DEventValueEdit ): Single of object;
      TSetValue = procedure ( Sender: TP3DEventValueEdit; AValue: Single ) of object;
      TGetValueName = function ( Sender: TP3DEventValueEdit ): String of object;
      TSetValueName = procedure ( Sender: TP3DEventValueEdit; AValue: String ) of object;

    private
      FGetValueEvent: TGetValue;
      FGetValueNameEvent: TGetValueName;
      FSetValueEvent: TSetValue;
      FSetValueNameEvent: TSetValueName;

    protected
      function GetValue: Single; override;
      procedure SetValue(AValue: Single); override;
      procedure SetValueName(AValue: AnsiString); override;
      function GetValueName: AnsiString; override;

    public
      property GetValueEvent: TGetValue read FGetValueEvent write FGetValueEvent;
      property SetValueEvent: TSetValue read FSetValueEvent write FSetValueEvent;
      property GetValueNameEvent: TGetValueName read FGetValueNameEvent write FGetValueNameEvent;
      property SetValueNameEvent: TSetValueName read FSetValueNameEvent write FSetValueNameEvent;
  end;

  { TP3DValueEdit }

  TP3DValueEdit = class ( TP3DCustomValueEdit )
    protected
      FValue: Single;
      FValueName: String;

      function GetValue: Single; override;
      procedure SetValue(AValue: Single); override;
      procedure SetValueName(AValue: AnsiString); override;
      function GetValueName: AnsiString; override;
  end;


  TP3DVectorEditOrientation = ( veoHorizontal, veoVertical );
  { TP3DVectorEdit }

  TP3DVectorEdit = class ( TP3DGraphicControl )
    private type
      TGetValue = function ( Sender: TP3DVectorEdit ): TVec4 of object;
      TSetValue = procedure ( Sender: TP3DVectorEdit; AValue: TVec4 ) of object;

    private
      FEdCount: Integer;
      FEds: array[ 0..3 ] of TP3DStreamablePointer;
      FGetValueEvent: TGetValue;
      FHAlignment: TP3DHorizontalAlignment;
      FOrientation: TP3DVectorEditOrientation;
      FSetValueEvent: TSetValue;
      FStep: Single;
      FTransmission: Single;
      FLabels: array [ 0..3 ] of String;
      FVAlignment: TP3DVerticalAlignment;

      function GetEd( Index: Integer ): TP3DEventValueEdit;
      function GetEdValueName( Sender: TP3DEventValueEdit ): String;
      function GetLabel( Index: Integer ): String;
      function GetValue: TVec4; virtual;
      procedure SetEd( Index: Integer ; AValue: TP3DEventValueEdit );
      procedure SetEdCount(AValue: Integer);
      procedure SetHAlignment(AValue: TP3DHorizontalAlignment);
      procedure SetLabel( Index: Integer ; AValue: String);
      procedure SetOrientation(AValue: TP3DVectorEditOrientation);
      procedure SetStep(AValue: Single);
      procedure SetTransmission(AValue: Single);
      procedure SetVAlignment(AValue: TP3DVerticalAlignment);
      procedure SetValue(AValue: TVec4); virtual;
      function GetEdValue( Sender: TP3DEventValueEdit ): Single;
      procedure SetEdValue( Sender: TP3DEventValueEdit; AValue: Single );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Realign(); override;

      procedure Update; override;

      property Eds[ Index: Integer ]: TP3DEventValueEdit read GetEd write SetEd;
      property EdCount: Integer read FEdCount write SetEdCount;
      property Value: TVec4 read GetValue write SetValue;
      property GetValueEvent: TGetValue read FGetValueEvent write FGetValueEvent;
      property SetValueEvent: TSetValue read FSetValueEvent write FSetValueEvent;
      property Step: Single read FStep write SetStep;
      property Transmission: Single read FTransmission write SetTransmission;
      property Labels[ Index: Integer ]: String read GetLabel write SetLabel;
      property HAlignment: TP3DHorizontalAlignment read FHAlignment write SetHAlignment;
      property VAlignment: TP3DVerticalAlignment read FVAlignment write SetVAlignment;
      property Orientation: TP3DVectorEditOrientation read FOrientation write SetOrientation;
  end;

  TP3DScrollBarKind = ( p3dsbHorizontal, p3dsbVertical );

  { TP3DScrollBar }

  TP3DScrollBar = class( TP3DGraphicControl )
    private
      FKind: TP3DScrollBarKind;
      FMax: Float;
      FMin: Float;
      FOnChange: TNotifyEvent;
      FClientPageSize: Float;
      FPercentage: Float;
      FPreset: TP3DButtonPreset;
      function GetPosition: Float;
      procedure SetKind(AValue: TP3DScrollBarKind);
      procedure SetMax(AValue: Float);
      procedure SetPageSize(AValue: Float);
      procedure SetPercentage(AValue: Float);
      procedure SetPosition(AValue: Float);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure SetAlign(const Value: TP3DControlAlign); override;

      procedure MouseMove(X, Y: Float); override;

    published
      property Kind: TP3DScrollBarKind read FKind write SetKind;
      property Percentage: Float read FPercentage write SetPercentage;
      property ClientPageSize: Float read FClientPageSize write SetPageSize;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property Position: Float read GetPosition write SetPosition;
      property Min: Float read FMin write FMin;
      property Max: Float read FMax write SetMax;
      property Preset: TP3DButtonPreset read FPreset write FPreset;
  end;

  { TP3DComboBox }

  TP3DComboBox = class;

  TP3DComboBoxChange = procedure ( Sender: TP3DComboBox; var AValue: String ) of object;

  TP3DComboBox = class ( TP3DButton )
    private
      FDownSymbol: TP3DTextBmp;
      FDropDownMenu: TP3DStreamablePointer;
      FOnChange: TP3DComboBoxChange;

      procedure DropDownItemClick( Sender: TP3DMenuItem );
      function GetDropDownMenu: TP3DPopupMenu;
      function GetItems: TP3DMenuItemList;
      procedure SetDropDownMenu(AValue: TP3DPopupMenu);

    protected
      procedure SetCaption( AValue: AnsiString ); override;
      property DropDownMenu: TP3DPopupMenu read GetDropDownMenu write SetDropDownMenu;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure DropDown;
      procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Float ); override;

    published
      property Items: TP3DMenuItemList read GetItems;
      property OnChange: TP3DComboBoxChange read FOnChange write FOnChange;
  end;

  { TP3DComboBoxDataBlock }

  TP3DComboBoxDataBlock = class ( TP3DComboBox )
    private
      FTypeFilter: TP3DDataBlockType;
      procedure SetTypeFilter(AValue: TP3DDataBlockType);
      property Items: TP3DMenuItemList read GetItems;

    public
      procedure Update; override;
      property TypeFilter: TP3DDataBlockType read FTypeFilter write SetTypeFilter;
  end;

  { TP3DCheckBox }

  TP3DCheckBox = class ( TP3DButton )
    protected
      FChecked: Boolean;
      FOnChange: TP3DNotifyEvent;
      FSymbolChecked: TP3DTextBmp;

      function GetChecked: Boolean; virtual;
      procedure SetChecked( AValue: Boolean ); virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float); override;

    published
      property Checked: Boolean read GetChecked write SetChecked;
      property OnChange: TP3DNotifyEvent read FOnChange write FOnChange;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DEventValueEdit }

function TP3DEventValueEdit.GetValue: Single;
begin
  if ( not Assigned( FGetValueEvent )) then
    Result:= inherited GetValue
  else
    Result:= FGetValueEvent( Self );
end;

procedure TP3DEventValueEdit.SetValue(AValue: Single);
begin
  if ( Assigned( FSetValueEvent )) then
    FSetValueEvent( Self, AValue );
  inherited SetValue(AValue);
end;

procedure TP3DEventValueEdit.SetValueName(AValue: AnsiString);
begin
  if ( Assigned( FSetValueNameEvent )) then
    FSetValueNameEvent( Self, AValue );
  inherited SetValueName(AValue);
end;

function TP3DEventValueEdit.GetValueName: AnsiString;
begin
  if ( not Assigned( FGetValueNameEvent )) then
    Result:= inherited GetValueName
  else
    Result:= FGetValueNameEvent( Self );
end;

{ TP3DValueEdit }

function TP3DValueEdit.GetValue: Single;
begin
  Result:= FValue;
end;

procedure TP3DValueEdit.SetValue(AValue: Single);
begin
  if FValue= AValue then Exit;
  FValue:= AValue;

  inherited SetValue( FValue );
end;

procedure TP3DValueEdit.SetValueName(AValue: AnsiString);
begin
  if ( FValueName <> AValue ) then
    FValueName:= AValue;
end;

function TP3DValueEdit.GetValueName: AnsiString;
begin
  Result:= FValueName;
end;

{ TP3DValueEdit }

procedure TP3DCustomValueEdit.SetTyping(AValue: Boolean);
begin
  if FTyping=AValue then Exit;
  FTyping:=AValue;
  Text:= FormatFloat( '0.' + DupeString( '0', FixedDigits ) + DupeString( '#', Precision - FixedDigits ), Value );
end;

function TP3DCustomValueEdit.GetValueName: AnsiString;
begin
  Result:= '';
end;

procedure TP3DCustomValueEdit.UpdateValueText( AValue: Single );
begin
  if ( Assigned( FValueText )) then
    FValueText.Free;
  FValueText:= TP3DTextBmp.CreateSimple( FormatFloat( '0.' + DupeString( '0', FixedDigits ) + DupeString( '#', Precision - FixedDigits ), AValue ), P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size );
  FValueCached:= AValue;
end;

procedure TP3DCustomValueEdit.UpdateNameText( AValue: String );
begin
  if ( Assigned( FValueNameText )) then
    FValueNameText.Free;
  FValueNameText:= TP3DTextBmp.CreateSimple( AValue + ':', P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size )
end;

function TP3DCustomValueEdit.GetValue: Single;
begin
  Result:= 0;
end;

procedure TP3DCustomValueEdit.SetValue(AValue: Single);
begin
  UpdateValueText( AValue );
end;

procedure TP3DCustomValueEdit.SetValueName(AValue: AnsiString);
begin
  if (( Assigned( FValueNameText )) and ( FValueNameText.Text = AValue + ':' )) then
    exit;
  UpdateNameText( AValue );
end;

constructor TP3DCustomValueEdit.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  //ValueName:= GetValueName;//Self.Name;
  UpdateValueText( Value );
  UpdateNameText( GetValueName );
  FStep:= 0.1;
  FTransmission:= 0.1;
  FPrecision:= 3;
  FFixedDigits:= 1;
end;

destructor TP3DCustomValueEdit.Destroy;
begin
  FValueNameText.Free;
  //WriteLn( Name + ': ' + IntToHex( Int64( FValueText ), 8 ));
  FValueText.Free;
  inherited Destroy;
end;

procedure TP3DCustomValueEdit.Draw;
var
  tp: TVec2;
  Preset: TP3DButtonPreset;
begin
  if ( not Focused ) then
    Typing:= False;
  if ( Typing ) then
    inherited Draw
  else
    begin
      if ( gcisMouseBtn1Down in InputState ) then
        Preset:= PresetDown
      else if ( gcisMouseOver in InputState ) then
        Preset:= PresetHover
      else
        Preset:= PresetNormal;
      Canvas.RenderRect( vec2( 0 ), ClippedRect.WidthHeight - 1, Preset.Color[ cp100 ]);
      Canvas.RenderRectShadowInner( vec2( 0 ), ClippedRect.WidthHeight, 2, 5, vec4( 0, 0, 0, 0.1 ));
      Canvas.RenderLineRect( vec2( 0 ), WidthHeight - 1, Preset.Color[ cp600 ], Preset.Color[ cp700 ], Preset.Color[ cp700 ], Preset.Color[ cp900 ] );

      if ( GetValueName + ':' <> FValueNameText.Text ) then
        UpdateNameText( GetValueName );
      if ( GetValue <> FValueCached ) then
        UpdateValueText( GetValue );
      tp:= vec2( 5, ClippedRect.WidthHeight.y / 2 ) - vec2( 0, FCaptionTxt.Height / 2 );
      Canvas.Font.Color:= Font.Color;
      Canvas.RenderText( FValueNameText, tp );
      Canvas.RenderText( FValueText, tp + vec2( ClippedRect.WidthHeight.x - 5 - FValueText.Width, 0 ));
    end;
end;

procedure TP3DCustomValueEdit.MouseMove(X, Y: Float);
begin
  inherited MouseMove(X, Y);

  if (( UIViewer.MouseDX <> 0 ) or ( UIViewer.MouseDY <> 0 )) then
    begin
      FCursorMoved:= True;
      Cursor:= curMoveLeftRight;
    end;
  if ( FCursorMoved and not Typing  ) then
    begin
      if ( gcisMouseBtn1Down in InputState ) then
        begin
          if ( P3DInput.Keyboard.Keys[ P3DK_LCTRL ] or P3DInput.Keyboard.Keys[ P3DK_RCTRL ]) then
            Value:= Value + Round( UIViewer.MouseDX * Transmission / Step ) * Step * 10
          else
            Value:= Value + Round( UIViewer.MouseDX * Transmission / Step ) * Step;
        end
      else
        begin
          FCursorMoved:= False;
          Cursor:= curArrow;
        end;
    end;
end;

procedure TP3DCustomValueEdit.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);
  if ( P3DInput.Mouse.Buttons[ 0 ] and P3DInput.Mouse.DButtons[ 0 ]) then
    FCursorMoved:= False;
end;

procedure TP3DCustomValueEdit.MouseUp(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseUp(mb1, mb2, mb3, X, Y);
  if ( not FCursorMoved and not P3DInput.Mouse.Buttons[ 0 ] and P3DInput.Mouse.DButtons[ 0 ]) then
    Typing:= True;
end;

procedure TP3DCustomValueEdit.KeyboardAction;
begin
  inherited KeyboardAction;
  if ( Typing AND P3DInput.Keyboard.Keys[ P3DK_RETURN ]) then
    begin
      try
        Value:= StrToFloat( Text );
        Typing:= False;
      except
        On E: Exception do;
      end;
    end;
  if ( Typing AND P3DInput.Keyboard.Keys[ P3DK_ESCAPE ]) then
    Typing:= False;
end;

{ TP3DLabel }

procedure TP3DLabel.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  if ( Assigned( FCaptionTxt )) then
    FCaptionTxt.Free;
  FCaptionTxt:= TP3DTextBmp.CreateSimple( AValue, P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size )
end;

procedure TP3DLabel.SetAutoSize(AValue: Boolean);
begin
  if FAutoSize=AValue then Exit;
  FAutoSize:=AValue;
  if ( AutoSize ) then
    Resize;
end;

procedure TP3DLabel.Resize;
begin
  WidthHeight:= vec2( FCaptionTxt.Width, FCaptionTxt.Height );
end;

procedure TP3DLabel.Draw;
var
  P: TVec2;
begin
  inherited Draw;
  Canvas.Font.Color:= Font.Color;
  Canvas.Font.Size:= Font.Size;
  Canvas.Font.Name:= Font.Name;
  Canvas.GetValidText( Caption, FCaptionTxt );
  case HAlignment of
    haLeft: P.x:= 0;
    haRight: P.x:= WidthHeight.x - FCaptionTxt.Width;
    haCenter: P.x:= ( WidthHeight.x - FCaptionTxt.Width ) / 2;
  end;
  case VAlignment of
    vaTop: P.y:= 0;
    vaBottom: P.y:= WidthHeight.y - FCaptionTxt.Height;
    vaCenter: P.y:= ( WidthHeight.y - FCaptionTxt.Height ) / 2;
  end;
  Canvas.RenderText( FCaptionTxt, P );
end;

constructor TP3DLabel.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Font:= TP3DCanvasFont.Create;
  Font.Color:= vec4( vec3( 0 ), 1 );
  Caption:= Name;
end;

destructor TP3DLabel.Destroy;
begin
  FCaptionTxt.Free;
  Font.Free;
  inherited Destroy;
end;

{ TP3DGroupBox }

procedure TP3DGroupBox.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;

  FCaption:=AValue;
  if ( Assigned( FCaptionTxt )) then
    FCaptionTxt.Free;
  FCaptionTxt:= TP3DTextBmp.CreateSimple( AValue, P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size );
end;

procedure TP3DGroupBox.SetCollapsed(AValue: Boolean);
begin
  if FCollapsed= AValue then Exit;
  FCollapsed:= AValue;
  UpdateCollapsed;
end;

procedure TP3DGroupBox.SetColor(AValue: TP3DColorPaletteMaterialDesign);
begin
  //if FColor=AValue then Exit;
  FColor:=AValue;
end;

procedure TP3DGroupBox.UpdateCollapsed;
begin
  if ( CanCollapse and Collapsed ) then
    begin
      FCollapseSymbol:= P3DSymbolTriangleRight;
      HideChildren:= True;
    end
  else
    begin
      FCollapseSymbol:= P3DSymbolTriangleDown;
      HideChildren:= False;
    end;
end;

class function TP3DGroupBox.HasShadow: Boolean;
begin
  Result:= True;
end;

procedure TP3DGroupBox.Close;
begin
  if ( Assigned( OnClose )) then
    OnClose( Self );
end;


procedure TP3DGroupBox.SetHeight( AValue: Float );
begin
  inherited SetHeight( AValue );
  if ( not ( CanCollapse and Collapsed )) then
    FStoredHeight:= AValue;
end;
{
procedure TP3DGroupBox.Realign(var ClientArea: TP3DRect);
begin
  if ( not ( CanCollapse and Collapsed )) then
    UpdateAutoSize;
  inherited Realign( ClientArea );
end;
}

constructor TP3DGroupBox.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Font:= TP3DCanvasFont.Create;
  Font.Size:= 12;
  Caption:= Name;
  Color:= Gray;
  BorderColor:= P3DThemeDefault.Primary[ cp200 ];
  Margin:= vec4( 6, 18, 6, 6 );
  FCollapseSymbol:= P3DSymbolTriangleDown;
  FCloseSymbol:= P3DSymbolClose;
end;

destructor TP3DGroupBox.Destroy;
begin
  Font.Free;
  FCaptionTxt.Free;
  FCollapseSymbolTxt.Free;
  FCloseSymbolTxt.Free;
  inherited Destroy;
end;

procedure TP3DGroupBox.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);

  Canvas.GetValidText( FCollapseSymbol, FCollapseSymbolTxt, P3DFontManager[ P3DSymbolFontName, Canvas.Font.Size ]);
  Canvas.GetValidText( FCloseSymbol, FCloseSymbolTxt, P3DFontManager[ P3DSymbolFontName, Canvas.Font.Size ]);

  if ( mb1 and CanCollapse and P3DRect( 3, 3, FCollapseSymbolTxt.Width, FCollapseSymbolTxt.Height ).PtInRect( vec2( X, Y ))) then
    Collapsed:= not Collapsed;
  if ( mb1 and CanClose and P3DRect( vec2( ClippedRect.Width - FCloseSymbolTxt.Width - 3, 3 ), vec2( FCloseSymbolTxt.Width, FCloseSymbolTxt.Height )).PtInRect( vec2( X, Y ))) then
    Close;
end;

procedure TP3DGroupBox.Draw();
var
  hw: Extended;
  pt1: TVec2;
  pt2: TVec2;
  r: TP3DRect;
begin
  inherited Draw;
  if ( Borderless ) then
    exit;
  Canvas.RenderRect( vec2( 0, 0 ), WidthHeight, Color[ cp300 ]);
  Canvas.GetValidText( Caption, FCaptionTxt,
    P3DFontManager[ Canvas.Font.Name, Canvas.Font.Size ]);
  if ( not Assigned( FCaptionTxt  )) then
    hw:= 0
  else
    hw:= FCaptionTxt.Height / 2;
  Canvas.Font.Assign( Font );
  Canvas.RenderText( FCaptionTxt, vec2( hw * 2, 2 ));
  //Canvas.RenderLineRect( vec2( hw ), vec2( Width, Height ) - hw, BorderColor );
  pt1:= vec2( hw );
  pt2:= WidthHeight - hw;
  Canvas.RenderLine( pt1, vec2( hw * 2, pt1.y ), BorderColor );
  if ( Assigned( FCaptionTxt )) then
    Canvas.RenderLine( vec2( FCaptionTxt.Width + hw * 2, pt1.y ), vec2( pt2.x, pt1.y ), BorderColor );
  Canvas.RenderLine( vec2( pt1.x, pt2.y ), pt2, BorderColor );
  Canvas.RenderLine( pt1, vec2( pt1.x, pt2.y ), BorderColor );
  Canvas.RenderLine( vec2( pt2.x, pt1.y ), pt2, BorderColor );

  //Canvas.RenderRect( vec2( hw * 2, 0 ), vec2( FCaptionTxt.Width, FCaptionTxt.Height ) + hw * 2, Color );

  if ( CanCollapse ) then
    begin
      Canvas.GetValidText( FCollapseSymbol, FCollapseSymbolTxt, P3DFontManager[ P3DSymbolFontName, Canvas.Font.Size ]);
      if ( P3DRect( 3, 3, FCollapseSymbolTxt.Width, FCollapseSymbolTxt.Height ).PtInRect( vec2( UIViewer.MouseX, UIViewer.MouseY ) - Canvas.TopLeft )) then
        Canvas.Font.Color:= P3DThemeDefault.Accent[ cp500 ]
      else
        Canvas.Font.Color:= P3DThemeDefault.Primary[ cp500 ];
      Canvas.RenderText(
        FCollapseSymbolTxt, vec2( 3 ));
    end;
  if ( CanClose ) then
    begin
      Canvas.GetValidText( FCloseSymbol, FCloseSymbolTxt,
                P3DFontManager[ P3DSymbolFontName, Canvas.Font.Size ]);
      r:= P3DRect( vec2( Width - FCloseSymbolTxt.Width - 3, 3 ), vec2( FCloseSymbolTxt.Width, FCloseSymbolTxt.Height ));
      if ( r.PtInRect( vec2( UIViewer.MouseX, UIViewer.MouseY ) - Canvas.TopLeft )) then
        Canvas.Font.Color:= P3DThemeDefault.Accent[ cp500 ]
      else
        Canvas.Font.Color:= P3DThemeDefault.Primary[ cp500 ];
      Canvas.RenderRectShadow( r.TopLeft, r.BottomRight, 5, Color[ cp500 ]);
      Canvas.RenderText( FCloseSymbolTxt, vec2( Width - FCloseSymbolTxt.Width - 3, 3 ));
    end;
end;

class function TP3DGroupBox.IsFocusControl: Boolean;
begin
  Result:= False;
//  Result:= True;
end;

procedure TP3DGroupBox.SetCanCollapse(AValue: Boolean);
begin
  if FCanCollapse= AValue then Exit;
  FCanCollapse:= AValue;
  UpdateCollapsed;
end;

procedure TP3DGroupBox.SetBordeless(AValue: Boolean);
begin
  if FBorderless=AValue then Exit;
  FBorderless:=AValue;

  if ( Borderless ) then
    begin
      CanCollapse:= False;
    end;
end;

{ TP3DEdit }

procedure TP3DEdit.SetSelLength(AValue: Integer );
begin
  if ( Sel1 > Sel2 ) then
    FSel1:= Max( 0, Min( AValue, Sel2 + AValue ))
  else
    FSel2:= Max( 0, Min( AValue, Sel1 + AValue ))
end;

function TP3DEdit.GetSelLength(): Integer;
begin
  Result:= abs( FSel2 - FSel1 );
end;

function TP3DEdit.GetPageScrollXPixel: Float;
begin
  if ( Assigned( FCaptionTxt )) then
    Result:= max( 0, FCaptionTxt.Width - ClippedRect.Width ) * ( 1 - PageScrollX );
end;

function TP3DEdit.GetSelStart(): Integer;
begin
  Result:= Min( FSel1, FSel2 );
end;

procedure TP3DEdit.SetPageScrollX(AValue: Single);
begin
  if FPageScrollX=AValue then Exit;
  FPageScrollX:= clamp( AValue, 0, 1 );
end;

procedure TP3DEdit.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  inherited;
//  FCaptionTxt.BuildIndex();
end;

procedure TP3DEdit.SetSel1( AValue: Integer );
begin
  FSel1:= Max( 0, Min( AValue, Length( FCaption )));
  UpdateScroll;
end;

procedure TP3DEdit.SetSel2( AValue: Integer );
begin
  FSel2:= Max( 0, Min( AValue, Length( FCaption )));
end;

procedure TP3DEdit.SetSelStart(AValue: Integer);
begin
  if ( Sel1 < Sel2 ) then
    Sel1:= AValue
  else
    FSel2:= Max( 0, Min( AValue, Length( FCaption )));
  UpdateScroll;
end;

procedure TP3DEdit.UpdateScroll;
var
  p: TVec4;
  t, page: Float;
begin
  if ( not Assigned( FCaptionTxt )) then
    exit;
  p:= FCaptionTxt.WidthFromTo( 0, Sel1 );
  t:= GetPixelScrollPosition( p.z );
  page:= GetPixelScrollPosition( FCaptionTxt.Width - ClippedRect.Width ) / 2;

  if ( not InRange( t, PageScrollX - page, PageScrollX + page )) then
    if ( PageScrollX > t ) then
      PageScrollX:= t - page
    else
      PageScrollX:= t + page;
end;

function TP3DEdit.GetPixelScrollPosition(X: Float): Float;
begin
  Result:= X / max( 1, FCaptionTxt.Width - ClippedRect.Width );
end;

procedure TP3DEdit.SetFocused(AValue: Boolean);
begin
  inherited SetFocused(AValue);
  P3DInput.Keyboard.ReadingTextInput:= AValue;
end;

procedure TP3DEdit.DeleteLeftOfCursor();
begin
  if (( FCaption > '' ) AND ( SelStart > 0 )) then
    begin
      if ( Sel1 = Sel2 ) then
        Sel1:= Sel1 - 1;
      Insert( '' );
    end;
end;

procedure TP3DEdit.DeleteRightOfCursor();
begin
  if (( FCaption > '' ) AND ( SelStart >= 0 )) then
    begin
      if ( Sel1 = Sel2 ) then
        Sel1:= Sel1 + 1;
      Insert( '' );
    end;
end;

procedure TP3DEdit.Insert( S: String );
begin
  Text:= Copy( FCaption, 1, SelStart ) + S + Copy( FCaption, SelStart + abs( SelLength ) + 1, Length( FCaption ));
  Sel1:= SelStart + Length( S );
  Sel2:= Sel1;
end;

constructor TP3DEdit.Create(const AOwner: IP3DStreamableContainer=nil);
begin
  inherited Create( AOwner );
  Text:= Name;
  PresetDown.Assign( PresetNormal );
  PresetHover.Assign( PresetNormal );
  //Cursor:= p;
end;

procedure TP3DEdit.Draw;
var
  Preset: TP3DButtonPreset;
  swidth: TVec4; //Length of the selection
begin
  if ( gcisMouseBtn1Down in InputState ) then
    Preset:= PresetDown
  else if ( gcisMouseOver in InputState ) then
    Preset:= PresetHover
  else
    Preset:= PresetNormal;


  Canvas.RenderRect( vec2( 0 ), ClippedRect.WidthHeight - 1, Preset.Color[ cp50 ], Preset.Color[ cp100 ], Preset.Color[ cp100 ], Preset.Color[ cp100 ] );
  Canvas.RenderRectShadowInner( vec2( 0 ), ClippedRect.WidthHeight, 2, 5, vec4( 0, 0, 0, 0.1 ));
  Canvas.RenderLineRect( vec2( 0 ), WidthHeight - 1, Preset.Color[ cp600 ], Preset.Color[ cp700 ], Preset.Color[ cp700 ], Preset.Color[ cp900 ] );

  Canvas.Font.Color:= Preset.FontColor;
  Canvas.Font.Size:= Font.Size;
  Canvas.Font.Name:= Font.Name;
  //PageScrollX:= ( 1 + sin( SDL_GetTicks / 1000 )) / 2;
  FTPos:= RenderButtonText( ClientRect + vec2( PageScrollXPixel, 0 ), Preset.FontColor );
  if (( Focused ) and ( Assigned( FCaptionTxt ))) then
    begin
      if ( Sel1 <> Sel2 ) then
        begin
          swidth:= FCaptionTxt.WidthFromTo( Min( Sel1, Sel2 ), Max( Sel1, Sel2 )); //Selection Rect
          Canvas.RenderRect( FTPos + vec2( swidth[ 0 ], swidth[ 1 ]), FTPos + vec2( swidth[ 0 ], swidth[ 1 ]) + vec2( swidth[ 2 ], swidth[ 3 ]), vec4( P3DThemeDefault.Accent[ cp900 ].xyz, 0.5 ), vec4( P3DThemeDefault.Accent[ cp400 ].xyz, 0.5), vec4( P3DThemeDefault.Accent[ cp600 ].xyz, 0.5 ), vec4( P3DThemeDefault.Accent[ cp900 ].xyz, 0.5 ));
        end;
      swidth:= FCaptionTxt.WidthFromTo( 0, Sel1 ); //Cursor
      Canvas.RenderRect( FTPos + vec2( swidth[ 2 ] - 1, swidth[ 1 ]), FTPos + vec2( swidth[ 2 ] + 1, swidth[ 3 ]), Preset.FontColor );
    end;

  if ( Assigned( FOnDraw )) then
    FOnDraw( Self, ClippedRect.Left, ClippedRect.Top, ClippedRect.Width, ClippedRect.Height );
end;

procedure TP3DEdit.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseDown( mb1, mb2, mb3, X, Y );
  if ( P3DInput.Mouse.Buttons[ 0 ]) then
    if ( P3DInput.Mouse.DButtons[ 0 ]) then
      begin
        Sel2:= CursorToSelPos( X );
        Sel1:= Sel2;
      end;
end;

procedure TP3DEdit.MouseMove(X, Y: Float);
begin
  inherited MouseMove(X, Y);
  if ( gcisMouseOver in InputState ) then
    begin
      if ( P3DInput.Mouse.Buttons[ 0 ]) then
        Sel1:= CursorToSelPos( X );
    end;
end;

function TP3DEdit.CursorToSelPos( X: Float ): Cardinal;
var
  i: Integer;
  tp: Single;
  nx: Single;
begin
  tp:= FTPos.x;
  nx:= X - tp;

  Canvas.GetValidText( Caption, FCaptionTxt,
    P3DFontManager[ Canvas.Font.Name, Canvas.Font.Size ]);

  Result:= Length( FCaption );
  if ( Result > 0 ) then
    for i:= Length( FCaption ) downto 0 do
      if ( i = Length( FCaption )) then
        begin
          if ( FCaptionTxt.LetterPosAndSize[ i - 1 ].X + FCaptionTxt.LetterPosAndSize[ i - 1 ].Z < nx ) then
            break; //PREVENT UNASSIGNED MEMORY ACCESS
        end
      else if ( FCaptionTxt.LetterPosAndSize[ i ].X <= nx ) then
        begin
          Result:= i;
          break;
        end;
end;

procedure TP3DEdit.KeyboardAction;
  procedure SetSel( Value: Integer );
  begin
    Sel1:= Value;
    if not ( P3DInput.Keyboard.KeysLayout[ P3DK_LSHIFT ] OR Boolean( P3DInput.Keyboard.KeysLayout[ P3DK_RSHIFT ])) then
      Sel2:= Sel1;
  end;

begin
  inherited KeyboardAction();
  if ( Focused ) then
    begin
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_BACKSPACE ] = ksPressed ) then
        DeleteLeftOfCursor;
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_DELETE ] = ksPressed ) then
        DeleteRightOfCursor;
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_LEFT ] = ksPressed ) then
        SetSel( Sel1 - 1 );
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_RIGHT ] = ksPressed ) then
        SetSel( Sel1 + 1 );
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_HOME ] = ksPressed ) then
        SetSel( 0 );
      if ( P3DInput.Keyboard.DKeysLayout[ P3DK_END ] = ksPressed ) then
        SetSel( Length( Text ));
      if ( P3DInput.Keyboard.InputText > '' ) then
        Insert( P3DInput.Keyboard.InputText );
    end;
end;

{ TP3DScrollBar }

function TP3DScrollBar.GetPosition: Float;
begin
  Result:= Percentage * ( Max - Min ) + Min;
end;

procedure TP3DScrollBar.SetKind(AValue: TP3DScrollBarKind);
begin
  if ( FKind = AValue ) then
    exit;

  FKind:= AValue;
end;

procedure TP3DScrollBar.SetMax(AValue: Float);
begin
  if FMax=AValue then Exit;
  FMax:= Math.Max( 0.0001, AValue );
end;

procedure TP3DScrollBar.SetPageSize(AValue: Float);
begin
  FClientPageSize:= Math.Min( 1.0, Math.Max( 0.0001, AValue ));
end;

procedure TP3DScrollBar.SetPercentage(AValue: Float);
begin
  if ( ClientPageSize = 1.0 ) then
    FPercentage:= 0.0
  else
    FPercentage:= Math.Min( 1.0, Math.Max( 0.0, AValue ));
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

procedure TP3DScrollBar.SetPosition(AValue: Float);
begin
  Percentage:= ( AValue - Min ) / Max;
end;

constructor TP3DScrollBar.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Preset:= TP3DButtonPreset.Create;
  Preset.Color:= P3DThemeDefault.Accent; //vec4( 225/255, 230/255, 234/255, 1 );
  Preset.FontColor:= PrimaryText;// vec4( vec3( 0 ), 0.3 );

  ClientPageSize:= 0.5;
  Percentage:= 0.0;
  Kind:= p3dsbVertical;
  FMax:= 1.0;
  FMin:= 0.0;
  WidthHeight:= vec2( 10 );
end;

destructor TP3DScrollBar.Destroy;
begin
  Preset.Free;
  inherited Destroy;
end;

procedure TP3DScrollBar.Draw;
var
  off, tp: Single;
const
  sb = 3;
begin
  inherited Draw;
  Canvas.RenderRect( vec2( 0 ), ClippedRect.WidthHeight, Preset.Color[ cp50 ]);
  case Kind of
    p3dsbHorizontal:
      begin
        off:= Math.Max( WidthHeight.y, ( WidthHeight.x - 2*sb ) * ClientPageSize );
        tp:= ( WidthHeight.x - off ) * Percentage;
        Canvas.RenderRoundRect( vec2( tp + sb, sb ), vec2( tp + off - sb, WidthHeight.y - sb ), WidthHeight.y / 6, 4, Preset.FontColor );
      end;
    p3dsbVertical:
      begin
        off:= Math.Max( WidthHeight.x, ( WidthHeight.y - 2*sb ) * ClientPageSize );
        tp:= ( WidthHeight.y - off ) * Percentage;
        Canvas.RenderRoundRect( vec2( sb, tp + sb ), vec2( WidthHeight.x - sb, tp + off - sb ), WidthHeight.x / 6, 4, Preset.FontColor );
      end;
  end;
end;

procedure TP3DScrollBar.SetAlign(const Value: TP3DControlAlign);
begin
  inherited SetAlign(Value);
  case ( Align ) of
    alNone, alClient:;
    alLeft, alRight: Kind:= p3dsbVertical;
    alTop, alBottom: Kind:= p3dsbHorizontal;
  end;
end;


procedure TP3DScrollBar.MouseMove(X, Y: Float);
var
  off, tp: Single;
begin
  inherited MouseMove(X, Y);
  if ( gcisMouseBtn1Down in InputState ) then
    case Kind of
      p3dsbHorizontal:
        begin
          off:= Math.Max( WidthHeight.y, WidthHeight.x * ClientPageSize );
          tp:= ( WidthHeight.x - off );
          if ( tp > 0 ) then
            Percentage:= ( X - off / 2 ) / tp;
        end;
      p3dsbVertical:
        begin
          off:= Math.Max( WidthHeight.x, WidthHeight.y * ClientPageSize );
          tp:= ( WidthHeight.y - off );
          if ( tp > 0 ) then
            Percentage:= ( Y - off / 2 ) / tp;
        end;
    end;
end;

{ TP3DVectorEdit }

function TP3DVectorEdit.GetEd(Index: Integer): TP3DEventValueEdit;
begin
  Result:= TP3DEventValueEdit( FEds[ Index ].AsValue );
end;

function TP3DVectorEdit.GetEdValueName(Sender: TP3DEventValueEdit): String;
begin
  if ( Sender = Eds[ 0 ]) then
    Result:= Labels [ 0 ]
  else if ( Sender = Eds[ 1 ]) then
    Result:= Labels [ 1 ]
  else if ( Sender = Eds[ 2 ]) then
    Result:= Labels [ 2 ]
  else if ( Sender = Eds[ 3 ]) then
    Result:= Labels [ 3 ]
  else
    Result:= '';
end;

function TP3DVectorEdit.GetLabel( Index: Integer ): String;
begin
  Result:= FLabels[ Index ];
end;

function TP3DVectorEdit.GetValue: TVec4;
begin
  if ( Assigned( GetValueEvent )) then
    Result:= GetValueEvent( Self )
  else
    Result:= vec4( -NaN, -NaN, -NaN, -NaN );
end;

procedure TP3DVectorEdit.SetEd(Index: Integer; AValue: TP3DEventValueEdit);
begin
  FEds[ Index ].AsValue:= AValue;
end;

procedure TP3DVectorEdit.SetEdCount(AValue: Integer);
var
  i: Integer;
begin
  if FEdCount=AValue then Exit;

  for i:= EdCount to AValue - 1 do
    begin
      Eds[ i ]:= TP3DEventValueEdit.Create( Self.Controls );
      Eds[ i ].SetValueEvent:= @SetEdValue;
      Eds[ i ].GetValueEvent:= @GetEdValue;
      Eds[ i ].GetValueNameEvent:= @GetEdValueName;
    end;

  for i:= EdCount downto AValue do
    Eds[ i ].Free;

  FEdCount:= AValue;
  Update;
end;


procedure TP3DVectorEdit.SetHAlignment(AValue: TP3DHorizontalAlignment);
begin
  if FHAlignment=AValue then Exit;
  FHAlignment:=AValue;
  Update;
end;

procedure TP3DVectorEdit.SetLabel( Index: Integer ; AValue: String);
begin
  FLabels[ Index ]:= AValue;
end;

procedure TP3DVectorEdit.SetOrientation(AValue: TP3DVectorEditOrientation);
begin
  if FOrientation=AValue then Exit;
  FOrientation:=AValue;
  Update;
end;

procedure TP3DVectorEdit.SetStep(AValue: Single);
var
  i: Integer;
begin
  if FStep=AValue then Exit;
  FStep:= AValue;
  for i:= 0 to EdCount - 1 do
    Eds[ i ].Step:= AValue;
end;

procedure TP3DVectorEdit.SetTransmission(AValue: Single);
var
  i: Integer;
begin
  if FTransmission=AValue then Exit;
  FTransmission:=AValue;
  for i:= 0 to EdCount - 1 do
    Eds[ i ].Transmission:= AValue;
end;

procedure TP3DVectorEdit.SetVAlignment(AValue: TP3DVerticalAlignment);
begin
  if FVAlignment=AValue then Exit;
  FVAlignment:=AValue;
  Update;
end;

procedure TP3DVectorEdit.SetValue(AValue: TVec4);
begin
  if ( Assigned( SetValueEvent )) then
    SetValueEvent( Self, AValue );
end;

function TP3DVectorEdit.GetEdValue(Sender: TP3DEventValueEdit): Single;
var
  i: Integer;
begin
  for i:= 0 to EdCount do
    if (( i < EdCount ) and ( Sender = Eds[ i ])) then
      break;
  if ( i < EdCount ) then
    Result:= Value[ i ]
  else
    Result:= 0;
end;

procedure TP3DVectorEdit.SetEdValue(Sender: TP3DEventValueEdit; AValue: Single);
var
  i: Integer;
  Vec: TVec4;
begin
  for i:= 0 to EdCount do
    if (( i < EdCount ) and ( Sender = Eds[ i ])) then
      break;
  if ( i < EdCount ) then
    begin
      Vec:= Value;
      Vec[ i ]:= AValue;
      Value:= Vec;
    end;
end;

constructor TP3DVectorEdit.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FLabels[ 0 ]:= 'X';
  FLabels[ 1 ]:= 'Y';
  FLabels[ 2 ]:= 'Z';
  FLabels[ 3 ]:= 'W';
  FEds[ 0 ]:= TP3DStreamablePointer.Create( 'Eds[0]', TP3DEventValueEdit );
  FEds[ 1 ]:= TP3DStreamablePointer.Create( 'Eds[1]', TP3DEventValueEdit );
  FEds[ 2 ]:= TP3DStreamablePointer.Create( 'Eds[2]', TP3DEventValueEdit );
  FEds[ 3 ]:= TP3DStreamablePointer.Create( 'Eds[3]', TP3DEventValueEdit );
  Properties.Add([ FEds[ 0 ], FEds[ 1 ], FEds[ 2 ], FEds[ 3 ]]);
  EdCount:= 3;
  HAlignment:= haRight;
  PageModeX:= pmStretch;
  Orientation:= veoVertical;
end;

procedure TP3DVectorEdit.Realign();
begin
  inherited Realign();
  Update;
end;

procedure TP3DVectorEdit.Update;
var
  i: Integer;
  Al: TP3DControlAlign;
begin
  case Orientation of
    veoHorizontal:
      begin
        PageModeY:= pmStretch;
        PageModeX:= pmExpand;
        Al:= alLeft;
      end;
    veoVertical:
      begin
        PageModeY:= pmExpand;
        PageModeX:= pmStretch;
        Al:= alTop;
      end;
  end;
  for i:= 0 to EdCount - 1 do
    begin
      Eds[ i ].Width:= 50;
      Eds[ i ].Height:= 20;
      Eds[ i ].Align:= Al;
      Eds[ i ].HAlignment:= HAlignment;
      Eds[ i ].VAlignment:= VAlignment;
      Eds[ i ].NeedsUpdate:= True;
    end;
  inherited Update;
end;


{ TP3DComboBox }

procedure TP3DComboBox.DropDownItemClick(Sender: TP3DMenuItem);
begin
  if ( Assigned( Sender )) then
    Caption:= Sender.Caption;
  DropDownMenu.Hide;
end;

function TP3DComboBox.GetDropDownMenu: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FDropDownMenu.AsValue );
end;

function TP3DComboBox.GetItems: TP3DMenuItemList;
begin
  Result:= DropDownMenu.FItems;
end;

procedure TP3DComboBox.SetDropDownMenu(AValue: TP3DPopupMenu);
begin
  FDropDownMenu.AsValue:= AValue;
end;

procedure TP3DComboBox.SetCaption(AValue: String);
begin
  if ( Assigned( OnChange )) then
    OnChange( Self, AValue );
  inherited SetCaption(AValue);
end;

constructor TP3DComboBox.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FDropDownMenu:= TP3DStreamablePointer.Create( 'DropDownMenu', TP3DPopupMenu );
  Properties.Add( FDropDownMenu );
  if ( UIViewer is TP3DUIViewer ) then
    DropDownMenu:= TP3DPopupMenu.Create( TP3DUIViewer( UIViewer ).Controls )
  else
    DropDownMenu:= TP3DPopupMenu.Create();
  DropDownMenu.OnItemClick:= @DropDownItemClick;
  DropDownMenu.AutoWidth:= False;
  Margin.X:= 20;
  Margin.Z:= 20;
end;

destructor TP3DComboBox.Destroy;
begin
  //if ( Assigned( FDropDownMenu ) and Assigned( Owner) and ( ParentList.IndexOf( FDropDownMenu ) > 0 )) then
  DropDownMenu.Free;
  if ( Assigned( FDownSymbol )) then
    FreeAndNil( FDownSymbol );
  inherited Destroy;
end;

procedure TP3DComboBox.Draw;
var
  P: TVec2;
begin
  inherited Draw;

  if ( Assigned( FDownSymbol )) then
    P:= vec2( ClippedRect.Width - FDownSymbol.Width, ( ClippedRect.Height - FDownSymbol.Height ) / 2 )
  else
    P:= vec2( 0 );

  Canvas.Font.Name:= 'Pascal3D-Symbols';
  Canvas.RenderText( P3DSymbolTriangleDown, P, FDownSymbol );
  Canvas.Font.Name:= 'Cabin-Regular';//'OpenSans-Semibold';//'RobotoCondensed-Light';
end;

procedure TP3DComboBox.DropDown;
begin
  DropDownMenu.Width:= Width;
  DropDownMenu.PopUp( ClippedRectScreen.Left, ClippedRectScreen.Top + ClippedRectScreen.Height );
end;

procedure TP3DComboBox.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    DropDown;
end;



{ TP3DCheckBox }

function TP3DCheckBox.GetChecked: Boolean;
begin
  Result:= FChecked;
end;

procedure TP3DCheckBox.SetChecked(AValue: Boolean);
begin
  if FChecked= AValue then Exit;
  FChecked:= AValue;

  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

constructor TP3DCheckBox.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FSymbolChecked:= TP3DTextBmp.CreateSimple( P3DSymbolCheck, P3DFontManagerBmp.Fonts[ 'Pascal3D-Symbols'], Canvas.Font.Size );
end;

destructor TP3DCheckBox.Destroy;
begin
  FSymbolChecked.Free;
  inherited Destroy;
end;

procedure TP3DCheckBox.Draw;
var
  Preset: TP3DButtonPreset;
  clf, ColorChecked: TVec4;
  P: TVec2;
  R: TP3DRect;

  CheckBoxWidth: Float;
  OffSet: Float;
begin
  if ( gcisMouseBtn1Down in InputState ) then
    Preset:= PresetDown
  else if ( gcisMouseOver in InputState ) then
    Preset:= PresetHover
  else
    Preset:= PresetNormal;

{  if (( Enabled ) and
    ( gcisMouseBtn1Down in InputState )) then
    begin
      RenderButton( Accent900, Accent400, Accent600, Accent900, Accent900, Accent900, Accent900, Accent900 );
      RenderButtonText( Accent50 );
    end
  else if (( Enabled ) and
    ( gcisMouseOver in InputState )) then
      begin
        RenderButton( Accent100, vec4( 1 ), Accent50, Accent100, Accent900, Accent900, Accent900, Accent900 );
        RenderButtonText( Accent700 );
      end
  else
    begin
      RenderButton( Accent200, Accent100, Accent200, Accent200, Accent900, Accent900, Accent900, Accent900 );
      RenderButtonText( Accent900 );
    end }

  {if (( Enabled ) and
      ( gcisMouseBtn1Down in InputState )) then
    RenderButton( Accent900, Accent400, Accent600, Accent900, Accent900, Accent900, Accent900, Accent900 )
  else if (( Enabled ) and
    ( gcisMouseOver in InputState )) then
    RenderButton( Accent100, vec4( 1 ), Accent50, Accent100, Accent900, Accent900, Accent900, Accent900 )
  else
    RenderButton( Accent200, Accent100, Accent200, Accent200, Accent900, Accent900, Accent900, Accent900 );}

  if (( Enabled ) and
    ( gcisMouseBtn1Down in InputState )) then
    with ( PresetDown ) do
      begin
        RenderButton( Color[ cp900 ], Color[ cp400 ], Color[ cp600 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
        //RenderButtonText( FontColor );
      end
  else if (( Enabled ) and
    ( gcisMouseOver in InputState )) then
    with ( PresetHover ) do
      begin
        RenderButton( Color[ cp100 ], vec4( 1 ), Color[ cp50 ], Color[ cp100 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
        //RenderButtonText( FontColor );
      end
  else
    with ( PresetNormal ) do
      begin
        RenderButton( Color[ cp200 ], Color[ cp100 ], Color[ cp200 ], Color[ cp200 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
        //RenderButtonText( FontColor );
      end;

  //Canvas.RenderRect( vec2( 0 ), vec2( Width, Height ) - 1, Preset.Color, Preset.Color, Preset.Color, Preset.Color );
  //Canvas.RenderLineRect( vec2( 0 ), vec2( Width, Height ) - 1, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor );


  if ( Focused ) then
    begin
      clf:= vec4( 0.5, 0.5, 0.5, 0.5 );
      Canvas.RenderLineRect( vec2( 4 ), WidthHeight - 4, clf, clf, clf, clf );
    end;

  CheckBoxWidth:= FCaptionTxt.Height;
  OffSet:= CheckBoxWidth + 10;

  if ( Caption > '' ) then
    begin
      if ( not Assigned( FCaptionTxt )) then
        FCaptionTxt:= TP3DTextBmp.CreateSimple( Caption, P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size );

      case HAlignment of
        haLeft: P.x:= OffSet;
        haRight: P.x:= WidthHeight.x - FCaptionTxt.Width - OffSet;
        haCenter: P.x:= ( WidthHeight.x - FCaptionTxt.Width + OffSet ) / 2;
      end;
      case VAlignment of
        vaTop: P.y:= 0;
        vaBottom: P.y:= WidthHeight.y - FCaptionTxt.Height;
        vaCenter: P.y:= ( WidthHeight.y - FCaptionTxt.Height ) / 2;
      end;
    end;

{  if ( Checked ) then
    ColorChecked:= Accent900//vec4( $78 / 255, $F2 / 255, $2C / 255, 1 )
  else
    ColorChecked:= Accent100;}

  case HAlignment of
    haLeft: R:= P3DRect( 5, P.Y, CheckBoxWidth, CheckBoxWidth );
    haRight: R:= P3DRect( WidthHeight.x - CheckBoxWidth - 5, P.Y, CheckBoxWidth, CheckBoxWidth );
    haCenter: R:= P3DRect( P.X - OffSet, P.Y, CheckBoxWidth, CheckBoxWidth );
  end;

  Canvas.RenderRectShadow( R.TopLeft + 1, R.BottomRight + 1, 2, vec4( 0, 0, 0, 0.1 ));

  with ( PresetNormal ) do
    begin
      if ( Checked ) then
        Canvas.RenderRect( R.TopLeft, R.BottomRight, Color[ cp900 ], Color[ cp400 ], Color[ cp600 ], Color[ cp900 ])
      else
        Canvas.RenderRect( R.TopLeft, R.BottomRight, Color[ cp400 ], Color[ cp200 ], Color[ cp300 ], Color[ cp400 ]);
      if ( Checked ) then
        begin
          Canvas.Font.Color:= Color[ cp50 ];
          Canvas.RenderText( FSymbolChecked, R.TopLeft );
        end;
    end;

  Canvas.Font.Color:= Preset.FontColor;
  Canvas.Font.Size:= Font.Size;
  Canvas.Font.Name:= Font.Name;
  Canvas.RenderText( Caption, P, FCaptionTxt );
end;

procedure TP3DCheckBox.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);

  if ( mb1 ) then
    Checked:= not Checked;
end;


{ TP3DComboBoxDataBlock }

procedure TP3DComboBoxDataBlock.SetTypeFilter(AValue: TP3DDataBlockType);
begin
  if FTypeFilter=AValue then Exit;
  FTypeFilter:=AValue;
  Update;
end;

procedure TP3DComboBoxDataBlock.Update;
{  procedure AddLibrary( Lib: TP3DLibrary );
  var
    HeaderAdded: Boolean;
    i: Integer;
    LibTitle: String;
  begin
    HeaderAdded:= False;
    if ( Lib = P3DData ) then
      LibTitle:= '<main scene>'
    else
      LibTitle:= ExtractFileNameOnly( Lib.FileWatch.FileName );
    for i:= 0 to Lib.DataBlocks.Count - 1 do
      if ( Lib.DataBlocks[ i ] is TypeFilter ) then
        begin
          if (( not HeaderAdded )) then
            begin
              Items[ Items.Add( LibTitle )].Enabled:= False;
              HeaderAdded:= True;
            end;
          if ( i = 20 ) then
            with ( Items[ Items.Add( '...')]) do
              begin
                Enabled:= False;
                break;
              end;
          with ( Items[ Items.Add( '  ' + Lib.DataBlocks[ i ].Name )]) do
        end;
  end;}

var
  i: Integer;
begin
  Canvas.Font.Size:= 16;
  with ( Items[ Items.Add( 'Clear' )]) do
    begin
      PresetNormal.FontColor:= vec4( 1.0, 0.5, 0.5, 1 );
      UserData:= nil;
    end;
  inherited Update;
end;


{$ENDIF}

