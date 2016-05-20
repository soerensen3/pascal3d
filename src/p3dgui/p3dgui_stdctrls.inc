//p3dgui_stdctrls.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

  { TP3DEdit }

  TP3DEdit = class ( TP3DButton )
    private
      FCursorRight: Boolean;
      FSel1,
      FSel2: Integer;
      function GetSelLength(): Integer;
      function GetSelStart(): Integer;
      procedure SetSel1(AValue: Integer);
      procedure SetSel2(AValue: Integer);
      procedure SetSelLength(AValue: Integer);
      procedure SetSelStart(AValue: Integer);

    protected
      procedure SetCaption(AValue: String); override;
      procedure SetFocused(AValue: Boolean); override;

      property Caption;
      procedure DeleteLeftOfCursor();
      procedure DeleteRightOfCursor();
      procedure Insert( S: String );

    public
      constructor Create(const AOwner: TP3DObjectList = nil; const AParent: TP3DGraphicControl=nil);
      procedure Draw(); override;
      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure MouseMove( X, Y: Integer ); override;
      function CursorToSelPos( X: Integer ): Cardinal;
      procedure KeyboardAction; override;

    published
      property Text: String read FCaption write SetCaption;
      property SelStart: Integer read GetSelStart write SetSelStart;
      property SelLength: Integer read GetSelLength write SetSelLength;
      property Sel1: Integer read FSel1 write SetSel1;
      property Sel2: Integer read FSel2 write SetSel2;
      property CursorRight: Boolean read FCursorRight write FCursorRight;
  end;

  { TP3DLabel }

  TP3DLabel = class ( TP3DGraphicControl )
    private
      FAutoSize: Boolean;
      FCaption: String;
      FCaptionTxt: TP3DText;
      FFont: TP3DCanvasFont;
      FHAlignment: THorizontalAlignment;
      FVAlignment: TVerticalAlignment;

      procedure SetAutoSize(AValue: Boolean);
      procedure SetCaption( AValue: String ); virtual;
      procedure Resize;

    public
      constructor Create(const AOwner: TP3DObjectList = nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

      procedure Draw; override;

    published
      property Caption: String read FCaption write SetCaption;
      property AutoSize: Boolean read FAutoSize write SetAutoSize;
      property Font: TP3DCanvasFont read FFont write FFont;
      property HAlignment: THorizontalAlignment read FHAlignment write FHAlignment;
      property VAlignment: TVerticalAlignment read FVAlignment write FVAlignment;
  end;

  { TP3DGroupBox }

  TP3DGroupBox = class( TP3DGraphicControl )
    private
      FBorderColor: TVec4;
      FCaption: String;
      FCaptionTxt: TP3DText;
      FColor: TVec4;
      FFont: TP3DCanvasFont;

      class function IsFocusControl: Boolean; override;
      procedure SetCaption(AValue: String);

    public
      constructor Create(const AOwner: TP3DObjectList = nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;
      procedure Draw(); override;

      property Caption: String read FCaption write SetCaption;
      property Font: TP3DCanvasFont read FFont write FFont;
      property Color: TVec4 read FColor write FColor;
      property BorderColor: TVec4 read FBorderColor write FBorderColor;
  end;

  { TP3DCustomValueEdit }

  TP3DCustomValueEdit = class ( TP3DEdit )
    private
      FStep: Single;
      FTransmission: Single;
      FTyping: Boolean;
      FCursorMoved: Boolean;
      FValueNameText: TP3DText;
      FValueText: TP3DText;

    protected
      function GetValue: Single; virtual;
      procedure SetValue(AValue: Single); virtual;

      procedure SetTyping(AValue: Boolean);
      procedure SetValueName(AValue: String); virtual;
      function GetValueName: String; virtual;

    public
      constructor Create(const AOwner: TP3DObjectList = nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;
      procedure Draw; override;
      procedure MouseMove(X, Y: Integer); override;
      procedure MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      procedure MouseUp(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      procedure KeyboardAction; override;

    published
      property ValueName: String read GetValueName write SetValueName;
      property Value: Single read GetValue write SetValue;
      property Typing: Boolean read FTyping write SetTyping;
      property Step: Single read FStep write FStep;
      property Transmission: Single read FTransmission write FTransmission;
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
      procedure SetValueName(AValue: String); override;
      function GetValueName: String; override;

    public
      property GetValueEvent: TGetValue read FGetValueEvent write FGetValueEvent;
      property SetValueEvent: TSetValue read FSetValueEvent write FSetValueEvent;
      property GetValueNameEvent: TGetValueName read FGetValueNameEvent write FGetValueNameEvent;
      property SetValueNameEvent: TSetValueName read FSetValueNameEvent write FSetValueNameEvent;
  end;

  TP3DValueEdit = class ( TP3DCustomValueEdit )
    protected
      FValue: Single;
      FValueName: String;

      function GetValue: Single; override;
      procedure SetValue(AValue: Single); override;
      procedure SetValueName(AValue: String); override;
      function GetValueName: String; override;
  end;

  TP3DScrollBarKind = ( p3dsbHorizontal, p3dsbVertical );

  { TP3DScrollBar }

  TP3DScrollBar = class( TP3DGraphicControl )
    private
      FKind: TP3DScrollBarKind;
      FMax: Float;
      FMin: Float;
      FOnChange: TNotifyEvent;
      FPageSize: Float;
      FPercentage: Float;
      FPreset: TP3DButtonPreset;
      function GetPosition: Float;
      procedure SetKind(AValue: TP3DScrollBarKind);
      procedure SetMax(AValue: Float);
      procedure SetPageSize(AValue: Float);
      procedure SetPercentage(AValue: Float);
      procedure SetPosition(AValue: Float);

    public
      constructor Create(const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

      procedure Draw; override;
      procedure Realign; override;
      procedure SetAlign(const Value: TP3DControlAlign); override;

      procedure MouseMove(X, Y: Integer); override;

    published
      property Kind: TP3DScrollBarKind read FKind write SetKind;
      property Percentage: Float read FPercentage write SetPercentage;
      property PageSize: Float read FPageSize write SetPageSize;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property Position: Float read GetPosition write SetPosition;
      property Min: Float read FMin write FMin;
      property Max: Float read FMax write SetMax;
      property Preset: TP3DButtonPreset read FPreset write FPreset;
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

procedure TP3DEventValueEdit.SetValueName(AValue: String);
begin
  if ( Assigned( FSetValueNameEvent )) then
    FSetValueNameEvent( Self, AValue );
  inherited SetValueName(AValue);
end;

function TP3DEventValueEdit.GetValueName: String;
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

procedure TP3DValueEdit.SetValueName(AValue: String);
begin
  if ( FValueName <> AValue ) then
    FValueName:= AValue;
end;

function TP3DValueEdit.GetValueName: String;
begin
  Result:= FValueName;
end;

{ TP3DValueEdit }

procedure TP3DCustomValueEdit.SetTyping(AValue: Boolean);
begin
  if FTyping=AValue then Exit;
  FTyping:=AValue;
  Text:= FloatToStr( Value );
end;

function TP3DCustomValueEdit.GetValueName: String;
begin
  Result:= '';
end;

function TP3DCustomValueEdit.GetValue: Single;
begin
  Result:= 0;
end;

procedure TP3DCustomValueEdit.SetValue(AValue: Single);
begin
  if ( Assigned( FValueText )) then
    FValueText.Free;
  FValueText:= p3dTextSimple( FloatToStrF( AValue, ffGeneral, 4, 0 ), P3DFontManager[ Font.Name, Font.Size ]);
end;

procedure TP3DCustomValueEdit.SetValueName(AValue: String);
begin
  if ( Assigned( FValueNameText )) then
    FValueNameText.Free;
  FValueNameText:= p3dTextSimple( AValue + ':', P3DFontManager[ Font.Name, Font.Size ]);
end;

constructor TP3DCustomValueEdit.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  ValueName:= GetValueName;//Self.Name;
  FValueText:= p3dTextSimple( FloatToStrF( Value, ffGeneral, 4, 0 ), P3DFontManager[ Font.Name, Font.Size ]);
  FStep:= 0.1;
  FTransmission:= 0.1;
end;

destructor TP3DCustomValueEdit.Destroy;
begin
  FValueNameText.Free;
  FValueText.Free;
  inherited Destroy;
end;

procedure TP3DCustomValueEdit.Draw;
var
  tp: TVec2;
begin
  if ( not Focused ) then
    Typing:= False;
  if ( Typing ) then
    inherited Draw
  else
    begin
      if ( GetValueName <> FValueNameText.Text ) then
        ValueName:= GetValueName;
      tp:= vec2( 5, Height / 2 ) - vec2( 0, FCaptionTxt.Height / 2 );
      Canvas.Font.Color:= Font.Color;
      Canvas.RenderText( FValueNameText, tp );
      Canvas.RenderText( FValueText, tp + vec2( Width - 5 - FValueText.Width, 0 ));
    end;
end;

procedure TP3DCustomValueEdit.MouseMove(X, Y: Integer);
begin
  inherited MouseMove(X, Y);

  if (( P3DInput.Mouse.DX <> 0 ) or ( P3DInput.Mouse.DY <> 0 )) then
    begin
      FCursorMoved:= True;
      Cursor:= curMoveLeftRight;
    end;
  if ( FCursorMoved ) then
    begin
      if ( gcisMouseBtn1Down in InputState ) then
        begin
          Value:= Value + Round( P3DInput.Mouse.DX * Transmission / Step ) * Step;
        end
      else
        begin
          FCursorMoved:= False;
          Cursor:= curArrow;
        end;
    end;
end;

procedure TP3DCustomValueEdit.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);
  if ( P3DInput.Mouse.Buttons[ 0 ] and P3DInput.Mouse.DButtons[ 0 ]) then
    FCursorMoved:= False;
end;

procedure TP3DCustomValueEdit.MouseUp(mb1, mb2, mb3: Boolean; X, Y: Integer);
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
end;

{ TP3DLabel }

procedure TP3DLabel.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  FCaptionTxt.Free;
  FCaptionTxt:= p3dTextSimple( AValue, P3DFontManager[ Font.Name, Font.Size ]);
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
  Width:= Round( FCaptionTxt.Width );
  Height:= Round( FCaptionTxt.Height );
end;

procedure TP3DLabel.Draw;
var
  P: TVec2;
begin
  inherited Draw;
  Canvas.Font.Color:= Font.Color;
  Canvas.Font.Size:= Font.Size;
  Canvas.Font.Name:= Font.Name;
  case HAlignment of
    haLeft: P.x:= 0;
    haRight: P.x:= Width - FCaptionTxt.Width;
    haCenter: P.x:= ( Width - FCaptionTxt.Width ) / 2;
  end;
  case VAlignment of
    vaTop: P.y:= 0;
    vaBottom: P.y:= Height - FCaptionTxt.Height;
    vaCenter: P.y:= ( Height - FCaptionTxt.Height ) / 2;
  end;
  Canvas.RenderText( FCaptionTxt, P );
end;

constructor TP3DLabel.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AOwner, AParent );
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
  FCaptionTxt.Free;
  FCaptionTxt:= p3dTextSimple( AValue, P3DFontManager[ Font.Name, Font.Size ]);
end;

constructor TP3DGroupBox.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Font:= TP3DCanvasFont.Create;
  Caption:= Name;
  Color:= vec4( 1 );
  BorderColor:= vec4( 0, 0, 0, 1 );
  Bounds:= P3DRect( 15, 25, 15, 15 );
end;

destructor TP3DGroupBox.Destroy;
begin
  Font.Free;
  FCaptionTxt.Free;
  inherited Destroy;
end;

procedure TP3DGroupBox.Draw;
var
  hw: Extended;
  pt1: TVec2;
  pt2: TVec2;
begin
  inherited Draw;
  Canvas.RenderRect( vec2( 0, 0 ), vec2( Width, Height ), Color );
  hw:= FCaptionTxt.Height / 2;
  //Canvas.RenderLineRect( vec2( hw ), vec2( Width, Height ) - hw, BorderColor );
  pt1:= vec2( hw );
  pt2:= vec2( Width, Height ) - hw;
  Canvas.RenderLine( pt1, vec2( hw * 2, pt1.y ), BorderColor );
  Canvas.RenderLine( vec2( FCaptionTxt.Width + hw * 2, pt1.y ), vec2( pt2.x, pt1.y ), BorderColor );
  Canvas.RenderLine( vec2( pt1.x, pt2.y ), pt2, BorderColor );
  Canvas.RenderLine( pt1, vec2( pt1.x, pt2.y ), BorderColor );
  Canvas.RenderLine( vec2( pt2.x, pt1.y ), pt2, BorderColor );

  //Canvas.RenderRect( vec2( hw * 2, 0 ), vec2( FCaptionTxt.Width, FCaptionTxt.Height ) + hw * 2, Color );
  Canvas.Font.Assign( Font );
  Canvas.RenderText( FCaptionTxt, vec2( hw * 2, 2 ));
end;

class function TP3DGroupBox.IsFocusControl: Boolean;
begin
  Result:= True;
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

function TP3DEdit.GetSelStart(): Integer;
begin
  Result:= Min( FSel1, FSel2 );
end;

procedure TP3DEdit.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  inherited;
  FCaptionTxt.BuildIndex();
end;

procedure TP3DEdit.SetSel1( AValue: Integer );
begin
  FSel1:= Max( 0, Min( AValue, Length( FCaption )));
end;

procedure TP3DEdit.SetSel2( AValue: Integer );
begin
  FSel2:= Max( 0, Min( AValue, Length( FCaption )));
end;

procedure TP3DEdit.SetSelStart(AValue: Integer);
begin
  if ( Sel1 < Sel2 ) then
    FSel1:= Max( 0, Min( AValue, Length( FCaption )))
  else
    FSel2:= Max( 0, Min( AValue, Length( FCaption )));
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
  if (( FCaption > '' ) AND ( SelStart > 0 )) then
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

constructor TP3DEdit.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Text:= Name;
  PresetDown.Assign( PresetNormal );
  PresetHover.Assign( PresetNormal );
  //Cursor:= p;
end;

procedure TP3DEdit.Draw;
var
  Preset: TP3DButtonPreset;
  tp: TVec2; //Text Position
  s: TVec2; //Cursor Position
  soff: TPoint; //Offset to the beginning of the selection
  swidth: TPoint; //Length of the selection
begin
  if ( gcisMouseBtn1Down in InputState ) then
    Preset:= PresetDown
  else if ( gcisMouseOver in InputState ) then
    Preset:= PresetHover
  else
    Preset:= PresetNormal;


  Canvas.RenderRect( vec2( 0 ), vec2( Width, Height ) - 1, Preset.Color, Preset.Color, Preset.Color, Preset.Color );
  Canvas.RenderRectShadowInner( vec2( 0 ), vec2( Width, Height ), 2, 5, vec4( 0, 0, 0, 0.1 ));
  Canvas.RenderLineRect( vec2( 0 ), vec2( Width, Height ) - 1, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor );

  Canvas.Font.Color:= Preset.FontColor;
  Canvas.Font.Size:= Font.Size;
  Canvas.Font.Name:= Font.Name;
  tp:= ( vec2( 0, Height ) - vec2( 0, FCaptionTxt.Height )) / 2;
  Canvas.RenderText( FCaptionTxt, tp );
  if ( Focused ) then
    begin
      if ( Sel1 > 0 ) then
        soff:= FCaptionTxt.WidthFromTo( 1, Sel1 + 1 )
      else
        soff:= Point( 0, 0 );

      if ( Sel1 <> Sel2 ) then
        begin
          swidth:= FCaptionTxt.WidthFromTo( Min( Sel1, Sel2 ) + 1, Max( Sel1, Sel2 ) + 1 );

          s:= tp;
          if ( Sel1 > Sel2 ) then
            s.x:= s.x - swidth.x;
          Canvas.RenderRect( s + vec2( soff.X, 0 ), s + vec2( soff.X + swidth.X, swidth.Y ), Preset.FontColor * 0.3 );
        end;

      swidth:= p3dTextSize( 'W', FCaptionTxt.Font );
      Canvas.RenderRect( tp + vec2( soff.X, 0 ), tp + vec2( soff.X + 2, soff.Y ), Preset.FontColor );
    end;

  if ( Assigned( FOnDraw )) then
    FOnDraw( Self, Canvas.Left, Canvas.Top, Canvas.Width, Canvas.Height );
end;

procedure TP3DEdit.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseDown( mb1, mb2, mb3, X, Y );
  if ( P3DInput.Mouse.Buttons[ 0 ]) then
    if ( P3DInput.Mouse.DButtons[ 0 ]) then
      begin
        Sel2:= CursorToSelPos( X );
        Sel1:= Sel2;
      end;
end;

procedure TP3DEdit.MouseMove(X, Y: Integer);
begin
  inherited MouseMove(X, Y);
  if ( gcisMouseOver in InputState ) then
    begin
      if ( P3DInput.Mouse.Buttons[ 0 ]) then
        Sel1:= CursorToSelPos( X );
    end;
end;

function TP3DEdit.CursorToSelPos( X: Integer ): Cardinal;
var
  i: Integer;
  tp: Single;
  nx: Single;
begin
  tp:= 0;
  nx:= X - tp;
  Result:= Length( FCaption );
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
begin
  if ( Focused ) then
    begin
      if ( P3DInput.Keyboard.Keys[ P3DK_BACKSPACE ] AND P3DInput.Keyboard.DKeys[ P3DK_BACKSPACE ]) then
        DeleteLeftOfCursor;
      if ( P3DInput.Keyboard.Keys[ P3DK_DELETE ] AND P3DInput.Keyboard.DKeys[ P3DK_DELETE ]) then
        DeleteRightOfCursor;
      if ( P3DInput.Keyboard.Keys[ P3DK_LEFT ] AND P3DInput.Keyboard.DKeys[ P3DK_LEFT ]) then
        begin
          Sel1:= Sel1 - 1;
          if not ( P3DInput.Keyboard.Keys[ P3DK_LSHIFT ] OR P3DInput.Keyboard.Keys[ P3DK_RSHIFT ]) then
            Sel2:= Sel1;
        end;
      if ( P3DInput.Keyboard.Keys[ P3DK_RIGHT ] AND P3DInput.Keyboard.DKeys[ P3DK_RIGHT ]) then
        begin
          Sel1:= Sel1 + 1;
          if not ( P3DInput.Keyboard.Keys[ P3DK_LSHIFT ] OR P3DInput.Keyboard.Keys[ P3DK_RSHIFT ]) then
            Sel2:= Sel1;
        end;
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
  FPageSize:= Math.Min( 1.0, Math.Max( 0.0001, AValue ));
end;

procedure TP3DScrollBar.SetPercentage(AValue: Float);
begin
  if ( PageSize = 1.0 ) then
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

constructor TP3DScrollBar.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Preset:= TP3DButtonPreset.Create;
  Preset.Color:= vec4( 165/255, 165/255, 237/255, 1 );
  Preset.OutlineColor:= vec4( vec3( 0.2 ), 0.5 );
  Preset.FontColor:= vec4( vec3( 0 ), 0.3 );

  PageSize:= 0.5;
  Percentage:= 0.0;
  Kind:= p3dsbVertical;
  FMax:= 1.0;
  FMin:= 0.0;
end;

destructor TP3DScrollBar.Destroy;
begin
  Preset.Free;
  inherited Destroy;
end;

procedure TP3DScrollBar.Draw;
var
  off, tp: Single;
begin
  inherited Draw;
  Canvas.RenderRect( vec2( 0 ), vec2( Width, Height ), Preset.Color );
  case Kind of
    p3dsbHorizontal:
      begin
        off:= Math.Max( Height, Width * PageSize );
        tp:= ( Width - off ) * Percentage;
        Canvas.RenderRect( vec2( tp, 0 ), vec2( tp + off, Height ), Preset.FontColor );
      end;
    p3dsbVertical:
      begin
        off:= Math.Max( Width, Height * PageSize );
        tp:= ( Height - off ) * Percentage;
        Canvas.RenderRect( vec2( 0, tp ), vec2( Width, tp + off ), Preset.FontColor );
      end;
  end;
end;

procedure TP3DScrollBar.Realign;
begin
  inherited;
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


procedure TP3DScrollBar.MouseMove(X, Y: Integer);
var
  off, tp: Single;
begin
  inherited MouseMove(X, Y);
  if ( gcisMouseBtn1Down in InputState ) then
    case Kind of
      p3dsbHorizontal:
        begin
          off:= Math.Max( Height, Width * PageSize );
          tp:= ( Width - off );
          if ( tp > 0 ) then
            Percentage:= ( X - off / 2 ) / tp;
        end;
      p3dsbVertical:
        begin
          off:= Math.Max( Width, Height * PageSize );
          tp:= ( Height - off );
          if ( tp > 0 ) then
            Percentage:= ( Y - off / 2 ) / tp;
        end;
    end;
end;

{$ENDIF}

