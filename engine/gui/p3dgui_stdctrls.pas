unit p3dgui_stdctrls;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  p3dbmpfont,
  math,
  p3dMath,
  p3dobjects,
  p3dinput,
  p3dcanvas,
  p3dgui,
  p3dgui_buttons,
  p3dgui_focuscontrol;

type

  { TP3DFocusControl }

  { TP3DEdit }

  TP3DEdit = class ( TP3DButton )
    private
      FCursorRight: Boolean;
      FSel1,
      FSel2: Integer;
      function GetSelLength(): Integer;
      function GetSelStart(): Integer;
      procedure SetCaption(AValue: String);
      procedure SetSel1(AValue: Integer);
      procedure SetSel2(AValue: Integer);
      procedure SetSelLength(AValue: Integer);
      procedure SetSelStart(AValue: Integer);

    protected
      procedure SetFocused(AValue: Boolean); override;

      property Caption;
      procedure DeleteLeftOfCursor();
      procedure DeleteRightOfCursor();
      procedure Insert( S: String );

    public
      constructor Create( AOwner: TP3DObjectList; AManager: TP3DGUIManager;
         const AParent: TP3DGraphicControl = nil );
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
      procedure SetCaption( AValue: String );
      procedure Resize;

    public
      constructor Create( AOwner: TP3DObjectList; AManager: TP3DGUIManager; const AParent: TP3DGraphicControl = nil );
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

      procedure SetCaption(AValue: String);

    public
      constructor Create( AOwner: TP3DObjectList; AManager: TP3DGUIManager;
         const AParent: TP3DGraphicControl = nil );
      destructor Destroy; override;
      procedure Draw(); override;

      property Caption: String read FCaption write SetCaption;
      property Font: TP3DCanvasFont read FFont write FFont;
      property Color: TVec4 read FColor write FColor;
      property BorderColor: TVec4 read FBorderColor write FBorderColor;
  end;

implementation

{ TP3DLabel }

procedure TP3DLabel.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  FCaptionTxt.Free;
  FCaptionTxt:= p3dTextSimple( AValue, P3DFontManager[ Font.Name ], Font.Size );
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

constructor TP3DLabel.Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AOwner, AManager, AParent );
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
  FCaptionTxt:= p3dTextSimple( AValue, P3DFontManager[ Font.Name ], Font.Size );
end;

constructor TP3DGroupBox.Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Font:= TP3DCanvasFont.Create;
  Caption:= Name;
  Color:= vec4( 1 );
  BorderColor:= vec4( 0, 0, 0, 1 );
  BoundsLeft:= 15;
  BoundsTop:= 15;
  BoundsBottom:= 15;
  BoundsRight:= 15;
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
  FCaption:=AValue;
end;

procedure TP3DEdit.SetSel1( AValue: Integer );
begin
  FSel1:= Max( 0, Min( AValue, Length( FCaption )));
  WriteLn( 'Selection 1: ', FSel1, ' Selection 2: ', FSel2 );
end;

procedure TP3DEdit.SetSel2( AValue: Integer );
begin
  FSel2:= Max( 0, Min( AValue, Length( FCaption )));
  WriteLn( 'Selection 1: ', FSel1, ' Selection 2: ', FSel2 );
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
  InputManager.Keyboard.ReadingTextInput:= AValue;
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
var
  str: String;
begin
  str:= FCaption;
  Text:= Copy( FCaption, 1, SelStart ) + S + Copy( FCaption, SelStart + abs( SelLength ) + 1, Length( FCaption ));
  Sel1:= SelStart + Length( S );
  Sel2:= Sel1;
end;

constructor TP3DEdit.Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Text:= Name;
  PresetDown.Assign( PresetNormal );
  PresetHover.Assign( PresetNormal );
end;

procedure TP3DEdit.Draw;
var
  Preset: TP3DButtonPreset;
  clf: TVec4;
  tp: TVec2;
  s: TVec4;
begin
  if ( gcisMouseBtn1Down in InputState ) then
    Preset:= PresetDown
  else if ( gcisMouseOver in InputState ) then
    Preset:= PresetHover
  else
    Preset:= PresetNormal;

  Canvas.RenderRect( vec2( 0 ), vec2( Width, Height ) - 1, Preset.Color, Preset.Color, Preset.Color, Preset.Color );
  Canvas.RenderLineRect( vec2( 0 ), vec2( Width, Height ) - 1, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor );

  Canvas.Font.Color:= Preset.FontColor;
  Canvas.Font.Size:= Font.Size;
  Canvas.Font.Name:= Font.Name;
  tp:= ( vec2( 0, Height ) - vec2( 0, FCaptionTxt.Height )) / 2;
  Canvas.RenderText( FCaptionTxt, tp );
  if ( Focused ) then
    begin
      s:= vec4( tp, 0, 0 ) + FCaptionTxt.WidthFromTo( Sel1, Sel2 );
      Canvas.RenderRect( s.XY, s.XY + s.ZW,
        Preset.FontColor * 0.3, Preset.FontColor * 0.3, Preset.FontColor * 0.3, Preset.FontColor * 0.3 );

      s:= vec4( tp, 0, 0 ) + FCaptionTxt.WidthFromTo( Sel1, Sel1 );
      Canvas.RenderRect( s.XY, s.XY + vec2( 2, s.W ),
        Preset.FontColor, Preset.FontColor, Preset.FontColor, Preset.FontColor );
    end;

  if ( Assigned( FOnDraw )) then
    FOnDraw( Self, Canvas.Left, Canvas.Top, Canvas.Width, Canvas.Height );
end;

procedure TP3DEdit.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseDown( mb1, mb2, mb3, X, Y );
  if ( InputManager.Mouse.Buttons[ 0 ]) then
    if ( InputManager.Mouse.DButtons[ 0 ]) then
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
      if ( InputManager.Mouse.Buttons[ 0 ]) then
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
var
  S: String;
begin
  if ( Focused ) then
    begin
      if ( InputManager.Keyboard.Keys[ P3DK_BACKSPACE ] AND InputManager.Keyboard.DKeys[ P3DK_BACKSPACE ]) then
        DeleteLeftOfCursor;
      if ( InputManager.Keyboard.Keys[ P3DK_DELETE ] AND InputManager.Keyboard.DKeys[ P3DK_DELETE ]) then
        DeleteRightOfCursor;
      if ( InputManager.Keyboard.Keys[ P3DK_LEFT ] AND InputManager.Keyboard.DKeys[ P3DK_LEFT ]) then
        begin
          Sel1:= Sel1 - 1;
          if not ( InputManager.Keyboard.Keys[ P3DK_LSHIFT ] OR InputManager.Keyboard.Keys[ P3DK_RSHIFT ]) then
            Sel2:= Sel1;
        end;
      if ( InputManager.Keyboard.Keys[ P3DK_RIGHT ] AND InputManager.Keyboard.DKeys[ P3DK_RIGHT ]) then
        begin
          Sel1:= Sel1 + 1;
          if not ( InputManager.Keyboard.Keys[ P3DK_LSHIFT ] OR InputManager.Keyboard.Keys[ P3DK_RSHIFT ]) then
            Sel2:= Sel1;
        end;
      if ( InputManager.Keyboard.InputText > '' ) then
        Insert( InputManager.Keyboard.InputText );
    end;
end;

end.

