//p3dgui_buttons.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

  type

    { TP3DButtonPreset }

    TP3DButtonPreset = class
      private
        FColor: TP3DColorRGBA;
        FFontColor: TP3DColorRGBA;
        FOutlineColor: TP3DColorRGBA;

      public
        procedure Assign( Preset: TP3DButtonPreset );

        property Color: TP3DColorRGBA read FColor write FColor;
        property OutlineColor: TP3DColorRGBA read FOutlineColor write FOutlineColor;
        property FontColor: TP3DColorRGBA read FFontColor write FFontColor;
    end;

    { TP3DButton }

    TP3DButton = class( TP3DGraphicControl )
      private
        FFont: TP3DCanvasFont;
        FHAlignment: TP3DHorizontalAlignment;
        FVAlignment: TP3DVerticalAlignment;

        function GetTextHeight: Single;
        function GetTextWidth: Single;

      protected
        FCaption: String;
        FPresetDown: TP3DButtonPreset;
        FPresetHover: TP3DButtonPreset;
        FPresetNormal: TP3DButtonPreset;
        FState: Integer;
        FCaptionTxt: TP3DTextBmp;

        function GetCaption: String; virtual;
        procedure SetCaption( AValue: String ); virtual;
        procedure RenderButton( C1, C2, C3, C4, CE1, CE2, CE3, CE4: TVec4 );//APreset: TP3DButtonPreset );
        function RenderButtonText( C: TVec4 ): TVec2;

      public
        constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
        destructor Destroy; override;

        procedure Draw; override;
        procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
        procedure ResizeToText( AMargin: Integer );

        class function IsFocusControl: Boolean; override;

        property PresetNormal: TP3DButtonPreset read FPresetNormal write FPresetNormal;
        property PresetDown: TP3DButtonPreset read FPresetDown write FPresetDown;
        property PresetHover: TP3DButtonPreset read FPresetHover write FPresetHover;
        property Font: TP3DCanvasFont read FFont write FFont;
        property Caption: String read GetCaption write SetCaption;
        property HAlignment: TP3DHorizontalAlignment read FHAlignment write FHAlignment;
        property VAlignment: TP3DVerticalAlignment read FVAlignment write FVAlignment;
        property TextWidth: Single read GetTextWidth;
        property TextHeight: Single read GetTextHeight;
    end;

    { TP3DGroupButton }

    TP3DGroupButton = class ( TP3DButton )
      private
        FDown: Boolean;
        FGroup: String;

        procedure SetDown(AValue: Boolean);
        procedure SetGroup(AValue: String);
        procedure DisableGroup;

      public
        procedure Draw; override;
        procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;

        property Down: Boolean read FDown write SetDown;
        property Group: String read FGroup write SetGroup;
    end;

    { TP3DSplitter }

    TP3DSplitter = class ( TP3DButton )
      constructor Create(const AData: TP3DLibrary=nil; const AParent: TP3DGraphicControl=nil); override;
      procedure MouseMove(X, Y: Integer); override;
      procedure GetControls( out Ctrl1, Ctrl2: TP3DGraphicControl );
      procedure SetAlign(const AValue: TP3DControlAlign); override;
    end;



{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DButtonPreset }

procedure TP3DButtonPreset.Assign(Preset: TP3DButtonPreset);
begin
  Color:= Preset.Color;
  FontColor:= Preset.FontColor;
  OutlineColor:= Preset.OutlineColor;
end;

function TP3DButton.GetTextHeight: Single;
begin
  Result:= FCaptionTxt.Height;
end;

function TP3DButton.GetCaption: String;
begin
  Result:= FCaption;
end;

function TP3DButton.GetTextWidth: Single;
begin
  Result:= FCaptionTxt.Width;
end;

procedure TP3DButton.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;

  FCaption:=AValue;
  if ( Assigned( FCaptionTxt )) then
    FCaptionTxt.Free;
  if ( AValue > '' ) then
    FCaptionTxt:= TP3DTextBmp.CreateSimple( AValue, P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size )
  else
    FCaptionTxt:= TP3DTextBmp.CreateSimple( ' ', P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size );
end;

procedure TP3DButton.RenderButton(C1, C2, C3, C4, CE1, CE2, CE3, CE4: TVec4);
var
  clf: TVec4;
begin
  Canvas.RenderRect( vec2( 0 ), Canvas.WidthHeight - 1, C1, C2, C3, C4 );
  Canvas.RenderLineRect( vec2( 0 ), WidthHeight - 1, CE1, CE2, CE3, CE4 );

  if ( Focused ) then
    begin
      clf:= vec4( 0.5, 0.5, 0.5, 0.5 );
      Canvas.RenderLineRect( vec2( 4 ), WidthHeight - 4, clf, clf, clf, clf );
    end;
end;

function TP3DButton.RenderButtonText(C: TVec4): TVec2;
var
  P: TVec2;
begin
  if ( Caption > '' ) then
    begin
      if ( not Assigned( FCaptionTxt )) then
        FCaptionTxt:= TP3DTextBmp.CreateSimple( Caption, P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size );

      P:= Canvas.CalcAlignedTextRect( FCaptionTxt, ClientRect, HAlignment, VAlignment );

      Canvas.Font.Color:= C * vec4( 1, 1, 1, 0.5 + 0.5 * Ord( Enabled ));
      Canvas.Font.Size:= Font.Size;
      Canvas.Font.Name:= Font.Name;
      Canvas.RenderText( Caption, P, FCaptionTxt );
      Result:= P;
    end
  else
    Result:= vec2( 0 );
end;


procedure TP3DButton.Draw;
begin
  if (( Enabled ) and
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
    end
end;

procedure TP3DButton.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseDown( mb1, mb2, mb3, X, Y );
  if ( Enabled and ( gcisMouseOver in InputState )) then
    Focused:= True;
end;

procedure TP3DButton.ResizeToText( AMargin: Integer);
begin
  if ( not Assigned( FCaptionTxt )) then
    FCaptionTxt:= TP3DTextBmp.CreateSimple( Caption, P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size );

  WidthHeight:= vec2( AMargin * 2 + FCaptionTxt.Width, AMargin * 2 + FCaptionTxt.Height );
end;

class function TP3DButton.IsFocusControl: Boolean;
begin
  Result:= True;
end;


constructor TP3DButton.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  PresetDown:= TP3DButtonPreset.Create;
  PresetHover:= TP3DButtonPreset.Create;
  PresetNormal:= TP3DButtonPreset.Create;
  Font:= TP3DCanvasFont.Create;
  Caption:= Name;

  PresetNormal.Color:= Accent50;//vec4( vec3( 0.93 ), 1 );
  PresetNormal.OutlineColor:= vec4( vec3( 0.4 ), 1 );
  PresetNormal.FontColor:= vec4( vec3( 0 ), 1 );
  PresetHover.Color:= Accent400;
  PresetHover.OutlineColor:= vec4( 0.875, 0.27, 0.07, 1 );
  PresetHover.FontColor:= PrimaryText;//vec4( vec3( 0 ), 1 );
  PresetDown.Color:= Accent900;
  PresetDown.OutlineColor:= vec4( 0.875, 0.27, 0.07, 1 );
  PresetDown.FontColor:= Accent50;
  HAlignment:= haCenter;
  VAlignment:= vaCenter;
end;

destructor TP3DButton.Destroy;
begin
  FPresetDown.Free;
  FPresetHover.Free;
  FPresetNormal.Free;
  FCaptionTxt.Free;
  Font.Free;
  inherited Destroy;
end;


{ TP3DGroupButton }

procedure TP3DGroupButton.SetDown(AValue: Boolean);
begin
  if FDown = AValue then Exit;
  FDown:= AValue;
  if ( Down ) then
    DisableGroup;
end;

procedure TP3DGroupButton.SetGroup(AValue: String);
begin
  if FGroup = AValue then Exit;
  FGroup:= AValue;
  if ( Down ) then
    DisableGroup;
end;

procedure TP3DGroupButton.DisableGroup;
var
  Control: TP3DGraphicControl;
begin
  if ( Group > '' ) then
    for Control in ParentCtrl do
      if (( Control <> Self ) and ( Control is TP3DGroupButton ) and ( TP3DGroupButton( Control ).Group = Group )) then
        TP3DGroupButton( Control ).FDown:= False;
end;

procedure TP3DGroupButton.Draw;
var
  Preset: TP3DButtonPreset;
begin
  if (( Enabled ) and ( gcisMouseOver in InputState )) then
    begin
      RenderButton( Accent100, vec4( 1 ), Accent50, Accent100, Accent900, Accent900, Accent900, Accent900 );
      RenderButtonText( Accent700 );
    end
  else if (( Enabled ) and ( Down )) then
    begin
      RenderButton( Accent900, Accent400, Accent600, Accent900, Accent900, Accent900, Accent900, Accent900 );
      RenderButtonText( Accent50 );
    end
  else
    begin
      RenderButton( Accent200, Accent100, Accent200, Accent200, Accent900, Accent900, Accent900, Accent900 );
      RenderButtonText( Accent900 );
    end;
end;

procedure TP3DGroupButton.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    Down:= True;
end;


{ TP3DSlider }

constructor TP3DSplitter.Create(const AData: TP3DLibrary;
  const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  WidthHeight:= vec2( 5 );
  Caption:= '.';
  Cursor:= curHResize;
end;

procedure TP3DSplitter.MouseMove(X, Y: Integer);
var
  Ctrl1, Ctrl2: TP3DGraphicControl;
begin
  inherited MouseMove(X, Y);

  if ( not ( gcisMouseBtn1Down in InputState )) then
    exit;

  GetControls( Ctrl1, Ctrl2 );

  if ( Assigned( Ctrl1 )) then
    begin
      if ( Align in [ alLeft, alRight ]) then
        Ctrl1.Width:= P3DInput.Mouse.X - Ctrl1.Canvas.Rect.Left
      else if ( Align in [ alTop, alBottom ]) then
        Ctrl1.Height:= P3DInput.Mouse.Y - Ctrl1.Canvas.Rect.Top;
    end;
  if ( Assigned( Ctrl2 )) then
    begin
      if ( Align in [ alLeft, alRight ]) then
        Ctrl2.Width:= Ctrl2.Canvas.Rect.Right - P3DInput.Mouse.X
      else if ( Align in [ alTop, alBottom ]) then
        Ctrl2.Height:= Ctrl2.Canvas.Rect.Bottom - P3DInput.Mouse.Y;
    end;
end;

procedure TP3DSplitter.GetControls(out Ctrl1, Ctrl2: TP3DGraphicControl);
  function FindNext( idx: Integer; incr: Integer ): TP3DGraphicControl;
  var
    n: Integer;
  begin
    n:= idx + incr;
    Result:= ParentCtrl[ n ];
    while ( Assigned( Result )) do
      if ( Result.Align = Align ) then
        break
      else
        begin
          Inc( n, incr );
          Result:= ParentCtrl[ n ];
        end;
  end;

var
  idx: Integer;
begin
  idx:= ParentCtrl.IndexOf( Self );

  Ctrl1:= nil;
  Ctrl2:= nil;
  if ( Align in [ alLeft, alTop ]) then
    begin
      Ctrl1:= FindNext( idx, -1 );
      Ctrl2:= FindNext( idx,  1 );
    end
  else if ( Align in [ alRight, alBottom ]) then
    begin
      Ctrl1:= FindNext( idx,  1 );
      Ctrl2:= FindNext( idx, -1 );
    end;
end;

procedure TP3DSplitter.SetAlign(const AValue: TP3DControlAlign);
begin
  if ( AValue = alClient ) then
    exit;
  inherited SetAlign(AValue);
  if ( Align in [ alLeft, alRight ]) then
    Cursor:= curHResize
  else if ( Align in [ alTop, alBottom ]) then
    Cursor:= curVResize;
end;

{$ENDIF}
 
