//p3dgui_buttons.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

  type

    { TP3DButtonPreset }

    TP3DButtonPreset = class
      private
        FColor: TP3DColorPaletteMaterialDesign;
        FFontColor: TP3DColorRGBA;

      public
        procedure Assign( Preset: TP3DButtonPreset );

        property Color: TP3DColorPaletteMaterialDesign read FColor write FColor;
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
        procedure RenderButton( C1, C2, C3, C4, CE1, CE2, CE3, CE4: TP3DColorRGBA );//APreset: TP3DButtonPreset );
        function RenderButtonText(C: TP3DColorRGBA): TVec2; inline;
        function RenderButtonText( R: TP3DRect; C: TP3DColorRGBA ): TVec2;
        procedure FontChange( ASender: TObject );

      public
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
        destructor Destroy; override;

        procedure Draw; override;
        procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Float ); override;
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
        procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Float ); override;

        property Down: Boolean read FDown write SetDown;
        property Group: String read FGroup write SetGroup;
    end;

    { TP3DSplitter }

    TP3DSplitter = class ( TP3DButton )
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      procedure MouseMove(X, Y: Float); override;
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

procedure TP3DButton.RenderButton(C1, C2, C3, C4, CE1, CE2, CE3, CE4: TP3DColorRGBA);
var
  clf: TVec4;
begin
  Canvas.RenderRect( vec2( 0 ), ClippedRect.WidthHeight +  - 1, C1, C2, C3, C4 );
  Canvas.RenderLineRect( vec2( 0 ), ClippedRect.WidthHeight - 1, CE1, CE2, CE3, CE4 );

  if ( Focused ) then
    begin
      clf:= vec4( 0.5, 0.5, 0.5, 0.5 );
      Canvas.RenderLineRect( vec2( 4 ), ClippedRect.WidthHeight - 4, clf, clf, clf, clf );
    end;
end;

function TP3DButton.RenderButtonText(C: TP3DColorRGBA): TVec2;
begin
  {Result:= }RenderButtonText( P3DRect( Margin.XY, ClippedRect.WidthHeight - Margin.ZW ), C ); //Internal Error?
end;

function TP3DButton.RenderButtonText(R: TP3DRect; C: TP3DColorRGBA): TVec2;
var
  P: TVec2;
begin
  if ( Caption > '' ) then
    begin
      if ( not Assigned( FCaptionTxt )) then
        FCaptionTxt:= TP3DTextBmp.CreateSimple( Caption, P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size );

      P:= Canvas.CalcAlignedTextRect( FCaptionTxt, R, HAlignment, VAlignment );

      Canvas.Font.Color:= C.Fade( 0.37 + 0.73 * Ord( Enabled ));
      Canvas.Font.Size:= Font.Size;
      Canvas.Font.Name:= Font.Name;
      Canvas.RenderText( Caption, P, FCaptionTxt );
      Result:= P;
    end
  else
    Result:= vec2( 0 );
end;

procedure TP3DButton.FontChange(ASender: TObject);
begin
  FCaptionTxt.Free;
  FCaptionTxt:= TP3DTextBmp.CreateSimple( Caption, P3DFontManagerBmp.Fonts[ Font.Name ], Font.Size );
end;


procedure TP3DButton.Draw;
begin
  if (( Enabled ) and
    ( gcisMouseBtn1Down in InputState )) then
    with ( PresetDown ) do
      begin
        RenderButton( Color[ cp900 ], Color[ cp400 ], Color[ cp600 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
        RenderButtonText( FontColor );
      end
  else if (( Enabled ) and
    ( gcisMouseOver in InputState )) then
    with ( PresetHover ) do
      begin
        RenderButton( Color[ cp100 ], vec4( 1 ), Color[ cp50 ], Color[ cp100 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
        RenderButtonText( FontColor );
      end
  else
    with ( PresetNormal ) do
      begin
        RenderButton( Color[ cp200 ], Color[ cp100 ], Color[ cp200 ], Color[ cp200 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
        if ( Enabled ) then
          RenderButtonText( FontColor )
        else
          RenderButtonText( FontColor.fade( 0.37 ))
      end
end;

procedure TP3DButton.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float);
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


constructor TP3DButton.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  PresetDown:= TP3DButtonPreset.Create;
  PresetHover:= TP3DButtonPreset.Create;
  PresetNormal:= TP3DButtonPreset.Create;
  Font:= TP3DCanvasFont.Create;
  Font.OnChange:= @FontChange;
  Caption:= Name;
  Margin:= vec4( 5 );

  PresetNormal.Color:= P3DThemeDefault.Accent;
  PresetNormal.FontColor:= P3DThemeDefault.Accent[ cp900 ];
  PresetHover.Color:= P3DThemeDefault.Accent;
  PresetHover.FontColor:= P3DThemeDefault.Accent[ cp700 ];
  PresetDown.Color:= P3DThemeDefault.Accent;
  PresetDown.FontColor:= P3DThemeDefault.Accent[ cp50 ];
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
    with ( PresetHover ) do
      begin
        RenderButton( Color[ cp100 ], vec4( 1 ), Color[ cp50 ], Color[ cp100 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
        RenderButtonText( FontColor );
      end
  else if (( Enabled ) and ( Down )) then
    with ( PresetDown ) do
      begin
        RenderButton( Color[ cp900 ], Color[ cp400 ], Color[ cp600 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
        RenderButtonText( FontColor );
      end
  else
    with ( PresetNormal ) do
      begin
        RenderButton( Color[ cp200 ], Color[ cp100 ], Color[ cp200 ], Color[ cp200 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
        RenderButtonText( FontColor );
      end;
end;

procedure TP3DGroupButton.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    Down:= True;
end;


{ TP3DSlider }

constructor TP3DSplitter.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  WidthHeight:= vec2( 5 );
  Caption:= '.';
  Cursor:= curHResize;
end;

procedure TP3DSplitter.MouseMove(X, Y: Float);
var
  Ctrl1, Ctrl2: TP3DGraphicControl;
  vMax: TVec2;

  function Subtract( var s: Float; Value: Float ): Float;
  begin
    Result:= Value;
    s:= s - Value;;
  end;

begin
  inherited MouseMove(X, Y);

  if ( not ( gcisMouseBtn1Down in InputState )) then
    exit;

  GetControls( Ctrl1, Ctrl2 );
  if ( not Assigned( Ctrl1 ) or not Assigned( Ctrl2 )) then
    exit;
  vMax:= Ctrl1.ClippedRect.TopLeft + Ctrl2.ClippedRect.BottomRight;//ParentCtrl.PageSize;

  //if ( Assigned( Ctrl1 )) then
  P3DApplication.MainWindow.Title:= Ctrl1.Name + ' <--> ' + Ctrl2.Name + ' [' + vMax.ToString() + ']';
  if ( Ctrl1.Align <> alClient ) then
    begin
      if ( Align in [ alLeft, alRight ]) then
        Ctrl1.Width:= Subtract( vMax.Ptr[ 0 ], {clamp( }UIViewer.MouseX - Ctrl1.ClippedRect.Left{, 1, vMax.X - 1 )})
      else if ( Align in [ alTop, alBottom ]) then
        Ctrl1.Height:= Subtract( vMax.Ptr[ 1 ], {clamp( }UIViewer.MouseY - Ctrl1.ClippedRect.Top{, 1, vMax.Y - 1 )});
    end;
  //if ( Assigned( Ctrl2 )) then
  if ( Ctrl2.Align <> alClient ) then
    begin
      if ( Align in [ alLeft, alRight ]) then
        Ctrl2.Width:= {vMax.X }Ctrl2.ClippedRect.Right - UIViewer.MouseX
      else if ( Align in [ alTop, alBottom ]) then
        Ctrl2.Height:= {vMax.Y;} Ctrl2.ClippedRect.Bottom - UIViewer.MouseY;
    end;
end;

procedure TP3DSplitter.GetControls(out Ctrl1, Ctrl2: TP3DGraphicControl);
type TAlignSet = set of TP3DControlAlign;

  function FindNext( idx: Integer; incr: Integer; Allowed: TAlignSet ): TP3DGraphicControl;
  var
    n: Integer;
  begin
    n:= idx + incr;
    Result:= ParentCtrl[ n ];
    while ( Assigned( Result )) do
      if ( Result.Align in Allowed ) then
        break
      else
        begin
          Inc( n, incr );
          Result:= ParentCtrl[ n ];
        end;
  end;

var
  idx, n1, n2: Integer;
  Allowed: TAlignSet;
begin
  idx:= ParentCtrl.IndexOf( Self );

  Ctrl1:= nil;
  Ctrl2:= nil;
  if ( Align in [ alLeft, alTop ]) then begin
    n1:= -1;
    n2:= 1;
  end else if ( Align in [ alRight, alBottom ]) then begin
    n1:= 1;
    n2:= -1;
  end;

  Ctrl1:= FindNext( idx, n1, [ Align, alClient ]);
  if ( Assigned( Ctrl1 )) then
    if ( Ctrl1.Align = alClient ) then
      Allowed:= [ alLeft, alTop, alRight, alBottom ]
    else
      Allowed:= [ Ctrl1.Align, alClient ]
  else
    Allowed:= [ Align ];

  Ctrl2:= FindNext( idx,  n2, Allowed );
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
 
