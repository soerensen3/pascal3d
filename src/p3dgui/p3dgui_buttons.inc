//p3dgui_buttons.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

  type

    { TP3DButtonPreset }

    TP3DButtonPreset = class
      private
        FColor: TVec4;
        FFontColor: TVec4;
        FOutlineColor: TVec4;

      public
        procedure Assign( Preset: TP3DButtonPreset );

        property Color: TVec4 read FColor write FColor;
        property OutlineColor: TVec4 read FOutlineColor write FOutlineColor;
        property FontColor: TVec4 read FFontColor write FFontColor;
    end;

    THorizontalAlignment = (
        haLeft,
        haCenter,
        haRight);

      TVerticalAlignment = (
        vaTop,
        vaCenter,
        vaBottom);

    { TP3DButton }

    TP3DButton = class( TP3DGraphicControl )
      private
        FFont: TP3DCanvasFont;
        FHAlignment: THorizontalAlignment;
        FVAlignment: TVerticalAlignment;

        function GetTextHeight: Single;
        function GetTextWidth: Single;

      protected
        FCaption: String;
        FPresetDown: TP3DButtonPreset;
        FPresetHover: TP3DButtonPreset;
        FPresetNormal: TP3DButtonPreset;
        FState: Integer;
        FCaptionTxt: TP3DText;

        procedure SetCaption( AValue: String ); virtual;

      public
        constructor Create(AOwner: TP3DObjectList; const AParent: TP3DGraphicControl=nil);
        destructor Destroy; override;

        procedure Draw; override;
        procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;

        class function IsFocusControl: Boolean; override;

        property PresetNormal: TP3DButtonPreset read FPresetNormal write FPresetNormal;
        property PresetDown: TP3DButtonPreset read FPresetDown write FPresetDown;
        property PresetHover: TP3DButtonPreset read FPresetHover write FPresetHover;
        property Font: TP3DCanvasFont read FFont write FFont;
        property Caption: String read FCaption write SetCaption;
        property HAlignment: THorizontalAlignment read FHAlignment write FHAlignment;
        property VAlignment: TVerticalAlignment read FVAlignment write FVAlignment;
        property TextWidth: Single read GetTextWidth;
        property TextHeight: Single read GetTextHeight;
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
    FCaptionTxt:= p3dTextSimple( AValue, P3DFontManager[ Font.Name, Font.Size ])
  else
    FCaptionTxt:= p3dTextSimple( ' ', P3DFontManager[ Font.Name, Font.Size ])
end;


procedure TP3DButton.Draw;
  function Brightness( c: TVec4; br: Single ): TVec4;
  begin
    Result:= clamp( c + br, 0, 1 );
  end;

var
  Preset: TP3DButtonPreset;
  clf: TVec4;
  P: TVec2;
begin
  if ( gcisMouseBtn1Down in InputState ) then
    Preset:= PresetDown
  else if ( gcisMouseOver in InputState ) then
    Preset:= PresetHover
  else
    Preset:= PresetNormal;

  Canvas.RenderRect( vec2( 0 ), vec2( Width, Height ) - 1, Preset.Color, Preset.Color, Preset.Color, Preset.Color );
  Canvas.RenderLineRect( vec2( 0 ), vec2( Width, Height ) - 1, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor, Preset.OutlineColor );


  if ( Focused ) then
    begin
      clf:= vec4( 0.5, 0.5, 0.5, 0.5 );
      Canvas.RenderLineRect( vec2( 4 ), vec2( Width, Height ) - 4, clf, clf, clf, clf );
    end;

  if ( Assigned( FCaptionTxt )) then
    begin
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
      Canvas.Font.Color:= Preset.FontColor;
      Canvas.Font.Size:= Font.Size;
      Canvas.Font.Name:= Font.Name;
      Canvas.RenderText( FCaptionTxt, P );
    end;
end;

procedure TP3DButton.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseDown( mb1, mb2, mb3, X, Y );
  if ( gcisMouseOver in InputState ) then
    Focused:= True;
end;

class function TP3DButton.IsFocusControl: Boolean;
begin
  Result:= True;
end;


constructor TP3DButton.Create(AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AOwner, AParent );
  PresetDown:= TP3DButtonPreset.Create;
  PresetHover:= TP3DButtonPreset.Create;
  PresetNormal:= TP3DButtonPreset.Create;
  Font:= TP3DCanvasFont.Create;
  Caption:= Name;

  PresetNormal.Color:= vec4( vec3( 0.93 ), 1 );
  PresetNormal.OutlineColor:= vec4( vec3( 0.4 ), 1 );
  PresetNormal.FontColor:= vec4( vec3( 0 ), 1 );
  PresetHover.Color:= vec4( vec3( 0 ), 1 );
  PresetHover.OutlineColor:= vec4( 0.875, 0.27, 0.07, 1 );
  PresetHover.FontColor:= vec4( 1 );
  PresetDown.Color:= vec4( 1 );
  PresetDown.OutlineColor:= vec4( 0.875, 0.27, 0.07, 1 );
  PresetDown.FontColor:= vec4( vec3( 0 ), 1 );
  HAlignment:= haCenter;
  VAlignment:= vaCenter;
end;

destructor TP3DButton.Destroy;
begin
  FCaptionTxt.Free;
  Font.Free;
  inherited Destroy;
end;



{$ENDIF}
 
