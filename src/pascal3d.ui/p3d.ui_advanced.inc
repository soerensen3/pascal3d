//p3dgui_sceneviewer.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}
type

  { TP3DColorRGBAWheel }

  TP3DColorRGBAWheel = class ( TP3DGraphicControl )
    private
      FBorderColor: TP3DColorRGBA;
      FHSV: TVec3;
      FOnChange: TP3DNotifyEvent;
      FSelXY: TVec2;

      procedure DrawCircleHSV;
      function GetPickColor: TVec3;
      procedure SetHSV(AValue: TVec3);
      procedure SetPickColor( AValue: TVec3 );
      procedure CalcHSVFromMousePos(X, Y: Float);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Draw; override;
      procedure MouseMove(X, Y: Float); override;
      procedure MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float); override;

      property BorderColor: TP3DColorRGBA read FBorderColor write FBorderColor;
      property RGB: TVec3 read GetPickColor write SetPickColor;
      property HSV: TVec3 read FHSV write SetHSV;
      property OnChange: TP3DNotifyEvent read FOnChange write FOnChange;
  end;

  { TP3DColorRGBAValue }

  TP3DColorRGBAValue = class ( TP3DGraphicControl )
    private
      FBorderColor: TVec4;
      FOnChange: TP3DNotifyEvent;
      FValue: Float;

      procedure SetValue(AValue: Float);
      procedure CalcValueFromMousePos(Y: Float);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      procedure Draw; override;
      procedure MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float); override;
      procedure MouseMove(X, Y: Float); override;

      property BorderColor: TVec4 read FBorderColor write FBorderColor;
      property Value: Float read FValue write SetValue;
      property OnChange: TP3DNotifyEvent read FOnChange write FOnChange;
  end;

  { TP3DColorRGBAPicker }

  TP3DColorRGBAPicker = class ( TP3DGraphicControl )
    private type
      TGetValue = function ( Sender: TP3DColorRGBAPicker ): TP3DColorRGBA of object;
      TSetValue = procedure ( Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA ) of object;

    private
      FBGColor: TP3DColorRGBA;
      FBorderColor: TP3DColorRGBA;
      FColorEdit: TP3DStreamablePointer;
      FColorValue: TP3DStreamablePointer;
      FColorWheel: TP3DStreamablePointer;
      FGetValueEvent: TGetValue;
      FSetValueEvent: TSetValue;

      procedure ColorChange( Sender: TP3DGraphicControl );
      function GetColor: TP3DColorRGBA;
      function GetColorEdit: TP3DVectorEdit;
      function GetColorValue: TP3DColorRGBAValue;
      function GetColorWheel: TP3DColorRGBAWheel;
      function GetEditCount: Integer;
      function GetEditValue( Sender: TP3DVectorEdit ): TVec4;
      procedure SetColor(AValue: TP3DColorRGBA);
      procedure SetColorEdit(AValue: TP3DVectorEdit);
      procedure SetColorValue(AValue: TP3DColorRGBAValue);
      procedure SetColorWheel(AValue: TP3DColorRGBAWheel);
      procedure SetEditCount(AValue: Integer);
      procedure SetEditValue( Sender: TP3DVectorEdit; AValue: TVec4 );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure UpdateColor;
      procedure Draw; override;
      class function HasShadow: Boolean; override;

      property ColorWheel: TP3DColorRGBAWheel read GetColorWheel write SetColorWheel;
      property ColorValue: TP3DColorRGBAValue read GetColorValue write SetColorValue;
      property ColorEdit: TP3DVectorEdit read GetColorEdit write SetColorEdit;
      property GetValueEvent: TGetValue read FGetValueEvent write FGetValueEvent;
      property SetValueEvent: TSetValue read FSetValueEvent write FSetValueEvent;
      property Color: TP3DColorRGBA read GetColor write SetColor;
      property BGColor: TP3DColorRGBA read FBGColor write FBGColor;
      property BorderColor: TP3DColorRGBA read FBorderColor write FBorderColor;
      property EditCount: Integer read GetEditCount write SetEditCount;
  end;

  { TP3DColorRGBAComboBox }

  TP3DColorRGBAComboBox = class ( TP3DGraphicControl )
    private type
      TGetValue = function ( Sender: TP3DColorRGBAPicker ): TP3DColorRGBA of object;
      TSetValue = procedure ( Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA ) of object;

    private
      FBorderColor: TP3DColorRGBA;
      FColorPicker: TP3DStreamablePointer;
      function GetColorPicker: TP3DColorRGBAPicker;
      function GetValue: TP3DColorRGBA;
      function Get_GetValueEvent: TGetValue;
      function Get_SetValueEvent: TSetValue;
      procedure SetColorPicker(AValue: TP3DColorRGBAPicker);
      procedure SetValue(AValue: TP3DColorRGBA);
      procedure Set_GetValueEvent(AValue: TGetValue);
      procedure Set_SetValueEvent(AValue: TSetValue);
      //FOnChange: TP3DComboBoxChange;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure DropDown;
      procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Float ); override;
      procedure MouseAction(X, Y: Float; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean); override;

      property ColorPicker: TP3DColorRGBAPicker read GetColorPicker write SetColorPicker;
      property GetValueEvent: TGetValue read Get_GetValueEvent write Set_GetValueEvent;
      property SetValueEvent: TSetValue read Get_SetValueEvent write Set_SetValueEvent;
      property BorderColor: TP3DColorRGBA read FBorderColor write FBorderColor;
      property Value: TP3DColorRGBA read GetValue write SetValue;

    published
      //property OnChange: TP3DComboBoxChange read FOnChange write FOnChange;
  end;

  { TP3DImage }

  //Unfinished

  TP3DImage = class ( TP3DGraphicControl )
    private
      FAutoSize: Boolean;
      FColor: TP3DColorRGBA;
      FKeepAspect: Boolean;
      FMaterial: TP3DStreamablePointer;
      FTexture: TP3DStreamablePointer;
      function GetMaterial: TP3DMaterial;
      function GetTexture: TP3DTexture;
      procedure SetMaterial(AValue: TP3DMaterial);
      procedure SetTexture(AValue: TP3DTexture); virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;
      procedure Draw; override;

      property Color: TP3DColorRGBA read FColor write FColor;

    published
      property Texture: TP3DTexture read GetTexture write SetTexture;
      property AutoSize: Boolean read FAutoSize write FAutoSize;
      property KeepAspect: Boolean read FKeepAspect write FKeepAspect;
      property Material: TP3DMaterial read GetMaterial write SetMaterial;
  end;

  { TP3DTrackBar }

  TP3DTrackBar = class( TP3DScrollBar )
    private
      FLabel: TP3DTextBmp;

    public
      procedure Draw; override;
      destructor Destroy; override;
  end;

  { TP3DTexViewer }

  TP3DTexViewer = class ( TP3DImage )
    protected
      FMipSlider: TP3DStreamablePointer;
      function GetMipSlider: TP3DTrackBar;
      procedure SetTexture(AValue: TP3DTexture); override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      procedure Draw; override;

      property MipSlider: TP3DTrackBar read GetMipSlider;
  end;

  { TP3DFrame }

  TP3DFrame = class ( TP3DGraphicControl )
    private
      FPosition: TVec2;
      FUIViewerFrame: TP3DStreamableContainer;
      FZoom: Float;

      function GetUIViewerFrame: TP3DUIViewerBase;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      procedure Paint(); override;
      procedure Logic(); override;

      procedure KeyboardAction(); override;
      procedure MouseAction(X, Y: Float; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean); override;
      function ChildrenMouseRay(X, Y: Float; const Recurse: Boolean=True): TP3DGraphicControl; override;

      property Zoom: Float read FZoom write FZoom;
      property Position: TVec2 read FPosition write FPosition;

    published
      property UIViewerFrame: TP3DUIViewerBase read GetUIViewerFrame;
  end;

  { TP3DCheckList }

  TP3DCheckList = class ( TP3DListView )
    protected type
      TOnSetChecked = procedure ( AItem: TP3DListItem; AItemIndex: Integer; AChecked: Boolean ) of object;
    protected
      FHAlignment: TP3DHorizontalAlignment;
      FVAlignment: TP3DVerticalAlignment;
      FSymbolChecked: TP3DTextBmp;
      FOnSetChecked: TOnSetChecked;

      function GetChecked( Index: Integer ): Boolean;
      procedure SetChecked( Index: Integer ; AValue: Boolean);

      property PresetSelection;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      function DrawListItem(p: TVec2; Node: TP3DListItem): TVec2; override;
      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float); override;

      property Checked[ Index: Integer ]: Boolean read GetChecked write SetChecked;
      property PresetHover: TP3DButtonPreset read FPresetSelection write FPresetSelection;
      property PresetDown: TP3DButtonPreset read FPresetDrag write FPresetDrag;
      property HAlignment: TP3DHorizontalAlignment read FHAlignment write FHAlignment;
      property VAlignment: TP3DVerticalAlignment read FVAlignment write FVAlignment;
      property OnSetChecked: TOnSetChecked read FOnSetChecked write FOnSetChecked;
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{
Original source for color conversion by
William Egge, public@eggcentric.com
http://www.eggcentric.com
}

function HSVToRGB( HSV: TVec3 ): TVec3;
const
  SectionSize = 60/360;
var
  Section: Single;
  SectionIndex: Integer;
  f: single;
  p, q, t: Single;
begin
  if ( HSV[ 0 ] < 0 ) then
    Result:= vec3( HSV[ 2 ])
  else
    begin
      Section:= HSV[ 0 ] / SectionSize;
      SectionIndex:= Floor( Section );
      f:= Section - SectionIndex;
      p:= HSV[ 2 ] * ( 1 - HSV[ 1 ]);
      q:= HSV[ 2 ] * ( 1 - HSV[ 1 ] * f );
      t:= HSV[ 2 ] * ( 1 - HSV[ 1 ] * ( 1 - f ) );
      case SectionIndex of
        0: Result:= vec3( HSV[ 2 ], t, p );
        1: Result:= vec3( q, HSV[ 2 ], p );
        2: Result:= vec3( p, HSV[ 2 ], t );
        3: Result:= vec3( p, q, HSV[ 2 ]);
        4: Result:= vec3( t, p, HSV[ 2 ]);
        else Result:= vec3( HSV[ 2 ], p, q );
      end;
    end;
end;

function RGBToHSV( RGB: TVec3 ): TVec3;
var
  MinIndex, MaxIndex: Integer;
  Range: Single;
begin
  MinIndex:= 0;
  if ( RGB.G < RGB.R  ) then
    MinIndex:= 1;

  if ( RGB.B < RGB[ MinIndex ]) then
    MinIndex:= 2;

  MaxIndex:= 0;
  if ( RGB.G > RGB.R ) then
    MaxIndex:= 1;

  if ( RGB.B > RGB[ MaxIndex ]) then
    MaxIndex:= 2;

  Range:= RGB[ MaxIndex ] - RGB[ MinIndex ];

  // Check for a gray level
  if ( Range = 0 ) then
    Result:= vec3(
      -1, // Can't determine on greys, so set to -1
      0, // Gray is at the center;
      RGB.R ) // could choose R, G, or B because they are all the same value.
  else
    begin
      case MaxIndex of
        0: Result[ 0 ]:= ( RGB.G - RGB.B ) / Range;
        1: Result[ 0 ]:= 2 + ( RGB.B - RGB.R ) / Range;
        2: Result[ 0 ]:= 4 + ( RGB.R - RGB.G ) / Range;
      end;
      Result[ 1 ]:= Range/RGB[MaxIndex];
      Result[ 2 ]:= RGB[MaxIndex];
      Result[ 0 ]:= Result[ 0 ] * ( 1 / 6 );
      if ( Result[ 0 ] < 0 ) then
        Result[ 0 ]:= 1 + Result[ 0 ];
    end;
end;



{ TP3DColorRGBAWheel }

procedure TP3DColorRGBAWheel.DrawCircleHSV;
const
  num_segments = 32;
var
  theta, co, si, t: Single;
  p, c: TVec2;
  i: Integer;
  r: Float;
  Color: TVec3;
begin
  if ( not Canvas.PrepareForDrawing( Canvas.Material )) then exit;

  c:= vec2( ClientRect.Width / 2, ClientRect.Height / 2 );
  r:= c[ 1 - c.GetBiggestComp ];

  theta:= 2 * PI / num_segments;
  co:= cos( theta );//precalculate the sine and cosine
  si:= sin( theta );

  p:= vec2( 0, r );//we start at angle = 0

  glBegin(GL_TRIANGLE_FAN);
  Color:= HSVToRGB( vec3( 0, 0, HSV[ 2 ]));
  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, 1 );
  glVertexAttrib2f( P3DAttribPosition, c.x, c.y );//start with center
  for i:= 0 to num_segments do
    begin
      Color:= HSVToRGB( vec3( i / num_segments, 1.0, HSV[ 2 ]));
      glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, 1 ); //cle( Self );
      glVertexAttrib2f( P3DAttribPosition, p.x + c.x, p.y + c.y);//output vertex

		//apply the rotation matrix
      t:= p.x;
      p:= vec2( co * p.x - si * p.y, si * t + co * p.y );
    end;
  glEnd(); //cle( Self );
end;

function TP3DColorRGBAWheel.GetPickColor: TVec3;
begin
  Result:= HSVToRGB( HSV );
end;

procedure TP3DColorRGBAWheel.SetHSV(AValue: TVec3);
var
  Angle: Float;
begin
  if FHSV= AValue then Exit;
  FHSV:= AValue;
  Angle:= 2 * PI * HSV[ 0 ];
  FSelXY:= vec2( -sin( Angle ), cos( Angle )) * HSV[ 1 ];
end;

procedure TP3DColorRGBAWheel.SetPickColor(AValue: TVec3);
begin
  HSV:= RGBToHSV( AValue );
end;

procedure TP3DColorRGBAWheel.CalcHSVFromMousePos(X, Y: Float);
var
  c: TVec2;
  r: Float;
  H: Float;
begin
  c:= vec2( ClientRect.Width / 2, ClientRect.Height / 2 );
  r:= c[ 1 - c.GetBiggestComp ];
  FSelXY:= ( vec2( X, Y ) - c ) / r;
  if ( FSelXY.length > 1 ) then
    FSelXY.Normalize;

  if ( FSelXY.length < epsilon ) then
    H:= 0
  else
    H:= 0.5 + arctan2( FSelXY.x, -FSelXY.y ) / ( 2 * PI );
  //Calculate the Angle and convert it to a H value where 0,+1 is located at the bottom
  HSV:= vec3( H, FSelXY.length, FHSV[ 2 ]);
  //WriteLn( 'HSV = <', HSV.ToString(), '> RGB = <', RGB.ToString(), '>' );
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

constructor TP3DColorRGBAWheel.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  BorderColor:= vec4( 0, 0, 0, 0.2 );
  HSV:= vec3( 0, 0, 1 );
end;

procedure TP3DColorRGBAWheel.Draw;
var
  c: TVec2;
  r: Float;
begin
  c:= vec2( ClientRect.Width / 2, ClientRect.Height / 2 );
  r:= c[ 1 - c.GetBiggestComp ];
  DrawCircleHSV;
  Canvas.RenderLineCircle( c, r, 32, BorderColor );
  c:= FSelXY * r + c;
  Canvas.RenderCircle( c, 5, 16, vec4( GetPickColor, 1 ));
  Canvas.RenderLineCircle( c, 5, 16, BorderColor );
end;

procedure TP3DColorRGBAWheel.MouseMove(X, Y: Float);
begin
  inherited MouseMove( X, Y );

  if ( gcisMouseBtn1Down in InputState ) then
    CalcHSVFromMousePos( X, Y );
end;

procedure TP3DColorRGBAWheel.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    CalcHSVFromMousePos( X, Y );
end;

{ TP3DColorRGBAValue }

procedure TP3DColorRGBAValue.SetValue(AValue: Float);
begin
  if FValue= AValue then Exit;
  FValue:= Max( 0, Min( 1, AValue ));
end;

procedure TP3DColorRGBAValue.CalcValueFromMousePos(Y: Float);
begin
  Value:= 1 - ( Y - ClientRect.Top ) / ClientRect.Height;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

constructor TP3DColorRGBAValue.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  BorderColor:= vec4( 0, 0, 0, 0.2 );
  Value:= 1.0;
end;

procedure TP3DColorRGBAValue.Draw;
var
  black, white: TVec4;
  c: TVec2;
begin
  inherited Draw;
  black:= vec4( 0, 0, 0, 1 );
  white:= vec4( 1 );
  Canvas.RenderRect( ClientRect.TopLeft, ClientRect.BottomRight, white, white, black, black );
  c:= vec2( Margin.x + ClientRect.Width / 2, Margin.y + ( 1 - Value ) * ClientRect.Height );
  Canvas.RenderCircle( c, 5, 16, vec4( vec3( Value ), 1 ));
  Canvas.RenderLineCircle( c, 5, 16, BorderColor );
end;

procedure TP3DColorRGBAValue.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    CalcValueFromMousePos( Y );
end;

procedure TP3DColorRGBAValue.MouseMove(X, Y: Float);
begin
  inherited MouseMove(X, Y);
  if ( gcisMouseBtn1Down in InputState ) then
    CalcValueFromMousePos( Y );
end;

{ TP3DColorRGBAPicker }

procedure TP3DColorRGBAPicker.ColorChange(Sender: TP3DGraphicControl);
begin
  if (( Sender = ColorValue ) and Assigned( SetValueEvent )) then
    begin
      ColorWheel.HSV:= vec3( ColorWheel.HSV[ 0 ], ColorWheel.HSV[ 1 ], ColorValue.Value );
        SetValueEvent( Self, vec4( ColorWheel.RGB, Color.A ));
    end;
  if (( Sender = ColorWheel ) and Assigned( SetValueEvent )) then
    SetValueEvent( Self, vec4( ColorWheel.RGB, Color.A ));
end;

function TP3DColorRGBAPicker.GetColor: TP3DColorRGBA;
begin
  Result:= ColorEdit.Value;
end;

function TP3DColorRGBAPicker.GetColorEdit: TP3DVectorEdit;
begin
  Result:= TP3DVectorEdit( FColorEdit.AsValue );
end;

function TP3DColorRGBAPicker.GetColorValue: TP3DColorRGBAValue;
begin
  Result:= TP3DColorRGBAValue( FColorValue.AsValue );
end;

function TP3DColorRGBAPicker.GetColorWheel: TP3DColorRGBAWheel;
begin
  Result:= TP3DColorRGBAWheel( FColorWheel.AsValue );
end;

function TP3DColorRGBAPicker.GetEditCount: Integer;
begin
  Result:= ColorEdit.EdCount;
end;


function TP3DColorRGBAPicker.GetEditValue(Sender: TP3DVectorEdit): TVec4;
begin
  if ( Assigned( GetValueEvent )) then
    Result:= GetValueEvent( Self )
  else
    Result:= vec4( ColorWheel.RGB, 1 );
end;

procedure TP3DColorRGBAPicker.SetColor( AValue: TP3DColorRGBA );
begin
  ColorWheel.RGB:= AValue.XYZ;
  ColorValue.FValue:= ColorWheel.HSV[ 2 ];
  if ( Assigned( SetValueEvent )) then
    SetValueEvent( Self, AValue );
end;

procedure TP3DColorRGBAPicker.SetColorEdit(AValue: TP3DVectorEdit);
begin
  FColorEdit.AsValue:= AValue;
end;

procedure TP3DColorRGBAPicker.SetColorValue(AValue: TP3DColorRGBAValue);
begin
  FColorValue.AsValue:= AValue;
end;

procedure TP3DColorRGBAPicker.SetColorWheel(AValue: TP3DColorRGBAWheel);
begin
  FColorWheel.AsValue:= AValue;
end;

procedure TP3DColorRGBAPicker.SetEditCount(AValue: Integer);
begin
  ColorEdit.EdCount:= AValue;
end;

procedure TP3DColorRGBAPicker.SetEditValue(Sender: TP3DVectorEdit; AValue: TVec4);
begin
  Color:= clamp( AValue, 0.0, 1.0 );
end;

class function TP3DColorRGBAPicker.HasShadow: Boolean;
begin
  Result:= True;
end;

constructor TP3DColorRGBAPicker.Create(const AOwner: IP3DStreamableContainer);
var
  container: TP3DGraphicControl;
begin
  inherited Create( AOwner );
  WidthHeight:= vec2( 200, 150 );
  BorderColor:= vec4( 0, 0, 0, 0.2 );
  BGColor:= vec4( 1 );
  container:= TP3DGraphicControl.Create( Self.Controls );
  container.Align:= alClient;

  FColorValue:= TP3DStreamablePointer.Create( 'ColorValue', TP3DColorRGBAValue );
  Properties.Add( FColorValue );
  ColorValue:= TP3DColorRGBAValue.Create( container.Controls );
  ColorValue.Align:= alRight;
  ColorValue.Margin:= vec4( 15 );
  ColorValue.OnChange:= @ColorChange;

  FColorWheel:= TP3DStreamablePointer.Create( 'ColorWheel', TP3DColorRGBAWheel );
  Properties.Add( FColorWheel );
  ColorWheel:= TP3DColorRGBAWheel.Create( container.Controls );
  ColorWheel.Align:= alClient;
  ColorWheel.OnChange:= @ColorChange;

  FColorEdit:= TP3DStreamablePointer.Create( 'ColorEdit', TP3DVectorEdit );
  Properties.Add( FColorEdit );
  ColorEdit:= TP3DVectorEdit.Create( Self.Controls );
  ColorEdit.Labels[ 0 ]:= 'R';
  ColorEdit.Labels[ 1 ]:= 'G';
  ColorEdit.Labels[ 2 ]:= 'B';
  ColorEdit.Labels[ 3 ]:= 'A';
  //ColorEdit.EdCount:= 4;
  ColorEdit.Align:= alBottom;
  ColorEdit.GetValueEvent:= @GetEditValue;
  ColorEdit.SetValueEvent:= @SetEditValue;
  ColorEdit.Height:= 20;
end;


procedure TP3DColorRGBAPicker.UpdateColor;
var
  c: TP3DColorRGBA;
begin
  c:= Color;
  if ( c.RGB <> ColorWheel.RGB ) then
    ColorWheel.RGB:= c.XYZ;
  if ( ColorWheel.HSV[ 2 ] <> ColorValue.Value ) then
    ColorValue.FValue:= ColorWheel.HSV[ 2 ];
end;

procedure TP3DColorRGBAPicker.Draw;
begin
  UpdateColor;

  Canvas.RenderRect( ClientRect.TopLeft, ClientRect.BottomRight, BGColor );
  Canvas.RenderLineRect( ClientRect.TopLeft, ClientRect.BottomRight, BorderColor );
end;

{ TP3DColorRGBAComboBox }

function TP3DColorRGBAComboBox.GetValue: TP3DColorRGBA;
begin
  Result:= ColorPicker.Color;
end;

function TP3DColorRGBAComboBox.Get_GetValueEvent: TGetValue;
begin
  Result:= ColorPicker.FGetValueEvent;
end;

function TP3DColorRGBAComboBox.Get_SetValueEvent: TSetValue;
begin
  Result:= ColorPicker.FSetValueEvent;
end;

function TP3DColorRGBAComboBox.GetColorPicker: TP3DColorRGBAPicker;
begin
  Result:= TP3DColorRGBAPicker( FColorPicker.AsValue );
end;

procedure TP3DColorRGBAComboBox.SetColorPicker(AValue: TP3DColorRGBAPicker);
begin
  FColorPicker.AsValue:= AValue;
end;

procedure TP3DColorRGBAComboBox.SetValue(AValue: TP3DColorRGBA);
begin
  ColorPicker.Color:= AValue;
end;

procedure TP3DColorRGBAComboBox.Set_GetValueEvent(AValue: TGetValue);
begin
  ColorPicker.FGetValueEvent:= AValue;
end;

procedure TP3DColorRGBAComboBox.Set_SetValueEvent(AValue: TSetValue);
begin
  ColorPicker.FSetValueEvent:= AValue;
end;

constructor TP3DColorRGBAComboBox.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  BorderColor:= vec4( 0, 0, 0, 0.2 );
  FColorPicker:= TP3DStreamablePointer.Create( 'ColorPicker', TP3DColorRGBAPicker );
  Properties.Add( FColorPicker );
  ColorPicker:= TP3DColorRGBAPicker.Create( UIViewer.Controls );
  ColorPicker.Visible:= False;
end;

destructor TP3DColorRGBAComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DColorRGBAComboBox.Draw;
begin
  Canvas.RenderRect( ClientRect.TopLeft, ClientRect.BottomRight, ColorPicker.Color );
  Canvas.RenderLineRect( ClientRect.TopLeft, ClientRect.BottomRight, BorderColor );
end;

procedure TP3DColorRGBAComboBox.DropDown;
begin
  ColorPicker.TopLeft:= min( UIViewer.ScreenCanvas.WidthHeight - ColorPicker.WidthHeight, vec2( ClippedRect.Left, ClippedRect.Top + Height ));
  ColorPicker.Visible:= True;
  ColorPicker.BringToFront;
end;

procedure TP3DColorRGBAComboBox.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    DropDown;
end;

procedure TP3DColorRGBAComboBox.MouseAction(X, Y: Float; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean);
var
  r: TP3DRect;
begin
  inherited MouseAction(X, Y, mb1, mb2, mb3, dmb1, dmb2, dmb3);
  if ( ColorPicker.Visible ) then
    begin
      r:= P3DRectEx( min( ColorPicker.ClippedRect.TopLeft, ClippedRect.TopLeft ) - 10,
                     max( ColorPicker.ClippedRect.BottomRight, ClippedRect.BottomRight ) + 10 );
      if ( not r.PtInRect( vec2( UIViewer.MouseX, UIViewer.MouseY ))) then
        ColorPicker.Visible:= False;
    end;
end;

{ TP3DImage }

procedure TP3DImage.SetTexture(AValue: TP3DTexture);
begin
  FTexture.AsValue:= AValue;
  if ( Assigned( Material )) then
    Material.Maps[ 0 ].Map:= AValue;
end;

function TP3DImage.GetMaterial: TP3DMaterial;
begin
  Result:= TP3DMaterial( FMaterial.AsValue );
end;

function TP3DImage.GetTexture: TP3DTexture;
begin
  Result:= TP3DTexture( FTexture.AsValue );
end;

procedure TP3DImage.SetMaterial(AValue: TP3DMaterial);
begin
  FMaterial.AsValue:= AValue;
end;

constructor TP3DImage.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Color:= vec4( 1 );
  KeepAspect:= True;
  FMaterial:= TP3DStreamablePointer.Create( 'Material', TP3DMaterial );
  FTexture:= TP3DStreamablePointer.Create( 'Texture', TP3DTexture );
  Properties.Add([ FTexture, FMaterial ]);
  Material:= Canvas.MaterialTexture;{TP3DMaterialBase.CreateBlock();
  Material.Unlit:= True;
  Material.UseVertexColor:= True;}
  //with( TP3DMaterialMapBase.Create( Material.Maps )) do
  //  Texture:= nil;
  //Material.BuildShader();
end;

destructor TP3DImage.Destroy;
begin
  if ( Assigned( Material ) and P3DDataBlockCache.IsValid( Material )) then
    Material.Free;
  inherited Destroy;
end;

procedure TP3DImage.Draw;
var
  c, p1, p2: TVec2;
  Shader: TP3DShader;
  Uniform, decl: TP3DShaderDecl;
  n: Integer;
  scale: Single;
begin
  if ( Assigned( Texture ) and ( Texture.Width * Texture.Height > 0 )) then
    begin
      c:= WidthHeight / 2;
      scale:= min( WidthHeight.X / Texture.Width, WidthHeight.Y / Texture.Height ) / 2;

      p1:= c - vec2( Texture.Width, Texture.Height ) * scale;
      p2:= c + vec2( Texture.Width, Texture.Height ) * scale;

      if ( AutoSize and KeepAspect ) then
        begin
          if (( p1.x < 0 ) or ( p1.y < 0 )) then
            begin

            end;
        end;
      Material.Maps[ 0 ].Map:= Texture;
      Canvas.PrepareForDrawing( Material );
      //Material.PassToShader();
//      if ( not Assigned( P3DFontManager.Material.Shader )) then
//        TP3DMaterialBase( P3DFontManager.Material ).BuildShader;
//      Shader:= P3DFontManager.Material.Shader;
//      Shader.Enable;
      Shader:= Material.Shader;

      {if ( Shader.Uniforms.FindByName( 'view', Uniform )) then
        Uniform.AsMat4:= Canvas.View;
      if ( Shader.Uniforms.FindByName( 'world', Uniform )) then
        Uniform.AsMat4:= Mat4Identity;
      if ( Shader.Uniforms.FindByName( 'proj', Uniform )) then
        Uniform.AsMat4:= Canvas.Proj;}

      Canvas.PassToShader( Material );



      if ( P3DShaderActive.Attributes.FindByName( 'TexCoord0', decl )) then
        begin
          glBegin( GL_TRIANGLE_FAN );

          glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
          decl.AsVec2:= vec2( 0, 1 );
          glVertexAttrib2f( P3DAttribPosition, p1.X, p1.Y );
          //glVertexAttrib2f( P3DAttribTexCoord0, 0, 1 );

          decl.AsVec2:= vec2( 1, 1 );
          glVertexAttrib2f( P3DAttribPosition, p2.X, p1.Y );
          //glVertexAttrib2f( P3DAttribTexCoord0, 1, 1 );

          decl.AsVec2:= vec2( 1, 0 );
          glVertexAttrib2f( P3DAttribPosition, p2.X, p2.Y );
          //glVertexAttrib2f( P3DAttribTexCoord0, 1, 0 );

          decl.AsVec2:= vec2( 0, 0 );
          glVertexAttrib2f( P3DAttribPosition, p1.X, p2.Y );
          //glVertexAttrib2f( P3DAttribTexCoord0, 0, 0 );
          glEnd();
        end;
    end;
end;

{ TP3DTrackBar }

procedure TP3DTrackBar.Draw;
begin
  inherited Draw;
  Canvas.Unlock();
  if ( gcisMouseBtn1Down in InputState ) then
    with ( P3DUI.ScreenCanvas ) do begin
      Lock;
      RenderText( round( Position ).ToString, vec2( P3DInput.Mouse.X, P3DInput.Mouse.Y ), FLabel );
      Unlock();
    end;
  Canvas.Lock;
end;

destructor TP3DTrackBar.Destroy;
begin
  FLabel.Free;
  inherited Destroy;
end;

{ TP3DTexViewer }

function TP3DTexViewer.GetMipSlider: TP3DTrackBar;
begin
  Result:= TP3DTrackBar( FMipSlider.AsValue );
end;

procedure TP3DTexViewer.SetTexture(AValue: TP3DTexture);
  function MaxMipLevel: Integer;
  begin
    Result:= Ceil( Log2( Max( Texture.Width, Texture.Height )));
  end;

begin
  inherited SetTexture(AValue);
  if ( Assigned( Texture )) then
    MipSlider.Max:= MaxMipLevel
  else
    MipSlider.Max:= 0.0;
end;

constructor TP3DTexViewer.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FMipSlider:= TP3DStreamablePointer.Create( 'MipSlider', TP3DTrackBar );
  FMipSlider.AsValue:= TP3DTrackBar.Create( Controls );
  MipSlider.Align:= alBottom;
  MipSlider.Max:= 1.0;
  Material:= Canvas.MaterialTextureManMip;
end;

procedure TP3DTexViewer.Draw;
var
  Uniform: TP3DShaderDecl;
begin
  Canvas.PrepareForDrawing( Material );

  TP3DMaterialNode( Material ).FindUniform( 'MipLevel' ).FindInput( 'Value' ).ValueProp.AsString:= FloatToStr( MipSlider.Position );

  inherited Draw;
end;

{ TP3DPanel }

constructor TP3DPanel.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Color:= P3DThemeDefault.Accent[ cp800 ];
end;

procedure TP3DPanel.Draw;
begin
  inherited Draw;
  if ( Color.a <> 0 ) then
    Canvas.RenderRect( vec2( 0 ), ClippedRect.WidthHeight, Color );
end;

{ TP3DFrame }

function TP3DFrame.GetUIViewerFrame: TP3DUIViewerBase;
begin
  Result:= TP3DUIViewerBase( FUIViewerFrame.AsValue );
end;

procedure TP3DFrame.KeyboardAction();
begin
  //UIViewerFrame.Zoom:= Zoom * UIViewer.Zoom;
  //UIViewerFrame.Position:= {P3DUIViewerActive.Position + }Position + ClippedRect.TopLeft * P3DUIViewerActive.Zoom;
  UIViewerFrame.Activate;
  inherited KeyboardAction();
  UIViewerFrame.Deactivate;
end;

procedure TP3DFrame.MouseAction(X, Y: Float; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean);
var
  p: TVec2;
begin
  //UIViewerFrame.Zoom:= Zoom * UIViewer.Zoom;
  //UIViewerFrame.Position:= {P3DUIViewerActive.Position + }Position + ClippedRect.TopLeft * P3DUIViewerActive.Zoom;
  UIViewerFrame.Activate;
  //p:= UIViewerFrame.TransformCoord( vec2( X, Y ));
  inherited MouseAction( X, Y, {X - Position.X, Y - Position.Y, }mb1, mb2, mb3, dmb1, dmb2, dmb3);
  //UIViewerFrame.Controls.MouseAction( UIViewerFrame.MouseX, UIViewerFrame.MouseY, mb1, mb2, mb3, dmb1, dmb2, dmb3 );
  //P3DApplication.MainWindow.Title:= UIViewerFrame.Mouse.ToString();
  UIViewerFrame.Deactivate;
end;

function TP3DFrame.ChildrenMouseRay(X, Y: Float; const Recurse: Boolean): TP3DGraphicControl;
var
  p: TVec2;
begin
  //UIViewerFrame.Zoom:= Zoom * UIViewer.Zoom;
  //UIViewerFrame.Position:= {P3DUIViewerActive.Position + }Position + ClippedRect.TopLeft * P3DUIViewerActive.Zoom;

  p:= UIViewerFrame.TransformCoord( vec2( X, Y ));
  //P3DApplication.MainWindow.Title:= vec2( X, Y ).ToString() + ' => ' + p.ToString();
  Result:= inherited ChildrenMouseRay( p.x, p.y );
end;

constructor TP3DFrame.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FUIViewerFrame:= TP3DStreamableContainer.Create( 'UIViewerFrame', TP3DUIViewerBase );
  Properties.Add( FUIViewerFrame );
  TP3DUIViewerBase.Create( FUIViewerFrame );
  UIViewerFrame.Controls:= Controls;
  Zoom:= 1.0;
  Controls.FUIViewer.AsValue:= UIViewerFrame;
end;

procedure TP3DFrame.Paint();
begin
  UIViewerFrame.Activate;
  UIViewerFrame.PrepareRender;
  //inherited Paint();
  if ( not HideChildren ) then
    Controls.Render();
  P3DViewports.Pop;
  UIViewerFrame.Deactivate;
  //UIViewer.Render;
end;

procedure TP3DFrame.Logic();
begin
  inherited Logic();
  UIViewerFrame.ViewRect:= ClippedRect * UIViewer.Zoom - UIViewer.Position;
  UIViewerFrame.Zoom:= Zoom * UIViewer.Zoom;
  UIViewerFrame.Position:= {P3DUIViewerActive.Position + }Position {+ ClippedRect.TopLeft * UIViewer.Zoom};
  //Controls.ClipRect:= UIViewerFrame.ViewRect - Position;
  //Controls.PageRect:= P3DRect( vec2( 0 ), UIViewerFrame.ViewRect.WidthHeight );
end;


{ TP3DCheckList }

function TP3DCheckList.GetChecked( Index: Integer ): Boolean;
begin
  Result:= Boolean( Children[ Index ].Data );
end;

procedure TP3DCheckList.SetChecked( Index: Integer ; AValue: Boolean);
begin
  Children[ Index ].Data:= Pointer( Integer( AValue ));
end;

constructor TP3DCheckList.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  HAlignment:= haRight;
  VAlignment:= vaCenter;
  FSymbolChecked:= TP3DTextBmp.CreateSimple( P3DSymbolCheck, P3DFontManagerBmp.Fonts[ 'Pascal3D-Symbols'], Canvas.Font.Size );
  PresetNormal.Color:= P3DThemeDefault.Accent;
  PresetNormal.FontColor:= P3DThemeDefault.Accent[ cp900 ];
  PresetHover.Color:= P3DThemeDefault.Accent;
  PresetHover.FontColor:= P3DThemeDefault.Accent[ cp700 ];
  PresetDown.Color:= P3DThemeDefault.Accent;
  PresetDown.FontColor:= P3DThemeDefault.Accent[ cp50 ];
end;

destructor TP3DCheckList.Destroy;
begin
  FreeAndNil( FSymbolChecked );
  inherited Destroy;
end;

function TP3DCheckList.DrawListItem(p: TVec2; Node: TP3DListItem): TVec2;
var
  Preset: TP3DButtonPreset;
  LineHeight: Float;
  CheckBoxWidth, OffSet: Single;
  R, RText, RCheckBox: TP3DRect;

  procedure RenderBtn;
  begin
    Canvas.Font.Size:= Font.Size;
    Canvas.Font.Name:= Font.Name;
    if (( Enabled ) and ( gcisMouseOver in InputState ) and ( R.PtInRect( UIViewer.Mouse - ClippedRect.TopLeft ))) then begin
      if ( gcisMouseBtn1Down in InputState ) then
        with ( PresetDown ) do begin
          RenderButton( Canvas, R, False, Color[ cp900 ], Color[ cp400 ], Color[ cp600 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
          Canvas.Font.Color:= FontColor;
        end
      else
        with ( PresetHover ) do begin
          RenderButton( Canvas, R, False, Color[ cp100 ], vec4( 1 ), Color[ cp50 ], Color[ cp100 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
          Canvas.Font.Color:= FontColor;
        end;
      exit;
    end;
    with ( PresetNormal ) do begin
      RenderButton( Canvas, R, False, Color[ cp200 ], Color[ cp100 ], Color[ cp200 ], Color[ cp200 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
      Canvas.Font.Color:= FontColor;
    end;
  end;

begin
  LineHeight:= Font.Size * 1.2;
  Node.FRenderPos:= p;
  p-= vec2( ScrollBarH.Position, ScrollBarV.Position );

  if ( not Assigned( Node.Text )) then
    Canvas.GetValidText( Node.Caption, Node.FText, P3DFontManager[ Font.Name, Font.Size ]);

  if ( gcisMouseBtn1Down in InputState ) then
    Preset:= PresetDown
  else if ( gcisMouseOver in InputState ) then
    Preset:= PresetHover
  else
    Preset:= PresetNormal;

  R:= P3DRect( p, vec2( ClippedRect.Width - ScrollBarWidth, LineHeight ));

  RenderBtn;

  R:= R.Inset( vec2( 10, 0 ), vec2( 10, 0 ));
  CheckBoxWidth:= Font.Size;
  OffSet:= CheckBoxWidth + 10;
  RText:= P3DRect( R.Left + OffSet, R.Top, R.Width - OffSet, R.Height );
  RCheckBox:= P3DAlignInRect( R, vec2( CheckBoxWidth ), HAlignment, VAlignment );
  RText:= P3DAlignInRect( R, vec2( Node.Text.Width, Node.Text.Height ), HAlignment, VAlignment );
  case HAlignment of
    haLeft: RText.Left:= RText.Left + OffSet;
    haCenter: RText.Left:= RText.Left + OffSet;
    haRight: RText.Left:= RText.Left - OffSet;
  end;

  Canvas.RenderText( Node.Caption, RText.TopLeft, Node.FText );

  Canvas.RenderRectShadow( RCheckBox.TopLeft + 1, RCheckBox.BottomRight + 1, 2, vec4( 0, 0, 0, 0.1 ));

  with ( PresetNormal ) do
    begin
      if ( Assigned( Node.Data )) then // if Checked
        Canvas.RenderRect( RCheckBox.TopLeft, RCheckBox.BottomRight, Color[ cp900 ], Color[ cp400 ], Color[ cp600 ], Color[ cp900 ])
      else
        Canvas.RenderRect( RCheckBox.TopLeft, RCheckBox.BottomRight, Color[ cp400 ], Color[ cp200 ], Color[ cp300 ], Color[ cp400 ]);
      if ( Assigned( Node.Data )) then // if Checked
        RenderButtonText( Canvas, FSymbolChecked.Text, FSymbolChecked, RCheckBox, haCenter, vaCenter, Color[ cp50 ]);
    end;

  Result.x:= Node.FRenderPos.x + Node.Text.Width;
  Result.y:= Node.FRenderPos.y + R.Height;
end;

procedure TP3DCheckList.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
  if ( Assigned( Selection )) then begin
    if ( Assigned( Selection.Data )) then
      Selection.Data:= nil
    else
      Selection.Data:= Pointer( 1 );
    if ( Assigned( FOnSetChecked )) then
      FOnSetChecked( Selection, Children.IndexOf( Selection ), Assigned( Selection.Data ));
  end
end;



{$ENDIF}
