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
      procedure CalcHSVFromMousePos( X, Y: Integer );

    public
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Draw; override;
      procedure MouseMove(X, Y: Integer); override;
      procedure MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer); override;

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
      procedure CalcValueFromMousePos( Y: Integer );

    public
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      procedure Draw; override;
      procedure MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer); override;
      procedure MouseMove(X, Y: Integer); override;

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
      FColorEdit: TP3DVectorEdit;
      FColorValue: TP3DColorRGBAValue;
      FColorWheel: TP3DColorRGBAWheel;
      FGetValueEvent: TGetValue;
      FSetValueEvent: TSetValue;

      procedure ColorChange( Sender: TP3DGraphicControl );
      function GetColor: TP3DColorRGBA;
      function GetEditValue( Sender: TP3DVectorEdit ): TVec4;
      procedure SetColor(AValue: TP3DColorRGBA);
      procedure SetEditValue( Sender: TP3DVectorEdit; AValue: TVec4 );
      class function HasShadow: Boolean; override;

    public
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure UpdateColor;
      procedure Draw; override;

      property ColorWheel: TP3DColorRGBAWheel read FColorWheel write FColorWheel;
      property ColorValue: TP3DColorRGBAValue read FColorValue write FColorValue;
      property ColorEdit: TP3DVectorEdit read FColorEdit write FColorEdit;
      property GetValueEvent: TGetValue read FGetValueEvent write FGetValueEvent;
      property SetValueEvent: TSetValue read FSetValueEvent write FSetValueEvent;
      property Color: TP3DColorRGBA read GetColor write SetColor;
      property BGColor: TP3DColorRGBA read FBGColor write FBGColor;
      property BorderColor: TP3DColorRGBA read FBorderColor write FBorderColor;
      property EditCount: Integer read FColorEdit.FEdCount write FColorEdit.FEdCount;
  end;

  { TP3DColorRGBAComboBox }

  TP3DColorRGBAComboBox = class ( TP3DGraphicControl )
    private type
      TGetValue = function ( Sender: TP3DColorRGBAPicker ): TP3DColorRGBA of object;
      TSetValue = procedure ( Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA ) of object;

    private
      FBorderColor: TP3DColorRGBA;
      FColorPicker: TP3DColorRGBAPicker;
      function GetValue: TP3DColorRGBA;
      procedure SetValue(AValue: TP3DColorRGBA);
      //FOnChange: TP3DComboBoxChange;

    public
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure Draw; override;
      procedure DropDown;
      procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
      procedure MouseAction(X, Y: Integer; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean); override;

      property ColorPicker: TP3DColorRGBAPicker read FColorPicker write FColorPicker;
      property GetValueEvent: TGetValue read FColorPicker.FGetValueEvent write FColorPicker.FGetValueEvent;
      property SetValueEvent: TSetValue read FColorPicker.FSetValueEvent write FColorPicker.FSetValueEvent;
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
      FMaterial: TP3DMaterialBase;
      FTexture: TP3DTexture;
      procedure SetTexture(AValue: TP3DTexture);

    public
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;      destructor Destroy; override;
      procedure Draw; override;

      property Color: TP3DColorRGBA read FColor write FColor;

    published
      property Texture: TP3DTexture read FTexture write SetTexture;
      property AutoSize: Boolean read FAutoSize write FAutoSize;
      property KeepAspect: Boolean read FKeepAspect write FKeepAspect;
      property Material: TP3DMaterialBase read FMaterial write FMaterial;
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
  if ( not Canvas.PrepareForDrawing ) then exit;

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

procedure TP3DColorRGBAWheel.CalcHSVFromMousePos(X, Y: Integer);
var
  c: TVec2;
  r: Float;
  H: Float;
begin
  c:= vec2( ClientRect.Width / 2, ClientRect.Height / 2 );
  r:= c[ 1 - c.GetBiggestComp ];
  FSelXY:= ( vec2( X, Y ) - c ) / r;
  if ( FSelXY.GetDist > 1 ) then
    FSelXY.Normalize;

  if ( FSelXY.GetDist < epsilon ) then
    H:= 0
  else
    H:= 0.5 + arctan2( FSelXY.x, -FSelXY.y ) / ( 2 * PI );
  //Calculate the Angle and convert it to a H value where 0,+1 is located at the bottom
  HSV:= vec3( H, FSelXY.GetDist, FHSV[ 2 ]);
  //WriteLn( 'HSV = <', HSV.ToString(), '> RGB = <', RGB.ToString(), '>' );
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

constructor TP3DColorRGBAWheel.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock( AData, AParent );
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

procedure TP3DColorRGBAWheel.MouseMove(X, Y: Integer);
begin
  inherited MouseMove( X, Y );

  if ( gcisMouseBtn1Down in InputState ) then
    CalcHSVFromMousePos( X, Y );
end;

procedure TP3DColorRGBAWheel.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
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

procedure TP3DColorRGBAValue.CalcValueFromMousePos(Y: Integer);
begin
  Value:= 1 - ( Y - ClientRect.Top ) / ClientRect.Height;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

constructor TP3DColorRGBAValue.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock( AData, AParent );
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

procedure TP3DColorRGBAValue.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseDown(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    CalcValueFromMousePos( Y );
end;

procedure TP3DColorRGBAValue.MouseMove(X, Y: Integer);
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
end;

procedure TP3DColorRGBAPicker.SetEditValue(Sender: TP3DVectorEdit; AValue: TVec4);
begin
  Color:= clamp( AValue, 0.0, 1.0 );
end;

class function TP3DColorRGBAPicker.HasShadow: Boolean;
begin
  Result:= True;
end;

constructor TP3DColorRGBAPicker.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
var
  container: TP3DGraphicControl;
begin
  inherited CreateBlock( AData, AParent );
  WidthHeight:= vec2( 200, 150 );
  BorderColor:= vec4( 0, 0, 0, 0.2 );
  BGColor:= vec4( 1 );
  container:= TP3DGraphicControl.CreateBlock( nil, Self );
  container.Align:= alClient;

  ColorValue:= TP3DColorRGBAValue.CreateBlock( nil, container );
  ColorValue.Align:= alRight;
  ColorValue.Margin:= vec4( 15 );
  ColorValue.OnChange:= @ColorChange;

  ColorWheel:= TP3DColorRGBAWheel.CreateBlock( nil, container );
  ColorWheel.Align:= alClient;
  ColorWheel.OnChange:= @ColorChange;

  ColorEdit:= TP3DVectorEdit.CreateBlock( nil, Self );
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

procedure TP3DColorRGBAComboBox.SetValue(AValue: TP3DColorRGBA);
begin
  ColorPicker.Color:= AValue;
end;

constructor TP3DColorRGBAComboBox.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock( AData, AParent );

  BorderColor:= vec4( 0, 0, 0, 0.2 );
  FColorPicker:= TP3DColorRGBAPicker.CreateBlock( AData );
  FColorPicker.Visible:= False;
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
  ColorPicker.TopLeft:= min( P3DGUIManager.ScreenCanvas.WidthHeight - ColorPicker.WidthHeight, vec2( Canvas.Left, Canvas.Top + Height ));
  ColorPicker.Visible:= True;
end;

procedure TP3DColorRGBAComboBox.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseClick(mb1, mb2, mb3, X, Y);
  if ( mb1 ) then
    DropDown;
end;

procedure TP3DColorRGBAComboBox.MouseAction(X, Y: Integer; mb1, mb2, mb3, dmb1, dmb2, dmb3: Boolean);
var
  r: TP3DRect;
begin
  inherited MouseAction(X, Y, mb1, mb2, mb3, dmb1, dmb2, dmb3);
  if ( ColorPicker.Visible ) then
    begin
      r:= P3DRectEx( min( ColorPicker.Canvas.Rect.TopLeft, Canvas.Rect.TopLeft ) - 10,
                     max( ColorPicker.Canvas.Rect.BottomRight, Canvas.Rect.BottomRight ) + 10 );
      if ( not r.PtInRect( vec2( P3DInput.Mouse.X, P3DInput.Mouse.Y ))) then
        ColorPicker.Visible:= False;
    end;
end;

{ TP3DImage }

procedure TP3DImage.SetTexture(AValue: TP3DTexture);
begin
  if FTexture=AValue then Exit;
  FTexture:=AValue;
  Material.Maps[ 0 ].Map:= AValue;
end;

constructor TP3DImage.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock( AData, AParent );
  Color:= vec4( 1 );
  KeepAspect:= True;
  Material:= TP3DMaterialBase.CreateBlock();
  Material.Unlit:= True;
  Material.UseVertexColor:= True;
  with( TP3DMaterialMapBase.Create( Material.Maps )) do
    begin
      Texture:= nil;
      ParentMaterial:= Material;
    end;
  Material.BuildShader();
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

      Material.PassToShader();
//      if ( not Assigned( P3DFontManager.Material.Shader )) then
//        TP3DMaterialBase( P3DFontManager.Material ).BuildShader;
//      Shader:= P3DFontManager.Material.Shader;
//      Shader.Enable;
      Shader:= Material.Shader;

      //glEnable(GL_TEXTURE_2D);
      //glActiveTexture( GL_TEXTURE0 );
      //glBindTexture( GL_TEXTURE_2D, Texture.GLTexture );


//      if ( Shader.Uniforms.FindByName( 'tex0', Uniform )) then
//        Uniform.AsInt:= 0;

      if ( Shader.Uniforms.FindByName( 'view', Uniform )) then
        Uniform.AsMat4:= Mat4Identity;
      if ( Shader.Uniforms.FindByName( 'world', Uniform )) then
        Uniform.AsMat4:= Mat4Identity;
      if ( Shader.Uniforms.FindByName( 'proj', Uniform )) then
        Uniform.AsMat4:= Canvas.Proj;

      if ( P3DShaderActive.Attributes.FindByName( 'TexCoord0', decl )) then
        begin
          glBegin( GL_TRIANGLE_FAN );

          glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A );
          decl.AsVec2:= vec2( 0 );
          glVertexAttrib2f( P3DAttribPosition, p1.X, p1.Y );
    //      glVertexAttrib2f( P3DAttribTexCoord0, 1, 0 );

          decl.AsVec2:= vec2( 1, 0 );
          glVertexAttrib2f( P3DAttribPosition, p2.X, p1.Y );
    //      glVertexAttrib2f( P3DAttribTexCoord0, 1, 1 );

          decl.AsVec2:= vec2( 1, 1 );
          glVertexAttrib2f( P3DAttribPosition, p2.X, p2.Y );
    //      glVertexAttrib2f( P3DAttribTexCoord0, 0, 1 );

          decl.AsVec2:= vec2( 0, 1 );
          glVertexAttrib2f( P3DAttribPosition, p1.X, p2.Y );
          //glVertexAttrib2f( P3DAttribTexCoord0, 0, 0 );
          glEnd();
        end;
    end;
end;

{$ENDIF}
