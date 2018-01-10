{$IFDEF INTERFACE}
type

  { TP3DFontLetter }

  TP3DFontLetter = class ( TP3DNamedStreamable )
    private
      Fuv1, Fuv2: TVec2;
      Fuv1_metrics: TVec2;
      FTexture: TP3DTexture;
      FLetter: WideChar; //UNICODE
      Fuv2_metrics: TVec2;
      procedure SetLetter(AValue: WideChar);

    public
      constructor Create( const AOwner: IP3DStreamableContainer = nil ); override;
      destructor Destroy; override;

      property Texture: TP3DTexture read FTexture write FTexture;
      property uv1: TVec2 read Fuv1 write Fuv1;
      property uv2: TVec2 read Fuv2 write Fuv2;
      property uv1_metrics: TVec2 read Fuv1_metrics write Fuv1_metrics;
      property uv2_metrics: TVec2 read Fuv2_metrics write Fuv2_metrics;

    published
      property Letter: WideChar read FLetter write SetLetter;
  end;

  { TP3DFontLetterList }
  {
  TP3DFontLetterList = class ( specialize gP3DCustomObjectList < TP3DFontLetter > )
    function Find( C: WideChar ): Integer;
  end;}

  TP3DFontLetterContainer = specialize gP3DStreamableContainer < TP3DFontLetter >;
  TP3DFontLetterPointer = specialize gP3DStreamablePointer < TP3DFontLetter >;
  TP3DFontLetterContainerList = specialize gP3DNamedStreamableContainerList < TP3DFontLetter, TP3DFontLetterContainer >;
  TP3DFontLetterPointerList = specialize gP3DNamedStreamableList < TP3DFontLetter, TP3DFontLetterPointer >;

  { TP3DFontBmp }

  TP3DFontBmp = class ( TP3DDataBlock )
    protected
      FFileName: String;
      FLetters: TP3DFontLetterContainerList;
      FMargin: Single;
      FMipMapLoadBias: Single;
      FSizeFactor: Single;
      FTexture: TP3DTexturePointer;

      procedure SetFileName(AValue: String);
      procedure SetMipMapLodBias(AValue: Single);
      function GetTexture: TP3DTexture;
      procedure SetTexture(AValue: TP3DTexture);

    public
      constructor CreateBlock( const AData: TP3DLibrary = nil ); override;
      constructor CreateByFont( AFont: TP3DFont; AWidth, AHeight, AMargin: Integer; const AData: TP3DLibrary = nil );

      destructor Destroy; override;

      class function GetContainer( ALib: TP3DLibrary): TP3DPropertyAccess; override;

      procedure ApplyShader;

    published
      property Letters: TP3DFontLetterContainerList read FLetters write FLetters;
      property Texture: TP3DTexture read GetTexture write SetTexture;
      property SizeFactor: Single read FSizeFactor write FSizeFactor;
      property Margin: Single read FMargin write FMargin;
      property MipMapLoadBias: Single read FMipMapLoadBias write SetMipMapLodBias;
  end;

  TP3DFontBmpContainer = specialize gP3DStreamableContainer < TP3DFontBmp >;
  TP3DFontBmpPointer = specialize gP3DStreamablePointer < TP3DFontBmp >;
  TP3DFontBmpContainerList = specialize gP3DNamedStreamableContainerList < TP3DFontBmp, TP3DFontBmpContainer >;
  TP3DFontBmpPointerList = specialize gP3DStreamableList < TP3DFontBmp, TP3DFontBmpPointer >;

{ TP3DFontBmpList }
{
TP3DFontBmpList = class ( specialize gP3DNamedStreamableList < TP3DFontBmp >)
  procedure LoadFromStream( S: TStream ); //gP3DListPropertyAccessObject needs this to compile
  procedure SaveToStream( S: TStream );
end;
}

TP3DMaterialModifierText = class ( TP3DMaterialModifier )
  private
    FTextAlpha: Float;
    FTextBias: Float;
    FTextSmoothing: Float;

  public
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

    class function GetIndex: Integer; override;

    procedure AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase); override;
    procedure PassToShader(Material: TP3DMaterialBase); override;
    procedure Reset;

  published
    property TextSmoothing: Float read FTextSmoothing write FTextSmoothing;
    property TextAlpha: Float read FTextAlpha write FTextAlpha;
    property TextBias: Float read FTextBias write FTextBias;
end;

{ TP3DFontManagerBmp }

TP3DFontManagerBmp = class ( TP3DStreamable )
  private
    FDefaultTexHeight: Integer;
    FDefaultTextMargin: Integer;
    FDefaultTextSize: Single;
    FDefaultTexWidth: Integer;
    FMaterial: TP3DMaterialBaseContainer;
    FTextModifier: TP3DMaterialModifierContainer;
    FItems: TP3DFontBmpPointerList;

    function GetCount: Integer;
    function GetFont( FontName: String ): TP3DFontBmp;
    function GetFontByP3DFont( Font: TP3DFont ): TP3DFontBmp;
    function GetItems( Index: Integer ): TP3DFontBmp;
    function GetMaterial: TP3DMaterialBase;
    function GetTextModifier: TP3DMaterialModifierText;
    procedure SetCount(AValue: Integer);
    procedure SetItems( Index: Integer ; AValue: TP3DFontBmp);

  public
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
    destructor Destroy; override;

    function AddByP3DFont( Font: TP3DFont ): Integer;
    property Fonts[ FontName: String ]: TP3DFontBmp read GetFont; default;
    property FontsByP3DFont[ Font: TP3DFont ]: TP3DFontBmp read GetFontByP3DFont;
    property Items[ Index: Integer ]: TP3DFontBmp read GetItems write SetItems;

  published
    function Find( FontName: String ): Integer;
    property Material: TP3DMaterialBase read GetMaterial;
    property DefaultTexWidth: Integer read FDefaultTexWidth write FDefaultTexWidth;
    property DefaultTexHeight: Integer read FDefaultTexHeight write FDefaultTexHeight;
    property DefaultTextSize: Single read FDefaultTextSize write FDefaultTextSize;
    property DefaultTextMargin: Integer read FDefaultTextMargin write FDefaultTextMargin;
    property TextModifier: TP3DMaterialModifierText read GetTextModifier;
    property Count: Integer read GetCount write SetCount;
end;

{ TP3DTextBmp }

TP3DTextBmp = class ( TPersistent )
  private
    FFont: TP3DFontBmp;
    FHeight: Single;
    FIndices: TP3DIntBufferGL;
    FLetterPosAndSize: TVec4List;
    FTexCoords: TP3DVec2BufferGL;
    FText: String;
    FVertices: TP3DVec2BufferGL;
    FWidth: Single;

    function AppendLetter( L: WideChar; fs: Single; p: TVec2 ): TVec2;

  public
    constructor Create;
    constructor CreateSimple( Text: String; Font: TP3DFontBmp; ASize: Single );
    destructor Destroy; override;

    procedure Render( p: TVec2; Color: TVec4; proj: TMat4 );

    function WidthFromTo( idx1: Integer; idx2: Integer ): TVec4;

    property Text: String read FText;
    property Width: Single read FWidth;
    property Height: Single read FHeight;
    property Font: TP3DFontBmp read FFont;
    property Vertices: TP3DVec2BufferGL read FVertices write FVertices;
    property TexCoords: TP3DVec2BufferGL read FTexCoords write FTexCoords;
    property Indices: TP3DIntBufferGL read FIndices write FIndices;
    property LetterPosAndSize: TVec4List read FLetterPosAndSize write FLetterPosAndSize;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
function TP3DTextBmp.AppendLetter( L: WideChar; fs: Single; p: TVec2 ): TVec2;
var
  v1,v2,v3,v4: TVec2;
  tc1, tc2, tc3, tc4, v3_skip: TVec2;
  n: Integer;
  letter: TP3DFontLetter;
begin
  Result:= p;

  n:= Font.Letters.FindByName( L );
  if ( n < 0 ) then
    exit;

  FText+= L;

  v1:= p;
  letter:= Font.Letters[ n ];
  v3:= p + ( letter.uv2 - letter.uv1 ) / Font.SizeFactor * fs;
  v3_skip:= p + ( letter.uv2_metrics - letter.uv1 ) / Font.SizeFactor * fs;
  v2:= vec2( v3.x, v1.y );
  v4:= vec2( v1.x, v3.y );
  tc1:= letter.uv1;
  tc3:= letter.uv2;
  tc2:= vec2( tc3.x, tc1.y );
  tc4:= vec2( tc1.x, tc3.y );

  n:= Vertices.AddArray([ v1 - Font.Margin, v2 - Font.Margin, v3 + Font.Margin, v4 + Font.Margin ]);

  TexCoords.AddArray([ tc1, tc2, tc3, tc4 ]);
  Indices.AddArray([ n + 0, n + 1, n + 2, n + 0, n + 2, n + 3 ]);
  LetterPosAndSize.Add( vec4( v1, v3 - v1 ));
  Result:= v3_skip;
  //v3;
end;

constructor TP3DTextBmp.Create;
begin
  inherited;
  Vertices:= TP3DVec2BufferGL.Create;
  Vertices.SetAttribArray( P3DAttribPosition );
  TexCoords:= TP3DVec2BufferGL.Create;
  Indices:= TP3DIntBufferGL.Create;
  LetterPosAndSize:= TVec4List.Create;
end;

constructor TP3DTextBmp.CreateSimple(Text: String; Font: TP3DFontBmp;
  ASize: Single);
var
  c: Char;
  p: TVec2;
  max_y: Float;
begin
  Create;

  FFont:= Font;
  p:= vec2( 0 );
  max_y:= 0;
  for c in Text do
    begin
      p:= AppendLetter( c, ASize, p );
      max_y:= max( max_y, p.y );
      p.Y:= 0;
    end;
  Vertices.PushData;
  TexCoords.PushData;
  Indices.PushData;
  FWidth:= p.x;
  FHeight:= max_y;
end;

destructor TP3DTextBmp.Destroy;
begin
  Vertices.Free;
  Indices.Free;
  LetterPosAndSize.Free;
  TexCoords.Free;

  inherited;
end;

procedure TP3DTextBmp.Render( p: TVec2; Color: TVec4; proj: TMat4 );
var
  Shader: TP3DShader;
  Uniform, decl: TP3DShaderDecl;
  mt, m: TMat4;
begin
  Font.ApplyShader;
  Shader:= P3DShaderActive;

  mt:= mat4translate( vec4( p, 0, 1 ));
  m:= proj * mt;
  if ( Shader.Uniforms.FindByName( 'proj', Uniform )) then
    Uniform.AsMat4:= m;
  if ( Shader.Uniforms.FindByName( 'view', Uniform )) then
    Uniform.AsMat4:= Mat4Identity;
  if ( Shader.Uniforms.FindByName( 'world', Uniform )) then
    Uniform.AsMat4:= Mat4Identity;

  glVertexAttrib4f( P3DAttribColor, Color.X, Color.Y, Color.Z, Color.A );

  Vertices.SetAttribArray( P3DAttribPosition );
  if ( P3DShaderActive.Attributes.FindByName( 'TexCoord0', decl )) then
    decl.AsVec2List:= TexCoords;

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 ));
end;

function TP3DTextBmp.WidthFromTo(idx1: Integer; idx2: Integer): TVec4;
  function GetP( idx: Integer ): TVec4;
  var
    n: Integer;
  begin
    if ( LetterPosAndSize.Count = 0 ) then
      begin
        n:= Font.Letters.FindByName( 'W' );
        if ( n = -1 ) then
          Result:= vec4( 0 )
        else
          Result:= vec4( vec2( 0 ), Font.Letters[ n ].uv2 - Font.Letters[ n ].uv1 );
      end
    else if ( idx >= LetterPosAndSize.Count ) then
      Result:= vec4( LetterPosAndSize[ LetterPosAndSize.Count - 1 ].XY + vec2( LetterPosAndSize[ LetterPosAndSize.Count - 1 ].Z, 0 ), vec2( 0, LetterPosAndSize[ LetterPosAndSize.Count - 1 ].W ))
    else
      Result:= LetterPosAndSize[ idx ].XYZW;
  end;

var
  p1: TVec4;
  p2: TVec4;
begin
  p1:= GetP( idx1 );
  p2:= GetP( idx2 );
  Result:= vec4( p1.XY, p2.XY - p1.XY + vec2( 0, p2.W ));
end;

{
function TP3DFontLetterList.Find(C: WideChar): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( C = Items[ i ].Letter ) then
      begin
        Result:= i;
        break;
      end;
end;
}
{ TP3DFontBmp }

procedure TP3DFontBmp.ApplyShader;
var
  Map: TP3DMaterialMapBase;
begin
  if ( P3DFontManagerBmp.Material.Maps.Count = 0 ) then
    begin
      Map:= TP3DMaterialMapBase.Create( P3DFontManagerBmp.Material.Maps );
      Map.ParentMaterial:= P3DFontManagerBmp.Material;
      //P3DFontManagerBmp.Material.Maps.Add( Map );
    end;
  P3DFontManagerBmp.Material.Maps[ 0 ].Map:= Texture;
  TP3DMaterialMapBase( P3DFontManagerBmp.Material.Maps.Items[ 0 ]).AlphaFactor:= 1;
  TP3DMaterialMapBase( P3DFontManagerBmp.Material.Maps.Items[ 0 ]).DiffuseFactor:= 0;
  if ( not Assigned( P3DFontManagerBmp.Material.Shader )) then
    P3DFontManagerBmp.Material.BuildShader();
  P3DFontManagerBmp.Material.PassToShader();
end;

function TP3DFontBmp.GetTexture: TP3DTexture;
begin
  Result:= FTexture.AsValue;
end;

procedure TP3DFontBmp.SetTexture(AValue: TP3DTexture);
begin
  FTexture.AsValue:= AValue;
end;

procedure TP3DFontBmp.SetFileName(AValue: String);
begin
  FFileName:= AValue;
end;

constructor TP3DFontBmp.CreateBlock(const AData: TP3DLibrary);
begin
  inherited CreateBlock( AData );
  Letters:= TP3DFontLetterContainerList.Create( 'Letters', smText );
  SizeFactor:= 1;
  MipMapLoadBias:= 0;//-0.65;
  Margin:= 0;

  FTexture:= TP3DTexturePointer.Create( 'Texture', smText );
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'SizeFactor', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'MipMapLoadBias', smText ));
  Properties.Add( FTexture );
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Margin', smText ));
  Properties.Add( FLetters );
  //Owner.Fonts.Add( Self );

  {property Letters: TP3DFontLetterList read FLetters write FLetters;
  property Texture: TP3DTexture read FTexture write FTexture;
  property FileName: String read FFileName write SetFileName;
  property FontName: String read FFontName;
  property SizeFactor: Single read FSizeFactor write FSizeFactor;
  property MipMapLoadBias: Single read FMipMapLoadBias write SetMipMapLodBias;}
end;

constructor TP3DFontBmp.CreateByFont(AFont: TP3DFont; AWidth, AHeight, AMargin: Integer; const AData: TP3DLibrary);

  procedure DrawText( ASurface: PSDL_Surface );
  var
    i, e: Integer;
    sz, oldsz: TPoint;
    low: Integer = 32;
    high: Integer = 126;
    cl: TSDL_Color;
    minx, maxx, miny, maxy, advance: Integer;
    fnt: PTTF_Font;
    text_surf: PSDL_Surface;
    r1, r2: Classes.TRect;
    letter: TP3DFontLetter;
  begin
    {if ( Font.Size <> P3DFontManagerBmp.DefaultTextSize ) then
      Font:= P3DFontManager.Fonts[ Font.FontName, round( P3DFontManagerBmp.DefaultTextSize )];}
//    FFontName:= Font.FontName;
    fnt:= TTF_OpenFont( PChar( AFont.FileWatch.FileName ), round( P3DFontManagerBmp.DefaultTextSize ));
    SizeFactor:= TTF_FontHeight( fnt ) / ASurface^.h;
    FMargin:= AMargin / ASurface^.h;
    //WriteLn( AFont.FontName + ' Font Size: ', TTF_FontHeight( fnt ), ' Pt', Font.Size );

    cl.r:= 0;
    cl.g:= 0;
    cl.b:= 255;    // Pixels should have 1 for inside in r and g channel and the opposite in b and a
    cl.a:= 255;

    sz:= Point( 0, 0 );
    oldsz:= Point( AMargin, AMargin );

    SDL_FillRect( ASurface, nil, SDL_MapRGBA( ASurface^.format, 255, 255, 0, 0 )); //Initialize pixels for distance fields
    for i:= low to high do
      if ( Boolean( TTF_GlyphIsProvided( fnt, i ))) then
        begin
          e:= TTF_SizeText( fnt, PAnsiChar( AnsiString( AnsiChar( i ))), @sz.X, @sz.Y );
          if ( e <> 0 ) then
            begin
              WriteLn( 'Glyph Error: ', i );
              continue;
            end;
          if ( oldsz.Y + sz.Y + AMargin > ASurface^.h ) then
            break;

          if ( oldsz.X + sz.X + AMargin * 2 > ASurface^.w ) then
            oldsz:= Point( AMargin, oldsz.y + sz.Y + AMargin ); //Skip Line

          text_surf:= TTF_RenderText_Solid( fnt, PAnsiChar( AnsiString( AnsiChar( i ))), cl );
          //SDL_SetSurfaceBlendMode( text_surf, SDL_BLENDMODE_BLEND );
          //SDL_SetSurfaceBlendMode( ASurface, SDL_BLENDMODE_BLEND );
          r1:= Rect( 0, 0, text_surf^.w, text_surf^.h );
          r2:= Rect( oldsz.X, oldsz.Y, text_surf^.w + oldsz.X + AMargin, oldsz.Y + text_surf^.h );

          SDL_BlitSurface( text_surf, PSDL_Rect( @r1 ), ASurface, PSDL_Rect( @r2 ));
          SDL_FreeSurface( text_surf );

          TTF_GlyphMetrics( fnt, i,
                            @minx, @maxx, @miny, @maxy, @advance );

          letter:= TP3DFontLetter.Create;
          letter.Letter:= WideChar( i );
          letter.uv1:= vec2( oldsz.X / ASurface^.w, oldsz.Y / ASurface^.h );
          letter.uv2:= vec2(( oldsz.X + sz.X ) / ASurface^.w, ( oldsz.Y + sz.Y ) / ASurface^.h );
          letter.uv1_metrics:= vec2(( oldsz.X + minx ) / ASurface^.w, ( oldsz.Y - maxy + TTF_FontAscent( fnt )) / ASurface^.h );
          letter.uv2_metrics:= vec2(( oldsz.X + advance ) / ASurface^.w, ( oldsz.Y - miny + TTF_FontAscent( fnt )) / ASurface^.h );

          Letters.Add( letter );
          oldsz.x:= oldsz.x + sz.x + AMargin;
        end
    else
      WriteLn( 'Glyph not provided: ', i );
    ;TTF_CloseFont( fnt );
  end;

  const
    INF: TIVec2 = ( FCoord: ( 9999, 9999 ));


  function DistSq( v: TIVec2 ): Cardinal;
  begin
    if ( v[ 0 ] > 65000 ) or ( v[ 1 ] > 65000 ) then
      Result:= high( Cardinal )
    else
      Result:= v[ 0 ] * v[ 0 ] + v[ 1 ] * v[ 1 ];
  end;

  function CalculateSignedDistanceField( ASurface: PSDL_Surface; Channel: Integer ): PSDL_Surface;
    function GetPixelOrig( Surface: PSDL_Surface; x, y: Integer ): TIVec2;
    var
      px1, px2: Byte;
    begin
      if (( x < 0 ) or ( x > Surface^.w - 1 ) or
          ( y < 0 ) or ( y > Surface^.w - 1 )) then
        Result:= INF
      else
        begin
          px1:= PByte( Pointer( DWord( Surface^.pixels ) + x * 4 + Channel * 2 + y * Surface^.pitch ))^;
          px2:= PByte( Pointer( DWord( Surface^.pixels ) + x * 4 + Channel * 2 + 1 + y * Surface^.pitch ))^;
          Result[ 0 ]:= Word( px1 or px1 shl 8 );
          Result[ 1 ]:= Word( px2 or px2 shl 8 );
        end;
    end;

    function GetPixel( Surface: PSDL_Surface; x, y: Integer ): TIVec2;
    var
      px1, px2: SmallInt;
    begin
      if (( x < 0 ) or ( x > Surface^.w - 1 ) or
          ( y < 0 ) or ( y > Surface^.w - 1 )) then
        Result:= INF
      else
        begin
          px1:= PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + y * Surface^.pitch ))^;
          px2:= PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + 2 + y * Surface^.pitch ))^;
          Result[ 0 ]:= px1;
          Result[ 1 ]:= px2;
        end;
    end;

    procedure SetPixel( Surface: PSDL_Surface; x, y: Integer; Value: TIVec2 );
    begin
      if not (( x < 0 ) or ( x > Surface^.w - 1 ) or
          ( y < 0 ) or ( y > Surface^.w - 1 )) then
        begin
          PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + y * Surface^.pitch ))^:= Max( low( SmallInt ), Min( high( SmallInt ), Value[ 0 ]));
          PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + 2 + y * Surface^.pitch ))^:= Max( low( SmallInt ), Min( high( SmallInt ), Value[ 1 ]));
          if (( Value[ 0 ] <> ( PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + y * Surface^.pitch ))^)) or
              ( Value[ 1 ] <> ( PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + 2 + y * Surface^.pitch )))^)) then
            WriteLn( Value[ 0 ], ' / ', PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + y * Surface^.pitch ))^, ', ',
                     Value[ 1 ], ' / ', PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + 2 + y * Surface^.pitch ))^ );
        end;
    end;

    function Compare( Surface: PSDL_Surface; px: TIVec2; x, y, OffSetX, OffSetY: Integer ): TIVec2;
    var
      other: TIVec2;
    begin
      other:= GetPixel( Surface, x + OffSetX, y + OffSetY );
      other+= ivec2( OffSetX, OffSetY );

      if ( DistSq( other ) < DistSq( px )) then
      	Result:= other
      else
        Result:= px;
    end;


  var
    x, y: Integer;
    px: TIVec2;
  begin
    Result:= SDL_CreateRGBSurface( SDL_SWSURFACE, AWidth, AHeight, 32, $000000FF, $0000FF00, $00FF0000, $FF000000 );

    SDL_LockSurface( Result );
    SDL_LockSurface( ASurface );
    for y:= 0 to Result^.h - 1 do
      for x:= 0 to Result^.w - 1 do
        begin
    	    px:= GetPixelOrig( ASurface, x, y );
          px[ 0 ]:= Min( INF[ 0 ], px[ 0 ]);
          px[ 1 ]:= Min( INF[ 1 ], px[ 1 ]);
    	  SetPixel( Result, x, y, px );
        end;

    // Pass 0
    for y:= 1 to Result^.h - 1 do
      begin
        for x:= 1 to Result^.w - 1 do
      	  begin
      	    px:= GetPixel( Result, x, y );
      	    px:= Compare( Result, px, x, y, -1,  0 );
      	    px:= Compare( Result, px, x, y,  0, -1 );
      	    px:= Compare( Result, px, x, y, -1, -1 );
      	    px:= Compare( Result, px, x, y,  1, -1 );
      	    SetPixel( Result, x, y, px );
      	  end;

        for x:= Result^.w - 2 downto 0 do
      	  begin
      	    px:= GetPixel( Result, x, y );
      	    px:= Compare( Result, px, x, y, 1, 0 );
      	    SetPixel( Result, x, y, px );
      	  end;
      end;

    // Pass 1
    for y:= Result^.h - 2 downto 0 do
      begin
        for x:= Result^.w - 2 downto 0 do
      	  begin
      	    px:= GetPixel( Result, x, y );
      	    px:= Compare( Result, px, x, y,  1,  0 );
      	    px:= Compare( Result, px, x, y,  0,  1 );
      	    px:= Compare( Result, px, x, y, -1,  1 );
      	    px:= Compare( Result, px, x, y,  1,  1 );
      	    SetPixel( Result, x, y, px );
      	  end;

        for x:= 1 to Result^.w - 1 do
      	  begin
      	    px:= GetPixel( Result, x, y );
      	    px:= Compare( Result, px, x, y, -1, 0 );
      	    SetPixel( Result, x, y, px );
      	  end;
      end;
    SDL_UnlockSurface( Result );
    SDL_UnlockSurface( ASurface );
  end;

  function CombineAndDownscale( dist1, dist2: PSDL_Surface; Downscale: Integer ): PSDL_Surface;
    function GetPixel( Surface: PSDL_Surface; x, y: Integer ): TIVec2;
    var
      px1, px2: SmallInt;
    begin
      if (( x < 0 ) or ( x > Surface^.w - 1 ) or
          ( y < 0 ) or ( y > Surface^.h - 1 )) then
        Result:= INF
      else
        begin
          px1:= PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + y * Surface^.pitch ))^;
          px2:= PSmallInt( Pointer( DWord( Surface^.pixels ) + x * 4 + 2 + y * Surface^.pitch ))^;
          Result[ 0 ]:= px1;
          Result[ 1 ]:= px2;
        end;
    end;

    procedure SetPixel( Surface: PSDL_Surface; x, y: Integer; Value: Byte );
    begin
      if not (( x < 0 ) or ( x > Surface^.w - 1 ) or
          ( y < 0 ) or ( y > Surface^.h - 1 )) then
        begin
          PByte( Pointer( DWord( Surface^.pixels ) + x * 4 + y * Surface^.pitch ))^:= Value;
          PByte( Pointer( DWord( Surface^.pixels ) + x * 4 + 1 + y * Surface^.pitch ))^:= Value;
          PByte( Pointer( DWord( Surface^.pixels ) + x * 4 + 2 + y * Surface^.pitch ))^:= Value;
          PByte( Pointer( DWord( Surface^.pixels ) + x * 4 + 3 + y * Surface^.pitch ))^:= Value;
        end;
    end;

  var
    idist1, idist2, y, x: Integer;
    dist, sy, sx: Integer;
  begin
    Result:= SDL_CreateRGBSurface( SDL_SWSURFACE, AWidth div Downscale, AHeight div Downscale, 32, $000000FF, $0000FF00, $00FF0000, $FF000000 );

    SDL_LockSurface( Result );
    SDL_LockSurface( dist1 );
    SDL_LockSurface( dist2 );

    for y:= 0 to Result^.h - 1 do
      for x:= 0 to Result^.w - 1 do
        begin
{    	  idist1:= round( sqrt( DistSq( GetPixel( dist1, x, y ))));
    	  idist2:= round( sqrt( DistSq( GetPixel( dist2, x, y ))));
          dist:= Max( 0, Min( 255, ( idist2 - idist1 ) * 8 + 128 ));}
          dist:= 0;
          for sy:= 0 to Downscale -1 do
            for sx:= 0 to Downscale -1 do
              begin
                idist1:= round( sqrt( DistSq( GetPixel( dist1, x * Downscale + sx, y * Downscale + sy ))));
                idist2:= round( sqrt( DistSq( GetPixel( dist2, x * Downscale + sx, y * Downscale + sy ))));
                dist+= Max( 0, Min( 255, ( idist2 - idist1 ) * 3 + 128 ));
              end;
          dist:= dist div ( Downscale * Downscale );

    		// Clamp and scale it, just for display purposes.
          SetPixel( Result, x, y, dist );
        end;
    SDL_UnlockSurface( Result );
    SDL_UnlockSurface( dist1 );
    SDL_UnlockSurface( dist2 );
  end;

var
  Surface, dist1, dist2: PSDL_Surface;
  FN: String;
begin
  CreateBlock( AData );
  FN:= P3DSearchPaths.FindFile( AFont.FontName + '.p3d' );
  if (( FN > '' ) and ( FileAge( AFont.FileWatch.FileName ) < FileAge( FN ))) then
    //LoadFromFile( FN + '?Fonts["' + AFont.FontName + '"]' )
  else
    begin
      Name:= AFont.FontName;
      Surface:= SDL_CreateRGBSurface( SDL_SWSURFACE, AWidth, AHeight, 32, $000000FF, $0000FF00, $00FF0000, $FF000000 );
      DrawText( Surface );

      dist1:= CalculateSignedDistanceField( Surface, 0 );
      dist2:= CalculateSignedDistanceField( Surface, 1 );
      SDL_FreeSurface( Surface );

      Surface:= CombineAndDownscale( dist1, dist2, 8 );

      SDL_FreeSurface( dist1 );
      SDL_FreeSurface( dist2 );

      Texture:= TP3DTexture.CreateFromSurface( Surface, AData );

      SDL_FreeSurface( Surface );

      Texture.MipMap:= False;

      Texture.SaveToFile( ExtractFilePath( AFont.FileWatch.FileName ) + AFont.FontName + '.png' );
      Texture.FileWatch.FileName:= AFont.FontName + '.png';
      //SaveToFile( ExtractFilePath( Font.FileWatch.FileName ) + Font.FontName + '.p3d' );

      MipMapLoadBias:= FMipMapLoadBias;
    end;
end;

destructor TP3DFontBmp.Destroy;
begin
  //Owner.Fonts.Remove( Self );
  inherited Destroy;
end;

{ TP3DMaterialModifierText }

class function TP3DMaterialModifierText.GetIndex: Integer;
begin
  Result:= 550;
end;

constructor TP3DMaterialModifierText.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Reset;
end;

procedure TP3DMaterialModifierText.AddPass(ShaderTree: TP3DShaderNodeTree;
  Material: TP3DMaterialBase);
  function ChangeSocketValueFloat( Node: TP3DShaderNode; Name: String; Value: Float ): TP3DShaderNode;
  var
    Socket: Integer;
  begin
    Socket:= Node.Inputs.FindSocketByName( Name );
    if ( Socket < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    TP3DShaderNodeSocketFloat( Node.Inputs[ Socket ]).Value:= Value;
    Result:= Node;
  end;
begin
  ShaderTree.AddCopy( '06000_Pass_Text_Alpha_Transform' );
end;

procedure TP3DMaterialModifierText.PassToShader(Material: TP3DMaterialBase);
var
  decl: TP3DShaderDecl;
begin
  decl:= Material.Shader.Uniforms.Names[ 'text_smoothing' ];
  if ( Assigned( decl )) then
    decl.AsFloat:= TextSmoothing;
  decl:= Material.Shader.Uniforms.Names[ 'text_basealpha' ];
  if ( Assigned( decl )) then
    decl.AsFloat:= TextAlpha;
  decl:= Material.Shader.Uniforms.Names[ 'text_bias' ];
  if ( Assigned( decl )) then
    decl.AsFloat:= TextBias;
end;

procedure TP3DMaterialModifierText.Reset;
begin
  TextBias:= -0.02;
  TextAlpha:= 1.0;
  TextSmoothing:= 1/32;
end;

{ TP3DFontManagerBmp }


function TP3DFontManagerBmp.GetFont( FontName: String ): TP3DFontBmp;
var
  n: Integer;
begin
  n:= Find( FontName );

  if ( n > -1 ) then
    Result:= Items[ n ]
  else
    Result:= FontsByP3DFont[ P3DFontManager[ FontName, round( DefaultTextSize )]];
end;

function TP3DFontManagerBmp.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

function TP3DFontManagerBmp.GetFontByP3DFont( Font: TP3DFont ): TP3DFontBmp;
begin
  Result:= Items[ AddByP3DFont( Font )];
end;

function TP3DFontManagerBmp.GetItems( Index: Integer ): TP3DFontBmp;
begin
  Result:= FItems[ Index ];
end;

function TP3DFontManagerBmp.GetMaterial: TP3DMaterialBase;
begin
  Result:= FMaterial.AsValue;
end;

function TP3DFontManagerBmp.GetTextModifier: TP3DMaterialModifierText;
begin
  Result:= TP3DMaterialModifierText( FTextModifier.AsValue );
end;

procedure TP3DFontManagerBmp.SetCount(AValue: Integer);
begin
  FItems.Count:= AValue;
end;

procedure TP3DFontManagerBmp.SetItems( Index: Integer ; AValue: TP3DFontBmp);
begin
  FItems[ Index ]:= AValue;
end;

procedure TP3DFontBmp.SetMipMapLodBias(AValue: Single);
begin
  FMipMapLoadBias:= AValue;
  if ( Assigned( Texture )) then
    begin
      Texture.Bind();
      glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_LOD_BIAS, MipMapLoadBias );
    end;
end;

class function TP3DFontBmp.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Fonts' );
end;

constructor TP3DFontManagerBmp.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  FItems:= TP3DFontBmpPointerList.Create( 'Items' );
  DefaultTexWidth:= 2048;
  DefaultTexHeight:= 2048;
  DefaultTextSize:= 48*2;
  DefaultTextMargin:= 48;
  FMaterial:= TP3DMaterialBaseContainer.Create( 'Material' );
  Properties.Add( FMaterial );
  FMaterial.AsValue:= TP3DMaterialBase.CreateBlock();
  Material.Name:= 'FontBmpMaterial';
  Material.FUnlit:= True;
  FTextModifier:= TP3DMaterialModifierContainer.Create( 'TextModifier' );
  Properties.Add( FTextModifier );
  FTextModifier.AsValue:= TP3DMaterialModifierText.Create( FTextModifier );
  Material.Modifiers.Add( TextModifier );
end;

destructor TP3DFontManagerBmp.Destroy;
begin
  inherited Destroy;
end;

function TP3DFontManagerBmp.AddByP3DFont(Font: TP3DFont): Integer;
var
  OldSize: Integer;
  Fnt: TP3DFontBmp;
  Lib: TP3DLibrary;
begin
  Result:= Find( Font.FontName );
  if ( Result < 0 ) then
    begin
      OldSize:= Font.Size;
      Font.Size:= round( DefaultTextSize );
      Lib:= TP3DLibrary.Create;
      //P3DData.Libraries.Add( Lib );
      Lib.JSONLoader.FileName:= ChangeFileExt( Font.FileWatch.FileName, '.p3d' );
      Lib.IsDesignerPackage:= True;
      Fnt:= TP3DFontBmp.CreateByFont( Font, DefaultTexWidth, DefaultTexHeight, DefaultTextMargin, Lib );
      {Lib.CreateDOMFile();
      Lib.SaveToDOM();
      Lib.Fonts.SaveToDOM( Lib.DOMFile.DocumentElement );
      Lib.SaveDOMFile();}
      //Lib.SaveToFile();
      Result:= FItems.Add( Fnt );
      Font.Size:= OldSize;
    end;
end;

function TP3DFontManagerBmp.Find(FontName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = FontName ) then
      begin
        Result:= i;
        break;
      end;
end;

function p3dTextSimpleBmp(Text: String; Font: TP3DFontBmp; fs: Single): TP3DTextBmp;
var
  c: Char;
  p: TVec2;
  max_y: Float;
begin
  Result:= TP3DTextBmp.Create;
  Result.FFont:= Font;
  p:= vec2( 0 );
  max_y:= 0;
  for c in Text do
    begin
      p:= Result.AppendLetter( c, fs, p );
      max_y:= max( max_y, p.y );
      p.Y:= 0;
    end;
  Result.Vertices.PushData;
  Result.TexCoords.PushData;
  Result.Indices.PushData;
  Result.FWidth:= p.x;
  Result.FHeight:= max_y;
end;


{ TP3DFontLetter }

procedure TP3DFontLetter.SetLetter(AValue: WideChar);
begin
  if FLetter=AValue then Exit;
  FLetter:=AValue;
  Name:= Letter;
end;

constructor TP3DFontLetter.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Letter', smText ));
  Properties.Add( TP3DPropAccessVec2.CreateField( 'uv1', @Fuv1, smText ));
  Properties.Add( TP3DPropAccessVec2.CreateField( 'uv2', @Fuv2, smText ));
  Properties.Add( TP3DPropAccessVec2.CreateField( 'uv1_metrics', @Fuv1_metrics, smText ));
  Properties.Add( TP3DPropAccessVec2.CreateField( 'uv2_metrics', @Fuv2_metrics, smText ));
end;

destructor TP3DFontLetter.Destroy;
begin
  if ( Assigned( Texture ) and P3DDataBlockCache.IsValid( Texture )) then
    FreeAndNil( FTexture );
  inherited Destroy;
end;

{$ENDIF}

{$IFDEF INITIALIZATION}
  if ( not Assigned( P3DFontManagerBmp )) then
    P3DFontManagerBmp:= TP3DFontManagerBmp.Create;
{$ENDIF}

{$IFDEF FINALIZATION}
  if ( Assigned( P3DFontManagerBmp )) then
    FreeAndNil( P3DFontManagerBmp );
{$ENDIF}
