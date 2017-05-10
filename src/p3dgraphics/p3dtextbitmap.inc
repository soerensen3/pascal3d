{$IFDEF INTERFACE}
type

  { TP3DFontLetter }

  TP3DFontLetter = class ( TP3DInterfacedPersistent )
  private
    Fuv1, Fuv2: TVec2;
    FTexture: TP3DTexture;
    FLetter: WideChar; //UNICODE

  public
    constructor Create;
    destructor Destroy; override;
    class function DOMNodeName: String; override;

    property Texture: TP3DTexture read FTexture write FTexture;
    property uv1: TVec2 read Fuv1 write Fuv1;
    property uv2: TVec2 read Fuv2 write Fuv2;

  published
    property Letter: WideChar read FLetter write FLetter;
  end;

  TCustomFontLetterList = specialize gP3DCustomObjectList < TP3DFontLetter >;

  { TP3DFontLetterList }

  TP3DFontLetterList = class ( TCustomFontLetterList )
    function Find( C: WideChar ): Integer;
    function SaveToDOM(AParent: TDOMElement ): TDOMElement;
    procedure LoadFromDOM( ADOMNode: TDOMElement );
  end;

  { TP3DFontBmp }

  TP3DFontBmp = class ( TP3DDataBlock )
    private
      FFileName: String;
      FLetters: TP3DFontLetterList;
      FMargin: Single;
      FMipMapLoadBias: Single;
      FSizeFactor: Single;
      FTexture: TP3DTexture;

      procedure ApplyShader;
      procedure SetFileName(AValue: String);
      procedure SetMipMapLodBias(AValue: Single);

    public
      constructor Create( const AData: TP3DLibrary = nil ); override;
      constructor CreateByFont( Font: TP3DFont; Width, Height, Margin: Integer; const AData: TP3DLibrary = nil );

      procedure FastDestroy; override;

      function SaveToDOM( AParent: TDOMElement ): TDOMElement; override;
      procedure LoadFromDOMNew( ADOMNode: TDOMElement ); override;
      class function DOMNodeName: String; override;

    published
      property Letters: TP3DFontLetterList read FLetters write FLetters;
      property Texture: TP3DTexture read FTexture write FTexture;
      property SizeFactor: Single read FSizeFactor write FSizeFactor;
      property Margin: Single read FMargin write FMargin;
      property MipMapLoadBias: Single read FMipMapLoadBias write SetMipMapLodBias;
  end;

{ TP3DFontListBmp }

TP3DFontBmpList = class ( specialize gP3DCustomObjectList < TP3DFontBmp >)
  procedure SaveToDOM( AParent: TDOMElement );
end;

TP3DMaterialModifierText = class ( TP3DMaterialModifier )
  private
    FTextAlpha: Float;
    FTextBias: Float;
    FTextSmoothing: Float;

  public
    constructor Create;

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

TP3DFontManagerBmp = class ( TP3DFontBmpList )
  private
    FDefaultTexHeight: Integer;
    FDefaultTextMargin: Integer;
    FDefaultTextSize: Single;
    FDefaultTexWidth: Integer;
    FMaterial: TP3DMaterialBase;
    FTextModifier: TP3DMaterialModifierText;
    function GetFont( FontName: String ): TP3DFontBmp;
    function GetFontByP3DFont( Font: TP3DFont ): TP3DFontBmp;

  public
    constructor Create;
    destructor Destroy; override;

    function AddByP3DFont( Font: TP3DFont ): Integer;
    property Fonts[ FontName: String ]: TP3DFontBmp read GetFont; default;
    property FontsByP3DFont[ Font: TP3DFont ]: TP3DFontBmp read GetFontByP3DFont;

  published
    function Find( FontName: String ): Integer;
    property Material: TP3DMaterialBase read FMaterial write FMaterial;
    property DefaultTexWidth: Integer read FDefaultTexWidth write FDefaultTexWidth;
    property DefaultTexHeight: Integer read FDefaultTexHeight write FDefaultTexHeight;
    property DefaultTextSize: Single read FDefaultTextSize write FDefaultTextSize;
    property DefaultTextMargin: Integer read FDefaultTextMargin write FDefaultTextMargin;
    property TextModifier: TP3DMaterialModifierText read FTextModifier write FTextModifier;
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

TP3DPropertyAccessFontBmp = specialize gP3DPropertyAccessDataBlock < TP3DFontBmp >;
TP3DPropertyAccessFontBmpList = specialize gP3DListPropertyAccess < TP3DFontBmpList, TP3DFontBmp, TP3DPropertyAccessFontBmp >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
function TP3DTextBmp.AppendLetter( L: WideChar; fs: Single; p: TVec2 ): TVec2;
var
  v1,v2,v3,v4: TVec2;
  tc1, tc2, tc3, tc4: TVec2;
  n: Integer;
  letter: TP3DFontLetter;
begin
  Result:= p;

  n:= Font.Letters.Find( L );
  if ( n < 0 ) then
    exit;

  FText+= L;

  v1:= p;
  letter:= Font.Letters[ n ];
  v3:= p + ( letter.uv2 - letter.uv1 ) / Font.SizeFactor * fs;
  v2:= vec2( v3.x, v1.y );
  v4:= vec2( v1.x, v3.y );
  tc1:= letter.uv1;
  tc3:= letter.uv2;
  tc2:= vec2( tc3.x, tc1.y );
  tc4:= vec2( tc1.x, tc3.y );

  n:= Vertices.Add([ v1 - Font.Margin, v2 - Font.Margin, v3 + Font.Margin, v4 + Font.Margin ]);
  TexCoords.Add([ tc1, tc2, tc3, tc4 ]);
  Indices.Add([ n + 0, n + 1, n + 2, n + 0, n + 2, n + 3 ]);
  LetterPosAndSize.Add( vec4( v1, v3 - v1 ));
  Result:= v3;
end;

constructor TP3DTextBmp.Create;
begin
  inherited;
  Vertices:= TP3DVec2BufferGL.Create( True );
  Vertices.SetAttribArray( P3DAttribPosition );
  TexCoords:= TP3DVec2BufferGL.Create( True );
  TexCoords.SetAttribArray( P3DAttribTexCoord0 );
  Indices:= TP3DIntBufferGL.Create( True );
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
  Uniform: TP3DShaderDecl;
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
  TexCoords.SetAttribArray( P3DAttribTexCoord0 );
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
        n:= Font.Letters.Find( 'W' );
        if ( n = -1 ) then
          Result:= vec4( 0 )
        else
          Result:= vec4( vec2( 0 ), Font.Letters.Items[ n ].uv2 - Font.Letters.Items[ n ].uv1 );
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

function TP3DFontLetterList.SaveToDOM(AParent: TDOMElement): TDOMElement;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure TP3DFontLetterList.LoadFromDOM(ADOMNode: TDOMElement);
var
  lst: TDOMNodeList;
  i: Integer;
begin
  Clear();
  lst:= ADOMNode.GetElementsByTagName( TP3DFontLetter.DOMNodeName );
  try
    Count:= lst.Count;
    for i:= 0 to lst.Count - 1 do
      begin
        Items[ i ]:= TP3DFontLetter.Create;
        Items[ i ].LoadFromDOMNew( TDOMElement( lst[ i ]));
      end;

  finally
    lst.Free;
  end;
end;

{ TP3DFontBmp }

procedure TP3DFontBmp.ApplyShader;
begin
  if ( P3DFontManagerBmp.Material.Maps.Count = 0 ) then
    P3DFontManagerBmp.Material.Maps.Add( TP3DMaterialMapBase.Create( P3DFontManagerBmp.Material ));
  P3DFontManagerBmp.Material.Maps[ 0 ].Map:= Texture;
  TP3DMaterialMapBase( P3DFontManagerBmp.Material.Maps[ 0 ]).AlphaFactor:= 1;
  TP3DMaterialMapBase( P3DFontManagerBmp.Material.Maps[ 0 ]).DiffuseFactor:= 0;
  if ( not Assigned( P3DFontManagerBmp.Material.Shader )) then
    P3DFontManagerBmp.Material.BuildShader();
  P3DFontManagerBmp.Material.PassToShader();
end;

procedure TP3DFontBmp.SetFileName(AValue: String);
begin
  FFileName:= AValue;
end;

constructor TP3DFontBmp.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  Letters:= TP3DFontLetterList.Create;
  SizeFactor:= 1;
  MipMapLoadBias:= 0;//-0.65;
  Margin:= 0;

  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'SizeFactor', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'MipMapLoadBias', smAttribute ));
  Properties.Add( TP3DPropertyAccessTexture.CreateField( 'Texture', @FTexture, smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Margin', smAttribute ));
  Owner.Fonts.Add( Self );

  {property Letters: TP3DFontLetterList read FLetters write FLetters;
  property Texture: TP3DTexture read FTexture write FTexture;
  property FileName: String read FFileName write SetFileName;
  property FontName: String read FFontName;
  property SizeFactor: Single read FSizeFactor write FSizeFactor;
  property MipMapLoadBias: Single read FMipMapLoadBias write SetMipMapLodBias;}
end;

constructor TP3DFontBmp.CreateByFont(Font: TP3DFont; Width, Height, Margin: Integer; const AData: TP3DLibrary);

  procedure DrawText( ASurface: PSDL_Surface; Font: TP3DFont );
  var
    i, e: Integer;
    sz, oldsz: TPoint;
    low: Integer = Integer( ' ');
    high: Integer = 255;
    cl: TSDL_Color;
//    minx, maxx, miny, maxy, advance, e: Integer;
    fnt: PTTF_Font;
    text_surf: PSDL_Surface;
    r1, r2: Classes.TRect;
    letter: TP3DFontLetter;
  begin
    {if ( Font.Size <> P3DFontManagerBmp.DefaultTextSize ) then
      Font:= P3DFontManager.Fonts[ Font.FontName, round( P3DFontManagerBmp.DefaultTextSize )];}
//    FFontName:= Font.FontName;
    fnt:= TTF_OpenFont( PChar( Font.FileWatch.FileName ), round( P3DFontManagerBmp.DefaultTextSize ));
    SizeFactor:= TTF_FontHeight( fnt ) / ASurface^.h;
    FMargin:= Margin / ASurface^.h;
    WriteLn( Font.FontName + ' Font Size: ', TTF_FontHeight( fnt ), ' Pt', Font.Size );

    cl.r:= 0;
    cl.g:= 0;
    cl.b:= 255;    // Pixels should have 1 for inside in r and g channel and the opposite in b and a
    cl.a:= 255;

    sz:= Point( 0, 0 );
    oldsz:= Point( Margin, Margin );

    SDL_FillRect( ASurface, nil, SDL_MapRGBA( ASurface^.format, 255, 255, 0, 0 )); //Initialize pixels for distance fields
    for i:= low to high do
      if ( Boolean( TTF_GlyphIsProvided( fnt, i ))) then
        begin
          e:= TTF_SizeText( fnt, PChar( AnsiString( Char( i ))), @sz.X, @sz.Y );
          if ( e <> 0 ) then
            continue;
          if ( oldsz.Y + sz.Y + Margin > ASurface^.h ) then
            break;

          if ( oldsz.X + sz.X + Margin * 2 > ASurface^.w ) then
            oldsz:= Point( 0, oldsz.y + sz.Y + Margin ); //Skip Line

          text_surf:= TTF_RenderText_Solid( fnt, PChar( AnsiString( Char( i ))), cl );
          //SDL_SetSurfaceBlendMode( text_surf, SDL_BLENDMODE_BLEND );
          //SDL_SetSurfaceBlendMode( ASurface, SDL_BLENDMODE_BLEND );
          r1:= Rect( 0, 0, text_surf^.w, text_surf^.h );
          r2:= Rect( oldsz.X, oldsz.Y, text_surf^.w + oldsz.X + Margin, oldsz.Y + text_surf^.h );

          SDL_BlitSurface( text_surf, PSDL_Rect( @r1 ), ASurface, PSDL_Rect( @r2 ));
          SDL_FreeSurface( text_surf );

          //TTF_GlyphMetrics( P3DFontManager.Fonts[ canvas.Font.Name, canvas.Font.Size ].Font, i,
          //                  @minx, @maxx, @miny, @maxy, @advance );

          letter:= TP3DFontLetter.Create;
          letter.Letter:= WideChar( i );
          letter.uv1:= vec2( oldsz.X / ASurface^.w, oldsz.Y / ASurface^.h );
          letter.uv2:= vec2(( oldsz.X + sz.X ) / ASurface^.w, ( oldsz.Y + sz.Y ) / ASurface^.h );
          Letters.Add( letter );
          oldsz.x:= oldsz.x + sz.x + Margin;
        end;
    TTF_CloseFont( fnt );
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
    Result:= SDL_CreateRGBSurface( SDL_SWSURFACE, Width, Height, 32, $000000FF, $0000FF00, $00FF0000, $FF000000 );

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
    Result:= SDL_CreateRGBSurface( SDL_SWSURFACE, Width div Downscale, Height div Downscale, 32, $000000FF, $0000FF00, $00FF0000, $FF000000 );

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
  Create( AData );
  FN:= P3DSearchPaths.FindFile( Font.FontName + '.p3d' );
  if (( FN > '' ) and ( FileAge( Font.FileWatch.FileName ) < FileAge( FN ))) then
    LoadFromFile( FN + '?' + Font.FontName )
  else
    begin
      Name:= Font.FontName;
      Surface:= SDL_CreateRGBSurface( SDL_SWSURFACE, Width, Height, 32, $000000FF, $0000FF00, $00FF0000, $FF000000 );
      DrawText( Surface, Font );

      dist1:= CalculateSignedDistanceField( Surface, 0 );
      dist2:= CalculateSignedDistanceField( Surface, 1 );
      SDL_FreeSurface( Surface );

      Surface:= CombineAndDownscale( dist1, dist2, 8 );

      SDL_FreeSurface( dist1 );
      SDL_FreeSurface( dist2 );

      Texture:= TP3DTexture.CreateFromSurface( Surface, AData );

      SDL_FreeSurface( Surface );

      Texture.MipMap:= False;

      Texture.SaveToFile( ExtractFilePath( Font.FileWatch.FileName ) + Font.FontName + '.png' );
      Texture.FileWatch.FileName:= Font.FontName + '.png';
      //SaveToFile( ExtractFilePath( Font.FileWatch.FileName ) + Font.FontName + '.p3d' );

      MipMapLoadBias:= FMipMapLoadBias;
    end;
end;

procedure TP3DFontBmp.FastDestroy;
begin
  Letters.Clear( True );
  Letters.Free;
  Owner.Fonts.Remove( Self );
  inherited FastDestroy;
end;

class function TP3DFontBmp.DOMNodeName: String;
begin
  Result:= 'font';
end;

{

constructor TP3DFontBmp.CreateFromDOM(XML: TDOMElement);
begin
  Create;

  LoadFromDOM( XML );
end;

constructor TP3DFontBmp.CreateFromFile(FName: String);
begin
  Create();
  LoadFromFile( FName );
end;

procedure TP3DFontBmp.LoadFromDOM(XML: TDOMElement);
var
  letter: TP3DFontLetter;
  child: TDOMNode;
  i: Integer;
  tex: String;
begin
  tex:= XML.Attributes.GetNamedItem( 'texture' ).NodeValue;
  FFontName:= XML.Attributes.GetNamedItem( 'name' ).NodeValue;
  Texture:= TP3DTexture.CreateFromFile( tex );
  MipMapLoadBias:= FMipMapLoadBias;
  Texture.MipMap:= False;
  FSizeFactor:= StrToFloatDef( XML.Attributes.GetNamedItem( 'size' ).NodeValue, 1 );
  for i:= 0 to XML.ChildNodes.Count - 1 do
    if ( XML.ChildNodes[ i ].NodeName = 'char' ) then
      begin
        child:= XML.ChildNodes[ i ];

        letter:= TP3DFontLetter.Create;
        Letters.Add( letter );
        letter.Letter:= child.Attributes.GetNamedItem( 'char' ).NodeValue[ 1 ];
        letter.uv1.FromString( child.Attributes.GetNamedItem( 'uv1' ).NodeValue );
        letter.uv2.FromString( child.Attributes.GetNamedItem( 'uv2' ).NodeValue );
        {$IFDEF DEBUG}
        WriteLn( 'letter ''' + letter.Letter + '''' );
        WriteLn( 'uv1 ''' + letter.uv1.ToString() + '''' );
        WriteLn( 'uv2 ''' + letter.uv2.ToString() + '''' );
        {$ENDIF}
      end;
end;

procedure TP3DFontBmp.LoadFromFile(FileName: String);
var
  xml: TXMLDocument;
  dir: String;
begin
  if ( lowercase( ExtractFileExt( FileName )) = '.p3d' ) then
    begin
      FileWatch.FileName:= P3DSearchPaths.FindFileEx( FileName, GetCurrentDir );
      if ( FileWatch.FileName = '' ) then
        raise Exception.Create( Format( 'Cannot load font from file! The specified file "%s" was not found!', [ FileName ]));
      ReadXMLFile( xml, FileWatch.FileName );
      dir:= GetCurrentDir;
      try
        xml.DocumentElement.SetAttribute( 'filename', FileWatch.FileName );
        SetCurrentDir( ExtractFilePath( FileWatch.FileName ));

        if ( not ( xml.DocumentElement.TagName = 'p3dfile' )) then
          raise Exception.Create( Format( 'Cannot load font from file! The specified file "%s" is not a font file!', [ FileName ]));

        LoadFromDOMNew( xml.DocumentElement );
      finally
        SetCurrentDir( dir );
        xml.Free;
      end;
    end
end;}

function TP3DFontBmp.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM(AParent);
  Letters.SaveToDOM( Result );
end;

procedure TP3DFontBmp.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew( ADOMNode );
  Letters.LoadFromDOM( ADOMNode );
end;

{
procedure TP3DFontBmp.SaveToDOM(XML: TDOMDocument );
var
  letter: TP3DFontLetter;
  node, root: TDOMElement;
  i: Integer;
begin
  XML.AppendChild( XML.CreateElement( 'font' ));
  root:= TDOMElement( XML.DocumentElement );
  root.AttribStrings[ 'texture' ]:= Texture.FileWatch.FileName;
  root.AttribStrings[ 'name' ]:= FontName;
  root.AttribStrings[ 'size' ]:= FloatToStr( FSizeFactor );
  for letter in Letters do
    begin
      node:= TDOMElement( root.AppendChild( root.OwnerDocument.CreateElement( 'char' )));

      node.AttribStrings[ 'char' ]:= letter.Letter;
      node.AttribStrings[ 'uv1' ]:= letter.uv1.ToString();
      node.AttribStrings[ 'uv2' ]:= letter.uv2.ToString();
    end;
end;

procedure TP3DFontBmp.SaveToFile(FileName: String);
var
  XML: TXMLDocument;
begin
  XML:= TXMLDocument.Create;
  XML.AppendChild( XML.CreateElement( 'p3dfile' ));
  SaveToDOM( XML.DocumentElement );
  WriteXML( XML, FileName );
end;
}
{ TP3DMaterialModifierText }

class function TP3DMaterialModifierText.GetIndex: Integer;
begin
  Result:= 550;
end;

constructor TP3DMaterialModifierText.Create;
begin
  inherited Create;
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

function TP3DFontManagerBmp.GetFontByP3DFont( Font: TP3DFont ): TP3DFontBmp;
begin
  Result:= Items[ AddByP3DFont( Font )];
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

constructor TP3DFontManagerBmp.Create;
begin
  inherited Create;
  DefaultTexWidth:= 2048;
  DefaultTexHeight:= 2048;
  DefaultTextSize:= 48*2;
  DefaultTextMargin:= 48;
  Material:= TP3DMaterialBase.Create();
  Material.Name:= 'FontBmpMaterial';
  Material.FUnlit:= True;
  TextModifier:= TP3DMaterialModifierText.Create;
  Material.Modifiers.Add( TextModifier );
end;

destructor TP3DFontManagerBmp.Destroy;
begin
  Material.Free;
  Clear( True );
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
      Lib:= P3DData.Libraries[ P3DData.Libraries.Add( TP3DLibrary.Create )];
      Lib.FileWatch.FileName:= ChangeFileExt( Font.FileWatch.FileName, '.p3d' );
      Lib.IsDesignerPackage:= True;
      Fnt:= TP3DFontBmp.CreateByFont( Font, DefaultTexWidth, DefaultTexHeight, DefaultTextMargin, Lib );
      {Lib.CreateDOMFile();
      Lib.SaveToDOM();
      Lib.Fonts.SaveToDOM( Lib.DOMFile.DocumentElement );
      Lib.SaveDOMFile();}
      Result:= Add( Fnt );
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

{$ENDIF}
