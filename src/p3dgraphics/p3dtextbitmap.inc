{$IFDEF INTERFACE}
type
  TP3DFontLetter = class
    uv1, uv2: TVec2;
    Texture: TP3DTexture;
    Letter: WideChar; //UNICODE
  end;

  TCustomFontLetterList = specialize gP3DCustomObjectList < TP3DFontLetter >;

  { TP3DFontLetterList }

  TP3DFontLetterList = class ( TCustomFontLetterList )
    function Find( C: WideChar ): Integer;
  end;

  { TP3DFontBmp }

  TP3DFontBmp = class
    private
      FFileName: String;
      FFontName: String;
      FLetters: TP3DFontLetterList;
      FSizeFactor: Single;
      FTexture: TP3DTexture;

      procedure ApplyShader;
      procedure SetFileName(AValue: String);

    public
      constructor Create;
      constructor CreateByFont( Font: TP3DFont; Width, Height, Margin: Integer );

      destructor Destroy; override;
      constructor CreateFromDOM( XML: TDOMElement );
      constructor CreateFromFile( FName: String );
      procedure LoadFromDOM( XML: TDOMElement );
      procedure SaveToDOM(XML: TDOMDocument);
      procedure SaveToFile( FName: String );

    published
      property Letters: TP3DFontLetterList read FLetters write FLetters;
      property Texture: TP3DTexture read FTexture write FTexture;
      property FileName: String read FFileName write SetFileName;
      property FontName: String read FFontName;
      property SizeFactor: Single read FSizeFactor write FSizeFactor;
  end;

TP3DCustomFontListBmp = specialize gP3DCustomObjectList < TP3DFontBmp >;

{ TP3DFontManagerBmp }

TP3DFontManagerBmp = class ( TP3DCustomFontListBmp )
  private
    FMaterial: TP3DMaterialBase;
    function GetFont( FontName: String ): TP3DFontBmp;

  public
    property Fonts [ FontName: String ]: TP3DFontBmp read GetFont; default;

  published
    function Find( FontName: String ): Integer;
    property Material: TP3DMaterialBase read FMaterial write FMaterial;
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
  v3:= p + ( letter.uv2 - letter.uv1 ) * Font.SizeFactor * fs;
  v2:= vec2( v3.x, v1.y );
  v4:= vec2( v1.x, v3.y );
  tc1:= letter.uv1;
  tc3:= letter.uv2;
  tc2:= vec2( tc3.x, tc1.y );
  tc4:= vec2( tc1.x, tc3.y );

  n:= Vertices.Add([ v1, v2, v3, v4 ]);
  TexCoords.Add([ tc1, tc2, tc3, tc4 ]);
  Indices.Add([ n + 0, n + 1, n + 2, n + 0, n + 2, n + 3 ]);
  LetterPosAndSize.Add( vec4( v1, v3 - v1 ));
  Result:= v3;
end;

constructor TP3DTextBmp.Create;
begin
  inherited;
  Vertices:= TP3DVec2BufferGL.Create( True );
  Vertices.SetAttribArray( 0 );
  TexCoords:= TP3DVec2BufferGL.Create( True );
  TexCoords.SetAttribArray( 4 );
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
  inherited;
end;

procedure TP3DTextBmp.Render( p: TVec2; Color: TVec4; proj: TMat4 );
var
  Shader: TP3DShader;
  Uniform: TP3DShaderDecl;
  mt, m: TMat4;
begin
  Font.ApplyShader;

  if ( not Assigned( P3DFontManager.Material.Shader )) then
    exit;

  Shader:= P3DFontManager.Material.Shader;


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

{ TP3DFontBmp }

procedure TP3DFontBmp.ApplyShader;
begin
  if ( P3DFontManager.Material.Maps.Count = 0 ) then
    P3DFontManager.Material.Maps.Add( TP3DMaterialMapBase.Create( P3DFontManager.Material ));
  P3DFontManager.Material.Maps[ 0 ].Map:= Texture;
  TP3DMaterialMapBase( P3DFontManager.Material.Maps[ 0 ]).AlphaFactor:= 1;
  if ( not Assigned( P3DFontManager.Material.Shader )) then
    P3DFontManager.Material.BuildShader();
  P3DFontManager.Material.Shader.Enable;
  P3DFontManager.Material.PassToShader();
end;

procedure TP3DFontBmp.SetFileName(AValue: String);
begin
  FFileName:= AValue;
end;

constructor TP3DFontBmp.Create;
begin
  inherited Create;
  Letters:= TP3DFontLetterList.Create;
  SizeFactor:= 1;
end;

constructor TP3DFontBmp.CreateByFont(Font: TP3DFont; Width, Height,
  Margin: Integer);

  procedure DrawText( ASurface: PSDL_Surface );
  var
    i: Integer;
    sz, oldsz: TPoint;
    low: Integer = Integer( ' ');
    high: Integer = 255;
    color: Byte;
    cl: TSDL_Color;
    minx, maxx, miny, maxy, advance, e: Integer;
    fnt: PTTF_Font;
    text_surf: PSDL_Surface;
    r1, r2: Classes.TRect;
    letter: TP3DFontLetter;
  begin
    color:= 0;

    FFontName:= Font.FontName;
    fnt:= Font.Font;
    SizeFactor:= TTF_FontHeight( fnt );

    cl.a:= 255;
    cl.r:= 255;
    cl.g:= 255;
    cl.b:= 255;

    sz:= Point( 0, 0 );
    oldsz:= Point( Margin, Margin );

    SDL_FillRect( ASurface, nil, SDL_MapRGBA( ASurface^.format, 255, 255, 255, 0 ));
    for i:= low to high do
      if ( Boolean( TTF_GlyphIsProvided( fnt, i ))) then
        begin
          e:= TTF_SizeText( fnt, PChar( AnsiString( Char( i ))), @sz.X, @sz.Y );
          if ( e <> 0 ) then
            continue;
          if ( oldsz.Y + sz.Y + Margin > Height ) then
            break;

          if ( oldsz.X + sz.X + Margin * 2 > Width ) then
            oldsz:= Point( 0, oldsz.y + sz.Y + Margin ); //Skip Line

          text_surf:= TTF_RenderText_Blended( fnt, PChar( AnsiString( Char( i ))), cl );
          SDL_SetSurfaceBlendMode( text_surf, SDL_BLENDMODE_BLEND );
          SDL_SetSurfaceBlendMode( ASurface, SDL_BLENDMODE_BLEND );
          r1:= Rect( 0, 0, text_surf^.w, text_surf^.h );
          r2:= Rect( oldsz.X, oldsz.Y, text_surf^.w + oldsz.X + Margin, oldsz.Y + text_surf^.h );

          SDL_BlitSurface( text_surf, PSDL_Rect( @r1 ), ASurface, PSDL_Rect( @r2 ));
          SDL_FreeSurface( text_surf );

          //TTF_GlyphMetrics( P3DFontManager.Fonts[ canvas.Font.Name, canvas.Font.Size ].Font, i,
          //                  @minx, @maxx, @miny, @maxy, @advance );

          letter:= TP3DFontLetter.Create;
          letter.Letter:= WideChar( i );
          letter.uv1:= vec2( oldsz.X / Width, oldsz.Y / Height );
          letter.uv2:= vec2(( oldsz.X + sz.X ) / Width, ( oldsz.Y + sz.Y ) / Height );
          Letters.Add( letter );
          oldsz.x:= oldsz.x + sz.x + Margin;
        end;
  end;

var
  Surface: PSDL_Surface;
begin
  Create;

  Surface:= SDL_CreateRGBSurface( 0, Width, Height, 32, $000000FF, $0000FF00, $00FF0000, $FF000000 );
  DrawText( Surface );

  Texture:= TP3DTexture.CreateFromSurface( Surface );
  Texture.FileWatch.FileName:= Font.FontName + '.png';
  SDL_FreeSurface( Surface );
end;

destructor TP3DFontBmp.Destroy;
begin
  Letters.Free;
  inherited Destroy;
end;

constructor TP3DFontBmp.CreateFromDOM(XML: TDOMElement);
begin
  Create;

  try
    LoadFromDOM( XML );
  finally
    XML.Free;
  end;
end;

constructor TP3DFontBmp.CreateFromFile(FName: String);
var
  dir: String;
  xml: TXMLDocument;
begin
  if ( lowercase( ExtractFileExt( FName )) = '.p3d' ) then
    begin
      FileName:= P3DSearchPaths.Models.FindFileEx( FName, GetCurrentDir );
      if ( FileName = '' ) then
        raise Exception.Create( Format( 'Cannot load font from file! The specified file "%s" was not found!', [ FName ]));
      ReadXMLFile( xml, FileName );
      dir:= GetCurrentDir;
      try
        xml.DocumentElement.SetAttribute( 'filename', FileName );
        SetCurrentDir( ExtractFilePath( FileName ));

        if ( not ( xml.DocumentElement.TagName = 'font' )) then
          raise Exception.Create( Format( 'Cannot load font from file! The specified file "%s" is not a font file!', [ FName ]));

        CreateFromDOM( xml.DocumentElement );
      finally
        SetCurrentDir( dir );
        xml.Free;
      end;
    end
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
  FSizeFactor:= StrToFloatDef( XML.Attributes.GetNamedItem( 'size' ).NodeValue, 1 / Texture.Height ) * Texture.Height;
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
  root.AttribStrings[ 'size' ]:= FloatToStr( FSizeFactor / Texture.Height );
  for letter in Letters do
    begin
      node:= TDOMElement( root.AppendChild( root.OwnerDocument.CreateElement( 'char' )));

      node.AttribStrings[ 'char' ]:= letter.Letter;
      node.AttribStrings[ 'uv1' ]:= letter.uv1.ToString();
      node.AttribStrings[ 'uv2' ]:= letter.uv2.ToString();
    end;
end;

procedure TP3DFontBmp.SaveToFile(FName: String);
var
  XML: TXMLDocument;
begin
  XML:= TXMLDocument.Create;
  SaveToDOM( XML );
  WriteXML( XML, FName );
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
    Result:= nil;
end;

function TP3DFontManagerBmp.Find(FontName: String): Integer;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ].FontName = FontName ) then
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
