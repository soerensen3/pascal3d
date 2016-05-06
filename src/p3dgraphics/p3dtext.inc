//p3dcanvas.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

type

  TP3DFontManager = class;

	{ TP3DFont }

  TP3DFont = class
    private
      FFileName: String;
      FFontManager: TP3DFontManager;
      FFontName: String;
      FSize: Integer;
      FFont: sdl2_ttf.PTTF_Font;

      procedure SetFileName(AValue: String);

    public
      constructor Create( AFileName: String; ASize: Integer );
      destructor Destroy; override;

      property Font: sdl2_ttf.PTTF_Font read FFont write FFont;

    published
      property FileName: String read FFileName write SetFileName;
      property FontName: String read FFontName;
      property Size: Integer read FSize write FSize;
      property FontManager: TP3DFontManager read FFontManager write FFontManager;
  end;

  TP3DCustomFontList = specialize gP3DCustomObjectList < TP3DFont >;

	{ TP3DFontManager }

  TP3DFontManager = class ( TP3DCustomFontList )
    private
      FDefaultFontsPath: String;
      FMaterial: TP3DMaterialBase;

      function GetFont( FontName: String; Height: Integer ): TP3DFont;

    public
      constructor Create;
      destructor Destroy; override;

      function Add( Item: TP3DFont ): Integer; override;
      function Add( FontName: String; Height: Integer ): Integer;

      property Fonts [ FontName: String; Height: Integer ]: TP3DFont read GetFont; default;

    published
      function Find( FontName: String; Height: Integer ): Integer;
      property Material: TP3DMaterialBase read FMaterial write FMaterial;
      property DefaultFontsPath: String read FDefaultFontsPath write FDefaultFontsPath;
  end;

  { TP3DText }

  TP3DText = class
    private
      FFont: TP3DFont;
      FHeight: Single;
      FLetterPosAndSize: TVec4List;
      FTexture: TP3DTexture;
      FIndices: TP3DIntBufferGL;
      FTexCoords: TP3DVec2BufferGL;
      FVertices: TP3DVec2BufferGL;
      FText: String;
      FWidth: Single;

      procedure ApplyShader;

      //function AppendLetter( L: WideChar; fs: Single; p: TVec2 ): TVec2;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Render( p: TVec2; Color: TVec4 );
      procedure Render( p: TVec2; Color: TVec4; proj: TMat4 );

      function WidthFromTo( idx1: Integer; idx2: Integer ): TPoint;
      procedure BuildIndex();

      property Text: String read FText;
      property Width: Single read FWidth;
      property Height: Single read FHeight;
      property Font: TP3DFont read FFont;
      property Vertices: TP3DVec2BufferGL read FVertices write FVertices;
      property TexCoords: TP3DVec2BufferGL read FTexCoords write FTexCoords;
      property Indices: TP3DIntBufferGL read FIndices write FIndices;
      property LetterPosAndSize: TVec4List read FLetterPosAndSize write FLetterPosAndSize;
      property Texture: TP3DTexture read FTexture write FTexture;
  end;

  function p3dTextSimple( AText: String; AFont: TP3DFont ): TP3DText;
  function p3dTextSize( AText: String; AFont: TP3DFont ): TPoint;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

function p3dTextSimple( AText: String; AFont: TP3DFont ): TP3DText;
var
  cl: TSDL_Color;
  surface: PSDL_Surface;
begin
  Result:= nil;
  cl.a:= 255; cl.r:= 255; cl.g:= 255; cl.b:= 255;

  surface:= TTF_RenderText_Blended( AFont.Font, PChar( AText ), cl );

  if ( not Assigned( surface )) then
    exit;

  Result:= TP3DText.Create;

  with Result do
    begin
      Texture:= TP3DTexture.CreateFromSurface( surface );
      FFont:= AFont;
      FText:= AText;
      FWidth:= surface^.w;
      FHeight:= surface^.h;

      vertices:= TP3DVec2BufferGL.Create( True );
      texcoords:= TP3DVec2BufferGL.Create( True );

      Vertices.Add([ vec2( 0, 0 ), vec2( Width, 0 ), vec2( Width, Height ), vec2( 0, Height )]);
      Vertices.PushData;
      Vertices.SetAttribArray( P3DAttribPosition );
      TexCoords.Add([ vec2( 0, 0 ), vec2( 1, 0 ), vec2( 1, 1 ), vec2( 0, 1 )]);
      TexCoords.SetAttribArray( P3DAttribTexCoord0 );
      TexCoords.PushData;
      Indices:= TP3DIntBufferGL.Create( True );
      Indices.Add([ 0, 1, 2, 0, 2, 3 ]);
      Indices.PushData;
    end;
end;

function p3dTextSize(AText: String; AFont: TP3DFont): TPoint;
begin
  if ( TTF_SizeText( AFont.Font, PChar( AText ), @Result.X, @Result.Y ) <> 0 ) then
    Result:= Point( -1, -1 );
end;

{ TP3DText }

procedure TP3DText.ApplyShader;
begin
  if ( P3DFontManager.Material.Maps.Count = 0 ) then
    P3DFontManager.Material.Maps.Add( TP3DMaterialMapBase.Create );
  P3DFontManager.Material.Maps[ 0 ].Map:= Texture;
  if ( not Assigned( P3DFontManager.Material.Shader )) then
    P3DFontManager.Material.BuildShader();
  P3DFontManager.Material.Shader.Enable;
  P3DFontManager.Material.PassToShader();
end;

constructor TP3DText.Create;
begin
  LetterPosAndSize:= TVec4List.Create;
end;

destructor TP3DText.Destroy;
begin
  LetterPosAndSize.Free;
  FVertices.Free;
  FTexCoords.Free;
  FIndices.Free;
  inherited Destroy;
end;

procedure TP3DText.Render(p: TVec2; Color: TVec4);
var
  cl: GLint;
  vt: GLint;
  tc: GLint;
  tx: GLint;
  mat: GLint;
  mt: TMat4;
  m: TMat4;
  OldShader: TP3DShader;

begin

  {OldShader:= ActShad;
  if ( Assigned( Font.FontManager.Shader )) then
    Font.FontManager.Shader.Enable;
  cl:= ActShad.Attributes.AddrByName( 'in_color' );
  //vt:= ActShad.Attributes.AddrByName( 'in_vertex' );

  //tc:= ActShad.Attributes.AddrByName( 'in_texc0' );
  tx:= ActShad.Uniforms.AddrByName( 'tex0' );
  mat:= ActShad.Uniforms.AddrByName( 'mat' );

  glActiveTexture( GL_TEXTURE0 );
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, Font.Textures[ 0 ].fGLTexture );

  mt:= mat4translate( vec4( p, 0, 1 ));
  m:= mt * p3dgeometry.proj2D;
  glUniform1i( tx, 0 );
  glUniformMatrix4fv( mat, 1, False, @m );
  glVertexAttrib4f( cl, Color.X, Color.Y, Color.Z, Color.A );
  Vertices.SetAttribArray( 0 );
  Vertices.Bind();
  TexCoords.SetAttribArray( 4 );
  TexCoords.Bind();
  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 ));
  glUniformMatrix4fv( mat, 1, False, @p3dgeometry.proj2D );
  if ( Assigned( OldShader )) then
    OldShader.Enable
  else
    ActShad.Disable;}
end;

procedure TP3DText.Render(p: TVec2; Color: TVec4; proj: TMat4);
var
  mt: TMat4;
  m: TMat4;
  Shader: TP3DShader;
  Uniform: TP3DShaderDecl;
begin
  ApplyShader;

  if ( not Assigned( P3DFontManager.Material.Shader )) then
    exit;

  Shader:= P3DFontManager.Material.Shader;

  mt:= mat4translate( vec4( p, 0, 1 ));
  m:= mt * proj;

  if ( Shader.Uniforms.FindByName( 'tex0', Uniform )) then
    Uniform.AsInt:= 0;
  if ( Shader.Uniforms.FindByName( 'proj', Uniform )) then
    Uniform.AsMat4:= m;
  if ( Shader.Uniforms.FindByName( 'view', Uniform )) then
    Uniform.AsMat4:= Mat4Identity;
  if ( Shader.Uniforms.FindByName( 'world', Uniform )) then
    Uniform.AsMat4:= Mat4Identity;

  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A ); cle( Self );
  Vertices.SetAttribArray( P3DAttribPosition );

  TexCoords.SetAttribArray( P3DAttribTexCoord0 );

  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 )); cle( Self );
end;

function TP3DText.WidthFromTo(idx1: Integer; idx2: Integer): TPoint;
begin
  Result:= p3dTextSize( Copy( FText, idx1, idx2-idx1 ), FFont );
end;

procedure TP3DText.BuildIndex;
var
  Cur: TPoint;
  sz: TPoint;
  c: Char;
begin
  LetterPosAndSize.Clear;
  Cur:= Point( 0, 0 );
  //TODO: FIX ERROR ON EMPTY TEXT
  for c in FText do
    begin
      sz:= p3dTextSize( c, Font );
      LetterPosAndSize.Add( vec4( Cur.X, Cur.Y, sz.x, sz.y ));
      Cur:= Point( Cur.x + sz.x, Cur.y + sz.y );
    end;
end;


{ TP3DFontManager }

function TP3DFontManager.GetFont(FontName: String; Height: Integer): TP3DFont;
var
  n: Integer;
begin
  n:= Find( FontName, Height );
  if ( n > -1 ) then
    Result:= Items[ n ]
  else
    Result:= Items[ Add( DefaultFontsPath + DirectorySeparator + FontName + '.ttf', Height )];
end;

function TP3DFontManager.Add(Item: TP3DFont): Integer;
begin
	Result:= inherited Add(Item);
  Item.FontManager:= Self;
end;

function TP3DFontManager.Add(FontName: String; Height: Integer): Integer;
begin
  Result:= Add( TP3DFont.Create( FontName, Height ));
end;

constructor TP3DFontManager.Create;
begin
  inherited;
  FMaterial:= TP3DMaterialBase.Create;
  FMaterial.FUnlit:= True;
  FMaterial.FUseVertexColor:= True;
end;

destructor TP3DFontManager.Destroy;
begin
  inherited Destroy;
  Material.Free;
end;

function TP3DFontManager.Find(FontName: String; Height: Integer): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to FItems.Count - 1 do
    if (( FontName = TP3DFont( FItems[ i ]).FontName ) and ( Height = TP3DFont( FItems[ i ]).Size )) then
      begin
        Result:= i;
        break;
      end;
end;


{ TP3DFont }

constructor TP3DFont.Create(AFileName: String; ASize: Integer);
begin
  inherited Create;
  Size:= ASize;
  FileName:= AFileName;
  Font:= TTF_OpenFont( PChar( AFileName ), Size );
  if ( not Assigned( Font )) then
    raise Exception.Create( 'p3dtext.TP3DFont: Failed to load font "' + AFileName + '"!' );
end;

destructor TP3DFont.Destroy;
begin
  sdl2_ttf.TTF_CloseFont( Font );
	inherited Destroy;
end;

procedure TP3DFont.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  FFontName:= ExtractFileNameOnly( AValue );
end;

{$ENDIF}

