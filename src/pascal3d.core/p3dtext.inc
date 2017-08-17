//p3dcanvas.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

const
  P3DSymbolFontName: String      = 'Pascal3D-Symbols';
  P3DSymbolArrowUp               = 'A';
  P3DSymbolArrowDown             = 'B';
  P3DSymbolArrowLeft             = 'C';
  P3DSymbolArrowRight            = 'D';
  P3DSymbolTriangleUp            = 'E';
  P3DSymbolTriangleDown          = 'F';
  P3DSymbolTriangleLeft          = 'G';
  P3DSymbolTriangleRight         = 'H';
  P3DSymbolTriangleUpDown        = 'I';
  P3DSymbolPlus                  = 'J';
  P3DSymbolMinus                 = 'K';
  P3DSymbolOptions               = 'L';
  P3DSymbolClose                 = 'M';
  P3DSymbolShrink                = 'N';
  P3DSymbolExpand                = 'O';
  P3DSymbolFolder                = 'P';
  P3DSymbolFile                  = 'Q';
  P3DSymbolObject                 = 'R';
  P3DSymbolMesh                  = 'S';
  P3DSymbolScene                 = 'T';
  P3DSymbolFont                  = 'U';
  P3DSymbolCamera                = 'V';
  P3DSymbolTexture               = 'W';
  P3DSymbolMaterial              = 'X';
  P3DSymbolShader                = 'Y';
  P3DSymbolLight                 = 'Z';
  P3DSymbolLibrary               = '[';
  P3DSymbolCheck                 = '\';
  P3DSymbolTerrain               = ']';
  P3DSymbolArmature              = '^';
  P3DSymbolClear                 = '_';
  P3DSymbolGrid                  = '`';

type
  TP3DFontManager = class;

	{ TP3DFont }

  TP3DFont = class
    private
      FFileWatch: TP3DFilePointer;
      FFontManager: TP3DFontManager;
      FFontName: String;
      FSize: Integer;
      FFont: sdl2_ttf.PTTF_Font;

    public
      constructor Create( AFileName: String; ASize: Integer );
      destructor Destroy; override;

      property Font: sdl2_ttf.PTTF_Font read FFont write FFont;

    published
      property FontName: String read FFontName;
      property Size: Integer read FSize write FSize;
      property FontManager: TP3DFontManager read FFontManager write FFontManager;
      property FileWatch: TP3DFilePointer read FFileWatch write FFileWatch;
  end;

  { TP3DFontManager }

  TP3DFontManager = class ( specialize gP3DCustomObjectList < TP3DFont >)
    private
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
      procedure Render;

      //function AppendLetter( L: WideChar; fs: Single; p: TVec2 ): TVec2;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Render( p: TVec2; Color: TVec4 );
      procedure RenderLetter(p: TVec2; Idx: Integer; Color: TVec4);
      procedure RenderLetter(p: TVec2; Idx: Integer; Color: TVec4; proj: TMat4);
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
  decl: TP3DShaderDecl;
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
      if ( P3DShaderActive.Attributes.FindByName( 'TexCoord0', decl )) then
        decl.AsVec2List:= TexCoords;
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
    P3DFontManager.Material.Maps.Add( TP3DMaterialMapBase.Create( P3DFontManager.Material ));
  P3DFontManager.Material.Maps[ 0 ].Map:= Texture;
  TP3DMaterialMapBase( P3DFontManager.Material.Maps[ 0 ]).AlphaFactor:= 1;
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
  if ( Assigned( FTexture ) and P3DDataBlockCache.IsValid( FTexture )) then
    FTexture.Free;
  inherited Destroy;
end;

procedure TP3DText.Render;
var
  decl: TP3DShaderDecl;
begin
  if ( P3DShaderActive.Attributes.FindByName( 'TexCoord0', decl )) then
    decl.AsVec2List:= TexCoords;
  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 )); cle( Self );
end;

procedure TP3DText.Render(p: TVec2; Color: TVec4);
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

  if ( Shader.Uniforms.FindByName( 'tex0', Uniform )) then
    Uniform.AsInt:= 0;

  if ( Shader.Uniforms.FindByName( 'view', Uniform )) then
    Uniform.AsMat4:= Mat4Identity;
  if ( Shader.Uniforms.FindByName( 'world', Uniform )) then
    Uniform.AsMat4:= Mat4Identity;

  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A ); cle( Self );
  Vertices.SetAttribArray( P3DAttribPosition );
  Render;
end;

procedure TP3DText.RenderLetter(p: TVec2; Idx: Integer; Color: TVec4);
var
  Shader: TP3DShader;
  Uniform, decl: TP3DShaderDecl;
begin
  ApplyShader;

  if ( not Assigned( P3DFontManager.Material.Shader )) then
    exit;

  Shader:= P3DFontManager.Material.Shader;

  if ( Shader.Uniforms.FindByName( 'tex0', Uniform )) then
    Uniform.AsInt:= 0;

  if ( Shader.Uniforms.FindByName( 'view', Uniform )) then
    Uniform.AsMat4:= Mat4Identity;
  if ( Shader.Uniforms.FindByName( 'world', Uniform )) then
    Uniform.AsMat4:= Mat4Identity;

  glVertexAttrib4f( P3DAttribColor, Color.R, Color.G, Color.B, Color.A ); cle( Self );
  Vertices.SetAttribArray( P3DAttribPosition );

  if ( P3DShaderActive.Attributes.FindByName( 'TexCoord0', decl )) then
    decl.AsVec2List:= TexCoords;
  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  glDrawElements( GL_TRIANGLES, 6, GL_UNSIGNED_INT, Pointer( 0 )); cle( Self );
//  glDrawElements( GL_TRIANGLES, 6, GL_UNSIGNED_INT, Pointer( Idx * 6 )); cle( Self );
end;

procedure TP3DText.RenderLetter(p: TVec2; Idx: Integer; Color: TVec4; proj: TMat4 );
var
  Shader: TP3DShader;
  Uniform: TP3DShaderDecl;
  mt, m: TMat4;
begin
  ApplyShader;

  if ( not Assigned( P3DFontManager.Material.Shader )) then
    exit;

  Shader:= P3DFontManager.Material.Shader;


  mt:= mat4translate( vec4( p, 0, 1 ));
  m:= proj * mt;

  if ( Shader.Uniforms.FindByName( 'proj', Uniform )) then
    Uniform.AsMat4:= m;
  RenderLetter( p, Idx, Color );
end;

procedure TP3DText.Render(p: TVec2; Color: TVec4; proj: TMat4);
var
  Shader: TP3DShader;
  Uniform: TP3DShaderDecl;
  mt, m: TMat4;
begin
  ApplyShader;

  if ( not Assigned( P3DFontManager.Material.Shader )) then
    exit;

  Shader:= P3DFontManager.Material.Shader;


  mt:= mat4translate( vec4( p, 0, 1 ));
  m:= proj * mt;

  if ( Shader.Uniforms.FindByName( 'proj', Uniform )) then
    Uniform.AsMat4:= m;
  Render( p, Color );
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
    Result:= Items[ Add( FontName + '.ttf', Height )];
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
  FMaterial.Name:= 'FontMaterial';
  FMaterial.FUnlit:= True;
  FMaterial.FUseVertexColor:= True;
end;

destructor TP3DFontManager.Destroy;
begin
  FreeAndNil( FMaterial );
  Clear( True );
  inherited Destroy;
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
var
  FileName: String;
begin
  inherited Create;
  Size:= ASize;
  FileName:= P3DSearchPaths.FindFile( AFileName );
  if ( FileName = '' ) then
    raise Exception.Create( 'p3dtext.TP3DFont: Failed to load font "' + AFileName + '"! Not found!' );
  FFileWatch:= TP3DFilePointer.Create( FileName );
  FFontName:= ExtractFileNameOnly( FileName );
  Font:= TTF_OpenFont( PChar( FileName ), Size );
end;

destructor TP3DFont.Destroy;
begin
  FileWatch.Free;
  sdl2_ttf.TTF_CloseFont( Font );
  inherited Destroy;
end;


{$ENDIF}

{$IFDEF INITIALIZATION}
  if ( TTF_Init() <> 0 ) then
    raise Exception.Create( 'Cannot initialize sdl2_text!' );
  if ( not Assigned( P3DFontManager )) then
    P3DFontManager:= TP3DFontManager.Create;
{$ENDIF}

{$IFDEF FINALIZATION}
  if ( Assigned( P3DFontManager )) then
    FreeAndNil( P3DFontManager );
  TTF_Quit();
{$ENDIF}

