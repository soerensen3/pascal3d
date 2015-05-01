unit p3dbmpfont;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  math,
  p3dMath,
  p3dtexture,
  p3dbuffers,
  p3dshaders,
  p3dgeometry,
  dglOpenGL;

type
  TP3DFontLetter = class
    uv1, uv2: TVec2;
    Texture: TP3DTexture;
    Letter: WideChar; //UNICODE
  end;

  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomFontLetterList}
  {$DEFINE TCustomListEnumerator:= TFontLetterEnumerator}
  {$DEFINE TCustomItem:= TP3DFontLetter}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DFontLetterList }

  TP3DFontLetterList = class ( TCustomFontLetterList )
    function Find( C: WideChar ): Integer;
  end;

  {TP3DPrintLetter = class
    FontLetter: TP3DFontLetter;
    TopLeft: TVec2; //Rect
    BottomRight: TVec2;
    Size: TVec2;
  end;}


  TP3DFontManager = class;
  TP3DFont = class
    private
      FFileName: String;
      FFontManager: TP3DFontManager;
      FFontName: String;
      FLetters: TP3DFontLetterList;
      FSizeFactor: Single;
      FTextures: TP3DTextureList;

      procedure SetFileName(AValue: String);

    public
      constructor Create;
      destructor Destroy; override;

    published
      property Letters: TP3DFontLetterList read FLetters write FLetters;
      property Textures: TP3DTextureList read FTextures write FTextures;
      property FileName: String read FFileName write SetFileName;
      property FontName: String read FFontName;
      property SizeFactor: Single read FSizeFactor write FSizeFactor;
      property FontManager: TP3DFontManager read FFontManager write FFontManager;
  end;

  { TCustomFontList }

  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomFontList}
  {$DEFINE TCustomListEnumerator:= TFontEnumerator}
  {$DEFINE TCustomItem:= TP3DFont}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DFontManager }

  TP3DFontManager = class ( TCustomFontList )
    private
      FShader: TShader;
      function GetFont( FontName: String ): TP3DFont;

    public
      property Fonts [ FontName: String ]: TP3DFont read GetFont; default;
      function Add(Item: TP3DFont): Integer; override;

    published
      function Find( FontName: String ): Integer;
      property Shader: TShader read FShader write FShader;
  end;

  { TP3DText }

  TP3DText = class
    private
      FFont: TP3DFont;
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
      destructor Destroy; override;

      procedure Render( p: TVec2; Color: TVec4 );
      procedure Render( p: TVec2; Color: TVec4; proj: TMat4 );

      function WidthFromTo( idx1: Integer; idx2: Integer ): TVec4;

      property Text: String read FText;
      property Width: Single read FWidth;
      property Height: Single read FHeight;
      property Font: TP3DFont read FFont;
      property Vertices: TP3DVec2BufferGL read FVertices write FVertices;
      property TexCoords: TP3DVec2BufferGL read FTexCoords write FTexCoords;
      property Indices: TP3DIntBufferGL read FIndices write FIndices;
      property LetterPosAndSize: TVec4List read FLetterPosAndSize write FLetterPosAndSize;
  end;

  { TP3DBitmapString }
{
  TP3DBitmapString = object
    procedure Init( S: String );
    procedure Deinit;
    Text: TP3DText;
  end;

  operator := ( bs: TP3DBitmapString; s: String ): TP3DBitmapString;
}
  function p3dTextSimple( Text: String; Font: TP3DFont; fs: Single ): TP3DText;

var
  P3DFontManager: TP3DFontManager;

implementation
{
operator:=(bs: TP3DBitmapString; s: String): TP3DBitmapString;
begin
  bs
end;
}
function p3dTextSimple(Text: String; Font: TP3DFont; fs: Single): TP3DText;
var
  c: Char;
  p: TVec2;
  max_y: Float;
begin
  Result:= TP3DText.Create;
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

{ TP3DFontManager }


function TP3DFontManager.GetFont( FontName: String ): TP3DFont;
var
  n: Integer;
begin
  n:= Find( FontName );
  if ( n > -1 ) then
    Result:= Items[ n ]
  else
    Result:= nil;
end;

function TP3DFontManager.Add(Item: TP3DFont): Integer;
begin
  Result:=inherited Add(Item);
  Item.FontManager:= Self;
end;

function TP3DFontManager.Find(FontName: String): Integer;
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

{ TP3DBitmapString }
{
procedure TP3DBitmapString.Init( S: String; Parent );
begin
  Text:= p3dTextSimple( Text, );
end;

procedure TP3DBitmapString.Deinit;
begin

end;
}
{ TP3DText }

function TP3DText.AppendLetter( L: WideChar; fs: Single; p: TVec2 ): TVec2;
var
  v1,v2,v3,v4: TVec2;
  tc1, tc2, tc3, tc4: TVec2;
  n: Integer;
begin
  Result:= vec2( 0 );

  n:= Font.Letters.Find( L );
  if ( n < 0 ) then
    exit;

  FText+= L;

  v1:= p;
  v3:= p + ( Font.Letters[ n ].uv2 - Font.Letters[ n ].uv1 ) * Font.SizeFactor * fs;
  v2:= vec2( v3.x, v1.y );
  v4:= vec2( v1.x, v3.y );
  tc1:= Font.Letters[ n ].uv1;
  tc3:= Font.Letters[ n ].uv2;
  tc2:= vec2( tc3.x, tc1.y );
  tc4:= vec2( tc1.x, tc3.y );

  n:= Vertices.Add([ v1, v2, v3, v4 ]);
  TexCoords.Add([ tc1, tc2, tc3, tc4 ]);
  Indices.Add([ n + 0, n + 1, n + 2, n + 0, n + 2, n + 3 ]);
  LetterPosAndSize.Add( vec4( v1, v3 - v1 ));
  Result:= v3;
end;

constructor TP3DText.Create;
begin
  inherited;
  Vertices:= TP3DVec2BufferGL.Create( True );
  Vertices.SetAttribArray( 0 );
  TexCoords:= TP3DVec2BufferGL.Create( True );
  TexCoords.SetAttribArray( 4 );
  Indices:= TP3DIntBufferGL.Create( True );
  LetterPosAndSize:= TVec4List.Create;
end;

destructor TP3DText.Destroy;
begin
  Vertices.Free;
  Indices.Free;
  LetterPosAndSize.Free;
  inherited;
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
  OldShader: TShader;

begin
  OldShader:= ActShad;
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
    ActShad.Disable;
end;

procedure TP3DText.Render(p: TVec2; Color: TVec4; proj: TMat4);
var
  cl: GLint;
  vt: GLint;
  tc: GLint;
  tx: GLint;
  mat: GLint;
  mt: TMat4;
  m: TMat4;
  OldShader: TShader;
begin
  OldShader:= ActShad;
  if ( Assigned( Font.FontManager.Shader )) then
    Font.FontManager.Shader.Enable;
  cl:= ActShad.Attributes.AddrByName( 'in_color' );

  tx:= ActShad.Uniforms.AddrByName( 'tex0' );
  mat:= ActShad.Uniforms.AddrByName( 'mat' );

  glActiveTexture( GL_TEXTURE0 );
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, Font.Textures[ 0 ].fGLTexture );

  mt:= mat4translate( vec4( p, 0, 1 ));
  m:= mt * proj;
  glUniform1i( tx, 0 );
  glUniformMatrix4fv( mat, 1, False, @m );
  glVertexAttrib4f( cl, Color.X, Color.Y, Color.Z, Color.A );
  Vertices.SetAttribArray( 0 );
  Vertices.Bind();
  TexCoords.SetAttribArray( 4 );
  TexCoords.Bind();
  Indices.Bind( GL_ELEMENT_ARRAY_BUFFER );
  glDrawElements( GL_TRIANGLES, Indices.Count, GL_UNSIGNED_INT, Pointer( 0 ));
  if ( Assigned( OldShader )) then
    OldShader.Enable
  else
    ActShad.Disable;
end;

function TP3DText.WidthFromTo(idx1: Integer; idx2: Integer): TVec4;
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

{ TP3DFont }

procedure TP3DFont.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
  FFontName:= ExtractFileNameOnly( AValue );
end;

constructor TP3DFont.Create;
begin
  inherited;
  Letters:= TP3DFontLetterList.Create;
  Textures:= TP3DTextureList.Create;
  SizeFactor:= 1.0;
end;

destructor TP3DFont.Destroy;
begin
  Letters.Free;
  Textures.Free;
  inherited Destroy;
end;

{ TP3DFontLetterList }

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

{$DEFINE OBJECTLIST}
{$DEFINE TCustomList:= TCustomFontLetterList}
{$DEFINE TCustomListEnumerator:= TFontLetterEnumerator}
{$DEFINE TCustomItem:= TP3DFontLetter}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{ TCustomFontList }

{$DEFINE TCustomList:= TCustomFontList}
{$DEFINE TCustomListEnumerator:= TFontEnumerator}
{$DEFINE TCustomItem:= TP3DFont}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}
{$UNDEF OBJECTLIST}

{ TP3DText }

initialization
  P3DFontManager:= TP3DFontManager.Create;

finalization
  P3DFontManager.Free;
end.

