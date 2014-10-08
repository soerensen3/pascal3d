unit shaders;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dglOpenGL,
  strutils,
  Math3D;

type
  TShaderDecl = class
    Name: String;
    DeclType: Integer;
    Addr: GLuint;
  end;

  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomShaderDeclList}
  {$DEFINE TCustomItem:= TShaderDecl}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}

  { TShaderDeclList }

  TShaderDeclList = class ( TCustomShaderDeclList )
    function FindByName( Name: String ): Integer;
    function AddrByName( Name: String ): GLint;
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
  end;

  TShader = class
  private
    FAttributes: TShaderDeclList;
    FShaderObj: GLHandle;
    FUniforms: TShaderDeclList;
  public
    procedure Enable;
    procedure Disable;

    constructor Create;
    destructor Destroy; override;

    property ShaderObj: GLHandle read FShaderObj write FShaderObj;
    property Uniforms: TShaderDeclList read FUniforms write FUniforms;
    property Attributes: TShaderDeclList read FAttributes write FAttributes;
  end;

  function LoadShaderToText(FName: String; const do_replace: Boolean = True ): String;
  function CreateVertexAndFragmentShader( VertexShader, Fragmentshader: String ): TShader;
  procedure DeleteShader( Shader: GLHandleARB );
  procedure ShaderEnable( Shader: GLHandleARB );
  procedure ShaderDisable;

  procedure ShaderSetParameteri( shader: GLHandleARB; const Name: String; Value: GLint );
  procedure ShaderSetParameterf( shader: GLHandleARB; const Name: String; Value: GLfloat );
  procedure ShaderSetParameter2f( shader: GLHandleARB; const Name: String; Vec2: TVec2 );
  procedure ShaderSetParameter3f( shader: GLHandleARB; const Name: String; Vec3: TVec3 );
  procedure ShaderSetParameter4f( shader: GLHandleARB; const Name: String; Vec4: TVec4 );
  procedure ShaderSetParameter4fv( shader: GLHandleARB; const Name: String; Value: TMat4 );

  function ShaderCheckForErrors( glObject: GLHandleARB ): String;

var
  ActiveShader: GLHandleARB;
  ActShad: TShader;

implementation

{$DEFINE TCustomList:= TCustomShaderDeclList}
{$DEFINE TCustomItem:= TShaderDecl}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}

function LoadShaderToText(FName: String; const do_replace: Boolean = True ): String;
var
  F: TStringList;

  procedure StartReplace;
  var
    i: Integer;
    cmd: String;
    start: Integer;
    fn: String;
  begin
    for i:= 0 to F.Count - 1 do
      begin
        start:= PosEx( '#pragma', F[ i ]);
        if ( start > 0 ) then
          begin
            cmd:= Copy( F[ i ], start, Length( F[ i ]) - start );
            if ( ExtractWord( 2, cmd, [ ' ' ]) = 'include' ) then
              begin
                fn:= ExtractDelimited( 2, cmd, [ '"' ]);
                F[ i ]:= Copy( F[ i ], 1, start - 1 ) +
//                  '/*' + fn + '*/' + #13#10 +
                  LoadShaderToText( fn, do_replace );// +
//                  '/*-------------------*/';
              end
            else
              Write( 'Unknown pragma: ', cmd );
          end
//        else
//          F[ i ]:= '/*' + IntToStr( i + 1 ) + '*/' + F[ i ];
      end;
  end;

begin
  if ( FileExists( FName )) then
    begin
      F:= TStringList.Create;
      F.LoadFromFile( FName );

      if ( do_replace ) then
        StartReplace;

      F.SaveToFile( FName +'.tmp' );
      Result:= F.Text;

      F.Free;
    end
  else
    Write( 'Error: Shader source does not exist: "', FName, '"' );
end;

procedure DeleteShader( Shader: GLHandle );
begin
  glDeleteProgram( Shader );
end;

procedure ShaderEnable( Shader: GLHandle );
begin
  glUseProgram( Shader );
  ActiveShader:= Shader;
end;

procedure ShaderDisable;
begin
  ActiveShader:= 0;
  glUseProgram( 0 );
end;

function ShaderCheckForErrors( glObject: GLHandle ): String;
var
  blen, slen: GLInt;
  InfoLog : PGLCharARB;
begin
  glGetObjectParameterivARB(glObject, GL_OBJECT_INFO_LOG_LENGTH_ARB, @blen);
  if blen > 1 then
  begin
    GetMem(InfoLog, blen*SizeOf(GLCharARB));

    glGetInfoLogARB(glObject, blen , slen, InfoLog);

    Result:= PChar(InfoLog);
    Dispose(InfoLog);
  end;
end;

function CreateVertexAndFragmentShader(VertexShader, FragmentShader: String
  ): TShader;
var
  FragmentShaderObject,
  VertexShaderObject: GLHandleARB;
  len: Integer;

  procedure DebugParams( Shader: TShader );
  var
    i: Integer;
    cnt: GLint;
    len, size, typ: Integer;
    pname: PChar;
    str: array[ 0..255 ] of Char;

    function GLAttrTypeToStr( Attr: GLenum ): String;
    begin
      case Attr of
        GL_INT: Result:= 'int';
        GL_SAMPLER_1D: Result:= 'sampler1D';
        GL_SAMPLER_2D: Result:= 'sampler2D';
        GL_SAMPLER_3D: Result:= 'sampler3D';
        GL_SAMPLER_CUBE: Result:= 'samplerCUBE';
        GL_FLOAT: Result:= 'float';
        GL_FLOAT_VEC2: Result:= 'vec2';
        GL_FLOAT_VEC3: Result:= 'vec3';
        GL_FLOAT_VEC4: Result:= 'vec4';
        GL_FLOAT_MAT2: Result:= 'mat2';
        GL_FLOAT_MAT3: Result:= 'mat3';
        GL_FLOAT_MAT4: Result:= 'mat4';
        GL_FLOAT_MAT2x3: Result:= 'mat2x3';
        GL_FLOAT_MAT2x4: Result:= 'mat2x3';
        GL_FLOAT_MAT3x2: Result:= 'mat3x2';
        GL_FLOAT_MAT3x4: Result:= 'mat3x4';
        GL_FLOAT_MAT4x2: Result:= 'mat4x2';
        GL_FLOAT_MAT4x3: Result:= 'mat4x3';
      end;
    end;

  begin
    cnt:= 0;
    WriteLn();
    glGetProgramInterfaceiv( Shader.ShaderObj, GL_PROGRAM_INPUT, GL_ACTIVE_RESOURCES, @cnt );
//    glGetObjectParameterivARB( Shader.ShaderObj, GL_OBJECT_ACTIVE_ATTRIBUTES_ARB, @cnt );
    for i:= 0 to cnt - 1 do
      with ( Shader.Attributes[ Shader.Attributes.Add( TShaderDecl.Create )]) do
        begin
          pname:= @str[ 0 ];

          glGetActiveAttrib( Shader.ShaderObj, i, 255, len, size, typ, pname );

          Name:= pname;
          DeclType:= typ;
          Addr:= glGetAttribLocation( Shader.ShaderObj, pname );

          WriteLn( 'attribute ', GLAttrTypeToStr( DeclType ), ' ', Name, ' @', Addr );
        end;
    glGetProgramInterfaceiv( Shader.ShaderObj, GL_UNIFORM, GL_ACTIVE_RESOURCES, @cnt );
//    glGetObjectParameterivARB( Shader.ShaderObj, GL_OBJECT_ACTIVE_UNIFORMS_ARB, @cnt );
    // for i in 0 to count:
    for i:= 0 to cnt - 1 do
      with ( Shader.Uniforms[ Shader.Uniforms.Add( TShaderDecl.Create )]) do
        begin
          pname:= @str[ 0 ];

          glGetActiveUniform( Shader.ShaderObj, i, 255, len, size, typ, pname );

          Name:= pname;
          DeclType:= typ;
          Addr:= glGetUniformLocation( Shader.ShaderObj, pname );

          WriteLn( 'uniform ', GLAttrTypeToStr( DeclType ), ' ', Name, ' @', Addr );
        end;
  end;

  var
    Res: Integer;
    failed: Boolean;
begin
  Result := TShader.Create;
  Result.ShaderObj:= glCreateProgramObjectARB();

//  WriteLn( 'Loading shader: vshader: ' + ExtractFileName( VertexShader ) + ' ' + ' fshader: '+ ExtractFileName( FragmentShader ));
  FragmentShaderObject := glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);
  VertexShaderObject := glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB);

  len:= Length( VertexShader );
  glShaderSourceARB( VertexShaderObject, 1, @VertexShader, @len );
  len:= Length( FragmentShader );
  glShaderSourceARB( FragmentShaderObject, 1, @FragmentShader, @len );

  glCompileShaderARB( FragmentShaderObject );
  glCompileShaderARB( VertexShaderObject );

  Res:= 0;
  glGetShaderiv( VertexShaderObject, GL_COMPILE_STATUS, @Res );
  failed:= Res = GL_FALSE;
  if ( Res <> GL_FALSE ) then
    glAttachObjectARB( Result.ShaderObj, VertexShaderObject )
  else
    WriteLn( 'Vertex shader: ' + #13#10, ShaderCheckForErrors( VertexShaderObject ));

  Res:= 0;
  glGetShaderiv( FragmentShaderObject, GL_COMPILE_STATUS, @Res );
  if ( not failed ) then
    failed:= Res = GL_FALSE;
  if ( Res <> GL_FALSE ) then
    glAttachObjectARB( Result.ShaderObj, FragmentShaderObject )
  else
    WriteLn( 'Fragment shader: ' + #13#10, ShaderCheckForErrors( FragmentShaderObject ));

  glDeleteObjectARB( FragmentShaderObject );
  glDeleteObjectARB( VertexShaderObject );

{  if ( failed ) then
    begin
      DeleteShader( Result.ShaderObj );
      FreeAndNil( Result );
    end
  else}
    begin
      glLinkProgramARB( Result.ShaderObj );
      DebugParams( Result );
    end;
end;

procedure ShaderSetParameteri( shader: GLHandleARB; const Name: String; Value: GLint );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharArb(Name);
  glUniform1iARB(glGetUniformLocationARB( shader, p), Value);
end;

procedure ShaderSetParameterf( shader: GLHandleARB; const Name: String; Value: GLfloat );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform1fARB(glGetUniformLocationARB( shader, p), Value);
end;

procedure ShaderSetParameter2f(shader: GLHandleARB; const Name: String;
  Vec2: TVec2);
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform2f( glGetUniformLocationARB( shader, p), Vec2.x, Vec2.y );
end;

procedure ShaderSetParameter3f( shader: GLHandleARB; const Name: String; Vec3: TVec3 );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform3f( glGetUniformLocationARB( shader, p), Vec3.x, Vec3.y, Vec3.z );
end;

procedure ShaderSetParameter4f( shader: GLHandleARB; const Name: String; Vec4: TVec4 );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform4f( glGetUniformLocationARB( shader, p), Vec4.x, Vec4.y, Vec4.z, Vec4.w );
end;


procedure ShaderSetParameter4fv(shader: GLHandleARB; const Name: String;
  Value: TMat4);
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniformMatrix4fvARB(glGetUniformLocationARB( shader, p), 1, False, @Value );
end;

{ TShaderDeclList }

function TShaderDeclList.FindByName(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Name = Items[ i ].Name ) then
      begin
        Result:= i;
        break;
      end;
end;

function TShaderDeclList.AddrByName(Name: String): GLint;
var
  n: Integer;
begin
  n:= FindByName( Name );
  if ( n >= 0 ) then
    Result:= Items[ n ].Addr
  else
    Result:= -1;
end;

procedure TShaderDeclList.Delete(Index: Integer);
begin
  Items[ Index ].Free;
  inherited Delete(Index);
end;

procedure TShaderDeclList.Clear;
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    Items[ i ].Free;
  inherited Clear;
end;

{ TShader }

procedure TShader.Enable;
begin
  ShaderEnable( ShaderObj );
  ActShad:= Self;
end;

procedure TShader.Disable;
begin
  ActShad:= nil;
  ShaderDisable;
end;

constructor TShader.Create;
begin
  inherited;
  FUniforms:= TShaderDeclList.Create;
  FAttributes:= TShaderDeclList.Create;
end;

destructor TShader.Destroy;
begin
  DeleteShader( ShaderObj );
  inherited Destroy;
end;

end.
