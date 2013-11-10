unit shaders;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dglOpenGL,
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
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
  end;

  TShader = class
    ShaderObj: GLHandle;

    procedure Enable;
    procedure Disable;


    destructor Destroy; override;
  end;

  function LoadShaderToText( FName: String ): String;
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

implementation

{$DEFINE TCustomList:= TCustomShaderDeclList}
{$DEFINE TCustomItem:= TShaderDecl}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}

function LoadShaderToText(FName: String): String;
var
  F: TStringList;
begin
  if ( FileExists( FName )) then
    begin
      F:= TStringList.Create;
      F.LoadFromFile( FName );
      Result:= F.Text;
      F.Free;
    end
  else
    Write( 'Error: Shader source does not exist!' );
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

  procedure DebugParams;
  var
    i: Integer;
    cnt: Integer;
    len, size, typ: Integer;
    name: PChar;
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
    glGetObjectParameterivARB( Result.ShaderObj, GL_OBJECT_ACTIVE_ATTRIBUTES_ARB, @cnt );
    for i:= 0 to cnt - 1 do
      begin
        name:= @str[ 0 ];
        glGetActiveAttrib( Result.ShaderObj, i, 255, len, size, typ, PGLchar( name ));
        WriteLn( 'attribute ', GLAttrTypeToStr( typ ), ' ', name, ' @', glGetAttribLocation( Result.ShaderObj, name ));
      end;
    glGetObjectParameterivARB( Result.ShaderObj, GL_OBJECT_ACTIVE_UNIFORMS_ARB, @cnt );
    // for i in 0 to count:
    for i:= 0 to cnt - 1 do
      begin
        name:= @str[ 0 ];
        glGetActiveUniform( Result.ShaderObj, i, 255, len, size, typ, PGLchar( name ));
        WriteLn( 'uniform ', GLAttrTypeToStr( typ ), ' ', name, ' @', glGetUniformLocation( Result.ShaderObj, name ));
      end;
  end;

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
  WriteLn( 'Fragment shader: ' + #13#10, ShaderCheckForErrors( FragmentShaderObject ));
  glCompileShaderARB( VertexShaderObject );
  WriteLn( 'Vertex shader: ' + #13#10, ShaderCheckForErrors( VertexShaderObject ));

  glAttachObjectARB( Result.ShaderObj, FragmentShaderObject );
  glAttachObjectARB( Result.ShaderObj, VertexShaderObject );

  glDeleteObjectARB( FragmentShaderObject );
  glDeleteObjectARB( VertexShaderObject );
  glLinkProgramARB( Result.ShaderObj );

  DebugParams;
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
end;

procedure TShader.Disable;
begin
  ShaderDisable;
end;

destructor TShader.Destroy;
begin
  DeleteShader( ShaderObj );
  inherited Destroy;
end;

end.

