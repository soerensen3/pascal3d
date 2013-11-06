unit shaders;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dglOpenGL,
  Math3D;

  function LoadShaderToText( FName: String ): String;
  function CreateVertexAndFragmentShader( VertexShader, Fragmentshader: String ): GLHandleARB;
  procedure DeleteShader( Shader: GLHandleARB );
  procedure ShaderEnable( Shader: GLHandleARB );
  procedure ShaderDisable;

  procedure ShaderSetParameteri( shader: GLHandleARB; const Name: String; Value: GLint );
  procedure ShaderSetParameterf( shader: GLHandleARB; const Name: String; Value: GLfloat );
  procedure ShaderSetParameter3f( shader: GLHandleARB; const Name: String; Value1, Value2, Value3: GLfloat );
  procedure ShaderSetParameter4fv( shader: GLHandleARB; const Name: String; Value: TMat4 );

  function ShaderCheckForErrors( glObject: GLHandleARB ): String;

var
  ActiveShader: GLHandleARB;

implementation

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
  ): GLHandleARB;
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
    glGetObjectParameterivARB( Result, GL_OBJECT_ACTIVE_ATTRIBUTES_ARB, @cnt );
    for i:= 0 to cnt - 1 do
      begin
        name:= @str[ 0 ];
        glGetActiveAttrib( Result, i, 255, len, size, typ, PGLchar( name ));
        WriteLn( 'attribute ', GLAttrTypeToStr( typ ), ' ', name, ' @', glGetAttribLocation( Result, name ));
      end;
    glGetObjectParameterivARB( Result, GL_OBJECT_ACTIVE_UNIFORMS_ARB, @cnt );
    // for i in 0 to count:
    for i:= 0 to cnt - 1 do
      begin
        name:= @str[ 0 ];
        glGetActiveUniform( Result, i, 255, len, size, typ, PGLchar( name ));
        WriteLn( 'uniform ', GLAttrTypeToStr( typ ), ' ', name, ' @', glGetUniformLocation( Result, name ));
      end;
  end;

begin
  Result := glCreateProgramObjectARB();

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

  glAttachObjectARB( Result, FragmentShaderObject );
  glAttachObjectARB( Result, VertexShaderObject );

  glDeleteObjectARB( FragmentShaderObject );
  glDeleteObjectARB( VertexShaderObject );
  glLinkProgramARB( Result );

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

procedure ShaderSetParameter3f( shader: GLHandleARB; const Name: String; Value1, Value2, Value3: GLfloat );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform3f(glGetUniformLocationARB( shader, p), Value1, Value2, Value3);
end;

procedure ShaderSetParameter4f( shader: GLHandleARB; const Name: String; Value1, Value2, Value3, Value4: GLfloat );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform4fARB(glGetUniformLocationARB( shader, p), Value1, Value2, Value3, Value4);
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

end.

