unit p3dshaders;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dglOpenGL,
  strutils,
  p3dMath;

type
  Float = Single;

  { TShaderDecl }

  TShaderDeclType = ( sdtUniform, sdtAttribute );

  TShaderDecl = class
    private
      FAddr: GLuint;
      FName: String;
      FProgramObject: GLuint;
      FShaderDeclType: TShaderDeclType;
      FVarType: Integer;

      function GetAsFloat: Float;
      function GetAsInt: Integer;
      function GetAsMat2: TMat2;
      function GetAsMat3: TMat3;
      function GetAsMat4: TMat4;
      function GetAsVec2: TVec2;
      function GetAsVec3: TVec3;
      function GetAsVec4: TVec4;
      procedure SetAsFloat( AValue: Float );
      procedure SetAsInt( AValue: Integer );
      procedure SetAsMat2( AValue: TMat2 );
      procedure SetAsMat3( AValue: TMat3 );
      procedure SetAsMat4( AValue: TMat4 );
      procedure SetAsVec2( AValue: TVec2 );
      procedure SetAsVec3( AValue: TVec3 );
      procedure SetAsVec4( AValue: TVec4 );

    public
      constructor Create( AProgramObject: GLuint; ADeclType: TShaderDeclType; AName: String; AAddr: GLuint );

      property AsInt: Integer read GetAsInt write SetAsInt;
      property AsFloat: Float read GetAsFloat write SetAsFloat;
      property AsVec2: TVec2 read GetAsVec2 write SetAsVec2;
      property AsVec3: TVec3 read GetAsVec3 write SetAsVec3;
      property AsVec4: TVec4 read GetAsVec4 write SetAsVec4;
      property AsMat2: TMat2 read GetAsMat2 write SetAsMat2;
      property AsMat3: TMat3 read GetAsMat3 write SetAsMat3;
      property AsMat4: TMat4 read GetAsMat4 write SetAsMat4;
      property VarType: Integer read FVarType write FVarType;
      property DeclType: TShaderDeclType read FShaderDeclType write FShaderDeclType;
      property ProgramObject: GLuint read FProgramObject write FProgramObject;
      property Name: String read FName write FName;
      property Addr: GLuint read FAddr write FAddr;
  end;

  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomShaderDeclList}
  {$DEFINE TCustomListEnumerator:= TShaderDeclEnumerator}
  {$DEFINE TCustomItem:= TShaderDecl}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TShaderDeclList }
  TShaderDeclList = class ( TCustomShaderDeclList )
    function FindByName( Name: String ): Integer;
    function AddrByName( Name: String ): GLint;
    procedure Delete( Index: Integer ); override;
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

      procedure UpdateUniforms();

      property ShaderObj: GLHandle read FShaderObj write FShaderObj;
      property Uniforms: TShaderDeclList read FUniforms write FUniforms;
      property Attributes: TShaderDeclList read FAttributes write FAttributes;
  end;

  function LoadShaderToText(FName: String; const do_replace: Boolean = True ): String;
  function CreateVertexAndFragmentShader( VertexShader, Fragmentshader: String ): TShader;
  procedure DeleteShader( Shader: GLHandle );
  procedure ShaderEnable( Shader: GLHandle );
  procedure ShaderDisable;

  {
  procedure ShaderSetParameteri( shader: GLHandle; const Name: String; Value: GLint );
  procedure ShaderSetParameterf( shader: GLHandle; const Name: String; Value: GLfloat );
  procedure ShaderSetParameter2f( shader: GLHandle; const Name: String; Vec2: TVec2 );
  procedure ShaderSetParameter3f( shader: GLHandle; const Name: String; Vec3: TVec3 );
  procedure ShaderSetParameter4f( shader: GLHandle; const Name: String; Vec4: TVec4 );
  procedure ShaderSetParameter4fv( shader: GLHandle; const Name: String; Value: TMat4 );
  }

  function ProgramCheckForErrors( glProgram: GLHandle ): String;
  function ShaderCheckForErrors( glShader: GLHandle ): String;

var
  ActiveShader: GLHandle;
  ActShad: TShader;

implementation

{ TShaderDecl }

function TShaderDecl.GetAsInt: Integer;
begin
  glGetUniformiv( ProgramObject, Addr, @Result );
end;

function TShaderDecl.GetAsFloat: Float;
begin
  case DeclType of
    sdtUniform: glGetUniformfv( ProgramObject, Addr, @Result );
    sdtAttribute: Result:= 0;
  end;
end;

function TShaderDecl.GetAsMat2: TMat2;
begin
  case DeclType of
    sdtUniform: glGetUniformfv( ProgramObject, Addr, @Result );
    sdtAttribute: Result:= mat2( 0 );
  end;
end;

function TShaderDecl.GetAsMat3: TMat3;
begin
  case DeclType of
    sdtUniform: glGetUniformfv( ProgramObject, Addr, @Result );
    sdtAttribute: Result:= mat3( 0 );
  end;
end;

function TShaderDecl.GetAsMat4: TMat4;
begin
  case DeclType of
    sdtUniform: glGetUniformfv( ProgramObject, Addr, @Result );
    sdtAttribute: Result:= mat4( 0 );
  end;
end;

function TShaderDecl.GetAsVec2: TVec2;
begin
  case DeclType of
    sdtUniform: glGetUniformfv( ProgramObject, Addr, @Result );
    sdtAttribute: Result:= vec2( 0 );
  end;
end;

function TShaderDecl.GetAsVec3: TVec3;
begin
  case DeclType of
    sdtUniform: glGetUniformfv( ProgramObject, Addr, @Result );
    sdtAttribute: Result:= vec3( 0 );
  end;
end;

function TShaderDecl.GetAsVec4: TVec4;
begin
  case DeclType of
    sdtUniform: glGetUniformfv( ProgramObject, Addr, @Result );
    sdtAttribute: Result:= vec4( 0 );
  end;
end;

procedure TShaderDecl.SetAsFloat(AValue: Float);
begin
  case DeclType of
    sdtUniform: glUniform1f( Addr, AValue );
  end;
end;

procedure TShaderDecl.SetAsInt(AValue: Integer);
begin
  case DeclType of
    sdtUniform: glUniform1i( Addr, AValue );
  end;
end;

procedure TShaderDecl.SetAsMat2(AValue: TMat2);
begin
  case DeclType of
    sdtUniform: glUniformMatrix2fv( Addr, 1, False, @AValue );
  end;
end;

procedure TShaderDecl.SetAsMat3(AValue: TMat3);
begin
  case DeclType of
    sdtUniform: glUniformMatrix3fv( Addr, 1, False, @AValue );
  end;
end;

procedure TShaderDecl.SetAsMat4(AValue: TMat4);
begin
  case DeclType of
    sdtUniform: glUniformMatrix4fv( Addr, 1, False, @AValue.m[ 0 ]);
  end;
end;

procedure TShaderDecl.SetAsVec2(AValue: TVec2);
begin
  case DeclType of
    sdtUniform: glUniform2fv( Addr, 1, @AValue );
  end;
end;

procedure TShaderDecl.SetAsVec3(AValue: TVec3);
begin
  case DeclType of
    sdtUniform: glUniform3fv( Addr, 1, @AValue );
  end;
end;

procedure TShaderDecl.SetAsVec4(AValue: TVec4);
begin
  case DeclType of
    sdtUniform: glUniform4fv( Addr, 1, @AValue );
  end;
end;

constructor TShaderDecl.Create(AProgramObject: GLuint;
  ADeclType: TShaderDeclType; AName: String; AAddr: GLuint);
begin
  inherited Create;
  DeclType:= ADeclType;
  ProgramObject:= AProgramObject;
  Name:= AName;
  Addr:= AAddr;
end;

{$DEFINE TCustomList:= TCustomShaderDeclList}
{$DEFINE TCustomItem:= TShaderDecl}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

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
      WriteLn( 'Loading ', FName, '...' );
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

function ProgramCheckForErrors( glProgram: GLHandle ): String;
var
  blen: GLInt;
  InfoLog : PGLChar;
  slen: PGLsizei;
begin
  glGetProgramiv( glProgram, GL_INFO_LOG_LENGTH, @blen );
  if ( blen > 1 ) then
    begin
      GetMem( InfoLog, blen * SizeOf( GLCharARB ));
      glGetProgramInfoLog( glProgram, blen, slen, InfoLog );
      Result:= PChar( InfoLog );
      Dispose( InfoLog );
    end;
end;

function ShaderCheckForErrors( glShader: GLHandle ): String;
var
  blen, slen: GLInt;
  InfoLog : PGLChar;
begin
  glGetShaderiv( glShader, GL_INFO_LOG_LENGTH, @blen );
  if ( blen > 1 ) then
    begin
      GetMem( InfoLog, blen * SizeOf( GLCharARB ));

      glGetShaderInfoLog( glShader, blen , slen, InfoLog );

      Result:= PChar( InfoLog );
      Dispose( InfoLog );
    end;
end;

function CreateShaderFromSrc( Source: String; SType: GLenum ): GLHandle;
var
  shader: GLint;
  Len: Integer;
  Success: GLboolean;
  typeN: String;
begin
  Result:= 0;
  shader:= glCreateShader( SType );
  Len:= Length( Source );
  glShaderSource( shader, 1, @Source, @Len );
  glCompileShader( shader );

  // make sure the compilation was successful
  glGetShaderiv( shader, GL_COMPILE_STATUS, @Success );
  if( not Success ) then
    begin
      glGetBooleanv( GL_SHADER_COMPILER, @Success );
      if ( not Success ) then
        WriteLn( 'Warning: Shader compiler not supported! Please make sure you have the latest driver installed and your video card supports shaders.' );

      case SType of
        GL_VERTEX_SHADER: typeN:= 'Vertex Shader';
        GL_FRAGMENT_SHADER: typeN:= 'Fragment Shader';
        GL_GEOMETRY_SHADER: typeN:= 'Geometry Shader';
      end;
      WriteLn( Format( 'Compilation failed (%s): "%s"', [ typeN, ShaderCheckForErrors( shader )]));
      glDeleteShader( shader );
    end
  else
    Result:= shader;
end;

function shaderAttachFromSrc( SProgram: GLint; SType: GLenum; Src: String ): Boolean;
var
  shader: GLHandle;
begin
  // compile the shader
  shader:= CreateShaderFromSrc( Src, SType );

  if ( shader <> 0 ) then
    begin
      // attach the shader to the program
      glAttachShader( SProgram, shader );

      // delete the shader - it won't actually be
      // destroyed until the program that it's attached
      // to has been destroyed
      glDeleteShader( shader );
      Result:= True;
    end
  else
    Result:= False;
end;

function CreateVertexAndFragmentShader(VertexShader, FragmentShader: String
  ): TShader;
var
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
//    glGetProgramInterfaceiv( Shader.ShaderObj, GL_PROGRAM_INPUT, GL_ACTIVE_RESOURCES, @cnt );
//    glGetObjectParameterivARB( Shader.ShaderObj, GL_OBJECT_ACTIVE_ATTRIBUTES_ARB, @cnt );
    glGetProgramiv( Shader.ShaderObj, GL_ACTIVE_ATTRIBUTES, @cnt );
    for i:= 0 to cnt - 1 do
      with ( Shader.Attributes[ Shader.Attributes.Add( TShaderDecl.Create( Shader.ShaderObj, sdtAttribute, '', -1 ))]) do
        begin
          pname:= @str[ 0 ];

          glGetActiveAttrib( Shader.ShaderObj, i, 255, len, size, typ, pname );

          Name:= pname;
          VarType:= typ;
          Addr:= glGetAttribLocation( Shader.ShaderObj, pname );

          WriteLn( 'attribute ', GLAttrTypeToStr( VarType ), ' ', Name, ' @', Addr );
        end;
    //glGetProgramInterfaceiv( Shader.ShaderObj, GL_UNIFORM, GL_ACTIVE_RESOURCES, @cnt );
    glGetProgramiv( Shader.ShaderObj, GL_ACTIVE_UNIFORMS, @cnt );
    //glGetObjectParameterivARB( Shader.ShaderObj, GL_OBJECT_ACTIVE_UNIFORMS_ARB, @cnt );
    // for i in 0 to count:
    for i:= 0 to cnt - 1 do
      with ( Shader.Uniforms[ Shader.Uniforms.Add( TShaderDecl.Create( Shader.ShaderObj, sdtUniform, '', -1 ))]) do
        begin
          pname:= @str[ 0 ];

          glGetActiveUniform( Shader.ShaderObj, i, 255, len, size, typ, pname );

          Name:= pname;
          VarType:= typ;
          Addr:= glGetUniformLocation( Shader.ShaderObj, pname );

          WriteLn( 'uniform ', GLAttrTypeToStr( VarType ), ' ', Name, ' @', Addr );
        end;
  end;

  var
    Res: Integer;
    failed: Boolean;
    status: String;
begin
  Result := TShader.Create;
  Result.ShaderObj:= glCreateProgram();

  Res:= Ord( shaderAttachFromSrc( Result.ShaderObj, GL_VERTEX_SHADER, VertexShader ) AND
             shaderAttachFromSrc( Result.ShaderObj, GL_FRAGMENT_SHADER, FragmentShader ));

{  if ( Res = 0 ) then
    begin
      DeleteShader( Result.ShaderObj );
      FreeAndNil( Result );
      exit;
    end;}
  glLinkProgram( Result.ShaderObj ); // link the shader program

  //glGetShaderiv( Result.ShaderObj, GL_LINK_STATUS, @Res );
  //CAUSING INVALID OPERATION ERRORS
  {if ( Res = GL_FALSE ) then // clean up if linking failed
    begin
      WriteLn( 'Linking failed: ', ProgramCheckForErrors( Result.ShaderObj ));
      DeleteShader( Result.ShaderObj );
      FreeAndNil( Result );
    end
  else}
    begin // shader compiled successfully
      glUseProgram( Result.ShaderObj );
      DebugParams( Result ); // show attributes and uniforms
      ShaderDisable;
    end;

  // creating vertex and fragment shader objects
{  FragmentShaderObject:= glCreateShader( GL_FRAGMENT_SHADER );
  VertexShaderObject:= glCreateShader( GL_VERTEX_SHADER );

  // loading the source from string
  len:= Length( VertexShader );
  glShaderSource( VertexShaderObject, 1, @VertexShader, @len );
  len:= Length( FragmentShader );
  glShaderSource( FragmentShaderObject, 1, @FragmentShader, @len );

  // compile shaders ...
  glCompileShader( FragmentShaderObject );
  glCompileShader( VertexShaderObject );

  // and check for compilation errors

  // vertex shader
  Res:= 0;
  glGetShaderiv( VertexShaderObject, GL_COMPILE_STATUS, @Res );
  failed:= Res = GL_FALSE;
  status:= ShaderCheckForErrors( VertexShaderObject );
  if ( Res <> GL_FALSE ) then // attach vertex shader
    glAttachShader( Result.ShaderObj, VertexShaderObject );
  if ( status > '' ) then
    WriteLn( 'Vertex shader: ' + #13#10, status );

  // fragment shader
  Res:= 0;
  glGetShaderiv( FragmentShaderObject, GL_COMPILE_STATUS, @Res );
  if ( not failed ) then
    failed:= Res = GL_FALSE;
  status:= ShaderCheckForErrors( FragmentShaderObject );
  if ( Res <> GL_FALSE ) then // attach fragment shader
    glAttachShader( Result.ShaderObj, FragmentShaderObject );
  if ( status > '' ) then
    WriteLn( 'Fragment shader: ' + #13#10, status );

  // delete the shader objects
  glDeleteShader( FragmentShaderObject );
  glDeleteShader( VertexShaderObject );


  if ( failed ) then // cleanup if compilation failed
    begin
      DeleteShader( Result.ShaderObj );
      FreeAndNil( Result );
    end
  else
    begin
      glLinkProgram( Result.ShaderObj ); // link the shader program

      glGetShaderiv( Result.ShaderObj, GL_LINK_STATUS, @Res );
      if ( Res = GL_FALSE ) then // clean up if linking failed
        begin
          DeleteShader( Result.ShaderObj );
          FreeAndNil( Result );
          WriteLn( 'Linking failed: ', ShaderCheckForErrors( Result.ShaderObj ));
        end
      else
        begin // shader compiled successfully
          glUseProgram( Result.ShaderObj );
          DebugParams( Result ); // show attributes and uniforms
          ShaderDisable;
        end;
    end;}
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
  glUniformMatrix4fvARB( glGetUniformLocationARB( shader, p), 1, False, @Value );
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
//  UpdateUniforms();
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

procedure TShader.UpdateUniforms;
var
  i: Integer;
begin
  for i:= 0 to Uniforms.Count - 1 do
    Uniforms[ i ].Addr:= glGetUniformLocation( ShaderObj, PChar( Uniforms[ i ].Name ));
end;

end.

