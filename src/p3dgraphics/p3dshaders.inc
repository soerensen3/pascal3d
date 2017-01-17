//p3dshaders.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

type
  Float = Single;

  { TP3DShaderDecl }

  TP3DShaderDeclType = ( sdtUniform, sdtAttribute );

  TP3DShaderDecl = class
    private
      FAddr: GLuint;
      FName: String;
      FProgramObject: GLuint;
      FShaderDeclType: TP3DShaderDeclType;
      FVarType: Integer;

      function GetAsFloat: Float;
      function GetAsFloatList: TFloatList;
      function GetAsInt: Integer;
      function GetAsIntList: TIntList;
      function GetAsMat2: TMat2;
      function GetAsMat3: TMat3;
      function GetAsMat4: TMat4;
      function GetAsQuatList: TQuaternionList;
      function GetAsString: String;
      function GetAsVec2: TVec2;
      function GetAsVec2List: TVec2List;
      function GetAsVec3: TVec3;
      function GetAsVec3List: TVec3List;
      function GetAsVec4: TVec4;
      function GetAsVec4List: TVec4List;
      procedure SetAsFloat( AValue: Float );
      procedure SetAsFloatList(AValue: TFloatList);
      procedure SetAsInt( AValue: Integer );
      procedure SetAsIntList(AValue: TIntList);
      procedure SetAsMat2( AValue: TMat2 );
      procedure SetAsMat3( AValue: TMat3 );
      procedure SetAsMat4( AValue: TMat4 );
      procedure SetAsQuatList(AValue: TQuaternionList);
      procedure SetAsVec2( AValue: TVec2 );
      procedure SetAsVec2List(AValue: TVec2List);
      procedure SetAsVec3( AValue: TVec3 );
      procedure SetAsVec3List(AValue: TVec3List);
      procedure SetAsVec4( AValue: TVec4 );
      procedure SetAsVec4List(AValue: TVec4List);

    public
      constructor Create( AProgramObject: GLuint; ADeclType: TP3DShaderDeclType; AName: String; AAddr: GLuint );

      property AsInt: Integer read GetAsInt write SetAsInt;
      property AsFloat: Float read GetAsFloat write SetAsFloat;
      property AsVec2: TVec2 read GetAsVec2 write SetAsVec2;
      property AsVec3: TVec3 read GetAsVec3 write SetAsVec3;
      property AsVec4: TVec4 read GetAsVec4 write SetAsVec4;
      property AsMat2: TMat2 read GetAsMat2 write SetAsMat2;
      property AsMat3: TMat3 read GetAsMat3 write SetAsMat3;
      property AsMat4: TMat4 read GetAsMat4 write SetAsMat4;
      property AsIntList: TIntList read GetAsIntList write SetAsIntList;
      property AsFloatList: TFloatList read GetAsFloatList write SetAsFloatList;
      property AsVec2List: TVec2List read GetAsVec2List write SetAsVec2List;
      property AsVec3List: TVec3List read GetAsVec3List write SetAsVec3List;
      property AsVec4List: TVec4List read GetAsVec4List write SetAsVec4List;
      property AsQuatList: TQuaternionList read GetAsQuatList write SetAsQuatList;
      property AsString: String read GetAsString;
      property VarType: Integer read FVarType write FVarType;
      property DeclType: TP3DShaderDeclType read FShaderDeclType write FShaderDeclType;
      property ProgramObject: GLuint read FProgramObject write FProgramObject;
      property Name: String read FName write FName;
      property Addr: GLuint read FAddr write FAddr;
  end;

  { TP3DShaderDeclList }
  TP3DCustomShaderDeclList = specialize gP3DCustomObjectList < TP3DShaderDecl >;

  TP3DShaderDeclList = class ( TP3DCustomShaderDeclList )
    private
      function GetNames( Name: String ): TP3DShaderDecl;
      procedure SetNames( Name: String ; AValue: TP3DShaderDecl );

    published
      function FindByName( Name: String ): Integer;
      function FindByName(Name: String; out Decl: TP3DShaderDecl): Boolean;
      function AddrByName( Name: String ): GLint;
      procedure Delete( Index: Integer ); override;
      procedure Clear( const FreeObjects: Boolean = True ); override;

    public
      property Names[ AName: String ]: TP3DShaderDecl read GetNames write SetNames;
  end;

  TP3DShader = class ( TP3DDataBlock )
    private
      FAttributes: TP3DShaderDeclList;
      FFShader: String;
      FProgramObj: GLHandle;
      FUniforms: TP3DShaderDeclList;
      FVShader: String;
      FVShaderWatch: TP3DFileWatch;
      FFShaderWatch: TP3DFileWatch;
      procedure SetFShader(AValue: String);
      procedure SetVShader(AValue: String);
      procedure ScanShaderObjects;
      procedure LinkProgram;

    public
      procedure Enable;
      procedure Disable;

      constructor Create( const AData: TP3DLibrary = nil ); override;
      constructor CreateFromVertexAndFragmentShaderFile( fnvshader, fnfshader: String; const AData: TP3DLibrary = nil );
      constructor CreateFromVertexAndFragmentShader( AVShader, AFShader: String; const AData: TP3DLibrary = nil );
      procedure FastDestroy; override;

      class function DOMNodeName: String; override;

      procedure UpdateUniforms();
      procedure BindAttribLocations();
      function DumpUniforms: String;
      function DumpAttributes: String;
      procedure Reload; reintroduce;

    published
      property ProgramObj: GLHandle read FProgramObj write FProgramObj;
      property Uniforms: TP3DShaderDeclList read FUniforms write FUniforms;
      property Attributes: TP3DShaderDeclList read FAttributes write FAttributes;
      property VShaderWatch: TP3DFileWatch read FVShaderWatch;
      property FShaderWatch: TP3DFileWatch read FFShaderWatch;
      property VShader: String read FVShader write SetVShader;
      property FShader: String read FFShader write SetFShader;
  end;
  TP3DShaderList = specialize gP3DCustomDatablockList < TP3DShader >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DShaderDecl }

function TP3DShaderDecl.GetAsInt: Integer;
begin
  glGetUniformiv( ProgramObject, Addr, @Result ); cle( Self );
end;

function TP3DShaderDecl.GetAsIntList: TIntList;
var
  BaseName: String;
  decl: GLuint;
  tmp: Integer;
  i: Integer;
begin
  case DeclType of
    sdtUniform:
      begin
        Result:= TIntList.Create;
        BaseName:= Copy2Symb( Name, '[' );
        i:= 0;
        decl:= Addr;
        while decl > 0 do
          begin
            glGetUniformiv( ProgramObject, Addr, @tmp ); cle( Self );
            Result.Add( tmp );
            Inc( i );
            decl:= glGetUniformLocation( ProgramObject, PChar( BaseName + '[' + IntToStr( i ) + ']' )); cle( Self );
          end;
      end;
    sdtAttribute: Result:= nil;
  end;
end;

function TP3DShaderDecl.GetAsFloat: Float;
begin
  case DeclType of
    sdtUniform: begin glGetUniformfv( ProgramObject, Addr, @Result ); cle( Self ); end;
    sdtAttribute: Result:= 0;
  end;
end;

function TP3DShaderDecl.GetAsFloatList: TFloatList;
var
  BaseName: String;
  decl: GLuint;
  tmp: Float;
  i: Integer;
begin
  case DeclType of
    sdtUniform:
      begin
        Result:= TFloatList.Create;
        BaseName:= Copy2Symb( Name, '[' );
        i:= 0;
        decl:= Addr;
        while decl > 0 do
          begin
            glGetUniformfv( ProgramObject, Addr, @tmp ); cle( Self );
            Result.Add( tmp );
            Inc( i );
            decl:= glGetUniformLocation( ProgramObject, PChar( BaseName + '[' + IntToStr( i ) + ']' )); cle( Self );
          end;
      end;
    sdtAttribute: Result:= nil;
  end;
end;

function TP3DShaderDecl.GetAsMat2: TMat2;
begin
  case DeclType of
    sdtUniform: begin glGetUniformfv( ProgramObject, Addr, @Result ); cle( Self ); end;
    sdtAttribute: Result:= mat2( 0 );
  end;
end;

function TP3DShaderDecl.GetAsMat3: TMat3;
begin
  case DeclType of
    sdtUniform: begin glGetUniformfv( ProgramObject, Addr, @Result ); cle( Self ); end;
    sdtAttribute: Result:= mat3( 0 );
  end;
end;

function TP3DShaderDecl.GetAsMat4: TMat4;
begin
  case DeclType of
    sdtUniform: begin glGetUniformfv( ProgramObject, Addr, @Result ); cle( Self ); end;
    sdtAttribute: Result:= mat4( 0 );
  end;
end;

function TP3DShaderDecl.GetAsQuatList: TQuaternionList;
var
  BaseName: String;
  decl: GLuint;
  tmp: TQuaternion;
  i: Integer;
begin
  case DeclType of
    sdtUniform:
      begin
        Result:= TQuaternionList.Create;
        BaseName:= Copy2Symb( Name, '[' );
        i:= 0;
        decl:= Addr;
        while decl > 0 do
          begin
            glGetUniformfv( ProgramObject, Addr, @tmp ); cle( Self );
            Result.Add( tmp );
            Inc( i );
            decl:= glGetUniformLocation( ProgramObject, PChar( BaseName + '[' + IntToStr( i ) + ']' )); cle( Self );
          end;
      end;
    sdtAttribute: Result:= nil;
  end;
end;

function TP3DShaderDecl.GetAsString: String;
begin
  Result:= '';
  case DeclType of
    sdtUniform:
      case VarType of
        GL_INT,
        GL_SAMPLER_1D,
        GL_SAMPLER_2D,
        GL_SAMPLER_3D,
        GL_SAMPLER_CUBE: Result:= IntToStr( GetAsInt );
        GL_FLOAT: Result:= FloatToStr( GetAsFloat );
        GL_FLOAT_VEC2: Result:= GetAsVec2.ToString();
        GL_FLOAT_VEC3: Result:= GetAsVec3.ToString();
        GL_FLOAT_VEC4: Result:= GetAsVec4.ToString();
        GL_FLOAT_MAT2: Result:= GetAsMat2.ToString( True );
        GL_FLOAT_MAT3: Result:= GetAsMat3.ToString( True );
        GL_FLOAT_MAT4: Result:= GetAsMat4.ToString( True );
      end;
  end;
end;

function TP3DShaderDecl.GetAsVec2: TVec2;
begin
  case DeclType of
    sdtUniform: begin glGetUniformfv( ProgramObject, Addr, @Result ); cle( Self ); end;
    sdtAttribute: Result:= vec2( 0 );
  end;
end;

function TP3DShaderDecl.GetAsVec2List: TVec2List;
var
  BaseName: String;
  decl: GLuint;
  tmp: TVec2;
  i: Integer;
begin
  case DeclType of
    sdtUniform:
      begin
        Result:= TVec2List.Create;
        BaseName:= Copy2Symb( Name, '[' );
        i:= 0;
        decl:= Addr;
        while decl > 0 do
          begin
            glGetUniformfv( ProgramObject, Addr, @tmp ); cle( Self );
            Result.Add( tmp );
            Inc( i );
            decl:= glGetUniformLocation( ProgramObject, PChar( BaseName + '[' + IntToStr( i ) + ']' )); cle( Self );
          end;
      end;
    sdtAttribute: Result:= nil;
  end;
end;

function TP3DShaderDecl.GetAsVec3: TVec3;
begin
  case DeclType of
    sdtUniform: begin glGetUniformfv( ProgramObject, Addr, @Result ); cle( Self ); end;
    sdtAttribute: Result:= vec3( 0 );
  end;
end;

function TP3DShaderDecl.GetAsVec3List: TVec3List;
var
  BaseName: String;
  decl: GLuint;
  tmp: TVec3;
  i: Integer;
begin
  case DeclType of
    sdtUniform:
      begin
        Result:= TVec3List.Create;
        BaseName:= Copy2Symb( Name, '[' );
        i:= 0;
        decl:= Addr;
        while decl > 0 do
          begin
            glGetUniformfv( ProgramObject, Addr, @tmp ); cle( Self );
            Result.Add( tmp );
            Inc( i );
            decl:= glGetUniformLocation( ProgramObject, PChar( BaseName + '[' + IntToStr( i ) + ']' )); cle( Self );
          end;
      end;
    sdtAttribute: Result:= nil;
  end;
end;

function TP3DShaderDecl.GetAsVec4: TVec4;
begin
  case DeclType of
    sdtUniform: begin glGetUniformfv( ProgramObject, Addr, @Result ); cle( Self ); end;
    sdtAttribute: Result:= vec4( 0 );
  end;
end;

function TP3DShaderDecl.GetAsVec4List: TVec4List;
var
  BaseName: String;
  decl: GLuint;
  tmp: TVec4;
  i: Integer;
begin
  case DeclType of
    sdtUniform:
      begin
        Result:= TVec4List.Create;
        BaseName:= Copy2Symb( Name, '[' );
        i:= 0;
        decl:= Addr;
        while decl > 0 do
          begin
            glGetUniformfv( ProgramObject, Addr, @tmp ); cle( Self );
            Result.Add( tmp );
            Inc( i );
            decl:= glGetUniformLocation( ProgramObject, PChar( BaseName + '[' + IntToStr( i ) + ']' )); cle( Self );
          end;
      end;
    sdtAttribute: Result:= nil;
  end;
end;

procedure TP3DShaderDecl.SetAsFloat(AValue: Float);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform1f( Addr, AValue ); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsFloatList(AValue: TFloatList);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform1fv( Addr, AValue.Count, AValue.PtrTo( 0 )); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsInt(AValue: Integer);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform1i( Addr, AValue ); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsIntList(AValue: TIntList);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform1iv( Addr, AValue.Count, AValue.PtrTo( 0 )); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsMat2(AValue: TMat2);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniformMatrix2fv( Addr, 1, False, @AValue ); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsMat3(AValue: TMat3);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniformMatrix3fv( Addr, 1, False, @AValue ); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsMat4(AValue: TMat4);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniformMatrix4fv( Addr, 1, False, @AValue.m[ 0 ]); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsQuatList(AValue: TQuaternionList);
var
  i: Integer;
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform:
      //glUniform4fv( Addr, AValue.Count, AValue.Ptr );
      for i:= 0 to AValue.Count - 1 do
        glUniform4f( Addr + i, AValue[ i ].x, AValue[ i ].y, AValue[ i ].z, AValue[ i ].w );//begin glUniform4fv( Addr, AValue.Count, AValue.PtrTo( 0 )); cle( Self ); end;
  end;
  //for i:= 0 to 10 do
  //  WriteLn( AValue[ i ].ToString());
end;

procedure TP3DShaderDecl.SetAsVec2(AValue: TVec2);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform2fv( Addr, 1, @AValue ); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsVec2List(AValue: TVec2List);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform2fv( Addr, AValue.Count, AValue.PtrTo( 0 )); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsVec3(AValue: TVec3);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform3fv( Addr, 1, @AValue ); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsVec3List(AValue: TVec3List);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform3fv( Addr, AValue.Count, AValue.PtrTo( 0 )); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsVec4(AValue: TVec4);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform4fv( Addr, 1, @AValue ); cle( Self ); end;
  end;
end;

procedure TP3DShaderDecl.SetAsVec4List(AValue: TVec4List);
begin
  if ( not Assigned( Self )) then
    exit;
  case DeclType of
    sdtUniform: begin glUniform4fv( Addr, AValue.Count, AValue.PtrTo( 0 )); cle( Self ); end;
  end;
end;

constructor TP3DShaderDecl.Create(AProgramObject: GLuint;
  ADeclType: TP3DShaderDeclType; AName: String; AAddr: GLuint);
begin
  inherited Create;
  DeclType:= ADeclType;
  ProgramObject:= AProgramObject;
  Name:= AName;
  Addr:= AAddr;
end;

function LoadShaderToText( var FName: String; const do_replace: Boolean = True ): String;
var
  F: TStringList;
  FileName: String;

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
  FileName:= P3DSearchPaths.Shaders.FindFileEx( FName, GetCurrentDir );

  if ( FileExists( FileName )) then
    begin
      F:= TStringList.Create;
      {$IFDEF VERBOSE}
      WriteLn( 'Loading ', FileName, '...' );
      {$ENDIF}
      F.LoadFromFile( FileName );

      if ( do_replace ) then
        StartReplace;

      F.SaveToFile( FileName +'.tmp' );
      Result:= F.Text;

      F.Free;
      FName:= FileName;
    end
  else
    P3DLog.LogException( Nil, 'Error: Shader source does not exist: "' + FName + '"' );
end;

procedure DeleteProgram( Shader: GLHandle );
begin
  glDeleteProgram( Shader ); cle( nil );
end;

function ProgramCheckForErrors( glProgram: GLHandle ): String;
var
  blen: GLInt;
  InfoLog : PGLChar;
  slen: PGLsizei;
begin
  glGetProgramiv( glProgram, GL_INFO_LOG_LENGTH, @blen ); cle( nil );
  slen:= Nil;
  if ( blen > 1 ) then
    begin
      GetMem( InfoLog, blen * SizeOf( GLCharARB ));
      glGetProgramInfoLog( glProgram, blen, slen, InfoLog ); cle( nil );
      Result:= PChar( InfoLog );
      Dispose( InfoLog );
    end;
end;

function ExtractText(const Str: string; const Delim1, Delim2: String ): string;
var
  pos1, pos2: integer;
begin
  result := '';
  pos1 := Pos(Delim1, Str);
  pos2 := Pos(Delim2, Str);
  if (pos1 > 0) and (pos2 > pos1) then
    result := Copy(Str, pos1 + 1, pos2 - pos1 - 1);
end;

function ShaderCheckForErrors( glShader: GLHandle; Shader: String ): String;
var
  blen, slen: GLInt;
  InfoLog : PGLChar;
  E: TStringList; //Errors
  C: TStringList; //Code
  line: Integer;
  i: Integer;
  msg: String;
  left_str, right_str: String;
  vendor: String;
begin
  glGetShaderiv( glShader, GL_INFO_LOG_LENGTH, @blen ); cle( nil );
  if ( blen > 1 ) then
    begin
      GetMem( InfoLog, blen * SizeOf( GLCharARB ));

      glGetShaderInfoLog( glShader, blen , slen, InfoLog ); cle( nil );

      E:= TStringList.Create;
      E.Text:= Trim( PChar( InfoLog ));
      Dispose( InfoLog );

      C:= TStringList.Create;
      C.Text:= Shader;

      vendor:= glGetString( GL_VENDOR );
      case vendor of
        'ATI Technologies': //UNKNOWN IF WORKING
          begin
            left_str:= 'ERROR: 0:';
            right_str:= ': error';
          end;
        'NVIDIA Corporation': //UNKNOWN IF WORKING
          begin
            left_str:= '(';
            right_str:= ') : error';
          end;
        'INTEL', 'Intel Open Source Technology Center':
          begin
            left_str:= ':';
            right_str:= '(';
          end;
      end;

      for i:= E.Count - 1 downto 0 do
        begin
          line:= StrToIntDef( ExtractText( E[ i ], left_str, right_str ), -1 );
          if ( line > -1 ) then
            begin
              msg:= '<pre><div class="hljs hljs-line-numbers" style="float: left;">' + IntToStr( line ) +
                '</div><code class="glsl">' + C[ line - 1 ] + '</code></pre>';
              E.Insert( i + 1, msg );
            end;
        end;
      Result:= E.Text;
      E.Free;
      C.Free;
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
  shader:= glCreateShader( SType ); cle( nil );
  Len:= Length( Source );
  glShaderSource( shader, 1, @Source, @Len ); cle( nil );
  glCompileShader( shader ); cle( nil );

  // make sure the compilation was successful
  glGetShaderiv( shader, GL_COMPILE_STATUS, @Success ); cle( nil );
  if( not Success ) then
    begin
      glGetBooleanv( GL_SHADER_COMPILER, @Success ); cle( nil );
      if ( not Success ) then
        P3DLog.LogInfo( nil, 'Warning: Shader compiler not supported! Please make sure you have the latest driver installed and your video card supports shaders.' );

      case SType of
        GL_VERTEX_SHADER: typeN:= 'Vertex Shader';
        GL_FRAGMENT_SHADER: typeN:= 'Fragment Shader';
        GL_GEOMETRY_SHADER: typeN:= 'Geometry Shader';
      end;
      P3DLog.LogException( nil, Format( 'Compilation failed (%s): '+LineEnding+'%s', [ typeN, Trim( ShaderCheckForErrors( shader, Source ))]));
      glDeleteShader( shader ); cle( nil );
    end
  else
    Result:= shader;
end;

function shaderAttachFromSrc( SProgram: GLint; SType: GLenum; Src: String ): GLHandle;
begin
  // compile the shader
  Result:= CreateShaderFromSrc( Src, SType );

  if ( Result <> 0 ) then
    begin
      // attach the shader to the program
      glAttachShader( SProgram, Result ); cle( nil );

      // delete the shader - it won't actually be
      // destroyed until the program that it's attached
      // to has been destroyed
      //glDeleteShader( Result ); cle( nil );
    end;
end;

procedure ShaderSetParameteri( shader: GLHandleARB; const Name: String; Value: GLint );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharArb(Name);
  glUniform1iARB(glGetUniformLocationARB( shader, p), Value); cle( nil );
end;

procedure ShaderSetParameterf( shader: GLHandleARB; const Name: String; Value: GLfloat );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform1fARB(glGetUniformLocationARB( shader, p), Value); cle( nil );
end;

procedure ShaderSetParameter2f(shader: GLHandleARB; const Name: String;
  Vec2: TVec2);
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform2f( glGetUniformLocationARB( shader, p), Vec2.x, Vec2.y ); cle( nil );
end;

procedure ShaderSetParameter3f( shader: GLHandleARB; const Name: String; Vec3: TVec3 );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform3f( glGetUniformLocationARB( shader, p), Vec3.x, Vec3.y, Vec3.z ); cle( nil );
end;

procedure ShaderSetParameter4f( shader: GLHandleARB; const Name: String; Vec4: TVec4 );
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniform4f( glGetUniformLocationARB( shader, p), Vec4.x, Vec4.y, Vec4.z, Vec4.w ); cle( nil );
end;


procedure ShaderSetParameter4fv(shader: GLHandleARB; const Name: String;
  Value: TMat4);
var
  p: PGLcharARB;
  l: GLint;
begin
  p := PGLCharARB(Name);
  glUniformMatrix4fvARB( glGetUniformLocationARB( shader, p), 1, False, @Value ); cle( nil );
end;

{ TP3DShaderDeclList }

function TP3DShaderDeclList.GetNames(Name: String): TP3DShaderDecl;
var
  i: Integer;
begin
  i:= FindByName( Name );
  if ( i > -1 ) then
    Result:= Items[ i ]
  else
    Result:= nil;
end;

procedure TP3DShaderDeclList.SetNames(Name: String; AValue: TP3DShaderDecl);
var
  i: Integer;
begin
  i:= FindByName( Name );
  if ( i > -1 ) then
    FItems[ i ]:= AValue;
end;

function TP3DShaderDeclList.FindByName(Name: String): Integer;
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
  if (( Result = -1 ) and Assigned( P3DShaderActive )) then
    Result:= glGetAttribLocation( P3DShaderActive.ProgramObj, PChar( Name )); cle( Self );
end;

function TP3DShaderDeclList.FindByName(Name: String; out Decl: TP3DShaderDecl ): Boolean;
begin
  Decl:= Names[ Name ];
  Result:= Assigned( Decl );
end;

function TP3DShaderDeclList.AddrByName(Name: String): GLint;
var
  n: Integer;
begin
  n:= FindByName( Name );
  if ( n >= 0 ) then
    Result:= Items[ n ].Addr
  else
    Result:= -1;
end;

procedure TP3DShaderDeclList.Delete(Index: Integer);
begin
  Items[ Index ].Free;
  inherited Delete(Index);
end;

procedure TP3DShaderDeclList.Clear(const FreeObjects: Boolean);
var
  i: Integer;
begin
  if ( FreeObjects ) then
    for i:= Count - 1 downto 0 do
      Items[ i ].Free;
  Count:= 0;
end;

{ TP3DShader }

procedure TP3DShader.SetFShader(AValue: String);
begin
  if FFShader=AValue then Exit;
  FFShader:=AValue;
  LinkProgram;
end;

procedure TP3DShader.SetVShader(AValue: String);
begin
  if FVShader=AValue then Exit;
  FVShader:=AValue;
  LinkProgram;
end;

procedure TP3DShader.ScanShaderObjects;
var
  i: Integer;
  cnt: GLint;
  len, size, typ: Integer;
  pname: PChar;
  str: array[ 0..255 ] of Char;
  msg: String;

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
  Enable;
  cnt:= 0;
  Attributes.Clear( True );
  Uniforms.Clear( True );

  msg:= '';
  glGetError();
  glGetProgramiv( ProgramObj, GL_ACTIVE_ATTRIBUTES, @cnt ); cle( Self, 'ProgramObj = ' + IntToStr( ProgramObj ));
  for i:= 0 to cnt - 1 do
    with ( Attributes[ Attributes.Add( TP3DShaderDecl.Create( ProgramObj, sdtAttribute, '', -1 ))]) do
      begin
        pname:= @str[ 0 ];

        glGetActiveAttrib( ProgramObj, i, 255, len, size, typ, pname ); cle( Self );

        Name:= pname;
        VarType:= typ;
        Addr:= glGetAttribLocation( ProgramObj, pname ); cle( Self );

        {$IFDEF VERBOSE}
        WriteLn( 'attribute ', GLAttrTypeToStr( VarType ), ' ', Name, ' @', Addr );
        {$ENDIF}
        msg+= LineEnding + '<span>attribute</span>' + GLAttrTypeToStr( VarType ) + ' ' + Name + ' @' + IntToStr( Addr );
      end;

  glGetProgramiv( ProgramObj, GL_ACTIVE_UNIFORMS, @cnt );

  for i:= 0 to cnt - 1 do
    with ( Uniforms[ Uniforms.Add( TP3DShaderDecl.Create( ProgramObj, sdtUniform, '', -1 ))]) do
      begin
        pname:= @str[ 0 ];

        glGetActiveUniform( ProgramObj, i, 255, len, size, typ, pname ); cle( Self );

        Name:= pname;
        VarType:= typ;
        Addr:= glGetUniformLocation( ProgramObj, pname ); cle( Self );

        {$IFDEF VERBOSE}
        WriteLn( 'uniform ', GLAttrTypeToStr( VarType ), ' ', Name, ' @', Addr );
        {$ENDIF}
        msg+=  LineEnding + '<span>uniform</span>' + GLAttrTypeToStr( VarType ) + ' ' + Name + ' @' + IntToStr( Addr );
      end;
  P3DLog.LogInfoXML( Self, 'Shader successfully linked <p class="messageheader">' + Trim( msg ) + '</p>' );
end;

procedure TP3DShader.LinkProgram;
var
  Res, maxLength: Integer;
  _vshader, _fshader: GLHandle;
  isLinked: GLint = 0;
begin
  if ( ProgramObj > -1 ) then
    DeleteProgram( ProgramObj );
  ProgramObj:= glCreateProgram();

  _vshader:= shaderAttachFromSrc( ProgramObj, GL_VERTEX_SHADER, vshader );

  _fshader:= shaderAttachFromSrc( ProgramObj, GL_FRAGMENT_SHADER, fshader );

  BindAttribLocations();

  glLinkProgram( ProgramObj );

  glGetProgramiv( ProgramObj, GL_LINK_STATUS, @isLinked );

  glDetachShader( ProgramObj, _vshader );
  glDetachShader( ProgramObj, _fshader );

  if ( isLinked = GL_FALSE ) then
    ProgramCheckForErrors( ProgramObj )
  else
    ScanShaderObjects;

  //ScanDeclarations2; // show attributes and uniforms
  Disable;
end;

procedure TP3DShader.Enable;
begin
  P3DAttributes.DisableAllAttributes;
  glUseProgram( ProgramObj );
  P3DShaderActive:= Self;
//  UpdateUniforms();
end;

procedure TP3DShader.Disable;
begin
  P3DShaderActive:= nil;
  glUseProgram( 0 );
end;

constructor TP3DShader.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  ProgramObj:= -1;
  FUniforms:= TP3DShaderDeclList.Create;
  FAttributes:= TP3DShaderDeclList.Create;
  Owner.Shaders.Add( Self );
end;

constructor TP3DShader.CreateFromVertexAndFragmentShaderFile(fnvshader, fnfshader: String; const AData: TP3DLibrary);
var
  path: String;
begin
  Create( AData );
  VShader:= LoadShaderToText( fnvshader );
  FShader:= LoadShaderToText( fnfshader );
  CreateFromVertexAndFragmentShader( vshader, fshader, Owner );
  FVShaderWatch:= TP3DFileWatch.Create( fnvshader );
  FFShaderWatch:= TP3DFileWatch.Create( fnfshader );
end;

constructor TP3DShader.CreateFromVertexAndFragmentShader(AVShader, AFShader: String; const AData: TP3DLibrary);
var
  len: Integer;

  procedure ScanDeclarations2;
  var
    numActiveAttribs: GLint = 0;
    numActiveUniforms: GLint = 0;
  begin
    glGetProgramInterfaceiv( ProgramObj, GL_PROGRAM_INPUT, GL_ACTIVE_RESOURCES, @numActiveAttribs); cle( Self );
    glGetProgramInterfaceiv( ProgramObj, GL_UNIFORM, GL_ACTIVE_RESOURCES, @numActiveUniforms); cle( Self );
    {$IFDEF VERBOSE}
    WriteLn( 'ATTRIBUTES: ', numActiveAttribs, ' UNIFORMS: ', numActiveUniforms );
    {$ENDIF}
    //std::vector<GLchar> nameData(256);
    //std::vector<GLenum> properties;
    //std::vector<GLint> values(properties.size());
    {for ( i:=  0 to numActiveAttribs - 1 ) do
      glGetProgramResourceiv( ShaderObj, GL_PROGRAM_INPUT, i, properties.size(),
        &properties[0], values.size(), NULL, &values[0]);

      nameData.resize(properties[0]); //The length of the name.
      glGetProgramResourceName(prog, GL_PROGRAM_INPUT, attrib, nameData.size(), NULL, &nameData[0]);
      std::string name((char*)&nameData[0], nameData.size() - 1);
    }
  end;

var
  Res: Integer;

begin
  Create( AData );
  if ( Assigned( FVShaderWatch )) then
    FVShaderWatch.Free;
  if ( Assigned( FFShaderWatch )) then
    FFShaderWatch.Free;
  FVShader:= AVShader;
  FFShader:= AFShader;
  LinkProgram;
end;

procedure TP3DShader.FastDestroy;
begin
  if ( Assigned( FVShaderWatch )) then
    FVShaderWatch.Free;
  if ( Assigned( FFShaderWatch )) then
    FFShaderWatch.Free;
  DeleteProgram( ProgramObj );
  Owner.Shaders.Remove( Self );
  FAttributes.Clear( True );
  FAttributes.Free;
  FUniforms.Clear( True );
  FUniforms.Free;
  inherited FastDestroy;
end;

class function TP3DShader.DOMNodeName: String;
begin
  Result:= 'shader';
end;

procedure TP3DShader.UpdateUniforms;
var
  i: Integer;
begin
  for i:= 0 to Uniforms.Count - 1 do
    Uniforms[ i ].Addr:= glGetUniformLocation( ProgramObj, PChar( Uniforms[ i ].Name ));
end;

procedure TP3DShader.BindAttribLocations;
begin
  glBindAttribLocation( ProgramObj, P3DAttribPosition,  'Position'  );
  glBindAttribLocation( ProgramObj, P3DAttribNormal,    'Normal'    );
  glBindAttribLocation( ProgramObj, P3DAttribColor,     'Color'     );
  glBindAttribLocation( ProgramObj, P3DAttribCotangent, 'Cotangent' );
  glBindAttribLocation( ProgramObj, P3DAttribTangent,   'Tangent'   );
  glBindAttribLocation( ProgramObj, P3DAttribTexCoord0, 'TexCoord0' );
  glBindAttribLocation( ProgramObj, P3DAttribTexCoord1, 'TexCoord1' );
  glBindAttribLocation( ProgramObj, P3DAttribTexCoord2, 'TexCoord2' );
  glBindAttribLocation( ProgramObj, P3DAttribTexCoord3, 'TexCoord3' );
  glBindAttribLocation( ProgramObj, P3DAttribTexCoord4, 'TexCoord4' );
  glBindAttribLocation( ProgramObj, P3DAttribTexCoord5, 'TexCoord5' );
  glBindAttribLocation( ProgramObj, P3DAttribTexCoord6, 'TexCoord6' );
  glBindAttribLocation( ProgramObj, P3DAttribTexCoord7, 'TexCoord7' );
end;

function TP3DShader.DumpUniforms: String;
var
  i: Integer;
begin
  Result:= 'Uniforms of Shader ' + Name;
  for i:= 0 to Uniforms.Count - 1 do
    Result+= LineEnding + Format( '%s (@%d) = %s', [ Uniforms[ i ].Name, Uniforms[ i ].Addr, Uniforms[ i ].AsString ]);
end;

function TP3DShader.DumpAttributes: String;
var
  Attrib, i: Integer;
  s: String;
begin
  Result:= 'Attributes of Shader ' + Name;
  for i:= 0 to Attributes.Count - 1 do
    begin
      glGetVertexAttribiv( Attributes[ i ].Addr, GL_VERTEX_ATTRIB_ARRAY_DIVISOR, @Attrib );
      if ( Attrib > 0 ) then
        s:= ' /' + IntToStr( Attrib )
      else
        s:= '';
      glGetVertexAttribiv( Attributes[ i ].Addr, GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING, @Attrib );
      Result+= LineEnding + Format( '%s (@%d%s) = %d', [ Attributes[ i ].Name, Attributes[ i ].Addr, s, Attrib ]);
    end;
end;

procedure TP3DShader.Reload;
var
  Changed: Boolean;
  FileName: String;
begin
  //TODO: REIMPLEMENT
  {inherited Reload;
  Changed:= False;
  if ( Assigned( VShaderWatch ) and VShaderWatch.CheckForChange ) then
    begin
      Changed:= True;
      FileName:= VShaderWatch.FileName;
      VShader:= LoadShaderToText( FileName );
    end;
  if ( Assigned( FShaderWatch ) and FShaderWatch.CheckForChange ) then
    begin
      Changed:= True;
      FileName:= FShaderWatch.FileName;
      FShader:= LoadShaderToText( FileName );
    end;
  if ( Changed ) then
    LinkProgram;}
end;

{$ENDIF}

