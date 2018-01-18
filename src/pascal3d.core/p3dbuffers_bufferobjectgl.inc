{$IFDEF INTERFACE}
type
  TP3DBufferObjectGL = class ( TItemList
                               {$IFDEF USE_ATTRIB}
                               , IP3DAttribute
                               {$ENDIF})
    private
      FBufferObject: GLuint;
      FStatic: Boolean;
      {$IFDEF USE_ATTRIB}
      FIdx: Integer;
      FDivisor: Integer;

      procedure SetDivisor( AValue: Integer );
      {$ENDIF}
      procedure GenBufferObject;
      procedure DeleteBufferObject;
      procedure SetStatic(AValue: Boolean);
      function GetInstance: TObject;

    public
      constructor Create;
      constructor Create(const AStatic: Boolean);
      destructor Destroy; override;

      procedure Bind(const Target: GLenum = GL_ARRAY_BUFFER );
      procedure PushData;
      {$IFDEF USE_ATTRIB}
      procedure UnsetAttribArray();
      procedure SetAttribArray( idx: Integer );
      property Divisor: Integer read FDivisor write SetDivisor;
      {$ENDIF}

      property Static: Boolean read FStatic write SetStatic;
      property BufferObject: GLuint read FBufferObject write FBufferObject;
  end;

{$ENDIF}


{$IFDEF IMPLEMENTATION}
{ TP3DBufferObjectGL }

procedure TP3DBufferObjectGL.GenBufferObject;
begin
  glGenBuffers( 1, @FBufferObject ); cle( Self );
end;

procedure TP3DBufferObjectGL.DeleteBufferObject;
begin
  {$IFDEF USE_ATTRIB}
  if ( FIdx > -1 ) then
    UnsetAttribArray();
  {$ENDIF}
  glBindBuffer( GL_ARRAY_BUFFER, FBufferObject ); cle( Self );
  glDeleteBuffers( 1, @FBufferObject ); cle( Self );
end;

procedure TP3DBufferObjectGL.SetStatic(AValue: Boolean);
begin
  if FStatic=AValue then Exit;
  FStatic:=AValue;
  PushData;
end;

function TP3DBufferObjectGL.GetInstance: TObject;
begin
  Result:= Self;
end;

procedure TP3DBufferObjectGL.PushData;
var
  mode: Integer;
begin
  glBindBuffer( GL_ARRAY_BUFFER, FBufferObject ); cle( Self );
  if ( FStatic ) then
    mode:= GL_STATIC_DRAW
  else
    mode:= GL_DYNAMIC_DRAW;
  glBufferData( GL_ARRAY_BUFFER, GetItemSize * Count, PtrTo( 0 ), mode ); cle( Self );
end;

{$IFDEF USE_ATTRIB}
procedure TP3DBufferObjectGL.SetAttribArray(idx: Integer);
begin
  glBindBuffer( GL_ARRAY_BUFFER, FBufferObject ); cle( Self );
  glVertexAttribPointer( idx, GLNUMCOMPONENTS, GLTYPE, False, 0, nil ); cle( Self );
  glEnableVertexAttribArray( idx ); cle( Self );
  if ( Divisor > 0 ) then
    glVertexAttribDivisor( idx, Divisor );
  FIdx:= idx;
  P3DAttributes.Add( Self );
end;

procedure TP3DBufferObjectGL.UnsetAttribArray();
begin
  if ( FIdx > -1 ) then
    begin
      if ( Divisor > 0 ) then
        glVertexAttribDivisor( FIdx, 0 );
      glDisableVertexAttribArray( Fidx ); cle( Self );
      FIdx:= -1;
      P3DAttributes.Remove( Self );
    end;
end;

procedure TP3DBufferObjectGL.SetDivisor( AValue: Integer );
begin
  if ( FDivisor = AValue ) then
    exit;
  if ( FIdx > -1 ) then
    glVertexAttribDivisor( FIdx, AValue );
  FDivisor:= AValue;
end;

{$ENDIF}

constructor TP3DBufferObjectGL.Create;
begin
  GenBufferObject;
  {$IFDEF USE_ATTRIB}
  FIdx:= -1;
  FDivisor:= 0;
  {$ENDIF}
end;

constructor TP3DBufferObjectGL.Create( const AStatic: Boolean );
begin
  Create;
  FStatic:= AStatic;
end;

destructor TP3DBufferObjectGL.Destroy;
begin
  DeleteBufferObject;
  inherited Destroy;
end;

procedure TP3DBufferObjectGL.Bind( const Target: GLenum );
begin
  glBindBuffer( Target, FBufferObject ); cle( Self ); cle( Self );
end;



{$ENDIF}
