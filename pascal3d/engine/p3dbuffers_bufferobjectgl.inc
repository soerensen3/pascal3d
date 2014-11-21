{$IFDEF INTERFACE}

{ TP3DBufferObjectGL }

TP3DBufferObjectGL = class ( TItemList )
  private
    FBufferObject: GLuint;
    FStatic: Boolean;
    procedure GenBufferObject;
    procedure DeleteBufferObject;
    procedure SetStatic(AValue: Boolean);

  public
    constructor Create(const Static: Boolean);
    destructor Destroy; override;

    procedure Bind(const Target: GLenum = GL_ARRAY_BUFFER );
    procedure PushData;
    {$IFDEF USE_ATTRIB}
    procedure SetAttribArray( idx: Integer );
    {$ENDIF}

    property Static: Boolean read FStatic write SetStatic;
end;

{$ENDIF}


{$IFDEF IMPLEMENTATION}
{ TP3DBufferObjectGL }

procedure TP3DBufferObjectGL.GenBufferObject;
begin
  glGenBuffers( 1, @FBufferObject );
end;

procedure TP3DBufferObjectGL.DeleteBufferObject;
begin
  glBindBuffer( GL_ARRAY_BUFFER, FBufferObject );
  glDeleteBuffers( 1, @FBufferObject );
end;

procedure TP3DBufferObjectGL.SetStatic(AValue: Boolean);
begin
  if FStatic=AValue then Exit;
  FStatic:=AValue;
  PushData;
end;

procedure TP3DBufferObjectGL.PushData;
var
  mode: Integer;
begin
  glBindBuffer( GL_ARRAY_BUFFER, FBufferObject );
  if ( FStatic ) then
    mode:= GL_STATIC_DRAW
  else
    mode:= GL_DYNAMIC_DRAW;
  glBufferData( GL_ARRAY_BUFFER, GetItemSize * Count, PtrTo( 0 ), mode );
end;

{$IFDEF USE_ATTRIB}
procedure TP3DBufferObjectGL.SetAttribArray(idx: Integer);
begin
  glBindBuffer( GL_ARRAY_BUFFER, FBufferObject );
  glVertexAttribPointer( idx, GLNUMCOMPONENTS, GLTYPE, False, 0, nil );
  glEnableVertexAttribArray( idx );
end;
{$ENDIF}

constructor TP3DBufferObjectGL.Create( const Static: Boolean );
begin
  inherited Create;
  GenBufferObject;
end;

destructor TP3DBufferObjectGL.Destroy;
begin
  DeleteBufferObject;
  inherited Destroy;
end;

procedure TP3DBufferObjectGL.Bind( const Target: GLenum );
begin
  glBindBuffer( Target, FBufferObject );
end;

{$ENDIF}
