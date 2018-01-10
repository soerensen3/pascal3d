{$IFDEF INTERFACE}


  const //predefined attribute locations (https://www.opengl.org/sdk/docs/tutorials/ClockworkCoders/attributes.php)
    P3DAttribPosition           = 0;
    P3DAttribNormal             = 2;
    P3DAttribColor              = 3;
    P3DAttribCotangent          = 6;
    P3DAttribTangent            = 7;
{    P3DAttribTexCoord0          = 8;
    P3DAttribTexCoord1          = 9;
    P3DAttribTexCoord2          = 10;
    P3DAttribTexCoord3          = 11;
    P3DAttribTexCoord4          = 12;
    P3DAttribTexCoord5          = 13;
    P3DAttribTexCoord6          = 14;
    P3DAttribTexCoord7          = 15;}


  type

    { TP3DBufferObjectGL }
    IP3DAttribute = interface ( IP3DBaseInterface )
      ['{29888C08-513D-4EFA-B916-6C50DDF240B8}']

      procedure UnsetAttribArray();
      procedure SetAttribArray( idx: Integer );
      procedure Free;
    end;

    { TP3DAttributeList }

    TP3DAttributeList = class ( specialize gP3DCustomObjectList < IP3DAttribute > )
      constructor Create;
      procedure DisableAllAttributes;
    end;

    { TP3DVertexBufferArray }

    TP3DVertexBufferArray = class ( TPersistent )
      private
        VBA: GLuint;

      public
        constructor Create;
        destructor Destroy; override;

        procedure Bind;
        procedure Unbind;
    end;

    TP3DIntAttrib = object
      public const
        GLType = GL_INT;
        GLNumComp = 1;
    end;

    TP3DIVec2Attrib = object
      public const
        GLType = GL_INT;
        GLNumComp = 2;
    end;

    TP3DIVec3Attrib = object
      public const
        GLType = GL_INT;
        GLNumComp = 3;
    end;

    TP3DIVec4Attrib = object
      public const
        GLType = GL_INT;
        GLNumComp = 4;
    end;

    TP3DFloatAttrib = object
      public const
        GLType = GL_FLOAT;
        GLNumComp = 1;
    end;

    TP3DVec2Attrib = object
      public const
        GLType = GL_FLOAT;
        GLNumComp = 2;
    end;

    TP3DVec3Attrib = object
      public const
        GLType = GL_FLOAT;
        GLNumComp = 3;
    end;

    TP3DVec4Attrib = object
      public const
        GLType = GL_FLOAT;
        GLNumComp = 4;
    end;

    { gP3DList }

    generic gP3DList < T > = class ( specialize gItemList < T > )
      public
        //procedure SaveToStream( AStream: TStream );
        //procedure LoadFromStream( AStream: TStream );
        //procedure LoadFromStream( AStream: TStream; ACount: Integer );
        //procedure AppendFromStream( AStream: TStream );
        //function Add( AItems: array of T ): Integer; overload;
    end;


    generic gP3DBufferObjectGL < T > = class ( specialize gP3DList < T > )
      private
        FBufferObject: GLuint;
        FStatic: Boolean;

        procedure GenBufferObject;
        procedure DeleteBufferObject; virtual;
        procedure SetStatic(AValue: Boolean);
        function GetInstance: TObject;

      public
        constructor Create; override;
        destructor Destroy; override;

        procedure Bind(const Target: GLenum = GL_ARRAY_BUFFER );
        procedure PushData;

        property Static: Boolean read FStatic write SetStatic;
        property BufferObject: GLuint read FBufferObject write FBufferObject;
    end;

    { gP3DBufferObjectGLAttrib }

    generic gP3DBufferObjectGLAttrib < T, TAttrib > = class( specialize gP3DBufferObjectGL < T >, IP3DAttribute )
      private
        FIdx: Integer;
        FDivisor: Integer;

        procedure SetDivisor( AValue: Integer );
        procedure DeleteBufferObject; override;

      public
        constructor Create; override;

        procedure UnsetAttribArray();
        procedure SetAttribArray( idx: Integer );
        property Divisor: Integer read FDivisor write SetDivisor;
    end;

    TP3DIntList =       specialize gP3DList                      < Int >;
    TP3DIVec2List =     specialize gP3DList                      < TIVec2 >;
    TP3DIVec3List =     specialize gP3DList                      < TIVec3 >;
    TP3DIVec4List =     specialize gP3DList                      < TIVec4 >;

    TP3DIntBufferGL =   specialize gP3DBufferObjectGLAttrib      < Int, TP3DIntAttrib >;
    TP3DIVec2BufferGL = specialize gP3DBufferObjectGLAttrib      < TIVec2, TP3DIVec2Attrib >;
    TP3DIVec3BufferGL = specialize gP3DBufferObjectGLAttrib      < TIVec3, TP3DIVec3Attrib >;
    TP3DIVec4BufferGL = specialize gP3DBufferObjectGLAttrib      < TIVec4, TP3DIVec4Attrib >;

    TP3DFloatList =     specialize gP3DList                      < Float >;
    TP3DVec2List =      specialize gP3DList                      < TVec2 >;
    TP3DVec3List =      specialize gP3DList                      < TVec3 >;
    TP3DVec4List =      specialize gP3DList                      < TVec4 >;
    TP3DQuatList =      specialize gP3DList                      < TQuat >;
    TP3DMat2BufferGL =  specialize gP3DBufferObjectGL            < TMat2 >;
    TP3DMat3BufferGL =  specialize gP3DBufferObjectGL            < TMat3 >;
    TP3DMat4BufferGL =  specialize gP3DBufferObjectGL            < TMat4 >;
    TP3DFloatBufferGL = specialize gP3DBufferObjectGLAttrib      < Float, TP3DFloatAttrib >;
    TP3DVec2BufferGL =  specialize gP3DBufferObjectGLAttrib      < TVec2, TP3DVec2Attrib >;
    TP3DVec3BufferGL =  specialize gP3DBufferObjectGLAttrib      < TVec3, TP3DVec3Attrib >;
    TP3DVec4BufferGL =  specialize gP3DBufferObjectGLAttrib      < TVec4, TP3DVec4Attrib >;
    TP3DQuatBufferGL =  specialize gP3DBufferObjectGLAttrib      < TQuat, TP3DVec4Attrib >;

    (*{$MACRO ON}

    {$DEFINE USE_ATTRIB}

    {$DEFINE GLTYPE:= GL_INT}
    {$DEFINE GLNUMCOMPONENTS:= 1}
    {$DEFINE TItemList:= TIntList}
    {$DEFINE TP3DBufferObjectGL:= TP3DIntBufferGL }
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLTYPE:= GL_FLOAT}
    {$DEFINE TItemList:= TFloatList}
    {$DEFINE TP3DBufferObjectGL:= TP3DFloatBufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 2}
    {$DEFINE TItemList:= TVec2List}
    {$DEFINE TP3DBufferObjectGL:= TP3DVec2BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 3}
    {$DEFINE TItemList:= TVec3List}
    {$DEFINE TP3DBufferObjectGL:= TP3DVec3BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 4}
    {$DEFINE TItemList:= TVec4List}
    {$DEFINE TP3DBufferObjectGL:= TP3DVec4BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 2}
    {$DEFINE GLTYPE:= GL_INT}
    {$DEFINE TItemList:= TIVec2List}
    {$DEFINE TP3DBufferObjectGL:= TP3DIVec2BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 3}
    {$DEFINE TItemList:= TIVec3List}
    {$DEFINE TP3DBufferObjectGL:= TP3DIVec3BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE GLNUMCOMPONENTS:= 4}
    {$DEFINE TItemList:= TIVec4List}
    {$DEFINE TP3DBufferObjectGL:= TP3DIVec4BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}


    {$UNDEF USE_ATTRIB}

    {$DEFINE GLTYPE:= GL_FLOAT}
    {$DEFINE TItemList:= TMat2List}
    {$DEFINE TP3DBufferObjectGL:= TP3DMat2BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE TItemList:= TMat3List}
    {$DEFINE TP3DBufferObjectGL:= TP3DMat3BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$DEFINE TItemList:= TMat4List}
    {$DEFINE TP3DBufferObjectGL:= TP3DMat4BufferGL}
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}*)

    TP3DIntBufferGLList = specialize gP3DCustomObjectList < TP3DIntBufferGL >;
    TP3DFloatBufferGLList = specialize gP3DCustomObjectList < TP3DFloatBufferGL >;
    TP3DVec2BufferGLList = specialize gP3DCustomObjectList < TP3DVec2BufferGL >;
    TP3DVec3BufferGLList = specialize gP3DCustomObjectList < TP3DVec3BufferGL >;
    TP3DVec4BufferGLList = specialize gP3DCustomObjectList < TP3DVec4BufferGL >;
    TP3DIVec2BufferGLList = specialize gP3DCustomObjectList < TP3DIVec2BufferGL >;
    TP3DIVec3BufferGLList = specialize gP3DCustomObjectList < TP3DIVec3BufferGL >;
    TP3DIVec4BufferGLList = specialize gP3DCustomObjectList < TP3DIVec4BufferGL >;
    TP3DMat2BufferGLList = specialize gP3DCustomObjectList < TP3DMat2BufferGL >;
    TP3DMat3BufferGLList = specialize gP3DCustomObjectList < TP3DMat3BufferGL >;
    TP3DMat4BufferGLList = specialize gP3DCustomObjectList < TP3DMat4BufferGL >;

    { gP3DPropAccessBufferGLList }
    type
    generic gP3DPropAccessBufferGLList  < TListProp, TListData, TPropType > = class ( specialize gP3DPropAccessList < TListProp, TListData, TPropType >)
      public
        procedure LoadFromStream(S: TStream);
        function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;
    end;

    generic gP3DPropAccessBufferGL  < TListProp, TListData, TPropType > = class ( specialize gP3DPropAccessList < TListProp, TListData, TPropType >)
      public
        procedure LoadFromStream(S: TStream);
        procedure SaveToStream(S: TStream);
        procedure LoadJSON(AValue: TJSONData); override;
        function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;
    end;

    TP3DPropAccessIntBufferGL   = specialize gP3DPropAccessBufferGL < TP3DIntBufferGL, Int, TP3DPropAccessInt >;
    TP3DPropAccessFloatBufferGL = specialize gP3DPropAccessBufferGL < TP3DFloatBufferGL, Float, TP3DPropAccessFloat >;
    TP3DPropAccessVec2BufferGL  = specialize gP3DPropAccessBufferGL < TP3DVec2BufferGL, TVec2, TP3DPropAccessVec2 >;
    TP3DPropAccessVec3BufferGL  = specialize gP3DPropAccessBufferGL < TP3DVec3BufferGL, TVec3, TP3DPropAccessVec3 >;
    TP3DPropAccessVec4BufferGL  = specialize gP3DPropAccessBufferGL < TP3DVec4BufferGL, TVec4, TP3DPropAccessVec4 >;
    TP3DPropAccessIVec2BufferGL = specialize gP3DPropAccessBufferGL < TP3DIVec2BufferGL, TIVec2, TP3DPropAccessIVec2 >;
    TP3DPropAccessIVec3BufferGL = specialize gP3DPropAccessBufferGL < TP3DIVec3BufferGL, TIVec3, TP3DPropAccessIVec3 >;
    TP3DPropAccessIVec4BufferGL = specialize gP3DPropAccessBufferGL < TP3DIVec4BufferGL, TIVec4, TP3DPropAccessIVec4 >;
//    TP3DPropAccessMat2BufferGL = specialize gP3DPropAccessBufferGL < TP3DMat2BufferGL, TMat2, TP3DPropAccessMat2 >;
//    TP3DPropAccessMat3BufferGL = specialize gP3DPropAccessBufferGL < TP3DMat3BufferGL, TMat3, TP3DPropAccessMat3 >;
//    TP3DPropAccessMat4BufferGL = specialize gP3DPropAccessBufferGL < TP3DMat4BufferGL, TMat4, TP3DPropAccessMat4 >;

    TP3DPropAccessIntBufferGLList   = specialize gP3DPropAccessList < TP3DIntBufferGLList, TP3DIntBufferGL, TP3DPropAccessIntBufferGL >;
    TP3DPropAccessFloatBufferGLList = specialize gP3DPropAccessList < TP3DFloatBufferGLList, TP3DFloatBufferGL, TP3DPropAccessFloatBufferGL >;
    TP3DPropAccessVec2BufferGLList  = specialize gP3DPropAccessList < TP3DVec2BufferGLList, TP3DVec2BufferGL, TP3DPropAccessVec2BufferGL >;
    TP3DPropAccessVec3BufferGLList  = specialize gP3DPropAccessList < TP3DVec3BufferGLList, TP3DVec3BufferGL, TP3DPropAccessVec3BufferGL >;
    TP3DPropAccessVec4BufferGLList  = specialize gP3DPropAccessList < TP3DVec4BufferGLList, TP3DVec4BufferGL, TP3DPropAccessVec4BufferGL >;
    TP3DPropAccessIVec2BufferGLList = specialize gP3DPropAccessList < TP3DIVec2BufferGLList, TP3DIVec2BufferGL, TP3DPropAccessIVec2BufferGL >;
    TP3DPropAccessIVec3BufferGLList = specialize gP3DPropAccessList < TP3DIVec3BufferGLList, TP3DIVec3BufferGL, TP3DPropAccessIVec3BufferGL >;
    TP3DPropAccessIVec4BufferGLList = specialize gP3DPropAccessList < TP3DIVec4BufferGLList, TP3DIVec4BufferGL, TP3DPropAccessIVec4BufferGL >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DVertexBufferArray }

constructor TP3DVertexBufferArray.Create;
begin
  glGenVertexArrays( 1, @VBA ); cle( Self );
  //if ( VBA = 0 ) then
  //  glGetError( );
end;

destructor TP3DVertexBufferArray.Destroy;
begin
  glDeleteVertexArrays( 1, @VBA ); cle( Self );
  inherited Destroy;
end;

procedure TP3DVertexBufferArray.Bind;
begin
  glBindVertexArray( VBA ); cle( Self );
end;

procedure TP3DVertexBufferArray.Unbind;
begin
  glBindVertexArray( 0 ); cle( Self );
end;


{ TP3DAttributeList }

constructor TP3DAttributeList.Create;
begin
  inherited Create;
  OwnsObjects:= False;
end;

procedure TP3DAttributeList.DisableAllAttributes;
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    Items[ i ].UnsetAttribArray();
end;

procedure gP3DPropAccessBufferGLList.LoadFromStream(S: TStream);
begin
  if ( not Assigned( AsValue )) then
    CreateObjectFromJSON( nil );
  AsValue.LoadFromStream( S );
end;

function gP3DPropAccessBufferGLList.GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess;
begin
  Result:=inherited GetArrayItemAsProp(Idx);
  Result.StoreMode:= smExternalBinary;
end;


 (* {$DEFINE USE_ATTRIB}

  {$DEFINE GLNUMCOMPONENTS:= 1}
  {$DEFINE TItemList:= TIntList}
  {$DEFINE TP3DBufferObjectGL:= TP3DIntBufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE TItemList:= TFloatList}
  {$DEFINE TP3DBufferObjectGL:= TP3DFloatBufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 2}
  {$DEFINE TItemList:= TVec2List}
  {$DEFINE TP3DBufferObjectGL:= TP3DVec2BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 3}
  {$DEFINE TItemList:= TVec3List}
  {$DEFINE TP3DBufferObjectGL:= TP3DVec3BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 4}
  {$DEFINE TItemList:= TVec4List}
  {$DEFINE TP3DBufferObjectGL:= TP3DVec4BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 2}
  {$DEFINE GLTYPE:= GL_INT}
  {$DEFINE TItemList:= TIVec2List}
  {$DEFINE TP3DBufferObjectGL:= TP3DIVec2BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 3}
  {$DEFINE TItemList:= TIVec3List}
  {$DEFINE TP3DBufferObjectGL:= TP3DIVec3BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE GLNUMCOMPONENTS:= 4}
  {$DEFINE TItemList:= TIVec4List}
  {$DEFINE TP3DBufferObjectGL:= TP3DIVec4BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$UNDEF USE_ATTRIB}

  {$DEFINE GLTYPE:= GL_FLOAT}
  {$DEFINE TItemList:= TMat2List}
  {$DEFINE TP3DBufferObjectGL:= TP3DMat2BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE TItemList:= TMat3List}
  {$DEFINE TP3DBufferObjectGL:= TP3DMat3BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE TItemList:= TMat4List}
  {$DEFINE TP3DBufferObjectGL:= TP3DMat4BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}
*)

{ gP3DList }
{

procedure gP3DList.SaveToStream(AStream: TStream);
begin
  AStream.WriteBuffer( Count, SizeOf( Integer ));
  AStream.WriteBuffer( PtrTo( 0 ), SizeOf( T ) * Count );
end;

procedure gP3DList.LoadFromStream(AStream: TStream);
var
  Size: Integer;
  pItem: Pointer;
begin
  AStream.ReadBuffer( Size, SizeOf( Integer ));
  Count:= Size;
  pItem:= PtrTo( 0 );
  AStream.ReadBuffer( pItem^, SizeOf( T ) * Count );
end;

procedure gP3DList.LoadFromStream(AStream: TStream; ACount: Integer);
var
  pItem: Pointer;
begin
  Count:= ACount;
  pItem:= PtrTo( 0 );
  AStream.ReadBuffer( pItem^, SizeOf( T ) * Count );
end;

procedure gP3DList.AppendFromStream(AStream: TStream);
var
  OldCount: Integer;
  NewCount: Cardinal;
  pItem: Pointer;
begin
  OldCount:= Count;
  NewCount:= AStream.ReadDWord;
  Count:= OldCount + NewCount;
  pItem:= PtrTo( 0 );
  AStream.ReadBuffer( pItem, SizeOf( T ) * NewCount );
end;

function gP3DList.Add(AItems: array of T): Integer;
var
  i: Integer;
begin
  Result:= Count;
  for i:= Low( AItems ) to High( AItems ) do
    Add( AItems[ I ]);
end;
}
{ gP3DPropAccessBufferGL }

procedure gP3DPropAccessBufferGL.LoadFromStream(S: TStream);
begin
  if ( not Assigned( AsValue )) then
    CreateObjectFromJSON( nil );

  AsValue.Growth:= 256;
  WriteLn( ClassName + '.LoadFromStream ', AsValue.Growth );
  AsValue.LoadFromStream( S );
end;

procedure gP3DPropAccessBufferGL.SaveToStream(S: TStream);
begin
  if ( Assigned( AsValue )) then
    AsValue.SaveToStream( S );
end;

procedure gP3DPropAccessBufferGL.LoadJSON(AValue: TJSONData);
var
  S: TJSONStringType;
  Obj: IP3DJSONStreamBinary;
  n: LongInt;
begin
  if ( not Assigned( AsValue )) then
    AsValue:= TList.Create;
  if (( AValue.JSONType = jtString ) and
      ( AValue.AsString[ 1 ] = '@' )) then
    begin
      S:= AValue.AsString;
      if ( not Assigned( Root )) then
        raise Exception.Create( 'Root not assigned: ' + Name );
      if ( not Root.GetInterface( IP3DJSONStreamBinary, Obj )) then
        Exception.CreateFmt( 'Cannot load property of type "%s" from stream because the root does not support IP3DJSONStreamBinary!', [ ClassName ]);
      n:= StrToInt( Copy( S, 2, Length( S ) - 1 ));
      Obj.GetBinaryStream.Position:= n;
      LoadFromStream( Obj.GetBinaryStream );
    end
  else
    inherited LoadJSON(AValue);
end;

function gP3DPropAccessBufferGL.GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess;
begin
  Result:= inherited GetArrayItemAsProp(Idx);
  Result.StoreMode:= smExternalBinary;
end;

{ gP3DBufferObjectGL }

procedure gP3DBufferObjectGL.GenBufferObject;
begin
  glGenBuffers( 1, @FBufferObject );// cle( Self );
end;

procedure gP3DBufferObjectGL.DeleteBufferObject;
begin
  glBindBuffer( GL_ARRAY_BUFFER, FBufferObject );// cle( Self );
  glDeleteBuffers( 1, @FBufferObject );// cle( Self );
end;

procedure gP3DBufferObjectGL.SetStatic(AValue: Boolean);
begin
  if FStatic=AValue then Exit;
  FStatic:=AValue;
  PushData;
end;

function gP3DBufferObjectGL.GetInstance: TObject;
begin
  Result:= Self;
end;

procedure gP3DBufferObjectGL.PushData;
var
  mode: Integer;
begin
  glBindBuffer( GL_ARRAY_BUFFER, FBufferObject );// cle( Self );
  if ( FStatic ) then
    mode:= GL_STATIC_DRAW
  else
    mode:= GL_DYNAMIC_DRAW;
  glBufferData( GL_ARRAY_BUFFER, SizeOf( T ) * Count, PtrTo( 0 ), mode );// cle( Self );
end;

constructor gP3DBufferObjectGL.Create;
begin
  inherited Create;
  GenBufferObject;
  FStatic:= True;
end;


destructor gP3DBufferObjectGL.Destroy;
begin
  DeleteBufferObject;
  inherited Destroy;
end;

procedure gP3DBufferObjectGL.Bind( const Target: GLenum );
begin
  glBindBuffer( Target, FBufferObject ); //cle( Self );
end;


procedure gP3DBufferObjectGLAttrib.SetAttribArray(idx: Integer);
begin
  glBindBuffer( GL_ARRAY_BUFFER, FBufferObject ); //cle( Self );
  glVertexAttribPointer( idx, TAttrib.GLNUMCOMP, TAttrib.GLTYPE, False, 0, nil ); //cle( Self );
  glEnableVertexAttribArray( idx ); //cle( Self );
  if ( Divisor > 0 ) then
    glVertexAttribDivisor( idx, Divisor );
  FIdx:= idx;
  P3DAttributes.Add( Self );
end;

procedure gP3DBufferObjectGLAttrib.UnsetAttribArray();
begin
  if ( FIdx > -1 ) then
    begin
      if ( Divisor > 0 ) then
        glVertexAttribDivisor( FIdx, 0 );
      glDisableVertexAttribArray( Fidx ); //cle( Self );
      FIdx:= -1;
      P3DAttributes.Remove( Self );
    end;
end;

procedure gP3DBufferObjectGLAttrib.SetDivisor( AValue: Integer );
begin
  if ( FDivisor = AValue ) then
    exit;
  if ( FIdx > -1 ) then
    glVertexAttribDivisor( FIdx, AValue );
  FDivisor:= AValue;
end;

procedure gP3DBufferObjectGLAttrib.DeleteBufferObject;
begin
  if ( FIdx > -1 ) then
    UnsetAttribArray();
  inherited DeleteBufferObject;
end;

constructor gP3DBufferObjectGLAttrib.Create;
begin
  inherited Create;
  FIdx:= -1;
  FDivisor:= 0;
end;




{$ENDIF}


