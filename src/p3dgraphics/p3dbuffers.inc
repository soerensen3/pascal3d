{$IFDEF INTERFACE}


  const //predefined attribute locations (https://www.opengl.org/sdk/docs/tutorials/ClockworkCoders/attributes.php)
    P3DAttribPosition           = 0;
    P3DAttribNormal             = 2;
    P3DAttribColor              = 3;
    P3DAttribCotangent          = 6;
    P3DAttribTangent            = 7;
    P3DAttribTexCoord0          = 8;
    P3DAttribTexCoord1          = 9;
    P3DAttribTexCoord2          = 10;
    P3DAttribTexCoord3          = 11;
    P3DAttribTexCoord4          = 12;
    P3DAttribTexCoord5          = 13;
    P3DAttribTexCoord6          = 14;
    P3DAttribTexCoord7          = 15;


  type

    { TP3DBufferObjectGL }
    IP3DAttribute = interface
      ['{29888C08-513D-4EFA-B916-6C50DDF240B8}']

      procedure UnsetAttribArray();
      procedure SetAttribArray( idx: Integer );
      procedure Free;
    end;

    { TP3DAttributeList }

    TP3DAttributeList = class ( specialize gP3DCustomObjectList < IP3DAttribute > )
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

    {$MACRO ON}

    {$DEFINE USE_ATTRIB}

    {$DEFINE GLTYPE:= GL_INT}
    {$DEFINE GLNUMCOMPONENTS:= 1}
    {$DEFINE TItemList:= TIntList}
    {$DEFINE TP3DBufferObjectGL:= TP3DIntBufferGL}
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
    {$INCLUDE p3dbuffers_bufferobjectgl.inc}

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

procedure TP3DAttributeList.DisableAllAttributes;
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    Items[ i ].UnsetAttribArray();
end;



  {$DEFINE USE_ATTRIB}

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


{$ENDIF}


