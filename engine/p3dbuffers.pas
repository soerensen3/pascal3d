unit p3dbuffers;

{$mode objfpc}{$H+}

interface
  uses
    Classes, SysUtils, p3dMath, dglOpenGL;


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
    {$DEFINE INTERFACE}

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

      {$UNDEF USE_ATTRIB}

      {$DEFINE TItemList:= TMat2List}
      {$DEFINE TP3DBufferObjectGL:= TP3DMat2BufferGL}
      {$INCLUDE p3dbuffers_bufferobjectgl.inc}

      {$DEFINE TItemList:= TMat3List}
      {$DEFINE TP3DBufferObjectGL:= TP3DMat3BufferGL}
      {$INCLUDE p3dbuffers_bufferobjectgl.inc}

      {$DEFINE TItemList:= TMat4List}
      {$DEFINE TP3DBufferObjectGL:= TP3DMat4BufferGL}
      {$INCLUDE p3dbuffers_bufferobjectgl.inc}

    {$UNDEF INTERFACE}


implementation

{ TP3DVertexBufferArray }

constructor TP3DVertexBufferArray.Create;
begin
  glGenVertexArrays( 1, @VBA );
  if ( VBA = 0 ) then

    glGetError( );
end;

destructor TP3DVertexBufferArray.Destroy;
begin
  glDeleteVertexArrays( 1, @VBA );
  inherited Destroy;
end;

procedure TP3DVertexBufferArray.Bind;
begin
  glBindVertexArray( VBA );
end;

procedure TP3DVertexBufferArray.Unbind;
begin
  glBindVertexArray( 0 );
end;

{$DEFINE IMPLEMENTATION}

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

  {$UNDEF USE_ATTRIB}

  {$DEFINE TItemList:= TMat2List}
  {$DEFINE TP3DBufferObjectGL:= TP3DMat2BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE TItemList:= TMat3List}
  {$DEFINE TP3DBufferObjectGL:= TP3DMat3BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

  {$DEFINE TItemList:= TMat4List}
  {$DEFINE TP3DBufferObjectGL:= TP3DMat4BufferGL}
  {$INCLUDE p3dbuffers_bufferobjectgl.inc}

{$UNDEF IMPLEMENTATION}



end.

