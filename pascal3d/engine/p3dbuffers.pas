unit p3dbuffers;

{$mode objfpc}{$H+}

interface
  uses
    Classes, SysUtils, p3dMath, dglOpenGL;

  type
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

