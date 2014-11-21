unit p3dMath;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math;


type
  {$MACRO ON}
  Float = Single;

  {.$DEFINE COLUMN_MAJOR_ORDER}

  { TVec2 }
  {$DEFINE INTERFACE}
  {$DEFINE TVECTOR:= TVec2}
  {$DEFINE NUMFIELD:=2}
  {$INCLUDE p3dmath_vector.inc}

  { TVec3 }
  {$DEFINE INTERFACE}
  {$DEFINE TVECTOR:= TVec3}
  {$DEFINE NUMFIELD:=3}
  {$INCLUDE p3dmath_vector.inc}

  { TVec3 }
  {$DEFINE INTERFACE}
  {$DEFINE TVECTOR:= TVec4}
  {$DEFINE NUMFIELD:=4}
  {$INCLUDE p3dmath_vector.inc}

  { TMat2 }
  {$DEFINE INTERFACE}
  {$DEFINE TMATRIX:= TMat2}
  {$DEFINE TVECTOR:= TVec2}
  {$DEFINE NUMFIELD:=2}
  {$INCLUDE p3dmath_matrix.inc}

  { TMat3 }
  {$DEFINE INTERFACE}
  {$DEFINE TMATRIX:= TMat3}
  {$DEFINE TVECTOR:= TVec3}
  {$DEFINE NUMFIELD:=3}
  {$INCLUDE p3dmath_matrix.inc}

  { TMat4 }
  {$DEFINE INTERFACE}
  {$DEFINE TMATRIX:= TMat4}
  {$DEFINE TVECTOR:= TVec4}
  {$DEFINE NUMFIELD:=4}
  {$INCLUDE p3dmath_matrix.inc}

  //CONSTRUCTORS
  {$INCLUDE p3dmath_vector_constructors.inc}
  {$INCLUDE p3dmath_matrix_constructors.inc}

  //HELPER FUNCTIONS
  { TVec2 }
  {$DEFINE TVECTOR:= TVec2}
  {$DEFINE NUMFIELD:=2}
  {$DEFINE HELPERFUNCT}
  {$INCLUDE p3dmath_vector.inc}

  { TVec3 }
  {$DEFINE TVECTOR:= TVec3}
  {$DEFINE NUMFIELD:=3}
  {$DEFINE HELPERFUNCT}
  {$INCLUDE p3dmath_vector.inc}

  { TVec4 }
  {$DEFINE TVECTOR:= TVec4}
  {$DEFINE NUMFIELD:=4}
  {$DEFINE HELPERFUNCT}
  {$INCLUDE p3dmath_vector.inc}

  { TQuaternion }
  {$DEFINE INTERFACE}
  {$DEFINE TVECTOR:= TVec4}
  {$INCLUDE p3dmath_quaternion.inc}

  {$INCLUDE p3dmath_consts.inc}

  {Type Arrays}
  {$DEFINE INTERFACE}

  {$INCLUDE p3dmath_arrays_decl.inc}

  {$UNDEF INTERFACE}

implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dmath_vector_constructors.inc}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dmath_matrix_constructors.inc}

{ TVec2 }
{$DEFINE TVECTOR:= TVec2}
{$DEFINE NUMFIELD:=2}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dmath_vector.inc}

{ TVec3 }
{$DEFINE TVECTOR:= TVec3}
{$DEFINE NUMFIELD:=3}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dmath_vector.inc}

{ TVec4 }
{$DEFINE TVECTOR:= TVec4}
{$DEFINE NUMFIELD:=4}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dmath_vector.inc}

{ TMat2 }
{$DEFINE IMPLEMENTATION}
{$DEFINE TMATRIX:= TMat2}
{$DEFINE TVECTOR:= TVec2}
{$DEFINE NUMFIELD:=2}
{$INCLUDE p3dmath_matrix.inc}

{ TMat3 }
{$DEFINE IMPLEMENTATION}
{$DEFINE TMATRIX:= TMat3}
{$DEFINE TVECTOR:= TVec3}
{$DEFINE NUMFIELD:=3}
{$INCLUDE p3dmath_matrix.inc}

{ TMat4 }
{$DEFINE IMPLEMENTATION}
{$DEFINE TMATRIX:= TMat4}
{$DEFINE TVECTOR:= TVec4}
{$DEFINE NUMFIELD:=4}
{$INCLUDE p3dmath_matrix.inc}

{ TQuaternion }
{$DEFINE IMPLEMENTATION}
{$DEFINE TVECTOR:= TVec4}
{$INCLUDE p3dmath_quaternion.inc}


{Type Arrays}
{$DEFINE IMPLEMENTATION}

{$INCLUDE p3dmath_arrays_decl.inc}

end.
