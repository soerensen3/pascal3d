  {$IFDEF IMPLEMENTATION}
    {$DEFINE _IMPLEMENTATION}
    {$UNDEF IMPLEMENTATION}
  {$ENDIF}

  {$DEFINE CONSTRUCTOR}

  {CONSTRUCTORS}
  { TVec2 }
  {$DEFINE TVECTOR:= TVec2}
  {$DEFINE NUMFIELD:=2}
  {$DEFINE VECTOR:= vec2}

  //function vec2( x: Float; y: Float ); TVec2;
  {$DEFINE TYPEX:=Float}
  {$DEFINE TYPEY:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec2( xy: Float ); TVec2;
  {$DEFINE TYPEX:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec2( xy: TVec3 ); TVec2;
  {$DEFINE TYPEX:=TVec3}
  {$INCLUDE math3d_vector.inc}

  //function vec2( xy: TVec4 ); TVec2;
  {$DEFINE TYPEX:=TVec4}
  {$INCLUDE math3d_vector.inc}


  { TVec3 }
  {$DEFINE TVECTOR:= TVec3}
  {$DEFINE NUMFIELD:=3}
  {$DEFINE VECTOR:= vec3}

  //function vec3( x: Float; y: Float; z: Float ); TVec3;
  {$DEFINE TYPEX:=Float}
  {$DEFINE TYPEY:=Float}
  {$DEFINE TYPEZ:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec3( xyz: Float ); TVec3;
  {$DEFINE TYPEX:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec3( xy: TVec2; z: Float ); TVec3;
  {$DEFINE TYPEX:=TVec2}
  {$DEFINE TYPEZ:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec3( x: Float; yz: TVec2 ); TVec3;
  {$DEFINE TYPEX:=Float}
  {$DEFINE TYPEY:=TVec2}
  {$INCLUDE math3d_vector.inc}

  //function vec3( xyz: TVec4 ); TVec3;
  {$DEFINE TYPEX:=TVec4}
  {$INCLUDE math3d_vector.inc}

  { TVec4 }
  {$DEFINE TVECTOR:= TVec4}
  {$DEFINE NUMFIELD:=4}
  {$DEFINE VECTOR:= vec4}

  //function vec4( x: Float; y: Float; z: Float; w: Float ); TVec4;
  {$DEFINE TYPEX:=Float}
  {$DEFINE TYPEY:=Float}
  {$DEFINE TYPEZ:=Float}
  {$DEFINE TYPEW:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec4( xyzw: Float ); TVec4;
  {$DEFINE TYPEX:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec4( xy: TVec2; z: Float; w: Float ); TVec4;
  {$DEFINE TYPEX:=TVec2}
  {$DEFINE TYPEZ:=Float}
  {$DEFINE TYPEW:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec4( x: Float; yz: TVec2; w: Float ); TVec4;
  {$DEFINE TYPEX:=Float}
  {$DEFINE TYPEY:=TVec2}
  {$DEFINE TYPEW:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec4( x: Float; y: Float; zw: TVec2 ); TVec4;
  {$DEFINE TYPEX:=Float}
  {$DEFINE TYPEY:=Float}
  {$DEFINE TYPEZ:=TVec2}
  {$INCLUDE math3d_vector.inc}

  //function vec4( xy: TVec2; zw: TVec2 ); TVec4;
  {$DEFINE TYPEX:=TVec2}
  {$DEFINE TYPEZ:=TVec2}
  {$INCLUDE math3d_vector.inc}

  //function vec4( xyz: TVec3; w: Float ); TVec4;
  {$DEFINE TYPEX:=TVec3}
  {$DEFINE TYPEW:=Float}
  {$INCLUDE math3d_vector.inc}

  //function vec4( x: Float; yzw: TVec3 ); TVec4;
  {$DEFINE TYPEX:=Float}
  {$DEFINE TYPEY:=TVec3}
  {$INCLUDE math3d_vector.inc}

  {END CONSTRUCTORS}

  {$UNDEF CONSTRUCTOR}
  {$UNDEF _IMPLEMENTATION}