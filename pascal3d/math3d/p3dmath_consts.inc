const
  //@abstract( Constant for converting degrees to radians by multiplying and radians to degress by dividing. ) @br
  //@bold( Example )@br@br
  //@longcode(
  //angle_rad:= 180 * deg2rad; //180 degrees or Pi
  //angle_deg:= Pi / deg2rad; //180 degrees or Pi
  //)
  deg2rad = 0.017453292;
  //Constant used internally to eliminate precision errors.
  epsilon = 1e-40;

  //This is a @link( TVec2 ) constant for the positive X-Axis when working with two dimension.
  vec2_Axis_PX: TVec2 = ( FCoord: ( 1.0, 0.0 ));
  //This is a @link( TVec2 ) constant for the positive Y-Axis when working with two dimension.
  vec2_Axis_PY: TVec2 = ( FCoord: ( 0.0, 1.0 ));
  //This is a @link( TVec2 ) constant for the negative X-Axis when working with two dimension.
  vec2_Axis_NX: TVec2 = ( FCoord: ( -1.0, 0.0 ));
  //This is a @link( TVec2 ) constant for the negative Y-Axis when working with two dimension.
  vec2_Axis_NY: TVec2 = ( FCoord: ( 0.0, -1.0 ));

  //This is a @link( TVec3 ) constant for the positive X-Axis when working with three dimension.
  vec3_Axis_PX: TVec3 = ( FCoord: ( 1.0, 0.0, 0.0 ));
  //This is a @link( TVec3 ) constant for the positive Y-Axis when working with three dimension.
  vec3_Axis_PY: TVec3 = ( FCoord: ( 0.0, 1.0, 0.0 ));
  //This is a @link( TVec3 ) constant for the positive Z-Axis when working with three dimension.
  vec3_Axis_PZ: TVec3 = ( FCoord: ( 0.0, 0.0, 1.0 ));
  //This is a @link( TVec3 ) constant for the negative X-Axis when working with three dimension.
  vec3_Axis_NX: TVec3 = ( FCoord: ( -1.0, 0.0, 0.0 ));
  //This is a @link( TVec3 ) constant for the negative Y-Axis when working with three dimension.
  vec3_Axis_NY: TVec3 = ( FCoord: ( 0.0, -1.0, 0.0 ));
  //This is a @link( TVec3 ) constant for the negative Z-Axis when working with three dimension.
  vec3_Axis_NZ: TVec3 = ( FCoord: ( 0.0, 0.0, -1.0 ));

  //This is a @link(TMat2) constant for an identity matrix. It is almost equivalent to @link(mat2)( 1 )
  //except for the fact that @link(mat2)() calls a function wheras @name is a constant and
  //probably faster.
  Mat2Identity: TMat2 = ( m: ( 1.0, 0.0,
                               0.0, 1.0 ));
  //This is a @link(TMat3) constant for an identity matrix. It is almost equivalent to @link(mat3)( 1 )
  //except for the fact that @link(mat3)() calls a function wheras @name is a constant and
  //probably faster.
  Mat3Identity: TMat3 = ( m: ( 1.0, 0.0, 0.0,
                               0.0, 1.0, 0.0,
                               0.0, 0.0, 1.0 ));
  //This is a @link(TMat4) constant for an identity matrix. It is almost equivalent to @link(mat4)( 1 )
  //except for the fact that @link(mat4)() calls a function wheras @name is a constant and
  //probably faster.
  Mat4Identity: TMat4 = ( m: ( 1.0, 0.0, 0.0, 0.0,
                               0.0, 1.0, 0.0, 0.0,
                               0.0, 0.0, 1.0, 0.0,
                               0.0, 0.0, 0.0, 1.0 ));
