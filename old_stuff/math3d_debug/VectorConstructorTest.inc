procedure ConstructorTestVec;
begin
  WriteLn( 'Constructor Test - Vector' );
  WriteLn( '--------------------------------------------------------------------' );
  WriteLn( 'function vec2( x: Float; y: Float ); TVec2;' );
  OutPutVec2( vec2( 1.0, 2.0 ));
  WriteLn( 'function vec2( xy: Float ); TVec2;' );
  OutPutVec2( vec2( 0.5 ));
  WriteLn( 'function vec2( xy: TVec3 ); TVec2;' );
  OutPutVec2( vec2( vec3( 1.0, 2.0, 3.0 )));
  WriteLn( 'function vec2( xy: TVec4 ); TVec2;' );
  OutPutVec2( vec2( vec4( 1.0, 2.0, 3.0, 4.0 )));
  WriteLn( 'function vec3( x: Float; y: Float; z: Float ); TVec3;' );
  OutPutVec3( vec3( 1.0, 2.0, 3.0 ));
  WriteLn( 'function vec3( xyz: Float ); TVec3;' );
  OutPutVec3( vec3( 0.5 ));
  WriteLn( 'function vec3( xy: TVec2; z: Float ); TVec3;' );
  OutPutVec3( vec3( vec2( 1.0, 2.0 ), 3.0 ));
  WriteLn( 'function vec3( x: Float; yz: TVec2 ); TVec3;' );
  OutPutVec3( vec3( 1.0, vec2( 2.0, 3.0 )));
  WriteLn( 'function vec3( xyz: TVec4 ); TVec3;' );
  OutPutVec3( vec3( vec4( 1.0, 2.0, 3.0, 4.0 )));
  WriteLn( 'function vec4( x: Float; y: Float; z: Float; w: Float ); TVec4;' );
  OutPutVec4( vec4( 1.0, 2.0, 3.0, 4.0 ));
  WriteLn( 'function vec4( xyzw: Float ); TVec4;' );
  OutPutVec4( vec4( 0.5 ));
  WriteLn( 'function vec4( xy: TVec2; z: Float; w: Float ); TVec4;' );
  OutPutVec4( vec4( vec2( 1.0, 2.0 ), 3.0, 4.0 ));
  WriteLn( 'function vec4( x: Float; yz: TVec2; w: Float ); TVec4;' );
  OutPutVec4( vec4( 1.0, vec2( 2.0, 3.0 ), 4.0 ));
  WriteLn( 'function vec4( x: Float; y: Float; zw: TVec2 ); TVec4;' );
  OutPutVec4( vec4( 1.0, 2.0, vec2( 3.0, 4.0 )));
  WriteLn( 'function vec4( xy: TVec2; zw: TVec2 ); TVec4;' );
  OutPutVec4( vec4( vec2( 1.0, 2.0 ), vec2( 3.0, 4.0 )));
end;
