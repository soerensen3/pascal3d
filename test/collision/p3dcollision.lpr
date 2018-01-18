program p3dcollision;

uses
  p3dMath,
  p3dgraphics;

procedure BoundingBoxTest;
var
  aB, bB: TP3DAABB;
  aP: TVec3;
  aSp: TP3DSphere;
  aMat: TMat4;
begin
  //Collision True
  WriteLn( 'Collision: A = AABB, B = AABB' );
  WriteLn( 'A = -4,-4,-4; 0, 0, 0; 4, 4, 4' );
  WriteLn( 'B = -4,-4,-4; 3, 3, 3; 4, 4, 4' );
  aB:= P3DAABBSzCtr( vec3( 8 ), vec3( 0 ));
  bB:= P3DAABBSzCtr( vec3( 8 ), vec3( 3 ));
  WriteLn( 'Collision: ', P3DCollideAABBwithAABB( aB, bB ));
  WriteLn();

  //Collision False
  WriteLn( 'Collision: A = AABB, B = AABB' );
  WriteLn( 'A = -4,-4,-4; 0, 0, 0; 4, 4, 4' );
  WriteLn( 'B = -4,-4,-4; 9, 9, 9; 4, 4, 4' );
  aB:= P3DAABBSzCtr( vec3( 8 ), vec3( 0 ));
  bB:= P3DAABBSzCtr( vec3( 8 ), vec3( 9 ));
  WriteLn( 'Collision: ', P3DCollideAABBwithAABB( aB, bB ));
  WriteLn();

  //Collision True
  WriteLn( 'Collision: A = P, B = AABB' );
  WriteLn( 'A = 7,7,7' );
  WriteLn( 'B = -4,-4,-4; 9, 9, 9; 4, 4, 4' );
  aP:= vec3( 7 );
  bB:= P3DAABBSzCtr( vec3( 8 ), vec3( 9 ));
  WriteLn( 'Collision: ', P3DCollidePOINTwithAABB( aP, bB ));
  WriteLn();

  //Collision False
  WriteLn( 'Collision: A = P, B = AABB' );
  WriteLn( 'A = -3,-3,-3' );
  WriteLn( 'B = -4,-4,-4; 9, 9, 9; 4, 4, 4' );
  aP:= vec3( -3 );
  bB:= P3DAABBSzCtr( vec3( 8 ), vec3( 9 ));
  WriteLn( 'Collision: ', P3DCollidePOINTwithAABB( aP, bB ));
  WriteLn();

  //Collision True
  WriteLn( 'Collision: A = SP, B = AABB' );
  WriteLn( 'A = 14,14,14 r:3' );
  WriteLn( 'B = -4,-4,-4; 9, 9, 9; 4, 4, 4' );
  aSp:= P3DSphere( vec3( 14 ), 3 );
  bB:= P3DAABBSzCtr( vec3( 8 ), vec3( 9 ));
  WriteLn( 'Collision: ', P3DCollideSPHEREwithAABB( aSp, bB ));
  WriteLn();

  //Collision False
  WriteLn( 'Collision: A = SP, B = AABB' );
  WriteLn( 'A = 17,17,17 r:3' );
  WriteLn( 'B = -4,-4,-4; 9, 9, 9; 4, 4, 4' );
  aSp:= P3DSphere( vec3( 17 ), 3 );
  bB:= P3DAABBSzCtr( vec3( 8 ), vec3( 9 ));
  WriteLn( 'Collision: ', P3DCollideSPHEREwithAABB( aSp, bB ));
  WriteLn();

  WriteLn( 'Transform: A = M B = SP' );
  WriteLn( 'A = MatScale( 2, 3, 1 )' );
  WriteLn( 'B = -4, -5, -6; 17,17,17; 4, 5, 6' );
  aMat:= mat4scale( vec4( 2, 3, 1, 1 ));
  bB:= P3DAABB( vec3( -4, -5, -6 ), vec3( 4, 5, 6 ), vec3( 17 ));
  bB:= P3DTransformAABBObjSpace( aMat, bB );
  WriteLn( 'AABB: ', bB.Min.ToString(), '; ', bB.Position.ToString(), '; ', bB.Max.ToString());
  WriteLn();
end;

procedure SphereTest;
var
  aSp, bSp: TP3DSphere;
  aP: TVec3;
  aMat: TMat4;
begin
  //Collision True
  WriteLn( 'Collision: A = SP, B = SP' );
  WriteLn( 'A = 17,17,17 r:3' );
  WriteLn( 'B = 14,14,14 r:5' );
  aSp:= P3DSphere( vec3( 17 ), 3 );
  bSp:= P3DSphere( vec3( 14 ), 5 );
  WriteLn( 'Collision: ', P3DCollideSPHEREwithSPHERE( aSp, bSp ));
  WriteLn();

  //Collision False
  WriteLn( 'Collision: A = SP, B = SP' );
  WriteLn( 'A = 17,17,17 r:3' );
  WriteLn( 'B = 14,14,14 r:5' );
  aSp:= P3DSphere( vec3( 5 ), 3 );
  bSp:= P3DSphere( vec3( 14 ), 5 );
  WriteLn( 'Collision: ', P3DCollideSPHEREwithSPHERE( aSp, bSp ));
  WriteLn();

  //Collision True
  WriteLn( 'Collision: A = P, B = SP' );
  WriteLn( 'A = 17,17,17' );
  WriteLn( 'B = 15,15,15 r:5' );
  aP:= vec3( 17 );
  bSp:= P3DSphere( vec3( 15 ), 5 );
  WriteLn( 'Collision: ', P3DCollidePOINTwithSPHERE( aP, bSp ));
  WriteLn();

  //Collision False
  WriteLn( 'Collision: A = SP, B = SP' );
  WriteLn( 'A = 17,17,17 r:3' );
  WriteLn( 'B = 14,14,14 r:5' );
  aSp:= P3DSphere( vec3( 5 ), 3 );
  bSp:= P3DSphere( vec3( 14 ), 5 );
  WriteLn( 'Collision: ', P3DCollideSPHEREwithSPHERE( aSp, bSp ));
  WriteLn();

  WriteLn( 'Transform: A = M B = SP' );
  WriteLn( 'MatScale( 2, 3, 1 )' );
  WriteLn( 'A = 17,17,17 r:3' );
  aMat:= mat4scale( vec4( 2, 3, 1, 1 ));
  bSp:= P3DSphere( vec3( 17 ), 3 );
  bSp:= P3DTransformSphereObjSpace( aMat, bSp );
  WriteLn( 'Sphere: ', bSp.Position.ToString(), ' r = ', bSp.Radius );
  WriteLn();
end;

begin
  BoundingBoxTest;
  SphereTest;
end.

