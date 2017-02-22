{$IFDEF INTERFACE}
type
  TP3DPlane = packed record
    case Boolean of
      True: ( vNormal: TVec3; d: Single );
      False: ( abcd: TVec4 );
  end;

  TP3DAABB = record
    Min,
    Max,
    Position: TVec3;
  end;

  TP3DSphere = record
    Position: TVec3;
    Radius: Float;
  end;

  operator = ( A, B: TP3DAABB ): Boolean;
  operator + ( A: TP3DAABB; B: TVec3 ): TP3DAABB;
  operator - ( A: TP3DAABB; B: TVec3 ): TP3DAABB;


const
  P3DInvalidAABB: TP3DAABB
      = ( Min: ( FCoord: ( -MaxSingle, -MaxSingle, -MaxSingle ));
          Max: ( FCoord: ( MaxSingle, MaxSingle, MaxSingle ));
          Position: ( FCoord: ( 0, 0, 0 )));

  function P3DCollideAABBwithAABB( a: TP3DAABB; b: TP3DAABB ): Boolean;
  function P3DCollidePOINTwithAABB( a: TVec3; b: TP3DAABB ): Boolean;
  function P3DCollideSPHEREwithAABB( a: TP3DSphere; b: TP3DAABB ): Boolean;
  function P3DCollideSPHEREwithSPHERE( a: TP3DSphere; b: TP3DSphere ): Boolean;
  function P3DCollidePOINTwithSPHERE( a: TVec3; b: TP3DSphere ): Boolean;
  function P3DCollideAABBwithAABB( a: TP3DAABB; b: TP3DAABB; out APlane ): Boolean;
  function P3DCollideSPHEREwithAABB( a: TP3DSphere; b: TP3DAABB; out APlane ): Boolean;
  function P3DCollideSPHEREwithSPHERE( a: TP3DSphere; b: TP3DSphere; out APlane ): Boolean;

  function P3DAABB( vMin, vMax, vPosition: TVec3 ): TP3DAABB;
  function P3DAABBSzCtr( vSize, vPosition: TVec3 ): TP3DAABB;
  function P3DSphere( vPosition: TVec3; Radius: Float ): TP3DSphere;
  function P3DTransformAABB( Matrix: TMat4; a: TP3DAABB ): TP3DAABB;
  function P3DTransformAABBObjSpace( Matrix: TMat4; a: TP3DAABB ): TP3DAABB;
  function P3DTransformSphere( Matrix: TMat4; a: TP3DSphere ): TP3DSphere;
  function P3DTransformSphereObjSpace( Matrix: TMat4; a: TP3DSphere ): TP3DSphere;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

operator=(A, B: TP3DAABB): Boolean;
begin
  Result:= ( A.Min = B.Min ) and ( A.Max = B.Max ) and ( A.Position = B.Position );
end;

operator+(A: TP3DAABB; B: TVec3): TP3DAABB;
begin
  Result:= P3DAABB( A.Min, A.Max, A.Position + B );
end;

operator-(A: TP3DAABB; B: TVec3): TP3DAABB;
begin
  Result:= P3DAABB( A.Min, A.Max, A.Position - B );
end;

function P3DAABB(vMin, vMax, vPosition: TVec3): TP3DAABB;
begin
  Result.Position:= vPosition;
  Result.Min:= vMin;
  Result.Max:= vMax;
end;

function P3DAABBSzCtr(vSize, vPosition: TVec3): TP3DAABB;
begin
  Result.Position:= vPosition;
  Result.Min:= -vSize / 2;
  Result.Max:=  vSize / 2;
end;

function P3DSphere( vPosition: TVec3; Radius: Float): TP3DSphere;
begin
  Result.Position:= vPosition;
  Result.Radius:= Radius;
end;

function P3DTransformAABB( Matrix: TMat4; a: TP3DAABB ): TP3DAABB;
var
  xa, xb, ya, yb, za, zb: TVec3;
  //http://dev.theomader.com/transform-bounding-boxes/
begin
  xa:= Matrix.Row[ 0 ].xyz * a.Min.X;
  xb:= Matrix.Row[ 0 ].xyz * a.Max.X;

  ya:= Matrix.Row[ 1 ].xyz * a.Min.Y;
  yb:= Matrix.Row[ 1 ].xyz * a.Max.Y;

  za:= Matrix.Row[ 2 ].xyz * a.Min.Z;
  zb:= Matrix.Row[ 2 ].xyz * a.Max.Z;

  Result.Min:= min( xa, xb ) + min( ya, yb ) + min( za, zb );
  Result.Max:= max( xa, xb ) + max( ya, yb ) + max( za, zb );
  Result.Position:= ( Matrix * vec4( a.Position, 1 )).xyz;
end;

function P3DTransformAABBObjSpace(Matrix: TMat4; a: TP3DAABB): TP3DAABB;
var
  xa, xb, ya, yb, za, zb: TVec3;
  //http://dev.theomader.com/transform-bounding-boxes/
begin
  xa:= Matrix.Row[ 0 ].xyz * a.Min.X;
  xb:= Matrix.Row[ 0 ].xyz * a.Max.X;

  ya:= Matrix.Row[ 1 ].xyz * a.Min.Y;
  yb:= Matrix.Row[ 1 ].xyz * a.Max.Y;

  za:= Matrix.Row[ 2 ].xyz * a.Min.Z;
  zb:= Matrix.Row[ 2 ].xyz * a.Max.Z;

  Result.Min:= min( xa, xb ) + min( ya, yb ) + min( za, zb );
  Result.Max:= max( xa, xb ) + max( ya, yb ) + max( za, zb );
  Result.Position:= a.Position + Matrix.Row[ 3 ].xyz;
end;

function P3DTransformSphere(Matrix: TMat4; a: TP3DSphere): TP3DSphere;
begin
  Result.Position:= ( Matrix * vec4( a.Position, 1 )).xyz;
  Result.Radius:= Max( a.Radius * Matrix._00, Max( a.Radius * Matrix._11, a.Radius * Matrix._22 )); //The result is still a sphere so take max radius * scale
end;

function P3DTransformSphereObjSpace(Matrix: TMat4; a: TP3DSphere): TP3DSphere;
begin
  Result.Position:= a.Position + Matrix.Row[ 3 ].xyz;
  Result.Radius:= Max( a.Radius * Matrix._00, Max( a.Radius * Matrix._11, a.Radius * Matrix._22 )); //The result is still a sphere so take max radius * scale
end;


function P3DCollideAABBwithAABB(a: TP3DAABB; b: TP3DAABB): Boolean;
var
  aMax, aMin, bMax, bMin: TVec3;

begin
  aMax:= a.Max + a.Position;
  aMin:= a.Min + a.Position;
  bMax:= b.Max + b.Position;
  bMin:= b.Min + b.Position;

  Result:= (( aMin.X <= bMax.X ) and ( aMax.X >= bMin.X )) and
           (( aMin.Y <= bMax.Y ) and ( aMax.Y >= bMin.Y )) and
           (( aMin.Z <= bMax.Z ) and ( aMax.Z >= bMin.Z ));
end;

function P3DCollidePOINTwithAABB(a: TVec3; b: TP3DAABB): Boolean;
var
  bMax, bMin: TVec3;
begin
  bMax:= b.Max + b.Position;
  bMin:= b.Min + b.Position;

  Result:= InRange( a.X, bMin.X, bMax.X ) and
           InRange( a.Y, bMin.Y, bMax.Y ) and
           InRange( a.Z, bMin.Z, bMax.Z );
end;

function P3DCollideSPHEREwithAABB(a: TP3DSphere; b: TP3DAABB): Boolean;
var
  p, bMax, bMin: TVec3;
begin
  bMax:= b.Max + b.Position;
  bMin:= b.Min + b.Position;
  // find closest point to the sphere center
  p:= clamp( a.Position, bMin, bMax ) - a.Position;
  Result:= len( p ) < a.Radius;
end;

function P3DCollideSPHEREwithSPHERE(a: TP3DSphere; b: TP3DSphere): Boolean;
begin
  Result:= len( a.Position - b.Position ) < a.Radius + b.Radius;
end;

function P3DCollidePOINTwithSPHERE(a: TVec3; b: TP3DSphere): Boolean;
begin
  Result:= ( a - b.Position ).GetDistSqr < sqr( b.Radius );
end;

function P3DCollideAABBwithAABB(a: TP3DAABB; b: TP3DAABB; out APlane: TP3DPlane ): Boolean;
begin

end;

function P3DCollideSPHEREwithAABB(a: TP3DSphere; b: TP3DAABB; out APlane: TP3DPlane ): Boolean;
var
  p, bMax, bMin: TVec3;
  l: p3dMath.Float;
begin
  bMax:= b.Max + b.Position;
  bMin:= b.Min + b.Position;
  // find closest point to the sphere center
  p:= clamp( a.Position, bMin, bMax ) - a.Position;
  l:= len( p );
  Result:= l < a.Radius;
  APlane.vNormal:= p / l;
  APlane.d:= -( p.x * APlane.vNormal.x +
                p.y * APlane.vNormal.y +
                p.z * APlane.vNormal.z );
end;

function P3DCollideSPHEREwithSPHERE(a: TP3DSphere; b: TP3DSphere; out APlane: TP3DPlane ): Boolean;
begin

end;

{$ENDIF}
