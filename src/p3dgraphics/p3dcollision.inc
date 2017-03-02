{$IFDEF INTERFACE}
type
  {shapes}
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

  TP3DSphere = packed record
    case Boolean of
      True: ( Position: TVec3; Radius: Float );
      False: ( xyzr: TVec4 );
  end;

  TP3DRay = packed record
    Orig: TVec3;
    Dir: TVec3;
  end;

  TP3DLine = packed record
    A: TVec3;
    B: TVec3;
  end;

  operator = ( A, B: TP3DAABB ): Boolean;
  operator + ( A: TP3DAABB; B: TVec3 ): TP3DAABB;
  operator - ( A: TP3DAABB; B: TVec3 ): TP3DAABB;


const
  P3DInvalidAABB: TP3DAABB
      = ( Min: ( FCoord: ( -MaxSingle, -MaxSingle, -MaxSingle ));
          Max: ( FCoord: ( MaxSingle, MaxSingle, MaxSingle ));
          Position: ( FCoord: ( 0, 0, 0 )));

  {constructors}
  function P3DAABB( vMin, vMax, vPosition: TVec3 ): TP3DAABB;
  function P3DAABBSzCtr( vSize, vPosition: TVec3 ): TP3DAABB;
  function P3DSphere( vPosition: TVec3; Radius: Float ): TP3DSphere;
  function P3DRay( Orig: TVec3; Dir: TVec3 ): TP3DRay;
  function P3DRay( Line: TP3DLine ): TP3DRay;
  function P3DPlane( vNormal: TVec3; d: Float ): TP3DPlane;
  function P3DPlane( vNormal: TVec3; p: TVec3 ): TP3DPlane;
  function P3DLine( A: TVec3; B: TVec3 ): TP3DLine;

  {collision}
  function P3DCollideAABBwithAABB( a: TP3DAABB; b: TP3DAABB ): Boolean;
  function P3DCollidePOINTwithAABB( a: TVec3; b: TP3DAABB ): Boolean;
  function P3DCollideSPHEREwithAABB( a: TP3DSphere; b: TP3DAABB ): Boolean;
  function P3DCollideSPHEREwithSPHERE( a: TP3DSphere; b: TP3DSphere ): Boolean;
  function P3DCollidePOINTwithSPHERE( a: TVec3; b: TP3DSphere ): Boolean;

  {collisions with planes returned}
  {function P3DCollideAABBwithAABB( a: TP3DAABB; b: TP3DAABB; out APlane: TP3DPlane; out BPlane: TP3DPlane ): Boolean;
  function P3DCollideSPHEREwithAABB( a: TP3DSphere; b: TP3DAABB; out APlane: TP3DPlane; out BPlane: TP3DPlane ): Boolean;
  function P3DCollideSPHEREwithSPHERE( a: TP3DSphere; b: TP3DSphere; out APlane: TP3DPlane; out BPlane: TP3DPlane ): Boolean;}

  {collisions with intersection returned}
  function P3DCollideRAYwithPLANE( a: TP3DRay; b: TP3DPlane; out vIntersection: TVec3 ): Boolean;

  {transforms}
  function P3DTransformAABB( Matrix: TMat4; a: TP3DAABB ): TP3DAABB;
  function P3DTransformAABBObjSpace( Matrix: TMat4; a: TP3DAABB ): TP3DAABB;
  function P3DTransformSphere( Matrix: TMat4; a: TP3DSphere ): TP3DSphere;
  function P3DTransformSphereObjSpace( Matrix: TMat4; a: TP3DSphere ): TP3DSphere;

  {closest point}
  function P3DFindClosestPointAABB( a: TP3DAABB; b: TVec3 ): TVec3; inline;
  function P3DFindClosestPointSPHERE( a: TP3DSphere; b: TVec3 ): TVec3; inline;
  function P3DFindClosestPointPLANE( a: TP3DPlane; b: TVec3 ): TVec3; inline;
  function P3DFindClosestPointLINE( a: TP3DLine; b: TVec3 ): TVec3; inline;
  function P3DFindClosestPointRAY( a: TP3DRay; b: TVec3 ): TVec3; inline;

  {normal}
  function P3DFindNormalToPointAABB( a: TP3DAABB; b: TVec3 ): TVec3; inline;
  function P3DFindNormalToPointSPHERE( a: TP3DSphere; b: TVec3 ): TVec3; inline;

  {plane}
  function P3DFindPlaneToPointAABB( a: TP3DAABB; b: TVec3 ): TP3DPlane;
  function P3DFindPlaneToPointSPHERE( a: TP3DSphere; b: TVec3 ): TP3DPlane;

  {utilities}
  function P3DSlideLINEalongPLANE( Line: TP3DLine; Plane: TP3DPlane; out vFinal: TVec3; out vIntersection: TVec3 ): Boolean;


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

function P3DRay(Orig: TVec3; Dir: TVec3): TP3DRay;
begin
  Result.Orig:= Orig;
  Result.Dir:= Dir;
end;

function P3DRay(Line: TP3DLine): TP3DRay;
begin
  Result:= P3DRay( Line.A, normalize( Line.B - Line.A ));
end;

function P3DPlane(vNormal: TVec3; d: Float): TP3DPlane;
begin
  Result.vNormal:= vNormal;
  Result.d:= d;
end;

function P3DPlane(vNormal: TVec3; p: TVec3): TP3DPlane;
begin
  Result.vNormal:= vNormal;
  Result.d:= dot( p, vNormal );
end;

function P3DLine(A: TVec3; B: TVec3): TP3DLine;
begin
  Result.A:= A;
  Result.B:= B;
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

function P3DCollideRAYwithPLANE(a: TP3DRay; b: TP3DPlane; out vIntersection: TVec3): Boolean;
var
  denom, t: p3dMath.Float;
  vPlanar: TVec3;
begin
  denom:= dot( b.vNormal, a.Dir );

  Result:= False;
  if ( abs( denom ) > -epsilon ) then
    begin
      vPlanar:= b.vNormal * b.d;
      t:= dot( vPlanar - a.Orig, b.vNormal ) / denom;
      if ( t >= -epsilon ) then
        begin
          vIntersection:= a.Orig + t * a.Dir;
          Result:= True;
        end;
    end;
end;

function P3DFindClosestPointAABB(a: TP3DAABB; b: TVec3): TVec3;
var
  aMax, aMin: TVec3;
begin
  aMax:= a.Max + a.Position;
  aMin:= a.Min + a.Position;
  Result:= clamp( b, aMin, aMax );
end;

function P3DFindClosestPointSPHERE(a: TP3DSphere; b: TVec3): TVec3;
begin
  Result:= normalize( b - a.Position ) * a.Radius + a.Position;
end;

function P3DFindClosestPointPLANE(a: TP3DPlane; b: TVec3): TVec3;
var
  d: Single;
begin
  d:= dot( a.vNormal, b ) - a.d ;
  Result:= b - a.vNormal * d;
end;

function P3DFindClosestPointLINE(a: TP3DLine; b: TVec3): TVec3;
var
  vA: TVec3;
  t: Single;
begin
  vA:= a.B - a.A;
  t:= clamp( dot( b - a.A, vA ) / max( epsilon, Dot( vA, vA )), 0, 1 );
  Result:= a.A + t * vA;
end;

function P3DFindClosestPointRAY(a: TP3DRay; b: TVec3): TVec3;
var
  t: Single;
begin
  t:= max( 0, dot( b - a.Orig, a.Dir ) / max( epsilon, Dot( a.Dir, a.Dir )));
  Result:= a.Orig + t * a.Dir;
end;

function P3DFindNormalToPointAABB(a: TP3DAABB; b: TVec3): TVec3;
var
  p: TVec3;
  n: Integer;
begin
  p:= ( b - P3DFindClosestPointAABB( a, b ));
  n:= p.GetBiggestComp;

  Result:= vec3( 0 );
  if ( p[ n ] > 0 ) then
    Result[ n ]:= 1
  else
    Result[ n ]:= -1;
end;

function P3DFindNormalToPointSPHERE(a: TP3DSphere; b: TVec3): TVec3;
begin
  Result:= normalize( b - a.Position );
end;

function P3DFindPlaneToPointAABB(a: TP3DAABB; b: TVec3): TP3DPlane;
begin
  Result:= P3DPlane( P3DFindNormalToPointAABB( a, b ), b );
end;

function P3DFindPlaneToPointSPHERE(a: TP3DSphere; b: TVec3): TP3DPlane;
begin
  Result:= P3DPlane( P3DFindNormalToPointSPHERE( a, b ), b );
end;

function P3DSlideLINEalongPLANE(Line: TP3DLine; Plane: TP3DPlane; out vFinal: TVec3; out vIntersection: TVec3): Boolean;
var
  Ray: TP3DRay;
  l, d: p3dMath.Float;
begin
  Ray:= P3DRay( Line );

  Result:= False;

  if ( P3DCollideRAYwithPLANE( Ray, Plane, vIntersection )) then
    begin
      l:= len( Line.B - Line.A );
      d:= len( Ray.Orig - vIntersection );
      Result:= d < l;
      if ( Result ) then
        vFinal:= vIntersection + ( Ray.Dir - project( Ray.Dir, Plane.vNormal )) * ( l - d );
    end;
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

{
function P3DCollideAABBwithAABB(a: TP3DAABB; b: TP3DAABB; out APlane: TP3DPlane; out BPlane: TP3DPlane): Boolean;
begin
  Result:= P3DCollideAABBwithAABB( a, b );
end;

function P3DCollideSPHEREwithAABB(a: TP3DSphere; b: TP3DAABB; out APlane: TP3DPlane; out BPlane: TP3DPlane): Boolean;
var
  p: TVec3;
  l: p3dMath.Float;
  n: Integer;
begin
  // find closest point to the sphere center in sphere space
  p:= P3DFindClosestPointAABB( b, a.Position ) - a.Position;
  l:= len( p );
  Result:= l <= a.Radius;
  if ( Result ) then
    APlane.vNormal:= p / max( epsilon, l )
  else
    APlane.vNormal:= vec3( 0 );//vec3_Axis_PZ;

  p+= a.Position;
  APlane.d:= -( p.x * APlane.vNormal.x +
                p.y * APlane.vNormal.y +
                p.z * APlane.vNormal.z );

  n:= APlane.vNormal.GetBiggestComp;
  BPlane.vNormal:= vec3( 0 );
  if ( APlane.vNormal[ n ] > 0 ) then
    BPlane.vNormal[ n ]:= -1
  else
    BPlane.vNormal[ n ]:= 1;

  BPlane.d:=  ( p.x * BPlane.vNormal.x +
                p.y * BPlane.vNormal.y +
                p.z * BPlane.vNormal.z );
end;

function P3DCollideSPHEREwithSPHERE(a: TP3DSphere; b: TP3DSphere; out APlane: TP3DPlane; out BPlane: TP3DPlane): Boolean;
begin
  Result:= P3DCollideSPHEREwithSPHERE( a, b );
  if ( not Result ) then
    exit;
  APlane.vNormal:= normalize( b.Position - a.Position );
  APlane.d:= -( dot( a.Position, APlane.vNormal ){ + a.Radius });
  BPlane.vNormal:= -APlane.vNormal;
  BPlane.d:= -( dot( b.Position, BPlane.vNormal ));
end;}

{$ENDIF}
