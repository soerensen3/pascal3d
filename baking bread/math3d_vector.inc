//math3d_vector.inc

{ TVector }

procedure TVector.SetCoord(AIndex: Integer; AValue: Float);
begin
  case AIndex of
    0: FX:= AValue;
    1: FY:= AValue;
    2: FZ:= AValue;
  end;
  //TODO: ONCHANGE EVENT
end;

function TVector.GetDist: Float;
begin
  Result:= sqrt( sqr( x ) + sqr( y ) + sqr( z ));
end;

function TVector.Normalize: TVector;
var
  len: Float;
begin
  len:= GetDist;
  if ( len > 0 ) then
    Self /= len;
  Result:= Self;
end;

function TVector.CrossProduct(v: TVector): TVector;
begin
  Result.x:= y * v.z - z * v.y;
  Result.y:= z * v.x - x * v.z;
  Result.z:= x * v.y - y * v.x;
end;

function TVector.DotProduct(v: TVector): Float;
begin
  Result:= x * v.x +
           y * v.y +
           z * v.z;
end;

function TVector.GetBiggestComp: Integer;
begin
  Result:= 0;
  if (( Abs( y ) > Abs( x )) and ( Abs( y ) > Abs( z ))) then
    Result:= 1
  else if (( Abs( z ) > Abs( x )) and ( Abs( z ) > Abs( y ))) then
    Result:= 2;
end;

function TVector.RotateRad(Axis: TVector; Angle: Float): TVector;
begin
  Result:= VecTransform( Self, MatrixRot( Axis, Angle ));
end;

function TVector.RotateDeg(Axis: TVector; Angle: Float): TVector;
begin
  Result:= VecTransform( Self, MatrixRot( Axis, deg2rad * Angle ));
end;

function TVector.Invert: TVector;
begin
  x:= -x;
  y:= -y;
  z:= -z;
  Result:= Self;
end;

function TVector.Ptr: PFloat;
begin
  Result:= @FX;
end;

function TVector.Compare(v: TVector): Boolean;
begin
  Result:= ( v.x = x ) and ( v.y = y ) and ( v.z = z );
end;


operator+(a, b: TVector)c: TVector;
begin
  c.x:= a.x + b.x;
  c.y:= a.y + b.y;
  c.z:= a.z + b.z;
end;

operator-(a, b: TVector)c: TVector;
begin
  c.x:= a.x - b.x;
  c.y:= a.y - b.y;
  c.z:= a.z - b.z;
end;

operator*(a, b: TVector)c: Float;
begin
  Result:= a.DotProduct( b );
{  c.x:= a.x * b.x;
  c.y:= a.y * b.y;
  c.z:= a.z * b.z;}
end;

operator*(a: TVector; s: Float)c: TVector;
begin
  c.x:= a.x * s;
  c.y:= a.y * s;
  c.z:= a.z * s;
end;

operator*(s: Float; b: TVector)c: TVector;
begin
  c.x:= b.x * s;
  c.y:= b.y * s;
  c.z:= b.z * s;
end;

operator/(a, b: TVector)c: TVector;
begin
  c.x:= a.x / b.x;
  c.y:= a.y / b.y;
  c.z:= a.z / b.z;
end;

operator/(a: TVector; s: Float)c: TVector;
begin
  c.x:= a.x / s;
  c.y:= a.y / s;
  c.z:= a.z / s;
end;

operator * ( a: TVector; b: TMatrix4f ) c: TVector; inline;
begin
  c:= VecTransform( a, b );
end;

operator*(a: TMatrix4f; b: TVector)c: TVector; inline;
begin
  c:= VecTransform( b, a );
end;

operator*(a: TMatrix4f; b: Float)c: TMatrix4f;
var
  i: Integer;
begin
  for i:= 0 to 15 do
    a[ i ]:= a[ i ] * b;
  c:= a;
end;

operator-(a: TVector)c: TVector;
begin
  c:= a * -1;
end;

function VecCrossProduct(v1: TVector; v2: TVector): TVector;
begin
  Result:= v1.CrossProduct( v2 );
end;

function VecDotProduct(v1: TVector; v2: TVector): Float;
begin
  Result:= v1.DotProduct( v2 );
end;

function VecNormalize(Vec: TVector): TVector;
begin
  Result:= Vec;
  Result.Normalize;
end;

function VecInvert(Vec: TVector): TVector;
begin
  Result:= Vec * -1;
end;

function VecTransform( Vec: TVector; Matrix: TMatrix4f ): TVector;
begin
  Result.x:= ( Vec.x * Matrix._00 ) + ( Vec.y * Matrix._10 ) + ( Vec.z * Matrix._20 ) + Matrix._30;
  Result.y:= ( Vec.x * Matrix._01 ) + ( Vec.y * Matrix._11 ) + ( Vec.z * Matrix._21 ) + Matrix._31;
  Result.z:= ( Vec.x * Matrix._02 ) + ( Vec.y * Matrix._12 ) + ( Vec.z * Matrix._22 ) + Matrix._32;
end;

function VecUnproject( Vec: TVector; matWorld, matView, matProj: TMatrix4f; Width, Height: Integer; out vDirection: TVector ): TVector;
var
//  v: TVector;
  m: TMatrix4f;
begin
  // Compute the vector of the pick ray in screen space
  Result.x:=  ((( 2.0 * Vec.x ) / Width ) - 1 ) / matProj._00;
  Result.y:= -((( 2.0 * Vec.y ) / Height ) - 1 ) / matProj._11;
  Result.z:= -1.0; //TODO: Check if left or righthanded

  // Get the inverse view matrix

//  m:= MatrixMul( matWorld, matView );
//  m:= MatrixMul( m, matView );
  MatrixInverse( matWorld * matView, m );

  // Transform the screen space pick ray into 3D space
  vDirection.x:= Result.x * m._00 + Result.y * m._10 + Result.z * m._20;
  vDirection.y:= Result.x * m._01 + Result.y * m._11 + Result.z * m._21;
  vDirection.z:= Result.x * m._02 + Result.y * m._12 + Result.z * m._22;
  vDirection.Normalize;
  Result.x:= m._30;
  Result.y:= m._31;
  Result.z:= m._32;
end;

function Max(Vec1: TVector; Vec2: TVector): TVector;
begin
  Result:= Vector( Max( Vec1.x, Vec2.x ), Max( Vec1.y, Vec2.y ), Max( Vec1.z, Vec2.z ));
end;

function Min(Vec1: TVector; Vec2: TVector): TVector;
begin
  Result:= Vector( Min( Vec1.x, Vec2.x ), Min( Vec1.y, Vec2.y ), Min( Vec1.z, Vec2.z ));
end;

function Max(Vec1: TVector; s: Single): TVector;
begin
  Result:= Vector( Max( Vec1.x, s ), Max( Vec1.y, s ), Max( Vec1.z, s ));
end;

function Min(Vec1: TVector; s: Single): TVector;
begin
  Result:= Vector( Min( Vec1.x, s ), Min( Vec1.y, s ), Min( Vec1.z, s ));
end;

