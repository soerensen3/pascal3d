{$IF NUMFIELD>2}
// This will return the cross product of two vectors. This function is only defined for TVec3 and above.
function cross( x: TVector; y: TVector ): TVector; overload;
  {$IFDEF IMPLEMENTATION}
begin
  Result:= x.CrossProduct( y );
end;
  {$ENDIF}
{$ENDIF}

// This will return the dot product of two vectors.
function dot( x: TVector; y: TVector ): Float; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result:= x.DotProduct( y );
end;
{$ENDIF}

// This return a normalized vector leaving the original vector untouched.
function normalize( x: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result:= x;
  Result.Normalize;
end;
{$ENDIF}

//computes the direction of a refracted ray if i specifies the normalized(!)
//direction of the incoming ray and n specifies the normalized(!) normal vector
//of the interface of two optical media (e.g. air and water). The vector n
//should point to the side from where i is coming, i.e. the dot product of n and
//i should be negative. The floating-point number r is the ratio of the
//refractive index of the medium from where the ray comes to the refractive index
//of the medium on the other side of the surface. Thus, if a ray comes from air
//(refractive index about 1.0) and hits the surface of water (refractive index 1.33),
//then the ratio r is 1.0 / 1.33 = 0.75.
function refract( i, n: TVector; r: Float ): TVector; overload;
{$IFDEF IMPLEMENTATION}
var
  d: Float;
begin
  d:= 1.0 - r * r * (1.0 - dot(n, i) * dot(n, i));
  if ( d < 0.0 ) then
    {$IF NUMFIELD = 2}
    Result:= vec2( 0 ) // total internal reflection
    {$ELSEIF NUMFIELD = 3}
    Result:= vec3( 0 ) // total internal reflection
    {$ELSEIF NUMFIELD = 4}
    Result:= vec4( 0 ) // total internal reflection
    {$ENDIF}
  else
    Result:= r * i - ( r * dot( n, i ) + sqrt( d )) * n;
end;
{$ENDIF}

//For a given incident vector I and surface normal N reflect returns the reflection direction calculated as I - 2.0 * dot(N, I) * N.
//N should be normalized in order to achieve the desired result.
function reflect( i, n: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
var
  d: Float;
begin
  Result:= I - 2.0 * dot(N, I) * N;
end;
{$ENDIF}


{$IF NUMFIELD = 4}
// This will unproject a vector using world, view and projection matrix. It will return the unprojected vector and the direction it is pointing to.
function unproject( x: TVector; matWorld, matView, matProj: TMatrix; Width, Height: Integer; out vDirection: TVector ): TVector;
  {$IFDEF IMPLEMENTATION}
var
  m: TMatrix;
begin
  // Compute the vector of the pick ray in screen space
  Result.x:=  ((( 2.0 * x.x ) / Width ) - 1 ) / matProj._00;
  Result.y:= -((( 2.0 * x.y ) / Height ) - 1 ) / matProj._11;
  Result.z:= -1.0; //TODO: Check if left or righthanded

  // Get the inverse view matrix

  mat4inverse( matWorld * matView, m );

  // Transform the screen space pick ray into 3D space
  vDirection.x:= Result.x * m._00 + Result.y * m._10 + Result.z * m._20;
  vDirection.y:= Result.x * m._01 + Result.y * m._11 + Result.z * m._21;
  vDirection.z:= Result.x * m._02 + Result.y * m._12 + Result.z * m._22;
  vDirection.Normalize;
  Result.x:= m._30;
  Result.y:= m._31;
  Result.z:= m._32;
end;
  {$ENDIF}
{$ENDIF}


// This will return the bigger components of two vectors. It works component-wise.
function max( x: TVector; y: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= max( x.x, y.x );
  {$IF NUMFIELD > 1}
  Result.y:= max( x.y, y.y );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= max( x.z, y.z );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= max( x.w, y.w );
  {$ENDIF}
end;
{$ENDIF}

// This will return the smaller components of two vectors. It works component-wise.
function min( x: TVector; y: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= min( x.x, y.x );
  {$IF NUMFIELD > 1}
  Result.y:= min( x.y, y.y );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= min( x.z, y.z );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= min( x.w, y.w );
  {$ENDIF}
end;
{$ENDIF}

// This will return the bigger components of one vector and a scalar. It works component-wise.
function max( x: TVector; s: Single ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= max( x.x, s );
  {$IF NUMFIELD > 1}
  Result.y:= max( x.y, s );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= max( x.z, s );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= max( x.w, s );
  {$ENDIF}
end;
{$ENDIF}


// This will return the smaller components of one vector and a scalar. It works component-wise.
function min( x: TVector; s: Single ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= min( x.x, s );
  {$IF NUMFIELD > 1}
  Result.y:= min( x.y, s );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= min( x.z, s );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= min( x.w, s );
  {$ENDIF}
end;
{$ENDIF}

//clamp returns the value of x constrained to the range minVal to maxVal. The returned value is computed as min(max(x, minVal), maxVal).
function clamp( x, minVal, maxVal: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result:= min( max( x, minVal ), maxVal );
end;
{$ENDIF}

//Returns x if x >= 0; otherwise, it returns -x.
function abs( x: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= abs( x.x );
  {$IF NUMFIELD > 1}
  Result.y:= abs( x.y );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= abs( x.z );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= abs( x.w );
  {$ENDIF}
end;
{$ENDIF}

//clamp returns the value of x constrained to the range minVal to maxVal. The returned value is computed as min(max(x, minVal), maxVal).
function clamp( x: TVector; minVal, maxVal: Float ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= min( max( x.x, minVal ), maxVal );
  {$IF NUMFIELD > 1}
  Result.y:= min( max( x.y, minVal ), maxVal );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= min( max( x.z, minVal ), maxVal );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= min( max( x.w, minVal ), maxVal );
  {$ENDIF}
end;
{$ENDIF}

//floor returns a value equal to the nearest integer that is less than or equal to x.
function floor( x: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= floor( x.x );
  {$IF NUMFIELD > 1}
  Result.y:= floor( x.y );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= floor( x.z );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= floor( x.w );
  {$ENDIF}
end;
{$ENDIF}

//ceil returns a value equal to the nearest integer that is greater than or equal to x.
function ceil( x: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= ceil( x.x );
  {$IF NUMFIELD > 1}
  Result.y:= ceil( x.y );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= ceil( x.z );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= ceil( x.w );
  {$ENDIF}
end;
{$ENDIF}

//round returns a value equal to the nearest integer to x.
function round( x: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= round( x.x );
  {$IF NUMFIELD > 1}
  Result.y:= round( x.y );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= round( x.z );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= round( x.w );
  {$ENDIF}
end;
{$ENDIF}

//distance returns the distance between the two points p0 and p1.
function distance( p0, p1: TVector ): Float; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result:= ( p1 - p0 ).GetDist;
end;
{$ENDIF}

//len returns the length of the vector. It's not called length because it would
//override the function Length from system, which is invisible then.
function len( p: TVector ): Float; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result:= p.GetDist;
end;
{$ENDIF}

//fract returns the fractional part of x.
function fract( x: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
begin
  Result.x:= frac( x.x );
  {$IF NUMFIELD > 1}
  Result.y:= frac( x.y );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= frac( x.z );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= frac( x.w );
  {$ENDIF}
end;
{$ENDIF}

function smoothstep( edge0, edge1, x: TVector ): TVector; overload;
{$IFDEF IMPLEMENTATION}
var
  t: TVector;
begin
   t:= clamp(( x - edge0 ) / ( edge1 - edge0 ), 0, 1 );
   Result:= t * t * ( - 2 * t + 3 );
end;
{$ENDIF}

