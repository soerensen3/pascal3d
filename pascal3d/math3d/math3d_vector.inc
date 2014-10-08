//math3d_vector.inc

{$IFDEF INTERFACE}
{ @classname serves as a generic class for all vector types. There are 3 vector types
  regarding which differ in the amount of the fields.}
TVector = packed object
  private
    {@exclude} procedure SetCoord( AIndex: Integer; AValue: Float );
    {@exclude} function GetCoord( AIndex: Integer ): Float;

  protected
    {@exclude}
    FCoord: array[ 0..NUMFIELD-1 ] of Float;

  public
    {Returns the length of the current vector.}
    function GetDist: Float;
    {Normalizes the current vector. For returning a normalized vector without
    changing the current one use VecNormalize}
    function Normalize: TVector;

    {$IF NUMFIELD>2}
    {Returns the cross product of the current vector and v, wheras the current
    vector is the first and v is the second.}
    function CrossProduct( v: TVector ): TVector;
    {$ENDIF}
    {Returns the dot product of the current vector and v, wheras the current
    vector is the first and v is the second.}
    function DotProduct( v: TVector ): Float;
    {Returns the index of the biggest component (X, Y, Z, W)
    which can be used with Coord
    @longCode( s:= v.Coord[ v.GetBiggestComp ]; )}
    function GetBiggestComp: Integer;

//    function RotateRad( Axis: TVec3; Angle: Float ): TVector;
//    function RotateDeg( Axis: TVec3; Angle: Float ): TVector;
//      function Transform( Matrix: TMatrix ): TVector;

    {Returns the negative of the current vector without altering the current.
    This is exivalent to @longCode( v2:= -v;)}
    function Invert: TVector;
    {Returns a pointer to the data of a vector. It can be used when passing to
    opengl for example. This is equivalent to
    @longCode( ptr:= @v.Coord[ 0 ]; )}
    function Ptr: PFloat;
//      function Mirror( Plane: pPlane ): TVector;

    {Compares two vectors and returns if there field values are equal.}
    function Compare( v: TVector ): Boolean;

    {Coord is one way to access the fields of a vector as a array.
    It is the default property so using @bold( v[ 0 ]) is the same as @bold( v.Coord[ 0 ] ).
    Alternatively you can use it's properties @bold( X, Y, Z, W ).
    However you can also write @bold( R, G, B, A ) if you use the vector for color values
    for example. Another alias is @bold( S, T, P, Q ) which is for texture coordinates.
    So all these code lines are equivalent:
    @longCode(
    v.Coords[ 0 ]:= 0.0;
    v[ 0 ]:= 0.0;
    v.x:= 0.0;
    v.r:= 0.0;
    v.s:= 0.0;
    )
    These different type of access are only for convenience but point to one and
    the same field. Except for the Coord alias everything works just like in @bold( glsl ).
    @br@br
    Depending on the vectors type some fields may not be availabe.@br
    For @link( TVec2 ) there is only @bold( X (R, S)) and @bold( Y (G, T)) defined.@br
    For @link( TVec3 ) there is only @bold( X (R, S)), @bold( Y (G, T)) and @bold( Z (B, P)) defined.@br
    For @link( TVec4 ) there is @bold( X (R, S)), @bold( Y (G, T)), @bold( Z (B, P)) and @bold( W (A, Q)) defined.@br
    @br@br
    Additionally @bold( glsl ) math3d supports swizzling. This means you can write:
    @longCode(
    v2:= v1.xxy; // equivalent to vec3( v1.x, v1.x, v1.y )
    )
    Unlike @bold( glsl ) this is not yet supported:
    @longCode(
    var
      v1: TVec2;
      v2: TVec3;
    begin
      v2:= v1.xxx; //Returns an error because v1 only has two members and cannot return a TVec3
    end;
    )
    This is due to an internal problem of how the math3d unit is written and is
    likely to change in the future. For more information on swizzling see this
    doc: https://www.opengl.org/wiki/Data_Type_%28GLSL%29#Swizzling
    }
    property Coord[ Index: Integer ]: Float read GetCoord write SetCoord; default;
    {This will return a string with the vector's values enclosed in <>. It can
    be used for debugging or printing the values of a vector on screen.}
    function ToString: String;

  published
    {@exclude} property X: Float index 0 read FCoord[ 0 ] write SetCoord;
    {@exclude} property R: Float index 0 read FCoord[ 0 ] write SetCoord;
    {@exclude} property S: Float index 0 read FCoord[ 0 ] write SetCoord;
    {$IF NUMFIELD>1}
    {@exclude} property Y: Float index 1 read FCoord[ 1 ] write SetCoord;
    {@exclude} property G: Float index 1 read FCoord[ 1 ] write SetCoord;
    {@exclude} property T: Float index 1 read FCoord[ 1 ] write SetCoord;
    {$ENDIF}

  published
    {$IF NUMFIELD>2}
    {@exclude} property Z: Float index 2 read FCoord[ 2 ] write SetCoord;
    {@exclude} property B: Float index 2 read FCoord[ 2 ] write SetCoord;
    {@exclude} property P: Float index 2 read FCoord[ 2 ] write SetCoord;

    {$ENDIF}
  published
    {$IF NUMFIELD>3}
    {@exclude} property W: Float index 3 read FCoord[ 3 ] write SetCoord;
    {@exclude} property A: Float index 3 read FCoord[ 3 ] write SetCoord;
    {@exclude} property Q: Float index 3 read FCoord[ 3 ] write SetCoord;
    {$ENDIF}
    {$INCLUDE math3d_vector_swizzle.inc}
end;

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF CONSTRUCTOR}
function Vector(
  {$IF NUMFIELD = 1}
  x: TYPEX;
  {$ELSEIF NUMFIELD = 2}
    {$IFDEF TYPEY}x{$ELSE}xy{$ENDIF}: TYPEX
    {$IFDEF TYPEY }
      ;y: TYPEY
    {$ENDIF}
  {$ELSEIF NUMFIELD = 3}
    {$IFDEF TYPEY}x
    {$ELSE}
      {$IFDEF TYPEZ}xy
      {$ELSE}xyz
      {$ENDIF}
    {$ENDIF}
    : TYPEX
    {$IFDEF TYPEY}
     {$IFDEF TYPEZ};y{$ELSE};yz{$ENDIF}: TYPEY
    {$ENDIF}
    {$IFDEF TYPEZ}
     ;z: TYPEZ
    {$ENDIF}
  {$ELSEIF NUMFIELD = 4}
    {$IFDEF TYPEY}x
    {$ELSE}
      {$IFDEF TYPEZ}xy
      {$ELSE}
        {$IFDEF TYPEW}xyz
        {$ELSE}xyzw
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
    : TYPEX

    {$IFDEF TYPEY}
      {$IFDEF TYPEZ};y
      {$ELSE}
        {$IFDEF TYPEW}
          ;yz
        {$ELSE}
          ;yzw
        {$ENDIF}
      {$ENDIF}
    : TYPEY
    {$ENDIF}

    {$IFDEF TYPEZ}
      {$IFDEF TYPEW};z{$ELSE};zw{$ENDIF}: TYPEZ
    {$ENDIF}

    {$IFDEF TYPEW}
      ;w: TYPEW
    {$ENDIF}
  {$ENDIF}
  ): TVector; overload; inline;

  {$IFDEF _IMPLEMENTATION}
  begin
    {$IF NUMFIELD=1}
    Result.x:= x;
    {$ELSEIF NUMFIELD=2}
      {$IFDEF TYPEY}
        Result.x:= x;
      {$ELSE}
        {$IF TYPEX = Float}
          Result.x:= xy;
          Result.y:= xy;
        {$ELSE}
          Result.x:= xy.x;
          Result.y:= xy.y;
        {$ENDIF}
      {$ENDIF}
      {$IFDEF TYPEY }
        Result.y:= y;
      {$ENDIF}
    {$ELSEIF NUMFIELD = 3}
      {$IFDEF TYPEY}
        Result.x:= x;
      {$ELSE}
        {$IFDEF TYPEZ}
          Result.x:= xy.x;
          Result.y:= xy.y;
        {$ELSE}
          {$IF TYPEX = Float}
            Result.x:= xyz;
            Result.y:= xyz;
            Result.z:= xyz;
          {$ELSE}
            Result.x:= xyz.x;
            Result.y:= xyz.y;
            Result.z:= xyz.z;
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
      {$IFDEF TYPEY}
        {$IFDEF TYPEZ}
          Result.y:= y;
        {$ELSE}
          Result.y:= yz.x;
          Result.z:= yz.y;
        {$ENDIF}
      {$ENDIF}
      {$IFDEF TYPEZ }
        Result.z:= z;
      {$ENDIF}
    {$ELSEIF NUMFIELD = 4}
      {$IFDEF TYPEY}
        Result.x:= x;
      {$ELSE}
        {$IFDEF TYPEZ}
          Result.x:= xy.x;
          Result.y:= xy.y;
        {$ELSE}
          {$IFDEF TYPEW}
            Result.x:= xyz.x;
            Result.y:= xyz.y;
            Result.z:= xyz.z;
          {$ELSE}
            {$IF TYPEX = Float}
              Result.x:= xyzw;
              Result.y:= xyzw;
              Result.z:= xyzw;
              Result.w:= 1.0;
            {$ELSE}
              Result.x:= xyzw.x;
              Result.y:= xyzw.y;
              Result.z:= xyzw.z;
              Result.w:= xyzw.w;
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}

      {$IFDEF TYPEY}
        {$IFDEF TYPEZ};
          Result.y:= y;
        {$ELSE}
          {$IFDEF TYPEW}
            Result.y:= yz.x;
            Result.z:= yz.y;
          {$ELSE}
            Result.y:= yzw.x;
            Result.z:= yzw.y;
            Result.w:= yzw.z;
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}

      {$IFDEF TYPEZ}
        {$IFDEF TYPEW};
          Result.z:= z;
        {$ELSE};
          Result.z:= zw.x;
          Result.w:= zw.y;
        {$ENDIF}
      {$ENDIF}

      {$IFDEF TYPEW}
        Result.w:= w;
      {$ENDIF}
    {$ENDIF}
  end;
  {$ENDIF}

  {$UNDEF TYPEX}
  {$UNDEF TYPEY}
  {$UNDEF TYPEZ}
  {$UNDEF TYPEW}
{$ENDIF}

{$IFDEF HELPERFUNCT}
{$IF NUMFIELD>2}
// This will return the cross product of two vectors. This function is only defined for TVec3 and above.
function VecCrossProduct( v1: TVector; v2: TVector ): TVector; overload;
{$ENDIF}

// This will return the dot product of two vectors.
function VecDotProduct( v1: TVector; v2: TVector ): Float; overload;
// This return a normalized vector leaving the original vector untouched.
function VecNormalize( Vec: TVector ): TVector; overload;
//  function VecMirror( Vec: TVector; Plane: TPlane ): TVector;

// This will return a negated vector and is equivalent to -v.
function VecInvert( Vec: TVector ): TVector; overload;
{$IF NUMFIELD = 4}
// This will unproject a vector using world, view and projection matrix. It will return the unprojected vector and the direction it is pointing to.
function VecUnproject( Vec: TVector; matWorld, matView, matProj: TMatrix; Width, Height: Integer; out vDirection: TVector ): TVector;
{$ENDIF}
// This will return the bigger components of two vectors. It works component-wise.
function Max( Vec1: TVector; Vec2: TVector ): TVector; overload;
// This will return the smaller components of two vectors. It works component-wise.
function Min( Vec1: TVector; Vec2: TVector ): TVector; overload;
// This will return the bigger components of one vector and a scalar. It works component-wise.
function Max( Vec1: TVector; s: Single ): TVector; overload;
// This will return the smaller components of one vector and a scalar. It works component-wise.
function Min( Vec1: TVector; s: Single ): TVector; overload;

//Operator for adding two vectors. Only same vector types can be added so far.
operator + ( a, b: TVector ) c: TVector; inline;
//Operator for subtracting two vectors. Only same vector types can be subtracted so far.
operator - ( a, b: TVector ) c: TVector; inline;
//Operator for multiplying two vectors component-wise. Only same vector types can be multiplied so far.
operator * ( a, b: TVector ) c: Float; inline;
//Operator for scaling a vector using a scalar (float).
operator * ( a: TVector; s: Float ) c: TVector; inline;
//Operator for scaling a vector using a scalar (float).
operator * ( s: Float; b: TVector ) c: TVector; inline;
//Operator for dividing two vectors component-wise. Only same vector types can be divided so far.
operator / ( a, b: TVector ) c: TVector; inline;
//Operator for scaling a vector using a scalar (float). This is the same as multiplying by -s
operator / ( a: TVector; s: Float ) c: TVector; inline;
//Operator for negating a vector.
operator - ( a: TVector ) c: TVector; inline;

{$UNDEF HELPERFUNCT}
{$ENDIF}


{$IFDEF IMPLEMENTATION}

{ TVector }

procedure TVector.SetCoord(AIndex: Integer; AValue: Float);
begin
  FCoord[ AIndex ]:= AValue;
  //TODO: ONCHANGE EVENT
end;

function TVector.GetCoord( AIndex: Integer ): Float;
begin
  Result:= FCoord[ AIndex ];
end;

function TVector.GetDist: Float;
begin
  Result:= sqrt( sqr( x )
         {$IF NUMFIELD>1}
         + sqr( y )
         {$ENDIF}
         {$IF NUMFIELD>2}
         + sqr( z )
         {$ENDIF}
         {$IF NUMFIELD>3}
         + sqr( w )
         {$ENDIF}
         );
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

{$IF NUMFIELD>2}
function TVector.CrossProduct(v: TVector): TVector;
begin
  Result.x:= y * v.z - z * v.y;
  Result.y:= z * v.x - x * v.z;
  Result.z:= x * v.y - y * v.x;
end;
{$ENDIF}

function TVector.DotProduct(v: TVector): Float;
begin
  Result:= x * v.x
          {$IF NUMFIELD>1}
           + y * v.y
          {$ENDIF}
          {$IF NUMFIELD>2}
           + z * v.z
          {$ENDIF}
          ;
end;

function TVector.GetBiggestComp: Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 0 to NUMFIELD - 1 do
    if ( Abs( FCoord[ i ]) > Abs( FCoord[ Result ])) then
      Result:= i;
end;
(*
{$IF NUMFIELD = 3}
function TVector.RotateRad(Axis: TVec3; Angle: Float): TVector;
begin
  Result:= VecTransform( Self, Mat4Rot( Axis, Angle ));
end;

function TVector.RotateDeg(Axis: TVec3; Angle: Float): TVector;
begin
  Result:= VecTransform( Self, Mat4Rot( Axis, deg2rad * Angle ));
end;
{$ENDIF}
*)

function TVector.Invert: TVector;
begin
  x:= -x;
  {$IF NUMFIELD > 1}
  y:= -y;
  {$ENDIF}
  {$IF NUMFIELD > 2}
  z:= -z;
  {$ENDIF}
  {$IF NUMFIELD > 3}
  w:= -w;
  {$ENDIF}
  Result:= Self;
end;

function TVector.Ptr: PFloat;
begin
  Result:= @FCoord[ 0 ];
end;

{$INCLUDE math3d_vector_swizzle.inc}


function TVector.ToString: String;
begin
  {$IF NUMFIELD = 2}
  Result:= Format( '<%.4f,%.4f>', [ X, Y ]);
  {$ENDIF}
  {$IF NUMFIELD = 3}
  Result:= Format( '<%.4f, %.4f, %.4f>', [ X, Y, Z ]);
  {$ENDIF}
  {$IF NUMFIELD = 4}
  Result:= Format( '<%.4f, %.4f, %.4f, %.4f>', [ X, Y, Z, W ]);
  {$ENDIF}
end;

function TVector.Compare(v: TVector): Boolean;
begin
  Result:= ( v.x = x )
       {$IF NUMFIELD > 2}
       and ( v.y = y )
       {$ENDIF}
       {$IF NUMFIELD > 2}
       and ( v.z = z )
       {$ENDIF}
       ;
end;


operator+(a, b: TVector)c: TVector;
begin
  c.x:= a.x + b.x;
  {$IF NUMFIELD > 1}
  c.y:= a.y + b.y;
  {$ENDIF}
  {$IF NUMFIELD > 2}
  c.z:= a.z + b.z;
  {$ENDIF}
  {$IF NUMFIELD > 3}
  c.w:= a.w + b.w;
  {$ENDIF}
end;

operator-(a, b: TVector)c: TVector;
begin
  c.x:= a.x - b.x;
  {$IF NUMFIELD > 1}
  c.y:= a.y - b.y;
  {$ENDIF}
  {$IF NUMFIELD > 2}
  c.z:= a.z - b.z;
  {$ENDIF}
  {$IF NUMFIELD > 3}
  c.w:= a.w - b.w;
  {$ENDIF}
end;

operator*(a, b: TVector)c: Float;
begin
  Result:= a.DotProduct( b );
end;

operator*(a: TVector; s: Float)c: TVector;
begin
  c.x:= a.x * s;
  {$IF NUMFIELD > 1}
  c.y:= a.y * s;
  {$ENDIF}
  {$IF NUMFIELD > 2}
  c.z:= a.z * s;
  {$ENDIF}
  {$IF NUMFIELD > 3}
  c.w:= a.w * s;
  {$ENDIF}
end;

operator*(s: Float; b: TVector)c: TVector;
begin
  c.x:= b.x * s;
  {$IF NUMFIELD > 1}
  c.y:= b.y * s;
  {$ENDIF}
  {$IF NUMFIELD > 2}
  c.z:= b.z * s;
  {$ENDIF}
  {$IF NUMFIELD > 3}
  c.w:= b.w * s;
  {$ENDIF}
end;

operator/(a, b: TVector)c: TVector;
begin
  c.x:= a.x / b.x;
  {$IF NUMFIELD > 1}
  c.y:= a.y / b.y;
  {$ENDIF}
  {$IF NUMFIELD > 2}
  c.z:= a.z / b.z;
  {$ENDIF}
  {$IF NUMFIELD > 3}
  c.w:= a.w / b.w;
  {$ENDIF}
end;

operator/(a: TVector; s: Float)c: TVector;
begin
  c.x:= a.x / s;
  {$IF NUMFIELD > 1}
  c.y:= a.y / s;
  {$ENDIF}
  {$IF NUMFIELD > 2}
  c.z:= a.z / s;
  {$ENDIF}
  {$IF NUMFIELD > 3}
  c.w:= a.w / s;
  {$ENDIF}
end;

operator-(a: TVector)c: TVector;
begin
  c:= a * -1;
end;

{$IF NUMFIELD > 2}
function VecCrossProduct(v1: TVector; v2: TVector): TVector;
begin
  Result:= v1.CrossProduct( v2 );
end;
{$ENDIF}

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

{$IF NUMFIELD = 4}

function VecUnproject( Vec: TVector; matWorld, matView, matProj: TMatrix; Width, Height: Integer; out vDirection: TVector ): TVector;
var
  m: TMatrix;
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
{$ENDIF}

function Max(Vec1: TVector; Vec2: TVector): TVector;
begin
  Result.x:= Max( Vec1.x, Vec2.x );
  {$IF NUMFIELD > 1}
  Result.y:= Max( Vec1.y, Vec2.y );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= Max( Vec1.z, Vec2.z );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= Max( Vec1.w, Vec2.w );
  {$ENDIF}
end;

function Min(Vec1: TVector; Vec2: TVector): TVector;
begin
  Result.x:= Min( Vec1.x, Vec2.x );
  {$IF NUMFIELD > 1}
  Result.y:= Min( Vec1.y, Vec2.y );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= Min( Vec1.z, Vec2.z );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= Min( Vec1.w, Vec2.w );
  {$ENDIF}
end;

function Max(Vec1: TVector; s: Single): TVector;
begin
  Result.x:= Max( Vec1.x, s );
  {$IF NUMFIELD > 1}
  Result.y:= Max( Vec1.y, s );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= Max( Vec1.z, s );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= Max( Vec1.w, s );
  {$ENDIF}
end;

function Min(Vec1: TVector; s: Single): TVector;
begin
  Result.x:= Min( Vec1.x, s );
  {$IF NUMFIELD > 1}
  Result.y:= Min( Vec1.y, s );
  {$ENDIF}
  {$IF NUMFIELD > 2}
  Result.z:= Min( Vec1.z, s );
  {$ENDIF}
  {$IF NUMFIELD > 3}
  Result.w:= Min( Vec1.w, s );
  {$ENDIF}
end;


{$UNDEF IMPLEMENTATION}
{$ENDIF}