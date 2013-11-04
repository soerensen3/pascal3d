//math3d_vector.inc

{$IFDEF INTERFACE}
{ TVector }
TVector = packed object
  private
    procedure SetCoord( AIndex: Integer; AValue: Float );
    function GetCoord( AIndex: Integer ): Float;

  protected
    FCoord: array[ 0..NUMFIELD-1 ] of Float;

  public
    function GetDist: Float;
    function Normalize: TVector;

    {$IF NUMFIELD>2}
    function CrossProduct( v: TVector ): TVector;
    {$ENDIF}
    function DotProduct( v: TVector ): Float;
    function GetBiggestComp: Integer;

//    function RotateRad( Axis: TVec3; Angle: Float ): TVector;
//    function RotateDeg( Axis: TVec3; Angle: Float ): TVector;
//      function Transform( Matrix: TMatrix ): TVector;
    function Invert: TVector;
    function Ptr: PFloat;
//      function Mirror( Plane: pPlane ): TVector;

    function Compare( v: TVector ): Boolean;

    property Coord[ Index: Integer ]: Float read GetCoord write SetCoord; default;

  published
    property X: Float index 0 read FCoord[ 0 ] write SetCoord;
    property R: Float index 0 read FCoord[ 0 ] write SetCoord;
    property S: Float index 0 read FCoord[ 0 ] write SetCoord;
    {$IF NUMFIELD>1}
    property Y: Float index 1 read FCoord[ 1 ] write SetCoord;
    property G: Float index 1 read FCoord[ 1 ] write SetCoord;
    property T: Float index 1 read FCoord[ 1 ] write SetCoord;
    {$ENDIF}
    {$IF NUMFIELD>2}
    property Z: Float index 2 read FCoord[ 2 ] write SetCoord;
    property B: Float index 2 read FCoord[ 2 ] write SetCoord;
    property P: Float index 2 read FCoord[ 2 ] write SetCoord;
    {$ENDIF}
    {$IF NUMFIELD>3}
    property W: Float index 3 read FCoord[ 3 ] write SetCoord;
    property A: Float index 3 read FCoord[ 3 ] write SetCoord;
    property Q: Float index 3 read FCoord[ 3 ] write SetCoord;
    {$ENDIF}
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
              Result.w:= xyzw;
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
function VecCrossProduct( v1: TVector; v2: TVector ): TVector; overload;
{$ENDIF}

function VecDotProduct( v1: TVector; v2: TVector ): Float; overload;
function VecNormalize( Vec: TVector ): TVector; overload;
//  function VecMirror( Vec: TVector; Plane: TPlane ): TVector;
function VecInvert( Vec: TVector ): TVector; overload;
{$IF NUMFIELD = 4}
function VecUnproject( Vec: TVector; matWorld, matView, matProj: TMatrix; Width, Height: Integer; out vDirection: TVector ): TVector;
{$ENDIF}

function Max( Vec1: TVector; Vec2: TVector ): TVector; overload;
function Min( Vec1: TVector; Vec2: TVector ): TVector; overload;
function Max( Vec1: TVector; s: Single ): TVector; overload;
function Min( Vec1: TVector; s: Single ): TVector; overload;

operator + ( a, b: TVector ) c: TVector; inline;
operator - ( a, b: TVector ) c: TVector; inline;
operator * ( a, b: TVector ) c: Float; inline;
operator * ( a: TVector; s: Float ) c: TVector; inline;
operator * ( s: Float; b: TVector ) c: TVector; inline;
operator / ( a, b: TVector ) c: TVector; inline;
operator / ( a: TVector; s: Float ) c: TVector; inline;
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
