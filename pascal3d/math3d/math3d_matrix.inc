//math3d_matrix.inc

{$IF NUMFIELD = 2}
  {$DEFINE MATIDENTITY:= Mat2Identity}
{$ELSEIF NUMFIELD = 3}
  {$DEFINE MATIDENTITY:= Mat3Identity}
{$ELSEIF NUMFIELD = 4}
  {$DEFINE MATIDENTITY:= Mat4Identity}
{$ENDIF}

{$IFDEF INTERFACE}
{ TMatrix }
type
  TMatrix = packed object
    private
      function GetIndex( index: Integer ): Float; inline;
      function GetMField( AIndex: Integer ): Float; inline;
      procedure SetIndex( index: Integer ; AValue: Float );
      procedure SetMField( AIndex: Integer; AValue: Float );

    public
      m: array [0..NUMFIELD*NUMFIELD-1] of Float;

      // First value indicates row, second value indicates column
      property _00: Float index                0  read GetMField write SetMField;
      {$IF NUMFIELD > 1 }
      property _01: Float index                1  read GetMField write SetMField;
      property _10: Float index NUMFIELD          read GetMField write SetMField;
      property _11: Float index NUMFIELD     + 1  read GetMField write SetMField;
      {$ENDIF}
      {$IF NUMFIELD > 2 }
      property _02: Float index                2  read GetMField write SetMField;
      property _12: Float index NUMFIELD     + 2  read GetMField write SetMField;
      property _20: Float index NUMFIELD * 2      read GetMField write SetMField;
      property _21: Float index NUMFIELD * 2 + 1  read GetMField write SetMField;
      property _22: Float index NUMFIELD * 2 + 2  read GetMField write SetMField;
      {$ENDIF}
      {$IF NUMFIELD > 3 }
      property _03: Float index                3  read GetMField write SetMField;
      property _13: Float index NUMFIELD     + 3  read GetMField write SetMField;
      property _23: Float index NUMFIELD * 2 + 3  read GetMField write SetMField;
      property _30: Float index NUMFIELD * 3      read GetMField write SetMField;
      property _31: Float index NUMFIELD * 3 + 1  read GetMField write SetMField;
      property _32: Float index NUMFIELD * 3 + 2  read GetMField write SetMField;
      property _33: Float index NUMFIELD * 3 + 3  read GetMField write SetMField;
      {$ENDIF}

      property _m[ index: Integer ]: Float read GetIndex write SetIndex; default;
  end;

function MatrixTranslate( Vec: TVector ): TMatrix; overload;
function MatrixScale( Vec: TVector ): TMatrix; overload;

{$IF NUMFIELD = 4}
function Mat4Rot( Axis: TVec3; Angle: Float ): TMatrix; overload;
//  function MatrixMirror( Plane: TPlane ): TMatrix;
function MatrixDeterminant( Mat: TMatrix ): Float;
function MatrixInverse( Mat: TMatrix; out MatInv: TMatrix ): Boolean;
function MatrixTranspose( Matrix: TMatrix ): TMatrix;
function MatrixPerspectiveFOVLH( FOV, Aspect, Near, Far: Float ): TMatrix;
function MatrixPerspectiveFOVRH( FOV, Aspect, Near, Far: Float ): TMatrix;
function MatrixOrthoLH( w, h, Near, Far: Float ): TMatrix;
function MatrixOrthoRH( w, h, Near, Far: Float ): TMatrix;
//  function MatrixCubeMap( Face: Cardinal ): TMatrix;
function MatrixLookAtLH( EyePt, LookAt, UpDir: TVec3 ): TMatrix;
function MatrixLookAtRH( EyePt, LookAt, UpDir: TVec3 ): TMatrix;
{$ENDIF}

operator * ( a: TMatrix; b: Float ) c: TMatrix; inline;
operator * ( a: TMatrix; b: TMatrix ) c: TMatrix; inline; // left first - right last
operator * ( a: TVector; b: TMatrix ) c: TVector; inline; // PRE-Multiply
operator * ( a: TMatrix; b: TVector ) c: TVector; inline; // POST-Multiply

{$UNDEF INTERFACE}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

operator*(a: TMatrix; b: Float)c: TMatrix;
var
  i: Integer;
begin
  for i:= 0 to 15 do
    a[ i ]:= a[ i ] * b;
  c:= a;
end;

(*
{$IF NUMFIELD=4}
operator * ( a: TVector; b: TMatrix ) c: TVector; inline;
begin
  c:= VecTransform( a, b );
end;

operator*(a: TMatrix; b: TVector)c: TVector; inline;
begin
  c:= VecTransform( b, a );
end;
{$ENDIF}
*)

{ TMatrix }

function TMatrix.GetIndex( index: Integer ): Float;
begin
  Result:= m[ index ];
end;

function TMatrix.GetMField(AIndex: Integer): Float;
begin
  Result:= m[ AIndex ];
end;

procedure TMatrix.SetIndex( index: Integer ; AValue: Float);
begin
  m[ index ]:= AValue;
end;

procedure TMatrix.SetMField(AIndex: Integer; AValue: Float);
begin
  m[ AIndex ]:= AValue;
end;

operator * ( a: TMatrix; b: TMatrix ) c: TMatrix; inline;
begin
  c._00:= a._00 * b._00
        {$IF NUMFIELD > 1}
        + a._01 * b._10
        {$ENDIF}
        {$IF NUMFIELD > 2}
        + a._02 * b._20
        {$ENDIF}
        {$IF NUMFIELD > 3}
        + a._03 * b._30
        {$ENDIF}
        ;
  {$IF NUMFIELD > 1}
  c._01:= a._00 * b._01
        + a._01 * b._11
        {$IF NUMFIELD > 2}
        + a._02 * b._21
        {$ENDIF}
        {$IF NUMFIELD > 3}
        + a._03 * b._31
        {$ENDIF}
        ;

  c._10:= a._10 * b._00
        + a._11 * b._10
        {$IF NUMFIELD > 2}
        + a._12 * b._20
        {$ENDIF}
        {$IF NUMFIELD > 3}
        + a._13 * b._30
        {$ENDIF}
        ;

  c._11:= a._10 * b._01
        + a._11 * b._11
        {$IF NUMFIELD > 2}
        + a._12 * b._21
        {$ENDIF}
        {$IF NUMFIELD > 3}
        + a._13 * b._31
        {$ENDIF}
        ;
  {$ENDIF}

  {$IF NUMFIELD > 2}
  c._02:= a._00 * b._02
        + a._01 * b._12
        + a._02 * b._22
        {$IF NUMFIELD > 3}
        + a._03 * b._32
        {$ENDIF}
        ;

  c._12:= a._10 * b._02
        + a._11 * b._12
        + a._12 * b._22
        {$IF NUMFIELD > 3}
        + a._13 * b._32
        {$ENDIF}
        ;
  {$ENDIF}

  {$IF NUMFIELD > 3}
  c._03:= a._00 * b._03
        + a._01 * b._13
        + a._02 * b._23
        + a._03 * b._33
        ;

  c._20:= a._20 * b._00
        + a._21 * b._10
        + a._22 * b._20
        + a._23 * b._30
        ;

  c._21:= a._20 * b._01 + a._21 * b._11 + a._22 * b._21 + a._23 * b._31;
  c._22:= a._20 * b._02 + a._21 * b._12 + a._22 * b._22 + a._23 * b._32;
  c._13:= a._10 * b._03 + a._11 * b._13 + a._12 * b._23 + a._13 * b._33;
  c._23:= a._20 * b._03 + a._21 * b._13 + a._22 * b._23 + a._23 * b._33;
  c._30:= a._30 * b._00 + a._31 * b._10 + a._32 * b._20 + a._33 * b._30;
  c._31:= a._30 * b._01 + a._31 * b._11 + a._32 * b._21 + a._33 * b._31;
  c._32:= a._30 * b._02 + a._31 * b._12 + a._32 * b._22 + a._33 * b._32;
  c._33:= a._30 * b._03 + a._31 * b._13 + a._32 * b._23 + a._33 * b._33;
  {$ENDIF}
end;

operator*( a: TVector; b: TMatrix ) c: TVector;
begin
  Result.x:= ( a.x * b._00 )
           {$IF NUMFIELD > 1}
           + ( a.y * b._10 )
           {$ENDIF}
           {$IF NUMFIELD > 2}
           + ( a.z * b._20 )
           {$ENDIF}
           {$IF NUMFIELD > 3}
           + ( a.w * b._30 )
           {$ENDIF}
           ;

  {$IF NUMFIELD > 1}
  Result.y:= ( a.x * b._01 )
           + ( a.y * b._11 )
           {$IF NUMFIELD > 2}
           + ( a.z * b._21 )
           {$ENDIF}
           {$IF NUMFIELD > 3}
           + ( a.w * b._31 )
           {$ENDIF}
           ;
  {$ENDIF}

  {$IF NUMFIELD > 2}
  Result.z:= ( a.x * b._02 )
           + ( a.y * b._12 )
           + ( a.z * b._22 )
           {$IF NUMFIELD > 3}
           + ( a.w * b._32 )
           {$ENDIF}
           ;
  {$ENDIF}

  {$IF NUMFIELD > 3}
  Result.w:= ( a.x * b._03 )
           + ( a.y * b._13 )
           + ( a.z * b._23 )
           + ( a.w * b._33 );
  {$ENDIF}
end;

operator*( a: TMatrix; b: TVector )c: TVector;
begin
  Result.x:= ( a._00 * b.x )
           {$IF NUMFIELD > 1}
           + ( a._01 * b.y )
           {$ENDIF}
           {$IF NUMFIELD > 2}
           + ( a._02 * b.z )
           {$ENDIF}
           {$IF NUMFIELD > 3}
           + ( a._03 * b.w )
           {$ENDIF}
           ;

  {$IF NUMFIELD > 1}
  Result.y:= ( a._10 * b.x )
           + ( a._11 * b.y )
           {$IF NUMFIELD > 2}
           + ( a._12 * b.z )
           {$ENDIF}
           {$IF NUMFIELD > 3}
           + ( a._13 * b.w )
           {$ENDIF}
           ;
  {$ENDIF}

  {$IF NUMFIELD > 2}
  Result.z:= ( a._20 * b.x )
           + ( a._21 * b.y )
           + ( a._22 * b.z )
           {$IF NUMFIELD > 3}
           + ( a._23 * b.w )
           {$ENDIF}
           ;
  {$ENDIF}

  {$IF NUMFIELD > 3}
  Result.w:= ( a._30 * b.x )
           + ( a._31 * b.y )
           + ( a._32 * b.z )
           + ( a._33 * b.w );
  {$ENDIF}
end;

function MatrixTranslate( Vec: TVector ): TMatrix;
begin
  Result:= MatIdentity;
  {$IF NUMFIELD = 2}
    Result._10:= Vec.x;
    Result._11:= Vec.y;
  {$ELSEIF NUMFIELD = 3}
    Result._20:= Vec.x;
    Result._21:= Vec.y;
    Result._22:= Vec.z;
  {$ELSEIF NUMFIELD = 4}
    Result._30:= Vec.x;
    Result._31:= Vec.y;
    Result._32:= Vec.z;
    Result._33:= Vec.w;
  {$ENDIF}
end;

function MatrixScale( Vec: TVector ): TMatrix;
begin
  Result:= MatIdentity;
  {$IF NUMFIELD = 2}
    Result._00:= Vec.x;
    Result._11:= Vec.y;
  {$ELSEIF NUMFIELD = 3}
    Result._00:= Vec.x;
    Result._11:= Vec.y;
    Result._22:= Vec.z;
  {$ELSEIF NUMFIELD = 4}
    Result._00:= Vec.x;
    Result._11:= Vec.y;
    Result._22:= Vec.z;
    Result._33:= Vec.w;
  {$ENDIF}
end;

{$IF NUMFIELD = 4}
function Mat4Rot(Axis: TVec3; Angle: Float ): TMatrix;
const
  x = 0;
  y = 1;
  z = 2;
  w = 3;
var
   cosine, sine, one_minus_cosine : Extended;
begin
   SinCos(Angle, Sine, Cosine);
   one_minus_cosine:=1 - cosine;
   Axis.Normalize;

   Result._00:= (one_minus_cosine * Sqr(Axis.x)) + Cosine;
   Result._01:= (one_minus_cosine * Axis.x * Axis.y) - (Axis.z * Sine);
   Result._02:= (one_minus_cosine * Axis.z * Axis.x) + (Axis.y * Sine);
   Result._03:= 0;

   Result._10:= (one_minus_cosine * Axis.x * Axis.y) + (Axis.z * Sine);
   Result._11:= (one_minus_cosine * Sqr(Axis.y)) + Cosine;
   Result._12:= (one_minus_cosine * Axis.y * Axis.z) - (Axis.x * Sine);
   Result._13:= 0;

   Result._20:= (one_minus_cosine * Axis.z * Axis.x) - (Axis.y * Sine);
   Result._21:= (one_minus_cosine * Axis.y * Axis.z) + (Axis.x * Sine);
   Result._22:= (one_minus_cosine * Sqr(Axis.z)) + Cosine;
   Result._23:= 0;

   Result._30:= 0;
   Result._31:= 0;
   Result._32:= 0;
   Result._33:= 1;
end;

function MatrixDet( const a1, a2, a3, b1, b2, b3, c1, c2, c3: Float ): Float;
begin
  Result:= a1 * ( b2 * c3 - b3 * c2 )
         - b1 * ( a2 * c3 - a3 * c2 )
         + c1 * ( a2 * b3 - a3 * b2 );
end;

function MatrixDeterminant(Mat: TMatrix): Float;
begin
  Result:= Mat._00 * MatrixDet( Mat[ 5 ], Mat[ 9 ], Mat[ 13 ],
                                Mat[ 6 ], Mat[ 10 ], Mat[ 14 ],
                                Mat[ 7 ], Mat[ 11 ], Mat[ 15 ])

         - Mat._01 * MatrixDet( Mat[ 4 ], Mat[ 8 ], Mat[ 12 ],
                                Mat[ 6 ], Mat[ 10 ], Mat[ 14 ],
                                Mat[ 7 ], Mat[ 11 ], Mat[ 15 ])

         + Mat._02 * MatrixDet( Mat[ 4 ], Mat[ 8 ], Mat[ 12 ],
                                Mat[ 5 ], Mat[ 9 ], Mat[ 13 ],
                                Mat[ 7 ], Mat[ 11 ], Mat[ 15 ])

         - Mat._03 * MatrixDet( Mat[ 4 ], Mat[ 8 ], Mat[ 12 ],
                                Mat[ 5 ], Mat[ 9 ], Mat[ 13 ],
                                Mat[ 6 ], Mat[ 10 ], Mat[ 14 ]);
end;

function MatrixInverse( Mat: TMatrix; out MatInv: TMatrix ): Boolean;
var
  det, a1, a2, a3, a4, b1, b2, b3, b4,
  c1, c2, c3, c4, d1, d2, d3, d4: Float;
begin
  det:= MatrixDeterminant( Mat );
  if ( Abs( det ) < epsilon ) then
    Result:= False
  else
    begin
      Result:= True;
      a1:= Mat._00; b1:= Mat._01; c1:= Mat._02; d1:= Mat._03;
      a2:= Mat._10; b2:= Mat._11; c2:= Mat._12; d2:= Mat._13;
      a3:= Mat._20; b3:= Mat._21; c3:= Mat._22; d3:= Mat._23;
      a4:= Mat._30; b4:= Mat._31; c4:= Mat._32; d4:= Mat._33;
      MatInv._00:= MatrixDet(b2, b3, b4, c2, c3, c4, d2, d3, d4);
      MatInv._10:=-MatrixDet(a2, a3, a4, c2, c3, c4, d2, d3, d4);
      MatInv._20:= MatrixDet(a2, a3, a4, b2, b3, b4, d2, d3, d4);
      MatInv._30:=-MatrixDet(a2, a3, a4, b2, b3, b4, c2, c3, c4);
      MatInv._01:=-MatrixDet(b1, b3, b4, c1, c3, c4, d1, d3, d4);
      MatInv._11:= MatrixDet(a1, a3, a4, c1, c3, c4, d1, d3, d4);
      MatInv._21:=-MatrixDet(a1, a3, a4, b1, b3, b4, d1, d3, d4);
      MatInv._31:= MatrixDet(a1, a3, a4, b1, b3, b4, c1, c3, c4);
      MatInv._02:= MatrixDet(b1, b2, b4, c1, c2, c4, d1, d2, d4);
      MatInv._12:=-MatrixDet(a1, a2, a4, c1, c2, c4, d1, d2, d4);
      MatInv._22:= MatrixDet(a1, a2, a4, b1, b2, b4, d1, d2, d4);
      MatInv._32:=-MatrixDet(a1, a2, a4, b1, b2, b4, c1, c2, c4);
      MatInv._03:=-MatrixDet(b1, b2, b3, c1, c2, c3, d1, d2, d3);
      MatInv._13:= MatrixDet(a1, a2, a3, c1, c2, c3, d1, d2, d3);
      MatInv._23:=-MatrixDet(a1, a2, a3, b1, b2, b3, d1, d2, d3);
      MatInv._33:= MatrixDet(a1, a2, a3, b1, b2, b3, c1, c2, c3);

      MatInv:= MatInv * ( 1/det );
    end;
end;

function MatrixTranspose(Matrix: TMatrix): TMatrix;
begin
  Result._00:= Matrix._00;
  Result._10:= Matrix._01;
  Result._20:= Matrix._02;
  Result._30:= Matrix._03;

  Result._01:= Matrix._10;
  Result._11:= Matrix._11;
  Result._21:= Matrix._12;
  Result._31:= Matrix._13;

  Result._02:= Matrix._20;
  Result._12:= Matrix._21;
  Result._22:= Matrix._22;
  Result._32:= Matrix._23;

  Result._03:= Matrix._30;
  Result._13:= Matrix._31;
  Result._23:= Matrix._32;
  Result._33:= Matrix._33;
end;

function MatrixPerspectiveFOVLH( FOV, Aspect, Near, Far: Float ): TMatrix;
var
  w, h: Float;
begin
  h:= Cot( FOV / 2 );
  w:= h / Aspect;
  Result._00:= w; Result._01:= 0; Result._02:= 0;                          Result._03:= 0;
  Result._10:= 0; Result._11:= h; Result._12:= 0;                          Result._13:= 0;
//  Result._20:= 0; Result._21:= 0; Result._22:= Far/( Far - Near );         Result._23:= 1;
  Result._20:= 0; Result._21:= 0; Result._22:= ( Far + Near )/( Near - Far ); Result._23:= -1;
//  Result._30:= 0; Result._31:= 0; Result._32:= -Near * Far/( Far - Near ); Result._33:= 0;
  Result._30:= 0; Result._31:= 0; Result._32:= (2 * Far * Near )/( Near - Far ); Result._33:= 0;
end;

function MatrixPerspectiveFOVRH( FOV, Aspect, Near, Far: Float ): TMatrix;
var
  w, h: Float;
begin
  h:= Cot( FOV / 2 );
  w:= h / Aspect;
  Result._00:= w; Result._01:= 0; Result._02:= 0;                          Result._03:= 0;
  Result._10:= 0; Result._11:= h; Result._12:= 0;                          Result._13:= 0;
  Result._20:= 0; Result._21:= 0; Result._22:= Far/( Near - Far );         Result._23:= -1;
  Result._30:= 0; Result._31:= 0; Result._32:= Near * Far/( Near - Far );  Result._33:= 0;
end;

function MatrixOrthoLH( w, h, Near, Far: Float ): TMatrix;
begin
  Result._00:= 2/w; Result._01:= 0;   Result._02:= 0;                   Result._03:= 0;
  Result._10:= 0;   Result._11:= 2/h; Result._12:= 0;                   Result._13:= 0;
  Result._20:= 0;   Result._21:= 0;   Result._22:= 1/( Far - Near );    Result._23:= 0;
  Result._30:= 0;   Result._31:= 0;   Result._32:= Near/( Near - Far ); Result._33:= 1;
end;

function MatrixOrthoRH( w, h, Near, Far: Float ): TMatrix;
begin
  Result._00:= 2/w; Result._01:= 0;   Result._02:= 0;                   Result._03:= 0;
  Result._10:= 0;   Result._11:= 2/h; Result._12:= 0;                   Result._13:= 0;
  Result._20:= 0;   Result._21:= 0;   Result._22:= 1/( Near - Far );    Result._23:= 0;
  Result._30:= 0;   Result._31:= 0;   Result._32:= Near/( Near - Far ); Result._33:= 1;
end;

function MatrixLookAtLH( EyePt, LookAt, UpDir: TVec3 ): TMatrix;
var
  xa, ya, za: TVec3;
begin
  za:= VecNormalize ( LookAt - EyePt );
  xa:= VecNormalize( VecCrossProduct( UpDir, za ));
  ya:= VecCrossProduct( za, xa );

  Result._00:= xa.x; Result._01:= ya.x; Result._02:= za.x; Result._03:= 0;
  Result._10:= xa.y; Result._11:= ya.y; Result._12:= za.y; Result._13:= 0;
  Result._20:= xa.z; Result._21:= ya.z; Result._22:= za.z; Result._23:= 0;

  Result._30:= -VecDotProduct( xa, EyePt );
  Result._31:= -VecDotProduct( ya, EyePt );
  Result._32:= -VecDotProduct( za, EyePt );
  Result._33:= 1;
end;

function MatrixLookAtRH( EyePt, LookAt, UpDir: TVec3 ): TMatrix;
var
  xa, ya, za: TVec3;
begin
  za:= VecNormalize( EyePt - LookAt );
  xa:= VecNormalize( VecCrossProduct( UpDir, za ));
  ya:= VecCrossProduct( za, xa );

  Result._00:= xa.x; Result._01:= ya.x; Result._02:= za.x; Result._03:= 0;
  Result._10:= xa.y; Result._11:= ya.y; Result._12:= za.y; Result._13:= 0;
  Result._20:= xa.z; Result._21:= ya.z; Result._22:= za.z; Result._23:= 0;

  Result._30:= -VecDotProduct( xa, EyePt );
  Result._31:= -VecDotProduct( ya, EyePt );
  Result._32:= -VecDotProduct( za, EyePt );
  Result._33:= 1;
end;

{$ENDIF}

{$UNDEF IMPLEMENTATION}
{$ENDIF}
