//math3d_matrix.inc
operator * ( a: TMatrix4f; b: TMatrix4f ) c: TMatrix4f; //inline;
begin
  c._00:= a._00 * b._00 + a._01 * b._10 + a._02 * b._20 + a._03 * b._30;
  c._01:= a._00 * b._01 + a._01 * b._11 + a._02 * b._21 + a._03 * b._31;
  c._02:= a._00 * b._02 + a._01 * b._12 + a._02 * b._22 + a._03 * b._32;
  c._03:= a._00 * b._03 + a._01 * b._13 + a._02 * b._23 + a._03 * b._33;

  c._10:= a._10 * b._00 + a._11 * b._10 + a._12 * b._20 + a._13 * b._30;
  c._11:= a._10 * b._01 + a._11 * b._11 + a._12 * b._21 + a._13 * b._31;
  c._12:= a._10 * b._02 + a._11 * b._12 + a._12 * b._22 + a._13 * b._32;
  c._13:= a._10 * b._03 + a._11 * b._13 + a._12 * b._23 + a._13 * b._33;

  c._20:= a._20 * b._00 + a._21 * b._10 + a._22 * b._20 + a._23 * b._30;
  c._21:= a._20 * b._01 + a._21 * b._11 + a._22 * b._21 + a._23 * b._31;
  c._22:= a._20 * b._02 + a._21 * b._12 + a._22 * b._22 + a._23 * b._32;
  c._23:= a._20 * b._03 + a._21 * b._13 + a._22 * b._23 + a._23 * b._33;

  c._30:= a._30 * b._00 + a._31 * b._10 + a._32 * b._20 + a._33 * b._30;
  c._31:= a._30 * b._01 + a._31 * b._11 + a._32 * b._21 + a._33 * b._31;
  c._32:= a._30 * b._02 + a._31 * b._12 + a._32 * b._22 + a._33 * b._32;
  c._33:= a._30 * b._03 + a._31 * b._13 + a._32 * b._23 + a._33 * b._33;
end;

function MatrixRot(Axis: TVector; Angle: Single): TMatrix4f;
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

   Result._00:=(one_minus_cosine * Sqr(Axis.x)) + Cosine;
   Result._01:=(one_minus_cosine * Axis.x * Axis.y) - (Axis.z * Sine);
   Result._02:=(one_minus_cosine * Axis.z * Axis.x) + (Axis.y * Sine);
   Result._03:=0;

   Result._10:=(one_minus_cosine * Axis.x * Axis.y) + (Axis.z * Sine);
   Result._11:=(one_minus_cosine * Sqr(Axis.y)) + Cosine;
   Result._12:=(one_minus_cosine * Axis.y * Axis.z) - (Axis.x * Sine);
   Result._13:=0;

   Result._20:=(one_minus_cosine * Axis.z * Axis.x) - (Axis.y * Sine);
   Result._21:=(one_minus_cosine * Axis.y * Axis.z) + (Axis.x * Sine);
   Result._22:=(one_minus_cosine * Sqr(Axis.z)) + Cosine;
   Result._23:=0;

   Result._30:=0;
   Result._31:=0;
   Result._32:=0;
   Result._33:=1;
end;

function MatrixTranslate(Vec: TVector): TMatrix4f;
begin
  Result:= MatrixIdentity;
  Result._30:= Vec.x;
  Result._31:= Vec.y;
  Result._32:= Vec.z;
end;

function MatrixScale(Vec: TVector): TMatrix4f;
begin
  Result:= MatrixIdentity;
  Result._00:= Vec.x;
  Result._11:= Vec.y;
  Result._22:= Vec.z;
//  Result._33:= 1.0;
end;
{
function MatrixDeterminant(const M: TAffineMatrix): Single;
begin
  Result:=  M[X, X] * (M[Y, Y] * M[Z, Z] - M[Z, Y] * M[Y, Z])
          - M[X, Y] * (M[Y, X] * M[Z, Z] - M[Z, X] * M[Y, Z])
          + M[X, Z] * (M[Y, X] * M[Z, Y] - M[Z, X] * M[Y, Y]);
end;

procedure InvertMatrix(var M : TMatrix);
var
   det : Single;
begin
   det:=MatrixDeterminant(M);
   if Abs(Det)<EPSILON then
      M:=IdentityHmgMatrix
   else begin
      AdjointMatrix(M);
      ScaleMatrix(M, 1/det);
   end;
end;}

function MatrixDet( const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single ): Single;
begin
  Result:= a1 * ( b2 * c3 - b3 * c2 )
         - b1 * ( a2 * c3 - a3 * c2 )
         + c1 * ( a2 * b3 - a3 * b2 );
end;

function MatrixDeterminant(Mat: TMatrix4f): Single;
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

function MatrixInverse( Mat: TMatrix4f; out MatInv: TMatrix4f ): Boolean;
//var
//  inv: array [0..15] of Double;
//  det: Double;
//  i: Integer;
var
  det, a1, a2, a3, a4, b1, b2, b3, b4,
  c1, c2, c3, c4, d1, d2, d3, d4: Single;
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
{  with ( Mat ) do
    begin
      inv[0]:= m[5]  * m[10] * m[15] -
               m[5]  * m[11] * m[14] -
               m[9]  * m[6]  * m[15] +
               m[9]  * m[7]  * m[14] +
               m[13] * m[6]  * m[11] -
               m[13] * m[7]  * m[10];

      inv[4]:= -m[4]  * m[10] * m[15] +
                m[4]  * m[11] * m[14] +
                m[8]  * m[6]  * m[15] -
                m[8]  * m[7]  * m[14] -
                m[12] * m[6]  * m[11] +
                m[12] * m[7]  * m[10];

      inv[8]:= m[4]  * m[9] * m[15] -
               m[4]  * m[11] * m[13] -
               m[8]  * m[5] * m[15] +
               m[8]  * m[7] * m[13] +
               m[12] * m[5] * m[11] -
               m[12] * m[7] * m[9];

      inv[12]:= -m[4]  * m[9] * m[14] +
                 m[4]  * m[10] * m[13] +
                 m[8]  * m[5] * m[14] -
                 m[8]  * m[6] * m[13] -
                 m[12] * m[5] * m[10] +
                 m[12] * m[6] * m[9];

      inv[1]:= -m[1]  * m[10] * m[15] +
                m[1]  * m[11] * m[14] +
                m[9]  * m[2] * m[15] -
                m[9]  * m[3] * m[14] -
                m[13] * m[2] * m[11] +
                m[13] * m[3] * m[10];

      inv[5]:= m[0]  * m[10] * m[15] -
               m[0]  * m[11] * m[14] -
               m[8]  * m[2] * m[15] +
               m[8]  * m[3] * m[14] +
               m[12] * m[2] * m[11] -
               m[12] * m[3] * m[10];

      inv[9]:= -m[0]  * m[9] * m[15] +
                m[0]  * m[11] * m[13] +
                m[8]  * m[1] * m[15] -
                m[8]  * m[3] * m[13] -
                m[12] * m[1] * m[11] +
                m[12] * m[3] * m[9];

      inv[13]:= m[0]  * m[9] * m[14] -
                m[0]  * m[10] * m[13] -
                m[8]  * m[1] * m[14] +
                m[8]  * m[2] * m[13] +
                m[12] * m[1] * m[10] -
                m[12] * m[2] * m[9];

      inv[2]:= m[1]  * m[6] * m[15] -
               m[1]  * m[7] * m[14] -
               m[5]  * m[2] * m[15] +
               m[5]  * m[3] * m[14] +
               m[13] * m[2] * m[7] -
               m[13] * m[3] * m[6];

      inv[6]:= -m[0]  * m[6] * m[15] +
                m[0]  * m[7] * m[14] +
                m[4]  * m[2] * m[15] -
                m[4]  * m[3] * m[14] -
                m[12] * m[2] * m[7] +
                m[12] * m[3] * m[6];

      inv[10]:= m[0]  * m[5] * m[15] -
                m[0]  * m[7] * m[13] -
                m[4]  * m[1] * m[15] +
                m[4]  * m[3] * m[13] +
                m[12] * m[1] * m[7] -
                m[12] * m[3] * m[5];

      inv[14]:= -m[0]  * m[5] * m[14] +
                 m[0]  * m[6] * m[13] +
                 m[4]  * m[1] * m[14] -
                 m[4]  * m[2] * m[13] -
                 m[12] * m[1] * m[6] +
                 m[12] * m[2] * m[5];

      inv[3]:= -m[1] * m[6] * m[11] +
                m[1] * m[7] * m[10] +
                m[5] * m[2] * m[11] -
                m[5] * m[3] * m[10] -
                m[9] * m[2] * m[7] +
                m[9] * m[3] * m[6];

      inv[7]:= m[0] * m[6] * m[11] -
               m[0] * m[7] * m[10] -
               m[4] * m[2] * m[11] +
               m[4] * m[3] * m[10] +
               m[8] * m[2] * m[7] -
               m[8] * m[3] * m[6];

      inv[11]:= -m[0] * m[5] * m[11] +
                 m[0] * m[7] * m[9] +
                 m[4] * m[1] * m[11] -
                 m[4] * m[3] * m[9] -
                 m[8] * m[1] * m[7] +
                 m[8] * m[3] * m[5];

      inv[15]:= m[0] * m[5] * m[10] -
                m[0] * m[6] * m[9] -
                m[4] * m[1] * m[10] +
                m[4] * m[2] * m[9] +
                m[8] * m[1] * m[6] -
                m[8] * m[2] * m[5];
//      det:= m[0] * inv[0] + m[1] * inv[4] + m[2] * inv[8] + m[3] * inv[12];
      det:= MatrixDeterminant( Mat );
    end;

  if (det = 0) then
    Result:= False
  else
    begin
      det:= 1.0 / det;

      for i:= 0 to 16 do
        MatInv.m[i]:= inv[i] * det;

      Result:= True;
    end;}
end;

function MatrixTranspose(Matrix: TMatrix4f): TMatrix4f;
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

function MatrixPerspectiveFOVLH( FOV, Aspect, Near, Far: Single ): TMatrix4f;
var
  w, h: Single;
begin
  h:= Cot( FOV / 2 );
  w:= h / Aspect;
  Result._00:= w; Result._01:= 0; Result._02:= 0;                          Result._03:= 0;
  Result._10:= 0; Result._11:= h; Result._12:= 0;                          Result._13:= 0;
  Result._20:= 0; Result._21:= 0; Result._22:= Far/( Far - Near );         Result._23:= 1;
  Result._30:= 0; Result._31:= 0; Result._32:= -Near * Far/( Far - Near ); Result._33:= 0;
end;

function MatrixPerspectiveFOVRH( FOV, Aspect, Near, Far: Single ): TMatrix4f;
var
  w, h: Single;
begin
  h:= Cot( FOV / 2 );
  w:= h / Aspect;
  Result._00:= w; Result._01:= 0; Result._02:= 0;                          Result._03:= 0;
  Result._10:= 0; Result._11:= h; Result._12:= 0;                          Result._13:= 0;
  Result._20:= 0; Result._21:= 0; Result._22:= Far/( Near - Far );         Result._23:= -1;
  Result._30:= 0; Result._31:= 0; Result._32:= Near * Far/( Near - Far );  Result._33:= 0;
end;

function MatrixOrthoLH( w, h, Near, Far: Single ): TMatrix4f;
begin
  Result._00:= 2/w; Result._01:= 0;   Result._02:= 0;                   Result._03:= 0;
  Result._10:= 0;   Result._11:= 2/h; Result._12:= 0;                   Result._13:= 0;
  Result._20:= 0;   Result._21:= 0;   Result._22:= 1/( Far - Near );    Result._23:= 0;
  Result._30:= 0;   Result._31:= 0;   Result._32:= Near/( Near - Far ); Result._33:= 1;
end;

function MatrixOrthoRH( w, h, Near, Far: Single ): TMatrix4f;
begin
  Result._00:= 2/w; Result._01:= 0;   Result._02:= 0;                   Result._03:= 0;
  Result._10:= 0;   Result._11:= 2/h; Result._12:= 0;                   Result._13:= 0;
  Result._20:= 0;   Result._21:= 0;   Result._22:= 1/( Near - Far );    Result._23:= 0;
  Result._30:= 0;   Result._31:= 0;   Result._32:= Near/( Near - Far ); Result._33:= 1;
end;

function MatrixLookAtLH( EyePt, LookAt, UpDir: TVector ): TMatrix4f;
var
  xa, ya, za: TVector;
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

function MatrixLookAtRH( EyePt, LookAt, UpDir: TVector ): TMatrix4f;
var
  xa, ya, za: TVector;
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




