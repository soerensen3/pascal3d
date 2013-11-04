unit Math3D;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math;

const
  deg2rad = 0.017453292;
  epsilon = 1e-40;

type
  Float = Real;

  { TVector }
  TVector = packed object
    private
      procedure SetCoord(AIndex: Integer; AValue: Float);

    protected
      FX: Float;
      FY: Float;
      FZ: Float;

    public
      function GetDist: Float;
      function Normalize: TVector;
      function CrossProduct( v: TVector ): TVector;
      function DotProduct( v: TVector ): Float;
      function GetBiggestComp: Integer;

      function RotateRad( Axis: TVector; Angle: Float ): TVector;
      function RotateDeg( Axis: TVector; Angle: Float ): TVector;
//      function Transform( Matrix: TMatrix ): TVector;
      function Invert: TVector;
      function Ptr: PFloat;
//      function Mirror( Plane: pPlane ): TVector;

      function Compare( v: TVector ): Boolean;

    published
      property X: Float index 0 read FX write SetCoord;
      property Y: Float index 1 read FY write SetCoord;
      property Z: Float index 2 read FZ write SetCoord;
  end;

  { TMatrix4f }

  TMatrix4f = packed object
    private
      function GetIndex( index: Integer ): Float; inline;
      function GetMField(AIndex: Integer): Float; inline;
      procedure SetIndex( index: Integer ; AValue: Float);
      procedure SetMField(AIndex: Integer; AValue: Float);

    public
      m: array [0..15] of Float;

      property _00: Float index 0  read GetMField write SetMField;
      property _01: Float index 1  read GetMField write SetMField;
      property _02: Float index 2  read GetMField write SetMField;
      property _03: Float index 3  read GetMField write SetMField;
      property _10: Float index 4  read GetMField write SetMField;
      property _11: Float index 5  read GetMField write SetMField;
      property _12: Float index 6  read GetMField write SetMField;
      property _13: Float index 7  read GetMField write SetMField;
      property _20: Float index 8  read GetMField write SetMField;
      property _21: Float index 9  read GetMField write SetMField;
      property _22: Float index 10 read GetMField write SetMField;
      property _23: Float index 11 read GetMField write SetMField;
      property _30: Float index 12 read GetMField write SetMField;
      property _31: Float index 13 read GetMField write SetMField;
      property _32: Float index 14 read GetMField write SetMField;
      property _33: Float index 15 read GetMField write SetMField;

      property _m[ index: Integer ]: Float read GetIndex write SetIndex; default;
  end;

  operator + ( a, b: TVector ) c: TVector; inline;
  operator - ( a, b: TVector ) c: TVector; inline;
  operator * ( a, b: TVector ) c: Float; inline;
  operator * ( a: TVector; s: Float ) c: TVector; inline;
  operator * ( s: Float; b: TVector ) c: TVector; inline;
  operator / ( a, b: TVector ) c: TVector; inline;
  operator / ( a: TVector; s: Float ) c: TVector; inline;
  operator * ( a: TMatrix4f; b: TMatrix4f ) c: TMatrix4f; inline;
  operator * ( a: TVector; b: TMatrix4f ) c: TVector; inline;
  operator * ( a: TMatrix4f; b: TVector ) c: TVector; inline;
  operator * ( a: TMatrix4f; b: Float ) c: TMatrix4f; inline;
  operator - ( a: TVector ) c: TVector; inline;

  function Vector( x, y, z: Float ): TVector;
  function VecCrossProduct( v1: TVector; v2: TVector ): TVector;
  function VecDotProduct( v1: TVector; v2: TVector ): Float;
  function VecNormalize( Vec: TVector ): TVector;
//  function VecMirror( Vec: TVector; Plane: TPlane ): TVector;
  function VecInvert( Vec: TVector ): TVector;
  function VecTransform( Vec: TVector; Matrix: TMatrix4f ): TVector; //inline;
  function VecUnproject( Vec: TVector; matWorld, matView, matProj: TMatrix4f; Width, Height: Integer; out vDirection: TVector ): TVector;
  function Max( Vec1: TVector; Vec2: TVector ): TVector; overload;
  function Min( Vec1: TVector; Vec2: TVector ): TVector; overload;
  function Max( Vec1: TVector; s: Single ): TVector; overload;
  function Min( Vec1: TVector; s: Single ): TVector; overload;

  function MatrixRot( Axis: TVector; Angle: Float ): TMatrix4f; overload;
  function MatrixTranslate( Vec: TVector ): TMatrix4f;
  function MatrixScale( Vec: TVector ): TMatrix4f;
//  function MatrixMirror( Plane: TPlane ): TMatrix;
  function MatrixDeterminant( Mat: TMatrix4f ): Float;
  function MatrixInverse( Mat: TMatrix4f; out MatInv: TMatrix4f ): Boolean;
  function MatrixTranspose( Matrix: TMatrix4f ): TMatrix4f;
  function MatrixPerspectiveFOVLH( FOV, Aspect, Near, Far: Float ): TMatrix4f;
  function MatrixPerspectiveFOVRH( FOV, Aspect, Near, Far: Float ): TMatrix4f;
  function MatrixOrthoLH( w, h, Near, Far: Float ): TMatrix4f;
  function MatrixOrthoRH( w, h, Near, Far: Float ): TMatrix4f;
//  function MatrixCubeMap( Face: Cardinal ): TMatrix4f;
  function MatrixLookAtLH( EyePt, LookAt, UpDir: TVector ): TMatrix4f;
  function MatrixLookAtRH( EyePt, LookAt, UpDir: TVector ): TMatrix4f;


const
  MatrixIdentity: TMatrix4f = ( m: ( 1.0, 0.0, 0.0, 0.0,
                                     0.0, 1.0, 0.0, 0.0,
                                     0.0, 0.0, 1.0, 0.0,
                                     0.0, 0.0, 0.0, 1.0));
  VecZero: TVector = ( FX: 0; FY: 0; FZ: 0 );
  VecOne: TVector = ( FX: 1; FY: 1; FZ: 1 );

  Axis_X: TVector = ( FX: 1.0; FY: 0.0; FZ: 0.0 );
  Axis_Y: TVector = ( FX: 0.0; FY: 1.0; FZ: 0.0 );
  Axis_Z: TVector = ( FX: 0.0; FY: 0.0; FZ: 1.0 );
  Axis_NX: TVector = ( FX: -1.0; FY: 0.0; FZ: 0.0 );
  Axis_NY: TVector = ( FX: 0.0; FY: -1.0; FZ: 0.0 );
  Axis_NZ: TVector = ( FX: 0.0; FY: 0.0; FZ: -1.0 );

implementation


function Vector( x, y, z: Float ): TVector;
begin
  Result.x:= x;
  Result.y:= y;
  Result.z:= z;
end;

{ TMatrix4f }

function TMatrix4f.GetIndex( index: Integer ): Float;
begin
  Result:= m[ index ];
end;

function TMatrix4f.GetMField(AIndex: Integer): Float;
begin
  Result:= PFloat( @m )[ AIndex ];
end;

procedure TMatrix4f.SetIndex( index: Integer ; AValue: Float);
begin
  m[ index ]:= AValue;
end;

procedure TMatrix4f.SetMField(AIndex: Integer; AValue: Float);
begin
  PFloat( @m )[ AIndex ]:= AValue;
end;

{$I math3d_vector.inc}
{$I math3d_matrix.inc}


end.

