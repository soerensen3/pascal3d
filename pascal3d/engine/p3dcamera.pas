unit p3dcamera;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, Math, p3dMath;

type

  { TCam }

  TViewFrustum = array [ 0..4 ] of TVec3;

  TCam = class( TPersistent )
    private
      FmatProj: TMat4;
      FmatView: TMat4;
      FmatView3x3: TMat3;
      FNear,
      FAspect,
      FFar,
      FFOV: Single;
      FLimitRot: Boolean;
      FLimitRotAngle: Single;
      FvDir: TVec3;
      FOrthogonal: Boolean;
      FvPos: TVec3;
      FvRot: TVec3;

      procedure SetAspect( const Value: Single );
      procedure SetFar( const Value: Single );
      procedure SetNear( const Value: Single );
      procedure SetFOV( const Value: Single );
      procedure SetOrthogonal( const Value: Boolean );
      procedure SetLimitRot( const Value: Boolean );

    public
      Frustum: TViewFrustum;

      constructor Create;

      procedure Update; virtual;
      procedure UpdateProj; virtual;
      procedure MoveTo( vPoint: TVec3 );
      procedure Move( vMov: TVec3 );
      procedure Rotate( vRotation: TVec3 );
      procedure AxisMove( vMov: TVec3 );
      procedure CalcFrustum;
      procedure LookAt( vPoint: TVec3 );
      procedure SetDistanceTo( vPoint: TVec3; Distance: Single );

      property vDir: TVec3 read FvDir write FvDir;
      property vRot: TVec3 read FvRot write FvRot;
      property vPos: TVec3 read FvPos write FvPos;
      property matView: TMat4 read FmatView write FmatView;
      property matView3x3: TMat3 read FmatView3x3 write FmatView3x3;
      property matProj: TMat4 read FmatProj write FmatProj;

    published
      property Near: Single read FNear write SetNear;
      property Far: Single read FFar write SetFar;
      property Aspect: Single read FAspect write SetAspect;
      property FOV: Single read FFOV write SetFOV;
      property LimitRotation: Boolean read FLimitRot write SetLimitRot;
      property LimitRotAngle: Single read FLimitRotAngle write FLimitRotAngle;
  end;


implementation

{ TCam }

procedure TCam.SetAspect(const Value: Single);
begin
  FAspect:= Value;
  UpdateProj;
end;

procedure TCam.SetFar(const Value: Single);
begin
  FFar:= Value;
  UpdateProj;
end;

procedure TCam.SetNear(const Value: Single);
begin
  FNear:= Value;
  UpdateProj;
end;

procedure TCam.SetFOV(const Value: Single);
begin
  FFOV:= Value;
  UpdateProj;
end;

procedure TCam.SetOrthogonal(const Value: Boolean);
begin
  FOrthogonal:= Value;
  UpdateProj;
end;

procedure TCam.SetLimitRot(const Value: Boolean);
begin
  FLimitRot:= Value;
end;

constructor TCam.Create;
begin

end;

procedure TCam.Update;
var
  vRight,
  vUp{,
  vDir}: TVec3;

  fSinRoll, fCosRoll,
  fSinYaw, fCosYaw,
  fSinPitch, fCosPitch: Single;
begin
  fSinRoll:= Sin( deg2rad * vRot.z );
  fCosRoll:= Cos( deg2rad * vRot.z );
  fSinYaw:= Sin( deg2rad * vRot.y );
  fCosYaw:= Cos( deg2rad * vRot.y );
  fSinPitch:= Sin( deg2rad * vRot.x );
  fCosPitch:= Cos( deg2rad * vRot.x );

  vRight.x:= fCosYaw * fCosRoll + fSinYaw * fSinPitch * fSinRoll;
  vRight.y:= fSinRoll * fCosPitch;
  vRight.z:= fCosYaw * fSinPitch * fSinRoll - fSinYaw * fCosRoll;

  vUp.x:= fSinYaw * fSinPitch * fCosRoll - fCosYaw * fSinRoll;
  vUp.y:= fCosRoll * fCosPitch;
  vUp.z:= fSinRoll * fSinYaw + fCosRoll * fCosYaw * fSinPitch;

  vDir.x:= fCosPitch * fSinYaw;
  vDir.y:= -fSinPitch;
  vDir.z:= fCosPitch * fCosYaw;

{  if ( Orthogonal ) then
    begin
      D3DXVec3Normalize( _vPos, vDir );
      _vPos.X:= vPos.X * _vPos.X;
      _vPos.Y:= vPos.Y * _vPos.Y;
      _vPos.Z:= vPos.Z * _vPos.Z;
    end
  else
    _vPos:= vPos.Vec;}

  matView._00:= vRight.x;
  matView._10:= vRight.y;
  matView._20:= vRight.z;
  matView._30:= -dot( vPos, vRight );
  matView._01:= vUp.x;
  matView._11:= vUp.y;
  matView._21:= vUp.z;
  matView._31:= -dot( vPos, vUp );
  matView._02:= vDir.x;
  matView._12:= vDir.y;
  matView._22:= vDir.z;
  matView._32:= -dot( vPos, vDir );
  matView._03:= 0.0;
  matView._13:= 0.0;
  matView._23:= 0.0;
  matView._33:= 1.0;

  matView3x3._00:= vRight.X;
  matView3x3._10:= vRight.Y;
  matView3x3._20:= vRight.Z;

  matView3x3._01:= vUp.X;
  matView3x3._11:= vUp.Y;
  matView3x3._21:= vUp.Z;

  matView3x3._02:= vDir.X;
  matView3x3._12:= vDir.Y;
  matView3x3._22:= vDir.Z;

//  matView:= MatrixMul( MatrixTranslate( vPos.Vec ), MatrixRot( vRot.Vec ));

//  Engine.DisplayDriver.vCamRot:= vRot.Vec;
  CalcFrustum;
//  Engine.DisplayDriver
//  Engine.d3ddev8.SetTransform( D3DTS_VIEW, matView );
end;

procedure TCam.UpdateProj;
begin
{  if ( FOrthogonal ) then
    begin
      if ( OrthoWidth > OrthoHeight ) then
        begin
          SX:= 1;
          SY:= OrthoHeight / OrthoWidth;
        end
      else
        begin
          SX:= OrthoWidth / OrthoHeight;
          SY:= 1;
        end;
      D3DXMatrixOrthoLH( matProj, SX, SY, Near, Far );
      D3DXMatrixScaling( Scal, FOV, FOV, FOV );
      D3DXMatrixMultiply( matProj, matProj, Scal );
    end
  else}
    matProj:= mat4perspectiveFOVLH( FOV, Aspect, Near, Far );

//  Engine.DisplayDriver.vCamRot:=
//    Vector( DegToRad( vRot.Vec.x ), DegToRad( vRot.Vec.y ), DegToRad( vRot.Vec.z ));
{  Engine.DisplayDriver.FOV:= FOV;
  Engine.DisplayDriver.Aspect:= Aspect;
  Engine.DisplayDriver.Near:= Near;
  Engine.DisplayDriver.Far:= Far;}
  CalcFrustum;
end;

procedure TCam.MoveTo( vPoint: TVec3 );
begin
  vPos:= vPoint;
end;

procedure TCam.Move( vMov: TVec3 );
begin
  FvPos+= vMov;
end;

procedure TCam.Rotate(vRotation: TVec3);
begin
  vRot:= vRot + vRotation;
  if ( LimitRotation ) then
    vRot.X:= Max( -LimitRotAngle, Min( LimitRotAngle, vRot.X ));
  Update;
end;

procedure TCam.AxisMove(vMov: TVec3);
var
  vNew: TVec3;
  v_Dir: TVec3;
begin
{  v_Dir:=
  vNew:= VecAdd( vPos.Vec, vDir );

  if (( not Collision ) or ( not Engine.ObjectEnvironment.CollideLine( vPos.Vec, vNew, _Plane, _Intersection, ctBoth ))) then
    Move( vDir )
  else
    begin
      _norm:= _Plane.vNormal;
      GetIntersection( vPos.Vec, vNew, _Plane, _hit, fPercent );
//      _norm.Mul( 0.25 );
//      vNew.AddV(_norm );
//      vNew.SubV( vPos.Vec );
//      Move( vNew );
//      vPos:= _hit;
    end;
  }
  Update;
end;

procedure TCam.CalcFrustum;
var
  ComboMatrix: TMat4;
begin
  ComboMatrix:= matView * matProj;

  Frustum[ 0 ].x:= -( comboMatrix._03 - comboMatrix._01 );
  Frustum[ 0 ].y:= -( comboMatrix._13 - comboMatrix._11 );
  Frustum[ 0 ].z:= -( comboMatrix._23 - comboMatrix._21 );

  Frustum[ 1 ].x:= -( comboMatrix._03 - comboMatrix._00 );
  Frustum[ 1 ].y:= -( comboMatrix._13 - comboMatrix._10 );
  Frustum[ 1 ].z:= -( comboMatrix._23 - comboMatrix._20 );

  Frustum[ 2 ].x:= -( comboMatrix._03 + comboMatrix._01 );
  Frustum[ 2 ].y:= -( comboMatrix._13 + comboMatrix._11 );
  Frustum[ 2 ].z:= -( comboMatrix._23 + comboMatrix._21 );

  Frustum[ 3 ].x:= -( comboMatrix._03 + comboMatrix._00 );
  Frustum[ 3 ].y:= -( comboMatrix._13 + comboMatrix._10 );
  Frustum[ 3 ].z:= -( comboMatrix._23 + comboMatrix._20 );

  Frustum[ 4 ].x:= -( comboMatrix._03 + comboMatrix._02 );
  Frustum[ 4 ].y:= -( comboMatrix._13 + comboMatrix._12 );
  Frustum[ 4 ].z:= -( comboMatrix._23 + comboMatrix._22 );
end;

procedure TCam.LookAt(vPoint: TVec3);
var
  Vec: TVec3;
begin
//  Vec:= VecGetAngle( VecSub( vPos.Vec, vPoint ));
//  vRot.Vec:= Vector( RadToDeg( Vec.x ), RadToDeg( Vec.y ), RadToDeg( Vec.z ));
end;

procedure TCam.SetDistanceTo(vPoint: TVec3; Distance: Single);
begin

end;

end.

