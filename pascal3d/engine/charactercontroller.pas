unit charactercontroller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math3D, RPhysics, Newton, scene;

type

  { TCharacterController }

  TCharacterController = class
    private
      FOnForceAndTorque: TOnForceAndTorqueEvent;
      FWalkSpeed: Single;
      fYawPitchRoll: tVec3;
      PhysicsEngine : TPhysicsEngine;
      Size          : TVec3;
      Movement      : TVec3;
      ExternalForce : TVec3;    // Is used to apply external forces, e.g. upon key press

      procedure ForceAndTorque( Sender: TDynamicBody; TimeStep: Single );

    public
      Body          : TBodySphere;
  //    Run           : Boolean;      // If true, velocity in the callback is scaled

      constructor Create( APhysicsEngine: TPhysicsEngine );
      destructor Destroy; override;

      procedure Walk( localAxis: TVec3; speed: Float );
      procedure WalkY( Negative: Boolean = False );
      procedure WalkZ( Negative: Boolean = False );
      procedure WalkX( Negative: Boolean = False );
      procedure Jump( Force: Single );
      procedure AddForce( V: TVec3 );
      procedure SetSize( Height: Float; Diameter: Float );
      procedure ApplyToCamera( Cam: tCamera );

      property OnForceAndTorque: TOnForceAndTorqueEvent read FOnForceAndTorque write FOnForceAndTorque;
      property YawPitchRoll: tVec3 read fYawPitchRoll write fYawPitchRoll;
      property WalkSpeed: Single read FWalkSpeed write FWalkSpeed;
   end;

implementation

{ TCharacterController }

constructor TCharacterController.Create(APhysicsEngine: TPhysicsEngine);
var
  UpDir: TVec3;
begin
  Size:= vec3( 1 );
  PhysicsEngine:= APhysicsEngine;

  Body:= TBodySphere.Create( APhysicsEngine );
  Body.OnForceAndTorque:= @ForceAndTorque;

  // Give it a realistic mass
  Body.Mass:= 1;
  // The player should not fall over, so we attach an up-vector joint to it.
  // This type of joint will make the player always stay up in the direction
  // that the joint was set to (in this case up on y)
  UpDir:= vec3( 0, 0, 1 );
  NewtonConstraintCreateUpVector( PhysicsEngine.World, @UpDir.X, Body.Body );
  NewtonBodySetLinearDamping( Body.Body, 1 );
  WalkSpeed:= 10;
end;

destructor TCharacterController.Destroy;
begin
  Body.Free;
  inherited Destroy;
end;

procedure TCharacterController.Walk( localAxis: TVec3; speed: Float );
var
  view: TMat4;
  cosx: ValReal;
  cosy: ValReal;
  cosz: ValReal;
  sinx: ValReal;
  siny: ValReal;
  sinz: ValReal;
begin
  with ( YawPitchRoll ) do
    begin
      cosx:= cos( deg2rad * X );
      cosy:= cos( deg2rad * Y );
      cosz:= cos( deg2rad * Z );
      sinx:= sin( deg2rad * X );
      siny:= sin( deg2rad * Y );
      sinz:= sin( deg2rad * Z );
    end;
  view:= mat4(   cosy * cosz,   cosx * sinz + sinx * siny * cosz, sinx * sinz - cosx * siny * cosz, 0,
               - cosy * sinz,   cosx * cosz - sinx * siny * sinz, sinx * cosz + cosx * siny * sinz, 0,
                 siny,        - sinx * cosy,                      cosx * cosy,                      0,
                 0,             0,                                0,                                1 );
  MatrixInverse( view, view );

  Movement+= vec3( vec4( localAxis, 1 ) * view ) * speed;
//  Movement+= vec3( view._10, view._11, view._12 ) * speed;
end;

procedure TCharacterController.WalkY(Negative: Boolean);
var
  Mult: Integer;
begin
  Mult:= Ord( Negative ) * ( -1 ) + Ord( not Negative ) * 1;
  Movement+= vec3( sin( - YawPitchRoll.Z * deg2rad ), cos( - YawPitchRoll.Z * deg2rad ), 0 ) * Mult;
end;

procedure TCharacterController.WalkZ(Negative: Boolean);
var
  Mult: Integer;
begin
  Mult:= Ord( Negative ) * ( -1 ) + Ord( not Negative ) * 1;
  Movement+= vec3( 0, 0, 1 * Mult );
end;

procedure TCharacterController.WalkX(Negative: Boolean);
var
  Mult: Integer;
begin
  Mult:= Ord( Negative ) * ( -1 ) + Ord( not Negative ) * 1;
  Movement+= vec3( sin( - YawPitchRoll.Z * deg2rad + PI / 2 ), cos( - YawPitchRoll.Z * deg2rad + PI / 2 ), 0 ) * Mult;
end;

procedure TCharacterController.Jump(Force: Single);
begin

end;

procedure TCharacterController.AddForce(V: TVec3);
begin
  ExternalForce += V;
end;

procedure TCharacterController.SetSize(Height: Float; Diameter: Float);
begin
  Size:= vec3( Diameter, Diameter, Height );
  Body.SetScale( Size );
end;

procedure TCharacterController.ApplyToCamera(Cam: tCamera);
var
  mat: TMat4;
begin
  Cam.YawPitchRoll:= YawPitchRoll;
  NewtonBodyGetMatrix( Body.Body, @mat );
  Cam.Position:= vec3( mat._30, mat._31, mat._32 + Size.Z / 2 );
  Cam.UpdateMatrices;
end;


procedure TCharacterController.ForceAndTorque(Sender: TDynamicBody;
  TimeStep: Single);
var
  Velocity: TVec3;
  GoalVelocity: TVec3;
  Accel: TVec3;
  L: Float;
begin
  Movement.Normalize;

  Velocity:= Body.GetVelocity;
  GoalVelocity:= Movement * WalkSpeed;

  Accel.X:= (( GoalVelocity.X - Velocity.X ) * TimeStep ) * 100;
  Accel.Y:= (( GoalVelocity.Y - Velocity.Y ) * TimeStep ) * 100;
  Accel.Z:= 0;

  L:= Accel.GetDist;
  if ( L > 200 ) then
    Accel /= L * 200;

  Sender.AddForce( Accel * Body.Mass );
  Sender.AddForce( ExternalForce );
  ExternalForce:= vec3( 0 );
  Movement:= vec3( 0 );
  if ( Assigned( OnForceAndTorque )) then
    OnForceAndTorque( Sender, TimeStep );
end;

end.

