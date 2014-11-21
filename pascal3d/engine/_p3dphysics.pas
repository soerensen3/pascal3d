unit p3dphysics;
interface
  uses
    Classes,
    p3dMath,
    LCLIntf,
    Newton;

  type
    REAL = Single; //Single Precision

    TPhysicsEngine = class;
    TDynamicBody = class;

    TOnForceAndTorqueEvent = procedure ( Sender: TDynamicBody; TimeStep: Single ) of Object;
    TBodyData = record
      Matrix: TMat4;
      Torque: TVec3;
      Omega: TVec3;
      Velocity: TVec3;
    end;

    { TStaticBody }

    TStaticBody = class abstract ( TPersistent )
      constructor Create( APhysicsEngine: TPhysicsEngine );
      destructor Destroy; override;
    end;

    { TDynamicBody }

    TDynamicBody = class abstract( TPersistent )
      private
        PhysicsEngine: TPhysicsEngine;
        FMass: Single;
        FOnForceAndTorque: TOnForceAndTorqueEvent;
        Backup: TBodyData;

        procedure SetMass( const Value: Single ); virtual;

      public
        Collision: NewtonCollision;
        Body: NewtonBody;

        constructor Create( APhysicsEngine: TPhysicsEngine );
        destructor Destroy; override;

        procedure SetPosition( x, y, z: Single ); overload;
        procedure SetPosition( Vec: TVec3 ); overload;
        function GetPosition(): TVec3;

        procedure SetRotation( mat: TMat3 );
        function GetMatrix(): TMat4;
        procedure SetMatrix( m: TMat4 );
        function GetRotationMatrix(): TMat4;

        procedure SetTorque( t: TVec3 );
        function GetTorque: TVec3;
        procedure AddTorque( t: TVec3 );
        procedure SetForce( t: TVec3 );
        function GetForce: TVec3;
        procedure AddForce( t: TVec3 );

        function GetOmega: TVec3;
        procedure SetOmega( v: TVec3 );
        function GetVelocity: TVec3;
        procedure SetVelocity( v: TVec3 );

        procedure SetScale( v: TVec3 );
        procedure GetScale( v: TVec3 );
        
        procedure BackupData;
        procedure RestoreData;

      published
        property Mass: Single read FMass write SetMass;
        property OnForceAndTorque: TOnForceAndTorqueEvent read FOnForceAndTorque write FOnForceAndTorque;
    end;

    { TBodyCube }

    TBodyCube = class( TDynamicBody )
      private
        FLengths: TVec3;

      public
        constructor Create( APhysicsEngine: TPhysicsEngine );

        procedure SetLengths( vec: TVec3 );
        function GetLengths: TVec3;
    end;

    TBodySphere = class( TDynamicBody )
      private
        FRadius: Float;//TVec3;

      public
        constructor Create( APhysicsEngine: TPhysicsEngine );

//        procedure SetRadius( v: TVec3 ); overload;
        procedure SetRadius( r: REAL ); //overload;
        function GetRadius: Float;
    end;

    { TBodyMesh }

    TBodyMesh = class( TDynamicBody )
      private
        FRadius: Single;
        procedure SetMass( const Value: Single ); override;

      public
        constructor Create( APhysicsEngine: TPhysicsEngine );

        procedure UpdateMesh( IB: TIntList; VB: TVec3List; ModMatrix: TMat4 );
    end;

    TPhysicsEngine = class( TPersistent )
      private
        TimeLastFrame: Integer;
        AccTimeSlice: Integer;

      public
        CollisionNull: NewtonCollision;
        World: NewtonWorld;

        constructor Create();
        destructor Destroy(); override;

        procedure Process( TimeStep: Single );
    end;
  procedure ForceAndTorqueCallback( const Body : NewtonBody; TimeStep : dFloat; ThreadIndex : Integer ); cdecl;

implementation

{ TBodyMesh }

procedure TBodyMesh.SetMass(const Value: Single);
begin
  //inherited SetMass(Value);
end;

constructor TBodyMesh.Create(APhysicsEngine: TPhysicsEngine);
begin
  inherited;
  //Mass:= 1.0;
end;

procedure TBodyMesh.UpdateMesh(IB: TIntList; VB: TVec3List; ModMatrix: TMat4);
var
  i: Integer;
  Pos: array [ 0..2 ] of TVec3;
begin
  if ( Assigned( Collision )) then
    NewtonDestroyCollision( Collision );

  Collision:= NewtonCreateTreeCollision( PhysicsEngine.World, 0 );
  NewtonTreeCollisionBeginBuild( Collision );

  for i:= 0 to IB.Count div 3 - 1 do
    begin
      Pos[ 0 ]:= vec3( vec4( VB[ IB[ i * 3 ]], 1 ) * ModMatrix );
      Pos[ 1 ]:= vec3( vec4( VB[ IB[ i * 3 + 1 ]], 1 ) * ModMatrix );
      Pos[ 2 ]:= vec3( vec4( VB[ IB[ i * 3 + 2 ]], 1 ) * ModMatrix );
      NewtonTreeCollisionAddFace( Collision, 3, @Pos, SizeOf( TVec3 ), 0 );
    end;

  NewtonTreeCollisionEndBuild( Collision, 1 );
  NewtonBodySetCollision( Body, Collision );
end;

{ TStaticBody }

constructor TStaticBody.Create(APhysicsEngine: TPhysicsEngine);
begin

end;

destructor TStaticBody.Destroy;
begin
  inherited Destroy;
end;


{ TPhysicsEngine }

constructor TPhysicsEngine.Create;
begin
  inherited;
  World:= NewtonCreate();
  CollisionNull:= NewtonCreateNull( World );
  TimeLastFrame:= GetTickCount();
end;

destructor TPhysicsEngine.Destroy;
begin
  NewtonDestroyAllBodies( World );
  NewtonDestroy( World );

  inherited;
end;

procedure TPhysicsEngine.Process( TimeStep: Single );
const
  Timing = 12;
begin
  NewtonInvalidateCache( World );
  AccTimeSlice  := AccTimeSlice + ( GetTickCount - TimeLastFrame );
  TimeLastFrame := GetTickCount;
  while ( AccTimeSlice > Timing ) do
    begin
      NewtonUpdate( World, ( Timing / 1000 ));
      AccTimeSlice:= AccTimeSlice - Timing;
    end;
end;

{ TDynamicBody }

procedure TDynamicBody.AddTorque( t: TVec3 );
begin
  NewtonBodyAddTorque( Body, @t );
end;

constructor TDynamicBody.Create( APhysicsEngine: TPhysicsEngine );
begin
  inherited Create;
  PhysicsEngine:= APhysicsEngine;
  Collision:= nil;
  Body:= NewtonCreateDynamicBody( PhysicsEngine.World, PhysicsEngine.CollisionNull, @Mat4Identity );
  NewtonBodySetUserData( Body, Self );
  NewtonBodySetForceAndTorqueCallback( Body, PNewtonApplyForceAndTorque( @ForceAndTorqueCallback ));
  FMass:= 1.0;
end;

destructor TDynamicBody.Destroy;
begin
  NewtonDestroyBody( Body );
  inherited;
end;

function TDynamicBody.GetTorque: TVec3;
begin
  NewtonBodyGetTorque( Body, @Result );
end;

function TDynamicBody.GetVelocity: TVec3;
begin
  NewtonBodyGetVelocity( Body, @Result );
end;

function TDynamicBody.GetPosition: TVec3;
var
  mat: TMat4;
begin
  NewtonBodyGetMatrix( Body, @mat );
  Result:= vec3( mat._30, mat._31, mat._32 );
end;

function TDynamicBody.GetRotationMatrix: TMat4;
begin
  NewtonBodyGetMatrix( Body, @Result );

  Result._30:= 0;
  Result._31:= 0;
  Result._32:= 0;
  Result._33:= 1;
end;

function TDynamicBody.GetMatrix: TMat4;
begin
  NewtonBodyGetMatrix( Body, @Result );
end;


procedure TDynamicBody.SetOmega(v: TVec3);
begin
  NewtonBodySetOmega( Body, @v );
end;

function TDynamicBody.GetOmega: TVec3;
begin
  NewtonBodyGetOmega( Body, @Result );
end;


procedure TDynamicBody.SetVelocity(v: TVec3);
begin
  NewtonBodySetVelocity( Body, @v );
end;

procedure TDynamicBody.SetScale(v: TVec3);
begin
  NewtonCollisionSetScale( Collision, v.x, v.y, v.z );
end;

procedure TDynamicBody.GetScale(v: TVec3);
begin
  NewtonCollisionGetScale( Collision, v.x, v.y, v.z );
end;

procedure TDynamicBody.SetPosition( Vec: TVec3 );
var
  m: TMat4;
begin
  NewtonBodyGetMatrix( Body, @m );
  m._30:= Vec.x;
  m._31:= Vec.y;
  m._32:= Vec.z;
  NewtonBodySetMatrix( Body, @m );
end;

procedure TDynamicBody.SetPosition( x, y, z: Single );
var
  m: TMat4;
begin
  NewtonBodyGetMatrix( Body, @m );
  m._30:= x;
  m._31:= y;
  m._32:= z;
  NewtonBodySetMatrix( Body, @m );
end;

procedure TDynamicBody.SetRotation( mat: TMat3 );
var
  m, m2: TMat4;
  p: TVec3;
begin
  NewtonBodyGetMatrix( Body, @m );
  m2:= mat4( mat );
  m2._31:= m._31;
  m2._32:= m._32;
  m2._33:= m._33;

  NewtonBodySetMatrix( Body, @m2 );
end;

procedure TDynamicBody.SetTorque( t: TVec3 );
begin
  NewtonBodySetTorque( Body, @t );
end;

{ TGeomBox }

constructor TBodyCube.Create( APhysicsEngine: TPhysicsEngine );
begin
  inherited;
  SetLengths( vec3( 1 ));
  Mass:= 1.0;
end;

function TBodyCube.GetLengths: TVec3;
begin
  Result:= FLengths;
end;

procedure TBodyCube.SetLengths( vec: TVec3 );
begin
  if ( Assigned( Collision )) then
    NewtonDestroyCollision( Collision );
  Collision:= NewtonCreateBox( PhysicsEngine.World, 2*vec.x, 2*vec.y, 2*vec.z, 0, nil );
  NewtonBodySetCollision( Body, Collision );
  FLengths:= vec;
end;

procedure ForceAndTorqueCallback( const Body : NewtonBody; TimeStep : Float; ThreadIndex : Integer ); cdecl;
var
  _Body: TDynamicBody;
begin
  _Body:= TDynamicBody( NewtonBodyGetUserData( Body ));
  if ( Assigned( _Body )) then
    if ( Assigned( _Body.FOnForceAndTorque )) then
      _Body.OnForceAndTorque( _Body, TimeStep );
end;


procedure TDynamicBody.SetMatrix(m: TMat4);
begin
  NewtonBodySetMatrix( Body, @m );
end;

procedure TDynamicBody.SetMass( const Value: Single );
begin
  FMass:= Value;
  NewtonBodySetMassProperties( Body, Mass, Collision );
end;

procedure TDynamicBody.BackupData;
begin
  Backup.Matrix:= GetMatrix;
  Backup.Torque:= GetTorque;
  Backup.Omega:= GetOmega;
  Backup.Velocity:= GetVelocity;
end;

procedure TDynamicBody.RestoreData;
begin
  SetMatrix( Backup.Matrix );
  SetTorque( Backup.Torque );
  SetOmega( Backup.Omega );
  SetVelocity( Backup.Velocity );
  SetMass( FMass );
  NewtonBodySetCollision( Body, Collision );
end;

procedure TDynamicBody.AddForce(t: TVec3);
begin
  NewtonBodyAddForce( Body, @t );
end;

function TDynamicBody.GetForce: TVec3;
begin
  NewtonBodyGetForce( Body, @Result );
end;

procedure TDynamicBody.SetForce(t: TVec3);
begin
  NewtonBodySetForce( Body, @t );
end;

{ TBodySphere }

constructor TBodySphere.Create(APhysicsEngine: TPhysicsEngine);
begin
  inherited;
  SetRadius( 1 );
end;

function TBodySphere.GetRadius: Float;
begin
  Result:= FRadius;
end;

procedure TBodySphere.SetRadius(r: REAL);
begin
  FRadius:= r;//vec3( r, r, r );
//  BackupData;
  if ( Assigned( Collision )) then
    NewtonDestroyCollision( Collision );
  Collision:= NewtonCreateSphere( PhysicsEngine.World, r, 0, nil );
  NewtonBodySetCollision( Body, Collision );
//  RestoreData;
end;
{
procedure TBodySphere.SetRadius(v: TVec3);
begin
  FRadius:= v;
  NewtonReleaseCollision( PhysicsEngine.World, Collision );
  Collision:= NewtonCreateSphere( PhysicsEngine.World, v.x, v.y, v.z, 0, nil );
  NewtonBodySetCollision( Body, Collision );
end;
}
{ TBodyMesh }
{
constructor TBodyMesh.Create(APhysicsEngine: TPhysicsEngine);
begin
  inherited;
  Collision:= NewtonCreateNull( PhysicsEngine.World );
  NewtonBodySetCollision( Body, Collision );
end;

procedure TBodyMesh.SetMass(const Value: Single);
begin
  inherited;
  NewtonBodySetMassMatrix( Body, FMass * 100, 1, 1, 1 );
end;

procedure TBodyMesh.UpdateMesh(IB: TIndexArray; VB: TVertexArray);
var
  i: Integer;
  v: array[ 0..2 ] of TVec3;
begin
  NewtonReleaseCollision( PhysicsEngine.World, Collision );
  Collision:= NewtonCreateTreeCollision( PhysicsEngine.World, 0 );
  NewtonTreeCollisionBeginBuild( Collision );

  for i:= 0 to IB.Count div 3 - 1 do
    begin
      v[ 0 ]:= VB[ IB[ i * 3 ]].vPos;
      v[ 1 ]:= VB[ IB[ i * 3 + 1 ]].vPos;
      v[ 2 ]:= VB[ IB[ i * 3 + 2 ]].vPos;
      NewtonTreeCollisionAddFace( Collision, 3, @v, SizeOf( TVec3 ), i );
    end;

  NewtonTreeCollisionEndBuild( Collision, 1 );
end;
}
end.