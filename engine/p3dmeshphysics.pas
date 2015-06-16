unit p3dmeshphysics;

interface
  uses
    RMesh,
    Newton,
    RPhysics,
    RBase,
    Math;

  type

    TGeomKind = ( gkUnknown, gkBox, gkSphere, gkMesh );
    TMeshPhysics = class( TMesh )
      private
        FPhysicsEngine: TPhysicsEngine;
        FGeomKind: TGeomKind;
        FVisible: Boolean;
        FCenter: TCVector;
        FMaterial: TMaterial;

        procedure SetGeomKind( const Value: TGeomKind );
        procedure GeomKindChanged;
        procedure SetMass(const Value: Single);

        procedure Set_vPos( const Value: TCVector ); override;
        procedure Set_vRot( const Value: TCVector ); override;
        procedure Set_vScale( const Value: TCVector ); override;

        function Get_vPos: TCVector; override;
        function Get_vRot: TCVector; override;
        function GetMass: Single;
        procedure RenderCallback;

      public
        Body: TBody;

        constructor Create( AOwner: TBaseObject; AEngine: TEngine; APhysicsEngine: TPhysicsEngine );
        destructor Destroy; override;

        procedure Process; override;
        procedure Render; override;

        procedure SetPos( Pos: TVector );
        procedure SetRot( Rot: TVector );
        function GetPos: TVector;

        procedure GeomChanged;

        function GetBBox: TBBox;

      published
        property GeomKind: TGeomKind read FGeomKind write SetGeomKind;
        property Mass: Single read GetMass write SetMass;
        property Visible: Boolean read FVisible write FVisible;
        property Center: TCVector read FCenter write FCenter;
        property Material: TMaterial read FMaterial write FMaterial;
    end;

implementation

{ TMesh_Physic }

constructor TMeshPhysics.Create( AOwner: TBaseObject; AEngine: TEngine;
  APhysicsEngine: TPhysicsEngine );
begin
  inherited Create( AOwner, AEngine );
  FPhysicsEngine:= APhysicsEngine;
  FCenter:= TCVector.Create;
  GeomKind:= gkBox;
  Mass:= 1.0;
  FVisible:= True;
end;

destructor TMeshPhysics.Destroy;
begin
  Body.Free;
  FCenter.Free;
  inherited;
end;

procedure TMeshPhysics.GeomKindChanged;
var
  BBox: TBBox;
begin
//  if ( Name <> 'Mesh_Physic1' ) then
//    exit;
  case ( FGeomKind ) of
    gkBox:
      begin
        BBox:= GetBBox;

        TBodyBox( Body ).SetLengths( BBox.vMin.x - BBox.vMax.x, BBox.vMin.y - BBox.vMax.y, BBox.vMin.z - BBox.vMax.z );
        Center.Vec:= BBox.vCenter;
      end;
    gkSphere:
      begin
        BBox:= GetBBox;

        TBodySphere( Body ).SetRadius( Max( BBox.vMin.x - BBox.vMax.x, Max( BBox.vMin.y - BBox.vMax.y, BBox.vMin.z - BBox.vMax.z )));
        Center.Vec:= BBox.vCenter;
      end;
    gkMesh:
      begin
        BBox:= GetBBox;

        TBodyMesh( Body ).UpdateMesh( Indices, Vertices );
        Center.Vec:= BBox.vCenter;
      end;
  end;
end;

procedure TMeshPhysics.SetMass( const Value: Single );
var
  m : Single;
  BBox: TBBox;
begin
  Body.Mass:= Value;
end;

procedure TMeshPhysics.SetGeomKind( const Value: TGeomKind );
var
  restore: Boolean;
begin
{  if ( Name <> 'Mesh_Physic1' ) then
    exit;}
  if ( FGeomKind = Value ) then
    exit;
  FGeomKind:= Value;
  if ( not Assigned( Body )) then
    case ( Value ) of
      gkBox: Body:= TBodyBox.Create( FPhysicsEngine );
      gkSphere: Body:= TBodySphere.Create( FPhysicsEngine );
      gkMesh: Body:= TBodyMesh.Create( FPhysicsEngine );
    else
      exit;
    end;

{  if ( Assigned( Body )) then
    begin
      restore:= True;
      Body.BackupData;
      Body.Free;
    end
  else
    restore:= False;
  case ( Value ) of
    gkBox: Body:= TBodyBox.Create( FPhysicsEngine );
    gkSphere: Body:= TBodySphere.Create( FPhysicsEngine );
    gkMesh: Body:= TBody.Create( FPhysicsEngine );
  else
    exit;
  end;
  if ( restore ) then
    Body.RestoreData;}
  GeomKindChanged;
end;

function TMeshPhysics.GetBBox: TBBox;
var
  i: Integer;
begin
  if ( Vertices.Count = 0 ) then
    exit;
  Result.vMin:= Vertices.vPos[ 0 ];
  Result.vMax:= Vertices.vPos[ 0 ];
  for i:= 1 to Vertices.Count - 1 do
    begin
      if ( Result.vMin.x > Vertices.vPos[ i ].x ) then
        Result.vMin.x:= Vertices.vPos[ i ].x;
      if ( Result.vMax.x < Vertices.vPos[ i ].x ) then
        Result.vMax.x:= Vertices.vPos[ i ].x;

      if ( Result.vMin.y > Vertices.vPos[ i ].y ) then
        Result.vMin.y:= Vertices.vPos[ i ].y;
      if ( Result.vMax.y < Vertices.vPos[ i ].y ) then
        Result.vMax.y:= Vertices.vPos[ i ].y;

      if ( Result.vMin.z > Vertices.vPos[ i ].z ) then
        Result.vMin.z:= Vertices.vPos[ i ].z;
      if ( Result.vMax.z < Vertices.vPos[ i ].z ) then
        Result.vMax.z:= Vertices.vPos[ i ].z;
    end;
  Result.vCenter:= VecMul( VecAdd( Result.vMin, Result.vMax ), 0.5 );
end;

procedure TMeshPhysics.Process;
begin
  inherited;
{  if ( Name <> 'Mesh_Physic1' ) then
    exit;}
  if ( Body <> nil ) then
    FvPos.Vec:= Body.GetPosition;
end;

procedure TMeshPhysics.Render;
var
  m, m2: TMatrix;
begin
  inherited;
{  if ( Name <> 'Mesh_Physic1' ) then
      m:= MatrixIdentity
    else
      begin}
  m:= Body.GetMatrix;
  FvPos.Vec:= Body.GetPosition;
//      end;
  if ( FVisible ) then
    begin
//      m2:= MatrixMul( MatrixMul( MatrixScale( vScale.Vec ), m ), MatrixTranslate( vPos.Vec ));
    //  m:= MatrixMul( m, MatrixTranslate( vPos.Vec ));
      Engine.DisplayDriver.SetMatrixWorld( m );
      if ( Assigned( Material )) then
        Material.Paint( RenderCallback );
    end;
end;

procedure TMeshPhysics.SetPos( Pos: TVector );
begin
  if ( Body <> nil ) then
    Body.SetPosition( Vector( Pos.x, Pos.y, Pos.z ));
  FvPos.Vec:= Pos;
  if ( IsNan( FvPos.x )) or ( IsNan( FvPos.y )) or ( IsNan( FvPos.z )) then
    FvPos.Vec:= VectorZero;  
end;

procedure TMeshPhysics.SetRot( Rot: TVector );
begin
  if ( Body <> nil ) then
    Body.SetRotation( MatrixRot( Vector( Rot.x, Rot.y, Rot.z )));
end;

procedure TMeshPhysics.GeomChanged;
begin
  GeomKindChanged;
end;

function TMeshPhysics.GetPos: TVector;
begin
{  if ( Name <> 'Mesh_Physic1' ) then
    begin
      Result:= FvPos.Vec;
      exit;
    end;}
  Result:= Body.GetPosition;
  if ( IsNan( Result.x )) or ( IsNan( Result.y )) or ( IsNan( Result.z )) then
    Result:= VectorZero;
end;

function TMeshPhysics.Get_vPos: TCVector;
begin
{  if ( Name = 'Mesh_Physic1' ) then
    begin}
  FvPos.Vec:= Body.GetPosition;
  if ( IsNan( FvPos.x )) or ( IsNan( FvPos.y )) or ( IsNan( FvPos.z )) then
    FvPos.Vec:= VectorZero;
//    end;
  Result:= FvPos;
end;

function TMeshPhysics.Get_vRot: TCVector;
begin
  FvRot.Vec:= Vector( 0, 0, 0 );
  Result:= FvRot;
end;


procedure TMeshPhysics.Set_vPos( const Value: TCVector );
begin
//  if ( Name = 'Mesh_Physic1' ) then
  if ( Body <> nil ) then
    Body.SetPosition( Value.Vec );
  FvPos:= Value;
end;

procedure TMeshPhysics.Set_vRot(const Value: TCVector);
begin

end;

procedure TMeshPhysics.Set_vScale(const Value: TCVector);
begin
  inherited;

end;

function TMeshPhysics.GetMass: Single;
begin
  Result:= Body.Mass;
end;

procedure TMeshPhysics.RenderCallback;
begin
  JustRender;
//  RenderTransformed( MatrixIdentity );
end;

initialization
  Rev_RegisterClass( TMeshPhysics );


end.
