//p3dmodel_resource.inc

{$IFDEF INTERFACE}

type
  { TP3DCamera }

  TP3DCameraHandedness = ( p3dchLeft, p3dchRight );
  TP3DCamera = class ( TP3DDataBlock )
    private
      FAspect: Single;
      FFar: Single;
      FForward: TVec3;
      FHandedness: TP3DCameraHandedness;
      FLeft: TVec3;
      FMatNormal: TMat3;
      FNear: Single;
      FPosition: TVec3;
      FProj: TMat4;
      FUp: TVec3;
      FView: TMat4;
      FYawPitchRoll: TVec3;

    public
      procedure UpdateMatrices;
      procedure PassToShader( world: TMat4 );
      procedure LoadFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil ); override;

      constructor Create( const AData: TP3DData = nil );
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil );

      property Position: TVec3 read FPosition write FPosition;
      property Forward: TVec3 read FForward;
      property Up: TVec3 read FUp;
      property Left: TVec3 read FLeft;
      property YawPitchRoll: TVec3 read FYawPitchRoll write FYawPitchRoll;
      property View: TMat4 read FView write FView;
      property MatNormal: TMat3 read FMatNormal write FMatNormal;
      property Proj: TMat4 read FProj write FProj;

    published
      property Handedness: TP3DCameraHandedness read fHandedness write fHandedness;
      property Near: Single read FNear write FNear;
      property Far: Single read FFar write FFar;
      property Aspect: Single read FAspect write FAspect;
  end;
  TP3DCameraList = specialize gP3DCustomDatablockList < TP3DCamera >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DCamera }

procedure TP3DCamera.UpdateMatrices;
var
  _mdlview: TMat4;
  cosx: ValReal;
  cosy: ValReal;
  cosz: ValReal;
  sinx: ValReal;
  siny: ValReal;
  sinz: ValReal;
begin
  View:= mat4translate( vec4( -Position, 1.0 ));
{  _mdlview:= Mat4Rot( vec3_Axis_PZ, deg2rad* YawPitchRoll.z );
  _mdlview*= Mat4Rot( vec3_Axis_PX, deg2rad* YawPitchRoll.x );
  _mdlview*= Mat4Rot( vec3_Axis_PY, deg2rad* YawPitchRoll.y );}

  //  http://stackoverflow.com/questions/23009549/roll-pitch-yaw-calculation
{  _mdlview:= Mat4Rot( vec3_Axis_PZ, deg2rad* YawPitchRoll.z ); // Yaw
  _mdlview*= Mat4Rot( vec3_Axis_PX, deg2rad* YawPitchRoll.x + PI / 2 ); // Roll
  _mdlview*= Mat4Rot( vec3_Axis_PY, deg2rad* YawPitchRoll.y ); // Pitch}
  with ( YawPitchRoll ) do
    begin
      cosx:= cos( -deg2rad * X );
      cosy:= cos( -deg2rad * Y );
      cosz:= cos( -deg2rad * Z );
      sinx:= sin( -deg2rad * X );
      siny:= sin( -deg2rad * Y );
      sinz:= sin( -deg2rad * Z );
    end;
  _mdlview:= mat4(   cosy * cosz,   cosx * sinz + sinx * siny * cosz, sinx * sinz - cosx * siny * cosz, 0,
                   - cosy * sinz,   cosx * cosz - sinx * siny * sinz, sinx * cosz + cosx * siny * sinz, 0,
                     siny,        - sinx * cosy,                      cosx * cosy,                      0,
                     0,             0,                                0,                                1 );
  MatNormal:= mat3( _mdlview );
  View:= View * _mdlview;

  FLeft:= vec3( MatNormal._00, MatNormal._01, MatNormal._02 );
  FUp:= vec3( MatNormal._10, MatNormal._11, MatNormal._12 );
  FForward:= vec3( MatNormal._20, MatNormal._21, MatNormal._22 );

  case Handedness of
    p3dchLeft: proj:= mat4perspectiveFOVLH( deg2rad* 90, Aspect, Near, Far );
    p3dchRight: proj:= mat4perspectiveFOVRH( deg2rad* 90, Aspect, Near, Far );
  end;
end;

procedure TP3DCamera.PassToShader(world: TMat4);
var
  mnorm: TMat4;
begin
  if ( Assigned( ActShad )) then
    begin
      glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'view'), 1, False, @View );
      glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'proj'), 1, False, @Proj );
      mat4inverse( world * View, mnorm );
      mnorm:= mat4transpose( mnorm );
      glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'mnormal'), 1, False, @mnorm ); // contains left, up, forward vectors
      glUniform3fv( ActShad.Uniforms.AddrByName( 'campos'), 1, @Position );
    end;
end;

procedure TP3DCamera.LoadFromDOM(ADOMNode: TDOMElement; const AData: TP3DData);
var
  AttrS: DOMString;
  FData: TP3DData;
begin
  Name:= ADOMNode.GetAttribute( 'name' );

  AttrS:= ADOMNode.GetAttribute( 'near' );
  if ( AttrS > '' ) then
    Near:= StrToFloatDef( AttrS, 0.0 );

  AttrS:= ADOMNode.GetAttribute( 'far' );
  if ( AttrS > '' ) then
    Far:= StrToFloatDef( AttrS, 0.0 );

  if ( AData = nil ) then
    FData:= P3DData
  else
    FData:= AData;

  FData.Cameras.Add( Self );
  FileWatch.FileName:= '';
end;

constructor TP3DCamera.Create(const AData: TP3DData);
begin
  inherited;
  Aspect:= 4/3;
  FNear:= 0.1;
  FFar:= 100;
  //FHandedness:= coRight;
end;

constructor TP3DCamera.CreateFromDOM(ADOMNode: TDOMElement;
  const AData: TP3DData);
begin
  Create( AData );
  LoadFromDOM( ADOMNode, AData );
end;

{$ENDIF}
