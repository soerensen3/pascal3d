unit p3dscene;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, p3dMath, p3dshaders, dglOpenGL, p3dviewport;


type
  //TODO: do a rename of all classes with p3d prefix and find a new name for tScene
  { TP3DCamera }

  TP3DCameraHandedness = ( p3dchLeft, p3dchRight );
  TP3DCamera = class
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

      constructor Create;

      property Position: TVec3 read FPosition write FPosition;
      property Forward: TVec3 read FForward;
      property Up: TVec3 read FUp;
      property Left: TVec3 read FLeft;
      property YawPitchRoll: TVec3 read FYawPitchRoll write FYawPitchRoll;
      property Handedness: TP3DCameraHandedness read fHandedness write fHandedness;
      property View: TMat4 read FView write FView;
      property MatNormal: TMat3 read FMatNormal write FMatNormal;
      property Proj: TMat4 read FProj write FProj;
      property Near: Single read FNear write FNear;
      property Far: Single read FFar write FFar;
      property Aspect: Single read FAspect write FAspect;
  end;

  { TP3DScene }
  TP3DScene = class;

  TP3DSceneEvent = procedure ( Scene: TP3DScene );
  TP3DSceneEventObj = procedure ( Scene: TP3DScene ) of object;
  TP3DRenderMode = ( rmDefault, rmShadow );
  TP3DScene = class
    private
      FCamera: TP3DCamera;
      FDrawObjects: TP3DSceneEvent;
      FDrawObjectsObj: TP3DSceneEventObj;
      FMatNormal: TMat3;
      FProj: TMat4;
      FRenderMode: TP3DRenderMode;
      FShader: TShader;
      FView: TMat4;

    public
      procedure RenderFromCamera( Cam: TP3DCamera );
      procedure Render; virtual;

      procedure UpdateMatrices; virtual;
      procedure PassToShader( world: TMat4 ); virtual;

      property DrawObjects: TP3DSceneEvent read FDrawObjects write FDrawObjects;
      property DrawObjectsObj: TP3DSceneEventObj read FDrawObjectsObj write FDrawObjectsObj;
      property RenderMode: TP3DRenderMode read FRenderMode write FRenderMode;
      property View: TMat4 read FView write FView;
      property MatNormal: TMat3 read FMatNormal write FMatNormal;
      property Proj: TMat4 read FProj write FProj;
      property Cam: TP3DCamera read FCamera write FCamera;
      property Shader: TShader read FShader write FShader;
  end;


implementation

{ TP3DScene }

procedure TP3DScene.UpdateMatrices;
begin
  if ( Assigned( fCamera )) then
    begin
      fview:= fCamera.view;
      fproj:= fCamera.proj;
      MatNormal:= fCamera.MatNormal;
    end;
end;

procedure TP3DScene.PassToShader(world: TMat4);
begin
  Cam.PassToShader( world );
end;

procedure TP3DScene.RenderFromCamera( Cam: TP3DCamera );
begin
  if ( P3DViewports.Count > 0 ) then
    Cam.Aspect:= P3DViewports.Peek.Width/P3DViewports.Peek.Height;
  Cam.UpdateMatrices;
  UpdateMatrices;
  PassToShader( Mat4Identity );

  //RenderMode:= rmShadow;

  if ( Assigned( DrawObjects )) then
    DrawObjects( Self );
  if ( Assigned( DrawObjectsObj )) then
    DrawObjectsObj( Self );

  if ( Assigned( Shader )) then
    Shader.Disable;
end;

procedure TP3DScene.Render;
begin
  RenderFromCamera( Cam );
end;

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

constructor TP3DCamera.Create;
begin
  inherited;
  Aspect:= 4/3;
  FNear:= 0.1;
  FFar:= 100;
  //FHandedness:= coRight;
end;

end.

