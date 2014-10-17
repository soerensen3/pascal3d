unit scene;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Math3D, shaders, dglOpenGL;


type

  { tCamera }

  tCamHandedness = ( coLeft, coRight );
  tCamera = class
    private
      ffar: Single;
      fHandedness: tCamHandedness;
      fmdlview: tMat3;
      fnear: Single;
      fPosition: tVec3;
      fproj: tMat4;
      fview: tMat4;
      fYawPitchRoll: tVec3;

    public
      procedure UpdateMatrices;

      constructor Create;

      property Position: tVec3 read fPosition write fPosition;
      property YawPitchRoll: tVec3 read fYawPitchRoll write fYawPitchRoll;
      property Handedness: tCamHandedness read fHandedness write fHandedness;
      property view: tMat4 read fview write fview;
      property mdlview: tMat3 read fmdlview write fmdlview;
      property proj: tMat4 read fproj write fproj;
      property near: Single read fnear write fnear;
      property far: Single read ffar write ffar;
  end;

  { tScene }
  tScene = class;

  tSceneEvent = procedure ( Scene: tScene );
  tRenderMode = ( rmDefault, rmShadow );
  tScene = class
    private
      fCamera: tCamera;
      fDrawObjects: tSceneEvent;
      fmdlview: tMat3;
      fproj: tMat4;
      fRenderMode: tRenderMode;
      fShader: tShader;
      fview: tMat4;

    public
      procedure RenderFromCamera( Cam: tCamera );
      procedure Render;

      procedure UpdateMatrices;

      property DrawObjects: tSceneEvent read fDrawObjects write fDrawObjects;
      property RenderMode: tRenderMode read fRenderMode write fRenderMode;
      property view: tMat4 read fview write fview;
      property mdlview: tMat3 read fmdlview write fmdlview;
      property proj: tMat4 read fproj write fproj;
      property Cam: tCamera read fCamera write fCamera;
      property Shader: tShader read fShader write fShader;
  end;


implementation

{ tScene }

procedure tScene.UpdateMatrices;
begin
  if ( Assigned( fCamera )) then
    begin
      fview:= fCamera.view;
      fproj:= fCamera.proj;
      mdlview:= fCamera.mdlview;
    end;
  if ( Assigned( Shader )) then
    begin
      Shader.Enable;

//      ShaderSetParameter4fv( Shader.ShaderObj, 'world', Mat4Identity );//world );
      glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'view'), 1, False, @view );
      glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'proj'), 1, False, @proj );
    end;
end;

procedure tScene.RenderFromCamera( Cam: tCamera );
begin
  Cam.UpdateMatrices;
  UpdateMatrices;

  RenderMode:= rmShadow;

  if ( Assigned( DrawObjects )) then
    DrawObjects( Self );

  Shader.Disable;
end;

procedure tScene.Render;
begin
  RenderFromCamera( Cam );
end;

{ tCamera }

procedure tCamera.UpdateMatrices;
var
  _mdlview: tMat4;
  cosx: ValReal;
  cosy: ValReal;
  cosz: ValReal;
  sinx: ValReal;
  siny: ValReal;
  sinz: ValReal;
begin
  view:= mat4translate( vec4( -Position, 1.0 ));
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
  mdlview:= mat3( _mdlview );
  view:= view * _mdlview;

  case Handedness of
    coLeft: proj:= mat4perspectiveFOVLH( deg2rad* 90, 4/3, near, far );
    coRight: proj:= mat4perspectiveFOVRH( deg2rad* 90, 4/3, near, far );
  end;
end;

constructor tCamera.Create;
begin
  inherited;
  fnear:= 0.1;
  ffar:= 100;
end;

end.

