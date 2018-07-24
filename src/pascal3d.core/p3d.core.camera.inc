//p3dcamera.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

type
  { TP3DCamera }

  TP3DCameraHandedness = ( p3dchLeft, p3dchRight );
  TP3DCameraFrustum = array [ 0..6 ] of TP3DPlane;

  TP3DCameraFrustumCorners = array[ 0..3 ] of TVec3;
  TP3DCameraFrustumCube = array[ 0..7 ] of TVec3;

  TP3DCamera = class ( TP3DDataBlock, IP3DAnimatable )
    private
      FAspect: Single;
      FFar: Single;
      FForward: TVec3;
      FFOV: Single;
      FHandedness: TP3DCameraHandedness;
      FInvView: TMat4;
      FIsIsometric: Boolean;
      FIsoZoom: Single;
      FLeft: TVec3;
      FMatNormal: TMat3;
      FNear: Single;
      FPosition: TVec3;
      FProj: TMat4;
      FUp: TVec3;
      FView: TMat4;
      FYawPitchRoll: TVec3;

      function GetView: TMat4;
      procedure SetView(AValue: TMat4);

      // IP3DAnimatable -->
    private
      FAction: TP3DAction;
      FAutoAspect: Boolean;
      FCustomProj: Boolean;
      FInvProj: TMat4;

      function GetAction: TP3DAction;
      procedure SetAction( AValue: TP3DAction );
      procedure SetAutoAspect(AValue: Boolean);
      procedure SetFOV(AValue: Single);

    public
      property Action: TP3DAction read GetAction write SetAction;
      // <-- IP3DAnimatable

    public
      class function GetContainer( ALib: TP3DLibrary): TP3DPropertyAccess; override;

      procedure UpdateMatrices( CamObject: TP3DObject );
      procedure UpdateView( CamObject: TP3DObject );
      procedure UpdateProj;
      procedure UpdateAspect;
      procedure PassToShader;
      function GetFrustum: TP3DCameraFrustum;
      function CalculateCameraFrustumCornersProj( ZDepth: Float ): TP3DCameraFrustumCorners;
      function CalculateCameraFrustumCorners( ZDepth: Float ): TP3DCameraFrustumCorners;
      function CalculateCameraFrustumCornersNDCView: TP3DCameraFrustumCube;
      function CalculateCameraFrustumCornersNDCWorld: TP3DCameraFrustumCube;
      function CalculateCameraFrustumCornersNDCTransform( Matrix: TMat4 ): TP3DCameraFrustumCube;

      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      property Position: TVec3 read FPosition write FPosition;
      property Forward: TVec3 read FForward;
      property Up: TVec3 read FUp;
      property Left: TVec3 read FLeft;
      property YawPitchRoll: TVec3 read FYawPitchRoll write FYawPitchRoll;
      property View: TMat4 read FView write FView; //GetView write SetView;
      property InvView: TMat4 read FInvView write FInvView;
      property MatNormal: TMat3 read FMatNormal write FMatNormal;
      property Proj: TMat4 read FProj write FProj;
      property InvProj: TMat4 read FInvProj write FInvProj;

    published
      property FOV: Single read FFOV write SetFOV;
      property IsIsometric: Boolean read FIsIsometric write FIsIsometric;
      property Handedness: TP3DCameraHandedness read fHandedness write fHandedness;
      property Near: Single read FNear write FNear;
      property Far: Single read FFar write FFar;
      property Aspect: Single read FAspect write FAspect;
      property IsoZoom: Single read FIsoZoom write FIsoZoom;
      property AutoAspect: Boolean read FAutoAspect write SetAutoAspect;
      property CustomProj: Boolean read FCustomProj write FCustomProj;
  end;

  {TP3DCameraContainer = specialize gP3DStreamableContainer < TP3DCamera >;
  TP3DCameraPointer = specialize gP3DStreamablePointer < TP3DCamera >;
  TP3DCameraPointerList = specialize gP3DStreamableList < TP3DCamera, TP3DCameraPointer >;
  }
  TP3DCameraContainerList = specialize gP3DNamedStreamableContainerList < TP3DCamera >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DCamera }

function TP3DCamera.GetView: TMat4;
begin
  //TODO: REMOVE SIMPLE WORKAROUND IN FAVOR OF SOMETHING BETTER
  if ( Users.Count > 0 ) then
    begin
      if ( Users.Items[ 0 ].GetInstance is TP3DObject ) then
        Result:= TP3DObject( Users.Items[ 0 ].GetInstance ).Transform.Matrix;
    end
  else
    Result:= Mat4Identity;
end;

procedure TP3DCamera.SetView(AValue: TMat4);
begin
  //TODO: REMOVE SIMPLE WORKAROUND IN FAVOR OF SOMETHING BETTER
  if ( Users.Count > 0 ) then
    if ( Users.Items[ 0 ].GetInstance is TP3DObject ) then
      TP3DObject( Users.Items[ 0 ].GetInstance ).Transform.Matrix:= AValue;
end;

class function TP3DCamera.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Cameras' );
end;

function TP3DCamera.GetAction: TP3DAction;
begin
  Result:= FAction;
end;

procedure TP3DCamera.SetAction(AValue: TP3DAction);
begin
  FAction:= AValue;
end;

procedure TP3DCamera.SetAutoAspect(AValue: Boolean);
begin
  if FAutoAspect=AValue then Exit;
  FAutoAspect:=AValue;
  UpdateAspect;
  UpdateProj;
end;

procedure TP3DCamera.SetFOV(AValue: Single);
begin
  if FFOV=AValue then Exit;
  FFOV:=AValue;
  UpdateProj;
end;

procedure TP3DCamera.UpdateMatrices( CamObject: TP3DObject );
begin
  UpdateView( CamObject );
  if ( AutoAspect ) then
    UpdateAspect;
  UpdateProj;
end;

procedure TP3DCamera.UpdateView(CamObject: TP3DObject);
begin
  if ( Assigned( CamObject )) then
    begin
      FView:= CamObject.Transform.MatrixInv;
      InvView:= CamObject.Transform.Matrix;
    end;
end;

procedure TP3DCamera.UpdateProj;
begin
  if ( not CustomProj ) then begin
    if ( IsIsometric ) then
      case Handedness of
        p3dchLeft: Proj:= mat4orthoLH( Aspect / IsoZoom, 1 / IsoZoom, Near, Far );
        //p3dchRight: Proj:= mat4orthoRH( Aspect * IsoZoom, 1 * IsoZoom, Near, Far );
      end
    else
      case Handedness of
        p3dchLeft: proj:= mat4perspectiveFOVLH( deg2rad * FOV, Aspect, Near, Far );
        p3dchRight: proj:= mat4perspectiveFOVRH( deg2rad * FOV, Aspect, Near, Far );
      end;
  end;
  mat4inverse( Proj, FInvProj );
end;

procedure TP3DCamera.UpdateAspect;
var
  vp: TP3DViewport;
begin
  vp:= P3DViewports.Peek;
  Aspect:= vp.Width / vp.Height;
end;

procedure TP3DCamera.PassToShader;
var
  mnorm: TMat4;
  matview: TMat4;
begin
  if ( Assigned( P3DShaderActive )) then
    begin
      matview:= View;
      glUniformMatrix4fv( P3DShaderActive.Uniforms.AddrByName( 'view'), 1, False, @matview ); cle( Self );
      glUniformMatrix4fv( P3DShaderActive.Uniforms.AddrByName( 'proj'), 1, False, @Proj ); cle( Self );
      //mat4inverse( View * world, mnorm );
      //mnorm:= mat4transpose( mnorm );
      Position:= FInvView.Row[ 3 ].XYZ;
      //glUniformMatrix4fv( P3DShaderActive.Uniforms.AddrByName( 'mnormal'), 1, False, @mnorm ); cle( Self ); // contains left, up, forward vectors
      glUniform3fv( P3DShaderActive.Uniforms.AddrByName( 'vCamPos'), 1, @Position ); cle( Self );
    end;
end;

function TP3DCamera.GetFrustum: TP3DCameraFrustum;
var
  comboMat: TMat4;
begin
  comboMat:= Proj * View;

  {Result[ 0 ].vNormal.x:= -( comboMat._14 - comboMat._12 ); //- ( Row4 - Row2 )
  Result[ 0 ].vNormal.y:= -( comboMat._24 - comboMat._22 );
  Result[ 0 ].vNormal.z:= -( comboMat._34 - comboMat._32 );

  Result[ 1 ].vNormal.x:= -( comboMat._14 - comboMat._11 ); //- ( Row4 - Row 1 ) -> Row 1 - Row 4
  Result[ 1 ].vNormal.y:= -( comboMat._24 - comboMat._21 );
  Result[ 1 ].vNormal.z:= -( comboMat._34 - comboMat._31 );

  Result[ 2 ].vNormal.x:= -( comboMat._14 + comboMat._12 );
  Result[ 2 ].vNormal.y:= -( comboMat._24 + comboMat._22 );
  Result[ 2 ].vNormal.z:= -( comboMat._34 + comboMat._32 );

  Result[ 3 ].vNormal.x:= -( comboMat._14 + comboMat._11 );
  Result[ 3 ].vNormal.y:= -( comboMat._24 + comboMat._21 );
  Result[ 3 ].vNormal.z:= -( comboMat._34 + comboMat._31 );

  Result[ 4 ].vNormal.x:= -( comboMat._14 + comboMat._13 );
  Result[ 4 ].vNormal.y:= -( comboMat._24 + comboMat._23 );
  Result[ 4 ].vNormal.z:= -( comboMat._34 + comboMat._33 );}
end;

function TP3DCamera.CalculateCameraFrustumCornersProj(ZDepth: Float): TP3DCameraFrustumCorners;
var
  tanHF, tanVF: Math.Float;
  corner: TVec3;
begin
  {tanHF:= tan( FOV * deg2rad / 2 );
  tanVF:= tan( FOV * deg2rad / Aspect / 2 );
  corner:= vec3( tanHF,  tanVF, 1 ) * ZDepth;
  Result[ 0 ]:= vec3( -corner.x, -corner.y, -corner.z );
  Result[ 1 ]:= vec3(  corner.x, -corner.y, -corner.z );
  Result[ 2 ]:= vec3( -corner.x,  corner.y, -corner.z );
  Result[ 3 ]:= vec3(  corner.x,  corner.y, -corner.z );
  Result[ 0 ]:= InvProj * vec3( -1, -1, -ZDepth );
  Result[ 1 ]:= InvProj * vec3(  1, -1, -ZDepth );
  Result[ 2 ]:= InvProj * vec3( -1,  1, -ZDepth );
  Result[ 3 ]:= InvProj * vec3(  1,  1, -ZDepth );}
end;

function TP3DCamera.CalculateCameraFrustumCorners(ZDepth: Float): TP3DCameraFrustumCorners;
var
  i: Integer;
begin
  Result:= CalculateCameraFrustumCornersProj( ZDepth );
  for i:= Low( Result ) to High( Result ) do
    Result[ i ]:= ( InvView * vec4( Result[ i ].XYZ, 1 )).XYZ;
end;

function TP3DCamera.CalculateCameraFrustumCornersNDCView: TP3DCameraFrustumCube;
  function TransformPoint( v: TVec4 ): TVec3;
  var
    p: TVec4;
  begin
    p:= InvProj * v;
    Result:= p.XYZ / p.W;
  end;

var
  m: TMat4;
begin
  m:= InvProj;
  Result:= CalculateCameraFrustumCornersNDCTransform( m );

  {Result[ 0 ]:= TransformPoint( vec4( -1, -1,  1, 1 ));
  Result[ 1 ]:= TransformPoint( vec4(  1, -1,  1, 1 ));
  Result[ 2 ]:= TransformPoint( vec4( -1,  1,  1, 1 ));
  Result[ 3 ]:= TransformPoint( vec4(  1,  1,  1, 1 ));
  Result[ 4 ]:= TransformPoint( vec4( -1, -1, -1, 1 ));
  Result[ 5 ]:= TransformPoint( vec4(  1, -1, -1, 1 ));
  Result[ 6 ]:= TransformPoint( vec4( -1,  1, -1, 1 ));
  Result[ 7 ]:= TransformPoint( vec4(  1,  1, -1, 1 ));}
end;

function TP3DCamera.CalculateCameraFrustumCornersNDCWorld: TP3DCameraFrustumCube;
var
  m: TMat4;
begin
  m:= InvView * InvProj;
  Result:= CalculateCameraFrustumCornersNDCTransform( m );
  {Result:= CalculateCameraFrustumCornersNDCView;
  Result[ 0 ]:= ( InvView * vec4( Result[ 0 ], 1 )).XYZ;
  Result[ 1 ]:= ( InvView * vec4( Result[ 1 ], 1 )).XYZ;
  Result[ 2 ]:= ( InvView * vec4( Result[ 2 ], 1 )).XYZ;
  Result[ 3 ]:= ( InvView * vec4( Result[ 3 ], 1 )).XYZ;
  Result[ 4 ]:= ( InvView * vec4( Result[ 4 ], 1 )).XYZ;
  Result[ 5 ]:= ( InvView * vec4( Result[ 5 ], 1 )).XYZ;
  Result[ 6 ]:= ( InvView * vec4( Result[ 6 ], 1 )).XYZ;
  Result[ 7 ]:= ( InvView * vec4( Result[ 7 ], 1 )).XYZ;}
end;

function TP3DCamera.CalculateCameraFrustumCornersNDCTransform( Matrix: TMat4 ): TP3DCameraFrustumCube;
function TransformPoint( v: TVec4 ): TVec3;
var
  p: TVec4;
begin
  p:= Matrix * v;
  Result:= p.XYZ / p.W;
end;

begin
  Result[ 0 ]:= TransformPoint( vec4( -1, -1,  1, 1 ));
  Result[ 1 ]:= TransformPoint( vec4(  1, -1,  1, 1 ));
  Result[ 2 ]:= TransformPoint( vec4( -1,  1,  1, 1 ));
  Result[ 3 ]:= TransformPoint( vec4(  1,  1,  1, 1 ));
  Result[ 4 ]:= TransformPoint( vec4( -1, -1, -1, 1 ));
  Result[ 5 ]:= TransformPoint( vec4(  1, -1, -1, 1 ));
  Result[ 6 ]:= TransformPoint( vec4( -1,  1, -1, 1 ));
  Result[ 7 ]:= TransformPoint( vec4(  1,  1, -1, 1 ));
end;

constructor TP3DCamera.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Aspect:= 4/3;
  FNear:= 0.1;
  FFar:= 100;
  FFOV:= 90;
  FIsoZoom:= 1.0;
  FAutoAspect:= True;
  //FHandedness:= coRight;
  //Owner.Cameras.Add( Self );
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'FOV', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Near', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Far', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'IsoZoom', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Handedness', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Aspect', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'IsIsometric', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'AutoAspect', smText ));
end;



{$ENDIF}
