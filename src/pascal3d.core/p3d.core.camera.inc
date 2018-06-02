//p3dcamera.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

type
  { TP3DCamera }

  TP3DCameraHandedness = ( p3dchLeft, p3dchRight );
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

    published
      property FOV: Single read FFOV write SetFOV;
      property IsIsometric: Boolean read FIsIsometric write FIsIsometric;
      property Handedness: TP3DCameraHandedness read fHandedness write fHandedness;
      property Near: Single read FNear write FNear;
      property Far: Single read FFar write FFar;
      property Aspect: Single read FAspect write FAspect;
      property IsoZoom: Single read FIsoZoom write FIsoZoom;
      property AutoAspect: Boolean read FAutoAspect write SetAutoAspect;
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
