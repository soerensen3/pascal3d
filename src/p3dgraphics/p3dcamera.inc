//p3dcamera.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

type
  { TP3DCamera }

  TP3DCameraHandedness = ( p3dchLeft, p3dchRight );
  TP3DCamera = class ( TP3DDataBlock )
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

    public
      procedure UpdateMatrices(CamObject: TP3DActor);
      procedure PassToShader( world: TMat4 );
      procedure LoadFromDOM( ADOMNode: TDOMElement ); override;

      constructor Create( const AData: TP3DData = nil );
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil );
      procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor ); override;

      property Position: TVec3 read FPosition write FPosition;
      property Forward: TVec3 read FForward;
      property Up: TVec3 read FUp;
      property Left: TVec3 read FLeft;
      property YawPitchRoll: TVec3 read FYawPitchRoll write FYawPitchRoll;
      property View: TMat4 read FView write FView; //GetView write SetView;
      property InView: TMat4 read FInvView write FInvView;
      property MatNormal: TMat3 read FMatNormal write FMatNormal;
      property Proj: TMat4 read FProj write FProj;
      property FOV: Single read FFOV write FFOV;
      property IsIsometric: Boolean read FIsIsometric write FIsIsometric;

    published
      property Handedness: TP3DCameraHandedness read fHandedness write fHandedness;
      property Near: Single read FNear write FNear;
      property Far: Single read FFar write FFar;
      property Aspect: Single read FAspect write FAspect;
      property IsoZoom: Single read FIsoZoom write FIsoZoom;
  end;
  TP3DCameraList = specialize gP3DCustomDatablockList < TP3DCamera >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DCamera }

function TP3DCamera.GetView: TMat4;
begin
  //TODO: REMOVE SIMPLE WORKAROUND IN FAVOR OF SOMETHING BETTER
  if ( Users.Count > 0 ) then
    begin
      if ( Users[ 0 ] is TP3DActor ) then
        Result:= TP3DActor( Users[ 0 ] ).Matrix;
    end
  else
    Result:= Mat4Identity;
end;

procedure TP3DCamera.SetView(AValue: TMat4);
begin
  //TODO: REMOVE SIMPLE WORKAROUND IN FAVOR OF SOMETHING BETTER
  if ( Users.Count > 0 ) then
    if ( Users[ 0 ] is TP3DActor ) then
      TP3DActor( Users[ 0 ] ).FMatrix:= AValue;
end;

procedure TP3DCamera.UpdateMatrices( CamObject: TP3DActor );
begin
  if ( Assigned( CamObject )) then
    mat4inverse( CamObject.Matrix, FView );
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
  InView:= CamObject.Matrix;
end;

procedure TP3DCamera.PassToShader(world: TMat4);
var
  mnorm: TMat4;
  matview: TMat4;
begin
  if ( Assigned( P3DShaderActive )) then
    begin
      matview:= View;
      glUniformMatrix4fv( P3DShaderActive.Uniforms.AddrByName( 'view'), 1, False, @matview ); cle( Self );
      glUniformMatrix4fv( P3DShaderActive.Uniforms.AddrByName( 'proj'), 1, False, @Proj ); cle( Self );
      mat4inverse( View * world, mnorm );
      mnorm:= mat4transpose( mnorm );
      glUniformMatrix4fv( P3DShaderActive.Uniforms.AddrByName( 'mnormal'), 1, False, @mnorm ); cle( Self ); // contains left, up, forward vectors
      glUniform3fv( P3DShaderActive.Uniforms.AddrByName( 'campos'), 1, @Position ); cle( Self );
    end;
end;

procedure TP3DCamera.LoadFromDOM(ADOMNode: TDOMElement);
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

  //FileWatch.FileName:= '';
end;

constructor TP3DCamera.Create(const AData: TP3DData);
begin
  inherited;
  Aspect:= 4/3;
  FNear:= 0.1;
  FFar:= 100;
  FFOV:= 90;
  FIsoZoom:= 1.0;
  //FHandedness:= coRight;
  Parent.Cameras.Add( Self );
end;

constructor TP3DCamera.CreateFromDOM(ADOMNode: TDOMElement;
  const AData: TP3DData);
begin
  Create( AData );
  LoadFromDOM( ADOMNode );
end;

procedure TP3DCamera.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DActor);
begin

end;

{$ENDIF}
