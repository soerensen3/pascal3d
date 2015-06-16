{$IFDEF INTERFACE}
type
  TFrame = class
    private
      FBones: TBoneList;

    public
      constructor Create;
      destructor Destroy; override;

    published
      property Bones: TBoneList read FBones write FBones;
  end;

  { TArmatureAction }

  TArmatureAction = class
    private
      FFrames: TFrameList;
      FName: String;

    public
      constructor Create;
      destructor Destroy; override;

    published
      property Frames: TFrameList read FFrames write FFrames;
      property Name: String read FName write FName;
  end;

  { TArmature }

  TArmature = class ( TRenderableObject )
    private
      FActions: TActionList;
      FCurrentAction: Integer;
      FCurrentFrame: Integer;
      FStillFrame: TFrame;
      procedure SetCurrentAction(AValue: Integer);
      procedure SetCurrentFrame(AValue: Integer);

    public
      constructor Create( AParentList: TObjectList );
      destructor Destroy; override;

      procedure Render( world: TMat4; const RenderFlag: TRenderFlags ); override;

    published
      property Actions: TActionList read FActions;
      property CurrentAction: Integer read FCurrentAction write SetCurrentAction;
      property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
      property StillFrame: TFrame read FStillFrame;
  end;

  TBone = class( TPersistent )
    private
      FBones: TBoneList;
      FMatrix: TMat4;
      FName: String;

    public
      constructor Create;
      destructor Destroy; override;

      property Matrix: TMat4 read FMatrix write FMatrix;
      property Name: String read FName write FName;
      property Bones: TBoneList read FBones;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TFrame }

constructor TFrame.Create;
begin
  inherited;
  FBones:= TBoneList.Create;
end;

destructor TFrame.Destroy;
begin
  Bones.Free;
  inherited Destroy;
end;

{ TActionList }

procedure TActionList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

{ TFrameList }

procedure TFrameList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

{ TArmatureAction }

constructor TArmatureAction.Create;
begin
  inherited;
  FFrames:= TFrameList.Create;
end;

destructor TArmatureAction.Destroy;
begin
  FFrames.Free;
  inherited Destroy;
end;

{ TArmature }

procedure TArmature.SetCurrentAction(AValue: Integer);
begin
  if ( FCurrentAction = AValue ) then
    Exit;

  FCurrentAction:= Min( Actions.Count - 1, Max( -1, AValue ));
end;

procedure TArmature.SetCurrentFrame(AValue: Integer);
begin
  if ( FCurrentFrame = AValue ) then
    Exit;

  if ( InRange( FCurrentAction, 0, Actions.Count - 1 )) then
    begin
      FCurrentFrame:= AValue mod FActions[ FCurrentAction ].Frames.Count;
      if ( FCurrentFrame < 0 ) then
        FCurrentFrame += FActions[ FCurrentAction ].Frames.Count;
    end
  else
    FCurrentFrame:= 0;
end;

constructor TArmature.Create(AParentList: TObjectList);
begin
  inherited;
  FActions:= TActionList.Create;
  FStillFrame:= TFrame.Create;
  FVisible:= True;
  FCurrentFrame:= 0;
  FCurrentAction:= -1;
end;

destructor TArmature.Destroy;
begin
  FStillFrame.Free;
  FActions.Free;
  inherited Destroy;
end;

procedure TArmature.Render(world: TMat4; const RenderFlag: TRenderFlags);
var
  Bone: TBone;
  view, proj: TMat4;
  BackupShader: TShader;

  procedure RenderBone( ABone: TBone; Mat: TMat4; const RootBone: Boolean = False );
  var
    matNew: TMat4;
    p1: TVec3;
    p2: TVec3;
  begin
    matNew:= ABone.Matrix * Mat;
    if ( not RootBone ) then
      begin
        p1:= vec3( vec4( 0 ) * Mat );
        p2:= vec3( vec4( 0 ) * matNew );
        RenderLines3D([ p1, p2 ], vec4( 1, 0, 0, 1 ));
      end;
    for Bone in ABone.Bones do
      RenderBone( Bone, matNew );
  end;

begin
  inherited Render(world, RenderFlag);
  if ( rfDebugShowArmature in RenderFlag ) then
    begin
      glDisable( GL_DEPTH_TEST );
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'view' ), @view );
      glGetUniformfv( ActShad.ShaderObj, ActShad.Uniforms.AddrByName( 'proj' ), @proj );

      BackupShader:= ActShad;

      Setup3D( view * proj );
      if ( CurrentAction = -1 ) then
        for Bone in StillFrame.Bones do
          RenderBone( Bone, world, True )
        else
          if ( InRange( FCurrentAction, 0, Actions.Count - 1 )) then
            if ( InRange( FCurrentFrame, 0, Actions[ FCurrentAction ].Frames.Count - 1 )) then
              for Bone in Actions[ FCurrentAction ].Frames[ FCurrentFrame ].Bones do
                RenderBone( Bone, world, True );

      glEnable( GL_DEPTH_TEST );

      BackupShader.Enable;
    end;
end;

{ TBone }

constructor TBone.Create;
begin
  inherited;
  FBones:= TBoneList.Create;
end;

destructor TBone.Destroy;
begin
  FBones.Free;
  inherited Destroy;
end;


{ TBoneList }

function TBoneList.FindByName(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = Name ) then
      begin
        Result:= i;
        break;
      end;
end;

procedure TBoneList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

{$ENDIF}
