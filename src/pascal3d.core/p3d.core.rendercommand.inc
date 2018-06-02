{ p3d.core.rendercommand.inc }

{$IFDEF INTERFACE}
type

  { TP3DRenderCommand }

  TP3DRenderCommand = class ( TP3DStreamable )
    public
      procedure Execute; virtual; abstract;
      procedure Refresh; virtual;
  end;

  { TP3DRenderCommandContainerList }

  TP3DRenderCommandContainerList = class ( specialize gP3DStreamableContainerList < TP3DRenderCommand >)
      public
        procedure Execute;
    end;

  { TP3DRenderCommandRenderList }

  TP3DRenderCommandRenderList = class ( TP3DRenderCommand )
    private
      FFlags: TP3DRenderListFlags;
      FRenderList: TP3DRenderList;
      FCameraObj: TP3DStreamablePointer;
      FScene: TP3DStreamablePointer;

      function GetCameraObj: TP3DObject;
      function GetScene: TP3DScene;
      procedure SetCameraObj(AValue: TP3DObject);
      procedure SetFlags(AValue: TP3DRenderListFlags);
      procedure SetScene(AValue: TP3DScene);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor Create( AScene: TP3DScene; ACameraObj: TP3DObject; AFlags: TP3DRenderListFlags; const AOwner: IP3DStreamableContainer=nil);
      destructor Destroy; override;

      procedure Execute; override;
      procedure Refresh; override;
      procedure UpdateRenderList;

      property RenderList: TP3DRenderList read FRenderList;
      property Scene: TP3DScene read GetScene write SetScene;
      property CameraObj: TP3DObject read GetCameraObj write SetCameraObj;
      property Flags: TP3DRenderListFlags read FFlags write SetFlags;
  end;

  TP3DRenderCommandClearFlag = (
    rccmDepth,
    rccmColor );

  TP3DRenderCommandClearFlags = set of TP3DRenderCommandClearFlag;

  { TP3DRenderCommandClear }

  TP3DRenderCommandClear = class ( TP3DRenderCommand )
    private
      FClearColor: TP3DColorRGBA;
      FClearFlags: TP3DRenderCommandClearFlags;

    public
      constructor Create( AClearFlags: TP3DRenderCommandClearFlags; AClearColor: TP3DColorRGBA; AOwner: IP3DStreamableContainer=nil);

      procedure Execute; override;

      property ClearFlags: TP3DRenderCommandClearFlags read FClearFlags write FClearFlags;
      property ClearColor: TP3DColorRGBA read FClearColor write FClearColor;
  end;

  { TP3DRenderCommandFramebuffer }

  TP3DRenderCommandFramebuffer = class ( TP3DRenderCommand )
    private
      FFramebuffer: TP3DFrameBuffer;

    public
      constructor Create( AFramebuffer: TP3DFrameBuffer; const AOwner: IP3DStreamableContainer=nil);

      procedure Execute; override;

      property Framebuffer: TP3DFrameBuffer read FFramebuffer write FFramebuffer;
  end;

  { TP3DRenderCommandFramebuffer }

  { TP3DRenderCommandFramebufferTarget }

  TP3DRenderCommandFramebufferTarget = class ( TP3DRenderCommand )
    private
      FFramebuffer: TP3DFrameBuffer;
      FTargetIndex: Integer;

    public
      constructor Create( AFramebuffer: TP3DFrameBuffer; ATargetIndex: Integer; const AOwner: IP3DStreamableContainer=nil);

      procedure Execute; override;

      property Framebuffer: TP3DFrameBuffer read FFramebuffer write FFramebuffer;
      property TargetIndex: Integer read FTargetIndex write FTargetIndex;
  end;

  { TP3DRenderCommandFramebufferUnbind }

  TP3DRenderCommandFramebufferUnbind = class ( TP3DRenderCommand )
    protected
      FFramebuffer: TP3DFrameBuffer;

    public
      constructor Create( AFramebuffer: TP3DFrameBuffer; const AOwner: IP3DStreamableContainer=nil);

      procedure Execute; override;

      property Framebuffer: TP3DFrameBuffer read FFramebuffer write FFramebuffer;
  end;

  { TP3DRenderCommandPushVP }

  TP3DRenderCommandPushVP = class ( TP3DRenderCommand )
    private
      FRect: TP3DRect;

    public
      constructor Create( ARect: TP3DRect; AOwner: IP3DStreamableContainer=nil );

      procedure Execute; override;

      property Rect: TP3DRect read FRect write FRect;
  end;

  { TP3DRenderCommandPopVP }

  TP3DRenderCommandPopVP = class ( TP3DRenderCommand )
    public
      procedure Execute; override;
  end;

  { TP3DRenderCommandSetActiveMat }

  TP3DRenderCommandSetActiveMat = class ( TP3DRenderCommand )
    protected
      FMaterial: TP3DStreamablePointer;

      function GetMaterial: TP3DMaterial;
      procedure SetMaterial(AValue: TP3DMaterial);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor Create( AMaterial: TP3DMaterial; const AOwner: IP3DStreamableContainer=nil );

      procedure Execute; override;

      property Material: TP3DMaterial read GetMaterial write SetMaterial;
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DRenderCommandRenderList }

function TP3DRenderCommandRenderList.GetCameraObj: TP3DObject;
begin
  Result:= TP3DObject( FCameraObj.AsValue );
end;

function TP3DRenderCommandRenderList.GetScene: TP3DScene;
begin
  Result:= TP3DScene( FScene.AsValue );
end;

procedure TP3DRenderCommandRenderList.SetCameraObj(AValue: TP3DObject);
begin
  FCameraObj.AsValue:= AValue;
end;

procedure TP3DRenderCommandRenderList.SetFlags(AValue: TP3DRenderListFlags);
begin
  if FFlags=AValue then Exit;
  FFlags:=AValue;
  UpdateRenderList;
end;

procedure TP3DRenderCommandRenderList.SetScene(AValue: TP3DScene);
begin
  FScene.AsValue:= AValue;
  UpdateRenderList;
end;

constructor TP3DRenderCommandRenderList.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FRenderList:= TP3DRenderList.Create;
  FCameraObj:= TP3DStreamablePointer.Create( 'CameraObj', TP3DObject, smText );
  FScene:= TP3DStreamablePointer.Create( 'Scene', TP3DScene, smText );
  Properties.Add([ FScene, FCameraObj ]);
end;

constructor TP3DRenderCommandRenderList.Create(AScene: TP3DScene; ACameraObj: TP3DObject; AFlags: TP3DRenderListFlags;
  const AOwner: IP3DStreamableContainer);
begin
  Create( AOwner );
  Scene:= AScene;
  CameraObj:= ACameraObj;
  Flags:= AFlags;
end;

destructor TP3DRenderCommandRenderList.Destroy;
begin
  FRenderList.Free;
  inherited Destroy;
end;

procedure TP3DRenderCommandRenderList.Execute;
begin
  Refresh;
  FRenderList.Render( CameraObj, Flags );
end;

procedure TP3DRenderCommandRenderList.Refresh;
begin
  UpdateRenderList;
  if ( Assigned( CameraObj ) and ( CameraObj.Data is TP3DCamera )) then
    TP3DCamera( CameraObj.Data ).UpdateMatrices( CameraObj );
end;

procedure TP3DRenderCommandRenderList.UpdateRenderList;
begin
  RenderList.Clear;
  if ( Assigned( Scene )) then
    Scene.MakeRenderList( Scene, Mat4Identity, FRenderList, Flags );
end;


{ TP3DRenderCommandClear }

constructor TP3DRenderCommandClear.Create(AClearFlags: TP3DRenderCommandClearFlags; AClearColor: TP3DColorRGBA;
  AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  ClearFlags:= AClearFlags;
  ClearColor:= AClearColor;
end;

procedure TP3DRenderCommandClear.Execute;
var
  Mask: GLuint;
begin
  Mask:= 0;
  glClearColor( ClearColor.R, ClearColor.G, ClearColor.B, ClearColor.A );
  if ( rccmColor in ClearFlags ) then
    Mask:= GL_COLOR_BUFFER_BIT;
  if ( rccmDepth in ClearFlags ) then
    Mask:= Mask or GL_DEPTH_BUFFER_BIT;
  glClear( Mask ); cle( Self );
end;


{ TP3DRenderCommandFramebuffer }

constructor TP3DRenderCommandFramebuffer.Create(AFramebuffer: TP3DFrameBuffer; const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Framebuffer:= AFramebuffer;
end;

procedure TP3DRenderCommandFramebuffer.Execute;
begin
  if ( Assigned( Framebuffer )) then
    Framebuffer.BindAll();
end;

{ TP3DRenderCommandFramebufferTarget }

constructor TP3DRenderCommandFramebufferTarget.Create(AFramebuffer: TP3DFrameBuffer; ATargetIndex: Integer;
  const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Framebuffer:= AFramebuffer;
  TargetIndex:= ATargetIndex;
end;

procedure TP3DRenderCommandFramebufferTarget.Execute;
begin
  if ( Assigned( Framebuffer )) then
    Framebuffer.Bind( TargetIndex );
end;


{ TP3DRenderCommand }

procedure TP3DRenderCommand.Refresh;
begin

end;

{ TP3DRenderCommandContainerList }

procedure TP3DRenderCommandContainerList.Execute;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    AsValues[ i ].Execute;
end;

{ TP3DRenderCommandFramebufferUnbind }

constructor TP3DRenderCommandFramebufferUnbind.Create(AFramebuffer: TP3DFrameBuffer;
  const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Framebuffer:= AFramebuffer;
end;

procedure TP3DRenderCommandFramebufferUnbind.Execute;
begin
  if ( Assigned( Framebuffer )) then
    Framebuffer.Unbind();
end;


{ TP3DRenderCommandSetActiveMat }

function TP3DRenderCommandSetActiveMat.GetMaterial: TP3DMaterial;
begin
  Result:= TP3DMaterial( FMaterial.AsValue );
end;

procedure TP3DRenderCommandSetActiveMat.SetMaterial(AValue: TP3DMaterial);
begin
  FMaterial.AsValue:= AValue;
end;

constructor TP3DRenderCommandSetActiveMat.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FMaterial:= TP3DStreamablePointer.Create( 'Material', TP3DMaterial, smText );
  Properties.Add( FMaterial );
end;

constructor TP3DRenderCommandSetActiveMat.Create(AMaterial: TP3DMaterial; const AOwner: IP3DStreamableContainer);
begin
  Create( AOwner );
  Material:= AMaterial;
end;

procedure TP3DRenderCommandSetActiveMat.Execute;
begin
  Material.PassToShader();
end;

{ TP3DRenderCommandPopVP }

procedure TP3DRenderCommandPopVP.Execute;
begin
  P3DViewports.Pop;
end;

{ TP3DRenderCommandPushVP }

constructor TP3DRenderCommandPushVP.Create(ARect: TP3DRect; AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FRect:= ARect;
end;

procedure TP3DRenderCommandPushVP.Execute;
begin
  P3DViewports.Push( FRect );
end;

{$ENDIF}

