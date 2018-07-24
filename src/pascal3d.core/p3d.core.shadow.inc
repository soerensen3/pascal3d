{ include.inc }

{$IFDEF INTERFACE}
type

  { TP3DIBLRenderAtmosphere }

  TP3DIBLRenderAtmosphere = class ( TP3DEffectCustom )
    protected
      FEnableFrameBuffer: TP3DRenderCommandFramebuffer;
      FFrameBuffer: TP3DStreamableContainer;
      FSunDir: TVec3;

      FIBLSpec,
      FMatAtmosphere,
      FCamAtmosphere,
      FMeshPlane: TP3DStreamablePointer;
      FUnSunDir: TP3DStreamablePointer;

      function GetFrameBuffer: TP3DFrameBuffer;
      function GetCamAtmosphere: TP3DObject;
      procedure SetSunDir(AValue: TVec3);
      function GetIBLSpec: TP3DTexture;
      function GetMatAtmosphere: TP3DMaterialNode;
      function GetMeshPlane: TP3DMesh;
      procedure SetIBLSpec(AValue: TP3DTexture);
      function GetUnSunDir: TP3DNodeClone;

      property FrameBuffer: TP3DFrameBuffer read GetFrameBuffer;
      property MatAtmosphere: TP3DMaterialNode read GetMatAtmosphere;
      property MeshPlane: TP3DMesh read GetMeshPlane;
      property CamAtmosphere: TP3DObject read GetCamAtmosphere;
      property CommandList: TP3DRenderCommandContainerList read FCommandList;
      property UnSunDir: TP3DNodeClone read GetUnSunDir;

      procedure CreateMatAtmosphere;
      procedure CreateLib; override;
      procedure PopulateCommandList; override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Render; override;

      property IBLSpec: TP3DTexture read GetIBLSpec write SetIBLSpec;
      property SunDir: TVec3 read FSunDir write SetSunDir;
  end;

  { TP3DIBLDiffuseConv }

  TP3DIBLDiffuseConv = class ( TP3DEffectCustom )
    protected
      FIBLSrc, FIBLDiffuse, FMatConv: TP3DStreamablePointer;
      FFrameBuffer: TP3DStreamableContainer;

      function GetFrameBuffer: TP3DFrameBuffer;
      function GetIBLDiffuse: TP3DTexture;
      function GetIBLSrc: TP3DTexture;
      procedure SetIBLDiffuse(AValue: TP3DTexture);
      procedure SetIBLSrc(AValue: TP3DTexture);
      function GetMatConv: TP3DMaterialNode;

      procedure UpdateFramebuffer;
      procedure CreateMatConv;
      procedure CreateLib; override;
      procedure PopulateCommandList; override;
      procedure PassUniform( Material: TP3DMaterialNode; Uniform: TP3DNodeClone );

      property MatConv: TP3DMaterialNode read GetMatConv;
      property FrameBuffer: TP3DFrameBuffer read GetFrameBuffer;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Render; override;

    published
      property IBLSrc: TP3DTexture read GetIBLSrc write SetIBLSrc;
      property IBLDiffuse: TP3DTexture read GetIBLDiffuse write SetIBLDiffuse;
  end;

  { TP3DIBLSpecularConv }

  TP3DIBLSpecularConv = class ( TP3DEffectCustom )
    protected
      FRoughness: Float;
      FResolution: Integer;
      FFramebuffers: TP3DFrameBufferContainerList;
      FIBLSpec, FIBLSrc, FMatConv, FMatCopy: TP3DStreamablePointer;

      function GetIBLSpec: TP3DTexture;
      procedure SetIBLSpec(AValue: TP3DTexture);
      function GetIBLSrc: TP3DTexture;
      procedure SetIBLSrc(AValue: TP3DTexture);
      procedure CreateMatCopy;
      procedure CreateMatConv;
      procedure UpdateFramebuffer;
      procedure PopulateCommandList; override;
      function GetMatConv: TP3DMaterialNode;
      function GetMatCopy: TP3DMaterialNode;
      procedure CreateLib; override;
      procedure PassUniform( Material: TP3DMaterialNode; Uniform: TP3DNodeClone );

      property MatConv: TP3DMaterialNode read GetMatConv;
      property MatCopy: TP3DMaterialNode read GetMatCopy;
      property Framebuffers: TP3DFrameBufferContainerList read FFramebuffers;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Render; override;

      property IBLSrc: TP3DTexture read GetIBLSrc write SetIBLSrc;
      property IBLSpec: TP3DTexture read GetIBLSpec write SetIBLSpec;
  end;

  { TP3DVarianceShadowMap_DepthWrite }

  TP3DVarianceShadowMap_DepthWrite = class ( TP3DEffectCustom )
    protected
      const NumCascades = 4;

    protected
      FMatDepth: TP3DStreamablePointer;
      FMatBlurX: TP3DStreamablePointer;
      FMatBlurY: TP3DStreamablePointer;
      FFrameBuffer: TP3DStreamableContainer;
      FFramebufferBlurX: TP3DStreamableContainer;
      FFramebufferBlurY: TP3DStreamableContainer;
      FLightObj: TP3DStreamablePointer;
      FRenderScene: TP3DRenderCommandRenderList;
      FCamObj: TP3DStreamablePointer;
      FTextures: specialize gP3DStreamablePointerList < TP3DTexture >;

      function GetCamObj: TP3DObject;
      procedure SetCamObj(AValue: TP3DObject);
      function GetOffscreen: TP3DTexture;
      function GetRenderTarget: TP3DTexture;
      procedure SetRenderTarget(AValue: TP3DTexture);
      function GetMatBlurX: TP3DMaterialNode;
      function GetMatBlurY: TP3DMaterialNode;
      function GetFrameBuffer: TP3DFrameBuffer;
      function GetFramebufferBlurX: TP3DFrameBuffer;
      function GetFramebufferBlurY: TP3DFrameBuffer;
      function GetMatDepth: TP3DMaterialNode;
      function GetLightObj: TP3DObject;
      function GetScene: TP3DScene;
      procedure SetLightObj(AValue: TP3DObject);
      procedure SetScene(AValue: TP3DScene);

      procedure CreateLib; override;
      procedure PopulateCommandList; override;
      procedure CreateMat;
      procedure UpdateOffscreen;
      procedure SetFramebufferTex( AFramebuffer: TP3DFrameBuffer; ATex: TP3DTexture );

      property Offscreen: TP3DTexture read GetOffscreen;
      property RenderTarget: TP3DTexture read GetRenderTarget write SetRenderTarget;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Render; override;

      property MatDepth: TP3DMaterialNode read GetMatDepth;
      property MatBlurX: TP3DMaterialNode read GetMatBlurX;
      property MatBlurY: TP3DMaterialNode read GetMatBlurY;
      property Framebuffer: TP3DFrameBuffer read GetFramebuffer;
      property FramebufferBlurX: TP3DFrameBuffer read GetFramebufferBlurX;
      property FramebufferBlurY: TP3DFrameBuffer read GetFramebufferBlurY;
      property LightObj: TP3DObject read GetLightObj write SetLightObj;
      property Scene: TP3DScene read GetScene write SetScene;
      property CamObj: TP3DObject read GetCamObj write SetCamObj;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DIBLRenderAtmosphere }

function TP3DIBLRenderAtmosphere.GetUnSunDir: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FUnSunDir.AsValue );
end;

function TP3DIBLRenderAtmosphere.GetFrameBuffer: TP3DFrameBuffer;
begin
  Result:= TP3DFrameBuffer( FFrameBuffer.AsValue );
end;

function TP3DIBLRenderAtmosphere.GetCamAtmosphere: TP3DObject;
begin
  Result:= TP3DObject( FCamAtmosphere.AsValue );
end;

procedure TP3DIBLRenderAtmosphere.SetSunDir(AValue: TVec3);
begin
  if FSunDir=AValue then Exit;
  FSunDir:= AValue;
  if ( Assigned( UnSunDir )) then
    UnSunDir.FindInput( 'Value' ).ValueProp.AsString:= AValue.ToString();
end;

function TP3DIBLRenderAtmosphere.GetIBLSpec: TP3DTexture;
begin
  Result:= TP3DTexture( FIBLSpec.AsValue );
end;

function TP3DIBLRenderAtmosphere.GetMatAtmosphere: TP3DMaterialNode;
begin
  Result:= TP3DMaterialNode( FMatAtmosphere.AsValue );
end;

function TP3DIBLRenderAtmosphere.GetMeshPlane: TP3DMesh;
begin
  Result:= TP3DMesh( FMeshPlane.AsValue );
end;

procedure TP3DIBLRenderAtmosphere.SetIBLSpec(AValue: TP3DTexture);
begin
  FIBLSpec.AsValue:= AValue;
  TP3DFrameBuffer.Create( FFrameBuffer );
  if ( Assigned( AValue )) then begin
    FrameBuffer.AttachColor( AValue );
    FrameBuffer.AttachDepth( AValue.Width, AValue.Height );
    FrameBuffer.CheckFBO;
    FrameBuffer.Unbind();
  end;
  PopulateCommandList;
end;

procedure TP3DIBLRenderAtmosphere.CreateMatAtmosphere;
var
  atm, texcoord, math_simple, spherical, uniform: TP3DNodeClone;
begin
  FMatAtmosphere.AsValue:= TP3DMaterialNode.CreateBlock( Lib );
  MatAtmosphere.MakeRootNode( 'output' );
  atm:= P3DData.P3DShaderNodeLib.FindNode( 'atmosphere' ).Clone( MatAtmosphere.NodeTree );
  MatAtmosphere.RootNode.FindInput( 'Color' ).Connected:= atm.FindOutput( 'Color' );
  uniform:= P3DData.P3DShaderNodeLib.FindNode( 'uniform_vec3' ).Clone( MatAtmosphere.Uniforms );
  uniform.FindInput( 'UnName' ).ValueProp.AsString:= 'SunDir';
  uniform.FindInput( 'Value' ).ValueProp.AsString:= '0,0,1';
  atm.FindInput( 'SunDir' ).Connected:= uniform.FindOutput( 'Vector' );
  texcoord:= P3DData.P3DShaderNodeLib.FindNode( 'texcoord' ).Clone( MatAtmosphere.NodeTree );
  math_simple:= P3DData.P3DShaderNodeLib.FindNode( 'vector_math_simple' ).Clone( MatAtmosphere.NodeTree );
  spherical:= P3DData.P3DShaderNodeLib.FindNode( 'inv_spherical_mapping' ).Clone( MatAtmosphere.NodeTree );
  atm.FindInput( 'Position' ).Connected:= spherical.FindOutput( 'Normal' );
  spherical.FindInput( 'UV' ).Connected:= math_simple.FindOutput( 'add' );
  math_simple.FindInput( 'Vector2' ).ValueProp.AsString:= '-0.25, 0.0, 0.0, 0.0';
  math_simple.FindInput( 'Vector1' ).Connected:= texcoord.FindOutput( 'UV' );
  FUnSunDir.AsValue:= uniform;
end;

procedure TP3DIBLRenderAtmosphere.CreateLib;
var
  Scene: TP3DScene;
  Obj: TP3DObject;
begin
  inherited CreateLib;
  Scene:= TP3DScene.CreateBlock( Lib );
  Obj:= TP3DObject.CreateBlock( Lib );
  Scene.Objects.Add( Obj );
  FMeshPlane.AsValue:= P3DMeshCreatePlane( vec2( 1 ), Lib );
  Obj.Data:= MeshPlane;
  CreateMatAtmosphere;
  MeshPlane.Material:= MatAtmosphere;
  FCamAtmosphere.AsValue:= TP3DObject.CreateBlock( Lib );
  CamAtmosphere.Transform.Position:= vec3( 0, 0, 1 );
  CamAtmosphere.Transform.Rotation:= vec3( 0, 0, 0 );
  CamAtmosphere.Data:= TP3DCamera.CreateBlock( Lib );
  TP3DCamera( CamAtmosphere.Data ).IsIsometric:= True;
end;


procedure TP3DIBLRenderAtmosphere.PopulateCommandList;
begin
  CommandList.Clear;
  TP3DRenderCommandFramebuffer.Create( FrameBuffer, FCommandList );
  TP3DRenderCommandClear.Create([ rccmColor, rccmDepth ], Black, FCommandList );
  TP3DRenderCommandRenderList.Create( TP3DScene( Lib.Scenes.First()), CamAtmosphere, P3DRenderListFlagsDefault, FCommandList );
  TP3DRenderCommandFramebufferUnbind.Create( FrameBuffer, FCommandList );
end;

constructor TP3DIBLRenderAtmosphere.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FIBLSpec:= TP3DStreamablePointer.Create( 'IBLSpec', TP3DTexture );
  FMatAtmosphere:= TP3DStreamablePointer.Create( 'MatAtmosphere', TP3DMaterialNode );
  FMeshPlane:= TP3DStreamablePointer.Create( 'MeshPlane', TP3DMesh );
  FCamAtmosphere:= TP3DStreamablePointer.Create( 'CamAtmosphere', TP3DObject );
  FUnSunDir:= TP3DStreamablePointer.Create( 'UnSunDir', TP3DNodeClone );
  FFrameBuffer:= TP3DStreamableContainer.Create( 'FrameBuffer', TP3DFrameBuffer );
  Properties.Add([ FIBLSpec, FMatAtmosphere, FMeshPlane, FCamAtmosphere, FUnSunDir, FFrameBuffer ]);
  TP3DFrameBuffer.Create( FFrameBuffer );
  FrameBuffer.Unbind();
  CreateLib;
  PopulateCommandList;
  FSunDir:= vec3( 0, 0, 1 );
end;

destructor TP3DIBLRenderAtmosphere.Destroy;
begin
  FCommandList.Clear;
  inherited Destroy;
end;

procedure TP3DIBLRenderAtmosphere.Render;
begin
  if ( Assigned( CommandList ) and Assigned( IBLSpec )) then begin
    CommandList.Execute;
    IBLSpec.UpdateMipMaps();
  end;
end;


{ TP3DIBLSpecularConv }

function TP3DIBLSpecularConv.GetMatConv: TP3DMaterialNode;
begin
  Result:= TP3DMaterialNode( FMatConv.AsValue );
end;

procedure TP3DIBLSpecularConv.CreateLib;
var
  Scene: TP3DScene;
  Obj, Cam: TP3DObject;
  MeshPlane: TP3DMesh;
begin
  inherited CreateLib;
  Cam:= TP3DObject.CreateBlock( Lib );
  Cam.Transform.Position:= vec3( 0, 0, 1 );
  Cam.Transform.Rotation:= vec3( 0, 0, 0 );
  Cam.Data:= TP3DCamera.CreateBlock( Lib );
  TP3DCamera( Cam.Data ).IsIsometric:= True;

  Scene:= TP3DScene.CreateBlock( Lib );
  Obj:= TP3DObject.CreateBlock( Lib );
  Scene.Objects.Add( Obj );
  MeshPlane:= P3DMeshCreatePlane( vec2( 1 ), Lib );
  Obj.Data:= MeshPlane;
  CreateMatConv;
  MeshPlane.Material:= MatConv;

  Scene:= TP3DScene.CreateBlock( Lib );
  Obj:= TP3DObject.CreateBlock( Lib );
  Scene.Objects.Add( Obj );
  MeshPlane:= P3DMeshCreatePlane( vec2( 1 ), Lib );
  Obj.Data:= MeshPlane;
  CreateMatCopy;
  MeshPlane.Material:= MatCopy;
end;

procedure TP3DIBLSpecularConv.PassUniform(Material: TP3DMaterialNode; Uniform: TP3DNodeClone);
begin
  if ( Uniform.FindInput( 'UnName' ).ValueProp.AsString = 'Size' ) then begin
    Uniform.FindInput( 'Value' ).ValueProp.AsString:= FloatToStr( max( 0.05, FRoughness / 2 ));
    FRoughness += 1 / Framebuffers.Count;
  end
  else if ( Uniform.FindInput( 'UnName' ).ValueProp.AsString = 'Resolution' ) then begin
    Uniform.FindInput( 'Value' ).ValueProp.AsString:= IntToStr( FResolution );
    FResolution:= FResolution div 2;
  end;
end;

function TP3DIBLSpecularConv.GetIBLSrc: TP3DTexture;
begin
  Result:= TP3DTexture( FIBLSrc.AsValue );
end;

procedure TP3DIBLSpecularConv.SetIBLSrc(AValue: TP3DTexture);
begin
  FIBLSrc.AsValue:= AValue;
  if ( Assigned( MatConv ) and Assigned( MatConv.Maps[ 0 ])) then
    MatConv.Maps[ 0 ].Map:= AValue;
  if ( Assigned( MatCopy ) and Assigned( MatCopy.Maps[ 0 ])) then
    MatCopy.Maps[ 0 ].Map:= AValue;
end;

procedure TP3DIBLSpecularConv.CreateMatCopy;
var
  texcoord, mix: TP3DNodeClone;
begin
  FMatCopy.AsValue:= TP3DMaterialNode.CreateBlock( Lib );
  MatCopy.MakeRootNode( 'output' );
  texcoord:= P3DData.P3DShaderNodeLib.FindNode( 'texcoord' ).Clone( MatCopy.NodeTree );

  MatCopy.RootNode.FindInput( 'Color' ).Connected:= MatCopy.Maps.AddCopy( 'texture2D' ).FindOutput( 'Color' );
  mix:= P3DData.P3DShaderNodeLib.FindNode( 'vector_math_simple' ).Clone( MatCopy.NodeTree );
  mix.FindInput( 'Vector1' ).Connected:= texcoord.FindOutput( 'UV' );
  mix.FindInput( 'Vector2' ).ValueProp.AsString:= '1,-1,0,0';
  MatCopy.Maps[ 0 ].FindInput( 'UV' ).Connected:= mix.FindOutput( 'mul' );


  MatCopy.Maps[ 0 ].Map:= IBLSrc;
end;

function TP3DIBLSpecularConv.GetMatCopy: TP3DMaterialNode;
begin
  Result:= TP3DMaterialNode( FMatCopy.AsValue );
end;

function TP3DIBLSpecularConv.GetIBLSpec: TP3DTexture;
begin
  Result:= TP3DTexture( FIBLSpec.AsValue );
end;

procedure TP3DIBLSpecularConv.SetIBLSpec(AValue: TP3DTexture);
begin
  FIBLSpec.AsValue:= AValue;
  IBLSpec.UpdateMipMaps();
  UpdateFramebuffer;
  PopulateCommandList;
end;

procedure TP3DIBLSpecularConv.CreateMatConv;
var
  texcoord, conv, uniform: TP3DNodeClone;
begin
  FMatConv.AsValue:= TP3DMaterialNode.CreateBlock( Lib );
  MatConv.MakeRootNode( 'output' );
  texcoord:= P3DData.P3DShaderNodeLib.FindNode( 'texcoord' ).Clone( MatConv.NodeTree );
  conv:= P3DData.P3DShaderNodeLib.FindNode( 'convolute_environment_spherical' ).Clone( MatConv.NodeTree );
  MatConv.RootNode.FindInput( 'Color' ).Connected:= conv.FindOutput( 'Color' );
  uniform:= P3DData.P3DShaderNodeLib.FindNode( 'uniform_float' ).Clone( MatConv.Uniforms );
  uniform.FindInput( 'UnName' ).ValueProp.AsString:= 'Size';
  uniform.FindInput( 'Value' ).ValueProp.AsString:= '0.001';
  conv.FindInput( 'Size' ).Connected:= uniform.FindOutput( 'Value' );

  uniform:= P3DData.P3DShaderNodeLib.FindNode( 'uniform_float' ).Clone( MatConv.Uniforms );
  uniform.FindInput( 'UnName' ).ValueProp.AsString:= 'Resolution';
  uniform.FindInput( 'Value' ).ValueProp.AsString:= '512';
  conv.FindInput( 'Resolution' ).Connected:= uniform.FindOutput( 'Value' );

  conv.FindInput( 'UV' ).Connected:= texcoord.FindOutput( 'UV' );
  conv.FindInput( 'Texture' ).Connected:= MatConv.Maps.AddCopy( 'texture2D' ).FindOutput( 'Texture' );
  MatConv.Maps[ 0 ].Map:= IBLSrc;
  MatConv.OnPassUniform:= @PassUniform;
end;

procedure TP3DIBLSpecularConv.UpdateFramebuffer;
var
  i: Integer;
begin
  Framebuffers.Clear;
  for i:= 0 to 9 do
    with TP3DFrameBuffer.Create( FFramebuffers ) do begin
      AutoViewPort:= False;
      AttachColor( IBLSpec, i );
      CheckFBO;
    end;
end;

procedure TP3DIBLSpecularConv.PopulateCommandList;
var
  TexSize: TIVec2;
  i: Integer;
begin
  FCommandList.Clear;
  if ( not Assigned( IBLSpec )) then
    exit;
  TexSize:= ivec2( IBLSpec.Width, IBLSpec.Height );
  for i:= 0 to Framebuffers.Count - 1 do begin
    //TP3DRenderCommandFramebuffer.Create( FFramebuffer, FCommandList );
    TP3DRenderCommandFramebuffer.Create( FFramebuffers[ i ], FCommandList );
    TP3DRenderCommandPushVP.Create( P3DRect( vec2( 0 ), vec2( TexSize )), FCommandList );
    //TP3DRenderCommandClear.Create([ rccmColor, rccmDepth ], vec4( Red500.rgb * i / 10, 1 ), FCommandList );
    if ( i > 0 ) then
      TP3DRenderCommandRenderList.Create( TP3DScene( Lib.Scenes.First()), Lib.Objects[ 0 ], P3DRenderListFlagsDefault, FCommandList )
    else
      TP3DRenderCommandRenderList.Create( Lib.Scenes[ 1 ], Lib.Objects[ 0 ], P3DRenderListFlagsDefault, FCommandList );
    TP3DRenderCommandPopVP.Create( FCommandList );
    TP3DRenderCommandFramebufferUnbind.Create( FFramebuffers[ i ], FCommandList );
    TexSize:= TexSize div 2;
  end;
end;

constructor TP3DIBLSpecularConv.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FIBLSpec:= TP3DStreamablePointer.Create( 'IBLSpec', TP3DTexture );
  Properties.Add( FIBLSpec );
  FIBLSrc:= TP3DStreamablePointer.Create( 'IBLSrc', TP3DTexture );
  Properties.Add( FIBLSrc );
  FMatConv:= TP3DStreamablePointer.Create( 'MatConv', TP3DMaterialNode );
  Properties.Add( FMatConv );
  FMatCopy:= TP3DStreamablePointer.Create( 'MatCopy', TP3DMaterialNode );
  Properties.Add( FMatCopy );
  FFramebuffers:= TP3DFrameBufferContainerList.Create( 'FFramebuffers', smText );
  Properties.Add( FFramebuffers );

  CreateLib;
  PopulateCommandList;
end;

destructor TP3DIBLSpecularConv.Destroy;
var
  i: Integer;
begin
  FFramebuffers.Clear;
  inherited Destroy;
end;

procedure TP3DIBLSpecularConv.Render;
begin
  if ( not Assigned( IBLSrc )) then
    exit;
  FRoughness:= 1 / Framebuffers.Count;
  FResolution:= IBLSrc.Width div 2;
  FCommandList.Execute;
end;


{ TP3DIBLDiffuseConv }

function TP3DIBLDiffuseConv.GetMatConv: TP3DMaterialNode;
begin
  Result:= TP3DMaterialNode( FMatConv.AsValue );
end;

function TP3DIBLDiffuseConv.GetFrameBuffer: TP3DFrameBuffer;
begin
  Result:= TP3DFrameBuffer( FFrameBuffer.AsValue );
end;

function TP3DIBLDiffuseConv.GetIBLDiffuse: TP3DTexture;
begin
  Result:= TP3DTexture( FIBLDiffuse.AsValue );
end;

function TP3DIBLDiffuseConv.GetIBLSrc: TP3DTexture;
begin
  Result:= TP3DTexture( FIBLSrc.AsValue );
end;

procedure TP3DIBLDiffuseConv.SetIBLDiffuse(AValue: TP3DTexture);
begin
  FIBLDiffuse.AsValue:= AValue;
  IBLDiffuse.UpdateMipMaps();
  UpdateFramebuffer;
  PopulateCommandList;
end;

procedure TP3DIBLDiffuseConv.SetIBLSrc(AValue: TP3DTexture);
begin
  FIBLSrc.AsValue:= AValue;
  if ( Assigned( MatConv ) and Assigned( MatConv.Maps[ 0 ])) then
    MatConv.Maps[ 0 ].Map:= AValue;
end;

procedure TP3DIBLDiffuseConv.UpdateFramebuffer;
begin
  TP3DFrameBuffer.Create( FFrameBuffer );
  Framebuffer.AutoViewPort:= False;
  Framebuffer.AttachColor( IBLDiffuse );
  Framebuffer.CheckFBO;
end;

procedure TP3DIBLDiffuseConv.CreateMatConv;
var
  texcoord, conv, uniform: TP3DNodeClone;
begin
  FMatConv.AsValue:= TP3DMaterialNode.CreateBlock( Lib );
  MatConv.MakeRootNode( 'output' );
  texcoord:= P3DData.P3DShaderNodeLib.FindNode( 'texcoord' ).Clone( MatConv.NodeTree );
  conv:= P3DData.P3DShaderNodeLib.FindNode( 'convolute_environment_spherical' ).Clone( MatConv.NodeTree );
  MatConv.RootNode.FindInput( 'Color' ).Connected:= conv.FindOutput( 'Color' );

  uniform:= P3DData.P3DShaderNodeLib.FindNode( 'uniform_float' ).Clone( MatConv.Uniforms );
  uniform.FindInput( 'UnName' ).ValueProp.AsString:= 'Resolution';
  uniform.FindInput( 'Value' ).ValueProp.AsString:= '512';
  conv.FindInput( 'Resolution' ).Connected:= uniform.FindOutput( 'Value' );

  conv.FindInput( 'UV' ).Connected:= texcoord.FindOutput( 'UV' );
  conv.FindInput( 'Texture' ).Connected:= MatConv.Maps.AddCopy( 'texture2D' ).FindOutput( 'Texture' );
  MatConv.Maps[ 0 ].Map:= IBLSrc;
  MatConv.OnPassUniform:= @PassUniform;
end;

procedure TP3DIBLDiffuseConv.CreateLib;
var
  Cam, Obj: TP3DObject;
  Scene: TP3DScene;
  MeshPlane: TP3DMesh;
begin
  inherited CreateLib;
  Cam:= TP3DObject.CreateBlock( Lib );
  Cam.Transform.Position:= vec3( 0, 0, 1 );
  Cam.Transform.Rotation:= vec3( 0, 0, 0 );
  Cam.Data:= TP3DCamera.CreateBlock( Lib );
  TP3DCamera( Cam.Data ).IsIsometric:= True;

  Scene:= TP3DScene.CreateBlock( Lib );
  Obj:= TP3DObject.CreateBlock( Lib );
  Scene.Objects.Add( Obj );
  MeshPlane:= P3DMeshCreatePlane( vec2( 1 ), Lib );
  Obj.Data:= MeshPlane;
  CreateMatConv;
  MeshPlane.Material:= MatConv;
end;

procedure TP3DIBLDiffuseConv.PopulateCommandList;
begin
  FCommandList.Clear;
  if ( not Assigned( IBLDiffuse )) then
    exit;

  TP3DRenderCommandFramebuffer.Create( Framebuffer, FCommandList );
  TP3DRenderCommandPushVP.Create( P3DRect( vec2( 0 ), vec2( IBLDiffuse.Width, IBLDiffuse.Height )), FCommandList );
  TP3DRenderCommandRenderList.Create( TP3DScene( Lib.Scenes.First()), Lib.Objects[ 0 ], P3DRenderListFlagsDefault, FCommandList );
  TP3DRenderCommandPopVP.Create( FCommandList );
  TP3DRenderCommandFramebufferUnbind.Create( Framebuffer, FCommandList );
end;

procedure TP3DIBLDiffuseConv.PassUniform(Material: TP3DMaterialNode;
  Uniform: TP3DNodeClone);
begin
  if ( Uniform.FindInput( 'UnName' ).ValueProp.AsString = 'Resolution' ) then
    Uniform.FindInput( 'Value' ).ValueProp.AsString:= IntToStr( IBLSrc.Width );
end;

constructor TP3DIBLDiffuseConv.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FIBLDiffuse:= TP3DStreamablePointer.Create( 'IBLDiffuse', TP3DTexture );
  Properties.Add( FIBLDiffuse );
  FIBLSrc:= TP3DStreamablePointer.Create( 'IBLSrc', TP3DTexture );
  Properties.Add( FIBLSrc );
  FMatConv:= TP3DStreamablePointer.Create( 'MatConv', TP3DMaterialNode );
  Properties.Add( FMatConv );
  FFrameBuffer:= TP3DStreamableContainer.Create( 'FrameBuffer', TP3DFrameBuffer );
  Properties.Add([ FFrameBuffer ]);
  TP3DFrameBuffer.Create( FFrameBuffer );

  CreateLib;
  PopulateCommandList;
end;

procedure TP3DIBLDiffuseConv.Render;
begin
  FCommandList.Execute;
end;

{ TP3DVarianceShadowMap_DepthWrite }

function TP3DVarianceShadowMap_DepthWrite.GetLightObj: TP3DObject;
begin
  Result:= TP3DObject( FLightObj.AsValue );
end;

function TP3DVarianceShadowMap_DepthWrite.GetScene: TP3DScene;
begin
  Result:= FRenderScene.Scene;
end;

procedure TP3DVarianceShadowMap_DepthWrite.SetLightObj(AValue: TP3DObject);
begin
  FLightObj.AsValue:= AValue;
end;

procedure TP3DVarianceShadowMap_DepthWrite.SetScene(AValue: TP3DScene);
begin
  FRenderScene.Scene:= AValue;
end;

function TP3DVarianceShadowMap_DepthWrite.GetFramebufferBlurX: TP3DFrameBuffer;
begin
  Result:= TP3DFrameBuffer( FFramebufferBlurX.AsValue );
end;

function TP3DVarianceShadowMap_DepthWrite.GetFramebufferBlurY: TP3DFrameBuffer;
begin
  Result:= TP3DFrameBuffer( FFramebufferBlurY.AsValue );
end;

function TP3DVarianceShadowMap_DepthWrite.GetCamObj: TP3DObject;
begin
  Result:= TP3DObject( FCamObj.AsValue );
end;

procedure TP3DVarianceShadowMap_DepthWrite.SetCamObj(AValue: TP3DObject);
begin
  FCamObj.AsValue:= AValue;
end;

function TP3DVarianceShadowMap_DepthWrite.GetOffscreen: TP3DTexture;
begin
  Result:= FramebufferBlurX.Maps[ 0 ];
end;

function TP3DVarianceShadowMap_DepthWrite.GetRenderTarget: TP3DTexture;
begin
  Result:= Framebuffer.Maps[ 0 ];
end;

procedure TP3DVarianceShadowMap_DepthWrite.SetRenderTarget(AValue: TP3DTexture);
begin
  SetFramebufferTex( Framebuffer, AValue );
  UpdateOffscreen;
end;

function TP3DVarianceShadowMap_DepthWrite.GetMatBlurX: TP3DMaterialNode;
begin
  Result:= TP3DMaterialNode( FMatBlurX.AsValue );
end;

function TP3DVarianceShadowMap_DepthWrite.GetMatBlurY: TP3DMaterialNode;
begin
  Result:= TP3DMaterialNode( FMatBlurY.AsValue );
end;

function TP3DVarianceShadowMap_DepthWrite.GetFrameBuffer: TP3DFrameBuffer;
begin
  Result:= TP3DFrameBuffer( FFramebuffer.AsValue );
end;

function TP3DVarianceShadowMap_DepthWrite.GetMatDepth: TP3DMaterialNode;
begin
  Result:= TP3DMaterialNode( FMatDepth.AsValue );
end;

procedure TP3DVarianceShadowMap_DepthWrite.CreateLib;
var
  Cam, Obj: TP3DObject;
  _Scene: TP3DScene;
  MeshPlane: TP3DMesh;
  TexDepth: TP3DTexture;
  i: Integer;
begin
  inherited CreateLib;
  Cam:= TP3DObject.CreateBlock( Lib );
  Cam.Data:= TP3DCamera.CreateBlock( Lib );
  TP3DCamera( Cam.Data ).IsIsometric:= True;
  TP3DCamera( Cam.Data ).IsoZoom:= 0.03;
  TP3DCamera( Cam.Data ).Far:= 20;
  TP3DCamera( Cam.Data ).CustomProj:= True;

  Cam:= TP3DObject.CreateBlock( Lib );
  Cam.Transform.Position:= vec3( 0, 0, 1 );
  Cam.Transform.Rotation:= vec3( 0, 0, 0 );
  Cam.Data:= TP3DCamera.CreateBlock( Lib );
  TP3DCamera( Cam.Data ).IsIsometric:= True;

  _Scene:= TP3DScene.CreateBlock( Lib );

  _Scene.Objects.Add( Cam );

  Obj:= TP3DObject.CreateBlock( Lib );
  _Scene.Objects.Add( Obj );
  MeshPlane:= P3DMeshCreatePlane( vec2( 1 ), Lib );
  Obj.Data:= MeshPlane;

  for i:= 0 to NumCascades - 1 do begin
    TexDepth:= TP3DTexture.CreateFromEmpty( 2048, 2048, 2, dtFloat );
    TexDepth.Name:= 'TexDepth' + IntToStr( i );
    FTextures.Add( TexDepth );
  end;

end;

procedure TP3DVarianceShadowMap_DepthWrite.PopulateCommandList;
begin
  TP3DRenderCommandFramebuffer.Create( Framebuffer, FCommandList );
  TP3DRenderCommandClear.Create([ rccmColor, rccmDepth ], White, FCommandList );
  TP3DRenderCommandSetActiveMat.Create( MatDepth, FCommandList );
  FRenderScene:= TP3DRenderCommandRenderList.Create( nil, Lib.Objects.First(), [ rlfMeshes, rlfDepthTest, rlfShadowCasters ], FCommandList );
  TP3DRenderCommandFramebufferUnbind.Create( Framebuffer, FCommandList, True );
  TP3DRenderCommandFramebuffer.Create( FramebufferBlurX, FCommandList );
  TP3DRenderCommandSetActiveMat.Create( MatBlurX, FCommandList );
  TP3DRenderCommandRenderList.Create( Lib.Scenes.First(), Lib.Scenes.First().Objects.First(), [ rlfMeshes, rlfShadowCasters, rlfShadowReceivers, rlfNonShadowCasters, rlfDepthTest ], FCommandList );
  TP3DRenderCommandFramebufferUnbind.Create( FramebufferBlurX, FCommandList, True );
  TP3DRenderCommandFramebuffer.Create( FramebufferBlurY, FCommandList );
  TP3DRenderCommandSetActiveMat.Create( MatBlurY, FCommandList );
  TP3DRenderCommandRenderList.Create( Lib.Scenes.First(), Lib.Scenes.First().Objects.First(), [ rlfMeshes, rlfShadowCasters, rlfShadowReceivers, rlfNonShadowCasters, rlfDepthTest ], FCommandList );
  TP3DRenderCommandFramebufferUnbind.Create( FramebufferBlurY, FCommandList, True );
end;

procedure TP3DVarianceShadowMap_DepthWrite.CreateMat;
var
  blur: TP3DNodeClone;
begin
  FMatDepth.AsValue:= TP3DMaterialNode.CreateBlock( Lib );
  MatDepth.MakeRootNode( 'output' );
  MatDepth.RootNode.FindInput( 'Color' ).Connected:= MatDepth.NodeTree.AddCopy( 'variance_depth_write' ).FindOutput( 'Depth' );

  FMatBlurX.AsValue:= TP3DMaterialNode.CreateBlock( Lib );
  MatBlurX.MakeRootNode( 'output' );
  blur:= MatBlurX.NodeTree.AddCopy( 'two_pass_blur' );
  MatBlurX.RootNode.FindInput( 'Color' ).Connected:= blur.FindOutput( 'hblur' );
  blur.FindInput( 'UV' ).Connected:= MatBlurX.NodeTree.AddCopy( 'texcoord' ).FindOutput( 'UV' );
  blur.FindInput( 'Map' ).Connected:= MatBlurX.Maps.AddCopy( 'texture2D' ).FindOutput( 'Texture' );
  blur.FindInput( 'Radius' ).ValueProp.AsString:= '5';

  FMatBlurY.AsValue:= TP3DMaterialNode.CreateBlock( Lib );
  MatBlurY.MakeRootNode( 'output' );
  blur:= MatBlurY.NodeTree.AddCopy( 'two_pass_blur' );
  MatBlurY.RootNode.FindInput( 'Color' ).Connected:= blur.FindOutput( 'vblur' );
  blur.FindInput( 'UV' ).Connected:= MatBlurY.NodeTree.AddCopy( 'texcoord' ).FindOutput( 'UV' );
  blur.FindInput( 'Map' ).Connected:= MatBlurY.Maps.AddCopy( 'texture2D' ).FindOutput( 'Texture' );
  blur.FindInput( 'Radius' ).ValueProp.AsString:= '5';
end;

procedure TP3DVarianceShadowMap_DepthWrite.UpdateOffscreen;
var
  Tex, NewTex: TP3DTexture;
begin
  Lib.Textures.Clear;
  Tex:= RenderTarget;

  if ( Assigned( Tex )) then begin
    Framebuffer.AttachDepth( Tex.Width, Tex.Height );
    Framebuffer.CheckFBO;
    NewTex:= TP3DTexture.CreateFromEmpty( Tex.Width, Tex.Height, Tex.NumChannels, Tex.DataType, Lib );
    NewTex.WrapModeX:= twClampToEdge;
    NewTex.WrapModeY:= twClampToEdge;
    SetFramebufferTex( FramebufferBlurX, NewTex );
    SetFramebufferTex( FramebufferBlurY, Tex );
    MatBlurX.Maps[ 0 ].Map:= Tex;
    MatBlurY.Maps[ 0 ].Map:= NewTex;
  end;
end;

procedure TP3DVarianceShadowMap_DepthWrite.SetFramebufferTex(AFramebuffer: TP3DFrameBuffer; ATex: TP3DTexture);
begin
  if ( AFramebuffer.Maps.Count > 0 ) then
    AFramebuffer.Maps[ 0 ]:= ATex
  else
    AFramebuffer.Maps.Add( ATex );
end;

constructor TP3DVarianceShadowMap_DepthWrite.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);

  FFramebuffer:= TP3DStreamableContainer.Create( 'Framebuffer', TP3DFrameBuffer, smText );
  Properties.Add( FFramebuffer );
  TP3DFrameBuffer.Create( FFramebuffer );

  FFramebufferBlurX:= TP3DStreamableContainer.Create( 'FramebufferBlurX', TP3DFrameBuffer, smText );
  Properties.Add( FFramebufferBlurX );
  TP3DFrameBuffer.Create( FFramebufferBlurX );

  FFramebufferBlurY:= TP3DStreamableContainer.Create( 'FramebufferBlurY', TP3DFrameBuffer, smText );
  Properties.Add( FFramebufferBlurY );
  TP3DFrameBuffer.Create( FFramebufferBlurY );

  FMatDepth:= TP3DStreamablePointer.Create( 'MatDepth', TP3DMaterialNode, smText );
  Properties.Add( FMatDepth );

  FMatBlurX:= TP3DStreamablePointer.Create( 'MatBlurX', TP3DMaterialNode, smText );
  Properties.Add( FMatBlurX );

  FMatBlurY:= TP3DStreamablePointer.Create( 'MatBlurY', TP3DMaterialNode, smText );
  Properties.Add( FMatBlurY );

  FLightObj:= TP3DStreamablePointer.Create( 'LightObj', TP3DObject, smText );
  Properties.Add( FLightObj );

  FCamObj:= TP3DStreamablePointer.Create( 'CamObj', TP3DObject, smText );
  Properties.Add( FCamObj );

  FTextures:= ( specialize gP3DStreamablePointerList < TP3DTexture >).Create( 'Textures' );
  Properties.Add( FTextures );

  CreateLib;
  CreateMat;
  PopulateCommandList;
end;

destructor TP3DVarianceShadowMap_DepthWrite.Destroy;
begin
  MatDepth.Free;
  inherited Destroy;
end;

procedure TP3DVarianceShadowMap_DepthWrite.Render;
var
  m, mlight: TMat4;
  NDC, CascadeNDC: TP3DCameraFrustumCube;
  AABB: TP3DAABB;
  CamShift, Extent: TVec3;
  i: Integer;
  function GetCascadeFromNDC( NDC: TP3DCameraFrustumCube; PNear: Float; PFar: Float ): TP3DCameraFrustumCube;
  var
    i: Integer;
  begin
    for i:= 0 to 3 do
      Result[ i ]:= mix( NDC[ i ], NDC[ i + 4 ], PNear );
    for i:= 0 to 3 do
      Result[ i + 4 ]:= mix( NDC[ i ], NDC[ i + 4 ], PFar );
  end;

begin
  mat4inverse( mat4translate( CamObj.Transform.Position ) * mat4( mat3( LightObj.Transform.Quaternion )), mlight );
  //CamShift:= -CamObj.Transform.Direction * mix( TP3DCamera( CamObj.Data ).Near, TP3DCamera( CamObj.Data ).Far, 0.5 );
  //TP3DCamera( FRenderScene.CameraObj.Data ).UpdateMatrices( FRenderScene.CameraObj );
  //TP3DCamera( CamObj.Data ).UpdateMatrices( CamObj );

  if ( LightObj.Data is TP3DLight ) then begin
    m:= mlight * TP3DCamera( CamObj.Data ).InvView * TP3DCamera( CamObj.Data ).InvProj;
    NDC:= TP3DCamera( CamObj.Data ).CalculateCameraFrustumCornersNDCTransform( m );
    for i:= 0 to 0 do begin // NumCascades - 1 do begin
      RenderTarget:= FTextures.AsValues[ i ];
      CascadeNDC:= GetCascadeFromNDC( NDC, 0.95, 1 );
      AABB:= P3DCalcNDCBoundingBox( CascadeNDC );

      //mlight:= mlight * mat4translate( AABB.Position );

      mlight:= mat4translate( CamObj.Transform.Position ) * mat4( mat3( LightObj.Transform.Matrix ));
      FRenderScene.CameraObj.Transform.Matrix:= mlight;
      TP3DCamera( FRenderScene.CameraObj.Data ).Proj:=
      mat4orthoRH( AABB.Min.X, AABB.Max.X, AABB.Min.Y, AABB.Max.Y, -AABB.Max.Z, -AABB.Min.Z );
      //mat4orthoRH( -TP3DCamera( CamObj.Data ).IsoZoom, TP3DCamera( CamObj.Data ).IsoZoom, -TP3DCamera( CamObj.Data ).IsoZoom, TP3DCamera( CamObj.Data ).IsoZoom, 0.1, TP3DCamera( CamObj.Data ).IsoZoom ); //

      TP3DCamera( FRenderScene.CameraObj.Data ).IsoZoom:= 1;
      TP3DCamera( FRenderScene.CameraObj.Data ).Near:= 0;
      TP3DCamera( FRenderScene.CameraObj.Data ).Far:= 1;
      TP3DCamera( FRenderScene.CameraObj.Data ).UpdateMatrices( FRenderScene.CameraObj );
      TP3DLight( LightObj.Data ).LightMatrix:= TP3DCamera( FRenderScene.CameraObj.Data ).Proj * TP3DCamera( FRenderScene.CameraObj.Data ).View;

      inherited Render;
    end;

  end;
  //glEnable( GL_CULL_FACE );
  //glCullFace( GL_FRONT );
  //glDepthFunc( GL_GREATER );
  //Framebuffer.BindAll();
  //glClearDepth( 0.0 );

  //Framebuffer.Unbind();
  //glCullFace( GL_BACK );
  //glDisable( GL_CULL_FACE );
  //glDepthFunc( GL_LESS );
  //glClearDepth( 1.0 );


  //if ( Assigned( RenderTarget )) then
  //  RenderTarget.UpdateMipMaps;
end;

{$ENDIF}

