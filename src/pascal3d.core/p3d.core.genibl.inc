{ include.inc }

{$IFDEF INTERFACE}
type

  { TP3DEffect }

  TP3DEffect = class ( TP3DStreamable )
    protected
      FCommandList: TP3DRenderCommandContainerList;
      FLib: TP3DStreamablePointer;

      function GetLib: TP3DLibrary;

      procedure CreateLib; virtual;
      procedure PopulateCommandList; virtual; abstract;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      property Lib: TP3DLibrary read GetLib;
  end;

  { TP3DIBLRenderAtmosphere }

  TP3DIBLRenderAtmosphere = class ( TP3DEffect )
    protected
      FEnableFrameBuffer: TP3DRenderCommandFramebuffer;
      FFrameBuffer: TP3DFrameBuffer;
      FSunDir: TVec3;

      FIBLSpec,
      FMatAtmosphere,
      FCamAtmosphere,
      FMeshPlane: TP3DStreamablePointer;
      FUnSunDir: TP3DStreamablePointer;

      function GetCamAtmosphere: TP3DObject;
      procedure SetSunDir(AValue: TVec3);
      function GetIBLSpec: TP3DTexture;
      function GetMatAtmosphere: TP3DMaterialNode;
      function GetMeshPlane: TP3DMesh;
      procedure SetIBLSpec(AValue: TP3DTexture);
      function GetUnSunDir: TP3DNodeClone;

      property FrameBuffer: TP3DFrameBuffer read FFrameBuffer;
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

      procedure Render;

      property IBLSpec: TP3DTexture read GetIBLSpec write SetIBLSpec;
      property SunDir: TVec3 read FSunDir write SetSunDir;
  end;

  { TP3DIBLDiffuseConv }

  TP3DIBLDiffuseConv = class ( TP3DEffect )
    protected
      FIBLSrc, FIBLDiffuse, FMatConv: TP3DStreamablePointer;
      FFramebuffer: TP3DFrameBuffer;

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

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Render;

    published
      property IBLSrc: TP3DTexture read GetIBLSrc write SetIBLSrc;
      property IBLDiffuse: TP3DTexture read GetIBLDiffuse write SetIBLDiffuse;
  end;

  { TP3DIBLSpecularConv }

  TP3DIBLSpecularConv = class ( TP3DEffect )
    protected
      FRoughness: Float;
      FResolution: Integer;
      FFramebuffers: array of TP3DFrameBuffer;
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
      function GetFramebuffer( Index: Integer ): TP3DFrameBuffer;

      property MatConv: TP3DMaterialNode read GetMatConv;
      property MatCopy: TP3DMaterialNode read GetMatCopy;
      property Framebuffers[ Index: Integer ]: TP3DFrameBuffer read GetFramebuffer;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Render;

      property IBLSrc: TP3DTexture read GetIBLSrc write SetIBLSrc;
      property IBLSpec: TP3DTexture read GetIBLSpec write SetIBLSpec;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DIBLRenderAtmosphere }

function TP3DIBLRenderAtmosphere.GetUnSunDir: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FUnSunDir.AsValue );
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
  FreeAndNil( FFrameBuffer );
  FFrameBuffer:= TP3DFrameBuffer.Create();
  if ( Assigned( AValue )) then begin
    FrameBuffer.AttachColor( AValue );
    FrameBuffer.AttachDepth( TP3DRenderBuffer.Create( AValue.Width, AValue.Height ));
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
  FCommandList:= TP3DRenderCommandContainerList.Create( 'CommandList' );
  FCamAtmosphere:= TP3DStreamablePointer.Create( 'CamAtmosphere', TP3DObject );
  FUnSunDir:= TP3DStreamablePointer.Create( 'UnSunDir', TP3DNodeClone );
  Properties.Add([ FIBLSpec, FMatAtmosphere, FMeshPlane, FCommandList, FCamAtmosphere, FUnSunDir ]);
  FFrameBuffer:= TP3DFrameBuffer.Create();
  FrameBuffer.Unbind();
  CreateLib;
  PopulateCommandList;
  FSunDir:= vec3( 0, 0, 1 );
end;

destructor TP3DIBLRenderAtmosphere.Destroy;
begin
  FCommandList.Clear;
  FreeAndNil( FFrameBuffer );
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
    FRoughness += 1 / Length( FFramebuffers );
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

function TP3DIBLSpecularConv.GetFramebuffer( Index: Integer ): TP3DFrameBuffer;
begin
  if ( InRange( Index, 0, Length( FFramebuffers ) - 1 )) then
    Result:= FFramebuffers[ Index ]
  else
    Result:= nil;
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
  for i:= 0 to High( FFramebuffers ) do
    FFramebuffers[ i ].Free;
  SetLength( FFramebuffers, 10 );
  for i:= 0 to High( FFramebuffers ) do begin
    FFramebuffers[ i ]:= TP3DFrameBuffer.Create();
    FFramebuffers[ i ].AutoViewPort:= False;
    FFramebuffers[ i ].AttachColor( IBLSpec, i );
    FFramebuffers[ i ].CheckFBO;
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
  for i:= 0 to High( FFramebuffers ) do begin
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

  CreateLib;
  FCommandList:= TP3DRenderCommandContainerList.Create( 'CommandList' );
  Properties.Add( FCommandList );
  PopulateCommandList;
end;

destructor TP3DIBLSpecularConv.Destroy;
var
  i: Integer;
begin
  for i:= 0 to High( FFramebuffers ) do
    FFramebuffers[ i ].Free;
  inherited Destroy;
end;

procedure TP3DIBLSpecularConv.Render;
begin
  if ( not Assigned( IBLSrc )) then
    exit;
  FRoughness:= 1 / Length( FFramebuffers );
  FResolution:= IBLSrc.Width div 2;
  FCommandList.Execute;
end;


{ TP3DIBLDiffuseConv }

function TP3DIBLDiffuseConv.GetMatConv: TP3DMaterialNode;
begin
  Result:= TP3DMaterialNode( FMatConv.AsValue );
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
  FFramebuffer:= TP3DFrameBuffer.Create();
  FFramebuffer.AutoViewPort:= False;
  FFramebuffer.AttachColor( IBLDiffuse );
  FFramebuffer.CheckFBO;
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

  TP3DRenderCommandFramebuffer.Create( FFramebuffer, FCommandList );
  TP3DRenderCommandPushVP.Create( P3DRect( vec2( 0 ), vec2( IBLDiffuse.Width, IBLDiffuse.Height )), FCommandList );
  TP3DRenderCommandRenderList.Create( TP3DScene( Lib.Scenes.First()), Lib.Objects[ 0 ], P3DRenderListFlagsDefault, FCommandList );
  TP3DRenderCommandPopVP.Create( FCommandList );
  TP3DRenderCommandFramebufferUnbind.Create( FFramebuffer, FCommandList );
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

  CreateLib;
  FCommandList:= TP3DRenderCommandContainerList.Create( 'CommandList' );
  Properties.Add( FCommandList );
  PopulateCommandList;
end;

procedure TP3DIBLDiffuseConv.Render;
begin
  FCommandList.Execute;
end;

{ TP3DEffect }

function TP3DEffect.GetLib: TP3DLibrary;
begin
  Result:= TP3DLibrary( FLib.AsValue );
end;

procedure TP3DEffect.CreateLib;
begin
  FLib.AsValue:= TP3DLibrary.Create();
end;

constructor TP3DEffect.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FLib:= TP3DStreamablePointer.Create( 'Lib', TP3DLibrary );
  Properties.Add( FLib );
end;

destructor TP3DEffect.Destroy;
begin
  if ( Assigned( FLib ) and Assigned( Lib )) then
    Lib.Free;
  inherited Destroy;
end;


{$ENDIF}

