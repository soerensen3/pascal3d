{ p3d.core.rendercommand.inc }

{$IFDEF INTERFACE}
type
  TP3DPlotCommandSetting = class;
  TP3DPlotCommandDefault = class;

  { TP3DPlotCommandContext }

  TP3DPlotCommandContext = class
    LayerDefault: TP3DPlotCommandDefault;
    NumTextures: Integer;
    NumPointLights: Integer;
    NumDirectionalLights: Integer;
    Shader: TP3DShader;
  end;

  TP3DPlotCommand = class ( TPersistent )
    private
      RefCount: Integer;

      procedure AddRef;
      procedure Release;

    public
      constructor Create;
      destructor Destroy; override;
      procedure Execute( AContext: TP3DPlotCommandContext ); virtual; abstract;
      function Debug: String; virtual abstract;
  end;

  { gP3DPlotList }
  {
  generic gP3DPlotList < T: TP3DPlotCommand > = record
    private type
    //  TAutoType = specialize TAuto < T >;
    //  TAutoTypeArray = array of TAutoType;
      TPlotList = specialize gP3DPlotList < T >;

    private
      FItems: array of T;//TAutoTypeArray;
      //RefCount: PLongint;
      function GetCount: Integer;
      function GetItems( AIndex: Integer ): T;

    public
      function Debug: String;
      procedure Execute;
      procedure Execute( AContext: TP3DPlotCommandContext );
      procedure SmartFinalize();
      procedure Append( Item: T );
      procedure Append( List: TPlotList );

      class operator Initialize(var aRec: specialize gP3DPlotList < T > );
      class operator Finalize(var aRec: specialize gP3DPlotList < T > );
      class operator AddRef(var aRec: specialize gP3DPlotList < T > );
      class operator Copy(constref aSource: specialize gP3DPlotList < T >; var aDest: specialize gP3DPlotList < T > );
      //procedure Assign(const aValue: T );
      //class operator +( const a, b: TPlotList ): TPlotList;

      property Count: Integer read GetCount;
      property Items[ AIndex: Integer ]: T read GetItems;
  end; }

  { gP3DPlotList }

  generic gP3DPlotList < T: TP3DPlotCommand > = class
    private type
      TPlotList = specialize gP3DPlotList < T >;

    private
      FItems: array of T;
      FOwnedByUser: Boolean;
      function GetCount: Integer;
      function GetItems( AIndex: Integer ): T;
      {%H-}constructor CreateTemporary;

    public
      constructor Create( const AOwnedByUser: Boolean = True );
      destructor Destroy; override;
      function CheckDestroy: Boolean;

      function Debug: String;
      procedure Execute;
      procedure Execute( AContext: TP3DPlotCommandContext );
      procedure Append( Item: T );
      procedure Append( List: TPlotList );
      procedure Clear;

      property Count: Integer read GetCount;
      property Items[ AIndex: Integer ]: T read GetItems;
      property OwnedByUser: Boolean read FOwnedByUser write FOwnedByUser;
  end;

  { TP3DPlotCommandSetting }

  TP3DPlotCommandSetting = class ( TP3DPlotCommand )
    public
      function Debug: String; override;

      function Clone: TP3DPlotCommandSetting; virtual; abstract;
  end;

  { TP3DPlotListSetting }

  TP3DPlotListSetting = specialize gP3DPlotList < TP3DPlotCommandSetting >;

  { TP3DPlotCommandLayer }
  TP3DPlotCommandLayer = class ( TP3DPlotCommand )
    private
      FShaderSettings: TP3DPlotListSetting;

      procedure SetShaderSettings(AValue: TP3DPlotListSetting);

    public
      destructor Destroy; override;

      procedure Execute( AContext: TP3DPlotCommandContext ); override;
      function Clone: TP3DPlotCommandLayer; virtual; abstract;
      function Debug: String; override;

      property ShaderSettings: TP3DPlotListSetting read FShaderSettings write SetShaderSettings;
  end;

  { TP3DPlotListLayer }

  TP3DPlotListLayer = specialize gP3DPlotList < TP3DPlotCommandLayer >;

  IP3DPlottableLayer = interface
    ['{C0045712-CFEE-4AF8-8198-66E71AEC555C}']

    function GetLayer( ASettings: TP3DPlotListSetting ): TP3DPlotCommandLayer;
  end;

  IP3DPlottableSettings = interface
    ['{9B61C114-C67F-423F-A723-C45E3EBF2BC0}']

    function GetSettings: TP3DPlotListSetting;
  end;

  { TP3DPlotCommandDefault }

  TP3DPlotCommandDefault = class ( TP3DPlotCommandLayer )
    public
      constructor Create( AShaderSettings: TP3DPlotListSetting );

      procedure Execute(AContext: TP3DPlotCommandContext); override;
      function Clone: TP3DPlotCommandLayer; override;
  end;

  //operator := ( a: TP3DPlotCommandSetting ): TP3DPlotListSetting;
  //operator := ( a: TP3DPlotCommandLayer ): TP3DPlotListLayer;

  var
    GlobalPlotCount: Integer = 0;
    GlobalPlotListCount: Integer = 0;


  { TP3DPlotCommandRenderList }
  {
  TP3DPlotCommandRenderList = class ( TP3DPlotCommand )
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
      constructor Create; override;
      constructor Create( AScene: TP3DScene; ACameraObj: TP3DObject; AFlags: TP3DRenderListFlags; const AOwner: IP3DStreamableContainer=nil);
      destructor Destroy; override;

      procedure Execute; override;
      procedure Refresh; override;
      procedure UpdateRenderList;

      property RenderList: TP3DRenderList read FRenderList;
      property Scene: TP3DScene read GetScene write SetScene;
      property CameraObj: TP3DObject read GetCameraObj write SetCameraObj;

    published
      property Flags: TP3DRenderListFlags read FFlags write SetFlags;
  end;

  TP3DPlotCommandClearFlag = (
    rccmDepth,
    rccmColor );

  TP3DPlotCommandClearFlags = set of TP3DPlotCommandClearFlag;

  { TP3DPlotCommandClear }

  TP3DPlotCommandClear = class ( TP3DPlotCommand )
    private
      FClearColor: TP3DColorRGBA;
      FClearFlags: TP3DPlotCommandClearFlags;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor Create( AClearFlags: TP3DPlotCommandClearFlags; AClearColor: TP3DColorRGBA; AOwner: IP3DStreamableContainer=nil);

      procedure Execute; override;

      property ClearColor: TP3DColorRGBA read FClearColor write FClearColor;

    published
      property ClearFlags: TP3DPlotCommandClearFlags read FClearFlags write FClearFlags;
  end;

  { TP3DPlotCommandFramebuffer }

  TP3DPlotCommandFramebuffer = class ( TP3DPlotCommand )
    private
      FFramebuffer: TP3DFrameBuffer;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor Create( AFramebuffer: TP3DFrameBuffer; const AOwner: IP3DStreamableContainer=nil);

      procedure Execute; override;

      property Framebuffer: TP3DFrameBuffer read FFramebuffer write FFramebuffer;
  end;

  { TP3DPlotCommandFramebuffer }

  { TP3DPlotCommandFramebufferTarget }

  TP3DPlotCommandFramebufferTarget = class ( TP3DPlotCommand )
    private
      FFramebuffer: TP3DFrameBuffer;
      FTargetIndex: Integer;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor Create( AFramebuffer: TP3DFrameBuffer; ATargetIndex: Integer; const AOwner: IP3DStreamableContainer=nil);

      procedure Execute; override;

      property Framebuffer: TP3DFrameBuffer read FFramebuffer write FFramebuffer;
      property TargetIndex: Integer read FTargetIndex write FTargetIndex;
  end;

  { TP3DPlotCommandFramebufferUnbind }

  TP3DPlotCommandFramebufferUnbind = class ( TP3DPlotCommand )
  private
    FUpdateMipMaps: Boolean;
    protected
      FFramebuffer: TP3DFrameBuffer;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor Create( AFramebuffer: TP3DFrameBuffer; const AOwner: IP3DStreamableContainer=nil; const AUpdateMipMaps: Boolean = False );

      procedure Execute; override;

      property Framebuffer: TP3DFrameBuffer read FFramebuffer write FFramebuffer;
      property UpdateMipMaps: Boolean read FUpdateMipMaps write FUpdateMipMaps;
  end;

  { TP3DPlotCommandPushVP }

  TP3DPlotCommandPushVP = class ( TP3DPlotCommand )
    private
      FRect: TP3DRect;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor Create( ARect: TP3DRect; AOwner: IP3DStreamableContainer=nil );

      procedure Execute; override;

      property Rect: TP3DRect read FRect write FRect;
  end;

  { TP3DPlotCommandPopVP }

  TP3DPlotCommandPopVP = class ( TP3DPlotCommand )
    public
      procedure Execute; override;
  end;

  { TP3DPlotCommandSetActiveMat }

  TP3DPlotCommandSetActiveMat = class ( TP3DPlotCommand )
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
  }

{$ENDIF}

{$IFDEF IMPLEMENTATION}




{ TP3DPlotCommandRenderList }
{
function TP3DPlotCommandRenderList.GetCameraObj: TP3DObject;
begin
  Result:= TP3DObject( FCameraObj.AsValue );
end;

function TP3DPlotCommandRenderList.GetScene: TP3DScene;
begin
  Result:= TP3DScene( FScene.AsValue );
end;

procedure TP3DPlotCommandRenderList.SetCameraObj(AValue: TP3DObject);
begin
  FCameraObj.AsValue:= AValue;
end;

procedure TP3DPlotCommandRenderList.SetFlags(AValue: TP3DRenderListFlags);
begin
  if FFlags=AValue then Exit;
  FFlags:=AValue;
  UpdateRenderList;
end;

procedure TP3DPlotCommandRenderList.SetScene(AValue: TP3DScene);
begin
  FScene.AsValue:= AValue;
  UpdateRenderList;
end;

constructor TP3DPlotCommandRenderList.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FRenderList:= TP3DRenderList.Create;
  FCameraObj:= TP3DStreamablePointer.Create( 'CameraObj', TP3DObject, smText );
  FScene:= TP3DStreamablePointer.Create( 'Scene', TP3DScene, smText );
  Properties.Add([ FScene, FCameraObj, TP3DPropAccessRTTI.Create( Self, 'Flags', smText )]);
end;

constructor TP3DPlotCommandRenderList.Create(AScene: TP3DScene; ACameraObj: TP3DObject; AFlags: TP3DRenderListFlags;
  const AOwner: IP3DStreamableContainer);
begin
  Create( AOwner );
  Scene:= AScene;
  CameraObj:= ACameraObj;
  Flags:= AFlags;
end;

destructor TP3DPlotCommandRenderList.Destroy;
begin
  FRenderList.Free;
  inherited Destroy;
end;

procedure TP3DPlotCommandRenderList.Execute;
begin
  Refresh;
  FRenderList.Render( CameraObj, Flags );
end;

procedure TP3DPlotCommandRenderList.Refresh;
begin
  UpdateRenderList;
  if ( Assigned( CameraObj ) and ( CameraObj.Data is TP3DCamera )) then
    TP3DCamera( CameraObj.Data ).UpdateMatrices( CameraObj );
end;

procedure TP3DPlotCommandRenderList.UpdateRenderList;
begin
  RenderList.Clear;
  if ( Assigned( Scene )) then
    Scene.MakeRenderList( Scene, Mat4Identity, FRenderList, Flags );
end;


{ TP3DPlotCommandClear }

constructor TP3DPlotCommandClear.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Properties.Add([ TP3DPropAccessColorRGBA.CreateField( 'ClearColor', @FClearColor, smText ),
                   TP3DPropAccessRTTI.Create( Self, 'ClearFlags', smText )]);
end;

constructor TP3DPlotCommandClear.Create(AClearFlags: TP3DPlotCommandClearFlags; AClearColor: TP3DColorRGBA;
  AOwner: IP3DStreamableContainer);
begin
  Create( AOwner );
  ClearFlags:= AClearFlags;
  ClearColor:= AClearColor;
end;

procedure TP3DPlotCommandClear.Execute;
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


{ TP3DPlotCommandFramebuffer }

constructor TP3DPlotCommandFramebuffer.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  //Properties.Add( TP3DPropAccessStreamable ); //Framebuffer
end;

constructor TP3DPlotCommandFramebuffer.Create(AFramebuffer: TP3DFrameBuffer; const AOwner: IP3DStreamableContainer);
begin
  Create( AOwner );
  Framebuffer:= AFramebuffer;
end;

procedure TP3DPlotCommandFramebuffer.Execute;
begin
  if ( Assigned( Framebuffer )) then
    Framebuffer.BindAll();
end;

{ TP3DPlotCommandFramebufferTarget }

constructor TP3DPlotCommandFramebufferTarget.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Properties.Add( TP3DPropAccessInt.CreateField( 'TargetIndex', @FTargetIndex, smText ));
end;

constructor TP3DPlotCommandFramebufferTarget.Create(AFramebuffer: TP3DFrameBuffer; ATargetIndex: Integer;
  const AOwner: IP3DStreamableContainer);
begin
  Create( AOwner );
  Framebuffer:= AFramebuffer;
  TargetIndex:= ATargetIndex;
end;

procedure TP3DPlotCommandFramebufferTarget.Execute;
begin
  if ( Assigned( Framebuffer )) then
    Framebuffer.Bind( TargetIndex );
end;


{ TP3DPlotCommandFramebufferUnbind }

constructor TP3DPlotCommandFramebufferUnbind.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
end;

constructor TP3DPlotCommandFramebufferUnbind.Create(AFramebuffer: TP3DFrameBuffer;
  const AOwner: IP3DStreamableContainer; const AUpdateMipMaps: Boolean);
begin
  Create( AOwner );
  Framebuffer:= AFramebuffer;
  FUpdateMipMaps:= AUpdateMipMaps;
end;

procedure TP3DPlotCommandFramebufferUnbind.Execute;
var
  i: Integer;
begin
  if ( Assigned( Framebuffer )) then begin
    Framebuffer.Unbind();
    if ( UpdateMipMaps ) then
      for i:= 0 to Framebuffer.Maps.Count - 1 do
        Framebuffer.Maps[ i ].UpdateMipMaps;
  end;
end;


{ TP3DPlotCommandSetActiveMat }

function TP3DPlotCommandSetActiveMat.GetMaterial: TP3DMaterial;
begin
  Result:= TP3DMaterial( FMaterial.AsValue );
end;

procedure TP3DPlotCommandSetActiveMat.SetMaterial(AValue: TP3DMaterial);
begin
  FMaterial.AsValue:= AValue;
end;

constructor TP3DPlotCommandSetActiveMat.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FMaterial:= TP3DStreamablePointer.Create( 'Material', TP3DMaterial, smText );
  Properties.Add( FMaterial );
end;

constructor TP3DPlotCommandSetActiveMat.Create(AMaterial: TP3DMaterial; const AOwner: IP3DStreamableContainer);
begin
  Create( AOwner );
  Material:= AMaterial;
end;

procedure TP3DPlotCommandSetActiveMat.Execute;
begin
  if ( Assigned( Material )) then
    Material.PassToShader();
end;

{ TP3DPlotCommandPopVP }

procedure TP3DPlotCommandPopVP.Execute;
begin
  P3DViewports.Pop;
end;

{ TP3DPlotCommandPushVP }

constructor TP3DPlotCommandPushVP.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Properties.Add( TP3DPropAccessVec4.CreateField( 'Rect', @FRect, smText ));
end;

constructor TP3DPlotCommandPushVP.Create(ARect: TP3DRect; AOwner: IP3DStreamableContainer);
begin
  Create( AOwner );
  FRect:= ARect;
end;

procedure TP3DPlotCommandPushVP.Execute;
begin
  P3DViewports.Push( FRect );
end;

class operator gP3DPlotList.Initialize(var aRec: specialize gP3DPlotList<T>);
begin
  aRec.FItems := nil;
  //SetLength( aRec.FItems, 0 );
  WriteLn( 'class operator Initialize: ', IntToHex( Integer( Pointer( @aRec )), 8 ));
end;

class operator gP3DPlotList.Finalize(var aRec: specialize gP3DPlotList<T>);
begin
  WriteLn( 'class operator Finalize: ', IntToHex( Integer( Pointer( @aRec )), 8 ));
  aRec.SmartFinalize();
end;

class operator gP3DPlotList.AddRef(var aRec: specialize gP3DPlotList<T>);
var
  i: Integer;
begin
  WriteLn( 'class operator gP3DPlotList.AddRef(var aRec: specialize gP3DPlotList<T>);' );
  for i:= low( aRec.FItems ) to high( aRec.FItems ) do
    aRec.FItems[ i ].AddRef;
  //if aRec.RefCount <> nil then
  //  InterLockedIncrement(aRec.RefCount^);
end;

class operator gP3DPlotList.Copy(constref aSource: specialize gP3DPlotList<T>; var aDest: specialize gP3DPlotList<T>);
var
  i: Integer;
begin
  WriteLn( 'class operator gP3DPlotList.Copy(constref aSource: specialize gP3DPlotList<T>; var aDest: specialize gP3DPlotList<T>);' );
  for i:= low( aSource.FItems ) to high( aSource.FItems ) do
    aDest.Append( aSource.FItems[ i ]);
  //if aDest.RefCount <> nil then
  //  aDest.SmartFinalize();
  //if aSource.RefCount <> nil then
  //  InterLockedIncrement(aSource.RefCount^);
  //aDest.RefCount := aSource.RefCount;
  //aDest.FItems := aSource.FItems;
end;

}
{ gP3DPlotList }

function gP3DPlotList.GetCount: Integer;
begin
  Result:= Length( FItems );
end;

function gP3DPlotList.GetItems( AIndex: Integer ): T;
begin
  Result:= FItems[ AIndex ];
end;

constructor gP3DPlotList.CreateTemporary;
begin
  Create;
  OwnedByUser:= False;
end;

constructor gP3DPlotList.Create( const AOwnedByUser: Boolean = True );
begin
  Inc( GlobalPlotListCount );
  FItems := nil;
  OwnedByUser:= AOwnedByUser;
  WriteLn( 'Create PlotList: ', ClassName );
end;

destructor gP3DPlotList.Destroy;
var
  i: Integer;
begin
  Dec( GlobalPlotListCount );
  WriteLn( 'Destroying PlotList: ', ClassName );
  Clear;

  inherited Destroy;
end;

function gP3DPlotList.CheckDestroy: Boolean;
begin
  if ( not Assigned( Self )) then
    exit;
  Result:= not OwnedByUser;
  if ( Result ) then
    Free;
end;

function gP3DPlotList.Debug: String;
var
  i: Integer;
begin
  Result:= '';
  if ( not Assigned( Self )) then
    exit;
  for i:= 0 to Count - 1 do begin
    Result+= FItems[ i ].Debug + LineEnding;
  end;
end;

procedure gP3DPlotList.Execute;
var
  Context: TP3DPlotCommandContext;
begin
  Context:= TP3DPlotCommandContext.Create;
  try
    Execute( Context );
  finally
    FreeAndNil( Context );
  end;
end;

procedure gP3DPlotList.Execute(AContext: TP3DPlotCommandContext);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do begin
    FItems[ i ].Execute( AContext );
  end;
end;

{procedure gP3DPlotList.SmartFinalize();
var
  i: Integer;
begin
  WriteLn( 'procedure gP3DPlotList.SmartFinalize();' );
  //SetLength( FItems, 0 );
  for i:= low( FItems ) to high( FItems ) do
    FItems[ i ].Release;
  SetLength( FItems, 0 );
  {if RefCount <> nil then
    if InterLockedDecrement(RefCount^)=0 then begin
      FreeMem(RefCount);
      //if ( Assigned( FFirst )) then
      //  WriteLn( 'Freeing FFirst: ', FFirst.ClassName );
      SetLength( FItems, 0 );
    end;}
end;
}
procedure gP3DPlotList.Append(Item: T);
var
  L: Integer;
begin
  if ( not Assigned( Item )) then
    exit;
  L:= Length( FItems );
  SetLength( FItems, L + 1 );
  FItems[ L ]:= Item;
  Item.AddRef;
end;

procedure gP3DPlotList.Append(List: TPlotList);
var
  l: Integer;
begin
  if ( Assigned( List )) then
    for l:= 0 to high( List.FItems ) do
      Append( List.FItems[ l ]);
end;

procedure gP3DPlotList.Clear;
var
  i: Integer;
begin
  for i:= low( FItems ) to high( FItems ) do
    FItems[ i ].Release;
  SetLength( FItems, 0 );
end;




{procedure gP3DPlotList.Assign(const aValue: T );
begin
  if RefCount <> nil then
    SmartFinalize();

  GetMem(RefCount, SizeOf(LongInt));
  RefCount^ := 0;

  InterLockedIncrement(RefCount^);
  SetLength( FItems, 1 );
  FItems[ 0 ].I:= aValue;
end;}

{class operator gP3DPlotList.+( const a, b: TPlotList ): TPlotList;
var
  l: Integer;
begin
  {if ( Length( a.FItems ) = 0 ) then
    Result:= b
  else if ( Length( b.FItems ) = 0 ) then
    Result:= a
  else begin}
  WriteLn( 'class operator gP3DPlotList<', T.ClassName, '>.+(a, b: specialize gP3DPlotList<',T.ClassName,'>): specialize gP3DPlotList <',T.ClassName,'>;' );
    initialize( Result );//default( TPlotList );  //initialize Result
    for l:= 0 to high( a.FItems ) do
      Result.Append( a.FItems[ l ]);
    for l:= 0 to high( b.FItems ) do
      Result.Append( b.FItems[ l ]);
    //WriteLn( 'a = ', a.Debug );
    //WriteLn( 'b = ', b.Debug );
    //WriteLn( 'Result = ', Result.Debug );
    //l:= Length( a.FItems ) + Length( b.FItems );
    //SetLength( Result.FItems, l );
    //Move( a.FItems[ 0 ], Result.FItems[ 0 ], Length( a.FItems ) * SizeOf( TAutoType ));
    //Move( b.FItems[ 0 ], Result.FItems[ Length( a.FItems )], Length( b.FItems ) * SizeOf( TAutoType ));
  //end;
end;
}

{ TP3DPlotCommandSetting }

function TP3DPlotCommandSetting.Debug: String;
begin
  Result:= '  ' + ClassName;
end;

{ TP3DPlotCommandLayer }

procedure TP3DPlotCommandLayer.SetShaderSettings(AValue: TP3DPlotListSetting);
begin
  FShaderSettings:=AValue;
end;

destructor TP3DPlotCommandLayer.Destroy;
begin
  FShaderSettings.CheckDestroy;
  inherited Destroy;
end;

procedure TP3DPlotCommandLayer.Execute(AContext: TP3DPlotCommandContext);
begin
  AContext.NumDirectionalLights:= 0;
  AContext.NumPointLights:= 0;
  AContext.NumTextures:= 0;
  ShaderSettings.Execute( AContext );
end;

function TP3DPlotCommandLayer.Debug: String;
begin
  Result:= ClassName + LineEnding + ShaderSettings.Debug;
end;

{ TP3DPlotCommandDefault }

constructor TP3DPlotCommandDefault.Create(AShaderSettings: TP3DPlotListSetting);
begin
  inherited Create;
  ShaderSettings:= AShaderSettings;
end;

procedure TP3DPlotCommandDefault.Execute(AContext: TP3DPlotCommandContext);
begin
  AContext.LayerDefault:= nil;
  inherited Execute(AContext);
  AContext.LayerDefault:= Self;
end;

function TP3DPlotCommandDefault.Clone: TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandDefault.Create( ShaderSettings );
end;

{ TP3DPlotCommand }

procedure TP3DPlotCommand.AddRef;
begin
  Inc( RefCount );
  WriteLn( ClassName + '.AddRef: ', RefCount );
end;

procedure TP3DPlotCommand.Release;
begin
  Dec( RefCount );
  WriteLn( ClassName + '.Release: ', RefCount );
  if ( RefCount = 0 ) then
    Free;
end;

constructor TP3DPlotCommand.Create;
begin
  Inc( GlobalPlotCount );
  inherited Create;
  RefCount:= 0;
end;

destructor TP3DPlotCommand.Destroy;
begin
  Dec( GlobalPlotCount );
  inherited Destroy;
end;

{$ENDIF}

