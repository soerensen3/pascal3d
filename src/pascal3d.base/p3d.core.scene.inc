//p3dscene.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
type
  { TP3DScene }

  TP3DSceneRenderCallback = procedure ( world: TMat4; AObject: TP3DObject ) of object;
  TP3DScene = class( TP3DDataBlock, IP3DRenderable, IP3DPlottableLayer )
    private
      FCam: TP3DStreamablePointer;
      FIsDesignTime: Boolean;
      FInheritedDesignTime: Boolean;
      FLights: TP3DLightPointerList;
      FObjects: TP3DObjectPointerList;
      FActiveCam: TP3DObject;
      FOnRenderCallback: TP3DSceneRenderCallback;
      FRootScene: TP3DScene;
      FPlotList: TP3DPlotListLayer;
      FLightList: TP3DPlotListSetting;

      function GetCam: TP3DObject;
      procedure SetCam( AValue: TP3DObject );
      procedure RenderCallback( world: TMat4; AObject: TP3DObject ); virtual;
      procedure OnObjChange( Sender: TObject; Item: TP3DStreamable; AAction: TP3DListOnChangeAction );

    public
      procedure UpdatePlotList;

    protected
      property ActiveCam: TP3DObject read FActiveCam write FActiveCam;
      property RootScene: TP3DScene read FRootScene write FRootScene;
      property Lights: TP3DLightPointerList read FLights write FLights;

    public
      class function GetContainer( ALib: TP3DLibrary): TP3DPropertyAccess; override;
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      //constructor Create( AFileName: String; const AWatchFileForChanges: Boolean = False; const AData: TP3DLibrary = nil );
      destructor Destroy; override;

      procedure UpdateLights;

//      function Debug: String;

      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
      procedure Render(const ARootScene: TP3DScene=nil);
      procedure MakeRenderList( ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList; const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );
      procedure RenderFromCamera( ACam: TP3DObject );
      procedure RenderFromCamera( ACam: TP3DObject; world: TMat4 );
      function AppendFile( AFileName: String ): TP3DLibrary;
      procedure AppendFromData(AData: TP3DLibrary);
      procedure PassToShader; virtual;
      //function Pick( X,Y: Integer; const ARootScene: TP3DScene=nil ): TP3DObject;

      property Objects: TP3DObjectPointerList read FObjects;
      property Cam: TP3DObject read GetCam write SetCam;
      property IsDesignTime: Boolean read FIsDesignTime write FIsDesignTime;
      property OnRenderCallback: TP3DSceneRenderCallback read FOnRenderCallback write FOnRenderCallback;
      property PlotList: TP3DPlotListLayer read FPlotList;
      property LightList: TP3DPlotListSetting read FLightList;

      //IP3DPlottableLayer -->
    public
      function GetLayer(ASettings: TP3DPlotListSetting): TP3DPlotCommandLayer;
      function GetLayers(ASettings: TP3DPlotListSetting): TP3DPlotListLayer;
      //<-- IP3DPlottableLayer
  end;

  {TP3DSceneContainer = specialize gP3DStreamableContainer < TP3DScene >;
  TP3DScenePointer = specialize gP3DStreamablePointer < TP3DScene >;
  TP3DScenePointerList = specialize gP3DStreamableList < TP3DScene, TP3DScenePointer >;
  }
  TP3DSceneContainerList = specialize gP3DNamedStreamableContainerList < TP3DScene >;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DScene }

procedure TP3DScene.SetCam(AValue: TP3DObject);
begin
  FCam.AsValue:= AValue;
end;

function TP3DScene.GetCam: TP3DObject;
begin
  Result:= TP3DObject( FCam.AsValue );
end;

procedure TP3DScene.RenderCallback(world: TMat4; AObject: TP3DObject);
begin
  if ( Assigned( OnRenderCallback )) then
    OnRenderCallback( world, AObject );
end;

procedure TP3DScene.OnObjChange(Sender: TObject; Item: TP3DStreamable; AAction: TP3DListOnChangeAction );
begin
  P3DData.ChangeScene( Self );
  if ( AAction = actClear ) then
    if ( Assigned( FPlotList )) then
      FPlotList.Clear
  else begin
    UpdatePlotList;
    if ( Assigned( Sender ) and ( Sender is TP3DLight )) then
      UpdateLights;
  end;
  {if ( AAction = actAdd ) then
    begin
      Item.AddUser( Self );
      Item.FScene:= Self;
      Item.FParent:= Self;
    end
  else
    begin
      Item.RemoveUser( Self );
      Item.FScene:= nil;
      Item.FParent:= nil;
    end;}
end;

procedure TP3DScene.UpdatePlotList;
var
  Obj: TP3DObject;
begin
  FPlotList.Clear;
  FLightList.Clear;
  for Obj in Objects do begin
    WriteLn( Obj.Data.ClassName, IfThen( Obj.Data is IP3DPlottableLayer, ' is ', ' is no ' ), 'IP3DPlottableLayer' );
    FPlotList.AppendList( Obj.GetLayers( nil ));
    if ( Obj.Data is TP3DLight ) then
      FLightList.Append( uniform_light( Obj ));
  end;

  WriteLn( FPlotList.Debug );
  WriteLn( FLightList.Debug );
end;

class function TP3DScene.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Scenes' );
end;

constructor TP3DScene.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FObjects:= TP3DObjectPointerList.Create( 'Objects', smText );
  FObjects.OnChange:= @OnObjChange;
  Lights:= TP3DLightPointerList.Create( 'Lights' );
  FLights.OnChange:= @OnObjChange;
  //Owner.Scenes.Add( Self );
  FCam:= TP3DStreamablePointer.Create( 'Cam', TP3DObject, smText );
  Properties.Add( FCam );
  Properties.Add( FObjects );
  Properties.Add( FLights );
  FPlotList:= TP3DPlotListLayer.Create();
  FLightList:= TP3DPlotListSetting.Create();
  FPlotList.OwnedByUser:= True;
  FLightList.OwnedByUser:= True;
end;

destructor TP3DScene.Destroy;
begin
  inherited Destroy;
  FreeAndNil( FPlotList );
  FreeAndNil( FLightList );
end;

{
constructor TP3DScene.Create(AFileName: String; const AWatchFileForChanges: Boolean; const AData: TP3DLibrary);
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  CreateBlock( AData );
  AppendFile( AFileName );
end;
}

procedure TP3DScene.UpdateLights;
var
  Obj: TP3DObject;
  light: TP3DLight;
begin
  Lights.Clear();
  FLightList.Clear;
  for Obj in Objects do
    if ( Obj.Data is TP3DLight ) then begin
      Lights.Add( TP3DLight( Obj.Data ));
      FLightList.Append( uniform_light( Obj ));
    end else if ( Obj.Data is TP3DScene ) then
      begin
        TP3DScene( Obj.Data ).UpdateLights;
        for light in TP3DScene( Obj.Data ).Lights do
          Lights.Add( light );
      end;
  {WriteLn( Name + ' light list:' );
  for light in Lights do
    WriteLn( ExtractFileName( light.Parent.FileWatch.FileName ) + '.' + light.Name );
  WriteLn( '-------------------' );}
end;


procedure TP3DScene.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DObject);
begin
  if ( Assigned( Scene )) then
    if ( Assigned( Scene.RootScene )) then
      RootScene:= Scene.RootScene
    else
      begin
        RootScene:= Scene;
        RootScene.UpdateLights;
      end;
  if ( Scene.FInheritedDesignTime or IsDesignTime ) then
    FInheritedDesignTime:= True;
  if ( Assigned( Cam )) then
    begin
      ActiveCam:= Cam;
      if ( Assigned( Cam.Data )) then
        TP3DCamera( Cam.Data ).UpdateMatrices( Cam );
      Objects.Render( world, Self );
    end
  else
    RenderFromCamera( Scene.ActiveCam, world );
end;

procedure TP3DScene.Render( const ARootScene: TP3DScene = nil );
begin
  ActiveCam:= Cam;
  RootScene:= ARootScene;
  if ( Assigned( Cam.Data )) then
    TP3DCamera( Cam.Data ).UpdateMatrices( Cam );
  UpdateLights;
  Objects.Render( Mat4Identity, Self );
end;

procedure TP3DScene.MakeRenderList(ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList; const AFlags: TP3DRenderListFlags);
begin
  if ( not Assigned( ARenderList )) then
    ARenderList:= TP3DRenderList.Create;
  if ( not Assigned( ARootScene )) then
    ARootScene:= Self;
  Objects.MakeRenderList( ARootScene, world, ARenderList, AFlags );
end;

procedure TP3DScene.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
begin
  MakeRenderList( ARootScene, AWorld, AList, AFlags );
end;

procedure TP3DScene.RenderFromCamera( ACam: TP3DObject );
begin
  RenderFromCamera( ACam, Mat4Identity );
end;

procedure TP3DScene.RenderFromCamera( ACam: TP3DObject; world: TMat4 );
begin
  ActiveCam:= ACam;
  if ( Assigned( ACam.Data )) then
    TP3DCamera( ACam.Data ).UpdateMatrices( ACam );
  Objects.Render( world, Self );
end;

function TP3DScene.AppendFile(AFileName: String): TP3DLibrary;
var
  n: Integer;
begin
  n:= P3DData.OpenLibrary( AFileName );
  Result:= P3DData.Libraries[ n ];
  AppendFromData( Result );
end;

procedure TP3DScene.AppendFromData( AData: TP3DLibrary );
var
  Scene: TP3DScene;
  Obj: TP3DObject;
begin
  for Scene in AData.Scenes do
    begin
      Obj:= TP3DObject.CreateBlock( OwnerLib );
      Obj.Data:= Scene;
      Objects.Add( Obj );
    end;
end;

procedure TP3DScene.PassToShader;
var
  ALightList: TP3DLightPointerList;
begin
  if ( Assigned( ActiveCam )) then
    begin
      if ( Assigned( RootScene )) then
        ALightList:= RootScene.Lights
      else
        ALightList:= Lights;

      if ( ActiveCam.Data is TP3DCamera ) then
        begin
          //TP3DCamera( ActiveCam.Data ).UpdateMatrices( ActiveCam );
          TP3DCamera( ActiveCam.Data ).PassToShader;
          //LightList.PassToActiveShader( TP3DCamera( ActiveCam.Data ).View, [ liPosition, liLightParams ]);
        end;
    end;
end;

function TP3DScene.GetLayer(ASettings: TP3DPlotListSetting): TP3DPlotCommandLayer;
begin
  Result:= TP3DPlotCommandScene.Create( Self );
end;

function TP3DScene.GetLayers(ASettings: TP3DPlotListSetting): TP3DPlotListLayer;
var
  l: TP3DPlotCommandScene;
begin
  l:= TP3DPlotCommandScene.Create( Self );
  if ( Assigned( ASettings )) then
    Result:= settings_default( layer( l ), settings_join([ FLightList, ASettings ]))
  else
    Result:= settings_default( layer( l ), FLightList );
end;

{
function TP3DScene.Pick(X, Y: Integer; const ARootScene: TP3DScene): TP3DObject;
begin
  if ( not Assigned( ARootScene )) then
    begin
      glScissor( X, Y, 1, 1 );
      glEnable( GL_SCISSOR_TEST );
      if ( not Assigned( PickMaterial )) then
        PickMaterial.Create();
      PickMaterial.Diff:= vec3( 0 );
      PickMaterial.PassToShader();
    end;

  RenderCallbackOnly();

  if ( not Assigned( ARootScene )) then
    begin
      glDisable( GL_SCISSOR_TEST );
    end;
end;
}





{$ENDIF}
