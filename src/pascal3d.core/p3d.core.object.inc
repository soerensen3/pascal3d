//p3dmodel_resource.inc

{$IFDEF INTERFACE}
type
 { TP3DObject }

  TP3DObjectPointerList = class;

  {{ IP3DTransform }

  IP3DTransform = interface ( IP3DBaseInterface )
    ['{31BC6803-4A40-42D7-AB13-E46CB7E6C8B4}']
    function GetDirection: TVec3;
    function GetMatrix: TMat4;
    function GetPosition: TVec3;
    function GetQuaternion: TQuat;
    function GetRotation: TVec3;
    function GetRotationOrder: TRotationOrder;
    function GetScale: TVec3;
    function GetProperties: TP3DPropertyList;
    procedure SetMatrix(AValue: TMat4);
    procedure SetPosition(AValue: TVec3);
    procedure SetQuaternion(AValue: TQuat);
    procedure SetRotation(AValue: TVec3);
    procedure SetRotationOrder(AValue: TRotationOrder);
    procedure SetScale(AValue: TVec3);

    procedure MoveAlongGlobalAxis( AAxis: TVec3; AValue: Float );
    procedure MoveAlongLocalAxis( AAxis: TVec3; AValue: Float );
    procedure RotateAroundGlobalAxis( AAxis: TVec3; AValue: Float );
    procedure RotateAroundLocalAxis( AAxis: TVec3; AValue: Float );

    property Position: TVec3 read GetPosition write SetPosition;
    property Direction: TVec3 read GetDirection;
    property Rotation: TVec3 read GetRotation write SetRotation;
    property RotationOrder: TRotationOrder read GetRotationOrder write SetRotationOrder;
    property Scale: TVec3 read GetScale write SetScale;
    property Matrix: TMat4 read GetMatrix write SetMatrix;
    property Quaternion: TQuat read GetQuaternion write SetQuaternion;
    property Properties: TP3DPropertyList read GetProperties;
  end;}

  TP3DObjectCollision = ( acNone, acBox, acSphere, acDataBlock );

  { TP3DObjectModifier }

  TP3DObjectModifier = class( TP3DStreamable )
    private
      FObject: TP3DObject;

      procedure SetObj(AValue: TP3DObject); virtual;

    public
      procedure PassToShader( Material: TP3DMaterial; var world: TMat4 ); virtual; abstract;
      procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); virtual; abstract;

    published
      property Obj: TP3DObject read FObject write SetObj;
  end;

  {TP3DObjectModifierContainer = specialize gP3DStreamableContainer < TP3DObjectModifier >;
  TP3DPropAccessObjectModifierPointer = specialize gP3DStreamablePointer < TP3DObjectModifier >;
  //TP3DObjectModifierContainerList = specialize gP3DStreamableContainerList < TP3DObjectModifier, TP3DObjectModifierContainer >;
  TP3DObjectModifierPointerList = specialize gP3DStreamableList < TP3DObjectModifier, TP3DPropAccessObjectModifierPointer >;
  }
  { TP3DObjectModifierContainerList }

  TP3DObjectModifierContainerList = class( specialize gP3DStreamableContainerList < TP3DObjectModifier >)
    private
      FParentObject: TP3DObject;

    public
      procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;

      property ParentObject: TP3DObject read FParentObject;
  end;

  TP3DObjectShadowModes = ( osShadowCaster, osShadowReceiver );
  TP3DObjectShadow = set of TP3DObjectShadowModes;

  TP3DObject = class ( TP3DDataBlock, IP3DTransform, IP3DRenderable, IP3DCollidable, IP3DAnimatable, IP3DPlottableLayer )
    protected
      FChildren: TP3DObjectPointerList;
      FCollision: TP3DObjectCollision;
      FData: TP3DStreamablePointer;
      FMatrixFinal: TMat4;
      FModifiers: TP3DObjectModifierContainerList;
      FParent: TP3DDataBlock;
      FScene: TP3DScene;
      FShadow: TP3DObjectShadow;
      FVisible: Boolean;

      function GetData: TP3DDataBlock;
      procedure SetData( AValue: TP3DDataBlock );
      procedure SetScene( AValue: TP3DScene );
      procedure OnObjChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );
      procedure OnTransformChange( ASender: TP3DStreamable );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      class function GetContainer( ALib: TP3DLibrary): TP3DPropertyAccess; override;

      procedure Render( world: TMat4; Scene: TP3DScene );
      procedure RenderCallbackOnly( world: TMat4; Scene: TP3DScene ); reintroduce;

      function CalculateParentMatrix: TMat4;

    published
      property Children: TP3DObjectPointerList read FChildren;
      property Visible: Boolean read FVisible write FVisible;
      property Data: TP3DDataBlock read GetData write SetData;
      property Scene: TP3DScene read FScene write SetScene;
      property Parent: TP3DDataBlock read FParent;
      property Collision: TP3DObjectCollision read FCollision write FCollision;
      property Modifiers: TP3DObjectModifierContainerList read FModifiers write FModifiers;
      property Shadow: TP3DObjectShadow read FShadow write FShadow;

      // IP3DRenderable -->
    public
      procedure Render( AWorld: TMat4; AScene: TP3DScene; ARenderObject: TP3DObject);
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );

      // <-- IP3DRenderable

      // IP3DAnimatable -->
    private
      FAction: TP3DStreamablePointer;

      function GetAction: TP3DAction;
      procedure SetAction( AValue: TP3DAction );

    public
      property Action: TP3DAction read GetAction write SetAction;
      // <-- IP3DAnimatable

      //IP3DCollidable -->
    private
      FAABB: TP3DAABB;
      FSphere: TP3DSphere;

      function GetAABB: TP3DAABB;
      procedure SetAABB( AValue: TP3DAABB );
      function GetSphere: TP3DSphere;
      procedure SetSphere( AValue: TP3DSphere );

    public
      function CalcAABB(): TP3DAABB;
      function CalcSphere: TP3DSphere;
      function CastRay( Orig, Dir: TVec3; out HitPos: TVec3 ): Boolean;
      function CastRay( Orig, Dir: TVec3; AObject: TP3DObject; out HitPos: TVec3 ): Boolean;

      function CollideObject( SelfMat: TMat4; ActMat: TMat4; AObject: TP3DObject; out Intersect: TVec3; out N: TVec3 ): Boolean;
      function CollideSphere( Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3 ): Boolean;
      function CollideAABB( Mat: TMat4; AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3 ): Boolean;
      function CollidePoint( Mat: TMat4; APoint: TVec3 ): Boolean;

      property AABB: TP3DAABB read GetAABB write SetAABB;
      property Sphere: TP3DSphere read GetSphere write SetSphere;
      //<-- IP3DCollidable

      //IP3DTransform -->
    private
      FTransform: TP3DStreamableContainer;

      function GetTransform: TP3DTransform;
      procedure SetTransform(AValue: TP3DTransform);

    public
      property Transform: TP3DTransform read GetTransform write SetTransform;

      //<-- IP3DTransform

      //IP3DPlottableLayer -->
    public
      function GetLayer(ASettings: TP3DPlotListSetting): TP3DPlotCommandLayer;
      //<-- IP3DPlottableLayer
  end;


  TP3DRenderListModifier = class;

  {TP3DObjectContainer = specialize gP3DStreamableContainer < TP3DObject >;
  TP3DObjectPointer = specialize gP3DStreamablePointer < TP3DObject >;
  //TP3DObjectPointerList = class( specialize gP3DNamedStreamableList < TP3DObject, TP3DObjectPointer >);
  }
  TP3DObjectContainerList = specialize gP3DNamedStreamableContainerList < TP3DObject >;

  TP3DObjectPointerList = class( specialize gP3DStreamablePointerList < TP3DObject > )
    private
      //FOwner: TPersistent;

    public
      procedure Render( world: TMat4; Scene: TP3DScene );
      procedure RenderCallBackOnly( world: TMat4; Scene: TP3DScene );
      procedure MakeRenderList( ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList;
                                const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
      function OutputDebugInfo: String;

      function CalcAABB(): TP3DAABB;
      function CastRay( Orig, Dir: TVec3; out HitPos: TVec3 ): TP3DObject;

      //property Owner: TPersistent read FOwner write FOwner;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DObject }

procedure TP3DObject.OnObjChange(ASender: TObject; AItem: TP3DStreamable;
  AAction: TP3DListOnChangeAction);
begin
  if ( not Assigned( AItem )) then
    exit;
  if ( AAction = actAdd ) then
    TP3DObject( AItem ).FParent:= Self
  else
    TP3DObject( AItem ).FParent:= nil;
  P3DData.ChangeObject( Self );
end;

procedure TP3DObject.OnTransformChange(ASender: TP3DStreamable);
begin
  P3DData.ChangeObject( Self );
end;

function TP3DObject.GetData: TP3DDataBlock;
begin
  Result:= TP3DDataBlock( FData.AsValue );
end;

procedure TP3DObject.SetData( AValue: TP3DDataBlock );
begin
  if ( AValue is TP3DObject ) then
    raise Exception.Create( 'TP3DObject.SetData: Error: Data can''t be an actor.' );
  FData.AsValue:= AValue;
end;

procedure TP3DObject.SetScene( AValue: TP3DScene );
begin
  if ( FScene = AValue ) then
    Exit;

  if ( Assigned( Scene )) then
    Scene.Objects.Remove( Self );
  FScene:= AValue;
  if ( Assigned( Scene )) then
    Scene.Objects.Remove( Self );
end;

class function TP3DObject.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Objects' );
end;

constructor TP3DObject.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  //Owner.Objects.Add( Self );
  FChildren:= TP3DObjectPointerList.Create( 'Children', smText );
  Children.OnChange:= @OnObjChange;
  FVisible:= True;

  FTransform:= TP3DStreamableContainer.Create( 'Transform', TP3DTransform, smText );
  FTransform.OnChange:= @OnTransformChange;
  Properties.Add( FTransform, 'Transform' );
  //WriteLn( 'FTransform: ', Assigned( FTransform ));
  TP3DTransform.Create( FTransform );

  FModifiers:= TP3DObjectModifierContainerList.Create( 'Modifiers', smText );
  FModifiers.FParentObject:= Self;

  FData:= TP3DStreamablePointer.Create( 'Data', TP3DDataBlock, smText );
  FAction:= TP3DStreamablePointer.Create( 'Action', TP3DAction, smText );

  Properties.Add([ TP3DPropAccessRTTI.Create( Self, 'Visible', smText ),
                   TP3DPropAccessRTTI.Create( Self, 'Collision', smText ),
                   TP3DPropAccessRTTI.Create( Self, 'Shadow', smText ),
                   FData,
                   FChildren,
                   FModifiers,
                   FAction ],
                   'Object' );
end;

procedure TP3DObject.Render( world: TMat4; Scene: TP3DScene );
var
  _world: TMat4;
begin
  _world:= world * Transform.Matrix;

  if ( Assigned( Scene.RootScene )) then
    Scene.RootScene.RenderCallback( _world, Self )
  else
    Scene.RenderCallback( _world, Self );

  if ( Data is IP3DRenderable ) then
    ( Data as IP3DRenderable ).Render( _world, Scene, Self );
  Children.Render( _world, Scene );
end;

procedure TP3DObject.RenderCallbackOnly(world: TMat4; Scene: TP3DScene);
var
  _world: TMat4;
begin
  _world:= world * Transform.Matrix;

  Children.Render( _world, Scene );

  if ( Assigned( Scene.RootScene )) then
    Scene.RootScene.RenderCallback( world, Self )
  else
    Scene.RenderCallback( world, Self );
end;

function TP3DObject.CalcAABB(): TP3DAABB;
var
  BB: TP3DAABB;
begin
  if ( Data is IP3DCollidable ) then
    with ( Data as IP3DCollidable ) do
      begin
        if ( AABB = P3DInvalidAABB ) then
          BB:= CalcAABB()
        else
          BB:= AABB;
        Result:= P3DTransformAABBObjSpace( Transform.Matrix, BB );
      end
  else
    Result:= P3DAABB( Transform.Position, Transform.Position, Transform.Position );
end;

function TP3DObject.CalcSphere: TP3DSphere;
begin
  Result:= Sphere;
end;

function TP3DObject.CastRay( Orig, Dir: TVec3; out HitPos: TVec3 ): Boolean;
begin
  if ( Data is IP3DCollidable ) then
    Result:= ( Data as IP3DCollidable ).CastRay( Orig, Dir, Self, HitPos );
end;

function TP3DObject.CastRay(Orig, Dir: TVec3; AObject: TP3DObject; out HitPos: TVec3): Boolean;
begin
  Result:= CastRay( Orig, Dir, HitPos );
end;

function TP3DObject.CalculateParentMatrix: TMat4;
begin
  if ( Parent is TP3DObject ) then
    Result:= TP3DObject( Parent ).CalculateParentMatrix * TP3DObject( Parent ).Transform.Matrix
  else
    Result:= Mat4Identity;
end;

function TP3DObject.CollideObject(SelfMat: TMat4; ActMat: TMat4; AObject: TP3DObject; out Intersect: TVec3; out N: TVec3): Boolean;
var
  APlane: TP3DPlane;
begin
  if ( AObject.Data is IP3DCollidable ) then
    case AObject.Collision of
      acNone: Result:= False;
      acBox: Result:= CollideAABB( SelfMat, P3DTransformAABBObjSpace( ActMat, ( AObject.Data as IP3DCollidable ).AABB ), Intersect, N );
      acSphere: Result:= CollideSphere( SelfMat, P3DTransformSphereObjSpace( ActMat, ( AObject.Data as IP3DCollidable ).Sphere ), Intersect, N );
      acDataBlock:
        WriteLn( 'Not implemented yet' );
    end
  else
    Result:= False;
end;

{
function TP3DObject.CollideObject( AObject: TP3DObject ): Boolean;
begin
  case Collision of
    acNone: Result:= False;
    acBox: AObject.CollideAABB( AABB );
  end;
end;
}
function TP3DObject.CollideSphere(Mat: TMat4; ASphere: TP3DSphere; out Intersect: TVec3; out N: TVec3): Boolean;
var
  APlane: TP3DPlane;
  box: TP3DAABB;
  sph: TP3DSphere;
begin
  if ( Data is IP3DCollidable ) then
    case Collision of
      acNone: Result:= False;
      acBox:
        begin
          box:= P3DTransformAABBObjSpace( Mat, ( Data as IP3DCollidable ).AABB );
          Result:= P3DCollideSPHEREwithAABB( ASphere, box );
          if ( Result ) then
            begin
              Intersect:= P3DFindClosestPointAABB( box, ASphere.Position );
              N:= P3DFindNormalToPointAABB( box, ASphere.Position );
            end;
        end;
      acSphere:
        begin
          sph:= P3DTransformSphereObjSpace( Mat, ( Data as IP3DCollidable ).Sphere );
          Result:= P3DCollideSPHEREwithSPHERE( sph, ASphere );
          if ( Result ) then
            begin
              Intersect:= P3DFindClosestPointSPHERE( sph, ASphere.Position );
              N:= P3DFindNormalToPointSPHERE( sph, ASphere.Position );
            end;
        end;
      acDataBlock:
        Result:= ( Data as IP3DCollidable ).CollideSphere( Mat, ASphere, Intersect, N )
    end
  else
    Result:= False;
end;

function TP3DObject.CollideAABB(Mat: TMat4; AAABB: TP3DAABB; out Intersect: TVec3; out N: TVec3): Boolean;
var
  APlane: TP3DPlane;
  box: TP3DAABB;
  sph: TP3DSphere;
begin
  if ( Data is IP3DCollidable ) then
    case Collision of
      acNone: Result:= False;
      acBox:
        begin
          box:= P3DTransformAABBObjSpace( Mat, ( Data as IP3DCollidable ).AABB );
          Result:= P3DCollideAABBwithAABB( AAABB, box );
          if ( Result ) then
            begin
              Intersect:= P3DFindClosestPointAABB( box, AAABB.Position );
              N:= P3DFindNormalToPointAABB( box, AAABB.Position );
            end;
        end;
      acSphere:
        begin
          sph:= P3DTransformSphereObjSpace( Mat, ( Data as IP3DCollidable ).Sphere );
          Result:= P3DCollideSPHEREwithAABB( sph, AAABB );
          if ( Result ) then
            begin
              Intersect:= P3DFindClosestPointSPHERE( sph, AAABB.Position );
              N:= P3DFindNormalToPointSPHERE( sph, AAABB.Position );
            end;
        end;
      acDataBlock:
        Result:= ( Data as IP3DCollidable ).CollideAABB( Mat, AAABB, Intersect, N )
    end
  else
    Result:= False;
end;

function TP3DObject.CollidePoint(Mat: TMat4; APoint: TVec3): Boolean;
begin

end;

function TP3DObject.GetTransform: TP3DTransform;
begin
  Result:= TP3DTransform( FTransform.AsValue );
end;

procedure TP3DObject.SetTransform(AValue: TP3DTransform);
begin
  FTransform.AsValue:= AValue;
end;

function TP3DObject.GetLayer(ASettings: TP3DPlotListSetting): TP3DPlotCommandLayer;
begin
  if ( Data is IP3DPlottableLayer ) then begin
    Result:= ( Data as IP3DPlottableLayer ).GetLayer( settings_join([ settings( uniform( 'world', Transform )), ASettings ]));
  end;
end;

procedure TP3DObject.Render( AWorld: TMat4; AScene: TP3DScene; ARenderObject: TP3DObject);
begin
  Render( AWorld, AScene );
end;

procedure TP3DObject.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);

  function CheckShadowAgainstFlags: Boolean;
  begin
    Result:= (( Shadow = []) and ( rlfNonShadowCasters in AFlags )) or
             (( osShadowReceiver in Shadow ) and ( rlfShadowReceivers in AFlags )) or
             (( osShadowCaster in Shadow ) and ( rlfShadowCasters in AFlags ));
  end;

var
  mat: TMat4;
begin
  if ( not Visible ) then
    exit;
  mat:= AWorld * Transform.Matrix;
  FMatrixFinal:= mat;
  AList.ActiveSubset:= Owner.GetFullPath;
  if (( CheckShadowAgainstFlags ) or ( Data is TP3DScene )) then begin
    if ( rlfObjects in AFlags ) then
      AList.PushRenderObject( Self, mat, P3DMaterialActive );
    if ( Assigned( Data )) then
      AList.Add( Data, mat, ARootScene, AFlags );
  end;
  Children.MakeRenderList( ARootScene, mat, AList, AFlags );
end;

function TP3DObject.GetAABB: TP3DAABB;
begin
  Result:= FAABB;
end;

procedure TP3DObject.SetAABB(AValue: TP3DAABB);
begin
  FAABB:= AValue;
end;

function TP3DObject.GetSphere: TP3DSphere;
begin
  Result:= FSphere;
end;

procedure TP3DObject.SetSphere(AValue: TP3DSphere);
begin
  FSphere:= AValue;
end;

function TP3DObject.GetAction: TP3DAction;
begin
  Result:= TP3DAction( FAction.AsValue );
end;

procedure TP3DObject.SetAction(AValue: TP3DAction);
begin
  FAction.AsValue:= AValue;
end;

{ TP3DObjectPointerList }

procedure TP3DObjectPointerList.Render( world: TMat4; Scene: TP3DScene );
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( AsValues[ i ] is TP3DObject ) then
      if ( TP3DObject( AsValues[ i ]).Visible ) then
        TP3DObject( AsValues[ i ]).Render( world, Scene );
end;

procedure TP3DObjectPointerList.RenderCallBackOnly(world: TMat4; Scene: TP3DScene);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( AsValues[ i ] is TP3DObject ) then
      if ( TP3DObject( AsValues[ i ]).Visible ) then
        TP3DObject( AsValues[ i ]).RenderCallbackOnly( world, Scene );
end;

procedure TP3DObjectPointerList.MakeRenderList(ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList; const AFlags: TP3DRenderListFlags);
var
  Obj: TP3DObject;
begin
  if ( not Assigned( ARenderList )) then
    ARenderList:= TP3DRenderList.Create;
  for Obj in Self do
    if ( Assigned( Obj )) then
      Obj.AddToRenderList( ARootScene, world, ARenderList, AFlags )
    else
      WriteLn( 'Empty object in list ', GetFullPath );
end;

function TP3DObjectPointerList.OutputDebugInfo: String;
  function DebugData( Data: TP3DDataBlock ): String;
  begin
    if ( Assigned( Data )) then
      Result:= Data.Name + ': ' + Data.ClassName
    else
      Result:= 'nil';
  end;

var
  Item: TP3DObject;
begin
  Result:= 'Debug information for Object List';
  for Item in Self do
    Result+= Format( 'Name: "%s" Class "%s" Visible %s Data: "%s"'+LineEnding, [ Item.Name, Item.ClassName, BoolToStr( TP3DObject( Item ).Visible, 'Yes', 'No' ), DebugData( TP3DObject( Item ).Data )]);
end;

function TP3DObjectPointerList.CalcAABB: TP3DAABB;
var
  Obj: TP3DObject;
  BB: TP3DAABB;
begin
  Result:= P3DAABB( vec3( 0 ), vec3( 0 ), vec3( 0 ));
  for Obj in Self do
    begin
      BB:= TP3DObject( Obj ).CalcAABB();
      Result.Min:= Min( Result.Min, BB.Min );
      Result.Max:= Max( Result.Max, BB.Max );
    end;
  Result.Position:= ( Result.Min + Result.Max ) / 2;
end;

function TP3DObjectPointerList.CastRay(Orig, Dir: TVec3; out HitPos: TVec3): TP3DObject;
var
  Obj, CastObj: TP3DObject;
  ClosestHitpos: TVec3;
begin
  Result:= nil;
  ClosestHitpos:= vec3( MaxFloat );
  for Obj in Self do
    if ( Obj.CastRay( Orig, Dir, HitPos )) then
      begin
        if ( distance( Orig, HitPos ) < distance( Orig, ClosestHitpos )) then
          begin
            Result:= Obj;
            ClosestHitpos:= HitPos;
          end;
      end
    else
      begin
        if ( Obj.Data is TP3DScene ) then
          CastObj:= TP3DScene( Obj.Data ).Objects.CastRay( Orig, Dir, HitPos )
        else
          CastObj:= nil;
        if ( Assigned( CastObj )) then
          if ( distance( Orig, HitPos ) < distance( Orig, ClosestHitpos )) then
            begin
              Result:= CastObj;
              ClosestHitpos:= HitPos;
            end;
        CastObj:= Obj.Children.CastRay( Orig, Dir, HitPos );
        if ( Assigned( Result )) then
          if ( distance( Orig, HitPos ) < distance( Orig, ClosestHitpos )) then
            begin
              Result:= Obj;
              ClosestHitpos:= HitPos;
            end;
      end;
end;

{ TP3DObjectModifierContainerList }

procedure TP3DObjectModifierContainerList.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  inherited Notify(ASender, AItem, AAction);
  TP3DObjectModifier( AItem ).Obj:= ParentObject;
end;

{ TP3DObjectModifier }

procedure TP3DObjectModifier.SetObj(AValue: TP3DObject);
begin
  if FObject=AValue then Exit;
  FObject:=AValue;
end;

{$ENDIF}

