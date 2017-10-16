//p3dmodel_resource.inc

{$IFDEF INTERFACE}
type
 { TP3DObject }

  TP3DObjectList = class;

  { IP3DTransform }

  IP3DTransform = interface
    ['{31BC6803-4A40-42D7-AB13-E46CB7E6C8B4}']
    function GetDirection: TVec3;
    function GetMatrix: TMat4;
    function GetPosition: TVec3;
    function GetQuaternion: TQuaternion;
    function GetRotation: TVec3;
    function GetRotationOrder: TRotationOrder;
    function GetScale: TVec3;
    function GetProperties: TP3DPropertyList;
    procedure SetMatrix(AValue: TMat4);
    procedure SetPosition(AValue: TVec3);
    procedure SetQuaternion(AValue: TQuaternion);
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
    property Quaternion: TQuaternion read GetQuaternion write SetQuaternion;
    property Properties: TP3DPropertyList read GetProperties;
  end;

  TP3DObjectCollision = ( acNone, acBox, acSphere, acDataBlock );

  TP3DObject = class ( specialize gP3DTransform < TP3DDataBlock >, IP3DTransform, IP3DRenderable, IP3DCollidable )
    private
      FChildren: TP3DObjectList;
      FCollision: TP3DObjectCollision;
      FData: TP3DDataBlock;
      FMatrixFinal: TMat4;
      FParent: TP3DDataBlock;
      FScene: TP3DScene;
      FVisible: Boolean;

      procedure SetData( AValue: TP3DDataBlock );
      procedure SetScene( AValue: TP3DScene );
      procedure OnObjChange( Sender: TP3DNamedInterfacedPersistentList; Item: TP3DObject; AAction: TP3DCustomListOnChangeAction );

    protected
      class function GetContainer( ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList; override;
      function GetDataPath: String; override;

    public
      constructor Create( const AData: TP3DLibrary = nil ); override;

      procedure LoadFromDOM( ADOMNode: TDOMElement ); override;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      function SaveToDOM( AParent: TDOMElement ): TDOMElement; override;

      procedure FastDestroy(); override;

      procedure Render( world: TMat4; Scene: TP3DScene );
      procedure RenderCallbackOnly( world: TMat4; Scene: TP3DScene ); reintroduce;

      procedure RemoveFromFields( Block: TP3DDataBlock ); override;

      // IP3DRenderable -->
      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );
      // <-- IP3DRenderable

      //IP3DCollidable -->
    private
      FAABB: TP3DAABB;
      FSphere: TP3DSphere;

      function GetAABB(): TP3DAABB;
      procedure SetAABB( AValue: TP3DAABB );
      function GetSphere(): TP3DSphere;
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

    public
      procedure ClearFields(); override;
      function CalculateParentMatrix: TMat4;

      class function DOMNodeName: String; override;

    published
      property Children: TP3DObjectList read FChildren;
      property Visible: Boolean read FVisible write FVisible;
      property Data: TP3DDataBlock read FData write SetData;
      property Scene: TP3DScene read FScene write SetScene;
      property Parent: TP3DDataBlock read FParent;
      property RotationOrder: TRotationOrder read GetRotationOrder write SetRotationOrder;
      property Collision: TP3DObjectCollision read FCollision write FCollision;
  end;

  TP3DPropertyAccessObject = specialize gP3DPropertyAccessDataBlock < TP3DObject >;
  TP3DRenderListModifier = class;

  { TP3DObjectList }
  TP3DObjectList = class( specialize gP3DNamedInterfacedPersistentList < TP3DObject > )
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

  TP3DPropertyAccessObjectList = specialize gP3DPropertyAccessDataBlockList < TP3DObjectList, TP3DObject, TP3DPropertyAccessObject >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DObject }

procedure TP3DObject.OnObjChange(Sender: TP3DNamedInterfacedPersistentList; Item: TP3DObject; AAction: TP3DCustomListOnChangeAction);
begin
  if ( AAction = actAdd ) then
    Item.FParent:= Self
  else
    Item.FParent:= nil;
end;

procedure TP3DObject.SetData( AValue: TP3DDataBlock );
begin
  if FData=AValue then Exit;

  if ( AValue is TP3DObject ) then
    raise Exception.Create( 'TP3DObject.SetData: Error: Data can''t be an actor.' );
  if ( Assigned( FData )) then
    FData.RemoveUser( Self );

  FData:= AValue;

  if ( Assigned( FData )) then
    FData.AddUser( Self );
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

class function TP3DObject.GetContainer( ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList;
begin
  Result:= ALib.Objects;
end;

function TP3DObject.GetDataPath: String;
begin
  Result:= 'Objects[ "' + Name + '" ]';
end;

constructor TP3DObject.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  //Owner.Objects.Add( Self );
  FChildren:= TP3DObjectList.Create( Self );
  Children.OnChange:= @OnObjChange;
  FVisible:= True;
  FScale:= vec3( 1 );
  FQuaternion:= quat( 0, 0, 0, 1 );
  FRotationOrder:= roXYZ;
  UpdateMatrix;

  Properties.Add([ TP3DPropertyAccessVec3.CreateGetSet( 'Position', @GetPosition, @SetPosition, smAttribute ),
                   TP3DPropertyAccessVec3.CreateGetSet( 'Rotation', @GetRotation, @SetRotation ),
                   TP3DPropertyAccessRTTI.Create( Self, 'RotationOrder', smAttribute ),
                   TP3DPropertyAccessQuat.CreateGetSet( 'Quaternion', @GetQuaternion, @SetQuaternion, smAttribute ),
                   TP3DPropertyAccessVec3.CreateGetSet( 'Direction', @GetDirection, nil ),
                   TP3DPropertyAccessVec3.CreateGetSet( 'Scale', @GetScale, @SetScale, smAttribute )],
                   'Transform' );

  Properties.Add([ TP3DPropertyAccessRTTI.Create( Self, 'Visible', smAttribute ),
                   TP3DPropertyAccessRTTI.Create( Self, 'Collision', smAttribute ),
                   TP3DPropertyAccessDataBlock.CreateFieldSet( 'Data', @Data, @SetData, smAttribute ),
                   TP3DPropertyAccessObjectList.CreateField( 'Children', @FChildren, smAttribute )],
                   'Object' );
end;

procedure TP3DObject.LoadFromDOM( ADOMNode: TDOMElement );
var
  Element: TDOMElement;
  DataName: DOMString;
  Child: TP3DObject;
  n: Integer;

  procedure LoadTransformFromDOM( DOM: TDOMElement );
  begin
    FPosition.FromString( DOM.GetAttribute( 'position' ));
    FQuaternion.FromString( DOM.GetAttribute( 'quaternion' ));
    FScale.FromString( DOM.GetAttribute( 'scale' ));
    UpdateEuler;
    UpdateMatrix;
  end;

begin
  inherited LoadFromDOM( ADOMNode );
{  DataName:= ADOMNode.GetAttribute( 'data' );
  if ( DataName > '' ) then
    begin
      n:= Owner.AppendFromPath( DataName );
      Data:= TP3DDataBlock( Owner.DataBlocks[ n ]);
    end
  else
    Data:= nil;

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'transform': LoadTransformFromDom( Element );
        'object':
          begin
            n:= Children.FindByName( Element.GetAttribute( 'name' ));
            if ( n > -1 ) then
              Children.Items[ n ].LoadFromDOM( Element )
            else
              begin
                Child:= TP3DObject.CreateFromDOM( Element, Owner );
                Children.Add( Child );
              end;
          end;
      else
        raise Exception.Create( 'Unknown tag inside Object Element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;}
end;

procedure TP3DObject.LoadFromDOMNew(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  Child: TP3DObject;
begin
  inherited LoadFromDOMNew(ADOMNode);

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'object': begin Child:= TP3DObject.CreateFromDOM( Element, Owner ); Children.Add( Child ); end;
      else
        raise Exception.Create( 'Unknown tag inside object element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
end;


function TP3DObject.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM( AParent );
  //Children.SaveToDOM( Result );
  {if ( Assigned( Data )) then
    begin
      //Result.AttribStrings[ 'data' ]:= Data.DOMPath;
      //Data.SaveToDOM( Result.OwnerDocument.DocumentElement );
    end;}
end;

procedure TP3DObject.FastDestroy;
begin
  FChildren.Clear;
  FChildren.Free;
  //Owner.Objects.Remove( Self );
  inherited FastDestroy;
end;

procedure TP3DObject.Render( world: TMat4; Scene: TP3DScene );
var
  _world: TMat4;
begin
  _world:= world * Matrix;

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
  _world:= world * Matrix;

  Children.Render( _world, Scene );

  if ( Assigned( Scene.RootScene )) then
    Scene.RootScene.RenderCallback( world, Self )
  else
    Scene.RenderCallback( world, Self );
end;

function TP3DObject.CalcAABB: TP3DAABB;
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
        Result:= P3DTransformAABBObjSpace( Matrix, BB );
      end
  else
    Result:= P3DAABB( Position, Position, Position );
end;

function TP3DObject.CalcSphere: TP3DSphere;
begin
  Result:= Sphere;
end;

procedure TP3DObject.RemoveFromFields(Block: TP3DDataBlock);
begin
  if ( Block = Data ) then
    Data:= nil;
  inherited RemoveFromFields( Block );
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

procedure TP3DObject.ClearFields;
begin
  try
    Children.Clear();
    Data:= nil;

  except
    on E: Exception do
      with P3DDataBlockCache[ P3DDataBlockCache.FindBlock( Self )] do
        WriteLn( 'Call Stack for Datablock: ' + Name + LineEnding + StackTrace + ' - ' + E.Message );
  end;
  inherited ClearFields;
end;

function TP3DObject.CalculateParentMatrix: TMat4;
begin
  if ( Parent is TP3DObject ) then
    Result:= TP3DObject( Parent ).CalculateParentMatrix * TP3DObject( Parent ).Matrix
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

procedure TP3DObject.Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
begin
  Render( world, Scene );
end;

procedure TP3DObject.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
var
  mat: TMat4;
begin
  if ( not Visible ) then
    exit;
  mat:= AWorld * Matrix;
  FMatrixFinal:= mat;
  if ( rlfObjects in AFlags ) then
    AList.PushRenderObject( Self, mat, P3DMaterialActive );
  if ( Assigned( Data )) then
    AList.Add( Data, mat, ARootScene, AFlags );
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

class function TP3DObject.DOMNodeName: String;
begin
  Result:= 'object';
end;

{ TP3DObjectList }

procedure TP3DObjectList.Render( world: TMat4; Scene: TP3DScene );
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ] is TP3DObject ) then
      if ( TP3DObject( Items[ i ]).Visible ) then
        TP3DObject( Items[ i ]).Render( world, Scene );
end;

procedure TP3DObjectList.RenderCallBackOnly(world: TMat4; Scene: TP3DScene);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ] is TP3DObject ) then
      if ( TP3DObject( Items[ i ]).Visible ) then
        TP3DObject( Items[ i ]).RenderCallbackOnly( world, Scene );
end;

procedure TP3DObjectList.MakeRenderList(ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList; const AFlags: TP3DRenderListFlags);
var
  Obj: TP3DObject;
begin
  if ( not Assigned( ARenderList )) then
    ARenderList:= TP3DRenderList.Create;
  for Obj in Self do
    Obj.AddToRenderList( ARootScene, world, ARenderList, AFlags );
end;

function TP3DObjectList.OutputDebugInfo: String;
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

function TP3DObjectList.CalcAABB: TP3DAABB;
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

function TP3DObjectList.CastRay(Orig, Dir: TVec3; out HitPos: TVec3): TP3DObject;
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

{$ENDIF}

