//p3dmodel_resource.inc

{$IFDEF INTERFACE}
type
 { TP3DActor }

  TP3DActorList = class;

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
  end;

  TP3DActor = class ( TP3DDataBlock, IP3DTransform )
    private
      FChildren: TP3DActorList;
      FData: TP3DDataBlock;
      FMatrixFinal: TMat4;
      FParent: TP3DDataBlock;
      FPosition: TVec3;
      FQuaternion: TQuaternion;
      FRotation: TVec3;
      FScale: TVec3;
      FScene: TP3DScene;
      FVisible: Boolean;
      FMatrix: TMat4;
      FRotationOrder: TRotationOrder;

      function GetDirection: TVec3;
      function GetMatrix: TMat4;
      function GetPosition: TVec3;
      function GetQuaternion: TQuaternion;
      function GetRotation: TVec3;
      function GetRotationOrder: TRotationOrder;
      function GetScale: TVec3;
      procedure SetData( AValue: TP3DDataBlock );
      procedure SetMatrix( AValue: TMat4 );
      procedure SetPosition( AValue: TVec3 );
      procedure SetQuaternion( AValue: TQuaternion );
      procedure SetRotation( AValue: TVec3 );
      procedure SetRotationOrder( AValue: TRotationOrder );
      procedure SetScale( AValue: TVec3 );
      procedure SetScene( AValue: TP3DScene );
      //property BoundingBox;
      procedure UpdateMatrix;
      procedure UpdateEuler;
      function GetDOMPath: String; override;
      procedure OnObjChange( Sender: TPersistent; Item: TP3DActor; Action: TP3DCustomListOnChangeAction );

    public
      constructor Create( const AData: TP3DLibrary = nil ); override;

      procedure LoadFromDOM( ADOMNode: TDOMElement ); override;
      function SaveToDOM( AParent: TDOMElement ): TDOMElement; override;

      procedure FastDestroy(); override;
      procedure Render( world: TMat4; Scene: TP3DScene ); reintroduce;
      procedure RenderCallbackOnly( world: TMat4; Scene: TP3DScene ); reintroduce;
      function  CalcBoundingBox(): TP3DBoundingBox; override;
      procedure RemoveFromFields( Block: TP3DDataBlock ); override;
      function  CastRay( Orig, Dir: TVec3; out HitPos: TVec3 ): Boolean; reintroduce;
      procedure ClearFields(); override;

      procedure MoveAlongGlobalAxis( AAxis: TVec3; AValue: Float );
      procedure MoveAlongLocalAxis( AAxis: TVec3; AValue: Float );
      procedure RotateAroundGlobalAxis( AAxis: TVec3; AValue: Float );
      procedure RotateAroundLocalAxis( AAxis: TVec3; AValue: Float );
      class function DOMNodeName: String; override;

      property Position: TVec3 read FPosition write SetPosition;
      property Direction: TVec3 read GetDirection;
      property Rotation: TVec3 read FRotation write SetRotation;
      property Scale: TVec3 read FScale write SetScale;
      property Matrix: TMat4 read FMatrix write SetMatrix;
      property MatrixFinal: TMat4 read FMatrixFinal;
      property Quaternion: TQuaternion read FQuaternion write SetQuaternion;

    published
      property Children: TP3DActorList read FChildren;
      property Visible: Boolean read FVisible write FVisible;
      property Data: TP3DDataBlock read FData write SetData;
      property Scene: TP3DScene read FScene write SetScene;
      property Parent: TP3DDataBlock read FParent;
      property RotationOrder: TRotationOrder read GetRotationOrder write SetRotationOrder;
  end;

  TP3DPropertyAccessActor = specialize gP3DPropertyAccessDataBlock < TP3DActor >;
  TP3DCustomActorList = specialize gP3DCustomDatablockList < TP3DActor >;

  TP3DRenderList = class;
  TP3DRenderListModifier = class;

  { TP3DActorList }
  TP3DActorList = class( TP3DCustomActorList )
    private
      FOwner: TPersistent;

    public
      constructor Create( AOwner: TPersistent );
      procedure Render( world: TMat4; Scene: TP3DScene );
      procedure RenderCallBackOnly( world: TMat4; Scene: TP3DScene );
      procedure MakeRenderList( ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList;
                                const AFlags: TP3DRenderListFlags = P3DRenderListFlagsDefault );
      function OutputDebugInfo: String;

      function CalcBoundingBox(): TP3DBoundingBox;
      function CastRay( Orig, Dir: TVec3; out HitPos: TVec3 ): TP3DActor;

      property Owner: TPersistent read FOwner write FOwner;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

function LoadMat4FromDOM( DOM: TDOMElement ): TMat4;
var
  c0, c1, c2, c3: TVec4;
begin
  c0.FromString( DOM.GetAttribute( 'c0' ));
  c1.FromString( DOM.GetAttribute( 'c1' ));
  c2.FromString( DOM.GetAttribute( 'c2' ));
  c3.FromString( DOM.GetAttribute( 'c3' ));
  Result:= mat4( c0, c1, c2, c3 );
end;

{ TP3DActor }

procedure TP3DActor.UpdateMatrix;
begin
  FMatrix:= mat4translate( vec4( Position, 1 )) * mat4( mat3( Quaternion )) * mat4scale( vec4( Scale, 1 ));
  FBoundingBox:= CalcBoundingBox();
end;

procedure TP3DActor.UpdateEuler;
begin
  FRotation:= QuatToEuler( Quaternion, deg2rad * Rotation, RotationOrder ) / deg2rad;
end;

function TP3DActor.GetDOMPath: String;
begin
  if ( Assigned( Parent )) then
    Result:= Parent.GetDOMPath + '/' + Name
  else
    Result:= '/' + Name
end;

procedure TP3DActor.OnObjChange(Sender: TPersistent; Item: TP3DActor; Action: TP3DCustomListOnChangeAction);
begin
  if ( Action = actAdd ) then
    Item.FParent:= Self
  else
    Item.FParent:= nil;
end;

procedure TP3DActor.SetData( AValue: TP3DDataBlock );
begin
  if FData=AValue then Exit;

  if ( AValue is TP3DActor ) then
    raise Exception.Create( 'TP3DActor.SetData: Error: Data can''t be an actor.' );
  if ( Assigned( FData )) then
    FData.RemoveUser( Self );

  FData:= AValue;

  if ( Assigned( FData )) then
    FData.AddUser( Self );
end;

procedure TP3DActor.SetMatrix(AValue: TMat4);
begin
  if FMatrix= AValue then Exit;
  FMatrix:= AValue;
  FPosition:= vec3( Matrix._30, Matrix._31, Matrix._32 );
  FRotation:= MatToEuler( mat3( AValue ), deg2rad * Rotation, RotationOrder ) / deg2rad;
  FQuaternion:= EulerToQuat( Rotation * deg2rad, RotationOrder );
  FScale:= mat3toScale( mat3( FMatrix ));
end;


function TP3DActor.GetDirection: TVec3;
begin
  Result:= Matrix.Row[ 2 ].xyz;//vec3( Matrix._20, Matrix._21, Matrix._22 );
end;

function TP3DActor.GetMatrix: TMat4;
begin
  Result:= FMatrix;
end;

function TP3DActor.GetPosition: TVec3;
begin
  Result:= FPosition;
end;

function TP3DActor.GetQuaternion: TQuaternion;
begin
  Result:= FQuaternion;
end;

function TP3DActor.GetRotation: TVec3;
begin
  Result:= FRotation;
end;

function TP3DActor.GetRotationOrder: TRotationOrder;
begin
  Result:= FRotationOrder;
end;

function TP3DActor.GetScale: TVec3;
begin
  Result:= FScale;
end;

procedure TP3DActor.SetPosition(AValue: TVec3);
begin
  if FPosition=AValue then Exit;
  FPosition:= AValue;
  UpdateMatrix;
end;

procedure TP3DActor.SetQuaternion(AValue: TQuaternion);
begin
  if ( FQuaternion = AValue ) then Exit;
  FQuaternion:= AValue;
  UpdateMatrix;
  UpdateEuler;
end;

procedure TP3DActor.SetRotation(AValue: TVec3);
begin
  if FRotation=AValue then Exit;
  FRotation:=AValue;
  FQuaternion:= EulerToQuat( AValue * deg2rad, RotationOrder );
  UpdateMatrix;
end;

procedure TP3DActor.SetRotationOrder(AValue: TRotationOrder);
begin
  FRotationOrder:= AValue;
  FQuaternion:= EulerToQuat( Rotation * deg2rad, RotationOrder );
  UpdateMatrix;
end;

procedure TP3DActor.SetScale(AValue: TVec3);
begin
  if ( FScale = AValue ) then Exit;
  FScale:= AValue;
  UpdateMatrix;
end;

procedure TP3DActor.SetScene( AValue: TP3DScene );
begin
  if ( FScene = AValue ) then
    Exit;

  if ( Assigned( Scene )) then
    Scene.Objects.Remove( Self );
  FScene:= AValue;
  if ( Assigned( Scene )) then
    Scene.Objects.Remove( Self );
end;

constructor TP3DActor.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  Owner.Objects.Add( Self );
  FChildren:= TP3DActorList.Create( Self );
  Children.OnChange:= @OnObjChange;
  FVisible:= True;
  FScale:= vec3( 1 );
  FQuaternion:= quat( 0, 0, 0, 1 );
  FRotationOrder:= roXYZ;
  UpdateMatrix;
  Properties.Add( TP3DPropertyAccessVec3.CreateGetSet( 'Position', @GetPosition, @SetPosition, smAttribute ));
  Properties.Add( TP3DPropertyAccessVec3.CreateGetSet( 'Rotation', @GetRotation, @SetRotation ));
  Properties.Add( TP3DPropertyAccessQuat.CreateGetSet( 'Quaternion', @GetQuaternion, @SetQuaternion, smAttribute ));
  Properties.Add( TP3DPropertyAccessVec3.CreateGetSet( 'Scale', @GetScale, @SetScale, smAttribute ));
  Properties.Add( TP3DPropertyAccessVec3.CreateGetSet( 'Direction', @GetDirection, nil ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'RotationOrder', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Visible', smAttribute ));
  Properties.Add( TP3DPropertyAccessDataBlock.CreateFieldSet( 'Data', @Data, @SetData, smAttribute ));
end;

procedure TP3DActor.LoadFromDOM( ADOMNode: TDOMElement );
var
  Element: TDOMElement;
  DataName: DOMString;
  Child: TP3DActor;
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
  DataName:= ADOMNode.GetAttribute( 'data' );
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
        'matrix': Matrix:= LoadMat4FromDOM( Element );
        'transform': LoadTransformFromDom( Element );
        'object':
          begin
            n:= Children.FindByName( Element.GetAttribute( 'name' ));
            if ( n > -1 ) then
              Children[ n ].LoadFromDOM( Element )
            else
              begin
                Child:= TP3DActor.CreateFromDOM( Element, Owner );
                Children.Add( Child );
              end;
          end;
      else
        raise Exception.Create( 'Unknown tag inside Object Element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
end;


function TP3DActor.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM( AParent );
  Children.SaveToDOM( Result );
  {if ( Assigned( Data )) then
    begin
      //Result.AttribStrings[ 'data' ]:= Data.DOMPath;
      //Data.SaveToDOM( Result.OwnerDocument.DocumentElement );
    end;}
end;

procedure TP3DActor.FastDestroy;
begin
  FChildren.Clear;
  FChildren.Free;
  Owner.Objects.Remove( Self );
  inherited FastDestroy;
end;

procedure TP3DActor.Render( world: TMat4; Scene: TP3DScene );
var
  _world: TMat4;
begin
  _world:= world * Matrix;

  if ( Assigned( Scene.RootScene )) then
    Scene.RootScene.RenderCallback( _world, Self )
  else
    Scene.RenderCallback( _world, Self );

  if ( Assigned( Data )) then
    Data.Render( _world, Scene, Self );
  Children.Render( _world, Scene );
end;

procedure TP3DActor.RenderCallbackOnly(world: TMat4; Scene: TP3DScene);
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

function TP3DActor.CalcBoundingBox: TP3DBoundingBox;
var
  BB: TP3DBoundingBox;
begin
  if ( Assigned( Data )) then
    begin
      if ( Data.BoundingBox = P3DInvalidBoundingBox ) then
        BB:= Data.CalcBoundingBox()
      else
        BB:= Data.BoundingBox;
      Result:= P3DBoundingBoxTransform( BB, Matrix );
    end
  else
    Result:= P3DBoundingBox( Position, Position, Position );
end;

procedure TP3DActor.RemoveFromFields(Block: TP3DDataBlock);
begin
  if ( Block = Data ) then
    Data:= nil;
  inherited RemoveFromFields( Block );
end;

function TP3DActor.CastRay( Orig, Dir: TVec3; out HitPos: TVec3 ): Boolean;
begin
  if ( Assigned( Data )) then
    Result:= Data.CastRay( Orig, Dir, Self, HitPos );
end;

procedure TP3DActor.ClearFields;
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


procedure TP3DActor.MoveAlongGlobalAxis( AAxis: TVec3; AValue: Float );
begin
  Position:= Position + AAxis * AValue;
end;

procedure TP3DActor.MoveAlongLocalAxis( AAxis: TVec3; AValue: Float );
begin
  Position:= Position + ( vec4( AAxis, 0 ) * Matrix ).xyz * AValue;
end;

procedure TP3DActor.RotateAroundGlobalAxis( AAxis: TVec3; AValue: Float );
begin
  Quaternion:= Quaternion * quatRotation( AAxis, AValue );
end;

procedure TP3DActor.RotateAroundLocalAxis( AAxis: TVec3; AValue: Float );
begin
  //TODO: TP3DActor.RotateAroundLocalAxis
end;

class function TP3DActor.DOMNodeName: String;
begin
  Result:= 'object';
end;

{ TP3DActorList }

constructor TP3DActorList.Create(AOwner: TPersistent);
begin
  inherited Create();
  Owner:= AOwner;
end;

procedure TP3DActorList.Render( world: TMat4; Scene: TP3DScene );
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ] is TP3DActor ) then
      if ( TP3DActor( Items[ i ]).Visible ) then
        TP3DActor( Items[ i ]).Render( world, Scene );
end;

procedure TP3DActorList.RenderCallBackOnly(world: TMat4; Scene: TP3DScene);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ] is TP3DActor ) then
      if ( TP3DActor( Items[ i ]).Visible ) then
        TP3DActor( Items[ i ]).RenderCallbackOnly( world, Scene );
end;

procedure TP3DActorList.MakeRenderList(ARootScene: TP3DScene; world: TMat4; var ARenderList: TP3DRenderList; const AFlags: TP3DRenderListFlags);
var
  Actor: TP3DActor;
  mat: TMat4;
begin
  if ( not Assigned( ARenderList )) then
    ARenderList:= TP3DRenderList.Create;
  for Actor in Self do
    begin
      mat:= world * Actor.Matrix;
      Actor.FMatrixFinal:= mat;
      ARenderList.Add( Actor.Data, mat, ARootScene, AFlags );
      Actor.Children.MakeRenderList( ARootScene, mat, ARenderList, AFlags );
    end;
end;

function TP3DActorList.OutputDebugInfo: String;
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
    Result+= Format( 'Name: "%s" Class "%s" Visible %s Data: "%s"'+LineEnding, [ Item.Name, Item.ClassName, BoolToStr( TP3DActor( Item ).Visible, 'Yes', 'No' ), DebugData( TP3DActor( Item ).Data )]);
end;

function TP3DActorList.CalcBoundingBox: TP3DBoundingBox;
var
  Obj: TP3DObject;
  BB: TP3DBoundingBox;
begin
  Result:= P3DBoundingBox( vec3( 0 ), vec3( 0 ), vec3( 0 ));
  for Obj in Self do
    begin
      BB:= TP3DActor( Obj ).CalcBoundingBox();
      Result.Min:= Min( Result.Min, BB.Min );
      Result.Max:= Max( Result.Max, BB.Max );
    end;
  Result.Center:= ( Result.Min + Result.Max ) / 2;
end;

function TP3DActorList.CastRay(Orig, Dir: TVec3; out HitPos: TVec3): TP3DActor;
var
  Actor, CastActor: TP3DActor;
  ClosestHitpos: TVec3;
begin
  Result:= nil;
  ClosestHitpos:= vec3( MaxFloat );
  for Actor in Self do
    if ( Actor.CastRay( Orig, Dir, HitPos )) then
      begin
        if ( distance( Orig, HitPos ) < distance( Orig, ClosestHitpos )) then
          begin
            Result:= Actor;
            ClosestHitpos:= HitPos;
          end;
      end
    else
      begin
        if ( Actor.Data is TP3DScene ) then
          CastActor:= TP3DScene( Actor.Data ).Objects.CastRay( Orig, Dir, HitPos )
        else
          CastActor:= nil;
        if ( Assigned( CastActor )) then
          if ( distance( Orig, HitPos ) < distance( Orig, ClosestHitpos )) then
            begin
              Result:= CastActor;
              ClosestHitpos:= HitPos;
            end;
        CastActor:= Actor.Children.CastRay( Orig, Dir, HitPos );
        if ( Assigned( Result )) then
          if ( distance( Orig, HitPos ) < distance( Orig, ClosestHitpos )) then
            begin
              Result:= Actor;
              ClosestHitpos:= HitPos;
            end;
      end;
end;

{$ENDIF}

