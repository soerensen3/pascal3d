//p3dmodel_resource.inc

{$IFDEF INTERFACE}
type
 { TP3DActor }

  TP3DActorList = class;
  TP3DActor = class ( TP3DDataBlock )
    private
      FChildren: TP3DActorList;
      FData: TP3DDataBlock;
      FPosition: TVec3;
      FQuaternion: TQuaternion;
      FRotation: TVec3;
      FScale: TVec3;
      FVisible: Boolean;
      FMatrix: TMat4;

      function GetDirection: TVec3;
      procedure SetData(AValue: TP3DDataBlock);
      procedure SetMatrix(AValue: TMat4);
      procedure SetPosition(AValue: TVec3);
      procedure SetQuaternion(AValue: TQuaternion);
      procedure SetRotation(AValue: TVec3);
      procedure SetScale(AValue: TVec3);
      property BoundingBox;
      procedure UpdateMatrix;
      procedure UpdateEuler;

    public
      constructor Create( const AData: TP3DData = nil );
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil );

      destructor Destroy; override;
      procedure Render( world: TMat4; Scene: TP3DScene ); reintroduce;
      function CalcBoundingBox(): TP3DBoundingBox; override;
      procedure NotifyDeleteOfData(Sender: TP3DDataBlock); override;

      property Position: TVec3 read FPosition write SetPosition;
      property Direction: TVec3 read GetDirection;
      property Rotation: TVec3 read FRotation write SetRotation;
      property Scale: TVec3 read FScale write SetScale;
      property Matrix: TMat4 read FMatrix write SetMatrix;
      property Quaternion: TQuaternion read FQuaternion write SetQuaternion;

    published
      property Children: TP3DActorList read FChildren;
      property Visible: Boolean read FVisible write FVisible;
      property Data: TP3DDataBlock read FData write SetData;
  end;

  TP3DCustomActorList = specialize gP3DCustomDatablockList < TP3DActor >;

  { TP3DActorList }
  TP3DActorList = class( TP3DCustomActorList )
    public
      procedure Render( world: TMat4; Scene: TP3DScene );
      function OutputDebugInfo: String;

      function CalcBoundingBox(): TP3DBoundingBox;
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
  FMatrix:= mat4scale( vec4( Scale, 1 )) * mat4( mat3( Quaternion )) * mat4translate( vec4( Position, 1 ));
end;

procedure TP3DActor.UpdateEuler;
begin
  //TODO: QUAT2EULER
  //Rotation:= quat2euler;
end;

procedure TP3DActor.SetData(AValue: TP3DDataBlock);
begin
  if FData=AValue then Exit;

  if ( Assigned( FData )) then
    FData.Users.Delete( FData.Users.IndexOf( Self ));

  FData:= AValue;

  if ( Assigned( FData )) then
    FData.Users.Add( Self );
end;

procedure TP3DActor.SetMatrix(AValue: TMat4);
begin
  if FMatrix= AValue then Exit;
  FMatrix:= AValue;
  FPosition:= vec3( Matrix._30, Matrix._31, Matrix._32 );
  FRotation:= mat4toEuler( AValue );
  FScale:= mat3toScale( mat3( FMatrix ));
end;


function TP3DActor.GetDirection: TVec3;
begin
  Result:= vec3( Matrix._20, Matrix._21, Matrix._22 );
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
  Quaternion:= quatEuler( AValue * deg2rad );
  UpdateMatrix;
end;

procedure TP3DActor.SetScale(AValue: TVec3);
begin
  if ( FScale = AValue ) then Exit;
  FScale:= AValue;
  UpdateMatrix;
end;

constructor TP3DActor.Create(const AData: TP3DData);
begin
  inherited;
  FChildren:= TP3DActorList.Create;
  FVisible:= True;
  Parent.Objects.Add( Self );
  FScale:= vec3( 1 );
end;

constructor TP3DActor.CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData );
var
  Element: TDOMElement;
  DataType: DOMString;
  DataName: DOMString;
  Child: TP3DActor;
  n: Integer;

  procedure LoadTransformFromDOM( DOM: TDOMElement );
  begin
    FPosition.FromString( DOM.GetAttribute( 'position' ));
    FQuaternion.FromString( DOM.GetAttribute( 'quaternion' ));
    FQuaternion:= -FQuaternion; //TODO: FIND OUT WHY THIS IS NECESSARY
    FScale.FromString( DOM.GetAttribute( 'scale' ));
    UpdateMatrix;
  end;
begin
  Create( AData );
  //if ( AutoNames ) then
  //  Name:= AParentList.FindUniqueName( ADOMNode.GetAttribute( 'name' ))
  //else
  Name:= ADOMNode.GetAttribute( 'name' );

  DataType:= ADOMNode.GetAttribute( 'type' );
  DataName:= ADOMNode.GetAttribute( 'data' );
  if ( DataName > '' ) then
    begin
      n:= AData.AppendFromDom( TDOMElement( ADOMNode.OwnerDocument.FirstChild ), DataName, FindClassForDOM( DataType ));
      Data:= TP3DDataBlock( AData.DataBlocks[ n ]);
    end;

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'matrix': Matrix:= LoadMat4FromDOM( Element );
        'transform': LoadTransformFromDom( Element );
        'object': begin Child:= TP3DActor.CreateFromDOM( Element, Parent ); Children.Add( Child ); end;
      else
        raise Exception.Create( 'Unknown tag inside Object Element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
end;


destructor TP3DActor.Destroy;
begin
  Data:= nil;
  FChildren.Clear;
  FChildren.Free;
  inherited Destroy;
end;

procedure TP3DActor.Render( world: TMat4; Scene: TP3DScene );
var
  _world: TMat4;
begin
  _world:= Matrix * world;
  if ( Assigned( Data )) then
    Data.Render( _world, Scene, Self );
  Children.Render( _world, Scene );
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

procedure TP3DActor.NotifyDeleteOfData(Sender: TP3DDataBlock);
begin
  inherited NotifyDeleteOfData(Sender);
  if ( Sender = Data ) then
    Data:= nil;
end;

{ TP3DActorList }

procedure TP3DActorList.Render( world: TMat4; Scene: TP3DScene );
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ] is TP3DActor ) then
      if ( TP3DActor( Items[ i ]).Visible ) then
        TP3DActor( Items[ i ]).Render( world, Scene );
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

{$ENDIF}
