{$IFDEF INTERFACE}
type
  TP3DBoundingBox = record
    Min, Max, Center: TVec3;
  end;

  operator = ( A, B: TP3DBoundingBox ): Boolean;
  operator + ( A: TP3DBoundingBox; B: TVec3 ): TP3DBoundingBox;
  operator - ( A: TP3DBoundingBox; B: TVec3 ): TP3DBoundingBox;


const
  P3DInvalidBoundingBox: TP3DBoundingBox
      = ( Min: ( FCoord: ( -MaxSingle, -MaxSingle, -MaxSingle ));
          Max: ( FCoord: ( MaxSingle, MaxSingle, MaxSingle ));
          Center: ( FCoord: ( 0, 0, 0 )));

  function BoundingBox( vMin, vMax, vCenter: TVec3 ): TP3DBoundingBox;
  function TransformBoundingBox( Box: TP3DBoundingBox; Matrix: TMat4 ): TP3DBoundingBox;

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

{ TP3DDataList }

function TP3DDataList.FindByFileName(AFileName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].FileWatch.FileName = ExpandFileName( AFileName )) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DActor.GetPosition: TVec3;
begin
  Result:= vec3( Matrix._30, Matrix._31, Matrix._32 );
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

function TP3DActor.GetDirection: TVec3;
begin
  Result:= vec3( Matrix._20, Matrix._21, Matrix._22 );
end;

procedure TP3DActor.SetDirection(AValue: TVec3);
begin
  Matrix._20:= AValue.X;
  Matrix._21:= AValue.Y;
  Matrix._22:= AValue.Z;
end;

procedure TP3DActor.SetPosition(AValue: TVec3);
begin
  Matrix._30:= AValue.X;
  Matrix._31:= AValue.Y;
  Matrix._32:= AValue.Z;
end;

constructor TP3DActor.Create( AParentList: TP3DObjectList );
begin
  inherited;
  FChildren:= TP3DActorList.Create;
  FVisible:= True;
end;

constructor TP3DActor.CreateFromDOM(AParentList: TP3DObjectList;
  ADOMNode: TDOMElement; AData: TP3DData);
var
  Element: TDOMElement;
  DataType: DOMString;
  DataName: DOMString;
  Child: TP3DActor;
  n: Integer;
begin
  Create( AParentList );
  //if ( AutoNames ) then
  //  Name:= AParentList.FindUniqueName( ADOMNode.GetAttribute( 'name' ))
  //else
  Name:= ADOMNode.GetAttribute( 'name' );

  if ( AData = nil ) then
    AData:= P3DData;

  DataType:= ADOMNode.GetAttribute( 'type' );
  DataName:= ADOMNode.GetAttribute( 'data' );
  case DataType of
    'mesh': Data:= AData.Meshes[( AData.Meshes.FindByName( DataName ))];    //SAFE EVEN IF NOT FOUND
    'material': Data:= AData.Materials[( AData.Materials.FindByName( DataName ))];
    'lamp': Data:= AData.Lights[( AData.Lights.FindByName( DataName ))];
    'camera': Data:= AData.Cameras[( AData.Cameras.FindByName( DataName ))];
  end;
  if ( not Assigned( Data )) then
    raise Exception.Create( Format( 'Error while creating object "%s"! A data block of type "%s" with the name "%s" the was not found!', [ Name, DataType, DataName ]));

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'matrix': Matrix:= LoadMat4FromDOM( Element );
        'object': begin Child:= TP3DActor.CreateFromDOM( AData.DataBlocks, Element, AData ); Children.Add( Child ); end;
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
      Result:= TransformBoundingBox( BB, Matrix );
    end
  else
    Result:= p3dmodel.BoundingBox( Position, Position, Position );
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
  Result:= BoundingBox( vec3( 0 ), vec3( 0 ), vec3( 0 ));
  for Obj in Self do
    begin
      BB:= TP3DActor( Obj ).CalcBoundingBox();
      Result.Min:= Min( Result.Min, BB.Min );
      Result.Max:= Max( Result.Max, BB.Max );
    end;
  Result.Center:= ( Result.Min + Result.Max ) / 2;
end;


operator=(A, B: TP3DBoundingBox): Boolean;
begin
  Result:= ( A.Min = B.Min ) and ( A.Max = B.Max ) and ( A.Center = B.Center );
end;

operator+(A: TP3DBoundingBox; B: TVec3): TP3DBoundingBox;
begin
  Result:= BoundingBox( A.Min + B, A.Max + B, A.Center + B );
end;

operator-(A: TP3DBoundingBox; B: TVec3): TP3DBoundingBox;
begin
  Result:= BoundingBox( A.Min - B, A.Max - B, A.Center - B );
end;

function calculateAttenuation( LightSources: TP3DLightList; i: Integer; dist: Float ): Float;
begin
  Result:= ( 1.0 / ( max( 0.01, //LightSources[i].constantAttenuation + //useless
                     LightSources[ i ].linearAttenuation * dist +
                     LightSources[ i ].quadraticAttenuation * dist * dist )));
end;


function BoundingBox(vMin, vMax, vCenter: TVec3): TP3DBoundingBox;
begin
  Result.Center:= vCenter;
  Result.Min:= vMin;
  Result.Max:= vMax;
end;

function TransformBoundingBox( Box: TP3DBoundingBox; Matrix: TMat4 ): TP3DBoundingBox;
  procedure _MinMax( p: TVec3 ); inline;
  begin
    p:= ( Matrix * vec4( p, 1 )).xyz;
    TransformBoundingBox.Min:= Min( TransformBoundingBox.Min, p );
    TransformBoundingBox.Max:= Max( TransformBoundingBox.Max, p );
  end;

begin  // This could be optimized more ... but well..
  Result:= BoundingBox( vec3( 0 ), vec3( 0 ), vec3( 0 ));
  with ( Box ) do
    begin
      _MinMax( Min );
      _MinMax( vec3( Min.xy, Max.z ));
      _MinMax( vec3( Min.x, Max.y, Min.z ));
      _MinMax( vec3( Min.x, Max.yz ));
      _MinMax( vec3( Max.x, Min.yz ));
      _MinMax( vec3( Max.x, Min.y, Max.z ));
      _MinMax( vec3( Max.xy, Min.z ));
      _MinMax( Max );
    end;
  Result.Center:= ( Result.Max + Result.Min ) / 2;
end;


{$ENDIF}
