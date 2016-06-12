//p3dmodel_datablock.inc

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

type

  { TP3DDataBlock }

  TP3DDataBlockList = class;
  TP3DDataBlock = class ( TP3DObject )
    private
      FBoundingBox: TP3DBoundingBox;
      FFileWatch: TP3DFileWatch;
      FParent: TP3DData;
      FUsers: TP3DDataBlockList;

    public
      constructor Create( const AData: TP3DData = nil );
      destructor Destroy; override;

      procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor ); virtual;
      procedure LoadFromDOM( ADOMNode: TDOMElement ); virtual;
      procedure Reload; virtual;
      function CalcBoundingBox(): TP3DBoundingBox; virtual;
      procedure NotifyDeleteOfData( Sender: TP3DDataBlock ); virtual;
      function CastRay( Orig, Dir: TVec3; Actor: TP3DActor; out HitPos: TVec3 ): Boolean; virtual;

      property BoundingBox: TP3DBoundingBox read FBoundingBox write FBoundingBox;
      property Parent: TP3DData read FParent write FParent;

    published
      property FileWatch: TP3DFileWatch read FFileWatch write FFileWatch;
      property Users: TP3DDataBlockList read FUsers;
  end;

  TP3DDataBlockType = class of TP3DDataBlock;


  { gP3DCustomDatablockList }

  generic gP3DCustomDatablockList <TCustomItem: TP3DDataBlock> = class ( TPersistent )
    type
      TP3DCustomObjectListEnumerator = specialize gP3DCustomListEnumeratorClass < TCustomItem >;
    private
      FFreeChildrenOnDestroy: Boolean;
      FItems: TList;
      FOnChange: TNotifyEvent;
      function GetCount: Integer;
      procedure SetCount( AValue: Integer );
      function GetItems( Index: Integer ): TCustomItem;
      function MoveNext( var Index: Integer ): TCustomItem;

    public
      constructor Create;
      destructor Destroy; override;

      function Add( Item: TCustomItem ): Integer; virtual;
      procedure Delete( Index: Integer ); virtual;
      function IndexOf( Item: TCustomItem ): Integer; virtual;
      procedure Clear( const FreeObjects: Boolean = False ); virtual;
      function GetEnumerator(): TP3DCustomObjectListEnumerator;
      procedure Insert( Index: Integer; Item: TCustomItem );
      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
      function FindByFileName( AFileName: String ): Integer;

      property Items[ Index: Integer ]: TCustomItem read GetItems; default;

    published
      property Count: Integer read GetCount write SetCount;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property FreeChildrenOnDestroy: Boolean read FFreeChildrenOnDestroy write FFreeChildrenOnDestroy;
  end;

  TP3DCustomDataBlockList = specialize gP3DCustomDatablockList <TP3DDataBlock>;
  TP3DDataBlockList = class ( TP3DCustomDataBlockList ) end;

  function P3DBoundingBox( vMin, vMax, vCenter: TVec3 ): TP3DBoundingBox;
  function P3DBoundingBoxTransform( Box: TP3DBoundingBox; Matrix: TMat4 ): TP3DBoundingBox;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

function FindDOMForClass( AClass: TP3DDataBlockType ): String;
begin
  case AClass.ClassName of
    'TP3DMaterialShader', 'TP3DMaterialBase': Result:= 'material';
    'TP3DMesh': Result:= 'mesh';
    'TP3DLight': Result:= 'lamp';
    'TP3DCamera': Result:= 'camera';
    'TP3DActor': Result:= 'object';
    'TP3DScene': Result:= 'scene';
  end;
end;

function FindClassForDOM( ADOMName: String ): TP3DDataBlockType;
begin
  case
    ADOMName of
      'material': Result:= TP3DMaterialShader;
      'mesh': Result:= TP3DMesh;
      'light', 'lamp': Result:= TP3DLight;
      'camera': Result:= TP3DCamera;
      'object': Result:= TP3DActor;
      'scene': Result:= TP3DScene;
    end;
end;

function FindClassForDOM( ADOMNode: TDOMElement ): TP3DDataBlockType;
begin
  case
    ADOMNode.NodeName of
      'material':
        if ( ADOMNode.GetAttribute( 'type' ) = 'shader' ) then
          Result:= TP3DMaterialShader
        else
          Result:= TP3DMaterialBase;
      'mesh': Result:= TP3DMesh;
      'light', 'lamp': Result:= TP3DLight;
      'camera': Result:= TP3DCamera;
      'object': Result:= TP3DActor;
      'scene': Result:= TP3DScene;
    end;
end;

{ gP3DCustomDatablockList }
function gP3DCustomDatablockList.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure gP3DCustomDatablockList.SetCount( AValue: Integer );
begin
  FItems.Count:= AValue;
end;

function gP3DCustomDatablockList.GetItems( Index: Integer ): TCustomItem;
begin
  if ( Index = -1 ) then
    Result:= nil
  else
    Result:= TCustomItem( FItems[ Index ]);
end;

constructor gP3DCustomDatablockList.Create;
begin
  inherited;
  FItems:= TList.Create;
end;

destructor gP3DCustomDatablockList.Destroy;
begin
  Clear( FFreeChildrenOnDestroy );
  FItems.Free;
  inherited;
end;

function gP3DCustomDatablockList.Add( Item: TCustomItem ): Integer;
begin
  Result:= IndexOf( Item );
  if ( Result = -1 ) then
    begin
      Result:= FItems.Add( Item );
      if ( Assigned( OnChange )) then
        OnChange( Self );
    end;
end;

procedure gP3DCustomDatablockList.Insert( Index: Integer; Item: TCustomItem );
begin
  FItems.Insert( Index, Item );
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

function gP3DCustomDatablockList.FindUniqueName(BaseStr: String): String;
var
  I: Integer;
begin
  I:= 0;
  repeat
    Inc( I );
    Result:= BaseStr + IntToStr( I );
  until ( FindByName( Result ) = -1 );
end;

function gP3DCustomDatablockList.FindByName(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = AName ) then
      begin
        Result:= i;
        break;
      end;
end;

function gP3DCustomDatablockList.FindByFileName(AFileName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Self[ i ].FileWatch.FileName = ExpandFileName( AFileName )) then
      begin
        Result:= i;
        break;
      end;
end;

procedure gP3DCustomDatablockList.Delete( Index: Integer );
begin
  FItems.Delete( Index );
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

function gP3DCustomDatablockList.IndexOf( Item: TCustomItem ): Integer;
begin
  Result:= Fitems.IndexOf( Item );
end;

procedure gP3DCustomDatablockList.Clear( const FreeObjects: Boolean );
var
  i: Integer;
begin
  if ( FreeObjects ) then
    for i:= 0 to Self.Count - 1 do
      Items[ i ].Free;

  FItems.Clear;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

function gP3DCustomDatablockList.GetEnumerator(): TP3DCustomObjectListEnumerator;
begin
  Result:= TP3DCustomObjectListEnumerator.Create( @MoveNext );
end;

function gP3DCustomDatablockList.MoveNext( var Index: Integer ): TCustomItem;
begin
  Inc( Index );
  if ( Index < Count ) then
    Result:= Items[ Index ]
  else
    Result:= nil;
end;

function P3DBoundingBox(vMin, vMax, vCenter: TVec3): TP3DBoundingBox;
begin
  Result.Center:= vCenter;
  Result.Min:= vMin;
  Result.Max:= vMax;
end;

function P3DBoundingBoxTransform(Box: TP3DBoundingBox; Matrix: TMat4 ): TP3DBoundingBox;
  procedure _MinMax( p: TVec3 ); inline;
  begin
    p:= ( Matrix * vec4( p, 1 )).xyz;
    P3DBoundingBoxTransform.Min:= Min( P3DBoundingBoxTransform.Min, p );
    P3DBoundingBoxTransform.Max:= Max( P3DBoundingBoxTransform.Max, p );
  end;

begin  // This could be optimized more ... but well..
  Result:= P3DBoundingBox( vec3( 0 ), vec3( 0 ), vec3( 0 ));
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

{ TP3DDataBlock }

constructor TP3DDataBlock.Create(const AData: TP3DData);
begin
  if ( AData = nil ) then
    Parent:= P3DData
  else
    Parent:= AData;
  inherited Create( Parent.DataBlocks );
  FileWatch:= TP3DFileWatch.Create( '<empty>', Self );
  BoundingBox:= P3DInvalidBoundingBox;
  FUsers:= TP3DDataBlockList.Create;
end;

destructor TP3DDataBlock.Destroy;
var
  User: TP3DDataBlock;
begin
  for User in Users do
    User.NotifyDeleteOfData( Self );
  FUsers.Free;
  FFileWatch.Free;
  inherited Destroy;
end;

procedure TP3DDataBlock.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DActor);
begin

end;

procedure TP3DDataBlock.LoadFromDOM(ADOMNode: TDOMElement);
begin

end;

procedure TP3DDataBlock.Reload;
begin

end;

function TP3DDataBlock.CalcBoundingBox: TP3DBoundingBox;
begin
  FBoundingBox.Center:= vec3( 0 );
  FBoundingBox.Max:= vec3( 0 );
  FBoundingBox.Min:= vec3( 0 );
  Result:= BoundingBox;
end;

procedure TP3DDataBlock.NotifyDeleteOfData( Sender: TP3DDataBlock );
begin
  //Nothing to do
end;

function TP3DDataBlock.CastRay(Orig, Dir: TVec3; Actor: TP3DActor; out
  HitPos: TVec3): Boolean;
begin
  Result:= False;
end;

operator=(A, B: TP3DBoundingBox): Boolean;
begin
  Result:= ( A.Min = B.Min ) and ( A.Max = B.Max ) and ( A.Center = B.Center );
end;

operator+(A: TP3DBoundingBox; B: TVec3): TP3DBoundingBox;
begin
  Result:= P3DBoundingBox( A.Min + B, A.Max + B, A.Center + B );
end;

operator-(A: TP3DBoundingBox; B: TVec3): TP3DBoundingBox;
begin
  Result:= P3DBoundingBox( A.Min - B, A.Max - B, A.Center - B );
end;

{$ENDIF}
