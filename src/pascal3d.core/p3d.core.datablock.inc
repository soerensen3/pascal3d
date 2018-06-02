//p3ddatablock.inc

{$IFDEF INTERFACE}

type
  TP3DRenderListFlag = ( rlfMaterials, rlfIndexColors, rlfWireFrame, rlfDepthTest, rlfLighting, rlfRenderEdges, rlfObjects, rlfMeshes, rlfScenes, rlfTileGrids, rlfJoints );
  TP3DRenderListFlags = set of TP3DRenderListFlag;

 const
   P3DRenderListFlagsDefault = [ rlfMaterials, rlfDepthTest, rlfLighting, rlfMeshes, rlfScenes, rlfTileGrids ];

type
  TP3DRenderList = class;
  //TP3DAction = class;

  { TP3DDataBlock

    introduces the following functionalities
    - It can be loaded from a link to a json path inside a json file.
    - It has a timestamp of the json file to check if it was already loaded
    - It offers a reload function to reload the block from a json or binary file.
    - It introduces reference counting (Users) if another datablock's property points to it.
    - Every datablock has a library as an owner.
    }
  TP3DDataBlock = class abstract( TP3DNamedStreamable )
    private
      FOwnerLib: TP3DStreamablePointer;
      FTimeStamp: Integer;
      function GetOwnerLib: TP3DLibrary;

    public
      class function GetContainer( ALib: TP3DLibrary ): TP3DPropertyAccess; virtual; abstract;

    public
      constructor Create( const AOwner: IP3DStreamableContainer = nil ); override; overload;
      constructor CreateBlock( const AData: TP3DLibrary = nil );
      destructor Destroy; override;

      property OwnerLib: TP3DLibrary read GetOwnerLib;
      property TimeStamp: Integer read FTimeStamp;
  end;

  TP3DDataBlockType = class of TP3DDataBlock;

  {TP3DDataBlockContainer = specialize gP3DStreamableContainer < TP3DDataBlock >;
  TP3DDataBlockPointer = specialize gP3DStreamablePointer < TP3DDataBlock >;}
  TP3DDataBlockContainerList = specialize gP3DNamedStreamableContainerList < TP3DDataBlock >;
  TP3DDataBlockPointerList = specialize gP3DStreamablePointerList < TP3DDataBlock >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure StringSplit( S: String; Delim: Char; out S1, S2: String );
begin
  S1:= ExtractWord( 1, S, [ Delim ]);
  S2:= Copy( S, Length( S1 ) + Length( Delim ) + 1, Length( S ));
end;


{ TP3DDataBlock }

function TP3DDataBlock.GetOwnerLib: TP3DLibrary;
begin
  Result:= TP3DLibrary( FOwnerLib.AsValue );
end;

constructor TP3DDataBlock.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FOwnerLib:= TP3DStreamablePointer.Create( 'OwnerLib', TP3DLibrary );
  FOwnerLib.AsValue:= TP3DStreamable( Owner.GetRoot.GetInstance );

  Properties.Add( FOwnerLib );
end;

constructor TP3DDataBlock.CreateBlock(const AData: TP3DLibrary);
var
  _OwnerLib: TP3DLibrary;
  p: TP3DPropertyAccess;
begin
  if ( AData = nil ) then
    _OwnerLib:= P3DData
  else
    _OwnerLib:= AData;

  p:= GetContainer( _OwnerLib );
  if ( not ( p is IP3DStreamableContainer )) then
    raise Exception.Create( 'No container found for ' + ClassName );

  Create( p as IP3DStreamableContainer );
end;

destructor TP3DDataBlock.Destroy;
begin
  WriteLn( 'Free datablock: ' + Name );
  inherited Destroy;
end;


{$ENDIF}
