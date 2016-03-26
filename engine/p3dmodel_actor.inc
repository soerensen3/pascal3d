//p3dmodel_resource.inc

{$IFDEF INTERFACE}
type
 { TP3DActor }

  //TP3DActorList = class;
  TP3DActor = class ( TP3DDataBlock )
    private
      FChildren: TP3DActorList;
      FData: TP3DDataBlock;
      FVisible: Boolean;

      function GetDirection: TVec3;
      function GetPosition: TVec3;
      procedure SetData(AValue: TP3DDataBlock);
      procedure SetDirection(AValue: TVec3);
      procedure SetPosition(AValue: TVec3);
      property BoundingBox;

    public
      Matrix: TMat4;

      constructor Create( AParentList: TP3DObjectList );
      constructor CreateFromDOM( AParentList: TP3DObjectList; ADOMNode: TDOMElement; AData: TP3DData = nil );

      destructor Destroy; override;
      procedure Render( world: TMat4; Scene: TP3DScene ); reintroduce;
      function CalcBoundingBox(): TP3DBoundingBox; override;

      property Position: TVec3 read GetPosition write SetPosition;
      property Direction: TVec3 read GetDirection write SetDirection;

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

{$ENDIF}

