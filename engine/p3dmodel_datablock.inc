//p3dmodel_datablock.inc

{$IFDEF INTERFACE}
type

  { TP3DDataBlock }

  TP3DDataBlock = class ( TP3DObject )
    private
      FBoundingBox: TP3DBoundingBox;
      FFileWatch: TP3DFileWatch;
      FUsers: TP3DActorList;

    public
      constructor Create( AParentList: TP3DObjectList );
      destructor Destroy; override;

      procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor ); virtual; abstract;
      procedure LoadFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil ); virtual; abstract;
      function CalcBoundingBox(): TP3DBoundingBox; virtual;

      property BoundingBox: TP3DBoundingBox read FBoundingBox write FBoundingBox;

    published
      property FileWatch: TP3DFileWatch read FFileWatch write FFileWatch;
      property Users: TP3DActorList read FUsers;
  end;

  { gP3DCustomObjectList }

  { gP3DCustomDatablockList }

  generic gP3DCustomDatablockList <TCustomItem: TP3DDataBlock> = class ( TPersistent )
    type
      TP3DCustomObjectListEnumerator = specialize gP3DCustomListEnumerator < TCustomItem >;
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
      property Count: Integer read GetCount write SetCount;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property FreeChildrenOnDestroy: Boolean read FFreeChildrenOnDestroy write FFreeChildrenOnDestroy;
  end;

  TP3DDataBlockList = specialize gP3DCustomDatablockList <TP3DDataBlock>;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

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
  i:= -1;
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

{$ENDIF}
