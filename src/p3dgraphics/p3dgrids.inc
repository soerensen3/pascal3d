
{$IFDEF INTERFACE}

{ TP3DTileGrid }

TP3DTileGrid = class ( TP3DDatablock )
  private
    FGridWorldUnit: Single;
    FHeight: Integer;
    FInstanceTex: TP3DTextureList;
    FMeshes: TP3DDataBlockList;
    FWidth: Integer;
    FGridData: TByteList;
    function GetIndexAt( x, y: Integer ): Byte;
    function GetMeshAt( x, y: Integer ): TP3DDataBlock;
    procedure SetHeight( AValue: Integer );
    procedure SetIndexAt( x, y: Integer; AValue: Byte );
    procedure SetMeshAt( x, y: Integer; AValue: TP3DDataBlock );
    procedure SetWidth( AValue: Integer );

  protected
    procedure UpdateSize;
    procedure Inititalize;
    procedure UpdateTextures;

  public
    constructor Create( const AData: TP3DData=nil ); override;
    destructor Destroy; override;

    procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor ); override;

    property MeshAt[ x, y: Integer ]: TP3DDataBlock read GetMeshAt write SetMeshAt;
    property IndexAt[ x, y: Integer ]: Byte read GetIndexAt write SetIndexAt;

  published
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Meshes: TP3DDataBlockList read FMeshes write FMeshes;
    property InstanceTex: TP3DTextureList read FInstanceTex write FInstanceTex;
    property GridWorldUnit: Single read FGridWorldUnit write FGridWorldUnit;
    property GridData: TByteList read FGridData write FGridData;
end;

TP3DMaterialModifierGrid = class ( TP3DMaterialModifier )

end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DTileGrid }

function TP3DTileGrid.GetIndexAt( x, y: Integer ): Byte;
begin
  Result:= GridData[ x + y * Width ];
end;

function TP3DTileGrid.GetMeshAt(x, y: Integer): TP3DDataBlock;
var
  Idx: Byte;
begin
  Idx:= GridData[ x + y * Width ];
  if ( InRange( Idx, 0, Meshes.Count - 1 )) then
    Result:= Meshes[ Idx ];
end;

procedure TP3DTileGrid.SetHeight( AValue: Integer );
begin
  if ( FHeight = AValue ) then
    Exit;
  FHeight:= AValue;
  UpdateSize;
end;

procedure TP3DTileGrid.SetIndexAt( x, y: Integer; AValue: Byte );
begin
  GridData[ x + y * Width ]:= AValue;
end;

procedure TP3DTileGrid.SetMeshAt(x, y: Integer; AValue: TP3DDataBlock);
begin
  GridData[ x + y * Width ]:= Meshes.IndexOf( AValue );
end;

procedure TP3DTileGrid.SetWidth( AValue: Integer );
begin
  if ( FWidth = AValue ) then
    Exit;

  FWidth:= AValue;
  UpdateSize;
end;

procedure TP3DTileGrid.UpdateSize;
begin
  GridData.Count:= Width * Height;
  Inititalize;
end;

procedure TP3DTileGrid.Inititalize;
var
  i: Integer;
begin
  for i:= 0 to GridData.Count - 1 do
    GridData[ i ]:= 0;
end;

procedure TP3DTileGrid.UpdateTextures;
begin
  {for i:= 0 to Meshes.Count - 1 do
    for j:= 0 to Width * Height - 1 do
      if (( GridData[ j ] = i ) and ( Assigned( Meshes[ i ]))) then
        begin
          p:= vec2( j mod Width, j div Width ) * GridWorldUnit;
          world * mat4translate( vec4( p, 0, 1 );
        end;}
end;

constructor TP3DTileGrid.Create( const AData: TP3DData );
begin
  inherited Create( AData );
  Meshes:= TP3DDataBlockList.Create;
  GridData:= TByteList.Create;
  GridWorldUnit:= 1;
  FWidth:= 4;
  Height:= 4;
  InstanceTex:= TP3DTextureList.Create;
end;

destructor TP3DTileGrid.Destroy;
begin
  FInstanceTex.Clear( True );
  FreeAndNil( FInstanceTex );
  FreeAndNil( FMeshes );
  FreeAndNil( FGridData );
  inherited Destroy;
end;

procedure TP3DTileGrid.Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor );
var
  i, j: Integer;
  p: TVec2;
begin
  inherited Render( world, Scene, RenderObject );
  for i:= 0 to Meshes.Count - 1 do
    for j:= 0 to Width * Height - 1 do
      if (( GridData[ j ] = i ) and ( Assigned( Meshes[ i ]))) then
        begin
          p:= vec2( j mod Width, j div Width ) * GridWorldUnit;
          Meshes[ i ].Render( world * mat4translate( vec4( p, 0, 1 )), Scene, RenderObject );
        end;
end;


{$ENDIF}
