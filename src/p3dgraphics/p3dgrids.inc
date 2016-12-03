
{$IFDEF INTERFACE}

{ TP3DTileGrid }

TP3DTileGrid = class ( TP3DDatablock )
  private
    FGridWorldUnit: Single;
    FHeight: Integer;
    FInstances: TP3DIntBufferGLList;
    FMeshes: TP3DDataBlockList;
    FModifiers: TP3DRenderListModifierList;
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

  public
    procedure UpdateArrays;

    constructor Create( const AData: TP3DData=nil ); override;
    destructor Destroy; override;

    procedure PassToShader( i, baseInstanceCount: Integer );

    property MeshAt[ x, y: Integer ]: TP3DDataBlock read GetMeshAt write SetMeshAt;
    property IndexAt[ x, y: Integer ]: Byte read GetIndexAt write SetIndexAt;
    procedure OnGridDataSet( Sender: TPersistent; ItemIndex: Integer; AValue: Byte );

  published
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
    property Meshes: TP3DDataBlockList read FMeshes write FMeshes;
    property GridWorldUnit: Single read FGridWorldUnit write FGridWorldUnit;
    property GridData: TByteList read FGridData write FGridData;
    property Instances: TP3DIntBufferGLList read FInstances write FInstances;
    property Modifiers: TP3DRenderListModifierList read FModifiers write FModifiers;
end;

{ TP3DRenderListModiferGrid }

TP3DRenderListModiferGrid = class ( TP3DRenderListModifier )
  private
    FGrid: TP3DTileGrid;
    FGridIndex: Integer;
  public
    constructor Create( AGrid: TP3DTileGrid; AGridIndex: Integer );

    procedure PassToShader( Obj: TP3DRenderObject ); override;
    procedure AddObject( var Obj: TP3DRenderObject ); override;

    property Grid: TP3DTileGrid read FGrid write FGrid;
    property GridIndex: Integer read FGridIndex write FGridIndex;
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

procedure TP3DTileGrid.UpdateArrays;
var
  i: Integer;
  j: Byte;
  Modifier: TP3DRenderListModiferGrid;
begin
  Instances.Count:= Meshes.Count;
  for i:= 0 to Meshes.Count - 1 do
    begin
      if ( Assigned( Meshes[ i ])) then
        begin
          if ( not Assigned( Instances[ i ])) then
            Instances.Items[ i ]:= TP3DIntBufferGL.Create( True )
          else
            Instances[ i ].Clear;
        end
      else if ( Assigned( Instances[ i ])) then
        begin
          Instances[ i ].Free;
          Instances[ i ]:= nil;
        end;
    end;

  for i:= 0 to Width * Height - 1 do
    begin
      j:= GridData[ i ];
      if ( Assigned( Meshes[ j ])) then
        Instances[ j ].Add( i );
    end;

  Modifiers.Clear( True );
  for i:= 0 to Instances.Count - 1 do
    if ( Assigned( Instances[ i ])) then
      begin
        Instances[ i ].PushData;
        Modifier:= TP3DRenderListModiferGrid.Create( Self, i );
        Modifiers.Add( Modifier );
      end
    else
      Modifiers.Add( nil );
end;

constructor TP3DTileGrid.Create( const AData: TP3DData );
begin
  inherited Create( AData );
  Meshes:= TP3DDataBlockList.Create;
  GridData:= TByteList.Create;
  GridData.OnSet:= @OnGridDataSet;
  GridWorldUnit:= 1;
  Instances:= TP3DIntBufferGLList.Create;
  Modifiers:= TP3DRenderListModifierList.Create;
  FWidth:= 32;
  Height:= 32;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Width' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Height' ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'GridWorldUnit' ));
  Properties.Add( TP3DPropertyAccessDataBlockList.CreateField( 'Meshes', @FMeshes ));
end;

destructor TP3DTileGrid.Destroy;
begin
  FModifiers.Clear( True );
  FreeAndNil( FModifiers );
  FInstances.Clear( True );
  FreeAndNil( FInstances );
  FreeAndNil( FMeshes );
  FreeAndNil( FGridData );
  inherited Destroy;
end;

procedure TP3DTileGrid.PassToShader(i, baseInstanceCount: Integer);
var
  decl: TP3DShaderDecl;
  instanceID: GLint;
begin
  if ( Assigned( Meshes[ i ]) and Assigned( Instances[ i ])) then
    begin
      instanceID:= P3DShaderActive.Attributes.AddrByName( 'instanceID' );

      decl:= P3DShaderActive.Uniforms.Names[ 'instanceGridWidth' ];
      if ( Assigned( decl )) then
        decl.AsInt:= Width;

      decl:= P3DShaderActive.Uniforms.Names[ 'instanceGridWorldSize' ];
      if ( Assigned( decl )) then
        decl.AsFloat:= GridWorldUnit;

      decl:= P3DShaderActive.Uniforms.Names[ 'world_instance_repeat' ];
      if ( Assigned( decl )) then
        decl.AsFloat:= baseInstanceCount;

      glVertexAttribDivisorARB( P3DShaderActive.Attributes.AddrByName( 'instanceID' ), baseInstanceCount ); cle( Self );

      Instances[ i ].SetAttribArray( instanceID );
    end;
  {for i:= 0 to Meshes.Count - 1 do
    for j:= 0 to Width * Height - 1 do
      if (( GridData[ j ] = i ) and ( Assigned( Meshes[ i ]))) then
        begin
          p:= vec2( j mod Width, j div Width ) * GridWorldUnit;
          Meshes[ i ].Render( world * mat4translate( vec4( p, 0, 1 )), Scene, RenderObject );
        end;}
end;

procedure TP3DTileGrid.OnGridDataSet(Sender: TPersistent; ItemIndex: Integer; AValue: Byte);
var
  OldValue: Byte;
begin
  OldValue:= GridData[ ItemIndex ];
  if ( OldValue = AValue ) then
    exit;
  if ( InRange( OldValue, 0, Instances.Count - 1 )) then
    if ( Assigned( Instances[ OldValue ])) then
      begin
        Instances[ OldValue ].Remove( ItemIndex );
        Instances[ OldValue ].PushData;
      end;
  if ( InRange( AValue, 0, Instances.Count - 1 )) then
    if ( Assigned( Instances[ AValue ])) then
      begin
        Instances[ AValue ].Add( ItemIndex );
        Instances[ AValue ].PushData;
      end;
end;

{ TP3DMaterialModifierGrid }

constructor TP3DRenderListModiferGrid.Create(AGrid: TP3DTileGrid; AGridIndex: Integer);
begin
  inherited Create;
  Grid:= AGrid;
  GridIndex:= AGridIndex;
end;

procedure TP3DRenderListModiferGrid.PassToShader(Obj: TP3DRenderObject);
begin
  if ( Assigned( Grid )) then
    Grid.PassToShader( GridIndex, Obj.Positions.Count );
end;

procedure TP3DRenderListModiferGrid.AddObject(var Obj: TP3DRenderObject);
begin
  Obj.InstanceCount:= Obj.InstanceCount * Grid.Instances[ GridIndex ].Count;
end;


{$ENDIF}
