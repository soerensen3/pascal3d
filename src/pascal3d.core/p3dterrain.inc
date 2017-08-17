//p3dterrain.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

{ TP3DMaterialModifierTerrain }

TP3DMaterialModifierTerrain = class ( TP3DMaterialModifier )
  private
    FMeshModifier: TP3DMeshModifier;

  published
    constructor Create( AMeshModifier: TP3DMeshModifier );

    procedure AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase); override;
    procedure PassToShader( Material: TP3DMaterialBase ); override;
    class function GetIndex: Integer; override;

    property MeshModifier: TP3DMeshModifier read FMeshModifier write FMeshModifier;
end;

{ TP3DMeshModifierTerrain }

TP3DMeshModifierTerrain = class ( TP3DMeshModifier )
  private
    FGridSize: Single;
    FUVMultiplier: Float;

  public
    constructor Create( AMesh: TP3DMesh ); override;
    destructor Destroy; override;

    procedure PassToShader( Material: TP3DMaterial; var world: TMat4 ); override;
    procedure LoadFromDOM(ADOMNode: TDOMElement); override;

  published
    property GridSize: Single read FGridSize write FGridSize;
    property UVMultiplier: Float read FUVMultiplier write FUVMultiplier;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}


{ TP3DMeshModifierTerrain }

constructor TP3DMeshModifierTerrain.Create(AMesh: TP3DMesh);
var
  i: Integer;
begin
  inherited Create( AMesh );
  GridSize:= 1.0;
  UVMultiplier:= 1.0;
  for i:= 0 to Mesh.MaterialGroups.Count -1 do
    if ( Mesh.MaterialGroups[ i ].Material is TP3DMaterialBase ) then
      begin
        TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).Modifiers.Add( TP3DMaterialModifierTerrain.Create( Self ));
        TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).BuildShader();
      end;
end;

destructor TP3DMeshModifierTerrain.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DMeshModifierTerrain.PassToShader( Material: TP3DMaterial; var world: TMat4);
var
  Position: TVec3;
  //Position: TVec3;
begin
  {Position:= vec3( round( Scene.ActiveCam.Position.X / GridSize ), round( Scene.ActiveCam.Position.Y / GridSize ), 0 );
  world.Row[ 3 ]:= vec4( Position * GridSize, 1 );
  glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'uvX' ),  Position.x{ * UVMultiplier });
  glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'uvY' ), -Position.y{ * UVMultiplier });
  glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'uvMultiplier' ), UVMultiplier );}
end;

procedure TP3DMeshModifierTerrain.LoadFromDOM(ADOMNode: TDOMElement);
begin
  GridSize:= StrToFloatDef( ADOMNode.GetAttribute( 'gridsize' ), 1 );
  UVMultiplier:= StrToFloatDef( ADOMNode.GetAttribute( 'uvmultiplier' ), 1 );
end;

{ TP3DMaterialModifierTerrain }

constructor TP3DMaterialModifierTerrain.Create(AMeshModifier: TP3DMeshModifier);
begin
  inherited Create;
  MeshModifier:= AMeshModifier;
end;

procedure TP3DMaterialModifierTerrain.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
  function ChangeSocketValueInt( Node: TP3DShaderNode; Name: String; Value: Integer ): TP3DShaderNode;
  var
    Socket: Integer;
  begin
    Socket:= Node.Inputs.FindSocketByName( Name );
    if ( Socket < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    TP3DShaderNodeSocketInt( Node.Inputs[ Socket ]).Value:= Value;
    Result:= Node;
  end;
var
  i: Integer;
begin
  ShaderTree.AddCopy( 'pass_terraininit' );
  for i:= 0 to Material.Maps.Count - 1 do
    ChangeSocketValueInt( ShaderTree.AddCopyEx( 'pass_terrainshift' ), 'MapIndex', i );
end;

procedure TP3DMaterialModifierTerrain.PassToShader(Material: TP3DMaterialBase);
begin
  //Already done somewhere else
end;

class function TP3DMaterialModifierTerrain.GetIndex: Integer;
begin
  Result:= 45;
end;


{$ENDIF}
