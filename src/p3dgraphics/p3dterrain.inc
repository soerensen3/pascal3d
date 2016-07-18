//p3dterrain.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

{ TP3DMeshModifierTerrain }

TP3DMeshModifierTerrain = class ( TP3DMeshModifier )
  private
    FNumCells: Integer;
    FTerrainSize: Integer;

  published
    constructor Create( AMesh: TP3DMesh );
    destructor Destroy; override;

    procedure PassToShader( Scene: TP3DScene; Material: TP3DMaterialShader; var world: TMat4 ); override;

    property TerrainSize: Integer read FTerrainSize write FTerrainSize;
    property NumCells: Integer read FNumCells write FNumCells;
    //property MatModifier: TP3DMaterialModifierTerrain
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}


{ TP3DMeshModifierTerrain }

constructor TP3DMeshModifierTerrain.Create(AMesh: TP3DMesh);
var
  i: Integer;
begin
  inherited Create( AMesh );
  TerrainSize:= 4096;
  NumCells:= 64;
  for i:= 0 to Mesh.MaterialGroups.Count -1 do
    if ( Mesh.MaterialGroups[ i ].Material is TP3DMaterialBase ) then
      TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).Modifiers.Add( );
end;

destructor TP3DMeshModifierTerrain.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DMeshModifierTerrain.PassToShader(Scene: TP3DScene;
  Material: TP3DMaterialShader; var world: TMat4);
var
  hcell: Single;
  Position: TVec3;
  //Position: TVec3;
begin
  hcell:= NumCells * 2;
  Position:= vec3( round( Scene.ActiveCam.Position.X / hcell ), round( Scene.ActiveCam.Position.Y / hcell ), 0 );
  world.Row[ 3 ]:= vec4( Position * hcell, 1 );
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'uvX' ), -round( Position.x ));
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'uvY' ), -round( Position.y ));
  //glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'height' ), -0.05 );
  glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'hcell' ), hcell / TerrainSize );
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'cellNumber' ), NumCells );
end;


{$ENDIF}
