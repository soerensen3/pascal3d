//p3dterrain.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

{ TP3DMaterialModifierTerrain }

TP3DMaterialModifierTerrain = class ( TP3DMaterialModifier )
  private
    FCamObject: TP3DActor;
    FNumCells: Integer;
    FTerrainSize: Integer;

  published
    constructor Create;

    procedure AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase); override;
    procedure PassToShader( Material: TP3DMaterialBase ); override;
    class function GetIndex: Integer; override;

    property TerrainSize: Integer read FTerrainSize write FTerrainSize;
    property NumCells: Integer read FNumCells write FNumCells;
    property CamObject: TP3DActor read FCamObject write FCamObject;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}


{ TP3DMaterialModifierTerrain }

constructor TP3DMaterialModifierTerrain.Create;
begin
  inherited Create;
  TerrainSize:= 4096;
  NumCells:= 64;
end;

procedure TP3DMaterialModifierTerrain.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
begin
  ShaderTree.AddCopy( 'pass_terrainshift' );
end;

procedure TP3DMaterialModifierTerrain.PassToShader(Material: TP3DMaterialBase);
var
  hcell: Single;
  //Position: TVec3;
begin
  if ( not Assigned( CamObject )) then
    exit;
  hcell:= NumCells * 2;
  //Position:= vec3( Int( CamObject.Position.X / hcell ) * hcell, Int( CamObject.Position.Y / hcell ) * hcell, 0 );
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'uvX' ), Round( -CamObject.Position.X / hcell ));
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'uvY' ), Round( -CamObject.Position.Y / hcell ));
  //glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'height' ), -0.05 );
  glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'hcell' ), hcell / TerrainSize );
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'cellNumber' ), NumCells );
end;

class function TP3DMaterialModifierTerrain.GetIndex: Integer;
begin
  Result:= 55;
end;

{$ENDIF}
