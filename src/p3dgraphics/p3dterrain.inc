//p3dterrain.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

{ TP3DMaterialModifierTerrain }

{ TP3DMeshModifierTerrain }

TP3DMeshModifierTerrain = class ( TP3DMeshModifier )
  private
    FCamObject: TP3DActor;
    FNumCells: Integer;
    FTerrainSize: Integer;

  published
    constructor Create;
    destructor Destroy; override;

    procedure PassToShader(Mesh: TP3DMesh; Material: TP3DMaterialShader); override;

    property TerrainSize: Integer read FTerrainSize write FTerrainSize;
    property NumCells: Integer read FNumCells write FNumCells;
    property CamObject: TP3DActor read FCamObject write FCamObject;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}


{ TP3DMeshModifierTerrain }

constructor TP3DMeshModifierTerrain.Create;
begin
  inherited Create;
  TerrainSize:= 4096;
  NumCells:= 64;
end;

destructor TP3DMeshModifierTerrain.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DMeshModifierTerrain.PassToShader(Mesh: TP3DMesh; Material: TP3DMaterialShader);
var
  hcell: Single;
  //Position: TVec3;
begin
  if ( not Assigned( CamObject )) then
    exit;
  hcell:= NumCells * 2;
  Position:= vec3( Int( CamObject.Position.X / hcell ) * hcell, Int( CamObject.Position.Y / hcell ) * hcell, 0 );
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'uvX' ), Round( -CamObject.Position.X / hcell ));
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'uvY' ), Round( -CamObject.Position.Y / hcell ));
  //glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'height' ), -0.05 );
  glUniform1f( P3DShaderActive.Uniforms.AddrByName( 'hcell' ), hcell / TerrainSize );
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'cellNumber' ), NumCells );
end;


{$ENDIF}
