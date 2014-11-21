unit p3dlighting;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, p3dMath, Math, p3dshaders, dglOpenGL;

type

  { TLight }

  TLight = class
    private
      FDiffuse: TVec4;
      FHalfVector: TVec3;
      FLinearAttenuation: Float;
      FPosition: TVec3;
      FQuadraticAttenuation: Float;
      FSpecular: TVec4;

      function GetLightRange: Float;

    public
      constructor Create;

      property Position: TVec3 read FPosition write FPosition;
      property Diffuse: TVec4 read FDiffuse write FDiffuse;
      property Specular: TVec4 read FSpecular write FSpecular;
      property HalfVector: TVec3 read FHalfVector write FHalfVector;
      property LinearAttenuation: Float read FLinearAttenuation write FLinearAttenuation;
      property QuadraticAttenuation: Float read FQuadraticAttenuation write FQuadraticAttenuation;
      property LightRange: Float read GetLightRange;
  end;

  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomLightList}
  {$DEFINE TCustomItem:= TLight}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TLightList }

  TLightInformation = ( liLightParams, liPosition );
  TLightInformationSet = set of TLightInformation;
  TLightList = class ( TCustomLightList )
    procedure Delete( Index: Integer ); override;
    procedure Clear; override;

    procedure PassToActiveShader( Position: TVec3; MaxLights: Integer; matWorldView: TMat4; Params: TLightInformationSet );
  end;

  function calculateAttenuation( LightSources: TLightList; i: Integer; dist: Float ): Float;

implementation

function calculateAttenuation( LightSources: TLightList; i: Integer; dist: Float ): Float;
begin
  Result:= ( 1.0 / ( max( 0.01, //LightSources[i].constantAttenuation + //useless
                     LightSources[ i ].linearAttenuation * dist +
                     LightSources[ i ].quadraticAttenuation * dist * dist )));
end;
{ TLight }

function TLight.GetLightRange: Float;
const approximation = 0.01;
begin
  if ( QuadraticAttenuation > 0 ) then
    Result:= sqrt( sqr( LinearAttenuation ) + 4/0.01 * QuadraticAttenuation ) + LinearAttenuation
             /
             ( 2 * QuadraticAttenuation )
  else if ( LinearAttenuation > 0 ) then
    Result:= 1 / approximation / LinearAttenuation
  else
    Result:= 0;
end;

constructor TLight.Create;
begin
  inherited;
  FLinearAttenuation:= 1;
end;

{ TLightList }

procedure TLightList.Delete(Index: Integer);
begin
  Items[ Index ].Free;
  inherited Delete(Index);
end;

procedure TLightList.Clear;
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    Items[ i ].Free;
  inherited Clear;
end;

procedure TLightList.PassToActiveShader(Position: TVec3; MaxLights: Integer;
  matWorldView: TMat4; Params: TLightInformationSet);
var
  numLights: Integer;
  i: Integer;
  l: String;
  pos: TVec4;
begin
  if ( not Assigned( ActShad )) then
    exit;
  numLights:= Count;
  glUniform1i( ActShad.Uniforms.AddrByName( 'numLightSource'), numLights );

  for i:= 0 to numLights - 1 do
    begin
      l:= 'LightSource[' + IntToStr( i ) + ']';

      if ( liLightParams in Params ) then
        begin
          //diffuse
          glUniform4f( ActShad.Uniforms.AddrByName( l + '.diffuse' ), Items[ i ].Diffuse.R, Items[ i ].Diffuse.G, Items[ i ].Diffuse.B, Items[ i ].Diffuse.A );
          //specular
//          ShaderSetParameter4f( ActiveShader, l + '.specular', Items[ i ].Specular );
          glUniform4f( ActShad.Uniforms.AddrByName( l + '.specular' ), Items[ i ].Specular.R, Items[ i ].Specular.G, Items[ i ].Specular.B, Items[ i ].Specular.A );

          //linear Attenuation
          glUniform1f( ActShad.Uniforms.AddrByName( l + '.linearAttenuation' ), Items[ i ].LinearAttenuation );

//          ShaderSetParameterf( ActiveShader, l + '.linearAttenuation', Items[ i ].LinearAttenuation );
          //quadratic Attenuation
          glUniform1f( ActShad.Uniforms.AddrByName( l + '.quadraticAttenuation' ), Items[ i ].QuadraticAttenuation );
//          ShaderSetParameterf( ActiveShader, l + '.quadraticAttenuation', Items[ i ].QuadraticAttenuation );
          //range
          glUniform1f( ActShad.Uniforms.AddrByName( l + '.range' ), Items[ i ].GetLightRange );
//          ShaderSetParameterf( ActiveShader, l + '.range', Items[ i ].GetLightRange );
        end;

      if ( liPosition in Params ) then
        begin
          //position in viewspace
          pos:= vec4( Items[ i ].Position, 1 ) * matWorldView;
          glUniform4f( ActShad.Uniforms.AddrByName( l + '.position' ), pos.X, pos.Y, pos.Z, pos.W );
          //halfVec not yet!
        end;
    end;
end;

{$DEFINE TCustomList:= TCustomLightList}
{$DEFINE TCustomItem:= TLight}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

end.

