{$IFDEF INTERFACE}
  { TP3DLight }

  TP3DLightType = ( ltPoint = 0, ltSun = 1, ltSpot = 2 );
  TP3DLight = class ( TP3DDataBlock )
    private
      FColor: TVec3;
      FEnergy: Float;
      FLightType: TP3DLightType;
      FLinearAttenuation: Float;
      FQuadraticAttenuation: Float;

      function GetLightRange: Float;

    public
      constructor Create( const AData: TP3DLibrary = nil ); override;
      procedure FastDestroy; override;
      class function DOMNodeName: String; override;

      procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor ); override;
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags ); override;

      property Color: TVec3 read FColor write FColor;

    published
      property Energy: Float read FEnergy write FEnergy;
      property LinearAttenuation: Float read FLinearAttenuation write FLinearAttenuation;
      property QuadraticAttenuation: Float read FQuadraticAttenuation write FQuadraticAttenuation;
      property LightRange: Float read GetLightRange;
      property LightType: TP3DLightType read FLightType write FLightType;
  end;

  TP3DCustomLightList = specialize gP3DCustomDatablockList < TP3DLight >;

  TP3DLightInformation = ( liLightParams, liPosition );
  TP3DLightInformationSet = set of TP3DLightInformation;

  { TP3DLightList }

  TP3DLightList = class ( TP3DCustomLightList )
    procedure Delete( Index: Integer ); override;

    procedure PassToActiveShader( matView: TMat4; Params: TP3DLightInformationSet );
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

function calculateAttenuation( LightSources: TP3DLightList; i: Integer; dist: Float ): Float;
begin
  Result:= ( 1.0 / ( max( 0.01, //LightSources[i].constantAttenuation + //useless
                     LightSources[ i ].linearAttenuation * dist +
                     LightSources[ i ].quadraticAttenuation * dist * dist )));
end;

{ TP3DLight }

function TP3DLight.GetLightRange: Float;
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

constructor TP3DLight.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  FLinearAttenuation:= 0.1;
  FColor:= vec3( 1 );
  FEnergy:= 1.0;
  Owner.Lights.Add( Self );
  Properties.Add( TP3DPropertyAccessVec3.CreateField( 'Color', @Color, smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Energy', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'LinearAttenuation', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'QuadraticAttenuation', smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'LightType', smAttribute ));
end;

procedure TP3DLight.FastDestroy;
begin
  Owner.Lights.Remove( Self );

  inherited FastDestroy;
end;

class function TP3DLight.DOMNodeName: String;
begin
  Result:= 'light';
end;

procedure TP3DLight.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DActor);
begin
  if ( LightType = ltSun ) then
    begin
      world:= mat4translate( vec4( -RenderObject.Direction * 3, 1 )) * world;
      //RenderLine3D( vec3( 0 ), RenderObject.Direction * 10, vec4( 0, 1, 1, 1 ));
    end
  else
    world:= world * RenderObject.Matrix;
end;

procedure TP3DLight.LoadFromDOM( ADOMNode: TDOMElement );
var
  AttrS: DOMString;
begin
  inherited LoadFromDOM( ADOMNode );
  case ADOMNode.GetAttribute( 'type' ) of
    'sun':
      LightType:= ltSun;
    'point':
      LightType:= ltPoint;
    'spot':
      LightType:= ltSpot;
    else
      raise Exception.Create( 'Cannot create light from DOM. Either no or an unsupported light type was specified: "' + ADOMNode.GetAttribute( 'type' ) + '"' );
  end;

//  AttrS:= ADOMNode.GetAttribute( 'position' );
//  if ( AttrS > '' ) then
//    Position.FromString( AttrS );
  AttrS:= ADOMNode.GetAttribute( 'color' );
  if ( AttrS > '' ) then
    Color.FromString( AttrS );
//  if ( AttrS > '' ) then
//    Position.FromString( AttrS );
//  AttrS:= ADOMNode.GetAttribute( 'direction' );
//  if ( AttrS > '' ) then
//    Direction.FromString( AttrS );
  AttrS:= ADOMNode.GetAttribute( 'energy' );
  if ( AttrS > '' ) then
    Energy:= StrToFloatDef( AttrS, 0.0, DefaultFormatSettings );
end;

procedure TP3DLight.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
begin
  if ( rlfLighting in AFlags ) then
    AList.Lights.Add( Self );
end;

{ TP3DLightList }

procedure TP3DLightList.Delete(Index: Integer);
begin
  Items[ Index ].Free;
  inherited Delete(Index);
end;


procedure TP3DLightList.PassToActiveShader( matView: TMat4; Params: TP3DLightInformationSet );
var
  numLights: Integer;
  i, j: Integer;
  l: String;
  pos: TVec4;
  obj: TP3DDataBlock;

begin
  if ( not Assigned( P3DShaderActive )) then
    exit;

  numLights:= 0;
  for i:= 0 to Count - 1 do
    for j:= 0 to Items[ i ].Users.Count -1 do
      begin
        l:= 'LightSource[' + IntToStr( numLights ) + ']';

        if ( liLightParams in Params ) then
          begin
            //Color
            glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.color' ), Items[ i ].Color.R, Items[ i ].Color.G, Items[ i ].Color.B, Items[ i ].Energy ); cle( Self );

            //linear Attenuation
            glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.linearAttenuation' ), Items[ i ].LinearAttenuation ); cle( Self );

            //quadratic Attenuation
            glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.quadraticAttenuation' ), Items[ i ].QuadraticAttenuation ); cle( Self );

            //range
            glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.range' ), Items[ i ].GetLightRange ); cle( Self );

            //energy
//            glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.energy' ), Items[ i ].Energy ); cle( Self );

            //type
            glUniform1i( P3DShaderActive.Uniforms.AddrByName( l + '.type' ), Ord( Items[ i ].LightType )); cle( Self );
          end;

        if ( liPosition in Params ) then
          begin
            //position in viewspace
            obj:= Items[ i ].Users[ j ];
            if ( obj is TP3DActor ) then
              with ( obj as TP3DActor ) do
                begin
                  pos:= matView * vec4( Position, 1 );
                  glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.position' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
                  pos:= vec4( normalize( mat3( matView ) * {mat3( matWorld ) * }Matrix.Row[ 2 ].XYZ ), 0 );
                  glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.direction' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
                  pos:= normalize( -pos + vec4( 0, 0, 1, 0 ));
                  glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.halfvector' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
                  //light_vect = light_position - face_center_position
                  //cam_vect = cam_position - face_center_position
                  //halfangle_vect = (light_vect.normal() + cam_vect.normal()).normal()
                end;
          end;
        Inc( numLights );
        //WriteLn( P3DShaderActive.DumpUniforms );
      end;
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'numLightSource'), numLights ); cle( Self );
end;

{$ENDIF}

