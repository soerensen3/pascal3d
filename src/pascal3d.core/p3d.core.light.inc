{$IFDEF INTERFACE}
  { TP3DLight }

  TP3DLightType = ( ltPoint = 0, ltSun = 1, ltSpot = 2 );
  TP3DLight = class ( TP3DDataBlock, IP3DRenderable )
    protected
      FColor: TP3DColorRGB;
      FEnergy: Float;
      FLightType: TP3DLightType;
      FLinearAttenuation: Float;
      FQuadraticAttenuation: Float;

      function GetLightRange: Float;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      class function GetContainer( ALib: TP3DLibrary): TP3DPropertyAccess; override;
      procedure Render( world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject );
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );

      property Color: TP3DColorRGB read FColor write FColor;

    published
      property Energy: Float read FEnergy write FEnergy;
      property LinearAttenuation: Float read FLinearAttenuation write FLinearAttenuation;
      property QuadraticAttenuation: Float read FQuadraticAttenuation write FQuadraticAttenuation;
      property LightRange: Float read GetLightRange;
      property LightType: TP3DLightType read FLightType write FLightType;
  end;

  TP3DLightInformation = ( liLightParams, liPosition );
  TP3DLightInformationSet = set of TP3DLightInformation;

  {TP3DLightContainer = specialize gP3DStreamableContainer < TP3DLight >;
  TP3DLightPointer = specialize gP3DStreamablePointer < TP3DLight >;
  }

  TP3DLightContainerList = specialize gP3DNamedStreamableContainerList < TP3DLight >;
  TP3DLightPointerList = specialize gP3DStreamablePointerList < TP3DLight >;

  { TP3DLightList }

  {TP3DLightPointerList = class ( specialize gP3DStreamableList < TP3DLight, TP3DPropAccessLightPointer > )
    public
      procedure PassToActiveShader( matView: TMat4; Params: TP3DLightInformationSet );
  end;}



{$ENDIF}

{$IFDEF IMPLEMENTATION}

function calculateAttenuation( LightSources: TP3DLightContainerList; i: Integer; dist: Float ): Float;
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

class function TP3DLight.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Lights' );
end;

constructor TP3DLight.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FLinearAttenuation:= 0.1;
  FColor:= vec3( 1 );
  FEnergy:= 1.0;
  //Owner.Lights.Add( Self );
  Properties.Add( TP3DPropAccessColorRGB.CreateField( 'Color', @Color, smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Energy', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'LinearAttenuation', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'QuadraticAttenuation', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'LightType', smText ));
end;

procedure TP3DLight.Render(world: TMat4; Scene: TP3DScene;
  RenderObject: TP3DObject );
begin
  if ( LightType = ltSun ) then
    begin
      world:= mat4translate( vec4( -RenderObject.Transform.Direction * 3, 1 )) * world;
      //RenderLine3D( vec3( 0 ), RenderObject.Direction * 10, vec4( 0, 1, 1, 1 ));
    end
  else
    world:= world * RenderObject.Transform.Matrix;
end;

procedure TP3DLight.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
begin
  if ( rlfLighting in AFlags ) then
    AList.Lights.PushRenderObject( Self, AWorld, nil );
end;

{ TP3DLightList }
{
procedure TP3DLightPointerList.PassToActiveShader( matView: TMat4; Params: TP3DLightInformationSet );
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
//            glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.energy' ), NamedItems[ i ].Energy ); cle( Self );

            //type
            glUniform1i( P3DShaderActive.Uniforms.AddrByName( l + '.type' ), Ord( Items[ i ].LightType )); cle( Self );
          end;

        if ( liPosition in Params ) then
          begin
            //position in viewspace
            obj:= Items[ i ].Users.Items[ j ];
            if ( obj is TP3DObject ) then
              with ( obj as TP3DObject ) do
                begin
                  pos:= matView * vec4( Transform.Position, 1 );
                  glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.position' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
                  pos:= vec4( normalize( mat3( matView ) * {mat3( matWorld ) * }Transform.Matrix.Row[ 2 ].XYZ ), 0 );
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
end;}

{$ENDIF}

