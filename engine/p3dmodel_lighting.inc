{$IFDEF INTERFACE}
  { TP3DLight }

  TP3DLightType = ( p3dltPoint = 0, p3dltSun = 1, p3dltSpot = 2 );
  TP3DLight = class ( TP3DDataBlock )
    private
      FColor: TVec3;
//      FDirection: TVec3;
      FEnergy: Float;
      FHalfVector: TVec3;
      FLightType: TP3DLightType;
      FLinearAttenuation: Float;
//      FPosition: TVec3;
      FQuadraticAttenuation: Float;

      function GetLightRange: Float;

    public
      constructor Create( AParentList: TP3DObjectList );
      constructor CreateFromDOM( AParentList: TP3DObjectList; Scene: TP3DModelScene; ADOMNode: TDOMElement );

      procedure Render( world: TMat4; Scene: TP3DModelScene; RenderObject: TP3DRenderableObject ); override;
      procedure LoadFromDOM( Scene: TP3DModelScene; ADOMNode: TDOMElement ); override;

      //property Position: TVec3 read FPosition write FPosition;
      //property Direction: TVec3 read FDirection write FDirection;
      property Color: TVec3 read FColor write FColor;
      property Energy: Float read FEnergy write FEnergy;
      //property HalfVector: TVec3 read FHalfVector write FHalfVector;
      property LinearAttenuation: Float read FLinearAttenuation write FLinearAttenuation;
      property QuadraticAttenuation: Float read FQuadraticAttenuation write FQuadraticAttenuation;
      property LightRange: Float read GetLightRange;
      property LightType: TP3DLightType read FLightType write FLightType;
  end;

  TP3DCustomLightList = specialize gP3DCustomObjectList < TP3DLight >;

  TP3DLightInformation = ( liLightParams, liPosition );
  TP3DLightInformationSet = set of TP3DLightInformation;

  { TP3DLightList }

  TP3DLightList = class ( TP3DCustomLightList )
    procedure Delete( Index: Integer ); override;

    procedure PassToActiveShader( MaxLights: Integer; Scene: TP3DModelScene; matWorld, matView: TMat4; Params: TP3DLightInformationSet );
  end;

  function calculateAttenuation( LightSources: TP3DLightList; i: Integer; dist: Float ): Float;

{$ENDIF}

{$IFDEF IMPLEMENTATION}


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

constructor TP3DLight.Create(AParentList: TP3DObjectList);
begin
  inherited Create( AParentList );
  FLinearAttenuation:= 0.1;
end;

constructor TP3DLight.CreateFromDOM( AParentList: TP3DObjectList;
  Scene: TP3DModelScene; ADOMNode: TDOMElement );
begin
  Create( AParentList );
  LoadFromDOM( Scene, ADOMNode );
end;

procedure TP3DLight.Render(world: TMat4; Scene: TP3DModelScene;
  RenderObject: TP3DRenderableObject);
begin
  if ( LightType = p3dltSun ) then
    begin
      world:= mat4translate( vec4( -RenderObject.Direction * 3, 1 )) * world;
      RenderLine3D( vec3( 0 ), RenderObject.Direction * 10, vec4( 0, 1, 1, 1 ));
    end
  else
    world:= RenderObject.Matrix * world;
end;

procedure TP3DLight.LoadFromDOM(Scene: TP3DModelScene; ADOMNode: TDOMElement);
var
  AttrS: DOMString;
begin
  Name:= ADOMNode.GetAttribute( 'name' );

  case ADOMNode.GetAttribute( 'type' ) of
    'sun':
      LightType:= p3dltSun;
    'point':
      LightType:= p3dltPoint;
    'spot':
      LightType:= p3dltSpot;
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
    Energy:= StrToFloatDef( AttrS, 0.0 );

  Scene.Lights.Add( Self );
  FileName:= '';
end;

{ TP3DLightList }

procedure TP3DLightList.Delete(Index: Integer);
begin
  Items[ Index ].Free;
  inherited Delete(Index);
end;


procedure TP3DLightList.PassToActiveShader(MaxLights: Integer;
  Scene: TP3DModelScene; matWorld, matView: TMat4;
  Params: TP3DLightInformationSet);
var
  numLights: Integer;
  i: Integer;
  l: String;
  pos: TVec4;
  obj: TP3DRenderableObject;
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
          //Color
          glUniform4f( ActShad.Uniforms.AddrByName( l + '.color' ), Items[ i ].Color.R, Items[ i ].Color.G, Items[ i ].Color.B, 1 );

          //linear Attenuation
          glUniform1f( ActShad.Uniforms.AddrByName( l + '.linearAttenuation' ), Items[ i ].LinearAttenuation );

          //quadratic Attenuation
          glUniform1f( ActShad.Uniforms.AddrByName( l + '.quadraticAttenuation' ), Items[ i ].QuadraticAttenuation );

          //range
          glUniform1f( ActShad.Uniforms.AddrByName( l + '.range' ), Items[ i ].GetLightRange );

          //energy
          glUniform1f( ActShad.Uniforms.AddrByName( l + '.energy' ), Items[ i ].Energy );

          //type
          glUniform1i( ActShad.Uniforms.AddrByName( l + '.type' ), Ord( Items[ i ].LightType ));
        end;

      if ( liPosition in Params ) then
        begin
          //position in viewspace
          obj:= TP3DRenderableObject( Scene.Objects[ Scene.Objects.FindByName( Items[ i ].Name )]);  //TODO: replace buggy PassToActiveShader
          with ( obj ) do
            begin
              pos:= matWorld * matView * vec4( Position, 1 );
              glUniform4f( ActShad.Uniforms.AddrByName( l + '.position' ), pos.X, pos.Y, pos.Z, pos.W );
              pos:= normalize( matWorld * matView * vec4( Direction, 0 ));
              glUniform4f( ActShad.Uniforms.AddrByName( l + '.direction' ), pos.X, pos.Y, pos.Z, pos.W );
              pos:= normalize( -pos + vec4( 0, 0, 1, 0 ));
              glUniform4f( ActShad.Uniforms.AddrByName( l + '.halfvector' ), pos.X, pos.Y, pos.Z, pos.W );
              //light_vect = light_position - face_center_position
              //cam_vect = cam_position - face_center_position
              //halfangle_vect = (light_vect.normal() + cam_vect.normal()).normal()
            end;
        end;
    end;
end;

{$ENDIF}

