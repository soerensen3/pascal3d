//p3d.core.material.inc
{$IFDEF INTERFACE}
  type
     TP3DMaterial = class;

     { TP3DMaterialMap }

     TP3DMaterialMap = class ( TP3DStreamable )
       protected
         FEnabled: Boolean;
         FHasTransparency: Boolean;
         FMap: TP3DStreamablePointer;
         FParentMaterial: TP3DStreamablePointer;
         FTexChannel: Integer;

         function GetMap: TP3DTexture;
         function GetName: String;
         function GetParentMaterial: TP3DMaterial;
         procedure SetMap( AValue: TP3DTexture );
         procedure SetTexChannel(AValue: Integer); virtual;

       public
         constructor Create( const AOwner: IP3DStreamableContainer = nil ); override;
         destructor Destroy; override;

       published
         property Enabled: Boolean read FEnabled write FEnabled;
         property TexChannel: Integer read FTexChannel write SetTexChannel;
         property Map: TP3DTexture read GetMap write SetMap;
         property ParentMaterial: TP3DMaterial read GetParentMaterial;
         property HasTransparency: Boolean read FHasTransparency;
         property Name: String read GetName;
     end;

     TP3DMaterialSubsetDict = specialize TFPGMap < String, TP3DShader >;

     TP3DMaterialMapClass = class of TP3DMaterialMap;


     {TP3DMaterialMapContainer = specialize gP3DStreamableContainer < TP3DMaterialMap >;
     TP3DMaterialMapPointer = specialize gP3DStreamablePointer < TP3DMaterialMap >;
     //TP3DMaterialMapContainerList = specialize gP3DStreamableContainerList < TP3DMaterialMap, TP3DMaterialMapContainer >;
     TP3DMaterialMapPointerList = specialize gP3DStreamableList < TP3DMaterialMap, TP3DMaterialMapPointer >;
     }
     { TP3DMaterialMapList }

     TP3DMaterialMapContainerList = class( specialize gP3DStreamableContainerList < TP3DMaterialMap >)
       private
         FMapClass: TP3DMaterialMapClass;
         FParentMaterial: TP3DMaterial;

       public
         procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;

         property ParentMaterial: TP3DMaterial read FParentMaterial write FParentMaterial;
         //needs map class
     end;

     TP3DMaterial = class ( TP3DDataBlock, IP3DAnimatable, IP3DPlottableSettings )
       private
         FMaps: TP3DMaterialMapContainerList;
         FAction: TP3DStreamablePointer;

         function GetAction: TP3DAction;

         function GetHasTransparency: Boolean; virtual;
         procedure SetAction(AValue: TP3DAction);
         function FindMap( AMap: TP3DTexture ): Integer;

       public
         class function GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess; override;

         function GetSettings: TP3DPlotListSetting; virtual;

         constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
         destructor Destroy; override;

       published
         property Maps: TP3DMaterialMapContainerList read FMaps write FMaps;
         property HasTransparency: Boolean read GetHasTransparency;
         property Action: TP3DAction read GetAction write SetAction;
     end;

     {TP3DMaterialContainer = specialize gP3DStreamableContainer < TP3DMaterial >;
     TP3DMaterialPointer = specialize gP3DStreamablePointer < TP3DMaterial >;
     TP3DMaterialPointerList = specialize gP3DStreamableList < TP3DMaterial, TP3DMaterialPointer >;
     }
     TP3DMaterialContainerList = specialize gP3DNamedStreamableContainerList < TP3DMaterial >;

     {$INCLUDE p3d.core.material.deprecated.inc}


     { TP3DMaterialPBR }

     TP3DMaterialPBR = class ( TP3DMaterial )
       private
         FAlbedo: TP3DColorRGBA;
         FAlbedoMap: TP3DMaterialMap;
         FMetalness: Float;
         FMetalnessMap: TP3DMaterialMap;
         FNormalMap: TP3DMaterialMap;
         FRoughness: Float;
         FRoughnessMap: TP3DMaterialMap;

       public
         constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
         destructor Destroy; override;

         function GetSettings: TP3DPlotListSetting; override;

         property Albedo: TP3DColorRGBA read FAlbedo write FAlbedo;
         property Roughness: Float read FRoughness write FRoughness;
         property Metalness: Float read FMetalness write FMetalness;
         property AlbedoMap: TP3DMaterialMap read FAlbedoMap;
         property RoughnessMap: TP3DMaterialMap read FRoughnessMap;
         property MetalnessMap: TP3DMaterialMap read FMetalnessMap;
         property NormalMap: TP3DMaterialMap read FNormalMap;
       // Albedo Color4
       // Roughness Float
       // Metalness Float

     end;

  var
    P3DMaterialActive: TP3DMaterial = nil;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$INCLUDE p3d.core.material.deprecated.inc}


{ TP3DMaterialMapShader }

procedure TP3DMaterialMap.SetMap(AValue: TP3DTexture);
var
  S: TP3DNodeSocketClone;
  n: String;
begin
  FMap.AsValue:= AValue;
  {if ( Assigned( ParentMaterial )) then begin
    S:= FindInput( 'Map' );
    if ( S is TP3DShaderNodeSocketClone ) then begin
      n:= IntToStr( ParentMaterial.FindMap( AValue ));
      if ( n <> TP3DShaderNodeSocketClone( S ).ValueProp.AsString ) then
        TP3DShaderNodeSocketClone( S ).ValueProp.AsString:= n;
    end;
  end;}
end;

procedure TP3DMaterialMap.SetTexChannel(AValue: Integer);
begin
  if FTexChannel=AValue then Exit;
  FTexChannel:=AValue;
end;


function TP3DMaterialMap.GetName: String;
begin
  if ( Assigned( Map )) then
    Result:= Map.Name
  else
    Result:= '<None>';
end;

function TP3DMaterialMap.GetParentMaterial: TP3DMaterial;
begin
  Result:= TP3DMaterial( FParentMaterial.AsValue );
end;

function TP3DMaterialMap.GetMap: TP3DTexture;
begin
  Result:= TP3DTexture( FMap.AsValue );
end;

constructor TP3DMaterialMap.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FTexChannel:= 0;
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'TexChannel', smText ));
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Name' ));
  FMap:= TP3DStreamablePointer.Create( 'Map', TP3DTexture, smText );
  FParentMaterial:= TP3DStreamablePointer.Create( 'ParentMaterial', TP3DMaterial );
  FParentMaterial.AsValue:= TP3DStreamable( TP3DPropAccessStreamable( Owner.GetContainer.Context.GetInstance ).Context.GetInstance );

  Properties.Add([ FParentMaterial, FMap ]);
  FEnabled:= True;
end;

destructor TP3DMaterialMap.Destroy;
begin
  if ( Assigned( FMap ) and Assigned( Map )) then
    Map:= nil;
  inherited Destroy;
end;

{ TP3DMaterial }

function TP3DMaterial.FindMap(AMap: TP3DTexture): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Maps.Count - 1 do
    if ( Maps[ i ].Map = AMap ) then begin
      Result:= i;
      break;
    end;
end;


class function TP3DMaterial.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Materials' );
end;

function TP3DMaterial.GetSettings: TP3DPlotListSetting;
begin

end;

function TP3DMaterial.GetAction: TP3DAction;
begin
  Result:= TP3DAction( FAction.AsValue );
end;

function TP3DMaterial.GetHasTransparency: Boolean;
begin
  Result:= False;
end;

procedure TP3DMaterial.SetAction(AValue: TP3DAction);
begin
  FAction.AsValue:= AValue;
end;

constructor TP3DMaterial.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  Maps:= TP3DMaterialMapContainerList.Create( 'Maps', smText );
  Maps.ParentMaterial:= Self;
  Maps.FItemClass:= TP3DMaterialMap;
  Properties.Add( Maps );
end;

destructor TP3DMaterial.Destroy;
begin
  //Owner.Materials.Remove( Self );
  //WriteLn( 'Destroying Material: ' + Name );
  inherited Destroy;
end;


{ TP3DMaterialMapContainerList }

procedure TP3DMaterialMapContainerList.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  inherited Notify( ASender, AItem, AAction );
end;

{ TP3DMaterialPBR }

constructor TP3DMaterialPBR.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Properties.Add([
    TP3DPropAccessColorRGBA.CreateField( 'Albedo', @FAlbedo, smText ),
    TP3DPropAccessFloat.CreateField( 'Roughness', @FRoughness, smText ),
    TP3DPropAccessFloat.CreateField( 'Metalness', @FMetalness, smText )]);
  FNormalMap:= TP3DMaterialMap.Create( Maps );
  FMetalnessMap:= TP3DMaterialMap.Create( Maps );
  FAlbedoMap:= TP3DMaterialMap.Create( Maps );
  FRoughnessMap:= TP3DMaterialMap.Create( Maps );
end;

destructor TP3DMaterialPBR.Destroy;
begin
  inherited Destroy;
end;

function TP3DMaterialPBR.GetSettings: TP3DPlotListSetting;
begin
  if ( AlbedoMap.Enabled and Assigned( AlbedoMap.Map )) then
    Result:= settings( sample( 'albedo', AlbedoMap.Map ))
  else
    Result:= settings( attrib( P3DAttribCo{ TP3DMaterialPBR }

constructor TP3DMaterialPBR.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Properties.Add([
    TP3DPropAccessColorRGBA.CreateField( 'Albedo', @FAlbedo, smText ),
    TP3DPropAccessFloat.CreateField( 'Roughness', @FRoughness, smText ),
    TP3DPropAccessFloat.CreateField( 'Metalness', @FMetalness, smText )]);
  FNormalMap:= TP3DMaterialMap.Create( Maps );
  FMetalnessMap:= TP3DMaterialMap.Create( Maps );
  FAlbedoMap:= TP3DMaterialMap.Create( Maps );
  FRoughnessMap:= TP3DMaterialMap.Create( Maps );
end;

destructor TP3DMaterialPBR.Destroy;
begin
  inherited Destroy;
end;

function TP3DMaterialPBR.GetSettings: TP3DPlotListSetting;
begin
  if ( AlbedoMap.Enabled and Assigned( AlbedoMap.Map )) then
    Result:= settings( sample( 'albedo', AlbedoMap.Map ))
  else
    Result:= settings( attrib( P3DAttribColor, Albedo ));

  if ( NormalMap.Enabled and Assigned( NormalMap.Map )) then
    Result.Append( sample_normal( 'NormalMap', NormalMap.Map ));

  if ( RoughnessMap.Enabled and Assigned( RoughnessMap.Map )) then
    Result.Append( sample_roughness( 'roughness', RoughnessMap.Map ))
  else
    Result.Append( uniform_roughness( 'roughness', Roughness ));

  if ( MetalnessMap.Enabled and Assigned( MetalnessMap.Map )) then
    Result.Append( sample_metalness( 'metalness', MetalnessMap.Map ))
  else
    Result.Append( uniform_metalness( 'metalness', Metalness ));

  Result.Append( TP3DPlotCommandMaterialPBR.Create());
end;



{$ENDIF}

{$IFDEF FINALIZATION}
  P3DMaterialActive:= nil;
{$ENDIF}

