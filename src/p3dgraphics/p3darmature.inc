//p3darmature.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

type
  TP3DBone = class;

  { TP3DBoneList }

  TP3DBoneList = class ( specialize gP3DCustomObjectList < TP3DBone > )
    public
      function FindByName( Name: String ): Integer;
  end;

  { TP3DArmature }

  TP3DArmature = class ( TP3DDataBlock )
    private
      FBones: TP3DBoneList;
      //FActions: TActionList;
      FCurrentAction: Integer;
      FCurrentFrame: Integer;
      //FStillFrame: TFrame;
      procedure SetCurrentAction(AValue: Integer);
      procedure SetCurrentFrame(AValue: Integer);

    public
      constructor Create( const AData: TP3DData= nil );
      destructor Destroy; override;

      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor); override;
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;

    published
      //property Actions: TActionList read FActions;
      property CurrentAction: Integer read FCurrentAction write SetCurrentAction;
      property CurrentFrame: Integer read FCurrentFrame write SetCurrentFrame;
      property Bones: TP3DBoneList read FBones;
      //property StillFrame: TFrame read FStillFrame;
  end;

  TP3DArmatureList = specialize gP3DCustomDatablockList < TP3DArmature >;

  { TP3DBone }

  TP3DBone = class( TPersistent )
    private
      FBones: TP3DBoneList;
      FName: String;
      FPosition: TVec3;
      FRotation: TQuaternion;

    public
      constructor Create;
      constructor CreateFromDOM( ADOMNode: TDOMElement );
      destructor Destroy; override;

      procedure LoadFromDOM(ADOMNode: TDOMElement);

      property Rotation: TQuaternion read FRotation write FRotation;
      property Position: TVec3 read FPosition write FPosition;
      property Name: String read FName write FName;
      property Bones: TP3DBoneList read FBones;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DBoneList }

function TP3DBoneList.FindByName(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Self[ i ].Name = Name ) then
      begin
        Result:= i;
        break;
      end;
end;

{ TP3DBone }

constructor TP3DBone.Create;
begin
  inherited Create;
  FBones:= TP3DBoneList.Create;
end;

constructor TP3DBone.CreateFromDOM(ADOMNode: TDOMElement);
begin
  Create;
  LoadFromDOM( ADOMNode );
end;

destructor TP3DBone.Destroy;
begin
  Bones.Clear( True );
  Bones.Free;
  inherited Destroy;
end;

procedure TP3DBone.LoadFromDOM(ADOMNode: TDOMElement);
begin
  Name:= ADOMNode.GetAttribute( 'name' );
  FPosition.FromString( ADOMNode.GetAttribute( 'position' ));
  FRotation.FromString( ADOMNode.GetAttribute( 'quaternion' ));
end;

{ TP3DArmature }

procedure TP3DArmature.SetCurrentAction(AValue: Integer);
begin

end;

procedure TP3DArmature.SetCurrentFrame(AValue: Integer);
begin

end;

constructor TP3DArmature.Create(const AData: TP3DData);
begin
  inherited Create( AData );
  FBones:= TP3DBoneList.Create;
end;

destructor TP3DArmature.Destroy;
begin
  FBones.Clear( True );
  FBones.Free;
  inherited Destroy;
end;

procedure TP3DArmature.Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor);
begin
  inherited Render(world, Scene, RenderObject);
end;

procedure TP3DArmature.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
begin
  Name:= ADOMNode.GetAttribute( 'name' );

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'bone': Bones.Add( TP3DBone.CreateFromDOM( Element ));
      else
        raise Exception.Create( 'Unknown tag inside Armature element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
end;
{$ENDIF}

