//p3darmature.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

type
  TP3DJoint = class;

  { TP3DJointList }

  TP3DJointList = class ( specialize gP3DCustomObjectList < TP3DJoint > )
    private
      FArmature: TP3DArmature;

    public
      constructor Create( AArmature: TP3DArmature );

      function FindByName( Name: String ): Integer;
      procedure SaveToDOM(AParent: TDOMElement);
      procedure LoadFromDOM( ADOMNode: TDOMElement );

      property Armature: TP3DArmature read FArmature write FArmature;
  end;


  TP3DArmatureAction = class;

  { TP3DPoseJoint }

  TP3DPoseJoint = class( TP3DInterfacedPersistent )
    private
      FAction: TP3DArmatureAction;
      FFrameIndex: Integer;
      FName: String;
      PosIdx: Integer;
      RotIdx: Integer;
      function GetRotation: TQuaternion;
      procedure SetRotation(AValue: TQuaternion);
      function GetPosition: TVec3;
      procedure SetPosition(AValue: TVec3);

    public
      constructor Create( AAction: TP3DArmatureAction; AFrameIndex: Integer );
      constructor CreateFromDOM( ADOMNode: TDOMElement; AAction: TP3DArmatureAction; AFrameIndex: Integer );

      procedure LoadFromDOM(ADOMNode: TDOMElement);
      class function DOMNodeName: String; override;

      property Rotation: TQuaternion read GetRotation write SetRotation;
      property Position: TVec3 read GetPosition write SetPosition;

    published
      property Action: TP3DArmatureAction read FAction write FAction;
      property Name: String read FName write FName;
      property FrameIndex: Integer read FFrameIndex;
  end;

  TP3DPoseJointList = class;

  { TP3DActionFrame }

  TP3DActionFrame = class( TP3DInterfacedPersistent )
    private
      FAction: TP3DArmatureAction;
      FIndex: Integer;
      FPose: TP3DPoseJointList;
      FRotations: TQuaternionList;
      FTranslations: TVec3List;

      procedure LoadPoseJointFromDOM( ADOMNode: TDOMElement );

    public
      constructor Create( AAction: TP3DArmatureAction );
      constructor CreateFromDOM( ADOMNode: TDOMElement; AAction: TP3DArmatureAction );
      destructor Destroy; override;

      procedure LoadFromDOM( ADOMNode: TDOMElement );
      function SaveToDOM(AParent: TDOMElement): TDOMElement; override;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      class function DOMNodeName: String; override;

    published
      property Translations: TVec3List read FTranslations write FTranslations;
      property Rotations: TQuaternionList read FRotations write FRotations;
      property Pose: TP3DPoseJointList read FPose write FPose;
      property Action: TP3DArmatureAction read FAction write FAction;
      property Index: Integer read FIndex write FIndex;
  end;

  { TP3DPoseJointList }

  TP3DPoseJointList = class ( specialize gP3DCustomObjectList < TP3DPoseJoint > )
    private
      FFrame: TP3DActionFrame;

    public
      constructor Create( AFrame: TP3DActionFrame );

      function FindByName( Name: String ): Integer;
      procedure SaveToDOM(AParent: TDOMElement);
      procedure LoadFromDOM( ADOMNode: TDOMElement );

      property Frame: TP3DActionFrame read FFrame write FFrame;
  end;

  { TP3DActionFrameList }

  TP3DActionFrameList = class ( specialize gP3DCustomObjectList < TP3DActionFrame >)
    private
      FAction: TP3DArmatureAction;

    public
      constructor Create( AAction: TP3DArmatureAction );

      procedure SaveToDOM( AParent: TDOMElement );
      procedure LoadFromDOM( ADOMNode: TDOMElement );
      property Action: TP3DArmatureAction read FAction write FAction;
  end;

  { TP3DArmatureAction }

  TP3DArmatureAction = class ( TP3DDataBlock )
    private
      FCurrentFrame: Float;
      FFrames: TP3DActionFrameList;
      FFrameSample: TP3DActionFrame;
      FJointTex: TP3DTexture;
      OwnsJointTex: Boolean;

      function GetJointTex: TP3DTexture;
      procedure SetJointTex(AValue: TP3DTexture);

    public
      procedure UpdateJointTex;

      constructor Create(const AData: TP3DLibrary=nil); override;
      procedure FastDestroy; override;
      procedure ClearFields; override;

      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      function SaveToDOM(AParent: TDOMElement): TDOMElement; override;
      procedure UpdateFrameSample;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      class function DOMNodeName: String; override;

    published
      property Frames: TP3DActionFrameList read FFrames write FFrames;
      property FrameSample: TP3DActionFrame read FFrameSample write FFrameSample;
      property CurrentFrame: Float read FCurrentFrame write FCurrentFrame;
      property JointTex: TP3DTexture read GetJointTex write SetJointTex;
  end;

  TP3DArmatureActionList = specialize gP3DCustomDatablockList < TP3DArmatureAction >;
  TP3DPropertyAccessArmatureAction = specialize gP3DPropertyAccessDataBlock < TP3DArmatureAction >;

  { TP3DArmature }

  TP3DArmature = class ( TP3DDataBlock )
    private
      FJoints: TP3DJointList;
      //FActions: TActionList;
      FCurrentAction: TP3DArmatureAction;
      FCurrentFrame: Float;
      FRoot: TP3DJointList;
      FRotations: TQuaternionList;
      FTranslations: TVec3List;
      //FStillFrame: TFrame;
      procedure SetCurrentAction(AValue: TP3DArmatureAction);
      procedure SetCurrentFrame(AValue: Float);

    public
      procedure ClearFields; override;

      constructor Create( const AData: TP3DLibrary= nil ); override;
      procedure FastDestroy; override;

      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor); override;
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      function SaveToDOM(AParent: TDOMElement): TDOMElement; override;
      class function DOMNodeName: String; override;

    published
      //property Actions: TActionList read FActions;
      property CurrentAction: TP3DArmatureAction read FCurrentAction write SetCurrentAction;
      property CurrentFrame: Float read FCurrentFrame write SetCurrentFrame;
      property Joints: TP3DJointList read FJoints;
      property Root: TP3DJointList read FRoot;
      property Translations: TVec3List read FTranslations write FTranslations;
      property Rotations: TQuaternionList read FRotations write FRotations;
      //property StillFrame: TFrame read FStillFrame;
  end;

  TP3DArmatureList = specialize gP3DCustomDatablockList < TP3DArmature >;
  TP3DPropertyAccessArmature = specialize gP3DPropertyAccessDataBlock < TP3DArmature >;

  { TP3DJoint }

  TP3DJoint = class( TP3DInterfacedPersistent )
    private
      FArmature: TP3DArmature;
      FJoints: TP3DJointList;
      FName: String;
      PosIdx: Integer;
      RotIdx: Integer;
      function GetRotation: TQuaternion;
      procedure SetRotation(AValue: TQuaternion);
      function GetPosition: TVec3;
      procedure SetPosition(AValue: TVec3);

    public
      constructor Create( AArmature: TP3DArmature );
      constructor CreateFromDOM( ADOMNode: TDOMElement; AArmature: TP3DArmature );
      function SaveToDOM(AParent: TDOMElement): TDOMElement; override;
      destructor Destroy; override;

      procedure LoadFromDOM(ADOMNode: TDOMElement);
      class function DOMNodeName: String; override;

      property Rotation: TQuaternion read GetRotation write SetRotation;
      property Position: TVec3 read GetPosition write SetPosition;
      property Joints: TP3DJointList read FJoints;
      property Armature: TP3DArmature read FArmature write FArmature;

    published
      property Name: String read FName write FName;
  end;

  { TP3DMaterialModifierArmature }

  TP3DMaterialModifierArmature = class ( TP3DMaterialModifier )
    private
      FMeshModifier: TP3DMeshModifier;

    published
      constructor Create( AMeshModifier: TP3DMeshModifier );

      procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
      procedure PassToShader( Material: TP3DMaterialBase ); override;
      class function GetIndex: Integer; override;

      property MeshModifier: TP3DMeshModifier read FMeshModifier write FMeshModifier;
  end;


  { TP3DMeshModifierArmature }

  TP3DMeshModifierArmature = class ( TP3DMeshModifier )
    private
      FArmature: TP3DArmature;

    public
      constructor Create( AMesh: TP3DMesh ); override;
      destructor Destroy; override;

      procedure PassToShader( Material: TP3DMaterial; var world: TMat4 ); override;
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;

    published
      property Armature: TP3DArmature read FArmature write FArmature;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DJointList }

constructor TP3DJointList.Create(AArmature: TP3DArmature);
begin
  inherited Create;
  Armature:= AArmature;
end;

function TP3DJointList.FindByName(Name: String): Integer;
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

procedure TP3DJointList.SaveToDOM(AParent: TDOMElement);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure TP3DJointList.LoadFromDOM(ADOMNode: TDOMElement);
var
  lst: TDOMNodeList;
  i: Integer;
begin
  Clear();
  lst:= ADOMNode.GetElementsByTagName( TP3DJoint.DOMNodeName );
  try
    Count:= lst.Count;
    for i:= 0 to lst.Count - 1 do
      begin
        Items[ i ]:= TP3DJoint.Create( Armature );
        Items[ i ].LoadFromDOMNew( TDOMElement( lst[ i ]));
      end;

  finally
    lst.Free;
  end;
end;

{ TP3DPoseJointList }

constructor TP3DPoseJointList.Create(AFrame: TP3DActionFrame);
begin
  inherited Create;
  Frame:= AFrame;
end;

function TP3DPoseJointList.FindByName(Name: String): Integer;
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

procedure TP3DPoseJointList.SaveToDOM(AParent: TDOMElement);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure TP3DPoseJointList.LoadFromDOM(ADOMNode: TDOMElement);
var
  lst: TDOMNodeList;
  i: Integer;
begin
  Clear();
  lst:= ADOMNode.GetElementsByTagName( TP3DPoseJoint.DOMNodeName );
  try
    Count:= lst.Count;
    for i:= 0 to lst.Count - 1 do
      begin
        Items[ i ]:= TP3DPoseJoint.Create( Frame.Action, Frame.Index );
        Items[ i ].LoadFromDOMNew( TDOMElement( lst[ i ]));
      end;

  finally
    lst.Free;
  end;
end;


{ TP3DPoseJoint }

function TP3DPoseJoint.GetRotation: TQuaternion;
begin
  Result:= Action.Frames[ FrameIndex ].Rotations[ RotIdx ];
end;

procedure TP3DPoseJoint.SetRotation(AValue: TQuaternion);
begin
  Action.Frames[ FrameIndex ].Rotations[ RotIdx ]:= AValue;
end;

function TP3DPoseJoint.GetPosition: TVec3;
begin
  Result:= Action.Frames[ FrameIndex ].Translations[ PosIdx ];
end;

procedure TP3DPoseJoint.SetPosition(AValue: TVec3);
begin
  Action.Frames[ FrameIndex ].Translations[ PosIdx ]:= AValue;
end;

constructor TP3DPoseJoint.Create(AAction: TP3DArmatureAction; AFrameIndex: Integer);
begin
  inherited Create;
  FFrameIndex:= AFrameIndex;
  Action:= AAction;
  RotIdx:= Action.Frames[ FrameIndex ].Rotations.Add( quat( 0, 0, 0, 1 ));
  PosIdx:= Action.Frames[ FrameIndex ].Translations.Add( vec3( 0 ));
  Properties.Context:= Action;
  Properties.Add( TP3DPropertyAccessQuat.CreateGetSet( 'Rotation', @GetRotation, @SetRotation, smAttribute ));
  Properties.Add( TP3DPropertyAccessVec3.CreateGetSet( 'Position', @GetPosition, @SetPosition, smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Name', smAttribute ));
end;

constructor TP3DPoseJoint.CreateFromDOM(ADOMNode: TDOMElement; AAction: TP3DArmatureAction; AFrameIndex: Integer);
begin
  Create( AAction, AFrameIndex );
  LoadFromDOM( ADOMNode );
end;

procedure TP3DPoseJoint.LoadFromDOM(ADOMNode: TDOMElement);
var
  Rot: TQuaternion;
  Pos: TVec3;
begin
  Name:= ADOMNode.GetAttribute( 'name' );
  Pos.FromString( ADOMNode.GetAttribute( 'position' ));;
  Position:= Pos;
  Rot.FromString( ADOMNode.GetAttribute( 'quaternion' ));
  Rotation:= Rot;
end;

class function TP3DPoseJoint.DOMNodeName: String;
begin
  Result:= 'pose';
end;

{ TP3DJoint }

function TP3DJoint.GetRotation: TQuaternion;
begin
  Result:= Armature.Rotations[ RotIdx ];
end;

procedure TP3DJoint.SetRotation(AValue: TQuaternion);
begin
  Armature.Rotations[ RotIdx ]:= AValue;
end;

function TP3DJoint.GetPosition: TVec3;
begin
  Result:= Armature.Translations[ PosIdx ];
end;

procedure TP3DJoint.SetPosition(AValue: TVec3);
begin
  Armature.Translations[ PosIdx ]:= AValue;
end;

constructor TP3DJoint.Create(AArmature: TP3DArmature);
begin
  inherited Create;
  Armature:= AArmature;
  Armature.Joints.Add( Self );
  FJoints:= TP3DJointList.Create( AArmature );
  RotIdx:= Armature.Rotations.Add( quat( 0, 0, 0, 1 ));
  PosIdx:= Armature.Translations.Add( vec3( 0 ));
  Properties.Add( TP3DPropertyAccessQuat.CreateGetSet( 'Rotation', @GetRotation, @SetRotation, smAttribute ));
  Properties.Add( TP3DPropertyAccessVec3.CreateGetSet( 'Position', @GetPosition, @SetPosition, smAttribute ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Name', smAttribute ));

end;

constructor TP3DJoint.CreateFromDOM(ADOMNode: TDOMElement; AArmature: TP3DArmature);
begin
  Create( AArmature );
  LoadFromDOM( ADOMNode );
end;

function TP3DJoint.SaveToDOM( AParent: TDOMElement ): TDOMElement;
begin
  Result:= inherited SaveToDOM(AParent);
  Joints.SaveToDOM( Result );
end;

destructor TP3DJoint.Destroy;
begin
  Joints.Clear( True );
  Joints.Free;
  inherited Destroy;
end;

procedure TP3DJoint.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  Rot: TQuaternion;
  Pos: TVec3;
begin
  Name:= ADOMNode.GetAttribute( 'name' );
  Pos.FromString( ADOMNode.GetAttribute( 'position' ));;
  Position:= Pos;
  Rot.FromString( ADOMNode.GetAttribute( 'quaternion' ));
  Rotation:= Rot;

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'joint': Joints.Add( TP3DJoint.CreateFromDOM( Element, Armature ));
      else
        raise Exception.Create( 'Unknown tag inside joint element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
end;

class function TP3DJoint.DOMNodeName: String;
begin
  Result:= 'joint';
end;

{ TP3DArmature }

procedure TP3DArmature.SetCurrentAction(AValue: TP3DArmatureAction);
begin
  if ( AValue = FCurrentAction ) then
    exit;

  if ( Assigned( CurrentAction )) then
    CurrentAction.RemoveUser( Self );

  FCurrentAction:= AValue;

  if ( Assigned( CurrentAction )) then
    CurrentAction.AddUser( Self );
end;

procedure TP3DArmature.SetCurrentFrame(AValue: Float);
begin
  FCurrentFrame:= AValue;
end;

procedure TP3DArmature.ClearFields;
begin
  CurrentAction:= nil;
  inherited ClearFields;
end;

constructor TP3DArmature.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  FJoints:= TP3DJointList.Create( Self );
  FRoot:= TP3DJointList.Create( Self );
  FRotations:= TQuaternionList.Create;
  FTranslations:= TVec3List.Create;
  Owner.Armatures.Add( Self );
  Properties.Add( TP3DPropertyAccessArmatureAction.CreateFieldSet( 'CurrentAction', @FCurrentAction, @SetCurrentAction, smAttribute ));
end;

procedure TP3DArmature.FastDestroy;
begin
  FRotations.Free;
  FTranslations.Free;
  FRoot.Clear( False );
  FRoot.Free;
  FJoints.Clear( True );
  FJoints.Free;
  Owner.Armatures.Remove( Self );

  inherited FastDestroy;
end;

procedure TP3DArmature.Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor);
begin
  inherited Render(world, Scene, RenderObject);
end;

procedure TP3DArmature.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  Action: DOMString;
  ActIdx: Integer;
begin
  inherited LoadFromDOM( ADOMNode );
  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'joint': Root.Add( TP3DJoint.CreateFromDOM( Element, Self ));
      else
        raise Exception.Create( 'Unknown tag inside Armature element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  Action:= ADOMNode.GetAttribute( 'action' );
  if ( Action > '' ) then
    begin
      ActIdx:= Owner.AppendFromPath( Action, TP3DArmatureAction );
      CurrentAction:= TP3DArmatureAction( Owner.DataBlocks[ ActIdx ]);
    end
  else
    CurrentAction:= nil;
end;

procedure TP3DArmature.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew(ADOMNode);
  Root.LoadFromDOM( ADOMNode );
end;

function TP3DArmature.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM(AParent);
  Root.SaveToDOM( Result );
end;

class function TP3DArmature.DOMNodeName: String;
begin
  Result:= 'armature';
end;

{ TP3DMeshModifierArmature }

constructor TP3DMeshModifierArmature.Create( AMesh: TP3DMesh );
var
  i: Integer;
begin
  inherited Create( AMesh );

  Properties.Add( TP3DPropertyAccessArmature.CreateField( 'Armature', @FArmature, smAttribute ));
end;

destructor TP3DMeshModifierArmature.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DMeshModifierArmature.PassToShader( Material: TP3DMaterial; var world: TMat4 );
  function RealMod (x, y : extended) : extended;
  begin
     Result := x - y * Trunc(x/y);
  end;
var
  decl: TP3DShaderDecl;
  joint: TP3DJoint;
  poslist: TVec3List;
  quatList: TQuaternionList;
  i: Integer;
  quat: TQuaternion;
begin
  if ( Assigned( Armature ) and Assigned( Armature.CurrentAction )) then
    begin
      {Armature.CurrentAction.UpdateFrameSample;
      decl:= Material.Shader.Uniforms.Names[ 'jointPos[0]' ];
      if ( Assigned( decl )) then
        begin
          {poslist:= TVec3List.Create;
          for joint in Armature.Joints do
            poslist.Add( joint.Position );
          decl.SetAsVec3List( poslist );
          poslist.Free;}
          decl.AsVec3List:= Armature.CurrentAction.FrameSample.Translations;
        end;
      //if ( Assigned( decl )) then
      //  decl.SetAsVec3List( Armature.CurrentAction.FrameSample.Translations );
      decl:= Material.Shader.Uniforms.Names[ 'jointQuat[0]' ];
      if ( Assigned( decl )) then
        decl.AsQuatList:= Armature.CurrentAction.FrameSample.Rotations;
      {for i:= 0 to 5 do //Armature.CurrentAction.FrameSample.Rotations.Count - 1 do
        begin
          glGetUniformfv( Material.Shader.ProgramObj, glGetUniformLocation( Material.Shader.ProgramObj, PChar( 'jointPos[' + IntToStr( i ) + ']' )), @quat );
          WriteLn( quat.xyz.ToString() );
        end;}
      //WriteLn( Material.Shader.DumpUniforms );
      //quatList:= decl.AsQuatList;
      //for i:= 0 to quatList.Count - 1 do
      //  WriteLn( quatList[ i ].ToString());
      //quatList.Free;}
      i:= Material.Maps.Count;
      glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
      glBindTexture( GL_TEXTURE_2D, Armature.CurrentAction.JointTex.GLTexture ); cle( Self );

      decl:= Material.Shader.Uniforms.Names[ 'jointTex' ];
      if ( Assigned( decl )) then
        decl.AsInt:= i;
      decl:= Material.Shader.Uniforms.Names[ 'jointTexWidth' ];
      if ( Assigned( decl )) then
        decl.AsInt:= Armature.CurrentAction.JointTex.Width div 2;
      decl:= Material.Shader.Uniforms.Names[ 'jointTexHeight' ];
      if ( Assigned( decl )) then
        decl.AsInt:= Armature.CurrentAction.JointTex.Height;

      Armature.CurrentAction.CurrentFrame:= SDL_GetTicks() / 100;
      decl:= Material.Shader.Uniforms.Names[ 'time' ];
      if ( Assigned( decl )) then
        decl.AsFloat:= frac( Armature.CurrentAction.CurrentFrame / Armature.CurrentAction.Frames.Count );
    end;
end;

procedure TP3DMeshModifierArmature.LoadFromDOM(ADOMNode: TDOMElement);
var
  data: DOMString;
  i: Integer;
begin
  data:= ADOMNode.GetAttribute( 'data' );
  Armature:= TP3DArmature( Mesh.Owner.DataBlocks[ Mesh.Owner.AppendFromPath( data, TP3DArmature )]);
  for i:= 0 to Mesh.MaterialGroups.Count -1 do
    if ( Mesh.MaterialGroups[ i ].Material is TP3DMaterialBase ) then
      begin
        TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).Modifiers.Add( TP3DMaterialModifierArmature.Create( Self ));
        TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).BuildShader();
      end;
end;

procedure TP3DMeshModifierArmature.LoadFromDOMNew(ADOMNode: TDOMElement);
var
  i: Integer;
begin
  inherited LoadFromDOMNew( ADOMNode );
  for i:= 0 to Mesh.MaterialGroups.Count -1 do
    if ( Mesh.MaterialGroups[ i ].Material is TP3DMaterialBase ) then
      begin
        TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).Modifiers.Add( TP3DMaterialModifierArmature.Create( Self ));
        TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).BuildShader();
      end;
end;

{ TP3DMaterialModifierArmature }

constructor TP3DMaterialModifierArmature.Create(AMeshModifier: TP3DMeshModifier);
begin
  inherited Create();
  MeshModifier:= AMeshModifier;
end;

procedure TP3DMaterialModifierArmature.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
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
begin
  if ( Assigned( TP3DMeshModifierArmature( MeshModifier ).Armature )) then
    ChangeSocketValueInt( ShaderTree.AddCopyEx( 'armature' ), 'numJoints', TP3DMeshModifierArmature( MeshModifier ).Armature.Joints.Count );
end;

procedure TP3DMaterialModifierArmature.PassToShader(Material: TP3DMaterialBase);
begin

end;

class function TP3DMaterialModifierArmature.GetIndex: Integer;
begin
  Result:= 25;
end;

{ TP3DArmatureAction }

procedure TP3DArmatureAction.SetJointTex(AValue: TP3DTexture);
begin
  if FJointTex=AValue then Exit;
  if ( Assigned( JointTex )) then
    begin
      JointTex.RemoveUser( Self );
      if ( OwnsJointTex ) then
        JointTex.Free;
    end;
  FJointTex:=AValue;
  OwnsJointTex:= False;
  if ( Assigned( JointTex )) then
    JointTex.AddUser( Self );
end;

function TP3DArmatureAction.GetJointTex: TP3DTexture;
begin
  if ( not Assigned( FJointTex )) then
    UpdateJointTex;
  Result:= FJointTex;
end;

procedure TP3DArmatureAction.UpdateJointTex;
var
  Buffer: TVec4List;
  Frame: TP3DActionFrame;
  i, j, w, h: Integer;
  m, m2, minv: TMat4;
  q: TQuaternion;
  t: TQuaternion;
  armature: TP3DArmature;

  function getNextPower2( AValue: Integer ): Integer;
  begin
    Result:= 2;
    while Result < AValue do
      Result:= sqr( Result );
  end;

  function FindArmature: TP3DArmature;
  var
    i: Integer;
  begin
    Result:= nil;
    for i:= 0 to Users.Count - 1 do
      if ( Users[ i ] is TP3DArmature ) then
        begin
          Result:= TP3DArmature( Users[ i ]);
          break;
        end;
  end;

begin
  armature:= FindArmature;
  if ( not Assigned( armature )) then
    exit;

  if ( Assigned( FJointTex )) then
    begin
      if ( OwnsJointTex ) then
        FJointTex.Free
      else
        FJointTex.RemoveUser( Self );
    end;
  if ( Frames.Count = 0 ) then
    exit;
  Buffer:= TVec4List.Create;
  w:= {( Frames[ 0 ].Translations.Count + Frames[ 0 ].Rotations.Count );//}getNextPower2( Frames[ 0 ].Translations.Count + Frames[ 0 ].Rotations.Count );
  h:= Frames.Count;//}getNextPower2( Frames.Count );
  for i:= 0 to h - 1 do
    for j:= 0 to w div 2 - 1 do
      if ( i >= Frames.Count ) or ( j >= Frames[ i ].Translations.Count ) then
      //if ( j <> 10 ) then
        Buffer.Add([ vec4( 0 ), vec4( 0 ), vec4( 0 ), vec4( 0 )])
      else
        begin
      //  Buffer.Add([ vec4( 0, 0, 0, 1 ), quatRotation( vec3_Axis_PZ, i * 5 )]);
{          m:= mat4( mat3( Parent.Armatures[ 0 ].Joints[ j ].Rotation ));
          m.Row[ 3 ]:= vec4( Parent.Armatures[ 0 ].Joints[ j ].Position , 1 );
          m2:= mat4( mat3( Frames[ i ].Rotations[ j ]));
          m2.Row[ 3 ]:= vec4( Frames[ i ].Translations[ j ], 1 );
          mat4inverse( m, minv );
          m:= minv*m2*mat4translate( vec4( -Parent.Armatures[ 0 ].Joints[ j ].Position , 1 ));
          q:= quat( mat3( m ));
          t:= m.Row[ 3 ];}
          q:= ( {-Owner.Armatures[ 0 ].Joints[ j ].Rotation * }Frames[ i ].Rotations[ j ]);

          t:=
              //  vec4( Frames[ i ].Translations[ j ]
              //- ( q * vec4( Parent.Armatures[ 0 ].Joints[ j ].Position, 1 )).xyz
              quat( 0.5*vec4(
                ( q * ( -armature.Joints[ j ].Position )) + Frames[ i ].Translations[ j ]
                , 0 ))*q;
          {m:=  mat4translate( Owner.Armatures[ 0 ].Joints[ j ].Position )
             * mat4( mat3(  )
             * mat4translate( -Owner.Armatures[ 0 ].Joints[ j ].Position );}
          Buffer.Add([ t, t, q, q ]); //prevent mix of translation and quaternion through pixel bleeding while having smooth transitions in y direction
          //Buffer.Add([ vec4( Frames[ i ].Translations[ j ], 1 ), m * mat4( mat3( Frames[ i ].Rotations[ j ]])));
        end;
  FJointTex:= TP3DTexture.CreateFromBuffer( w * 2, h, 4, dtFloat, Buffer.Ptr );
  OwnsJointTex:= True;
  FJointTex.AddUser( Self );
  Buffer.Free;
end;

constructor TP3DArmatureAction.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  Frames:= TP3DActionFrameList.Create( Self );
//  FrameSample:= TP3DActionFrame.Create( Self, -1 );
  Owner.Actions.Add( Self );
end;

procedure TP3DArmatureAction.FastDestroy;
begin
//  FrameSample.Free;
  Frames.Clear( True );
  Frames.Free;
  if ( Assigned( JointTex )) then
    JointTex.Free;
  Owner.Actions.Remove( Self );
  inherited FastDestroy;
end;

procedure TP3DArmatureAction.ClearFields;
begin
  JointTex:= nil;
  inherited ClearFields;
end;

procedure TP3DArmatureAction.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
begin
  inherited LoadFromDOM(ADOMNode);
  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'frame': TP3DActionFrame.CreateFromDOM( Element, Self );
      else
        raise Exception.Create( 'Unknown tag inside Action Element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  UpdateJointTex;
end;

function TP3DArmatureAction.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM(AParent);
  Frames.SaveToDOM( Result );
end;

procedure TP3DArmatureAction.UpdateFrameSample;
var
  FrameStart, FrameEnd, i: Integer;
  blend: Float;
begin
  CurrentFrame:= SDL_GetTicks() / 100;
  FrameSample.Translations.Clear;
  FrameSample.Rotations.Clear;
  FrameStart:= Trunc( CurrentFrame ) mod Frames.Count;
  //Frames[ FrameStart ].Translations.AssignTo( FrameSample.Translations );
  //Frames[ FrameStart ].Rotations.AssignTo( FrameSample.Rotations );
  //for i:= 0 to FrameSample.Rotations.Count - 1 do
  //  WriteLn( FrameSample.Rotations[ i ].ToString());

  //for i:= 0 to Frames[ FrameStart ].Translations.Count - 1 do
  //  WriteLn( FrameSample.Translations[ i ].ToString());
  FrameEnd:= ( FrameStart + 1 ) mod Frames.Count;
  blend:= frac( CurrentFrame );
  for i:= 0 to Frames[ FrameStart ].Translations.Count - 1 do
    begin
    FrameSample.Translations.Add( Frames[ FrameEnd ].Translations[ i ] * blend
                                + Frames[ FrameStart ].Translations[ i ] * ( 1 - blend ));
    //WriteLn( FrameSample.Translations[ i ].ToString());
    end;
  for i:= 0 to Frames[ FrameStart ].Rotations.Count - 1 do
    begin
    FrameSample.Rotations.Add( Quat( Frames[ FrameEnd ].Rotations[ i ] * blend
                             + Frames[ FrameStart ].Rotations[ i ] * ( 1 - blend )));
    //WriteLn( FrameSample.Rotations[ i ].ToString());
    end;
end;

procedure TP3DArmatureAction.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew(ADOMNode);
  Frames.LoadFromDOM( ADOMNode );
end;

class function TP3DArmatureAction.DOMNodeName: String;
begin
  Result:= 'action';
end;

{ TP3DActionFrame }

procedure TP3DActionFrame.LoadPoseJointFromDOM(ADOMNode: TDOMElement);
begin
  Pose.Add( TP3DPoseJoint.CreateFromDOM( ADOMNode, Action, Index ));
end;

constructor TP3DActionFrame.Create( AAction: TP3DArmatureAction );
begin
  inherited Create();
  Action:= AAction;
  Rotations:= TQuaternionList.Create;
  Translations:= TVec3List.Create;
  Pose:= TP3DPoseJointList.Create( Self );
  Index:= Action.Frames.Add( Self );
end;

constructor TP3DActionFrame.CreateFromDOM(ADOMNode: TDOMElement; AAction: TP3DArmatureAction );
begin
  Create( AAction );
  LoadFromDOM( ADOMNode );
end;

destructor TP3DActionFrame.Destroy;
begin
  Action.Frames.Remove( Self );
  Pose.Clear( True );
  Pose.Free;
  Rotations.Free;
  Translations.Free;
  inherited Destroy;
end;

procedure TP3DActionFrame.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
begin
  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'joint': LoadPoseJointFromDOM( Element );
      else
        raise Exception.Create( 'Unknown tag inside Action Element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
end;

function TP3DActionFrame.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM(AParent);
  Pose.SaveToDOM( Result );
end;

procedure TP3DActionFrame.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew(ADOMNode);
  Pose.LoadFromDOM( ADOMNode );
end;

class function TP3DActionFrame.DOMNodeName: String;
begin
  Result:= 'frame';
end;

{ TP3DActionFrameList }

constructor TP3DActionFrameList.Create(AAction: TP3DArmatureAction);
begin
  inherited Create;
  Action:= AAction;
end;

procedure TP3DActionFrameList.SaveToDOM(AParent: TDOMElement);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure TP3DActionFrameList.LoadFromDOM(ADOMNode: TDOMElement);
var
  lst: TDOMNodeList;
  i: Integer;
begin
  Clear();
  lst:= ADOMNode.GetElementsByTagName( TP3DActionFrame.DOMNodeName );
  try
    for i:= 0 to lst.Count - 1 do
      with ( TP3DActionFrame.Create( Action )) do
        LoadFromDOMNew( TDOMElement( lst[ i ]));

  finally
    lst.Free;
  end;
end;


{$ENDIF}

{$IFDEF INITIALIZATION}
  P3DMeshModifierClassFactory.Add( TP3DMeshModifierArmature );
{$ENDIF}

