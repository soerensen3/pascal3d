//p3darmature.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

type
  TP3DJoint = class;

  { TP3DJointList }

  TP3DJointList = class ( specialize gP3DCustomObjectList < TP3DJoint > )
    public
      function FindByName( Name: String ): Integer;
  end;

  { TP3DActionFrame }

  TP3DActionFrame = class( TPersistent )
    private
      FRotations: TQuaternionList;
      FTranslations: TVec3List;

      procedure LoadPoseJointFromDOM( ADOMNode: TDOMElement );

    public
      constructor Create();
      constructor CreateFromDOM( ADOMNode: TDOMElement );
      destructor Destroy; override;

      procedure LoadFromDOM( ADOMNode: TDOMElement );

    published
      property Translations: TVec3List read FTranslations write FTranslations;
      property Rotations: TQuaternionList read FRotations write FRotations;
  end;

  TP3DActionFrameList = specialize gP3DCustomObjectList < TP3DActionFrame >;

  { TP3DArmatureAction }

  TP3DArmatureAction = class ( TP3DDataBlock )
    private
      FCurrentFrame: Float;
      FFrames: TP3DActionFrameList;
      FFrameSample: TP3DActionFrame;
      FJointTex: TP3DTexture;

    public
      procedure UpdateJointTex;

      constructor Create(const AData: TP3DData=nil);
      destructor Destroy; override;

      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      procedure UpdateFrameSample;

    published
      property Frames: TP3DActionFrameList read FFrames write FFrames;
      property FrameSample: TP3DActionFrame read FFrameSample write FFrameSample;
      property CurrentFrame: Float read FCurrentFrame write FCurrentFrame;
      property JointTex: TP3DTexture read FJointTex write FJointTex;
  end;

  TP3DArmatureActionList = specialize gP3DCustomDatablockList < TP3DArmatureAction >;

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
      constructor Create( const AData: TP3DData= nil );
      destructor Destroy; override;

      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DActor); override;
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;

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

  { TP3DJoint }

  TP3DJoint = class( TPersistent )
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
      destructor Destroy; override;

      procedure LoadFromDOM(ADOMNode: TDOMElement);

      property Rotation: TQuaternion read GetRotation write SetRotation;
      property Position: TVec3 read GetPosition write SetPosition;
      property Name: String read FName write FName;
      property Joints: TP3DJointList read FJoints;
      property Armature: TP3DArmature read FArmature write FArmature;
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
      FGridSize: Single;

    public
      constructor Create( AMesh: TP3DMesh ); override;
      destructor Destroy; override;

      procedure PassToShader( Scene: TP3DScene; Material: TP3DMaterialShader; var world: TMat4 ); override;
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;

    published
      property GridSize: Single read FGridSize write FGridSize;
      property Armature: TP3DArmature read FArmature write FArmature;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DJointList }

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
  FJoints:= TP3DJointList.Create;
  RotIdx:= Armature.Rotations.Add( quat( 0, 0, 0, 1 ));
  PosIdx:= Armature.Translations.Add( vec3( 0 ));
end;

constructor TP3DJoint.CreateFromDOM(ADOMNode: TDOMElement; AArmature: TP3DArmature);
begin
  Create( AArmature );
  LoadFromDOM( ADOMNode );
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

{ TP3DArmature }

procedure TP3DArmature.SetCurrentAction(AValue: TP3DArmatureAction);
begin
  FCurrentAction:= AValue;
end;

procedure TP3DArmature.SetCurrentFrame(AValue: Float);
begin
  FCurrentFrame:= AValue;
end;

constructor TP3DArmature.Create(const AData: TP3DData);
begin
  inherited Create( AData );
  FJoints:= TP3DJointList.Create;
  FRoot:= TP3DJointList.Create;
  FRotations:= TQuaternionList.Create;
  FTranslations:= TVec3List.Create;
end;

destructor TP3DArmature.Destroy;
begin
  FRotations.Free;
  FTranslations.Free;
  FRoot.Clear( False );
  FRoot.Free;
  FJoints.Clear( True );
  FJoints.Free;
  inherited Destroy;
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
  Name:= ADOMNode.GetAttribute( 'name' );
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
  ActIdx:= Parent.AppendFromDom( TDOMElement( ADOMNode.OwnerDocument.FirstChild ), Action, TP3DArmatureAction );
  CurrentAction:= TP3DArmatureAction( Parent.DataBlocks[ ActIdx ]);
end;

{ TP3DMeshModifierArmature }

constructor TP3DMeshModifierArmature.Create( AMesh: TP3DMesh );
var
  i: Integer;
begin
  inherited Create( AMesh );


end;

destructor TP3DMeshModifierArmature.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DMeshModifierArmature.PassToShader(Scene: TP3DScene; Material: TP3DMaterialShader; var world: TMat4);
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
      decl.AsInt:= i;
      decl:= Material.Shader.Uniforms.Names[ 'jointTexWidth' ];
      decl.AsInt:= Armature.CurrentAction.JointTex.Width div 2;
      decl:= Material.Shader.Uniforms.Names[ 'jointTexHeight' ];
      decl.AsInt:= Armature.CurrentAction.JointTex.Height;
      decl:= Material.Shader.Uniforms.Names[ 'time' ];
      Armature.CurrentAction.CurrentFrame:= SDL_GetTicks() / 100;
      decl.AsFloat:= Armature.CurrentAction.CurrentFrame / Armature.CurrentAction.Frames.Count;
    end;
end;

procedure TP3DMeshModifierArmature.LoadFromDOM(ADOMNode: TDOMElement);
var
  data: DOMString;
  i: Integer;
begin
  data:= ADOMNode.GetAttribute( 'data' );
  Armature:= TP3DArmature( Mesh.Parent.DataBlocks[ Mesh.Parent.AppendFromDom( TDOMElement( ADOMNode.OwnerDocument.FirstChild ), data, TP3DArmature )]);
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

procedure TP3DArmatureAction.UpdateJointTex;
var
  Buffer: TVec4List;
  Frame: TP3DActionFrame;
  i, j, w, h: Integer;
  m, m2, minv: TMat4;
  q: TQuaternion;
  t: TVec4;
  armature: TP3DArmature;

  function getNextPower2( AValue: Integer ): Integer;
  begin
    Result:= 2;
    while Result < AValue do
      Result:= sqr( Result );
  end;

begin
  if ( Assigned( JointTex )) then
    JointTex.Free;
  Buffer:= TVec4List.Create;
  w:= ( Frames[ 0 ].Translations.Count + Frames[ 0 ].Rotations.Count );//getNextPower2( Frames[ 0 ].Translations.Count + Frames[ 0 ].Rotations.Count );
  h:= Frames.Count;//getNextPower2( Frames.Count );
  for i:= 0 to h - 1 do
    for j:= 0 to w div 2 - 1 do
      if ( i >= Frames.Count ) or ( j >= Frames[ i ].Translations.Count ) then
      //if ( j <> 10 ) then
        Buffer.Add([ vec4( 0 ), vec4( 0 )])
      else
        begin
      //  Buffer.Add([ vec4( 0, 0, 0, 1 ), quatRotation( vec3_Axis_PZ, i * 5 )]);
          armature:= Parent.Armatures[ 0 ];
{          m:= mat4( mat3( Parent.Armatures[ 0 ].Joints[ j ].Rotation ));
          m.Row[ 3 ]:= vec4( Parent.Armatures[ 0 ].Joints[ j ].Position , 1 );
          m2:= mat4( mat3( Frames[ i ].Rotations[ j ]));
          m2.Row[ 3 ]:= vec4( Frames[ i ].Translations[ j ], 1 );
          mat4inverse( m, minv );
          m:= minv*m2*mat4translate( vec4( -Parent.Armatures[ 0 ].Joints[ j ].Position , 1 ));
          q:= quat( mat3( m ));
          t:= m.Row[ 3 ];}
          q:= -Parent.Armatures[ 0 ].Joints[ j ].Rotation * Frames[ i ].Rotations[ j ];
          t:=
              //  vec4( Frames[ i ].Translations[ j ]
              //- ( q * vec4( Parent.Armatures[ 0 ].Joints[ j ].Position, 1 )).xyz
              vec4(
                -(q*Parent.Armatures[ 0 ].Joints[ j ].Position) + Frames[ i ].Translations[ j ]
                , 1 );
          Buffer.Add([ t, t, q, q ]); //prevent mix of translation and quaternion through pixel bleeding while having smooth transitions in y direction
          //Buffer.Add([ vec4( Frames[ i ].Translations[ j ], 1 ), m * mat4( mat3( Frames[ i ].Rotations[ j ]])));
        end;
  JointTex:= TP3DTexture.CreateFromBuffer( w * 2, h, 4, dtFloat, Buffer.Ptr );
  P3DData.Textures.Add( JointTex );
  Buffer.Free;
end;

constructor TP3DArmatureAction.Create(const AData: TP3DData);
begin
  inherited Create( AData );
  Frames:= TP3DActionFrameList.Create;
  FrameSample:= TP3DActionFrame.Create();
end;

destructor TP3DArmatureAction.Destroy;
begin
  FrameSample.Free;
  Frames.Free;
  if ( Assigned( JointTex )) then
    JointTex.Free;
  inherited Destroy;
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
        'frame': Frames.Add( TP3DActionFrame.CreateFromDOM( Element ));
      else
        raise Exception.Create( 'Unknown tag inside Action Element: '+ Element.NodeName );
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  UpdateJointTex;
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

{ TP3DActionFrame }

procedure TP3DActionFrame.LoadPoseJointFromDOM(ADOMNode: TDOMElement);
var
  Position: TVec3;
  Rotation: TQuaternion;
begin
  Position.FromString( ADOMNode.GetAttribute( 'position' ));
  Rotation.FromString( ADOMNode.GetAttribute( 'quaternion' ));
  Translations.Add( Position );
  Rotations.Add( Rotation );
end;

constructor TP3DActionFrame.Create;
begin
  inherited Create();
  Rotations:= TQuaternionList.Create;
  Translations:= TVec3List.Create;
end;

constructor TP3DActionFrame.CreateFromDOM(ADOMNode: TDOMElement);
begin
  Create();
  LoadFromDOM( ADOMNode );
end;

destructor TP3DActionFrame.Destroy;
begin
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

{$ENDIF}

