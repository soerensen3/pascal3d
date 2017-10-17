//p3darmature.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

type
  TP3DJointList = class;

  { TP3DJoint }

  TP3DJoint = class( TP3DDataBlock, IP3DRenderable )
    private
      FArmature: TP3DArmature;
      FLength: Float;
      FMatrix: TMat4;
      FMatrixInv: TMat4;
      FQuaternion: TQuaternion;
      FPosition: TVec3;
      FTail: TVec3;

      procedure SetArmature(AValue: TP3DArmature);
      procedure SetQuaternion(AValue: TQuaternion);
      procedure SetPosition(AValue: TVec3);
      procedure SetTail(AValue: TVec3);
      procedure UpdateMatrix;
      class function GetContainer(ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList; override;
      function GetDataPath: String; override;

    public
      constructor Create( const AData: TP3DLibrary = nil ); override;
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DLibrary = nil ); override;
      procedure FastDestroy; override;

      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
      procedure AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      class function DOMNodeName: String; override;

      property Armature: TP3DArmature read FArmature write SetArmature;
      property Quaternion: TQuaternion read FQuaternion write SetQuaternion;
      property Position: TVec3 read FPosition write SetPosition;
      property Matrix: TMat4 read FMatrix;
      property MatrixInv: TMat4 read FMatrixInv;
      property Tail: TVec3 read FTail write SetTail;

    published
      property Length: Float read FLength;
  end;

  TP3DPropertyAccessJoint = specialize gP3DPropertyAccessDataBlock < TP3DJoint >;

  { TP3DJointList }

  TP3DJointList = class ( specialize gP3DNamedInterfacedPersistentList < TP3DJoint > )
    private
      FArmature: TP3DArmature;

    public
      procedure SaveToDOM(AParent: TDOMElement);
      procedure LoadFromDOM( ADOMNode: TDOMElement );

      property Armature: TP3DArmature read FArmature write FArmature;
  end;

  TP3DPropertyAccessJointList = specialize gP3DPropertyAccessDataBlockList < TP3DJointList, TP3DJoint, TP3DPropertyAccessJoint >;

  TP3DArmatureTransformMethod = ( atmMatrix, atmDualQuaternion );

  { TP3DArmature }

  TP3DArmature = class ( TP3DDataBlock, IP3DRenderable )
    private
      FJoints: TP3DJointList;
      FRotations: TQuaternionList;
      FTransformMethod: TP3DArmatureTransformMethod;
      FTranslations: TVec3List;
      OwnsJointTex: Boolean;
      FJointTex: TP3DTexture;

      class function GetContainer( ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList; override;
      function GetDataPath: String; override;

      function GetJointTex: TP3DTexture;
      procedure SetJointTex(AValue: TP3DTexture);
      procedure UpdateJointTex( Root: TP3DObject );
      function CalculateJointMatrixFromObj( AObj: TP3DObject; InvMatrix: TMat4 ): TMat4;

    public
      procedure ClearFields; override;
      procedure RemoveFromFields(Block: TP3DDataBlock); override;

      constructor Create( const AData: TP3DLibrary= nil ); override;

      procedure FastDestroy; override;

      // IP3DRenderable
      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );

      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      function SaveToDOM(AParent: TDOMElement): TDOMElement; override;
      class function DOMNodeName: String; override;

    published
      property Joints: TP3DJointList read FJoints;
      property JointTex: TP3DTexture read GetJointTex write SetJointTex;
      property Translations: TVec3List read FTranslations write FTranslations;
      property Rotations: TQuaternionList read FRotations write FRotations;
      property TransformMethod: TP3DArmatureTransformMethod read FTransformMethod write FTransformMethod;
  end;

  TP3DArmatureList = specialize gP3DNamedInterfacedPersistentList < TP3DArmature >;
  TP3DPropertyAccessArmature = specialize gP3DPropertyAccessDataBlock < TP3DArmature >;
  TP3DPropertyAccessArmatureList = specialize gP3DPropertyAccessDataBlockList < TP3DArmatureList, TP3DArmature, TP3DPropertyAccessArmature >;



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
      FInitialized: Boolean;
      procedure SetArmature(AValue: TP3DArmature);
      procedure SetMesh(AValue: TP3DMesh); override;

    public
      constructor Create( AMesh: TP3DMesh ); override;
      destructor Destroy; override;

      procedure UpdateMaterials;

      procedure PassToShader( Material: TP3DMaterial; var world: TMat4 ); override;
      procedure LoadFromDOM(ADOMNode: TDOMElement); override;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;

      property Initialized: Boolean read FInitialized;

    published
      property Armature: TP3DArmature read FArmature write SetArmature;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DJointList }

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
        NamedItems[ i ]:= TP3DJoint.Create( Armature.Owner );
        NamedItems[ i ].Armature:= Armature;
        NamedItems[ i ].LoadFromDOMNew( TDOMElement( lst[ i ]));
      end;

  finally
    lst.Free;
  end;
end;

{ TP3DPoseJointList }
{
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
        WriteLn( 'Q: ', Items[ i ].Quaternion.ToString(), ' T: ', Items[ i ].Position.ToString());
      end;

  finally
    lst.Free;
  end;
end;
}

{ TP3DPoseJoint }
{
function TP3DPoseJoint.GetQuaternion: TQuaternion;
begin
  if ( FrameIndex > -1 ) then
    Result:= Action.Frames[ FrameIndex ].Rotations[ RotIdx ]
  else
    Result:= Action.FrameSample.Rotations[ RotIdx ];
end;

function TP3DPoseJoint.GetMatrix: TMat4;
begin
  with ( Action.Frames[ FrameIndex ]) do
  Result:= mat4translate( vec4( Translations[ PosIdx ], 1 )) * mat4( {mat3scale( vec3( Length )) * }mat3( Rotations[ RotIdx ]))
end;

procedure TP3DPoseJoint.SetQuaternion(AValue: TQuaternion);
begin
  if ( FrameIndex > -1 ) then
    Action.Frames[ FrameIndex ].Rotations[ RotIdx ]:= AValue
  else
    Action.FrameSample.Rotations[ RotIdx ]:= AValue;
end;


function TP3DPoseJoint.GetPosition: TVec3;
begin
  if ( FrameIndex > -1 ) then
    Result:= Action.Frames[ FrameIndex ].Translations[ PosIdx ]
  else
    Result:= Action.FrameSample.Translations[ PosIdx ];
end;

procedure TP3DPoseJoint.SetPosition(AValue: TVec3);
begin
  if ( FrameIndex > -1 ) then
    Action.Frames[ FrameIndex ].Translations[ PosIdx ]:= AValue
  else
    Action.FrameSample.Translations[ PosIdx ]:= AValue;
end;

constructor TP3DPoseJoint.Create(AAction: TP3DArmatureAction; AFrameIndex: Integer);
begin
  inherited Create;
  FFrameIndex:= AFrameIndex;
  Action:= AAction;

  if ( FrameIndex > -1 ) then
    begin
      RotIdx:= Action.Frames[ FrameIndex ].Rotations.Add( quat( 0, 0, 0, 1 ));
      PosIdx:= Action.Frames[ FrameIndex ].Translations.Add( vec3( 0 ));
    end
  else
    begin
      RotIdx:= Action.FrameSample.Rotations.Add( quat( 0, 0, 0, 1 ));
      PosIdx:= Action.FrameSample.Translations.Add( vec3( 0 ));
    end;

  Properties.Context:= Action;
  Properties.Add( TP3DPropertyAccessQuat.CreateGetSet( 'Quaternion', @GetQuaternion, @SetQuaternion, smAttribute ));
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
  Quaternion:= Rot;
end;

class function TP3DPoseJoint.DOMNodeName: String;
begin
  Result:= 'pose';
end;
}
{ TP3DJoint }


procedure TP3DJoint.SetArmature(AValue: TP3DArmature);
begin
  if FArmature=AValue then Exit;

  if ( Assigned( Armature )) then
    Armature.Joints.Remove( Self );
  FArmature:=AValue;

  if ( Assigned( Armature )) then
    Armature.Joints.Add( Self );
end;

procedure TP3DJoint.SetQuaternion(AValue: TQuaternion);
begin
  if FQuaternion=AValue then Exit;
  FQuaternion:=AValue;
  UpdateMatrix;
end;

procedure TP3DJoint.SetPosition(AValue: TVec3);
begin
  if FPosition=AValue then Exit;
  FPosition:=AValue;
  UpdateMatrix;
  FLength:= len( Tail - Position );
end;

procedure TP3DJoint.SetTail(AValue: TVec3);
begin
  if FTail=AValue then Exit;
  FTail:=AValue;
  FLength:= len( Tail - Position );
end;

procedure TP3DJoint.UpdateMatrix;
begin
  FMatrix:= mat4translate( vec4( Position, 1 )) * mat4( mat3( Quaternion ));
  FMatrixInv:= -FMatrix
end;

class function TP3DJoint.GetContainer(ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList;
begin
  Result:= ALib.Joints;
end;

function TP3DJoint.GetDataPath: String;
begin
  Result:= 'Joints[ "' + Name + '" ]';
end;

constructor TP3DJoint.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  Owner.Joints.Add( Self );
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Length', smAttribute ));
  Properties.Add( TP3DPropertyAccessVec3.CreateFieldSet( 'Position', @FPosition, @SetPosition, smAttribute ));
  Properties.Add( TP3DPropertyAccessQuat.CreateFieldSet( 'Quaternion', @Quaternion, @SetQuaternion, smAttribute ));
  Properties.Add( TP3DPropertyAccessVec3.CreateFieldSet( 'Tail', @FTail, @SetTail, smAttribute ));
end;

constructor TP3DJoint.CreateFromDOM(ADOMNode: TDOMElement; const AData: TP3DLibrary);
var
  n: Integer;
begin
  Create( AData );

  LoadFromDOM( ADOMNode );
end;

procedure TP3DJoint.FastDestroy;
begin
  Owner.Joints.Add( Self );
  inherited FastDestroy;
end;

procedure TP3DJoint.Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
begin

end;

procedure TP3DJoint.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
begin
  if ( rlfJoints in AFlags ) then
    AList.PushRenderObject( Self, AWorld, nil );
end;

procedure TP3DJoint.LoadFromDOMNew(ADOMNode: TDOMElement);
var
  n: Integer;
begin
  {
  n:= Owner.AppendFromDOM( TDOMElement( ADOMNode.ParentNode ));
  if (( n > -1 ) and ( Owner.DataBlocks[ n ] is TP3DArmature )) then
    Armature:= Owner.DataBlocks[ n ] as TP3DArmature;
  inherited LoadFromDOMNew(ADOMNode);
  }
end;

class function TP3DJoint.DOMNodeName: String;
begin
  Result:= 'joint';
end;

{ TP3DArmature }

procedure TP3DArmature.SetJointTex(AValue: TP3DTexture);
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

class function TP3DArmature.GetContainer( ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList;
begin
  Result:= ALib.Armatures;
end;

function TP3DArmature.GetDataPath: String;
begin
  Result:= 'Armatures[ "' + Name + '" ]';
end;

function TP3DArmature.GetJointTex: TP3DTexture;
begin
  Result:= FJointTex;
end;

procedure TP3DArmature.UpdateJointTex(Root: TP3DObject);
var
  Buffer: TVec4List;
  i, j, w, h, n: Integer;
  m: TMat4;
  q: TQuaternion;
  t: TVec4;
  rlist: TP3DRenderList;
  Capt: String;
  Joint: TP3DJoint;
  JointObj: TP3DObject;

  function getNextPower2( AValue: Integer ): Integer;
  begin
    Result:= 2;
    while Result < AValue do
      Result:= sqr( Result );
  end;

begin
  if ( Assigned( FJointTex )) then
    begin
      if ( OwnsJointTex ) then
        FJointTex.Free
      else
        FJointTex.RemoveUser( Self );
    end;

  rlist:= TP3DRenderList.Create;
  Root.AddToRenderList( nil, Mat4Identity, rlist, [ rlfJoints ]);
  Buffer:= TVec4List.Create;
  w:= getNextPower2( Joints.Count );
  h:= 1;
  for i:= 0 to h - 1 do
    for j:= 0 to w - 1 do
      if ( j >= Joints.Count ) then
        Buffer.Add([ vec4( 1, 0, 0, 0 ), vec4( 0, 1, 0, 0 ), vec4( 0, 0, 1, 0 ), vec4( 0, 0, 0, 1 )])
      else
        begin
          n:= rlist.Find( Joints.NamedItems[ j ], nil );

          Capt:= '';
          Joint:= Joints.NamedItems[ j ];
          JointObj:= TP3DObject( Joint.Users[ 0 ]);
          case TransformMethod of
            atmMatrix:
              begin
                //Capt += '[{ ' + Joints.NamedItems[ j ].MatrixInv.ToString + '}, {' + rlist[ n ].Matrices[ 0 ].ToString + '}]';
                if ( Assigned( JointObj )) then
                  //m:= Joints.NamedItems[ j ].MatrixInv * rlist[ n ].Matrices[ 0 ]
                  //m:= Joint.MatrixInv * JointObj.CalculateParentMatrix * ( mat4translate( vec4( JointObj.Position, 1 )) * mat4( mat3( JointObj.Quaternion )))
                  {begin
                    m:= mat4( mat3( JointObj.Quaternion ));
                    m:= m * mat4translate( vec4( JointObj.Position, 1 ));
                    m:= JointObj.CalculateParentMatrix * m;
                    m:= Joint.MatrixInv * m;
                  end}
                  m:= CalculateJointMatrixFromObj( JointObj, -( Root.CalculateParentMatrix * Root.Matrix ))
                else
                  m:= Mat4Identity;

                Buffer.Add([ m.Row[ 0 ], m.Row[ 1 ], m.Row[ 2 ], m.Row[ 3 ]]);
              end;
            atmDualQuaternion: //only half implemented
              begin
                if ( n > -1 ) then
                  q:= -Joints.NamedItems[ j ].Quaternion * quat( rlist[ n ].Rotations[ 0 ])
                else
                  m:= Mat4Identity;

                Buffer.Add([ q, t ]);
              end;
          end;
          //P3DApplication.MainWindow.Title:= Capt
        end;
  FJointTex:= TP3DTexture.CreateFromBuffer( w * ( 2 + Ord( TransformMethod = atmMatrix ) * 2 ), h, 4, dtFloat, Buffer.Ptr, Owner );
  OwnsJointTex:= True;
  FJointTex.AddUser( Self );
  //FJointTex.SaveToFile( '/tmp/jointtex' + FJointTex.Name + '.png' );
  Buffer.Free;
  rlist.Free;
end;

function TP3DArmature.CalculateJointMatrixFromObj(AObj: TP3DObject; InvMatrix: TMat4 ): TMat4;
var
  Obj: TP3DObject;
begin
  Result:= Mat4Identity;
  Obj:= AObj;
  while Assigned( Obj ) do
    begin
      //Result:= Result * ( mat4translate( vec4( Obj.Position, 1 )) * mat4( mat3( Obj.Quaternion )));
      Result:= ( mat4translate( vec4( Obj.Position, 1 )) * mat4( mat3( Obj.Quaternion ))) * Result;
      if ( Obj.Parent is TP3DObject ) then
        Obj:= TP3DObject( Obj.Parent )
      else
        Obj:= nil;
    end;
  Result:= InvMatrix * Result;
  if ( Assigned( AObj ) and ( AObj.Data is TP3DJoint )) then
    Result:= Result * TP3DJoint( AObj.Data ).MatrixInv;
end;

procedure TP3DArmature.ClearFields;
begin
  inherited ClearFields;
end;

procedure TP3DArmature.RemoveFromFields(Block: TP3DDataBlock);
begin
  inherited RemoveFromFields(Block);
end;

constructor TP3DArmature.Create(const AData: TP3DLibrary);
begin
  inherited Create( AData );
  FJoints:= TP3DJointList.Create( Self );
  FJoints.Armature:= Self;
  FRotations:= TQuaternionList.Create;
  FTranslations:= TVec3List.Create;
  //Owner.Armatures.Add( Self );
  //Properties.Add( TP3DPropertyAccessArmatureAction.CreateFieldSet( 'CurrentAction', @FCurrentAction, @SetCurrentAction, smAttribute ));
  Properties.Add( TP3DPropertyAccessJointList.CreateField( 'Joints', @FJoints, smAttribute ));
end;

procedure TP3DArmature.FastDestroy;
begin
  FRotations.Free;
  FTranslations.Free;
  FJoints.Clear( False );
  FJoints.Free;
  //Owner.Armatures.Remove( Self );

  inherited FastDestroy;
end;

procedure TP3DArmature.Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
begin

end;

procedure TP3DArmature.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
begin

end;

procedure TP3DArmature.LoadFromDOM(ADOMNode: TDOMElement);
var
  Element: TDOMElement;
  ActIdx: Integer;
begin
  inherited LoadFromDOM( ADOMNode );
  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      //case Element.NodeName of
        //'joint': Root.Add( TP3DJoint.CreateFromDOM( Element, Self ));
      //else
      //  raise Exception.Create( 'Unknown tag inside Armature element: '+ Element.NodeName );
      //end;
      Element:= TDOMElement( Element.NextSibling );
    end;
end;

procedure TP3DArmature.LoadFromDOMNew(ADOMNode: TDOMElement);
begin
  inherited LoadFromDOMNew(ADOMNode);
  //Root.LoadFromDOM( ADOMNode );
end;

function TP3DArmature.SaveToDOM(AParent: TDOMElement): TDOMElement;
begin
  Result:= inherited SaveToDOM(AParent);
  //Root.SaveToDOM( Result );
end;

class function TP3DArmature.DOMNodeName: String;
begin
  Result:= 'armature';
end;

{ TP3DMeshModifierArmature }

procedure TP3DMeshModifierArmature.SetArmature(AValue: TP3DArmature);
begin
  if FArmature=AValue then Exit;
  FArmature:=AValue;
  if ( Assigned( Mesh )) then
    begin
      Mesh.FArmature:= AValue;
      //UpdateMaterials;
    end;
end;

procedure TP3DMeshModifierArmature.SetMesh(AValue: TP3DMesh);
begin
  inherited SetMesh(AValue);
  if ( Assigned( Mesh )) then
    begin
      Mesh.FArmature:= Armature;
      //UpdateMaterials;
    end;
end;

constructor TP3DMeshModifierArmature.Create( AMesh: TP3DMesh );
begin
  inherited Create( AMesh );

  Properties.Add( TP3DPropertyAccessArmature.CreateFieldSet( 'Armature', @FArmature, @SetArmature, smAttribute ));
  FInitialized:= False;
end;

destructor TP3DMeshModifierArmature.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DMeshModifierArmature.UpdateMaterials;
var
  i: Integer;
begin
  if ( Assigned( Mesh )) then
    begin
      for i:= 0 to Mesh.MaterialGroups.Count -1 do
        if ( Mesh.MaterialGroups[ i ].Material is TP3DMaterialBase ) then
          begin
            TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).Modifiers.Add( TP3DMaterialModifierArmature.Create( Self ));
            TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).BuildShader();
          end;
      FInitialized:= True;
    end;
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
  if ( not Initialized ) then
    UpdateMaterials;
  if ( Assigned( Armature ) {and Assigned( Armature.CurrentAction )}) then
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
      Armature.UpdateJointTex( TP3DObject( Armature.Users.Items[ 0 ]) );
      i:= Material.Maps.Count;
      glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
      glBindTexture( GL_TEXTURE_2D, Armature.JointTex.GLTexture ); cle( Self );

      decl:= Material.Shader.Uniforms.Names[ 'jointTex' ];
      if ( Assigned( decl )) then
        decl.AsInt:= i;
      decl:= Material.Shader.Uniforms.Names[ 'jointTexWidth' ];
      if ( Assigned( decl )) then
        decl.AsInt:= Armature.JointTex.Width;
      decl:= Material.Shader.Uniforms.Names[ 'jointTexHeight' ];
      if ( Assigned( decl )) then
        decl.AsInt:= Armature.JointTex.Height;

      //Armature.CurrentAction.CurrentFrame:= SDL_GetTicks() / 100;
      //P3DApplication.MainWindow.Title:= FloatToStr( Armature.CurrentAction.CurrentFrame ) + ':' + FloatToStr( frac( Armature.CurrentAction.CurrentFrame / Armature.CurrentAction.Frames.Count ));
      decl:= Material.Shader.Uniforms.Names[ 'time' ];
      if ( Assigned( decl )) then
        decl.AsFloat:= 0;//frac( Armature.CurrentAction.CurrentFrame / Armature.CurrentAction.Frames.Count );
    end;
end;

procedure TP3DMeshModifierArmature.LoadFromDOM(ADOMNode: TDOMElement);
var
  data: DOMString;
  i: Integer;
begin
  {
  data:= ADOMNode.GetAttribute( 'data' );
  Armature:= TP3DArmature( Mesh.Owner.DataBlocks[ Mesh.Owner.AppendFromPath( data, TP3DArmature )]);
  for i:= 0 to Mesh.MaterialGroups.Count -1 do
    if ( Mesh.MaterialGroups[ i ].Material is TP3DMaterialBase ) then
      begin
        TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).Modifiers.Add( TP3DMaterialModifierArmature.Create( Self ));
        TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).BuildShader();
      end;}
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
    begin
      ChangeSocketValueInt( ShaderTree.AddCopyEx( '00025_pass_armature_matrix' ), 'numJoints', TP3DMeshModifierArmature( MeshModifier ).Armature.Joints.Count );
      //ChangeSocketValueInt( ShaderTree.AddCopyEx( '00025_pass_armature' ), 'numJoints', TP3DMeshModifierArmature( MeshModifier ).Armature.Joints.Count );
      ShaderTree.AddCopyEx( '01050_pass_armature_debug' );
    end;
end;

procedure TP3DMaterialModifierArmature.PassToShader(Material: TP3DMaterialBase);
begin

end;

class function TP3DMaterialModifierArmature.GetIndex: Integer;
begin
  Result:= 25;
end;

{ TP3DArmatureAction }

{$ENDIF}

{$IFDEF INITIALIZATION}
  P3DMeshModifierClassFactory.Add( TP3DMeshModifierArmature );
{$ENDIF}

