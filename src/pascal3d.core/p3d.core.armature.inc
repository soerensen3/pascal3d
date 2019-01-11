//p3darmature.inc
//part of p3dgraphics.pas

{$IFDEF INTERFACE}

type
  TP3DArmature = class;
  { TP3DJoint }

  TP3DJoint = class( TP3DNamedStreamable, IP3DTransform )
    protected
      FArmature: TP3DArmature;
      FLength: Float;
      //FMatrixInv: TMat4;
      FTransform: TP3DStreamableContainer;
      FTail: TVec3;

      function GetTransform: TP3DTransform;
      procedure SetArmature(AValue: TP3DArmature); virtual;
      procedure SetTail(AValue: TVec3);
      procedure UpdateMatrix;
      procedure OnTransformChange( Sender: TObject );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      function GetJointParent: TP3DJoint; virtual; abstract;
      //procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
      //procedure AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);

      property Armature: TP3DArmature read FArmature write SetArmature;
      //property MatrixInv: TMat4 read FMatrixInv;
      property Transform: TP3DTransform read GetTransform;
      property Tail: TVec3 read FTail write SetTail;

    published
      property Length: Float read FLength;
  end;

  TP3DRestJointPointerList = class;

  { TP3DRestJoint }

  TP3DRestJoint = class ( TP3DJoint )
    protected
      FChildren: TP3DRestJointPointerList;
      FParent: TP3DStreamablePointer;
      FTransformLocal: TP3DStreamableContainer;

      function GetParent: TP3DRestJoint;
      function GetTransformLocal: TP3DTransform;
      procedure SetArmature(AValue: TP3DArmature); override;
      procedure SetParent(AValue: TP3DRestJoint);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      function GetJointParent: TP3DJoint; override;

    published
      property Children: TP3DRestJointPointerList read FChildren write FChildren;
      property Parent: TP3DRestJoint read GetParent write SetParent;
      property TransformLocal: TP3DTransform read GetTransformLocal;
  end;


  TP3DPoseJointPointerList = class;

  { TP3DPoseJoint }

  TP3DPoseJoint = class ( TP3DJoint )
    protected
      FChildren: TP3DPoseJointPointerList;
      FParent: TP3DPoseJoint;
      FRestJoint: TP3DRestJoint;

      procedure SetParent(AValue: TP3DPoseJoint);
      procedure OnChildrenChange(Sender: TObject; Item: TP3DStreamable; Action: TP3DListOnChangeAction);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      function GetJointParent: TP3DJoint; override;

    published
      property Children: TP3DPoseJointPointerList read FChildren;
      property Parent: TP3DPoseJoint read FParent write SetParent;
      property RestJoint: TP3DRestJoint read FRestJoint write FRestJoint;
  end;

  {TP3DRestJointContainer = specialize gP3DStreamableContainer < TP3DRestJoint >;
  TP3DRestJointPointer = class( specialize gP3DStreamablePointer < TP3DRestJoint >);
  //TP3DRestJointContainerList = specialize gP3DNamedStreamableContainerList < TP3DRestJoint, TP3DRestJointContainer >;

  TP3DPoseJointContainer = specialize gP3DStreamableContainer < TP3DPoseJoint >;
  TP3DPoseJointPointer = specialize gP3DStreamablePointer < TP3DPoseJoint >;
  //TP3DPoseJointContainerList = specialize gP3DNamedStreamableContainerList < TP3DPoseJoint, TP3DPoseJointContainer >;
  }
  TP3DRestJointPointerList = class( specialize gP3DStreamablePointerList < TP3DRestJoint >);
  TP3DPoseJointPointerList = class( specialize gP3DStreamablePointerList < TP3DPoseJoint >);
  { TP3DJointList }

  TP3DRestJointContainerList = class ( specialize gP3DNamedStreamableContainerList < TP3DRestJoint >)
    private
      FArmature: TP3DArmature;

    public
      property Armature: TP3DArmature read FArmature write FArmature;
  end;

  TP3DObjectModifierArmature = class;

  { TP3DPoseJointList }

  TP3DPoseJointContainerList = class ( specialize gP3DNamedStreamableContainerList < TP3DPoseJoint >)
    private
      FArmature: TP3DArmature;
      FModifier: TP3DObjectModifierArmature;

    public
      property Armature: TP3DArmature read FArmature write FArmature;
      property Modifier: TP3DObjectModifierArmature read FModifier write FModifier;
  end;

  TP3DArmatureTransformMethod = ( atmMatrix, atmDualQuaternion );

  { TP3DArmature }

  TP3DMaterialModifierArmature = class;

  TP3DArmature = class ( TP3DDataBlock, IP3DRenderable )
    protected
      FJoints: TP3DRestJointContainerList;
      FMaterialModifier: TP3DMaterialModifierArmature;
      FRotations: TQuatList;
      FTransformMethod: TP3DArmatureTransformMethod;
      FTranslations: TVec3List;

      function CalculateJointMatrix( AJoint: TP3DPoseJoint; InvMatrix: TMat4 ): TMat4;
      function GetObjArmatureModifier( AObj: TP3DObject ): TP3DObjectModifierArmature;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      class function GetContainer( ALib: TP3DLibrary): TP3DPropertyAccess; override;
      // IP3DRenderable
      procedure Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
      procedure AddToRenderList( ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags );

    published
      property Joints: TP3DRestJointContainerList read FJoints;
      property Translations: TVec3List read FTranslations write FTranslations;
      property Rotations: TQuatList read FRotations write FRotations;
      property TransformMethod: TP3DArmatureTransformMethod read FTransformMethod write FTransformMethod;
      property MaterialModifier: TP3DMaterialModifierArmature read FMaterialModifier write FMaterialModifier;
  end;

  {TP3DArmatureContainer = specialize gP3DStreamableContainer < TP3DArmature >;
  TP3DArmaturePointer = specialize gP3DStreamablePointer < TP3DArmature >;
  TP3DArmaturePointerList = specialize gP3DStreamableList < TP3DArmature, TP3DArmaturePointer >;
  }
  TP3DArmatureContainerList = specialize gP3DNamedStreamableContainerList < TP3DArmature >;


  { TP3DMaterialModifierArmature }

  TP3DMaterialModifierArmature = class ( TP3DMaterialModifier )
    private
      FArmature: TP3DArmature;

    published
      constructor Create( AArmature: TP3DArmature );

      procedure AddPass( ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase ); override;
      procedure PassToShader( Material: TP3DMaterialBase ); override;
      class function GetIndex: Integer; override;

      property Armature: TP3DArmature read FArmature write FArmature;
  end;


  { TP3DMeshModifierArmature }

{  TP3DMeshModifierArmature = class ( TP3DMeshModifier )
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
  end deprecated;}

  { TP3DObjectModifierArmature }

  TP3DObjectModifierArmature = class ( TP3DObjectModifier, IP3DPlottableSettings )
    private
      FArmature: TP3DStreamablePointer;
      FInitialized: Boolean;
      FPose: TP3DPoseJointContainerList;
      OwnsJointTex: Boolean;
      FJointTex: TP3DStreamablePointer;
      LinesAuto: specialize TAuto< TP3DVec3BufferGL >;

      function GetJointTex: TP3DTexture;
      procedure SetJointTex(AValue: TP3DTexture);
      function GetArmature: TP3DArmature;
      procedure SetArmature( AValue: TP3DArmature );
      procedure SetObj( AValue: TP3DObject ); override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      procedure UpdateMaterials;

      procedure PassToShader( Material: TP3DMaterial; var world: TMat4 ); override;
      procedure AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase); override;

      procedure UpdateJointTex( ARoot: TP3DObject );
      procedure UpdateLines(ARoot: TP3DObject);

      function GetSettings: TP3DPlotListSetting;

      property Initialized: Boolean read FInitialized;

    published
      property Armature: TP3DArmature read GetArmature write SetArmature;
      property Lines: TP3DVec3BufferGL read LinesAuto.FInstance;
      property JointTex: TP3DTexture read GetJointTex;
      property Pose: TP3DPoseJointContainerList read FPose;
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DJointList }

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
function TP3DPoseJoint.GetQuaternion: TQuat;
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

procedure TP3DPoseJoint.SetQuaternion(AValue: TQuat);
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
  Properties.Add( TP3DPropertyAccessQuat.CreateGetSet( 'Quaternion', @GetQuaternion, @SetQuaternion, smText ));
  Properties.Add( TP3DPropertyAccessVec3.CreateGetSet( 'Position', @GetPosition, @SetPosition, smText ));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'Name', smText ));
end;

constructor TP3DPoseJoint.CreateFromDOM(ADOMNode: TDOMElement; AAction: TP3DArmatureAction; AFrameIndex: Integer);
begin
  Create( AAction, AFrameIndex );
  LoadFromDOM( ADOMNode );
end;

procedure TP3DPoseJoint.LoadFromDOM(ADOMNode: TDOMElement);
var
  Rot: TQuat;
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
  FArmature:=AValue;
end;

function TP3DJoint.GetTransform: TP3DTransform;
begin
  Result:= TP3DTransform( FTransform.AsValue );
end;
{
procedure TP3DJoint.SetQuaternion(AValue: TQuat);
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
}

procedure TP3DJoint.SetTail(AValue: TVec3);
begin
  if FTail=AValue then Exit;
  FTail:=AValue;
  FLength:= _length( Tail - Transform.Position );
end;

procedure TP3DJoint.UpdateMatrix;
begin
  //FMatrix:= mat4translate( vec4( Position, 1 )) * mat4( mat3( Quaternion ));
  //mat4inverse( Transform.Matrix, FMatrixInv );
end;

procedure TP3DJoint.OnTransformChange(Sender: TObject);
begin
  UpdateMatrix;
end;

{class function TP3DJoint.GetContainer(ALib: TP3DLibrary): TP3DNamedInterfacedPersistentList;
begin
  Result:= ALib.Joints;
end;

function TP3DJoint.GetDataPath: String;
begin
  Result:= 'Joints[ "' + Name + '" ]';
end;}

constructor TP3DJoint.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FTransform:= TP3DStreamableContainer.Create( 'Transform', TP3DTransform, smText );
  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Length', smText ));
  //Properties.Add( TP3DPropAccessVec3.CreateFieldSet( 'Position', @FPosition, @SetPosition, smText ));
  //Properties.Add( TP3DPropAccessQuat.CreateFieldSet( 'Quaternion', @Quaternion, @SetQuaternion, smText ));
  Properties.Add( FTransform, 'Transform' );
  Properties.Add( TP3DPropAccessVec3.CreateFieldSet( 'Tail', @FTail, @SetTail, smText ));
  TP3DTransform.Create( FTransform );
  Transform.OnChange:= @OnTransformChange;
end;

destructor TP3DJoint.Destroy;
begin
  inherited Destroy;
end;
{
procedure TP3DJoint.Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
begin

end;

procedure TP3DJoint.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
begin
  if ( rlfJoints in AFlags ) then
    AList.PushRenderObject( Self, AWorld, nil );
end;
}
{ TP3DArmature }

class function TP3DArmature.GetContainer(ALib: TP3DLibrary): TP3DPropertyAccess;
begin
  Result:= ALib.Properties.GetPropByName( 'Armatures' );
end;

function TP3DArmature.CalculateJointMatrix(AJoint: TP3DPoseJoint; InvMatrix: TMat4): TMat4;
var
  Joint: TP3DPoseJoint;
begin
  Result:= Mat4Identity;
  Joint:= AJoint;
  while Assigned( Joint ) do
    begin
      //Result:= Joint.Transform.Matrix * Result;//( mat4( mat3( Joint.Transform.Quaternion ))) * mat4translate( vec4( Joint.Transform.Position, 1 )) * Result;
//      Result:= Joint.Transform.Matrix * Joint.RestJoint.TransformLocal.Matrix * Result;
      Result:= ( Joint.RestJoint.TransformLocal.Matrix * Joint.Transform.Matrix ) * Result;

      //( mat4translate( vec4( Joint.Transform.Position, 1 )) * mat4( mat3( Joint.Transform.Quaternion ))) *
               //( mat4translate( vec4( Joint.RestJoint.Transform.Position, 1 )) * mat4( mat3( Joint.RestJoint.Transform.Quaternion ))) * Result;
      //Result:= mat4( mat3( Obj.Quaternion )) * mat4translate( vec4( Obj.Position, 1 )) * Result;

      Joint:= TP3DPoseJoint( Joint.GetJointParent );
    end;
  Result:= {InvMatrix * }Result;
  {Modifier:= ObjGetArmatureModifier( AObj );
  if ( Assigned( AObj ) and ( AObj.Data is TP3DJoint )) then
    Result:= Result * TP3DJoint( AObj.Data ).MatrixInv;}
end;

function TP3DArmature.GetObjArmatureModifier(AObj: TP3DObject): TP3DObjectModifierArmature;
var
  Modifier: TP3DObjectModifier;
begin
  Result:= nil;
  for Modifier in AObj.Modifiers do
    if (( Modifier is TP3DObjectModifierArmature ) and ( TP3DObjectModifierArmature( Modifier ).Armature = Self )) then
      begin
        Result:= TP3DObjectModifierArmature( Modifier );
        break;
      end;
end;

constructor TP3DArmature.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FJoints:= TP3DRestJointContainerList.Create( 'Joints', smText );
  FJoints.Armature:= Self;
  FRotations:= TQuatList.Create;
  FTranslations:= TVec3List.Create;
  Properties.Add([ FJoints ]);
  //MaterialModifier:= TP3DMaterialModifierArmature.Create( Self );
end;

destructor TP3DArmature.Destroy;
begin
  FRotations.Free;
  FTranslations.Free;
  //FJoints.Free;
  //MaterialModifier.Free;

  inherited Destroy;
end;

procedure TP3DArmature.Render(world: TMat4; Scene: TP3DScene; RenderObject: TP3DObject);
begin

end;

procedure TP3DArmature.AddToRenderList(ARootScene: TP3DScene; AWorld: TMat4; AList: TP3DRenderList; AFlags: TP3DRenderListFlags);
begin

end;

{ TP3DObjectModifierArmature }

function TP3DObjectModifierArmature.GetArmature: TP3DArmature;
begin
  Result:= TP3DArmature( FArmature.AsValue );
end;

procedure TP3DObjectModifierArmature.SetArmature(AValue: TP3DArmature);
var
  Joint: TP3DPoseJoint;
  i: Integer;
  p: TP3DRestJoint;
begin
  if ( FArmature.AsValue = AValue ) then
    exit;

  FArmature.AsValue:= AValue;
  Pose.Clear;
  if ( Assigned( AValue )) then
  for i:= 0 to Armature.Joints.Count - 1 do begin
    Joint:= TP3DPoseJoint.Create( Pose );
    Joint.RestJoint:= Armature.Joints[ i ];
    //Joint.Transform.Matrix:= Joint.RestJoint.TransformLocal.Matrix;
    Joint.Name:= Joint.RestJoint.Name;
  end;

  for Joint in Pose do begin
    p:= Joint.RestJoint.Parent;
    if ( Assigned( p )) then
      Joint.FParent:= Pose.GetByName( p.Name );
  end;

  UpdateMaterials;
end;

procedure TP3DObjectModifierArmature.SetObj(AValue: TP3DObject);
begin
  inherited SetObj(AValue);
  UpdateMaterials;
end;

constructor TP3DObjectModifierArmature.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FPose:= TP3DPoseJointContainerList.Create( 'Pose', smText );
  FArmature:= TP3DStreamablePointer.Create( 'Armature', TP3DArmature, smText );
  FJointTex:= TP3DStreamablePointer.Create( 'JointTex', TP3DTexture, smText );
  LinesAuto:= TP3DVec3BufferGL.Create;
  Properties.Add([ FPose, FJointTex, FArmature ]);
end;

procedure TP3DObjectModifierArmature.SetJointTex(AValue: TP3DTexture);
begin
  FJointTex.AsValue:=AValue;
  OwnsJointTex:= False;
end;

function TP3DObjectModifierArmature.GetJointTex: TP3DTexture;
begin
  Result:= TP3DTexture( FJointTex.AsValue );
end;

procedure TP3DObjectModifierArmature.UpdateJointTex(ARoot: TP3DObject);
var
  Buffer: TVec4List;
  i, j, w, h, n: Integer;
  m: TMat4;
  q: TQuat;
  t: TVec4;

  Joint: TP3DPoseJoint;


  function getNextPower2( AValue: Integer ): Integer;
  begin
    Result:= 2;
    while Result < AValue do
      Result:= sqr( Result );
  end;

begin
  {if ( Assigned( FJointTex.AsValue )) then
    begin
      if ( OwnsJointTex ) then
        FJointTex.AsValue.Free
      else
        FJointTex.AsValue:= nil;
    end;}

  //rlist:= TP3DRenderList.Create;
  //ARoot.AddToRenderList( nil, Mat4Identity, rlist, [ rlfJoints ]);

  Buffer:= TVec4List.Create;
  w:= getNextPower2( Pose.Count );
  h:= 1;
  for i:= 0 to h - 1 do
    for j:= 0 to w - 1 do
      if ( j >= Pose.Count ) then
        Buffer.AddArray([ vec4( 1, 0, 0, 0 ), vec4( 0, 1, 0, 0 ), vec4( 0, 0, 1, 0 ), vec4( 0, 0, 0, 1 )])
      else
        begin
          //n:= rlist.Find( Joints.Items[ j ], nil );

          Joint:= Pose[ j ];
          //JointObj:= TP3DObject( Joint.Users[ 0 ]);
          case Armature.TransformMethod of
            atmMatrix:
              begin
                //if ( Assigned( JointObj )) then
                  m:= Armature.CalculateJointMatrix( Joint, -( ARoot.CalculateParentMatrix * ARoot.Transform.Matrix )) * Joint.RestJoint.Transform.MatrixInv
                //else
                //  m:= Mat4Identity;
                ;
                //Buffer.AddArray([ m.Col[ 0 ], m.Col[ 1 ], m.Col[ 2 ], m.Col[ 3 ]]);
                Buffer.AddArray([ m.Row[ 0 ], m.Row[ 1 ], m.Row[ 2 ], m.Row[ 3 ]]);
              end;
            {atmDualQuaternion: //only half implemented
              begin
                if ( n > -1 ) then
                  q:= -Joints.NamedItems[ j ].Quaternion * quat( rlist[ n ].Rotations[ 0 ])
                else
                  m:= Mat4Identity;

                Buffer.Add([ q, t ]);
              end;}
          end;
        end;
  ;
  //for i:= 0 to Buffer.Count - 1 do
  //  WriteLn( Buffer[ i ].ToString());
  if ( Assigned( FJointTex.AsValue )) then begin
    JointTex.Bind();
    JointTex.LoadFromBuffer( w * ( 2 + Ord( Armature.TransformMethod = atmMatrix ) * 2 ), h, 4, dtFloat, Buffer.Ptr )
  end else
    FJointTex.AsValue:= TP3DTexture.CreateFromBuffer( w * ( 2 + Ord( Armature.TransformMethod = atmMatrix ) * 2 ), h, 4, dtFloat, Buffer.Ptr, Armature.OwnerLib );
  OwnsJointTex:= True;
  JointTex.MipMap:= False;
  //FJointTex.SaveToFile( '/tmp/jointtex' + FJointTex.Name + '.png' );
  Buffer.Free;
  //rlist.Free;
end;

procedure TP3DObjectModifierArmature.UpdateLines(ARoot: TP3DObject);
  {function GetArmModifier: TP3DObjectModifierArmature;
  var
    m: TP3DObjectModifier;
  begin
    for m in ARoot.Modifiers do
      if (( m is TP3DObjectModifierArmature ) and ( TP3DObjectModifierArmature( m ).Armature = Self )) then begin
        Result:= TP3DObjectModifierArmature( m );
        break;
      end;
  end;}

var
  Joint: TP3DPoseJoint;
  m: TMat4;
  p1, p2: TVec3;
begin
  Lines.Count:= 0; //Joints.Count * 2;

  for Joint in Pose do begin
    //m:= {ARoot.CalculateParentMatrix * ARoot.Transform.Matrix;//} CalculateJointMatrix( Joint, ( ARoot.CalculateParentMatrix * ARoot.Transform.Matrix ));
    m:= Armature.CalculateJointMatrix( Joint, -( ARoot.CalculateParentMatrix * ARoot.Transform.Matrix )) * Joint.RestJoint.Transform.MatrixInv;

    //p1:= vec3( m._30, m._31, m._32 );
    //Joint.Transform;
    //Lines.Add( p1 );
    //Lines.Add( p1 + mat3( m ) * Joint.Tail );
    //p1:= vec3( m._30, m._31, m._32 );
    //p1:= ( m * vec4( vec3( 0 ), 1 )).xyz;
    //p2:= p1 + ( m * vec4( Joint.Tail, 1 )).xyz;
    p1:= Joint.RestJoint.Transform.Position;
    p2:= Joint.RestJoint.Tail; //p1 + ( Joint.RestJoint.Transform.MatrixInv * vec4( Joint.RestJoint.Tail, 1 )).xyz;
    Lines.Add(( m * vec4( p1, 1 )).xyz );
    Lines.Add(( m * vec4( p2, 1 )).xyz );
  end;
  Lines.PushData;
end;

function TP3DObjectModifierArmature.GetSettings: TP3DPlotListSetting;
begin
  Result:= settings([
    extensions([ 'armature' ]),
    uniform( 'jointTexWidth', JointTex.Width ),
    uniform( 'jointTexHeight', JointTex.Height ),
    TP3DPlotCommandUniformTexture.Create( 'jointTex', JointTex )
  ]);
end;


procedure TP3DObjectModifierArmature.UpdateMaterials;
var
  Mesh: TP3DMesh;
  i: Integer;
begin
  if ( Assigned( Obj ) and ( Obj.Data is TP3DMesh )) then
    begin
      Mesh:= TP3DMesh( Obj.Data );
      if ( Mesh.Material is TP3DMaterialBase ) then
        begin
          //TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).Modifiers.Add( TP3DMaterialModifierArmature.Create( Self ));
          TP3DMaterialBase( Mesh.Material ).BuildShaderSubset( Obj.Modifiers, Obj.Owner.GetFullPath );
        end;

      {for i:= 0 to Mesh.MaterialGroups.Count -1 do
        if ( Mesh.MaterialGroups[ i ].Material is TP3DMaterialBase ) then
          begin
            //TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).Modifiers.Add( TP3DMaterialModifierArmature.Create( Self ));
            TP3DMaterialBase( Mesh.MaterialGroups[ i ].Material ).BuildShaderSubset( Obj.Modifiers, Obj.Owner.GetFullPath );
          end;}
      FInitialized:= True;
    end;
end;

procedure TP3DObjectModifierArmature.PassToShader(Material: TP3DMaterial; var world: TMat4);
  function RealMod (x, y : extended) : extended;
  begin
     Result := x - y * Trunc(x/y);
  end;
var
  decl: TP3DShaderDecl;
  joint: TP3DJoint;
  poslist: TVec3List;
  quatList: TQuatList;
  i: Integer;
  quat: TQuat;
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
      //Material.PassToShader();
      //TODO: fix this
      //Armature.UpdateJointTex( TP3DObject( Armature.Users.Items[ 0 ]));
      i:= Material.Maps.Count;
      glActiveTexture( GL_TEXTURE0 + i ); cle( Self );
      glBindTexture( GL_TEXTURE_2D, JointTex.GLTexture ); cle( Self );

      decl:= P3DShaderActive.Uniforms.Names[ 'jointTex' ];
      if ( Assigned( decl )) then
        decl.AsInt:= i;
      decl:= P3DShaderActive.Uniforms.Names[ 'jointTexWidth' ];
      if ( Assigned( decl )) then
        decl.AsInt:= JointTex.Width;
      decl:= P3DShaderActive.Uniforms.Names[ 'jointTexHeight' ];
      if ( Assigned( decl )) then
        decl.AsInt:= JointTex.Height;
      decl:= P3DShaderActive.Uniforms.Names[ 'to_arm' ];
      if ( Assigned( decl )) then
        decl.AsMat4:= Mat4Identity;
      //Armature.CurrentAction.CurrentFrame:= SDL_GetTicks() / 100;
      //P3DApplication.MainWindow.Title:= FloatToStr( Armature.CurrentAction.CurrentFrame ) + ':' + FloatToStr( frac( Armature.CurrentAction.CurrentFrame / Armature.CurrentAction.Frames.Count ));
      decl:= P3DShaderActive.Uniforms.Names[ 'time' ];
      if ( Assigned( decl )) then
        decl.AsFloat:= 0;//frac( Armature.CurrentAction.CurrentFrame / Armature.CurrentAction.Frames.Count );
    end;
end;

procedure TP3DObjectModifierArmature.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
begin
  if ( Assigned( Armature )) then
    Armature.MaterialModifier.AddPass( ShaderTree, Material );
end;


{ TP3DMaterialModifierArmature }

constructor TP3DMaterialModifierArmature.Create(AArmature: TP3DArmature);
begin
  inherited Create();
  Armature:= AArmature;
end;

procedure TP3DMaterialModifierArmature.AddPass(ShaderTree: TP3DShaderNodeTree; Material: TP3DMaterialBase);
  function ChangeSocketValueInt( Node: TP3DNodeClone; Name: String; Value: Integer ): TP3DNodeClone;
  var
    Socket: TP3DNodeSocketClone;
  begin
    Socket:= Node.FindInput( Name );
    if ( not Assigned( Socket )) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    Socket.ValueProp.AsString:= IntToStr( Value );
    Result:= Node;
  end;
begin
  if ( Assigned( Armature )) then
    begin
      ChangeSocketValueInt( ShaderTree.AddCopy( '00025_pass_armature_matrix' ), 'numJoints', Armature.Joints.Count );
      //ChangeSocketValueInt( ShaderTree.AddCopy( '00025_pass_armature' ), 'numJoints', TP3DMeshModifierArmature( MeshModifier ).Armature.Joints.Count );
      ShaderTree.AddCopy( '01050_pass_armature_debug' );
    end;
end;

procedure TP3DMaterialModifierArmature.PassToShader(Material: TP3DMaterialBase);
begin

end;

class function TP3DMaterialModifierArmature.GetIndex: Integer;
begin
  Result:= 25;
end;

{ TP3DPoseJoint }

procedure TP3DPoseJoint.OnChildrenChange(Sender: TObject; Item: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  if ( Action = actAdd ) then
    TP3DPoseJoint( Item ).Parent:= Self
  else
    TP3DPoseJoint( Item ).Parent:= nil;
end;

procedure TP3DPoseJoint.SetParent(AValue: TP3DPoseJoint);
begin
  if FParent=AValue then Exit;
  FParent:=AValue;
end;

constructor TP3DPoseJoint.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FChildren:= TP3DPoseJointPointerList.Create( 'Children', smText );
  Properties.Add( FChildren );
  FChildren.OnChange:= @OnChildrenChange;
end;

function TP3DPoseJoint.GetJointParent: TP3DJoint;
begin
  Result:= Parent;
end;

{ TP3DRestJoint }

procedure TP3DRestJoint.SetArmature(AValue: TP3DArmature);
begin
  if FArmature=AValue then Exit;

  if ( Assigned( Armature )) then
    Armature.Joints.Remove( Self );
  FArmature:=AValue;

  if ( Assigned( Armature )) then
    Armature.Joints.Add( Self );
end;

procedure TP3DRestJoint.SetParent(AValue: TP3DRestJoint);
begin
  FParent.AsValue:= AValue;
end;

function TP3DRestJoint.GetTransformLocal: TP3DTransform;
begin
  Result:= TP3DTransform( FTransformLocal.AsValue );
end;

function TP3DRestJoint.GetParent: TP3DRestJoint;
begin
  Result:= TP3DRestJoint( FParent.AsValue );
end;

constructor TP3DRestJoint.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FChildren:= TP3DRestJointPointerList.Create( 'Children', smText );
  FParent:= TP3DStreamablePointer.Create( 'Parent', TP3DRestJoint, smText );
  FTransformLocal:= TP3DStreamableContainer.Create( 'TransformLocal', TP3DTransform, smText );

  Properties.Add( FTransformLocal, 'Joint' );
  Properties.Add( FParent, 'Joint' );
  Properties.Add( FChildren, 'Joint' );

  TP3DTransform.Create( FTransformLocal );

  //FTransformLocal.OnChange:= @OnTransformChange;
end;



destructor TP3DRestJoint.Destroy;
begin
  inherited Destroy;
end;

function TP3DRestJoint.GetJointParent: TP3DJoint;
begin
  Result:= Parent;
end;


{$ENDIF}

{$IFDEF INITIALIZATION}
  P3DMeshModifierClassFactory.Add( TP3DMeshModifierArmature );
{$ENDIF}

