unit p3dmodel;

{$mode objfpc}{$H+}

{$DEFINE VERBOSE}
{$DEFINE BUFFERS}

interface
uses
  Classes, SysUtils, dglOpenGL, Math, p3dMath, p3dshaders, p3dshadernodes, p3dtexture,
  p3dgeometry, LCLIntf, p3dfilewatch, p3dobjects, p3dbuffers, DOM, XMLRead, p3dgenerics, p3dscene;

type

  TP3DRenderFlag = ( rfShadowMap, rfWireFrame, rfDebugShowLocation, rfDebugShowBoundingBox, rfDebugShowArmature );
  TP3DRenderFlags = set of TP3DRenderFlag;

  { TP3DMaterialMap }

  TP3DMapMode = ( p3dmmMix, p3dmmMultiply, p3dmmAdd, p3dmmSubtract );
  TP3DMaterialMap = record
    Map: TP3DTexture;
    DiffuseFactor: Single;
    NormalFactor: Single;
    SpecularFactor: Single;
    TexChannel: Integer;
    Mode: TP3DMapMode;
  end;

  TP3DMesh = class;
  TP3DModelScene = class;

  { TP3DMaterial }

  TP3DMaterial = class
    Maps: array [ 0..7 ] of TP3DMaterialMap;
    NumMaps: Integer;
    Diff: TVec3;
    Spec: TVec3;
    Spec_Hardness: Single;
    alpha: Real;
    Name: String;
    Shader: TShader;

    procedure BuildShader;

    constructor Create;
    destructor Destroy; override;
    procedure LoadFromDOM(DOM: TDOMElement; Scene: TP3DModelScene );
  end;

  { TP3DMaterialGroup }

  TP3DMaterialGroup = class
    PolyStart, PolyEnd,
      IndexStart, IndexEnd: Integer;
    Material: TP3DMaterial;
  end;

  TP3DMaterialGroupList = specialize TP3DCustomObjectList < TP3DMaterialGroup >;

  TP3DFaceVertex = record
    v, n, t, c: Integer;
    texc: array of Integer;
  end;

  TP3DFace = record
    verts: array of TP3DFaceVertex;
    mat: TP3DMaterial;
  end;

  PP3DFaceArray = ^TP3DFaceArray;
  TP3DFaceArray = array of TP3DFace;

  { TBone }

  {$MACRO ON}
{  {$DEFINE TCustomList:= TCustomModelList}
  {$DEFINE TCustomListEnumerator:= TModelEnumerator}
  {$DEFINE TCustomItem:= TModel}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}}

  TP3DCustomMaterialList = specialize gP3DCustomObjectList < TP3DMaterial >;


  {TBone = class;

  {$DEFINE TCustomList:= TCustomBoneList}
  {$DEFINE TCustomListEnumerator:= TBoneEnumerator}
  {$DEFINE TCustomItem:= TBone}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  TFrame = class;

  {$DEFINE TCustomList:= TCustomFrameList}
  {$DEFINE TCustomListEnumerator:= TFrameEnumerator}
  {$DEFINE TCustomItem:= TFrame}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  TArmatureAction = class;

  {$DEFINE TCustomList:= TCustomActionList}
  {$DEFINE TCustomListEnumerator:= TActionEnumerator}
  {$DEFINE TCustomItem:= TArmatureAction}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TBoneList }

  TBoneList = class( TCustomBoneList )
    public
      function FindByName( Name: String ): Integer;
      procedure Clear; override;
  end;

  { TFrameList }

  TFrameList = class( TCustomFrameList )
    public
      procedure Clear; override;
  end;

  { TActionList }

  TActionList = class( TCustomActionList )
    public
      procedure Clear; override;
  end;}

  { TMaterialList }

  TP3DMaterialList = class( TP3DCustomMaterialList )
    public
      function FindByName( Name: String ): Integer;
  end;

  TP3DRenderableObject = class;
  { TP3DRenderableObjectList }

  TP3DRenderableObjectList = class( TP3DObjectList )
    public
      procedure Render( world: TMat4; Scene: TP3DModelScene );
      function OutputDebugInfo: String;
  end;

  TP3DBoundingBox = record
    Min, Max, Center: TVec3;
  end;

  { TP3DRenderableObject }

  TP3DRenderableObject = class ( TP3DObject )
    private
      FChildren: TP3DRenderableObjectList;
      FVisible: Boolean;

    public
      constructor Create( AParentList: TP3DObjectList );

      destructor Destroy; override;
      procedure Render( world: TMat4; Scene: TP3DModelScene ); virtual;

    published
      property Children: TP3DRenderableObjectList read FChildren;
      property Visible: Boolean read FVisible write FVisible;
  end;

 {$DEFINE INTERFACE}
 {$INCLUDE p3dmodel_mesh.inc}
 {$INCLUDE p3dmodel_lighting.inc}
 {$INCLUDE p3dmodel_scene.inc}

 {$UNDEF INTERFACE}

implementation


{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dmodel_mesh.inc}
{$INCLUDE p3dmodel_lighting.inc}
{$INCLUDE p3dmodel_scene.inc}

{ TMaterial }

procedure TP3DMaterial.BuildShader;
var
  ShaderTree: TP3DShaderNodeTree;
  Node: TP3DShaderNode;
  Compiled: TP3DShaderCompiled;
  i: Integer;

  function AddNode( Name: String ): TP3DShaderNode;
  begin
    Node:= P3DShaderLib.FindNode( Name );
    if ( not Assigned( Node )) then
      raise Exception.Create( 'Cannot build shader: Node "' + Name + '" not found!' );
    Result:= Node.MakeCopy;
    ShaderTree.Nodes.Add( Result );
  end;

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

  function ChangeSocketValueFloat( Node: TP3DShaderNode; Name: String; Value: Single ): TP3DShaderNode;
  var
    Socket: Integer;
  begin
    Socket:= Node.Inputs.FindSocketByName( Name );
    if ( Socket < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Name + '" not found!' );
    TP3DShaderNodeSocketFloat( Node.Inputs[ Socket ]).Value:= Value;
    Result:= Node;
  end;

  function ConnectSocket( Node1: TP3DShaderNode; Node2: TP3DShaderNode; Socket1: String; Socket2: String ): TP3DShaderNode;
  var
    Socket1n, Socket2n: Integer;
  begin
    Socket1n:= Node1.Inputs.FindSocketByName( Socket1 );
    Socket2n:= Node2.Outputs.FindSocketByName( Socket2 );
    if ( Socket1n < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Socket1 + '" not found!' );
    if ( Socket2n < 0 ) then
      raise Exception.Create( 'Cannot build shader: Socket "' + Socket2 + '" not found!' );
    Node1.Inputs[ Socket1n ].Connected:= Node2.Outputs[ Socket2n ];
    Result:= Node1;
  end;

const Mode: array [ p3dmmMix .. p3dmmSubtract ] of String = ( 'mix', 'multiply', 'add', 'subtract' );

begin
  ShaderTree:= TP3DShaderNodeTree.Create;

  try

  AddNode( 'lib_maps' );

  AddNode( 'Pass_Init' );

  AddNode( 'lib_lighting' );


  for i:= 0 to 7 do
    ChangeSocketValueInt( ChangeSocketValueInt( AddNode( 'TC_Init' ), 'Location', i + 8 ), 'TexCoordIndex', i );
  for i:= 0 to NumMaps - 1 do
    begin
      ChangeSocketValueInt( ChangeSocketValueInt( AddNode( 'Pass_Read' ), 'Index', i ), 'TexCoordIndex', Maps[ i ].TexChannel );
      if ( Maps[ i ].DiffuseFactor > 0 ) then
        ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( AddNode( 'Pass_Diffuse' ), 'Index', i ), 'Factor', Maps[ i ].DiffuseFactor ),
        AddNode( 'Pass_Combine' ), 'Mode', Mode[ Maps[ i ].Mode ]);
      if ( Maps[ i ].NormalFactor > 0 ) then
        ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( AddNode( 'Pass_Normal' ), 'Index', i ), 'Factor', Maps[ i ].NormalFactor ),
        AddNode( 'Pass_Combine' ), 'Mode', Mode[ Maps[ i ].Mode ]);
      if ( Maps[ i ].SpecularFactor > 0 ) then
         ConnectSocket( ChangeSocketValueFloat( ChangeSocketValueInt( AddNode( 'Pass_Specular' ), 'Index', i ), 'Factor', Maps[ i ].SpecularFactor ),
         AddNode( 'Pass_Combine' ), 'Mode', Mode[ Maps[ i ].Mode ]);
    end;

  AddNode( 'Pass_Mix' );
  AddNode( 'Lighting' );
  AddNode( 'Pass_Final' );


  Compiled:= ShaderTree.Compile;
  //WriteLn( 'Material: ' + Name );
  //for i:= 0 to NumMaps - 1 do
  //  WriteLn( '  Map: ' + Maps[ i ].Map.FileName + ' Diffuse: ', Maps[ i ].DiffuseFactor, ' Normal: ', Maps[ i ].NormalFactor );
  //WriteLn( 'vshader: ' + Compiled.FindBuffer( 'vshader' ).Code );
  //WriteLn( 'fshader: ' + Compiled.FindBuffer( 'fshader' ).Code );
  if ( Assigned( Shader )) then
    Shader.Free;
  Shader:= p3dshaders.CreateVertexAndFragmentShader( Compiled.FindBuffer( 'vshader' ).Code, Compiled.FindBuffer( 'fshader' ).Code );
  finally
    ShaderTree.Free;
    Compiled.Free;
  end;
end;

constructor TP3DMaterial.Create;
begin
  inherited;
  NumMaps:= 0;
//  Diff_Map:= -1;
end;

destructor TP3DMaterial.Destroy;
begin
  if ( Assigned( Shader )) then
    Shader.Free;
  inherited Destroy;
end;

procedure TP3DMaterial.LoadFromDOM(DOM: TDOMElement; Scene: TP3DModelScene);
var
  SpecTmp: TVec4;
  tex: TDOMElement;

  procedure LoadTex;
  var
    TexName: String;
    tx: TP3DTexture;
  begin
    if ( NumMaps = 8 ) then
      raise Exception.Create( 'Maximum Number of Maps reached: 8' );
    Maps[ NumMaps ].DiffuseFactor:= StrToFloatDef( tex.GetAttribute( 'diffuse' ), 0.0 );
    Maps[ NumMaps ].NormalFactor:= StrToFloatDef( tex.GetAttribute( 'normal' ), 0.0 );
    Maps[ NumMaps ].SpecularFactor:= StrToFloatDef( tex.GetAttribute( 'specular' ), 0.0 );
    Maps[ NumMaps ].TexChannel:= StrToIntDef( tex.GetAttribute( 'layer' ), 0 );
    case tex.GetAttribute( 'mode' ) of
      'add': Maps[ NumMaps ].Mode:= p3dmmAdd;
      'multiply': Maps[ NumMaps ].Mode:= p3dmmMultiply;
      'subtract': Maps[ NumMaps ].Mode:= p3dmmSubtract;
      else
        Maps[ NumMaps ].Mode:= p3dmmMix;
    end;
    TexName:= tex.GetAttribute( 'file' );
    if ( not FileExists( TexName )) then
      raise Exception.Create( 'Error: The specified texture "' + TexName + '" could not be found!' )
    else
      begin
        tx:= TP3DTexture.Create( TexName );
        if ( Assigned( tx )) then
          Maps[ NumMaps ].Map:= tx
        else
          begin
            WriteLn( 'Error: An unknown error occured while loading texture "' + TexName + '"!' );
            exit;
          end;
      end;
    Inc( NumMaps );
  end;

begin
  Name:= DOM.GetAttribute( 'name' );
  Diff.FromString( DOM.GetAttribute( 'diffuse' ));
  SpecTmp.FromString( DOM.GetAttribute( 'specular' ));
  Spec:= SpecTmp.YZW;
  Spec_Hardness:= SpecTmp.X;
  tex:= TDOMElement( DOM.FirstChild );
  while Assigned( tex ) do
    begin
      case tex.NodeName of
        'texture': LoadTex;
        '#comment':;
      else
        raise Exception.Create( 'Unknown tag inside Material Element: '+ tex.NodeName );
      end;
    tex:= TDOMElement( tex.NextSibling );
  end;
  BuildShader;
end;

{ TMaterialList }

function TP3DMaterialList.FindByName(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = Name ) then
      begin
        Result:= i;
        break;
      end;
end;

constructor TP3DRenderableObject.Create( AParentList: TP3DObjectList );
begin
  inherited;
  FChildren:= TP3DRenderableObjectList.Create;
end;


destructor TP3DRenderableObject.Destroy;
begin
  FChildren.Clear;
  FChildren.Free;
  inherited Destroy;
end;

procedure TP3DRenderableObject.Render( world: TMat4; Scene: TP3DModelScene );
begin
  Children.Render( world, Scene );
end;

{ TP3DRenderableObjectList }

procedure TP3DRenderableObjectList.Render( world: TMat4; Scene: TP3DModelScene );
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ] is TP3DRenderableObject ) then
      if ( TP3DRenderableObject( Items[ i ]).Visible ) then
        TP3DRenderableObject( Items[ i ]).Render( world, Scene );
end;

function TP3DRenderableObjectList.OutputDebugInfo: String;
var
  Item: TP3DObject;
begin
  Result:= 'Debug information for Object List';
  for Item in Self do
    Result+= Format( 'Name: "%s" Class "%s" Visible %s'+LineEnding, [ Item.Name, Item.ClassName, BoolToStr( TP3DRenderableObject( Item ).Visible, 'Yes', 'No' )]);
end;




{{$DEFINE TCustomList:= TCustomModelList}
{$DEFINE TCustomListEnumerator:= TModelEnumerator}
{$DEFINE TCustomItem:= TModel}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}}

{
{$DEFINE TCustomList:= TCustomBoneList}
{$DEFINE TCustomListEnumerator:= TBoneEnumerator}
{$DEFINE TCustomItem:= TBone}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TCustomFrameList}
{$DEFINE TCustomListEnumerator:= TFrameEnumerator}
{$DEFINE TCustomItem:= TFrame}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TCustomActionList}
{$DEFINE TCustomListEnumerator:= TActionEnumerator}
{$DEFINE TCustomItem:= TArmatureAction}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}
}

{$UnDef IMPLEMENTATION}


end.

