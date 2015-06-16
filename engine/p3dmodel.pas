unit p3dmodel;

{$mode objfpc}{$H+}

{$DEFINE VERBOSE}
{$DEFINE BUFFERS}

interface
uses
  Classes, SysUtils, dglOpenGL, Math, p3dMath, strutils, p3dshaders, p3dtexture,
  p3dgeometry, LCLIntf, p3dfilewatch, p3dobjects, p3dbuffers, DOM, XMLRead;

type

  TRenderFlag = ( rfShadowMap, rfWireFrame, rfDebugShowLocation, rfDebugShowBoundingBox, rfDebugShowArmature );
  TRenderFlags = set of TRenderFlag;

  { TMaterial }
  TP3DMaterialMap = record
    Map: TP3DTexture;
    DiffuseFactor: Single;
    NormalFactor: Single;
  end;

  TP3DMesh = class;
  TP3DScene = class;

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
    ShaderShadow: TShader;

    constructor Create;
    procedure LoadFromDOM(DOM: TDOMElement; Scene: TP3DScene);
  end;

  TP3DFaceVertex = record
    v, n, t, b: Integer;
    texc: array of Integer;
  end;

  TP3DFace = record
    verts: array of TP3DFaceVertex;
    mat: TP3DMaterial;
  end;

  PP3DFaceArray = ^TP3DFaceArray;
  TP3DFaceArray = array of TP3DFace;
//  TMaterialArray = array of TMaterial;

  { TBone }

  {$MACRO ON}
{  {$DEFINE TCustomList:= TCustomModelList}
  {$DEFINE TCustomListEnumerator:= TModelEnumerator}
  {$DEFINE TCustomItem:= TModel}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}}

  {$DEFINE TCustomList:= TP3DCustomMaterialList}
  {$DEFINE TCustomListEnumerator:= TP3DMaterialEnumerator}
  {$DEFINE TCustomItem:= TP3DMaterial}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}


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
      procedure Clear; override;
  end;

  { TRenderableObjectList }

  TRenderableObjectList = class( TObjectList )
    public
      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []);
  end;

  TBoundingBox = record
    Min, Max, Center: TVec3;
  end;

  { TRenderableObject }

  TRenderableObject = class ( TBaseObject )
    private
      FChildren: TRenderableObjectList;
      FVisible: Boolean;

    public
      constructor Create( AParentList: TObjectList );
      destructor Destroy; override;
      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []); virtual;

    published
      property Children: TRenderableObjectList read FChildren;
      property Visible: Boolean read FVisible write FVisible;
  end;

 {$DEFINE INTERFACE}
 {$INCLUDE p3dmodel_mesh.inc}
 {$INCLUDE p3dmodel_scene.inc}

 {$UNDEF INTERFACE}

implementation


{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dmodel_mesh.inc}
{$INCLUDE p3dmodel_scene.inc}

{ TMaterial }

constructor TP3DMaterial.Create;
begin
  inherited;
  NumMaps:= 0;
//  Diff_Map:= -1;
end;

procedure TP3DMaterial.LoadFromDOM(DOM: TDOMElement; Scene: TP3DScene);
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
      else
        raise Exception.Create( 'Unknown tag inside Material Element: '+ tex.NodeName );
      end;
    tex:= TDOMElement( tex.NextSibling );
  end;
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

procedure TP3DMaterialList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

constructor TRenderableObject.Create( AParentList: TObjectList );
begin
  inherited;
  FChildren:= TRenderableObjectList.Create;
end;

destructor TRenderableObject.Destroy;
begin
  FChildren.Clear;
  FChildren.Free;
  inherited Destroy;
end;

procedure TRenderableObject.Render(world: TMat4; const RenderFlag: TRenderFlags);
begin
  Children.Render( world, RenderFlag );
end;

{ TRenderableObjectList }

procedure TRenderableObjectList.Render(world: TMat4; const RenderFlag: TRenderFlags = []);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    if ( Items[ i ] is TRenderableObject ) then
      if ( TRenderableObject( Items[ i ]).Visible ) then
        TRenderableObject( Items[ i ]).Render( world, RenderFlag );
end;




{{$DEFINE TCustomList:= TCustomModelList}
{$DEFINE TCustomListEnumerator:= TModelEnumerator}
{$DEFINE TCustomItem:= TModel}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}}

{$DEFINE TCustomList:= TP3DCustomMaterialList}
{$DEFINE TCustomListEnumerator:= TP3DMaterialEnumerator}
{$DEFINE TCustomItem:= TP3DMaterial}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}
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

