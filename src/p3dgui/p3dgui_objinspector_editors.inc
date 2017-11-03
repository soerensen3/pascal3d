//p3dgui_objinspector_editors.inc
//part of p3dgui.pas


{$IFDEF INTERFACE}
type
  { TP3DOIEditorObject }

  TP3DOIEditorObject = class ( TP3DOIEditorGroup )
    private
      FCollisionEdit: TP3DPropertyEditorEnum;
      FNameEdit: TP3DPropertyEditorString;
      FTypeLbl: TP3DLabel;
      FVisibleCheckBox: TP3DPropertyEditorBoolean;

    public
      procedure Update; override;
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

    published
      property NameEdit: TP3DPropertyEditorString read FNameEdit write FNameEdit;
      property CollisionEdit: TP3DPropertyEditorEnum read FCollisionEdit write FCollisionEdit;
      property VisibleCheckBox: TP3DPropertyEditorBoolean read FVisibleCheckBox write FVisibleCheckBox;
      property TypeLbl: TP3DLabel read FTypeLbl write FTypeLbl;
  end;

  { TP3DOIEditorTransform3D }

  TP3DOIEditorTransform3D = class ( TP3DOIEditorGroup )
    private
      FPositionEdit: TP3DVectorEdit;
      FQuaternionEdit: TP3DPropertyEditorVec4;
      FRotationEdit: TP3DVectorEdit;
      FRotationMethodCombo: TP3DComboBox;
      FScaleEdit: TP3DVectorEdit;

      function EdGetValue( Sender: TP3DVectorEdit ): TVec4;
      procedure EdSetValue( Sender: TP3DVectorEdit; AValue: TVec4 );
      procedure ComboChange( Sender: TP3DComboBox; var AValue: String );

    public
      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property PositionEdit: TP3DVectorEdit read FPositionEdit write FPositionEdit;
      property RotationEdit: TP3DVectorEdit read FRotationEdit write FRotationEdit;
      property RotationMethodCombo: TP3DComboBox read FRotationMethodCombo write FRotationMethodCombo;
      property QuaternionEdit: TP3DPropertyEditorVec4 read FQuaternionEdit write FQuaternionEdit;
      property ScaleEdit: TP3DVectorEdit read FScaleEdit write FScaleEdit;
  end;

  { TP3DOIEditorDatablock }

  TP3DOIEditorDatablock = class ( TP3DOIEditorGroup )
    private
      FDataEd: TP3DPropertyEditorInterfacedPersistent;
      FNameEd: TP3DLabel;
      FTypeLbl: TP3DLabel;
      FDataProp: TP3DPropertyAccessDataBlock;

    public
      procedure Update; override;
      procedure DataDragDrop(Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean);
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

    published
      property NameEd: TP3DLabel read FNameEd write FNameEd;
      property TypeLbl: TP3DLabel read FTypeLbl write FTypeLbl;
      property DataEd: TP3DPropertyEditorInterfacedPersistent read FDataEd write FDataEd;
  end;

  { TP3DOIEditorPageObject }

  TP3DOIEditorPageObject = class ( TP3DOIEditorPage )
    private
      FEditorData: TP3DOIEditorDatablock;
      FEditorObject: TP3DOIEditorObject;
      FEditorTransform: TP3DOIEditorTransform3D;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; override;

    published
      property EditorObject: TP3DOIEditorObject read FEditorObject write FEditorObject;
      property EditorTransform: TP3DOIEditorTransform3D read FEditorTransform write FEditorTransform;
      property EditorData: TP3DOIEditorDatablock read FEditorData write FEditorData;
  end;

  { TP3DOIEditorPageGrid }

  TP3DOIEditorPageGrid = class ( TP3DOIEditorPage )
    private
      FGridHeightEd: TP3DPropertyEditorInt;
      FGridWidthEd: TP3DPropertyEditorInt;
      FGridWorldUnitEd: TP3DPropertyEditorFloat;
      //FMeshList: TP3DPropertyEditorDataBlockList;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; override;
      procedure OnMeshSelect( Sender: TP3DListView; var NewSelection: TP3DListItem );

    published
      //property MeshList: TP3DPropertyEditorDataBlockList read FMeshList write FMeshList;
      property GridWidthEd: TP3DPropertyEditorInt read FGridWidthEd write FGridWidthEd;
      property GridHeightEd: TP3DPropertyEditorInt read FGridHeightEd write FGridHeightEd;
      property GridWorldUnitEd: TP3DPropertyEditorFloat read FGridWorldUnitEd write FGridWorldUnitEd;
  end;


  { TP3DOIEditorDataLight }

  TP3DOIEditorDataLight = class ( TP3DOIEditorGroup )
    private
      FLight: TP3DLight;
      FLightColorCombo: TP3DColorRGBAComboBox;
      FLightEnergyEdit: TP3DPropertyEditorFloat;
      FLightLinearAttenuationEdit: TP3DEventValueEdit;
      FLightQuadraticAttenuationEdit: TP3DEventValueEdit;
      FLightType: TP3DComboBox;

      procedure ComboChange( Sender: TP3DComboBox; var AValue: String );
      function GetLightColor( Sender: TP3DColorRGBAPicker ): TP3DColorRGBA;
      procedure SetLightColor( Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA );
      function GetLightAttenuation(Sender: TP3DEventValueEdit): Float;
      procedure SetLightAttenuation(Sender: TP3DEventValueEdit; AValue: Float);

    public
      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property LightTypeCombo: TP3DComboBox read FLightType write FLightType;
      property LightColorCombo: TP3DColorRGBAComboBox read FLightColorCombo write FLightColorCombo;
      property LightLinearAttenuationEdit: TP3DEventValueEdit read FLightLinearAttenuationEdit write FLightLinearAttenuationEdit;
      property LightQuadraticAttenuationEdit: TP3DEventValueEdit read FLightQuadraticAttenuationEdit write FLightQuadraticAttenuationEdit;
      property LightEnergyEdit: TP3DPropertyEditorFloat read FLightEnergyEdit write FLightEnergyEdit;
      property Light: TP3DLight read FLight write FLight;
  end;

  { TP3DOIEditorPageLight }

  TP3DOIEditorPageLight = class ( TP3DOIEditorPage )
    private
      FEditorLight: TP3DOIEditorDataLight;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; override;

    published
      property EditorLight: TP3DOIEditorDataLight read FEditorLight write FEditorLight;
  end;
{
  property FOV: Single read FFOV write FFOV;
  property IsIsometric: Boolean read FIsIsometric write FIsIsometric;

published
  property Handedness: TP3DCameraHandedness read fHandedness write fHandedness;
  property Near: Single read FNear write FNear;
  property Far: Single read FFar write FFar;
  property Aspect: Single read FAspect write FAspect;
  property IsoZoom: Single read FIsoZoom write FIsoZoom;}

  { TP3DOIEditorDataLight }

  { TP3DOIEditorDataCamera }

  TP3DOIEditorDataCamera = class ( TP3DOIEditorGroup )
    private
      FCamera: TP3DCamera;
      FCamHandednessCombo: TP3DComboBox;
      FCamTypeCombo: TP3DComboBox;
      FClippingEdit: TP3DVectorEdit;
      FZoomEdit: TP3DEventValueEdit;
      procedure ComboChange( Sender: TP3DComboBox; var AValue: String );
      function GetCamParamV( Sender: TP3DVectorEdit ): TVec4;
      procedure SetCamParamV( Sender: TP3DVectorEdit; AValue: TVec4 );
      function GetCamParamF(Sender: TP3DEventValueEdit): Float;
      procedure SetCamParamF(Sender: TP3DEventValueEdit; AValue: Float);
      function GetEdValueName( Sender: TP3DEventValueEdit ): String;

    public
      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property HandednessCombo: TP3DComboBox read FCamHandednessCombo write FCamHandednessCombo;
      property CamTypeCombo: TP3DComboBox read FCamTypeCombo write FCamTypeCombo;
      property ClippingEdit: TP3DVectorEdit read FClippingEdit write FClippingEdit;
      property ZoomEdit: TP3DEventValueEdit read FZoomEdit write FZoomEdit;
      property Camera: TP3DCamera read FCamera write FCamera;
  end;

  { TP3DOIEditorPageCamera }

  TP3DOIEditorPageCamera = class ( TP3DOIEditorPage )
    private
      FEditorCamera: TP3DOIEditorDataCamera;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; override;

    published
      property EditorCamera: TP3DOIEditorDataCamera read FEditorCamera write FEditorCamera;
  end;

  { TP3DOIEditorDataTexture }

  TP3DOIEditorDataTexture = class ( TP3DOIEditorGroup )
    private
      FDimensionsLbl: TP3DLabel;
      FFileNameLbl: TP3DLabel;
      FFilteringCombo: TP3DComboBox;
      FFilteringMipCombo: TP3DComboBox;
      FMipMappingCombo: TP3DCheckBox;
      FPreviewImg: TP3DImage;

      procedure ComboChange( Sender: TP3DComboBox; var AValue: String );
      procedure CheckboxChange( Sender: TP3DGraphicControl );

    public
      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property PreviewImg: TP3DImage read FPreviewImg write FPreviewImg;
      property FilteringCombo: TP3DComboBox read FFilteringCombo write FFilteringCombo;
      property FilteringMipCombo: TP3DComboBox read FFilteringMipCombo write FFilteringMipCombo;
      property MipMappingCombo: TP3DCheckBox read FMipMappingCombo write FMipMappingCombo;
      property DimensionsLbl: TP3DLabel read FDimensionsLbl write FDimensionsLbl;
      property FileNameLbl: TP3DLabel read FFileNameLbl write FFileNameLbl;
  end;

  { TP3DOIEditorDataMaterialBase }

  TP3DOIEditorDataMaterialBase = class ( TP3DOIEditorGroup )
    private
      FAlphaEdit: TP3DEventValueEdit;
      FAlphaTreshHoldEdit: TP3DPropertyEditorFloat;
      FDiffCombo: TP3DColorRGBAComboBox;
      FSpecCombo: TP3DColorRGBAComboBox;
      FSpec_HardnessEdit: TP3DEventValueEdit;
      FTranspModeCombo: TP3DPropertyEditorEnum;
      FUnlitEdit: TP3DCheckBox;
      FUseVertexColorEdit: TP3DCheckBox;

      procedure CheckboxChange( Sender: TP3DGraphicControl );
      function GetMatColorParam( Sender: TP3DColorRGBAPicker ): TP3DColorRGBA;
      procedure SetMatColorParam( Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA );
      function GetMatFloatParam(Sender: TP3DEventValueEdit): Float;
      procedure SetMatFloatParam(Sender: TP3DEventValueEdit; AValue: Float);

    public
      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

    published
      property DiffCombo: TP3DColorRGBAComboBox read FDiffCombo write FDiffCombo;
      property SpecCombo: TP3DColorRGBAComboBox read FSpecCombo write FSpecCombo;
      property Spec_HardnessEdit: TP3DEventValueEdit read FSpec_HardnessEdit write FSpec_HardnessEdit;
      property AlphaEdit: TP3DEventValueEdit read FAlphaEdit write FAlphaEdit;
      property UseVertexColorEdit: TP3DCheckBox read FUseVertexColorEdit write FUseVertexColorEdit;
      property UnlitEdit: TP3DCheckBox read FUnlitEdit write FUnlitEdit;
      property TranspModeCombo: TP3DPropertyEditorEnum read FTranspModeCombo write FTranspModeCombo;
      property AlphaTreshHoldEdit: TP3DPropertyEditorFloat read FAlphaTreshHoldEdit write FAlphaTreshHoldEdit;
  end;

  { TP3DOIEditorDataMaterialMap }

  TP3DOIEditorDataMaterialMap = class ( TP3DOIEditorGroup )
    private
      FAlphaEdit: TP3DEventValueEdit;
      FDiffuseEdit: TP3DEventValueEdit;
      FDisplaceEdit: TP3DEventValueEdit;
      FInfluence: TP3DGraphicControl;
      FMap: TP3DMaterialMap;
      FModeCombo: TP3DComboBox;
      FNormalEdit: TP3DEventValueEdit;
      FSpecularEdit: TP3DEventValueEdit;
      //FTextureEditor: TP3DOIEditorDataTexture;

      procedure SetMap(AValue: TP3DMaterialMap);
      //property Mapping: TP3DGraphicControl read FMapping write FMapping;
      property Influence: TP3DGraphicControl read FInfluence write FInfluence;
      function GetFloatParam(Sender: TP3DEventValueEdit): Float;
      procedure SetFloatParam(Sender: TP3DEventValueEdit; AValue: Float);
      procedure ComboChange( Sender: TP3DComboBox; var AValue: String );

    public
      procedure Update; override;
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      class function IsCompatible(AObj: TInterfacedPersistent): Boolean; override;

    published
      //property TextureEditor: TP3DOIEditorDataTexture read FTextureEditor write FTextureEditor;
      property Map: TP3DMaterialMap read FMap write SetMap;

      property DiffuseEdit: TP3DEventValueEdit read FDiffuseEdit write FDiffuseEdit;
      property SpecularEdit: TP3DEventValueEdit read FSpecularEdit write FSpecularEdit;
      property NormalEdit: TP3DEventValueEdit read FNormalEdit write FNormalEdit;
      property AlphaEdit: TP3DEventValueEdit read FAlphaEdit write FAlphaEdit;
      property DisplaceEdit: TP3DEventValueEdit read FDisplaceEdit write FDisplaceEdit;
      property ModeCombo: TP3DComboBox read FModeCombo write FModeCombo;
  end;

  {
  property DiffuseFactor: Single read FDiffuseFactor write FDiffuseFactor;
  property NormalFactor: Single read FNormalFactor write FNormalFactor;
  property SpecularFactor: Single read FSpecularFactor write FSpecularFactor;
  property DisplaceFactor: Single read FDisplaceFactor write FDisplaceFactor;
  property Mode: TP3DMapMode read FMode write FMode;
  }

  TP3DOIEditorDataMaterialMaps = class ( TP3DOIEditorGroup )
    private
      FMapsListView: TP3DPropertyEditorMaterialMapList;
      FMaterial: TP3DMaterial;
      FOnSelectionChange: TP3DListViewSelectionChangeEvent;

      procedure SelectionChange( Sender: TP3DListView; var NewSelection: TP3DListItem );
      procedure SetMaterial(AValue: TP3DMaterial);

    public
      procedure Update; override;
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      class function IsCompatible(AObj: TInterfacedPersistent): Boolean; override;

    published
      property MapsListView: TP3DPropertyEditorMaterialMapList read FMapsListView write FMapsListView;
      property Material: TP3DMaterial read FMaterial write SetMaterial;
      property OnSelectionChange: TP3DListViewSelectionChangeEvent read FOnSelectionChange write FOnSelectionChange;
  end;

  { TP3DOIEditorDataMeshMaterial }

  TP3DOIEditorDataMeshMaterial = class ( TP3DOIEditorGroup )
    private
      FMatListView: {TP3DPropertyEditorMaterialList}TP3DListView;
      FMesh: TP3DMesh;

      function GetSelectionChange: TP3DListViewSelectionChangeEvent;
      procedure SetSelectionChange(AValue: TP3DListViewSelectionChangeEvent);

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;
      class function IsCompatible(AObj: TInterfacedPersistent): Boolean; override;

    published
      property MatListView: {TP3DPropertyEditorMaterialList}TP3DListView read FMatListView write FMatListView;
      property Mesh: TP3DMesh read FMesh write FMesh;
      property OnSelectionChange: TP3DListViewSelectionChangeEvent read GetSelectionChange write SetSelectionChange;
  end;

  { TP3DOIEditorDataProperties }

  TP3DOIEditorDataProperties = class ( TP3DOIEditorGroup )
    public
      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
  end;


  { TP3DOIEditorPageMaps }

  TP3DOIEditorPageMaps = class ( TP3DOIEditorPage )
    private
      FMapsEditor: TP3DOIEditorDataMaterialMaps;
      FMapSelectedEditor: TP3DOIEditorDataMaterialMap;
      FMaterial: TP3DMaterial;
      FTextureEditor: TP3DOIEditorDataTexture;

      procedure SetMaterial(AValue: TP3DMaterial);
      procedure SelectionChange( Sender: TP3DListView; var NewSelection: TP3DListItem );

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; override;

    published
      property MapsEditor: TP3DOIEditorDataMaterialMaps read FMapsEditor write FMapsEditor;
      property MapSelectedEditor: TP3DOIEditorDataMaterialMap read FMapSelectedEditor write FMapSelectedEditor;
      property TextureEditor: TP3DOIEditorDataTexture read FTextureEditor write FTextureEditor;
      property Material: TP3DMaterial read FMaterial write SetMaterial;
  end;

  { TP3DOIEditorPageMaterial }

  TP3DOIEditorPageMaterial = class ( TP3DOIEditorPage )
    private
      FEditorMatBase: TP3DOIEditorDataMaterialBase;
      FEditorMatMesh: TP3DOIEditorDataMeshMaterial;
      FEditorPageMaps: TP3DOIEditorPageMaps;

      procedure SelectionChange( Sender: TP3DListView; var NewSelection: TP3DListItem );

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; override;

    published
      property EditorMatMesh: TP3DOIEditorDataMeshMaterial read FEditorMatMesh write FEditorMatMesh;
      property EditorMatBase: TP3DOIEditorDataMaterialBase read FEditorMatBase write FEditorMatBase;
      property EditorPageMaps: TP3DOIEditorPageMaps read FEditorPageMaps write FEditorPageMaps;
  end;

  { TP3DOIEditorPageMeshModifier }

  TP3DOIEditorPageObjectModifier = class ( TP3DOIEditorPage )
    private
      FEditorObjectModifiers: TP3DPropertyEditorObjectModifierList;
      FEditorSelectedMod: TP3DOIEditorDataProperties;

      procedure SelectionChange( Sender: TP3DListView; var NewSelection: TP3DListItem );

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; override;

    published
      property EditorMeshModifiers: TP3DPropertyEditorObjectModifierList read FEditorObjectModifiers write FEditorObjectModifiers;
      property EditorSelectedMod: TP3DOIEditorDataProperties read FEditorSelectedMod write FEditorSelectedMod;
  end;

  { TP3DOIEditorPageProperties }

  TP3DOIEditorPageProperties = class ( TP3DOIEditorPage )
    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; override;
  end;

  { TP3DOIEditorActionKey }

  TP3DOIEditorActionKey = class ( TP3DOIEditorGroup )
    private
      FKey: TP3DActionKey;
      FTime: TP3DPropertyEditorFloat;
      FValue: TP3DPropertyEditorFloat;
      procedure SetKey(AValue: TP3DActionKey);

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

      property Time: TP3DPropertyEditorFloat read FTime write FTime;
      property Value: TP3DPropertyEditorFloat read FValue write FValue;
      property Key: TP3DActionKey read FKey write SetKey;
  end;

  { TP3DOIEditorActionChannel }

  TP3DOIEditorActionChannel = class ( TP3DOIEditorGroup )
    private
      FChannel: TP3DActionChannel;
      FKeys: TP3DPropertyEditorActionKeyList;
      FPropStr: TP3DPropertyEditorString;
      FTime: TP3DPropertyEditorFloat;

      procedure SetChannel(AValue: TP3DActionChannel);

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;

      property Time: TP3DPropertyEditorFloat read FTime write FTime;
      property Keys: TP3DPropertyEditorActionKeyList read FKeys write FKeys;
      property PropStr: TP3DPropertyEditorString read FPropStr write FPropStr;
      property Channel: TP3DActionChannel read FChannel write SetChannel;
  end;


  { TP3DOIEditorPageAction }

  TP3DOIEditorPageAction = class ( TP3DOIEditorPage )
    private
      FAction: TP3DAction;
      FChannel: TP3DOIEditorActionChannel;
      FChannels: TP3DPropertyEditorActionChannelList;
      FEditorKey: TP3DOIEditorActionKey;

      procedure ChannelSelectionChange( Sender: TP3DListView; var NewSelection: TP3DListItem );
      procedure KeysSelectionChange( Sender: TP3DListView; var NewSelection: TP3DListItem );

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      procedure Update; override;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; override;
      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; override;
      property Channels: TP3DPropertyEditorActionChannelList read FChannels write FChannels;
      property Channel: TP3DOIEditorActionChannel read FChannel write FChannel;
      property Action: TP3DAction read FAction write FAction;
      property EditorKey: TP3DOIEditorActionKey read FEditorKey write FEditorKey;
  end;

var
  TileGridSelIndex: Integer = 0;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DPropertyEditorBoolean }

function TP3DPropertyEditorBoolean.GetObjInsp: TP3DObjectInspector;
begin
  Result:= FObjInsp;
end;

procedure TP3DPropertyEditorBoolean.SetObjInsp(AValue: TP3DObjectInspector);
begin
  FObjInsp:= AValue;
end;

procedure TP3DPropertyEditorBoolean.SetProp(AValue: TP3DPropertyAccess);
begin
  if ( AValue = Prop ) then
    exit;

  FProp:= AValue;

  if ( Assigned( AValue )) then
    begin
      FChecked:= Boolean( StrToInt( AValue.AsString ));
      Canvas.GetValidText( Prop.Name + ':', FLabel );
    end
  else
    Canvas.GetValidText( '<noprop>', FLabel );
end;

function TP3DPropertyEditorBoolean.GetProp: TP3DPropertyAccess;
begin
  Result:= FProp;
end;

procedure TP3DPropertyEditorBoolean.SetChecked(AValue: Boolean);
begin
  if ( Assigned( Prop )) then
    Prop.AsString:= IntToStr( Integer( AValue ));
end;

function TP3DPropertyEditorBoolean.GetChecked: Boolean;
begin
  if ( Assigned( Prop )) then
    Result:= Boolean( StrToInt( FProp.AsString ))
  else
    Result:= False;
end;

function TP3DPropertyEditorBoolean.GetCaption: String;
begin
  {if ( Assigned( Prop )) then
    Result:= Prop.Name
  else
    Result:= '<None>';}
  Result:= ''; //We want the caption to align left
end;

constructor TP3DPropertyEditorBoolean.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  Height:= 20;
  HAlignment:= haRight;
end;

procedure TP3DPropertyEditorBoolean.FastDestroy;
begin
  FreeAndNil( FLabel );
  inherited FastDestroy;
end;

procedure TP3DPropertyEditorBoolean.Draw;
begin
  inherited Draw;
  begin
    Canvas.RenderRect( vec2( 0 ), vec2( FLabel.Width - 5, Height ), Primary200 );
    Canvas.RenderRect( vec2( FLabel.Width - 5, 0 ), vec2( FLabel.Width + 5, Height ), Primary200, vec4( Primary200.XYZ, 0 ), vec4( Primary200.XYZ, 0 ), Primary200 );
    Canvas.RenderText( FLabel, vec2( 0 ));
  end;
end;


function AddContainer( Parent: TP3DGraphicControl; Title: String ): TP3DGraphicControl;
var
  Lbl: TP3DLabel;
  Owner: TP3DLibrary;
begin
  if ( Assigned( Parent )) then
    Owner:= Parent.Owner
  else
    Owner:= P3DData;
  Result:= TP3DGraphicControl.Create( Owner, Parent );
  Result.Align:= alTop;
  Result.Height:= 20;
  Lbl:= TP3DLabel.Create( Owner, Result );
  Lbl.Align:= alLeft;
  Lbl.Caption:= Title + ' '; //One space for safety distance - looks better!
  Lbl.AutoSize:= True;
end;

{ TP3DOIEditorObject }

procedure TP3DOIEditorObject.Update;
begin
  inherited;
  if ( Obj is IP3DNamedObject ) then
    with ( Obj as IP3DNamedObject ) do
      begin
        NameEdit.Prop:= Properties.GetPropByName( 'Name' );
        TypeLbl.Caption:= ClassName;
        if ( Obj is TP3DObject ) then
          begin
            CollisionEdit.Prop:= TP3DObject( Obj ).Properties.GetPropByName( 'Collision' );
            CollisionEdit.Parent.Visible:= True;
            VisibleCheckBox.Prop:= TP3DObject( Obj ).Properties.GetPropByName( 'Visible' );
            VisibleCheckBox.Visible:= True;
          end
        else
          begin
            CollisionEdit.Prop:= nil;
            CollisionEdit.Parent.Visible:= False;
            VisibleCheckBox.Prop:= nil;
            VisibleCheckBox.Visible:= False;
          end;
      end;
end;

constructor TP3DOIEditorObject.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Object';

  NameEdit:= TP3DPropertyEditorString.Create( AData, Self );
  NameEdit.Height:= 20;
  NameEdit.Align:= alTop;

  TypeLbl:= TP3DLabel.Create( AData, AddContainer( Self, 'Type ' ));
  TypeLbl.Height:= 20;
  TypeLbl.Align:= alTop;

  CollisionEdit:= TP3DPropertyEditorEnum.Create( AData, Self );
  CollisionEdit.Height:= 20;
  CollisionEdit.Align:= alClient;

  VisibleCheckBox:= TP3DPropertyEditorBoolean.Create( AData, Self );
  VisibleCheckBox.Height:= 20;
  VisibleCheckBox.Align:= alTop;
  VisibleCheckBox.HAlignment:= haRight;
end;

class function TP3DOIEditorObject.IsCompatible( AObj: TInterfacedPersistent ): Boolean;
begin
  Result:= AObj is IP3DNamedObject;
end;

{ TP3DOIEditorDatablock }

procedure TP3DOIEditorDatablock.Update;
begin
  inherited;
  if ( Obj is TP3DObject ) then
    with ( Obj as TP3DObject ) do
      begin
        FDataProp.Field:= @Data;
        DataEd.Prop:= FDataProp;
        if ( Assigned( Data )) then
          begin
            //DataCombo.Caption:= Data.Name;
            //DataCombo.TypeFilter:= TP3DDataBlock;
            if ( Data.Owner = Owner ) then
              NameEd.Caption:= Data.Name
            else
              NameEd.Caption:= Data.Owner.Name + '.' + Data.Name;
            TypeLbl.Caption:= Data.ClassName;
          end
        else
          begin
            //DataCombo.Caption:= '<None>';
            NameEd.Caption:= '<None>';
            TypeLbl.Caption:= '<None>';
          end;
      end
  else if ( Obj is TP3DDataBlock ) then
    with ( Obj as TP3DDataBlock ) do
      begin
        FDataProp.Field:= @Obj;
        DataEd.Prop:= FDataProp;
        //DataCombo.Caption:= Name;
        NameEd.Caption:= Name;
        TypeLbl.Caption:= ClassName;
        //FileNameLbl.Caption:= Owner.FileWatch.FileName + '/' + DataPath;
        //FileNameLbl.Caption:= ExtractFileName( FileWatch.FileName );
      end
end;

procedure TP3DOIEditorDatablock.DataDragDrop(Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean);
begin
  if ( Source is TP3DListViewFile ) then
    with ( TP3DListViewFile( Source )) do
      begin
        {if ( Assigned( Selection )) then
          if ( P3DMatchFileName( Selection.Caption, '*.p3d' )) then
            begin
              try
                Sc:= SceneMain.ActiveScene;
                if ( Assigned( Sc )) then
                  Sc.AppendFile( AppendPathDelim( Directory ) + Selection.Caption );
              except
                on E: Exception do
                  SDL_ShowSimpleMessageBox( SDL_MESSAGEBOX_ERROR, PChar( 'Error loading file ' + Selection.Caption ), PChar( E.Message ), Nil );
              end;
            end;}
      end
  else if ( Source is TP3DTreeViewObjects ) then
    with ( TP3DTreeViewObjects( Source )) do
      if (( TPersistent( DragItem.Data ) is TP3DDataBlock ) and ( Self.Obj is TP3DObject )) then
        begin
          TP3DObject( Self.Obj ).Data:= TP3DDataBlock( DragItem.Data );
          Update; //TODO: Update whole Object Inspector from here
        end;
  Accept:= False;
end;

constructor TP3DOIEditorDatablock.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Data';
  Height:= 60;

  NameEd:= TP3DLabel.Create( nil, AddContainer( Self, 'Name ' ));
  NameEd.Height:= 20;
  NameEd.Align:= alTop;

  TypeLbl:= TP3DLabel.Create( nil, AddContainer( Self, 'Type ' ));
  TypeLbl.Height:= 20;
  TypeLbl.Align:= alTop;

  DataEd:= TP3DPropertyEditorInterfacedPersistent.Create( nil, Self );
  DataEd.Height:= 20;
  DataEd.Align:= alTop;
  FDataProp:= TP3DPropertyAccessDataBlock.Create( 'Data', smDoNotStore );
  //DataCombo.OnDragDrop:= @DataDragDrop;
end;

destructor TP3DOIEditorDatablock.Destroy;
begin
  FDataProp.Free;
  inherited Destroy;
end;

class function TP3DOIEditorDatablock.IsCompatible(AObj: TInterfacedPersistent ): Boolean;
begin
  Result:= ( AObj is TP3DObject ) or ( AObj is TP3DDataBlock );
end;


{ TP3DOIEditorTransform }

function TP3DOIEditorTransform3D.EdGetValue(Sender: TP3DVectorEdit): TVec4;
begin
  if ( Obj is IP3DTransform ) then
    with ( Obj as IP3DTransform ) do
      begin
        if ( Sender = PositionEdit ) then
          Result:= vec4( Transform.Position, 0 )
        else if ( Sender = RotationEdit ) then
          Result:= vec4( Transform.Rotation, 0 )
        else if ( Sender = ScaleEdit ) then
          Result:= vec4( Transform.Scale, 0 );
      end
  else
    Result:= vec4( 0 );
end;

procedure TP3DOIEditorTransform3D.EdSetValue(Sender: TP3DVectorEdit; AValue: TVec4 );
begin
  if ( Obj is IP3DTransform ) then
    with ( Obj as IP3DTransform ) do
      begin
        if ( Sender = PositionEdit ) then
          Transform.Position:= AValue.xyz
        else if ( Sender = RotationEdit ) then
          Transform.Rotation:= AValue.xyz
        else if ( Sender = ScaleEdit ) then
          Transform.Scale:= AValue.xyz;
      end;
end;

procedure TP3DOIEditorTransform3D.ComboChange(Sender: TP3DComboBox;
  var AValue: String);
begin
  if ( Obj is IP3DTransform ) then
    with ( Obj as IP3DTransform ) do
      case RotationMethodCombo.Caption of
        'ZYX Euler': Transform.RotationOrder:= roZYX;
        'ZXY Euler': Transform.RotationOrder:= roZXY;
        'YZX Euler': Transform.RotationOrder:= roYZX;
        'YXZ Euler': Transform.RotationOrder:= roYXZ;
        'XZY Euler': Transform.RotationOrder:= roXZY;
        'XYZ Euler': Transform.RotationOrder:= roXYZ;
      end;
end;

procedure TP3DOIEditorTransform3D.Update;
begin
  inherited;
  if ( Obj is IP3DTransform ) then
    begin
      QuaternionEdit.Prop:= ( Obj as IP3DTransform ).Transform.Properties.GetPropByName( 'Quaternion' );
      case ( Obj as IP3DTransform ).Transform.RotationOrder of
        roZYX: RotationMethodCombo.FCaption:= 'ZYX Euler';
        roZXY: RotationMethodCombo.FCaption:= 'ZXY Euler';
        roYZX: RotationMethodCombo.FCaption:= 'YZX Euler';
        roYXZ: RotationMethodCombo.FCaption:= 'YXZ Euler';
        roXZY: RotationMethodCombo.FCaption:= 'XZY Euler';
        roXYZ: RotationMethodCombo.FCaption:= 'XYZ Euler';
      end;
    end;
end;

class function TP3DOIEditorTransform3D.IsCompatible( AObj: TInterfacedPersistent ): Boolean;
begin
  Result:= AObj is IP3DTransform;
end;

constructor TP3DOIEditorTransform3D.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Transform';

  PositionEdit:= TP3DVectorEdit.Create( nil, AddContainer( Self, 'Position ' ));
  PositionEdit.Align:= alClient;
  PositionEdit.GetValueEvent:= @EdGetValue;
  PositionEdit.SetValueEvent:= @EdSetValue;

  RotationEdit:= TP3DVectorEdit.Create( nil, AddContainer( Self, 'Rotation ' ));
  RotationEdit.Align:= alClient;
  RotationEdit.GetValueEvent:= @EdGetValue;
  RotationEdit.SetValueEvent:= @EdSetValue;
  RotationEdit.Transmission:= 5.0;

  RotationMethodCombo:= TP3DComboBox.Create( nil, AddContainer( Self, 'Rotation Mode ' ));
  RotationMethodCombo.Align:= alClient;
  RotationMethodCombo.OnChange:= @ComboChange;
  RotationMethodCombo.Items.Add( 'ZYX Euler' );
  RotationMethodCombo.Items.Add( 'ZXY Euler' );
  RotationMethodCombo.Items.Add( 'YZX Euler' );
  RotationMethodCombo.Items.Add( 'YXZ Euler' );
  RotationMethodCombo.Items.Add( 'XZY Euler' );
  RotationMethodCombo.Items.Add( 'XYZ Euler' );
  //RotationMethodCombo.Items.Add( 'Quaternion XYZW' );

  QuaternionEdit:= TP3DPropertyEditorVec4.Create( nil, Self );
  QuaternionEdit.Align:= alTop;

  ScaleEdit:= TP3DVectorEdit.Create( nil, AddContainer( Self, 'Scale ' ));
  ScaleEdit.Align:= alClient;
  ScaleEdit.GetValueEvent:= @EdGetValue;
  ScaleEdit.SetValueEvent:= @EdSetValue;
end;


{ TP3DOIEditorPageObject }

constructor TP3DOIEditorPageObject.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  EditorObject:= TP3DOIEditorObject.Create( nil, Self );
  EditorObject.Align:= alTop;

  EditorTransform:= TP3DOIEditorTransform3D.Create( nil, Self );
  EditorTransform.Align:= alTop;

  EditorData:= TP3DOIEditorDatablock.Create( nil, Self );
  EditorData.Align:= alTop;
end;

procedure TP3DOIEditorPageObject.Update;
begin
  inherited;
  EditorObject.Obj:= Obj;
  EditorTransform.Obj:= Obj;
  EditorData.Obj:= Obj;
end;

class function TP3DOIEditorPageObject.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= AObj is TInterfacedPersistent;
end;

function TP3DOIEditorPageObject.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= inherited CreateButton(AParent, AMouseClick);
  Result.Caption:= P3DSymbolObject;
end;

{ TP3DOIEditorPageGrid }

constructor TP3DOIEditorPageGrid.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  {MeshList:= TP3DPropertyEditorDataBlockList.Create( nil, Self );
  MeshList.Align:= alTop;
  MeshList.Height:= 100;
  MeshList.OnSelectionChange:= @OnMeshSelect;}
  GridWidthEd:= TP3DPropertyEditorInt.Create( nil, AddContainer( Self, 'Width ' ));
  GridWidthEd.Align:= alClient;
  GridHeightEd:= TP3DPropertyEditorInt.Create( nil, AddContainer( Self, 'Height ' ));
  GridHeightEd.Align:= alClient;
  GridWorldUnitEd:= TP3DPropertyEditorFloat.Create( nil, AddContainer( Self, 'GridWorldUnit ' ));
  GridWorldUnitEd.Align:= alClient;
end;

procedure TP3DOIEditorPageGrid.Update;
var
  Grid: TP3DTileGrid;
begin
  inherited Update;
  if ( Obj is TP3DTileGrid ) then
    Grid:= TP3DTileGrid( Obj )
  else if ( Obj is TP3DObject ) and ( TP3DObject( Obj ).Data is TP3DTileGrid ) then
    Grid:= TP3DTileGrid( TP3DObject( Obj ).Data )
  else
    begin
      //MeshList.Prop:= nil;
      GridWidthEd.Prop:= nil;
      GridHeightEd.Prop:= nil;
      GridWorldUnitEd.Prop:= nil;
      exit;
    end;
  //MeshList.Prop:= TP3DPropertyAccessDataBlockList( Grid.Properties.GetPropByName( 'Scenes' ));
  GridWidthEd.Prop:= Grid.Properties.GetPropByName( 'Width' );
  GridHeightEd.Prop:= Grid.Properties.GetPropByName( 'Height' );
  GridWorldUnitEd.Prop:= Grid.Properties.GetPropByName( 'GridWorldUnit' );
end;

class function TP3DOIEditorPageGrid.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= ( AObj is TP3DTileGrid ) or (( AObj is TP3DObject ) and ( TP3DObject( AObj ).Data is TP3DTileGrid ));
end;

function TP3DOIEditorPageGrid.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= inherited CreateButton(AParent, AMouseClick);
  Result.Caption:= P3DSymbolGrid;
end;

procedure TP3DOIEditorPageGrid.OnMeshSelect(Sender: TP3DListView; var NewSelection: TP3DListItem);
begin
  //TileGridSelIndex:= MeshList.Children.IndexOf( NewSelection );
end;


{ TP3DOIEditorDataLight }

procedure TP3DOIEditorDataLight.ComboChange(Sender: TP3DComboBox;
  var AValue: String);
begin
  if ( Assigned( Light )) then
    case AValue of
      'Point': Light.LightType := ltPoint;
      'Spot': Light.LightType := ltSpot;
      'Sun': Light.LightType := ltSun;
    end;
end;

function TP3DOIEditorDataLight.GetLightColor(Sender: TP3DColorRGBAPicker): TP3DColorRGBA;
begin
  if ( Assigned( Light )) then
    Result:= vec4( Light.Color, 1 )
  else
    Result:= vec4( 0 );
end;

procedure TP3DOIEditorDataLight.SetLightColor(Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA);
begin
  if ( Assigned( Light )) then
    Light.Color:= AValue.RGB;
end;

function TP3DOIEditorDataLight.GetLightAttenuation( Sender: TP3DEventValueEdit ): Float;
begin
  if ( Assigned( Light )) then
    begin
      if ( Sender = LightLinearAttenuationEdit ) then
        Result:= Light.LinearAttenuation
      else if ( Sender = LightQuadraticAttenuationEdit ) then
        Result:= Light.QuadraticAttenuation;
    end;
end;

procedure TP3DOIEditorDataLight.SetLightAttenuation(Sender: TP3DEventValueEdit;
  AValue: Float);
begin
  if ( Assigned( Light )) then
    begin
      if ( Sender = LightLinearAttenuationEdit ) then
        Light.LinearAttenuation:= AValue
      else if ( Sender = LightQuadraticAttenuationEdit ) then
        Light.QuadraticAttenuation:= AValue;
    end;
end;

procedure TP3DOIEditorDataLight.Update;
begin
  inherited;

  if (( Obj is TP3DObject ) and ( TP3DObject( Obj ).Data is TP3DLight )) then
    Light:= TP3DLight( TP3DObject( Obj ).Data )
  else if ( Obj is TP3DLight ) then
    Light:= TP3DLight( Obj )
  else
    Light:= nil;

  if ( Assigned( Light )) then
    begin
      LightEnergyEdit.Prop:= Light.Properties.GetPropByName( 'Energy' );
      case Light.LightType of
        ltPoint: LightTypeCombo.Caption:= 'Point';
        ltSpot: LightTypeCombo.Caption:= 'Spot';
        ltSun: LightTypeCombo.Caption:= 'Sun';
      end;
    end;
end;

class function TP3DOIEditorDataLight.IsCompatible( AObj: TInterfacedPersistent ): Boolean;
begin
  Result:= (( AObj is TP3DObject ) and ( TP3DObject( AObj ).Data is TP3DLight )) or ( AObj is TP3DLight );
end;

constructor TP3DOIEditorDataLight.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Light';

  LightTypeCombo:= TP3DComboBox.Create( nil, AddContainer( Self, 'Light type ' ));
  LightTypeCombo.Align:= alClient;
  LightTypeCombo.Items.Add( 'Sun' );
  LightTypeCombo.Items.Add( 'Point' );
  LightTypeCombo.Items.Add( 'Spot' );
  LightTypeCombo.OnChange:= @ComboChange;

  LightColorCombo:= TP3DColorRGBAComboBox.Create( nil, AddContainer( Self, 'Light color ' ));
  LightColorCombo.GetValueEvent:= @GetLightColor;
  LightColorCombo.SetValueEvent:= @SetLightColor;
  LightColorCombo.Align:= alClient;

  LightLinearAttenuationEdit:= TP3DEventValueEdit.Create( nil, AddContainer( Self, 'Linear Attenuation ' ));
  LightLinearAttenuationEdit.GetValueEvent:= @GetLightAttenuation;
  LightLinearAttenuationEdit.SetValueEvent:= @SetLightAttenuation;
  LightLinearAttenuationEdit.Align:= alClient;

  LightQuadraticAttenuationEdit:= TP3DEventValueEdit.Create( nil, AddContainer( Self, 'Quadratic Attenuation ' ));
  LightQuadraticAttenuationEdit.GetValueEvent:= @GetLightAttenuation;
  LightQuadraticAttenuationEdit.SetValueEvent:= @SetLightAttenuation;
  LightQuadraticAttenuationEdit.Align:= alClient;

  LightEnergyEdit:= TP3DPropertyEditorFloat.Create( nil, AddContainer( Self, 'Energy ' ));
  LightEnergyEdit.Align:= alClient;
end;

{ TP3DOIEditorDataCamera }

procedure TP3DOIEditorDataCamera.ComboChange(Sender: TP3DComboBox;
  var AValue: String);
begin
  if ( Assigned( Camera )) then
    if ( Sender = HandednessCombo ) then
      case AValue of
        'Left': Camera.Handedness:= p3dchLeft;
        'Right': Camera.Handedness:= p3dchRight;
      end
    else
      case AValue of
        'Perspective': Camera.IsIsometric:= False;
        'Isometric': Camera.IsIsometric:= True;
      end;
end;

function TP3DOIEditorDataCamera.GetCamParamV(Sender: TP3DVectorEdit): TVec4;
begin
  if ( Assigned( Camera )) then
    Result:= vec4( Camera.Near, Camera.Far, 0, 0 )
  else
    Result:= vec4( 0 );
end;

procedure TP3DOIEditorDataCamera.SetCamParamV(Sender: TP3DVectorEdit; AValue: TVec4);
begin
  if ( Assigned( Camera )) then
    begin
      Camera.Near:= AValue.X;
      Camera.Far:= AValue.Y;
    end;
end;

function TP3DOIEditorDataCamera.GetCamParamF(Sender: TP3DEventValueEdit): Float;
begin
  if ( Assigned( Camera )) then
    begin
      if ( Camera.IsIsometric ) then
        Result:= Camera.IsoZoom
      else
        Result:= Camera.FOV;
    end
  else
    Result:= 0;
end;

procedure TP3DOIEditorDataCamera.SetCamParamF(Sender: TP3DEventValueEdit; AValue: Float);
begin
  if ( Assigned( Camera )) then
    begin
      if ( Camera.IsIsometric ) then
        Camera.IsoZoom := AValue
      else
        Camera.FOV := AValue;
    end;
end;

function TP3DOIEditorDataCamera.GetEdValueName(Sender: TP3DEventValueEdit): String;
begin
  if ( Assigned( Camera )) then
    begin
      if ( Camera.IsIsometric ) then
        Result:= 'Zoom'
      else
        Result:= 'FOV';
    end
  else
    Result:= '';
end;

procedure TP3DOIEditorDataCamera.Update;
begin
  inherited Update;
  if (( Obj is TP3DObject ) and ( TP3DObject( Obj ).Data is TP3DCamera )) then
    Camera:= TP3DCamera( TP3DObject( Obj ).Data )
  else if ( Obj is TP3DCamera ) then
    Camera:= TP3DCamera( Obj )
  else
    Camera:= nil;

  if ( Assigned( Camera )) then
    begin
      case Camera.Handedness of
        p3dchLeft: HandednessCombo.Caption:= 'Left';
        p3dchRight: HandednessCombo.Caption:= 'Right';
      end;
      case Camera.IsIsometric of
        True: CamTypeCombo.Caption:= 'Isometric';
        False: CamTypeCombo.Caption:= 'Perspective';
      end;
    end;
end;

class function TP3DOIEditorDataCamera.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= ( AObj is TP3DCamera ) or (( AObj is TP3DObject ) and ( TP3DObject( AObj ).Data is TP3DCamera ));
end;

constructor TP3DOIEditorDataCamera.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Camera';

  ZoomEdit:= TP3DEventValueEdit.Create( AData, Self );
  ZoomEdit.GetValueEvent:= @GetCamParamF;
  ZoomEdit.SetValueEvent:= @SetCamParamF;
  ZoomEdit.GetValueNameEvent:= @GetEdValueName;
  ZoomEdit.Align:= alTop;
  ZoomEdit.Height:= 20;

  ClippingEdit:= TP3DVectorEdit.Create( AData, AddContainer( Self, 'Clipping' ));
  ClippingEdit.GetValueEvent:= @GetCamParamV;
  ClippingEdit.SetValueEvent:= @SetCamParamV;
  ClippingEdit.Align:= alClient;
  ClippingEdit.Labels[ 0 ]:= 'Near';
  ClippingEdit.Labels[ 1 ]:= 'Far';
  ClippingEdit.EdCount:= 2;

  HandednessCombo:= TP3DComboBox.Create( AData, AddContainer( Self, 'Handedness' ));
  HandednessCombo.Align:= alClient;
  HandednessCombo.Items.Add( 'Left' );
  HandednessCombo.Items.Add( 'Right' );
  HandednessCombo.OnChange:= @ComboChange;

  CamTypeCombo:= TP3DComboBox.Create( AData, AddContainer( Self, 'Camera type' ));
  CamTypeCombo.Align:= alClient;
  CamTypeCombo.Items.Add( 'Perspective' );
  CamTypeCombo.Items.Add( 'Isometric' );
  CamTypeCombo.OnChange:= @ComboChange;
end;

{ TP3DOIEditorDataTexture }

procedure TP3DOIEditorDataTexture.ComboChange(Sender: TP3DComboBox;
  var AValue: String);
var
  Tex: TP3DTexture;
begin
  if ( not Assigned( Obj )) then
      exit;
  Tex:= TP3DTexture( Obj );

  if ( Sender = FilteringCombo ) then
    case ( AValue ) of
      'Nearest': Tex.Filtering:= tfNearest;
      'Linear': Tex.Filtering:= tfLinear;
    end;

  if ( Sender = FilteringMipCombo ) then
    case ( AValue ) of
      'Nearest': Tex.FilteringMipMap:= tfNearest;
      'Linear': Tex.FilteringMipMap:= tfLinear;
    end;
end;

procedure TP3DOIEditorDataTexture.CheckboxChange(Sender: TP3DGraphicControl);
begin
  if ( not Assigned( Obj )) then
      exit;
  TP3DTexture( Obj ).MipMap:= MipMappingCombo.Checked;
end;

procedure TP3DOIEditorDataTexture.Update;
var
  Tex: TP3DTexture;
begin
  inherited;
  if ( not IsCompatible( Obj )) then
    exit;
  Tex:= TP3DTexture( Obj );

  PreviewImg.Texture:= Tex;

  case Tex.Filtering of
    tfNearest: FilteringCombo.FCaption:= 'Nearest';
    tfLinear: FilteringCombo.FCaption:= 'Linear';
  end;

  case Tex.FilteringMipMap of
    tfNearest: FilteringMipCombo.FCaption:= 'Nearest';
    tfLinear: FilteringMipCombo.FCaption:= 'Linear';
  end;

  MipMappingCombo.FChecked:= Tex.MipMap;

  DimensionsLbl.Caption:= Format( '%dx%d', [ Tex.Width, Tex.Height ]);

  FileNameLbl.Caption:= Format( '%s%s', [ StringOfChar( '*', Ord( Tex.FileWatch.CheckForChange )), ExtractFileName( Tex.FileWatch.FileName )]);
end;

class function TP3DOIEditorDataTexture.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= AObj is TP3DTexture;
end;

constructor TP3DOIEditorDataTexture.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Texture';

  PreviewImg:= TP3DImage.Create( AData, Self );
  PreviewImg.Align:= alTop;
  PreviewImg.Width:= 200;
  PreviewImg.Height:= 200;

  FilteringCombo:= TP3DComboBox.Create( AData, AddContainer( Self, 'Filtering ' ));
  FilteringCombo.Align:= alClient;
  FilteringCombo.Items.Add( 'Nearest' );
  FilteringCombo.Items.Add( 'Linear' );
  FilteringCombo.OnChange:= @ComboChange;

  FilteringMipCombo:= TP3DComboBox.Create( AData, AddContainer( Self, 'Filtering Mipmap ' ));
  FilteringMipCombo.Align:= alClient;
  FilteringMipCombo.Items.Add( 'Nearest' );
  FilteringMipCombo.Items.Add( 'Linear' );
  FilteringMipCombo.OnChange:= @ComboChange;

  MipMappingCombo:= TP3DCheckBox.Create( AData, Self );
  MipMappingCombo.Caption:= 'Use Mipmap';
  MipMappingCombo.Align:= alTop;
  MipMappingCombo.HAlignment:= haRight;
  MipMappingCombo.OnChange:= @CheckboxChange;
  MipMappingCombo.Height:= 20;

  DimensionsLbl:= TP3DLabel.Create( AData, AddContainer( Self, 'Size ' ));
  DimensionsLbl.Align:= alClient;
  DimensionsLbl.HAlignment:= haRight;

  FileNameLbl:= TP3DLabel.Create( AData, AddContainer( Self, 'Filename ' ));
  FileNameLbl.Align:= alClient;
  FileNameLbl.HAlignment:= haRight;
end;

{ TP3DOIEditorDataMaterialBase }

procedure TP3DOIEditorDataMaterialBase.CheckboxChange(Sender: TP3DGraphicControl);
begin
  if ( not IsCompatible( Obj )) then
    exit;
  with ( Obj as TP3DMaterialBase ) do
    begin
      if ( Sender = UseVertexColorEdit ) then
        UseVertexColor:= UseVertexColorEdit.Checked
      else if ( Sender = UnlitEdit ) then
        Unlit:= UnlitEdit.Checked;
    end;
end;

function TP3DOIEditorDataMaterialBase.GetMatColorParam(Sender: TP3DColorRGBAPicker): TP3DColorRGBA;
begin
  if ( not IsCompatible( Obj )) then
    begin
      Result:= vec4( 0 );
      exit;
    end;
  with ( Obj as TP3DMaterialBase ) do
    begin
      if ( Sender = DiffCombo.ColorPicker ) then
        Result:= vec4( Diff, 1 )
      else if ( Sender = SpecCombo.ColorPicker ) then
        Result:= vec4( Spec, 1 );
    end;
end;

procedure TP3DOIEditorDataMaterialBase.SetMatColorParam(Sender: TP3DColorRGBAPicker; AValue: TP3DColorRGBA);
begin
  if ( not IsCompatible( Obj )) then
    exit;
  with ( Obj as TP3DMaterialBase ) do
    begin
      if ( Sender = DiffCombo.ColorPicker ) then
        Diff:= AValue.XYZ
      else if ( Sender = SpecCombo.ColorPicker ) then
        Spec:= AValue.XYZ;
    end;
end;

function TP3DOIEditorDataMaterialBase.GetMatFloatParam(Sender: TP3DEventValueEdit): Float;
begin
  if ( not IsCompatible( Obj )) then
    begin
      Result:= 0;
      exit;
    end;
  with ( Obj as TP3DMaterialBase ) do
    begin
      if ( Sender = AlphaEdit ) then
        Result:= Alpha
      else if ( Sender = Spec_HardnessEdit ) then
        Result:= Spec_Hardness;
    end;
end;

procedure TP3DOIEditorDataMaterialBase.SetMatFloatParam(Sender: TP3DEventValueEdit; AValue: Float);
begin
  if ( not IsCompatible( Obj )) then
    exit;
  with ( Obj as TP3DMaterialBase ) do
    begin
      if ( Sender = AlphaEdit ) then
        Alpha:= AValue
      else if ( Sender = Spec_HardnessEdit ) then
        Spec_Hardness:= AValue;
    end;
end;

procedure TP3DOIEditorDataMaterialBase.Update;
begin
  inherited;
  if ( not IsCompatible( Obj )) then
    exit;
  with ( Obj as TP3DMaterialBase ) do
    begin
      UseVertexColorEdit.FChecked:= UseVertexColor;
      UnlitEdit.FChecked:= Unlit;
      TranspModeCombo.Prop:= Properties.GetPropByName( 'TransparencyMode' );
      AlphaTreshHoldEdit.Prop:= Properties.GetPropByName( 'AlphaThreshhold' );
    end;
end;

class function TP3DOIEditorDataMaterialBase.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= AObj is TP3DMaterialBase;
end;

constructor TP3DOIEditorDataMaterialBase.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Material';

  UseVertexColorEdit:= TP3DCheckBox.Create( nil, Self );
  UseVertexColorEdit.Caption:= 'Use Vertex Colors';
  UseVertexColorEdit.Align:= alTop;
  UseVertexColorEdit.HAlignment:= haRight;
  UseVertexColorEdit.OnChange:= @CheckboxChange;
  UseVertexColorEdit.Height:= 20;

  UnlitEdit:= TP3DCheckBox.Create( AData, Self );
  UnlitEdit.Caption:= 'Unlit';
  UnlitEdit.Align:= alTop;
  UnlitEdit.HAlignment:= haRight;
  UnlitEdit.OnChange:= @CheckboxChange;
  UnlitEdit.Height:= 20;

  DiffCombo:= TP3DColorRGBAComboBox.Create( AData, AddContainer( Self, 'Material Diffuse' ));
  DiffCombo.GetValueEvent:= @GetMatColorParam;
  DiffCombo.SetValueEvent:= @SetMatColorParam;
  DiffCombo.Align:= alClient;

  SpecCombo:= TP3DColorRGBAComboBox.Create( AData, AddContainer( Self, 'Material Specular' ));
  SpecCombo.GetValueEvent:= @GetMatColorParam;
  SpecCombo.SetValueEvent:= @SetMatColorParam;
  SpecCombo.Align:= alClient;

  AlphaEdit:= TP3DEventValueEdit.Create( AData, AddContainer( Self, 'Alpha' ));
  AlphaEdit.GetValueEvent:= @GetMatFloatParam;
  AlphaEdit.SetValueEvent:= @SetMatFloatParam;
  AlphaEdit.Align:= alClient;

  Spec_HardnessEdit:= TP3DEventValueEdit.Create( AData, AddContainer( Self, 'Specular Hardness' ));
  Spec_HardnessEdit.GetValueEvent:= @GetMatFloatParam;
  Spec_HardnessEdit.SetValueEvent:= @SetMatFloatParam;
  Spec_HardnessEdit.Align:= alClient;

  TranspModeCombo:= TP3DPropertyEditorEnum.Create( AData, AddContainer( Self, 'Transparency Mode' ));
  TranspModeCombo.Align:= alClient;

  AlphaTreshHoldEdit:= TP3DPropertyEditorFloat.Create( AData, Self );
  AlphaTreshHoldEdit.Height:= 20;
  AlphaTreshHoldEdit.Align:= alTop;
end;

{ TP3DOIEditorDataMaterialMaps }

procedure TP3DOIEditorDataMaterialMaps.SelectionChange(Sender: TP3DListView;
  var NewSelection: TP3DListItem);
begin
  if ( Assigned( OnSelectionChange )) then
    OnSelectionChange( Sender, NewSelection );
end;

procedure TP3DOIEditorDataMaterialMaps.SetMaterial(AValue: TP3DMaterial);
var
  Map: TP3DMaterialMap;
begin
  if FMaterial= AValue then Exit;
  FMaterial:= AValue;

  {MapsListView.Children.Clear( True );

  if ( Assigned( Material )) then
    for Map in Material.Maps do
      if ( Assigned( Map.Map )) then
        MapsListView.Children.Add( Map.Map.Name, Map )
      else
        MapsListView.Children.Add( '<Not defined>', nil );}
  if ( Assigned( Material )) then
    MapsListView.Prop:= TP3DPropertyAccessMaterialMapList( Material.Properties.GetPropByName( 'Maps' ))
  else
    MapsListView.Prop:= nil;
end;

procedure TP3DOIEditorDataMaterialMaps.Update;
begin
  inherited;
  if ( Obj is TP3DMaterial ) then
    Material:= TP3DMaterial( Obj )
  else
    Material:= nil;
end;

constructor TP3DOIEditorDataMaterialMaps.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Material Maps';

  MapsListView:= TP3DPropertyEditorMaterialMapList.Create( AData, Self );
  MapsListView.Align:= alTop;
  MapsListView.Height:= 100;
  MapsListView.OnSelectionChange:= @SelectionChange;
end;

class function TP3DOIEditorDataMaterialMaps.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= AObj is TP3DMaterial;
end;

{ TP3DOIEditorDataMaterialMap }

procedure TP3DOIEditorDataMaterialMap.SetMap(AValue: TP3DMaterialMap);
begin
  if FMap= AValue then Exit;
  FMap:= AValue;
end;

function TP3DOIEditorDataMaterialMap.GetFloatParam(Sender: TP3DEventValueEdit): Float;
begin
  Result:= 0;

  if ( not ( Assigned( Map ) and ( Map is TP3DMaterialMapBase ))) then
    exit;

  if ( Sender = DiffuseEdit ) then
    Result:= TP3DMaterialMapBase( Map ).DiffuseFactor
  else if ( Sender = NormalEdit ) then
    Result:= TP3DMaterialMapBase( Map ).NormalFactor
  else if ( Sender = SpecularEdit ) then
    Result:= TP3DMaterialMapBase( Map ).SpecularFactor
  else if ( Sender = AlphaEdit ) then
    Result:= TP3DMaterialMapBase( Map ).AlphaFactor
  else if ( Sender = DisplaceEdit ) then
    Result:= TP3DMaterialMapBase( Map ).DisplaceFactor;
end;

procedure TP3DOIEditorDataMaterialMap.SetFloatParam(Sender: TP3DEventValueEdit; AValue: Float);
begin
  if ( not ( Assigned( Map ) and ( Map is TP3DMaterialMapBase ))) then
    exit;

  if ( Sender = DiffuseEdit ) then
    TP3DMaterialMapBase( Map ).DiffuseFactor := AValue
  else if ( Sender = NormalEdit ) then
    TP3DMaterialMapBase( Map ).NormalFactor := AValue
  else if ( Sender = SpecularEdit ) then
    TP3DMaterialMapBase( Map ).SpecularFactor := AValue
  else if ( Sender = AlphaEdit ) then
    TP3DMaterialMapBase( Map ).AlphaFactor := AValue
  else if ( Sender = DisplaceEdit ) then
    TP3DMaterialMapBase( Map ).DisplaceFactor := AValue;
end;

procedure TP3DOIEditorDataMaterialMap.ComboChange(Sender: TP3DComboBox;
  var AValue: String);
begin
  if ( not ( Assigned( Map ) and ( Map is TP3DMaterialMapBase ))) then
    exit;

  case AValue of
    'add': TP3DMaterialMapBase( Map ).Mode:= mapAdd;
    'mix': TP3DMaterialMapBase( Map ).Mode:= mapMix;
    'multiply': TP3DMaterialMapBase( Map ).Mode:= mapMultiply;
    'subtract': TP3DMaterialMapBase( Map ).Mode:= mapSubtract;
  end;
end;

procedure TP3DOIEditorDataMaterialMap.Update;
begin
  inherited;
  //Mapping.Visible:= Obj is TP3DMaterialMapShader;
//  Influence.Visible:= Obj is TP3DMaterialMapBase;

  if ( Obj is TP3DMaterialMap ) then
    begin
      Map:= TP3DMaterialMap( Obj );
      //TextureEditor.Obj:= Map.Map;
      case TP3DMaterialMapBase( Map ).Mode of
        mapAdd: ModeCombo.Caption:= 'add';
        mapMix: ModeCombo.Caption:= 'mix';
        mapMultiply: ModeCombo.Caption:= 'multiply';
        mapSubtract: ModeCombo.Caption:= 'subtract';
      end;
    end
  else
    begin
      Map:= nil;
      //TextureEditor.Obj:= nil;
    end;
end;

constructor TP3DOIEditorDataMaterialMap.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Influence';
  //NoBounds:= True;

  //Mapping:= AddContainer( Self, 'Mapping' );

  //TextureEditor:= TP3DOIEditorDataTexture.Create( nil, AParent );
  //TextureEditor.Align:= alTop;

  {Influence:= TP3DGraphicControl.Create( nil, Self ); //AddContainer( Self, 'Influence' );
  Influence.Align:= alTop;
  Influence.Height:= 5 * 20;}
  Influence:= Self;

  DiffuseEdit:= TP3DEventValueEdit.Create( nil, AddContainer( Influence, 'Diffuse factor' ));
  DiffuseEdit.GetValueEvent:= @GetFloatParam;
  DiffuseEdit.SetValueEvent:= @SetFloatParam;
  DiffuseEdit.Align:= alClient;
  DiffuseEdit.Parent.Align:= alTop;

  SpecularEdit:= TP3DEventValueEdit.Create( nil, AddContainer( Influence, 'Specular factor' ));
  SpecularEdit.GetValueEvent:= @GetFloatParam;
  SpecularEdit.SetValueEvent:= @SetFloatParam;
  SpecularEdit.Align:= alClient;
  SpecularEdit.Parent.Align:= alTop;

  NormalEdit:= TP3DEventValueEdit.Create( nil, AddContainer( Influence, 'Normal factor' ));
  NormalEdit.GetValueEvent:= @GetFloatParam;
  NormalEdit.SetValueEvent:= @SetFloatParam;
  NormalEdit.Align:= alClient;
  NormalEdit.Parent.Align:= alTop;

  AlphaEdit:= TP3DEventValueEdit.Create( nil, AddContainer( Influence, 'Alpha factor' ));
  AlphaEdit.GetValueEvent:= @GetFloatParam;
  AlphaEdit.SetValueEvent:= @SetFloatParam;
  AlphaEdit.Align:= alClient;
  AlphaEdit.Parent.Align:= alTop;

  DisplaceEdit:= TP3DEventValueEdit.Create( nil, AddContainer( Influence, 'Displace factor' ));
  DisplaceEdit.GetValueEvent:= @GetFloatParam;
  DisplaceEdit.SetValueEvent:= @SetFloatParam;
  DisplaceEdit.Align:= alClient;
  DisplaceEdit.Parent.Align:= alTop;


  ModeCombo:= TP3DComboBox.Create( nil, AddContainer( Influence, 'Mix Mode' ));
  ModeCombo.OnChange:= @ComboChange;
  ModeCombo.Align:= alClient;
  ModeCombo.Items.Add( 'add' );
  ModeCombo.Items.Add( 'subtract' );
  ModeCombo.Items.Add( 'multiply' );
  ModeCombo.Items.Add( 'mix' );
end;

class function TP3DOIEditorDataMaterialMap.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= AObj is TP3DMaterialMap;
end;


{ TP3DOIEditorDataMesh }

function TP3DOIEditorDataMeshMaterial.GetSelectionChange: TP3DListViewSelectionChangeEvent;
begin
  Result:= MatListView.OnSelectionChange;
end;

procedure TP3DOIEditorDataMeshMaterial.SetSelectionChange(AValue: TP3DListViewSelectionChangeEvent);
begin
  MatListView.OnSelectionChange:= AValue;
end;

procedure TP3DOIEditorDataMeshMaterial.Update;
var
  Mat: TP3DMeshMaterialGroup;
begin
  inherited;

  if (( Obj is TP3DObject ) and ( TP3DObject( Obj ).Data is TP3DMesh )) then
    Mesh:= TP3DMesh( TP3DObject( Obj ).Data )
  else if ( Obj is TP3DMesh ) then
    Mesh:= TP3DMesh( Obj )
  else
    Mesh:= nil;

  if ( not Assigned( Mesh )) then
    exit;
  MatListView.Children.Clear( True );
  for Mat in Mesh.MaterialGroups do
    MatListView.Children.Add( Mat.Material.Name, Mat.Material );
end;

constructor TP3DOIEditorDataMeshMaterial.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Caption:= 'Mesh Materials';

  MatListView:= TP3DPropertyEditorMaterialList.Create( AData, Self );
  MatListView.Align:= alTop;
  MatListView.Height:= 100;
end;

class function TP3DOIEditorDataMeshMaterial.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= (( AObj is TP3DObject ) and ( TP3DObject( AObj ).Data is TP3DMesh )) or ( AObj is TP3DMesh );
end;


{ TP3DOIEditorPageMaterial }

procedure TP3DOIEditorPageMaterial.SelectionChange(Sender: TP3DListView;
  var NewSelection: TP3DListItem);
begin
  if ( Assigned( NewSelection )) then
    begin
      EditorMatBase.Obj:= TInterfacedPersistent( NewSelection.Data );
      if ( Assigned( EditorPageMaps )) then
        EditorPageMaps.Obj:= TInterfacedPersistent( NewSelection.Data );
    end
  else
    begin
      EditorMatBase.Obj:= nil;
      if ( Assigned( EditorPageMaps )) then
        EditorPageMaps.Obj:= nil;
    end;
end;

constructor TP3DOIEditorPageMaterial.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );

  EditorMatMesh:= TP3DOIEditorDataMeshMaterial.Create( AData, Self );
  EditorMatMesh.Align:= alTop;
  EditorMatMesh.OnSelectionChange:= @SelectionChange;

  EditorMatBase:= TP3DOIEditorDataMaterialBase.Create( AData, Self );
  EditorMatBase.Align:= alTop;
end;

procedure TP3DOIEditorPageMaterial.Update;
begin
  inherited Update;
  EditorMatMesh.Obj:= Obj;
  //EditorMatMaps.Obj:= Obj;
  EditorMatBase.Obj:= Obj;
end;

class function TP3DOIEditorPageMaterial.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= TP3DOIEditorDataMeshMaterial.IsCompatible( AObj ) or TP3DOIEditorDataMaterialBase.IsCompatible( AObj );
end;

function TP3DOIEditorPageMaterial.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= inherited CreateButton(AParent, AMouseClick);
  Result.Caption:= P3DSymbolMaterial;
end;

{ TP3DOIEditorPageMaps }

procedure TP3DOIEditorPageMaps.SetMaterial(AValue: TP3DMaterial);
begin
  FMaterial:= AValue;
  MapsEditor.Material:= AValue;
  MapsEditor.Visible:= Assigned( AValue );
end;

procedure TP3DOIEditorPageMaps.SelectionChange(Sender: TP3DListView;
  var NewSelection: TP3DListItem);
begin
  if ( Assigned( NewSelection )) then
    begin
      MapSelectedEditor.Obj:= TP3DMaterialMap( MapsEditor.MapsListView.LProp.Values[ Integer( NewSelection.Data )]);
      if ( Assigned( MapSelectedEditor.Obj )) then
        TextureEditor.Obj:= TP3DMaterialMap( MapSelectedEditor.Obj ).Map
      else
        TextureEditor.Obj:= nil;
    end
  else
    begin
      TextureEditor.Obj:= nil;
      MapSelectedEditor.Obj:= nil;
    end;
  MapSelectedEditor.Visible:= Assigned( MapSelectedEditor.Obj );
  TextureEditor.Visible:= Assigned( TextureEditor.Obj );
end;

constructor TP3DOIEditorPageMaps.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );

  MapsEditor:= TP3DOIEditorDataMaterialMaps.Create( AData, Self );
  MapsEditor.Align:= alTop;
  //MapsEditor.Height:= 100;
  MapsEditor.OnSelectionChange:= @SelectionChange;

  TextureEditor:= TP3DOIEditorDataTexture.Create( AData, Self );
  TextureEditor.Align:= alTop;

  MapSelectedEditor:= TP3DOIEditorDataMaterialMap.Create( AData, Self );
  MapSelectedEditor.Align:= alTop;
  //Height:= Controls.PageSize.Y;
  //AutoSize:= True;
end;

procedure TP3DOIEditorPageMaps.Update;
begin
  inherited Update;
  TextureEditor.Obj:= Obj;
  MapSelectedEditor.Obj:= Obj;
  if ( Obj is TP3DMaterial ) then
    Material:= TP3DMaterial( Obj )
  else
    Material:= nil;
end;

class function TP3DOIEditorPageMaps.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= TP3DOIEditorDataMeshMaterial.IsCompatible( AObj ) or ( AObj is TP3DMaterial ) or ( TP3DOIEditorDataTexture.IsCompatible( AObj ));
end;

function TP3DOIEditorPageMaps.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= inherited CreateButton(AParent, AMouseClick);
  Result.Caption:= P3DSymbolTexture;
end;

{ TP3DOIEditorPageLight }

constructor TP3DOIEditorPageLight.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  EditorLight:= TP3DOIEditorDataLight.Create( AData, Self );
  EditorLight.Align:= alTop;
end;

procedure TP3DOIEditorPageLight.Update;
begin
  inherited Update;
  EditorLight.Obj:= Obj;
end;

class function TP3DOIEditorPageLight.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= TP3DOIEditorDataLight.IsCompatible( AObj );
end;

function TP3DOIEditorPageLight.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= inherited CreateButton(AParent, AMouseClick);
  Result.Caption:= P3DSymbolLight;
end;

{ TP3DOIEditorPageCamera }

constructor TP3DOIEditorPageCamera.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  EditorCamera:= TP3DOIEditorDataCamera.Create( AData, Self );
  EditorCamera.Align:= alTop;
end;

procedure TP3DOIEditorPageCamera.Update;
begin
  inherited Update;
  EditorCamera.Obj:= Obj;
end;

class function TP3DOIEditorPageCamera.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= TP3DOIEditorDataCamera.IsCompatible( AObj );
end;

function TP3DOIEditorPageCamera.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= inherited CreateButton(AParent, AMouseClick);
  Result.Caption:= P3DSymbolCamera;
end;

{ TP3DOIEditorPageProperties }

constructor TP3DOIEditorPageProperties.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
end;

procedure TP3DOIEditorPageProperties.Update;
  function AddCategory( Prop: TP3DPropertyAccess; var Cat: TP3DOIEditorGroup ):TP3DOIEditorGroup;
  begin
    if ( not Assigned( Cat ) or ( Cat.Caption <> Prop.Category )) then
      begin
        Cat:= TP3DOIEditorGroup.Create( Owner, Self );
        Cat.Align:= alTop;
        Cat.Caption:= Prop.Category;
        Cat.PageModeY:= pmExpand;
      end;
    Result:= Cat;
  end;

var
  _O: TInterfacedPersistent;
  O: TP3DInterfacedPersistent absolute _O;
  Cat: TP3DOIEditorGroup;
  Prop: TP3DPropertyAccess;

begin
  Controls.Clear( True );

  _O:= Obj;
  if ( O is TP3DInterfacedPersistent ) then
    for Prop in O.Properties do
      AddCategory( Prop, Cat ).AddPropertyEditor( Prop );

  inherited Update;
end;

class function TP3DOIEditorPageProperties.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= AObj is TP3DInterfacedPersistent;
end;

function TP3DOIEditorPageProperties.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= inherited CreateButton(AParent, AMouseClick);
  Result.Caption:= P3DSymbolOptions;
end;

{ TP3DOIEditorDataProperties }

procedure TP3DOIEditorDataProperties.Update;
  function AddCategory( Prop: TP3DPropertyAccess; var Cat: TP3DOIEditorGroup ):TP3DOIEditorGroup;
  begin
    if ( not Assigned( Cat ) or ( Cat.Caption <> Prop.Category )) then
      begin
        Cat:= TP3DOIEditorGroup.Create( Owner, Self );
        Cat.Align:= alTop;
        Cat.Caption:= Prop.Category;
        Cat.PageModeY:= pmExpand;
      end;
    Result:= Cat;
  end;

var
  _O: TInterfacedPersistent;
  O: TP3DInterfacedPersistent absolute _O;
  Cat: TP3DOIEditorGroup;
  Prop: TP3DPropertyAccess;

begin
  Controls.Clear( True );

  _O:= Obj;
  Cat:= nil;
  if ( O is TP3DInterfacedPersistent ) then
    for Prop in O.Properties do
      AddCategory( Prop, Cat ).AddPropertyEditor( Prop );

  inherited Update;
end;

class function TP3DOIEditorDataProperties.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= AObj is TP3DInterfacedPersistent;
end;

{ TP3DOIEditorActionKey }

procedure TP3DOIEditorActionKey.SetKey(AValue: TP3DActionKey);
begin
  if FKey=AValue then Exit;
  FKey:=AValue;

  if ( Assigned( Key )) then
    begin
     Time.Prop:= AValue.Properties.GetPropByName( 'Time' );
     Value.Prop:= AValue.Properties.GetPropByName( 'Value' );
    end;
  Visible:= Assigned( Key );
end;

constructor TP3DOIEditorActionKey.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  Time:= TP3DPropertyEditorFloat.Create( Owner, Self );
  Time.Align:= alTop;
  Value:= TP3DPropertyEditorFloat.Create( Owner, Self );
  Value.Align:= alTop;
end;

procedure TP3DOIEditorActionKey.Update;
begin
  if ( Obj is TP3DActionKey ) then
    Key:= TP3DActionKey( Obj )
  else
    Key:= nil;
end;

class function TP3DOIEditorActionKey.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= TP3DOIEditorPageAction.IsCompatible( AObj );
end;

{ TP3DOIEditorActionChannel }

procedure TP3DOIEditorActionChannel.SetChannel(AValue: TP3DActionChannel);
begin
  FChannel:= AValue;
  if ( Assigned( Channel )) then
    begin
      PropStr.Prop:= AValue.Properties.GetPropByName( 'PropStr' );
      Keys.Prop:= AValue.Properties.GetPropByName( 'Keys' );
    end;
  Visible:= Assigned( Channel );
end;

constructor TP3DOIEditorActionChannel.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  PropStr:= TP3DPropertyEditorString.Create( Owner, Self );
  PropStr.Align:= alTop;
  Keys:= TP3DPropertyEditorActionKeyList.Create( Owner, Self );
  Keys.Align:= alTop;
  Keys.Height:= 100;
end;

procedure TP3DOIEditorActionChannel.Update;
begin
  if ( Obj is TP3DActionChannel ) then
    Channel:= TP3DActionChannel( Obj )
  else
    Channel:= nil;
end;

class function TP3DOIEditorActionChannel.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= TP3DOIEditorPageAction.IsCompatible( AObj );
end;


{ TP3DOIEditorPageAction }

procedure TP3DOIEditorPageAction.ChannelSelectionChange(Sender: TP3DListView; var NewSelection: TP3DListItem);
begin
  if ( Assigned( Action ) and Assigned( NewSelection )) then
    Channel.Channel:= Action.Channels[ Integer( NewSelection.Data )]
  else
    Channel:= nil;
end;

procedure TP3DOIEditorPageAction.KeysSelectionChange(Sender: TP3DListView; var NewSelection: TP3DListItem);
begin
  if ( Assigned( Action ) and Assigned( NewSelection )) then
    EditorKey.Key:= Action.Channels[ Integer( Channels.Selection.Data )].Keys[ Integer( NewSelection.Data )]
  else
    EditorKey.Key:= nil;
end;

constructor TP3DOIEditorPageAction.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);
  Channels:= TP3DPropertyEditorActionChannelList.Create( Owner, Self );
  Channels.Align:= alTop;
  Channels.Height:= 100;
  Channels.OnSelectionChange:= @ChannelSelectionChange;
  Channel:= TP3DOIEditorActionChannel.Create( Owner, Self );
  Channel.Align:= alTop;
  Channel.Keys.OnSelectionChange:= @KeysSelectionChange;
  EditorKey:= TP3DOIEditorActionKey.Create( Owner, Self );
  EditorKey.Align:= alTop;
  Channel.Visible:= False;
  EditorKey.Visible:= False;
end;

procedure TP3DOIEditorPageAction.Update;
begin
  inherited Update;

  if ( Obj is TP3DAction ) then
    FAction:= TP3DAction( Obj )
  else if ( Obj is TP3DDataBlock ) then
    //FAction:= TP3DDataBlock( Obj ).Action
  else
    FAction:= nil;
  if ( Assigned( Action )) then
    Channels.Prop:= Action.Properties.GetPropByName( 'Channels' );
end;

class function TP3DOIEditorPageAction.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= AObj is TP3DDataBlock;
end;

function TP3DOIEditorPageAction.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= inherited CreateButton(AParent, AMouseClick);
  Result.Caption:= P3DSymbolArmature;
end;

{ TP3DOIEditorPageMeshModifier }

procedure TP3DOIEditorPageObjectModifier.SelectionChange(Sender: TP3DListView; var NewSelection: TP3DListItem);
begin
  if ( Assigned( NewSelection )) then
    FEditorSelectedMod.Obj:= TP3DInterfacedPersistent( FEditorObjectModifiers.LProp.Values[ Integer( NewSelection.Data )])
  else
    FEditorSelectedMod.Obj:= nil;
end;

constructor TP3DOIEditorPageObjectModifier.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create(AData, AParent);

  FEditorObjectModifiers:= TP3DPropertyEditorObjectModifierList.Create( AData, Self );
  FEditorObjectModifiers.Align:= alTop;
  FEditorObjectModifiers.Height:= 100;
  FEditorObjectModifiers.OnSelectionChange:= @SelectionChange;

  FEditorSelectedMod:= TP3DOIEditorDataProperties.Create( AData, Self );
  FEditorSelectedMod.Borderless:= True;
  FEditorSelectedMod.Margin:= vec4( 0 );
  FEditorSelectedMod.Align:= alTop;
  FEditorSelectedMod.PageModeY:= pmExpand;
end;

procedure TP3DOIEditorPageObjectModifier.Update;
begin
  inherited Update;
  if ( Obj is TP3DObject ) then
    FEditorObjectModifiers.Prop:= TP3DObject( Obj ).Properties.GetPropByName( 'Modifiers' )
  else
    FEditorObjectModifiers.Prop:= nil;
  FEditorSelectedMod.Obj:= nil;
end;

class function TP3DOIEditorPageObjectModifier.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= ( AObj is TP3DObject );
end;

function TP3DOIEditorPageObjectModifier.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= inherited CreateButton(AParent, AMouseClick);
  Result.Caption:= P3DSymbolModifier;
end;


{$ENDIF}
