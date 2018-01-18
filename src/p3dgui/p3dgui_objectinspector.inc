//p3dgui_objinspector.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}
type

  { TP3DOIEditorGroup }

  TP3DObjectInspector = class;
  TP3DOIEditorGroup = class ( TP3DGroupBox )
    private
      FObj: TP3DInterfacedObject;
      FObjInsp: TP3DObjectInspector;

      procedure SetObj(AValue: TP3DInterfacedObject); virtual;
      function AddPropertyEditor( Prop: TP3DPropertyAccess ): TP3DGraphicControl;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Update; virtual;

      class function IsCompatible( AObj: TP3DInterfacedObject ): Boolean; virtual;

    published
      property Obj: TP3DInterfacedObject read FObj write SetObj;
      property ObjInsp: TP3DObjectInspector read FObjInsp write FObjInsp;
  end;


  TP3DOIEditorList = specialize gP3DCustomObjectList < TP3DOIEditorGroup >;


  { TP3DOIEditorPage }

  TP3DOIEditorPage = class ( TP3DGraphicControl )
    private
      FButton: TP3DStreamablePointer;
      FGroups: TP3DOIEditorList;
      FObj: TP3DInterfacedObject;
      FObjInsp: TP3DObjectInspector;

      function GetButton: TP3DGroupButton;
      procedure SetButton(AValue: TP3DGroupButton);
      procedure SetObj(AValue: TP3DInterfacedObject);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Update; virtual;
      class function IsCompatible( AObj: TP3DInterfacedObject ): Boolean; virtual;

      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; virtual;
      procedure UpdateButton;

    published
      property Groups: TP3DOIEditorList read FGroups write FGroups;
      property Button: TP3DGroupButton read GetButton write SetButton;
      property Obj: TP3DInterfacedObject read FObj write SetObj;
      property ObjInsp: TP3DObjectInspector read FObjInsp write FObjInsp;
  end;

  TP3DOIPageList = specialize gP3DCustomObjectList < TP3DOIEditorPage >;

  TP3DObjectInspectorSetObjEvent = procedure ( Sender: TP3DObjectInspector; AValue: TP3DInterfacedObject ) of object;

  { TP3DObjectInspector }

  TP3DObjectInspector = class( TP3DGraphicControl )
    private
      FActivePage: Integer;
      FContainer: TP3DGraphicControl;
      FObj: TP3DInterfacedObject;
      FOnSetObj: TP3DObjectInspectorSetObjEvent;
      FPages: TP3DOIPageList;
      FScrollbar: TP3DScrollBar;
      FToolbar: TP3DGraphicControl;

      procedure SetActivePage(AValue: Integer);
      procedure SetObj(AValue: TP3DInterfacedObject);
      procedure ScrollBarChange( Sender: TObject );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      function AddPage(Page: TP3DOIEditorPage): TP3DOIEditorPage;
      procedure ButtonMouseClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
      procedure MouseWheel( deltax, deltay: Integer); override;

      procedure Update;
      procedure UpdateScrollBar;
      procedure Realign(); override;

    published
      property Obj: TP3DInterfacedObject read FObj write SetObj;
      property Pages: TP3DOIPageList read FPages write FPages;
      property Toolbar: TP3DGraphicControl read FToolbar write FToolbar;
      property ActivePage: Integer read FActivePage write SetActivePage;
      property Container: TP3DGraphicControl read FContainer write FContainer;
      property Scrollbar: TP3DScrollBar read FScrollbar write FScrollbar;
      property OnSetObj: TP3DObjectInspectorSetObjEvent read FOnSetObj write FOnSetObj;
  end;

  {$INCLUDE p3dgui_propertyeditor.inc}
  {$INCLUDE p3dgui_objinspector_editors.inc}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

function GetPropEdForPropRTTI( AProp: TP3DPropAccessRTTI ): TP3DGraphicControlClass;
begin

  case AProp.PropInfo^.PropType^.Kind of
    tkInteger: Result := TP3DPropertyEditorInt;
    tkChar: Result := TP3DPropertyEditorString;
    tkEnumeration: Result := TP3DPropertyEditorEnum;
    tkFloat: Result:= TP3DPropertyEditorFloat;
    tkAstring: Result := TP3DPropertyEditorString;
    tkWString: Result := TP3DPropertyEditorString;
    tkWChar: Result := TP3DPropertyEditorString;
    tkVariant: Result := TP3DPropertyEditorString;
    tkInt64: Result := TP3DPropertyEditorInt;
    tkString: Result := TP3DPropertyEditorString;
//    tkSet: Result := TP3DPropertyEditorString;
    tkClass: Result := TP3DPropertyEditorString;
    tkBool: Result := TP3DPropertyEditorBoolean;
  end;
end;

function GetPropEdForProp( AProp: TP3DPropertyAccess ): TP3DGraphicControlClass;
begin
  if ( AProp is IP3DPropAccessStreamable ) then
    Result:= TP3DPropertyEditorStreamable
  else if ( AProp is TP3DStreamableListBase ) then
    Result:= TP3DPropertyEditorStreamableList
{  if ( AProp is TP3DPropAccessObject ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessObjectList ) then
    Result:= TP3DPropertyEditorObjectList
  else if ( AProp is TP3DPropAccessArmature ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessArmatureList ) then
    Result:= TP3DPropertyEditorArmatureList
  else if ( AProp is TP3DPropAccessAction ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessActionList ) then
    Result:= TP3DPropertyEditorActionList
  else if ( AProp is TP3DPropAccessActionChannelList ) then
    Result:= TP3DPropertyEditorActionChannelList
  else if ( AProp is TP3DPropAccessActionKeyList ) then
    Result:= TP3DPropertyEditorActionKeyList
  else if ( AProp is TP3DPropAccessCamera ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessCameraList ) then
    Result:= TP3DPropertyEditorCameraList
  else if ( AProp is TP3DPropAccessDataBlock ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessDataBlockList ) then
    Result:= TP3DPropertyEditorDataBlockList
  else if ( AProp is TP3DPropAccessFontBmp ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessFontBmpList ) then
    Result:= TP3DPropertyEditorFontBmpList
//  else if ( AProp is TP3DPropAccessJoint ) then
//    Result:= TP3DPropertyEditorInterfacedPersistent
//  else if ( AProp is TP3DPropAccessJointList ) then
//    Result:= TP3DPropertyEditorJointList
  else if ( AProp is TP3DPropAccessLibrary ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessLibraryList ) then
    Result:= TP3DPropertyEditorLibraryList
  else if ( AProp is TP3DPropAccessLight ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessLightList ) then
    Result:= TP3DPropertyEditorLightList
  else if ( AProp is TP3DPropAccessMaterial ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessMaterialList ) then
    Result:= TP3DPropertyEditorMaterialList
  else if ( AProp is TP3DPropAccessMaterialMapList ) then
    Result:= TP3DPropertyEditorMaterialMapList
  else if ( AProp is TP3DPropAccessMesh ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessMeshList ) then
    Result:= TP3DPropertyEditorMeshList
  else if ( AProp is TP3DPropAccessScene ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessSceneList ) then
    Result:= TP3DPropertyEditorSceneList}
  {else if ( AProp is TP3DPropAccessShader ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessShaderList ) then
    Result:= TP3DPropertyEditorShaderList

  else if ( AProp is TP3DPropAccessTexture ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessTextureList ) then
    Result:= TP3DPropertyEditorTextureList
  else if ( AProp is TP3DPropAccessGraphicControl ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropAccessGraphicControlList ) then
    Result:= TP3DPropertyEditorGraphicControlList}
  else if ( AProp is TP3DPropAccessFilePointer ) then
    Result:= TP3DPropertyEditorString

  else if ( AProp is TP3DPropAccessFloat ) then
    Result:= TP3DPropertyEditorFloat
  else if ( AProp is TP3DPropAccessInt ) then
    Result:= TP3DPropertyEditorInt
  else if ( AProp is TP3DPropAccessQuat ) then
    Result:= TP3DPropertyEditorVec4
  else if ( AProp is TP3DPropAccessQuatList ) then
    Result:= TP3DPropertyEditorQuatList
  else if ( AProp is TP3DPropAccessRTTI ) then
    Result:= GetPropEdForPropRTTI( TP3DPropAccessRTTI( AProp ))
  else if ( AProp is TP3DPropAccessString ) then
    Result:= TP3DPropertyEditorString
  else if ( AProp is TP3DPropAccessVec2 ) then
    Result:= TP3DPropertyEditorVec2
  else if ( AProp is TP3DPropAccessVec2List ) then
    Result:= TP3DPropertyEditorVec2List
  else if ( AProp is TP3DPropAccessVec3 ) then
    Result:= TP3DPropertyEditorVec3
  else if ( AProp is TP3DPropAccessVec3List ) then
    Result:= TP3DPropertyEditorVec3List
  else if ( AProp is TP3DPropAccessVec4 ) then
    Result:= TP3DPropertyEditorVec4
  else if ( AProp is TP3DPropAccessVec4List ) then
    Result:= TP3DPropertyEditorVec4List
  else if ( AProp is TP3DPropAccessColorRGB ) then
    Result:= TP3DPropertyEditorColorRGB
  else if ( AProp is TP3DPropAccessColorRGBA ) then
    Result:= TP3DPropertyEditorColorRGBA
  else
    Result:= TP3DPropertyEditorString
end;

{ TP3DOIEditorGroup }

procedure TP3DOIEditorGroup.SetObj(AValue: TP3DInterfacedObject);
begin
  if FObj=AValue then Exit;
  FObj:=AValue;
  Update;
end;

function TP3DOIEditorGroup.AddPropertyEditor(Prop: TP3DPropertyAccess): TP3DGraphicControl;
  function AddContainer( Parent: TP3DGraphicControl; Title: String ): TP3DGraphicControl;
  var
    Lbl: TP3DLabel;
    Owner: TP3DLibrary;
  begin
    Result:= TP3DGraphicControl.Create( Parent.Controls );
    Result.Align:= alTop;
    Result.Width:= 20;
    Lbl:= TP3DLabel.Create( Result.Controls );
    Lbl.Align:= alLeft;
    Lbl.Caption:= Title + ' '; //One space for safety distance - looks better!
    Lbl.AutoSize:= True;
  end;

  function FindObjInsp( Ctrl: TP3DGraphicControl ): TP3DObjectInspector;
  begin
    while Assigned( Ctrl ) and not ( Ctrl is TP3DObjectInspector ) do
      Ctrl:= Ctrl.Parent;
    if ( Ctrl is TP3DObjectInspector ) then
      Result:= TP3DObjectInspector( Ctrl )
    else
      Result:= nil;
  end;

var
//  Cnt: TP3DGraphicControl;
  Cl: TP3DGraphicControlClass;
  OI: TP3DObjectInspector;
begin
  if ( not Assigned( Prop )) then
    exit;

  //Cnt:= AddContainer( Self, Prop.Name );

  Cl:= GetPropEdForProp( Prop );

  Result:= Cl.Create( Self.Controls );

  OI:= FindObjInsp( Self );
  if ( Result is IP3DPropertyEditor ) then
    begin
      ( Result as IP3DPropertyEditor ).Prop:= Prop;
      ( Result as IP3DPropertyEditor ).ObjInsp:= OI;
    end;
  Result.Align:= alTop;
end;

procedure TP3DOIEditorGroup.Update;
begin
  Visible:= IsCompatible( Obj );
end;

constructor TP3DOIEditorGroup.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  PageModeY:= pmExpand;
  CanCollapse:= True;
end;

destructor TP3DOIEditorGroup.Destroy;
begin
  Controls.Clear;
  inherited Destroy;
end;

class function TP3DOIEditorGroup.IsCompatible(AObj: TP3DInterfacedObject): Boolean;
begin
  Result:= False;
end;

{ TP3DOIEditorPage }

procedure TP3DOIEditorPage.SetButton(AValue: TP3DGroupButton);
begin
  if FButton.AsValue= AValue then Exit;
  FButton.AsValue:= AValue;
  if ( Assigned( Button )) then
    UpdateButton;
end;

function TP3DOIEditorPage.GetButton: TP3DGroupButton;
begin
  Result:= TP3DGroupButton( FButton.AsValue );
end;

procedure TP3DOIEditorPage.SetObj(AValue: TP3DInterfacedObject);
begin
  if FObj= AValue then Exit;
  FObj:= AValue;
  Update;
end;

constructor TP3DOIEditorPage.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FButton:= TP3DStreamablePointer.Create( 'Button', TP3DGraphicControl );
  Properties.Add( FButton );
  PageModeY:= pmExpand;
  Groups:= TP3DOIEditorList.Create();
end;

destructor TP3DOIEditorPage.Destroy;
begin
  Groups.Clear;
  Groups.Free;
  //if ( P3DDataBlockCache.IsValid( Button )) then
  inherited Destroy;
end;

procedure TP3DOIEditorPage.Update;
begin
  UpdateButton;
end;

class function TP3DOIEditorPage.IsCompatible(AObj: TP3DInterfacedObject): Boolean;
begin
  Result:= False;
end;

function TP3DOIEditorPage.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= TP3DGroupButton.Create( AParent.Controls );
  Result.Align:= alLeft;
  Result.HAlignment:= haCenter;
  Result.VAlignment:= vaCenter;
  Result.Font.Name:= P3DSymbolFontName;
  Result.Font.Size:= 24;
  //Result.Caption:= Symbol;
  Result.Width:= 32;
  Result.Group:= 'Pages';
  Result.OnMouseClick:= AMouseClick;
  Button:= Result;
end;

procedure TP3DOIEditorPage.UpdateButton;
begin
  Button.Visible:= IsCompatible( Obj );
end;


{ TP3DObjectInspector }

procedure TP3DObjectInspector.SetObj(AValue: TP3DInterfacedObject);
begin
  if FObj=AValue then Exit;

  if ( Assigned( OnSetObj )) then
    OnSetObj( Self, AValue );

  FObj:=AValue;

  Update;
end;

procedure TP3DObjectInspector.ScrollBarChange(Sender: TObject);
begin
  Container.Scroll:= vec2( Scroll.x, Scrollbar.Percentage );
end;

procedure TP3DObjectInspector.SetActivePage(AValue: Integer);
var
  i: Integer;
begin
  if FActivePage= AValue then Exit;
  FActivePage:= AValue;

  if ( not Pages[ ActivePage ].Button.Down ) then
    Pages[ ActivePage ].Button.Down:= True;

  for i:= 0 to Pages.Count - 1 do
    Pages[ i ].Visible:= i = ActivePage;

  UpdateScrollBar;
end;

constructor TP3DObjectInspector.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  Toolbar:= TP3DGraphicControl.Create( Self.Controls );
  Toolbar.Align:= alTop;
  Toolbar.Height:= 32;

  Scrollbar:= TP3DScrollBar.Create( Self.Controls );
  Scrollbar.Align:= alRight;
  Scrollbar.Kind:= p3dsbVertical;
  Scrollbar.Width:= 10;
  Scrollbar.OnChange:= @ScrollBarChange;

  Pages:= TP3DOIPageList.Create;
  Container:= TP3DGraphicControl.Create( Self.Controls );

  Container.Align:= alClient;

  AddPage( TP3DOIEditorPageObject.Create( Container.Controls ));
  AddPage( TP3DOIEditorPageLight.Create( Container.Controls ));
  AddPage( TP3DOIEditorPageCamera.Create( Container.Controls ));
  with ( AddPage( TP3DOIEditorPageMaterial.Create( Container.Controls )) as TP3DOIEditorPageMaterial ) do
    EditorPageMaps:= TP3DOIEditorPageMaps( AddPage( TP3DOIEditorPageMaps.Create( Container.Controls )));
  AddPage( TP3DOIEditorPageObjectModifier.Create( Container.Controls ));
  AddPage( TP3DOIEditorPageGrid.Create( Container.Controls ));
  AddPage( TP3DOIEditorPageAction.Create( Container.Controls ));
  AddPage( TP3DOIEditorPageProperties.Create( Container.Controls ));
end;

destructor TP3DObjectInspector.Destroy;
begin
  //Pages.Clear( True );
  Pages.Free;
  //Toolbar.Controls.Clear( True );
  //if ( P3DDataBlockCache.IsValid( Toolbar )) then
    Toolbar.Free;
  //if ( P3DDataBlockCache.IsValid( Container )) then
    Container.Free;
  //if ( P3DDataBlockCache.IsValid( Scrollbar )) then
    Scrollbar.Free;
  inherited Destroy;
end;

function TP3DObjectInspector.AddPage(Page: TP3DOIEditorPage): TP3DOIEditorPage;
begin
  Page.Align:= alTop;
  Page.Obj:= Obj;
  Page.Visible:= False;
  Pages.Add( Page );
  Page.CreateButton( Toolbar, @ButtonMouseClick );
  if ( Pages.Count = 1 ) then
    Page.Button.Down:= True;
  Result:= Page;
  //Page.Height:= Page.Controls.PageSize.Y;
end;

procedure TP3DObjectInspector.ButtonMouseClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
var
  i: Integer;
begin
  if ( mb1 ) then
    for i:= 0 to Pages.Count - 1 do
      if ( Pages[ i ].Button = Sender ) then
        begin
          Pages[ i ].Visible:= True;
          ActivePage:= i;
        end
      else
        Pages[ i ].Visible:= False;
end;

procedure TP3DObjectInspector.MouseWheel(deltax, deltay: Integer);
begin
  inherited MouseWheel( deltax, deltay );
  if (( P3DGUIManager.LastMouseOverCtrl = Self ) or
     ( Assigned( P3DGUIManager.LastMouseOverCtrl ) and ( not P3DGUIManager.LastMouseOverCtrl.Controls.CanScrollY ) and
     ( Self.Controls.ChildOf( P3DGUIManager.LastMouseOverCtrl )))) then
    if ( MouseRay( P3DInput.Mouse.X, P3DInput.Mouse.Y )) then
      Scrollbar.Percentage:= Scrollbar.Percentage - deltay / 10;
end;


procedure TP3DObjectInspector.Update;
var
  i: Integer;
begin
  Visible:= Assigned( Obj );

  if ( not Pages[ ActivePage ].IsCompatible( Obj )) then
    ActivePage:= 0;

  for i:= 0 to Pages.Count - 1 do
    begin
      Pages[ i ].Obj:= Obj;
      Pages[ i ].Visible:= i = ActivePage;
    end;
  UpdateScrollBar;
end;

procedure TP3DObjectInspector.UpdateScrollBar;
begin
  if ( InRange( ActivePage, 0, Pages.Count - 1 ) and Assigned( Container )) then
    begin
      if ( Container.PageSize.y > 0 ) then
        begin
          Scrollbar.ClientPageSize:= Container.Controls.PageRect.Height / Container.PageSize.y;
          Scrollbar.Max:= Container.PageSize.y
        end
      else
        begin
          Scrollbar.ClientPageSize:= 1;
          Scrollbar.Max:= 1;
        end;
    end;
end;

procedure TP3DObjectInspector.Realign();
begin
  inherited Realign();
  if ( Assigned( Scrollbar ) and Assigned( Container )) then
    UpdateScrollBar;
end;



{$INCLUDE p3dgui_propertyeditor.inc}
{$INCLUDE p3dgui_objinspector_editors.inc}
{$ENDIF}
