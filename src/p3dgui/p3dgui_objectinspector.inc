//p3dgui_objinspector.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}
type

  { TP3DOIEditorGroup }

  TP3DObjectInspector = class;
  TP3DOIEditorGroup = class ( TP3DGroupBox )
    private
      FObj: TInterfacedPersistent;
      FObjInsp: TP3DObjectInspector;

      procedure SetObj(AValue: TInterfacedPersistent); virtual;
      function AddPropertyEditor( Prop: TP3DPropertyAccess ): TP3DGraphicControl;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      procedure FastDestroy; override;

      procedure Update; virtual;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; virtual;

    published
      property Obj: TInterfacedPersistent read FObj write SetObj;
      property ObjInsp: TP3DObjectInspector read FObjInsp write FObjInsp;
  end;


  TP3DOIEditorList = specialize gP3DCustomObjectList < TP3DOIEditorGroup >;


  { TP3DOIEditorPage }

  TP3DOIEditorPage = class ( TP3DGraphicControl )
    private
      FButton: TP3DGroupButton;
      FGroups: TP3DOIEditorList;
      FObj: TInterfacedPersistent;
      FObjInsp: TP3DObjectInspector;

      procedure SetButton(AValue: TP3DGroupButton);
      procedure SetObj(AValue: TInterfacedPersistent);

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      procedure FastDestroy; override;

      procedure Update; virtual;
      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; virtual;

      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; virtual;
      procedure UpdateButton;

    published
      property Groups: TP3DOIEditorList read FGroups write FGroups;
      property Button: TP3DGroupButton read FButton write SetButton;
      property Obj: TInterfacedPersistent read FObj write SetObj;
      property ObjInsp: TP3DObjectInspector read FObjInsp write FObjInsp;
  end;

  TP3DOIPageList = specialize gP3DCustomObjectList < TP3DOIEditorPage >;

  TP3DObjectInspectorSetObjEvent = procedure ( Sender: TP3DObjectInspector; AValue: TInterfacedPersistent ) of object;

  { TP3DObjectInspector }

  TP3DObjectInspector = class( TP3DGraphicControl )
    private
      FActivePage: Integer;
      FContainer: TP3DGraphicControl;
      FObj: TInterfacedPersistent;
      FOnSetObj: TP3DObjectInspectorSetObjEvent;
      FPages: TP3DOIPageList;
      FScrollbar: TP3DScrollBar;
      FToolbar: TP3DGraphicControl;

      procedure SetActivePage(AValue: Integer);
      procedure SetObj(AValue: TInterfacedPersistent);
      procedure ScrollBarChange( Sender: TObject );

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      procedure FastDestroy; override;

      function AddPage(Page: TP3DOIEditorPage): TP3DOIEditorPage;
      procedure ButtonMouseClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
      procedure MouseWheel( deltax, deltay: Integer); override;

      procedure Update;
      procedure UpdateScrollBar;
      procedure Realign(); override;

    published
      property Obj: TInterfacedPersistent read FObj write SetObj;
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

function GetPropEdForPropRTTI( AProp: TP3DPropertyAccessRTTI ): TP3DGraphicControlClass;
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
  if ( AProp is TP3DPropertyAccessActor ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessActorList ) then
    Result:= TP3DPropertyEditorActorList
  else if ( AProp is TP3DPropertyAccessArmature ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessArmatureList ) then
    Result:= TP3DPropertyEditorArmatureList
  else if ( AProp is TP3DPropertyAccessAction ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessActionList ) then
    Result:= TP3DPropertyEditorActionList
  else if ( AProp is TP3DPropertyAccessActionChannelList ) then
    Result:= TP3DPropertyEditorActionChannelList
  else if ( AProp is TP3DPropertyAccessActionKeyList ) then
    Result:= TP3DPropertyEditorActionKeyList
  else if ( AProp is TP3DPropertyAccessCamera ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessCameraList ) then
    Result:= TP3DPropertyEditorCameraList
  else if ( AProp is TP3DPropertyAccessDataBlock ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessDataBlockList ) then
    Result:= TP3DPropertyEditorDataBlockList
  else if ( AProp is TP3DPropertyAccessFontBmp ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessFontBmpList ) then
    Result:= TP3DPropertyEditorFontBmpList
  else if ( AProp is TP3DPropertyAccessJoint ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessJointList ) then
    Result:= TP3DPropertyEditorJointList
  else if ( AProp is TP3DPropertyAccessLibrary ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessLibraryList ) then
    Result:= TP3DPropertyEditorLibraryList
  else if ( AProp is TP3DPropertyAccessLight ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessLightList ) then
    Result:= TP3DPropertyEditorLightList
  else if ( AProp is TP3DPropertyAccessMaterial ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessMaterialList ) then
    Result:= TP3DPropertyEditorMaterialList
  else if ( AProp is TP3DPropertyAccessMaterialMapList ) then
    Result:= TP3DPropertyEditorMaterialMapList
  else if ( AProp is TP3DPropertyAccessMesh ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessMeshList ) then
    Result:= TP3DPropertyEditorMeshList
  else if ( AProp is TP3DPropertyAccessScene ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessSceneList ) then
    Result:= TP3DPropertyEditorSceneList
  else if ( AProp is TP3DPropertyAccessShader ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessShaderList ) then
    Result:= TP3DPropertyEditorShaderList

  else if ( AProp is TP3DPropertyAccessTexture ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessTextureList ) then
    Result:= TP3DPropertyEditorTextureList
  else if ( AProp is TP3DPropertyAccessGraphicControl ) then
    Result:= TP3DPropertyEditorInterfacedPersistent
  else if ( AProp is TP3DPropertyAccessGraphicControlList ) then
    Result:= TP3DPropertyEditorGraphicControlList
  else if ( AProp is TP3DPropertyAccessFileWatch ) then
    Result:= TP3DPropertyEditorString

  else if ( AProp is TP3DPropertyAccessFloat ) then
    Result:= TP3DPropertyEditorFloat
  else if ( AProp is TP3DPropertyAccessInt ) then
    Result:= TP3DPropertyEditorInt
  else if ( AProp is TP3DPropertyAccessQuat ) then
    Result:= TP3DPropertyEditorVec4
  else if ( AProp is TP3DPropertyAccessRTTI ) then
    Result:= GetPropEdForPropRTTI( TP3DPropertyAccessRTTI( AProp ))
  else if ( AProp is TP3DPropertyAccessString ) then
    Result:= TP3DPropertyEditorString
  else if ( AProp is TP3DPropertyAccessVec2 ) then
    Result:= TP3DPropertyEditorVec2
  else if ( AProp is TP3DPropertyAccessVec3 ) then
    Result:= TP3DPropertyEditorVec3
  else if ( AProp is TP3DPropertyAccessVec4 ) then
    Result:= TP3DPropertyEditorVec4
  else if ( AProp is TP3DPropertyAccessColorRGB ) then
    Result:= TP3DPropertyEditorColorRGB
  else if ( AProp is TP3DPropertyAccessColorRGBA ) then
    Result:= TP3DPropertyEditorColorRGBA
  else
    Result:= TP3DPropertyEditorString
end;

{ TP3DOIEditorGroup }

procedure TP3DOIEditorGroup.SetObj(AValue: TInterfacedPersistent);
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
    if ( Assigned( Parent )) then
      Owner:= Parent.Owner
    else
      Owner:= P3DData;
    Result:= TP3DGraphicControl.Create( Owner, Parent );
    Result.Align:= alTop;
    Result.Width:= 20;
    Lbl:= TP3DLabel.Create( Owner, Result );
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

  Result:= Cl.Create( Owner, Self );

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

constructor TP3DOIEditorGroup.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  PageModeY:= pmExpand;
  CanCollapse:= True;
end;

procedure TP3DOIEditorGroup.FastDestroy;
begin
  Controls.Clear( True );
  inherited FastDestroy;
end;

class function TP3DOIEditorGroup.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= False;
end;

{ TP3DOIEditorPage }

procedure TP3DOIEditorPage.SetButton(AValue: TP3DGroupButton);
begin
  if FButton= AValue then Exit;
  FButton:= AValue;
  if ( Assigned( Button )) then
    UpdateButton;
end;

procedure TP3DOIEditorPage.SetObj(AValue: TInterfacedPersistent);
begin
  if FObj= AValue then Exit;
  FObj:= AValue;
  Update;
end;

constructor TP3DOIEditorPage.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  PageModeY:= pmExpand;
  Groups:= TP3DOIEditorList.Create();
end;

procedure TP3DOIEditorPage.FastDestroy;
begin
  Groups.Clear( True );
  Groups.Free;
  if ( P3DData.IsValid( Button )) then
    Button.Free;
  inherited FastDestroy;
end;

procedure TP3DOIEditorPage.Update;
begin
  UpdateButton;
end;

class function TP3DOIEditorPage.IsCompatible(AObj: TInterfacedPersistent): Boolean;
begin
  Result:= False;
end;

function TP3DOIEditorPage.CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton;
begin
  Result:= TP3DGroupButton.Create( nil, AParent );
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

procedure TP3DObjectInspector.SetObj(AValue: TInterfacedPersistent);
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

constructor TP3DObjectInspector.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );

  Toolbar:= TP3DGraphicControl.Create( nil, Self );
  Toolbar.Align:= alTop;
  Toolbar.Height:= 32;

  Scrollbar:= TP3DScrollBar.Create( nil, Self );
  Scrollbar.Align:= alRight;
  Scrollbar.Kind:= p3dsbVertical;
  Scrollbar.Width:= 10;
  Scrollbar.OnChange:= @ScrollBarChange;

  Pages:= TP3DOIPageList.Create;
  Container:= TP3DGraphicControl.Create( nil, Self );

  Container.Align:= alClient;

  AddPage( TP3DOIEditorPageObject.Create( nil, Container ));
  AddPage( TP3DOIEditorPageLight.Create( nil, Container ));
  AddPage( TP3DOIEditorPageCamera.Create( nil, Container ));
  with ( AddPage( TP3DOIEditorPageMaterial.Create( nil, Self )) as TP3DOIEditorPageMaterial ) do
    EditorPageMaps:= TP3DOIEditorPageMaps( AddPage( TP3DOIEditorPageMaps.Create( nil, Container )));
  AddPage( TP3DOIEditorPageGrid.Create( nil, Container ));
  AddPage( TP3DOIEditorPageProperties.Create( nil, Container ));
end;

procedure TP3DObjectInspector.FastDestroy;
begin
  //Pages.Clear( True );
  Pages.Free;
  //Toolbar.Controls.Clear( True );
  if ( P3DData.IsValid( Toolbar )) then
    Toolbar.Free;
  if ( P3DData.IsValid( Container )) then
    Container.Free;
  if ( P3DData.IsValid( Scrollbar )) then
    Scrollbar.Free;
  inherited FastDestroy;
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
