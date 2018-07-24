//p3dgui_objinspector.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}
type

  { TP3DOIEditorGroup }

  TP3DObjectInspector = class;
  TP3DOIEditorGroup = class ( TP3DGroupBox )
    private
      FObj: TP3DStreamablePointer;

      function GetObj: TP3DStreamable;
      function AddPropertyEditor( Prop: TP3DPropertyAccess ): TP3DGraphicControl;
      procedure SetObj(AValue: TP3DStreamable); virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      procedure Update; override;
      procedure Draw(); override;

      class function IsCompatible( AObj: TP3DStreamable ): Boolean; virtual;

    published
      property Obj: TP3DStreamable read GetObj write SetObj;
  end;


  TP3DOIEditorList = specialize gP3DCustomObjectList < TP3DOIEditorGroup >;


  { TP3DOIEditorPage }

  TP3DOIEditorPage = class ( TP3DGraphicControl )
    private
      FButton: TP3DStreamablePointer;
      FGroups: TP3DOIEditorList;
      FObj: TP3DStreamablePointer;

      function GetButton: TP3DGroupButton;
      function GetObj: TP3DStreamable;
      procedure SetObj(AValue: TP3DStreamable); virtual;
      procedure SetButton(AValue: TP3DGroupButton);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure Update; override;
      class function IsCompatible( AObj: TP3DStreamable ): Boolean; virtual;

      function CreateButton(AParent: TP3DGraphicControl; AMouseClick: TP3DGUIMouseClick): TP3DGroupButton; virtual;
      procedure UpdateButton;

    published
      property Groups: TP3DOIEditorList read FGroups write FGroups;
      property Button: TP3DGroupButton read GetButton write SetButton;
      property Obj: TP3DStreamable read GetObj write SetObj;
  end;

  TP3DOIPageList = specialize gP3DCustomObjectList < TP3DOIEditorPage >;

  TP3DObjectInspectorSetObjEvent = procedure ( Sender: TP3DObjectInspector; AValue: TP3DInterfacedObject ) of object;

  { TP3DObjectInspector }

  TP3DObjectInspector = class( TP3DGraphicControl )
    private
      FActivePage: Integer;
      FContainer: TP3DStreamablePointer;
      FObj: TP3DStreamablePointer;
      FOnSetObj: TP3DObjectInspectorSetObjEvent;
      FPages: TP3DOIPageList;
      FScrollbar: TP3DStreamablePointer;
      FToolbar: TP3DStreamablePointer;

      function GetContainer: TP3DGraphicControl;
      function GetObj: TP3DStreamable;
      function GetScrollbar: TP3DScrollBar;
      function GetToolbar: TP3DGraphicControl;
      procedure SetActivePage(AValue: Integer);
      procedure SetContainer(AValue: TP3DGraphicControl);
      procedure ScrollBarChange( Sender: TObject );
      procedure SetObj(AValue: TP3DStreamable);
      procedure SetScrollbar(AValue: TP3DScrollBar);
      procedure SetToolbar(AValue: TP3DGraphicControl);
      procedure OnIntfSetSelection( Sender: TObject );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      function AddPage(Page: TP3DOIEditorPage): TP3DOIEditorPage;
      procedure ButtonMouseClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float );
      procedure MouseWheel( deltax, deltay: Integer); override;

      procedure Update; override;
      procedure UpdateScrollBar;
      procedure Realign(); override;

    published
      property Obj: TP3DStreamable read GetObj write SetObj;
      property Pages: TP3DOIPageList read FPages write FPages;
      property Toolbar: TP3DGraphicControl read GetToolbar write SetToolbar;
      property ActivePage: Integer read FActivePage write SetActivePage;
      property Container: TP3DGraphicControl read GetContainer write SetContainer;
      property Scrollbar: TP3DScrollBar read GetScrollbar write SetScrollbar;
      property OnSetObj: TP3DObjectInspectorSetObjEvent read FOnSetObj write FOnSetObj;
  end;

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
    tkSet: Result := TP3DPropertyEditorSet;
    tkClass: Result := TP3DPropertyEditorString;
    tkBool: Result := TP3DPropertyEditorBoolean;
  end;
end;

function GetPropEdForProp( AProp: TP3DPropertyAccess ): TP3DGraphicControlClass;
begin
  if (( AProp is TP3DPropAccessStreamable ) and ( TP3DPropAccessStreamable( AProp ).ItemClass = TP3DTexture )) then
    Result:= TP3DPropertyEditorTexture
  else if ( AProp is TP3DStreamableContainer ) then
    Result:= TP3DPropertyEditorStreamableProperties
  else if ( AProp is IP3DPropAccessStreamable ) then
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
  else if ( AProp is TP3DPropAccessBoolean ) then
    Result:= TP3DPropertyEditorBoolean
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

procedure TP3DOIEditorGroup.SetObj(AValue: TP3DStreamable);
begin
  if Obj=AValue then Exit;
  FObj.AsValue:=AValue;
  Update;
end;

function TP3DOIEditorGroup.GetObj: TP3DStreamable;
begin
  Result:= FObj.AsValue;
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

var
//  Cnt: TP3DGraphicControl;
  Cl: TP3DGraphicControlClass;
begin
  if ( not Assigned( Prop )) then
    exit;

  //Cnt:= AddContainer( Self, Prop.Name );

  Cl:= GetPropEdForProp( Prop );
  if ( not Assigned( Cl )) then begin
    Result:= TP3DButton.Create( Self.Controls );
    TP3DButton( Result ).Caption:= 'No property editor found for prop "' + Prop.Name + ': ' + Prop.ClassName + '"';
  end else
    Result:= Cl.Create( Self.Controls );

  if ( Result is IP3DPropertyEditor ) then
    ( Result as IP3DPropertyEditor ).Prop:= Prop;
  Result.Align:= alTop;
end;

procedure TP3DOIEditorGroup.Update;
begin
  Visible:= IsCompatible( Obj );
  inherited Update;
end;

procedure TP3DOIEditorGroup.Draw();
var
  hw: Float;
  r: TP3DRect;
begin
  Canvas.RenderRect( vec2( 0, 0 ), WidthHeight, Color[ cp300 ]);
  Canvas.GetValidText( Caption, FCaptionTxt,
    P3DFontManager[ Canvas.Font.Name, Canvas.Font.Size ]);
  if ( not Assigned( FCaptionTxt  )) then
    hw:= 0
  else
    hw:= FCaptionTxt.Height / 2;
  Canvas.Font.Assign( Font );
  Canvas.RenderText( FCaptionTxt, vec2( hw * 2, 2 ));

  if ( CanCollapse ) then
    begin
      Canvas.GetValidText( FCollapseSymbol, FCollapseSymbolTxt, P3DFontManager[ P3DSymbolFontName, Canvas.Font.Size ]);
      if ( P3DRect( 3, 3, FCollapseSymbolTxt.Width, FCollapseSymbolTxt.Height ).PtInRect( vec2( UIViewer.MouseX, UIViewer.MouseY ) - Canvas.TopLeft )) then
        Canvas.Font.Color:= P3DThemeDefault.Accent[ cp500 ]
      else
        Canvas.Font.Color:= P3DThemeDefault.Primary[ cp500 ];
      Canvas.RenderText(
        FCollapseSymbolTxt, vec2( 3 ));
    end;
  if ( CanClose ) then
    begin
      Canvas.GetValidText( FCloseSymbol, FCloseSymbolTxt,
                P3DFontManager[ P3DSymbolFontName, Canvas.Font.Size ]);
      r:= P3DRect( vec2( Width - FCloseSymbolTxt.Width - 3, 3 ), vec2( FCloseSymbolTxt.Width, FCloseSymbolTxt.Height ));
      if ( r.PtInRect( vec2( UIViewer.MouseX, UIViewer.MouseY ) - Canvas.TopLeft )) then
        Canvas.Font.Color:= P3DThemeDefault.Accent[ cp500 ]
      else
        Canvas.Font.Color:= P3DThemeDefault.Primary[ cp500 ];
      Canvas.RenderRectShadow( r.TopLeft, r.BottomRight, 5, Color[ cp500 ]);
      Canvas.RenderText( FCloseSymbolTxt, vec2( Width - FCloseSymbolTxt.Width - 3, 3 ));
    end;
end;

constructor TP3DOIEditorGroup.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Margin:= vec4( 2, 18, 2, 2 );
  FObj:= TP3DStreamablePointer.Create( 'Obj', TP3DStreamable );
  Properties.Add( FObj );
  PageModeY:= pmExpand;
  CanCollapse:= True;
end;

class function TP3DOIEditorGroup.IsCompatible(AObj: TP3DStreamable): Boolean;
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

function TP3DOIEditorPage.GetObj: TP3DStreamable;
begin
  Result:= FObj.AsValue;
end;

procedure TP3DOIEditorPage.SetObj(AValue: TP3DStreamable);
begin
  if Obj= AValue then Exit;
  FObj.AsValue:= AValue;
  Update;
end;

constructor TP3DOIEditorPage.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FButton:= TP3DStreamablePointer.Create( 'Button', TP3DGraphicControl );
  FObj:= TP3DStreamablePointer.Create( 'Obj', TP3DStreamable );
  Properties.Add([ FButton, FObj ]);
  PageModeY:= pmExpand;
  Groups:= TP3DOIEditorList.Create();
  Groups.OwnsObjects:= False;
end;

destructor TP3DOIEditorPage.Destroy;
begin
  Groups.Free;
  //if ( P3DDataBlockCache.IsValid( Button )) then
  inherited Destroy;
end;

procedure TP3DOIEditorPage.Update;
begin
  UpdateButton;
  inherited Update;
end;

class function TP3DOIEditorPage.IsCompatible(AObj: TP3DStreamable): Boolean;
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

procedure TP3DObjectInspector.SetObj(AValue: TP3DStreamable);
begin
  if Obj=AValue then Exit;

  if ( Assigned( OnSetObj )) then
    OnSetObj( Self, AValue );

  FObj.AsValue:=AValue;

  Update;
end;

procedure TP3DObjectInspector.ScrollBarChange(Sender: TObject);
begin
  Container.Scroll:= vec2( Scroll.x, Scrollbar.Percentage );
end;

procedure TP3DObjectInspector.SetScrollbar(AValue: TP3DScrollBar);
begin
  FScrollbar.AsValue:= AValue;
end;

procedure TP3DObjectInspector.SetToolbar(AValue: TP3DGraphicControl);
begin
  FToolbar.AsValue:= AValue;
end;

procedure TP3DObjectInspector.OnIntfSetSelection(Sender: TObject);
begin
  Obj:= P3DMainIntf.Selection;
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

function TP3DObjectInspector.GetContainer: TP3DGraphicControl;
begin
  Result:= TP3DGraphicControl( FContainer.AsValue );
end;

function TP3DObjectInspector.GetObj: TP3DStreamable;
begin
  Result:= FObj.AsValue;
end;

function TP3DObjectInspector.GetScrollbar: TP3DScrollBar;
begin
  Result:= TP3DScrollBar( FScrollbar.AsValue );
end;

function TP3DObjectInspector.GetToolbar: TP3DGraphicControl;
begin
  Result:= TP3DGraphicControl( FToolbar.AsValue );
end;

procedure TP3DObjectInspector.SetContainer(AValue: TP3DGraphicControl);
begin
  FContainer.AsValue:= AValue;
end;

constructor TP3DObjectInspector.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );

  FObj:= TP3DStreamablePointer.Create( 'Obj', TP3DStreamable );
  Properties.Add( FObj );

  FToolbar:= TP3DStreamablePointer.Create( 'Toolbar', TP3DGraphicControl );
  Properties.Add( FToolbar );
  Toolbar:= TP3DGraphicControl.Create( Self.Controls );
  Toolbar.Align:= alTop;
  Toolbar.Height:= 32;
  Toolbar.Visible:= False;

  FScrollbar:= TP3DStreamablePointer.Create( 'Scrollbar', TP3DScrollBar );
  Properties.Add( FScrollbar );
  Scrollbar:= TP3DScrollBar.Create( Self.Controls );
  Scrollbar.Align:= alRight;
  Scrollbar.Kind:= p3dsbVertical;
  Scrollbar.Width:= 10;
  Scrollbar.OnChange:= @ScrollBarChange;

  Pages:= TP3DOIPageList.Create;
  Pages.OwnsObjects:= False;
  FContainer:= TP3DStreamablePointer.Create( 'Container', TP3DGraphicControl );
  Properties.Add( FContainer );
  Container:= TP3DGraphicControl.Create( Self.Controls );

  Container.Align:= alClient;

  {AddPage( TP3DOIEditorPageObject.Create( Container.Controls ));
  AddPage( TP3DOIEditorPageLight.Create( Container.Controls ));
  AddPage( TP3DOIEditorPageCamera.Create( Container.Controls ));
  with ( AddPage( TP3DOIEditorPageMaterial.Create( Container.Controls )) as TP3DOIEditorPageMaterial ) do
    EditorPageMaps:= TP3DOIEditorPageMaps( AddPage( TP3DOIEditorPageMaps.Create( Container.Controls )));
  AddPage( TP3DOIEditorPageObjectModifier.Create( Container.Controls ));
  AddPage( TP3DOIEditorPageGrid.Create( Container.Controls ));
  AddPage( TP3DOIEditorPageAction.Create( Container.Controls ));}
  AddPage( TP3DOIEditorPageProperties.Create( Container.Controls ));
  if ( Assigned( P3DMainIntf )) then
    P3DMainIntf.AddSetSelectionHandler( @OnIntfSetSelection );
end;

destructor TP3DObjectInspector.Destroy;
begin
  if ( Assigned( P3DMainIntf )) then
    P3DMainIntf.RemoveSetSelectionHandler( @OnIntfSetSelection );
  //Pages.Clear( True );
  Pages.Free;
  //Toolbar.Controls.Clear( True );
  //if ( P3DDataBlockCache.IsValid( Toolbar )) then
   // Toolbar.Free;
  //if ( P3DDataBlockCache.IsValid( Container )) then
   // Container.Free;
  //if ( P3DDataBlockCache.IsValid( Scrollbar )) then
   // Scrollbar.Free;
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

procedure TP3DObjectInspector.ButtonMouseClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);
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
  if (( P3DUI.LastMouseOverCtrl = Self ) or
     ( Assigned( P3DUI.LastMouseOverCtrl ) and ( not P3DUI.LastMouseOverCtrl.Controls.CanScrollY ) and
     ( Self.Controls.ChildOf( P3DUI.LastMouseOverCtrl )))) then
    if ( MouseRay( UIViewer.MouseX, UIViewer.MouseY )) then
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
  inherited Update;
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

{$ENDIF}
