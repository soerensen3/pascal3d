//p3dgui_objinspector.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}
type

  { TP3DOIEditorGroup }

  TP3DOIEditorGroup = class ( TP3DGroupBox )
    private
      FObj: TInterfacedPersistent;

      procedure SetObj(AValue: TInterfacedPersistent); virtual;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      procedure FastDestroy; override;

      procedure Update; virtual;

      class function IsCompatible( AObj: TInterfacedPersistent ): Boolean; virtual;

    published
      property Obj: TInterfacedPersistent read FObj write SetObj;
  end;


  TP3DOIEditorList = specialize gP3DCustomObjectList < TP3DOIEditorGroup >;


  { TP3DOIEditorPage }

  TP3DOIEditorPage = class ( TP3DGraphicControl )
    private
      FButton: TP3DGroupButton;
      FGroups: TP3DOIEditorList;
      FObj: TInterfacedPersistent;

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
  end;

  TP3DOIPageList = specialize gP3DCustomObjectList < TP3DOIEditorPage >;

  { TP3DObjectInspector }

  TP3DObjectInspector = class( TP3DGraphicControl )
    private
      FActivePage: Integer;
      FContainer: TP3DGraphicControl;
      FObj: TInterfacedPersistent;
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

      procedure Update;
      procedure UpdateScrollBar;
      procedure Realign( var ClientArea: TP3DRect ); override;

    published
      property Obj: TInterfacedPersistent read FObj write SetObj;
      property Pages: TP3DOIPageList read FPages write FPages;
      property Toolbar: TP3DGraphicControl read FToolbar write FToolbar;
      property ActivePage: Integer read FActivePage write SetActivePage;
      property Container: TP3DGraphicControl read FContainer write FContainer;
      property Scrollbar: TP3DScrollBar read FScrollbar write FScrollbar;
  end;

  {$INCLUDE p3dgui_propertyeditor.inc}
  {$INCLUDE p3dgui_objinspector_editors.inc}
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DOIEditorGroup }

procedure TP3DOIEditorGroup.SetObj(AValue: TInterfacedPersistent);
begin
  if FObj=AValue then Exit;
  FObj:=AValue;
  Update;
end;

procedure TP3DOIEditorGroup.Update;
begin
  Visible:= IsCompatible( Obj );
end;

constructor TP3DOIEditorGroup.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
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
  Height:= 5000;
  Groups:= TP3DOIEditorList.Create();
end;

procedure TP3DOIEditorPage.FastDestroy;
begin
  Groups.Clear( True );
  Groups.Free;
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
  FObj:=AValue;
  Update;
end;

procedure TP3DObjectInspector.ScrollBarChange(Sender: TObject);
begin
  Container.Scroll:= vec2( Scroll.x, Scrollbar.Position );
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

  //Container.Height:= 5000;
  Container.Height:= Pages[ ActivePage ].Height;

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

  //Container.Align:= alTop;
  Container.Height:= 5000;
  Container.Align:= alClient;

  AddPage( TP3DOIEditorPageObject.Create( nil, Container ));
  AddPage( TP3DOIEditorPageLight.Create( nil, Container ));
  AddPage( TP3DOIEditorPageCamera.Create( nil, Container ));
  with ( AddPage( TP3DOIEditorPageMaterial.Create( nil, Self )) as TP3DOIEditorPageMaterial ) do
    EditorPageMaps:= TP3DOIEditorPageMaps( AddPage( TP3DOIEditorPageMaps.Create( nil, Container )));
  AddPage( TP3DOIEditorPageGrid.Create( nil, Container ));
end;

procedure TP3DObjectInspector.FastDestroy;
begin
  Pages.Clear( True );
  Pages.Free;
  Toolbar.Controls.Clear( True );
  Toolbar.Free;
  Container.Free;
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
  {if ( InRange( ActivePage, 0, Pages.Count - 1 ) and Assigned( Container )) then
    begin
      if ( Container.Controls.PageSize.y > 0 ) then
        begin
          Scrollbar.PageSize:= Container.Height / Container.Controls.PageSize.y;
          Scrollbar.Max:= Container.Controls.PageSize.y
        end
      else
        begin
          Scrollbar.PageSize:= 1;
          Scrollbar.Max:= 1;
        end;
    end;}
end;

procedure TP3DObjectInspector.Realign(var ClientArea: TP3DRect);
begin
  inherited Realign( ClientArea );
  if ( Assigned( Scrollbar ) and Assigned( Container )) then
    UpdateScrollBar;
end;


{$INCLUDE p3dgui_propertyeditor.inc}
{$INCLUDE p3dgui_objinspector_editors.inc}
{$ENDIF}
