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
      constructor Create(const AOwner: TP3DObjectList= nil; const AParent: TP3DGraphicControl= nil);
      destructor Destroy; override;

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
      constructor Create(const AOwner: TP3DObjectList= nil; const AParent: TP3DGraphicControl= nil);
      destructor Destroy; override;

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
      FObj: TInterfacedPersistent;
      FPages: TP3DOIPageList;
      FToolbar: TP3DGraphicControl;

      procedure SetActivePage(AValue: Integer);
      procedure SetObj(AValue: TInterfacedPersistent);

    public
      constructor Create(const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

      function AddPage(Page: TP3DOIEditorPage): TP3DOIEditorPage;
      procedure ButtonMouseClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );

      procedure Update;

    published
      property Obj: TInterfacedPersistent read FObj write SetObj;
      property Pages: TP3DOIPageList read FPages write FPages;
      property Toolbar: TP3DGraphicControl read FToolbar write FToolbar;
      property ActivePage: Integer read FActivePage write SetActivePage;
  end;

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

constructor TP3DOIEditorGroup.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AOwner, AParent );
  CanCollapse:= True;
end;

destructor TP3DOIEditorGroup.Destroy;
begin
  Controls.Clear( True );
  inherited Destroy;
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

constructor TP3DOIEditorPage.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AOwner, AParent );
  Groups:= TP3DOIEditorList.Create();
end;

destructor TP3DOIEditorPage.Destroy;
begin
  Groups.Clear( True );
  Groups.Free;
  inherited Destroy;
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
end;

constructor TP3DObjectInspector.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;

  Toolbar:= TP3DGraphicControl.Create( nil, Self );
  Toolbar.Align:= alTop;
  Toolbar.Height:= 32;

  Pages:= TP3DOIPageList.Create;

  AddPage( TP3DOIEditorPageObject.Create( nil, Self ));
  AddPage( TP3DOIEditorPageLight.Create( nil, Self ));
  AddPage( TP3DOIEditorPageCamera.Create( nil, Self ));
  with ( AddPage( TP3DOIEditorPageMaterial.Create( nil, Self )) as TP3DOIEditorPageMaterial ) do
     EditorPageMaps:= TP3DOIEditorPageMaps( AddPage( TP3DOIEditorPageMaps.Create( nil, Self )));
{  AddEditor( TP3DOIEditorDataLight.Create( nil, Self ));
  AddEditor( TP3DOIEditorDataTexture.Create( nil, Self ));
  AddEditor( TP3DOIEditorDataMaterialBase.Create( nil, Self ));
  AddEditor( TP3DOIEditorDataMaterialMaps.Create( nil, Self ));
  AddEditor( TP3DOIEditorDataMesh.Create( nil, Self ));}
end;

destructor TP3DObjectInspector.Destroy;
begin
  Pages.Clear( True );
  Pages.Free;
  Toolbar.Controls.Clear( True );
  Toolbar.Free;
  inherited Destroy;
end;

function TP3DObjectInspector.AddPage(Page: TP3DOIEditorPage): TP3DOIEditorPage;
begin
  Page.Align:= alClient;
  Page.Obj:= Obj;
  Page.Visible:= False;
  Pages.Add( Page );
  Page.CreateButton( Toolbar, @ButtonMouseClick );
  if ( Pages.Count = 1 ) then
    Page.Button.Down:= True;
  Result:= Page;
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
end;


{$INCLUDE p3dgui_objinspector_editors.inc}
{$ENDIF}