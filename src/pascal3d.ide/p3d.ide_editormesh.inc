{ p3d.ide_editorMesh.inc }

{$IFDEF INTERFACE}

{ TP3DEditorMesh }

TP3DEditorMesh = class ( TP3DEditor )
  private
    FPopup: TP3DStreamablePointer;
    FPopupList: TP3DStreamablePointer;
    function GetPopupMenu: TP3DPopupMenu;
    function GetPopupMenuList: TP3DPopupMenu;

  public
    constructor Create( const AOwner: IP3DStreamableContainer=nil ); override;
    destructor Destroy; override;

    procedure ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String); override;
    function GetProperty: TP3DStreamable; override;
    procedure SetProperty(AValue: TP3DStreamable); override;

    property Popup: TP3DPopupMenu read GetPopupMenu;
    property PopupList: TP3DPopupMenu read GetPopupMenuList;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DEditorMesh }

function TP3DEditorMesh.GetPopupMenu: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopup.AsValue );
end;

function TP3DEditorMesh.GetPopupMenuList: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopupList.AsValue );
end;

constructor TP3DEditorMesh.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FName:= 'Mesh';
  FCaption:= P3DSymbolMesh;
  FButtons:= P3DSymbolMeshNew + LineEnding +
             P3DSymbolMeshDelete + LineEnding +
             P3DSymbolMaterial;
  FProp:= TP3DStreamablePointer.CreateGetSet( 'Active Mesh', @GetProperty, @SetProperty );
  FProp.FItemClass:= TP3DMesh;

  Properties.Add( FProp );

  FPopup:= TP3DStreamablePointer.Create( 'Popup', TP3DPopupMenu );
  Properties.Add( FPopup );
  FPopup.AsValue:= TP3DPopupMenu.Create();
  //Popup.OnItemClick:= @PopupClick;
  Popup.AutoWidth:= True;
  Popup.Items.Add( 'Delete' );

  FPopupList:= TP3DStreamablePointer.Create( 'PopupList', TP3DPopupMenu );
  Properties.Add( FPopupList );
  FPopupList.AsValue:= TP3DPopupMenu.Create();
  PopupList.AutoWidth:= True;
  PopupList.Items.Add( 'Assign Active Material' );

  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.RegisterPopup( TP3DMesh, Popup );
    P3DMainIntf.RegisterPopup( TP3DMeshContainerList, PopupList );
  end;
end;

destructor TP3DEditorMesh.Destroy;
begin
  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.UnregisterPopup( TP3DMesh );
    P3DMainIntf.UnregisterPopup( TP3DMeshContainerList );
  end;

  inherited Destroy;
end;

procedure TP3DEditorMesh.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  if ( AButtonCaption = P3DSymbolMeshNew ) then
//    P3DMainIntf.NewMesh( Self )
  else if ( AButtonCaption = P3DSymbolMeshDelete ) then
//    P3DMainIntf.DeleteMesh( Self );
  else if ( AButtonCaption = P3DSymbolMaterial ) then
    P3DMainIntf.AssignMeshMaterial( Self );
end;

function TP3DEditorMesh.GetProperty: TP3DStreamable;
begin
  Result:= P3DMainIntf.ActiveMesh;
end;

procedure TP3DEditorMesh.SetProperty(AValue: TP3DStreamable);
begin
  P3DMainIntf.SetActiveMesh( Self, AValue );
end;

{$ENDIF}

