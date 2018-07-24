{ p3d.ide_editorscene.inc }

{$IFDEF INTERFACE}

{ TP3DEditorScene }

TP3DEditorScene = class ( TP3DEditor )
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
{ TP3DEditorScene }

function TP3DEditorScene.GetPopupMenu: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopup.AsValue );
end;

function TP3DEditorScene.GetPopupMenuList: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopupList.AsValue );
end;

constructor TP3DEditorScene.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FName:= 'Scene';
  FCaption:= P3DSymbolScene;
  FButtons:= P3DSymbolSceneNew + LineEnding +
             P3DSymbolSceneDelete + LineEnding +
             P3DSymbolObject;
  FProp:= TP3DStreamablePointer.CreateGetSet( 'Active Scene', @GetProperty, @SetProperty );
  FProp.FItemClass:= TP3DScene;

  Properties.Add( FProp );

  FPopup:= TP3DStreamablePointer.Create( 'Popup', TP3DPopupMenu );
  Properties.Add( FPopup );
  FPopup.AsValue:= TP3DPopupMenu.Create();
//  Popup.OnItemClick:= @PopupClick;
  Popup.AutoWidth:= True;
  Popup.Items.Add( 'Delete' );
  Popup.Items.Add( 'Add New Object' );

  FPopupList:= TP3DStreamablePointer.Create( 'PopupList', TP3DPopupMenu );
  Properties.Add( FPopupList );
  FPopupList.AsValue:= TP3DPopupMenu.Create();
  PopupList.AutoWidth:= True;
  PopupList.Items.Add( 'New Scene' );

  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.RegisterPopup( TP3DScene, Popup );
    P3DMainIntf.RegisterPopup( TP3DSceneContainerList, PopupList );
  end;
end;

destructor TP3DEditorScene.Destroy;
begin
  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.UnregisterPopup( TP3DScene );
    P3DMainIntf.UnregisterPopup( TP3DSceneContainerList );
  end;

  inherited Destroy;
end;

procedure TP3DEditorScene.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  if ( AButtonCaption = P3DSymbolSceneNew ) then
//    P3DMainIntf.NewScene( Self )
  else if ( AButtonCaption = P3DSymbolSceneDelete ) then
//    P3DMainIntf.DeleteScene( Self );
  else if ( AButtonCaption = P3DSymbolObject ) then
    if ( Assigned( P3DMainIntf.ActiveScene )) then
      P3DMainIntf.ActiveScene.Objects.Add( TP3DObject.CreateBlock( P3DMainIntf.ActiveScene.OwnerLib ));
end;

function TP3DEditorScene.GetProperty: TP3DStreamable;
begin
  Result:= P3DMainIntf.ActiveScene;
end;

procedure TP3DEditorScene.SetProperty(AValue: TP3DStreamable);
begin
  P3DMainIntf.SetActiveScene( Self, AValue );
end;

{$ENDIF}

