{ p3d.ide_editormaterial.inc }

{$IFDEF INTERFACE}

{ TP3DEditorMaterial }

TP3DEditorMaterial = class ( TP3DEditor )
  protected
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
{ TP3DEditorMaterial }

function TP3DEditorMaterial.GetPopupMenu: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopup.AsValue );
end;

function TP3DEditorMaterial.GetPopupMenuList: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopupList.AsValue );
end;

constructor TP3DEditorMaterial.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FName:= 'Material';
  FCaption:= P3DSymbolMaterial;
  FButtons:= P3DSymbolMaterialNew + LineEnding +
             P3DSymbolMaterialDelete + LineEnding +
             P3DSymbolTextureNew;
  FProp:= TP3DStreamablePointer.CreateGetSet( 'Active Material', @GetProperty, @SetProperty );
  FProp.FItemClass:= TP3DMaterial;

  Properties.Add( FProp );

  FPopup:= TP3DStreamablePointer.Create( 'Popup', TP3DPopupMenu );
  Properties.Add( FPopup );
  FPopup.AsValue:= TP3DPopupMenu.Create();
  //Popup.OnItemClick:= @PopupClick;
  Popup.AutoWidth:= True;
  Popup.Items.Add( 'Delete' );
  Popup.Items.Add( 'Add Map' );

  FPopupList:= TP3DStreamablePointer.Create( 'PopupList', TP3DPopupMenu );
  Properties.Add( FPopupList );
  FPopupList.AsValue:= TP3DPopupMenu.Create();
  PopupList.AutoWidth:= True;
  PopupList.Items.Add( 'New Node Material' );

  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.RegisterPopup( TP3DMaterial, Popup );
    P3DMainIntf.RegisterPopup( TP3DMaterialContainerList, PopupList );
  end;
end;

destructor TP3DEditorMaterial.Destroy;
begin
  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.UnregisterPopup( TP3DMaterial );
    P3DMainIntf.UnregisterPopup( TP3DMaterialContainerList );
  end;
  inherited Destroy;
end;

procedure TP3DEditorMaterial.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  if ( AButtonCaption = P3DSymbolMaterialNew ) then
    P3DMainIntf.NewMaterial( Self )
  else if ( AButtonCaption = P3DSymbolMaterialDelete ) then
    P3DMainIntf.DeleteMaterial( Self )
  else if ( AButtonCaption = P3DSymbolTextureNew ) then
    P3DMainIntf.NewMaterialMap( Self );
end;

function TP3DEditorMaterial.GetProperty: TP3DStreamable;
begin
  Result:= P3DMainIntf.ActiveMaterial;
end;

procedure TP3DEditorMaterial.SetProperty(AValue: TP3DStreamable);
begin
  P3DMainIntf.SetActiveMaterial( Self, AValue );
end;

{$ENDIF}

