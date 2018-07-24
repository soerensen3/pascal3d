{ p3d.ide_editortexture.inc }

{$IFDEF INTERFACE}

{ TP3DDialogNewTexture }

TP3DDialogNewTexture = class( TP3DModalWindow )
  protected
    FDataType: TP3DTextureDataType;
    FTextureName: String;
    FCreateBtn: TP3DStreamablePointer;
    FTextureHeight: Integer;
    FTextureWidth: Integer;

    function GetCreateBtn: TP3DButton;
    procedure SetCreateBtn(AValue: TP3DButton);
    procedure ButtonClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);

  public
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

    property CreateBtn: TP3DButton read GetCreateBtn write SetCreateBtn;
    property TextureName: String read FTextureName write FTextureName;
    property TextureWidth: Integer read FTextureWidth write FTextureWidth;
    property TextureHeight: Integer read FTextureHeight write FTextureHeight;

  published
    property DataType: TP3DTextureDataType read FDataType write FDataType;
end;


{ TP3DEditorTexture }

TP3DEditorTexture = class ( TP3DEditor )
private
  function GetPopupMenu: TP3DPopupMenu;
  function GetPopupMenuList: TP3DPopupMenu;
  protected
    FFileOpenDlg: TP3DStreamablePointer;
    FNewTextureDlg: TP3DStreamablePointer;
    FPopup: TP3DStreamablePointer;
    FPopupList: TP3DStreamablePointer;

    function GetNewTextureDlg: TP3DDialogNewTexture;
    function GetFileOpenDlg: TP3DDialogFile;
    procedure FileOpenDlgCallback( Sender: TP3DModalWindow; Result: TP3DModalResult );
    procedure NewTextureDlgCallback( Sender: TP3DModalWindow; Result: TP3DModalResult );
    function GetProperty: TP3DStreamable; override;
    procedure SetProperty(AValue: TP3DStreamable); override;

  public
    constructor Create( const AOwner: IP3DStreamableContainer=nil ); override;
    destructor Destroy; override;

    procedure ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String); override;

    property FileOpenDlg: TP3DDialogFile read GetFileOpenDlg;
    property NewTextureDlg: TP3DDialogNewTexture read GetNewTextureDlg;
    property Popup: TP3DPopupMenu read GetPopupMenu;
    property PopupList: TP3DPopupMenu read GetPopupMenuList;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DEditorTexture }

constructor TP3DEditorTexture.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FName:= 'Texture';
  FCaption:= P3DSymbolTexture;
  FButtons:= P3DSymbolTextureNew + LineEnding +
             P3DSymbolFileOpen + LineEnding +
             P3DSymbolTextureDelete;
  FProp:= TP3DStreamablePointer.CreateGetSet( 'Active Texture', @GetProperty, @SetProperty );
  FProp.FItemClass:= TP3DTexture;

  FFileOpenDlg:= TP3DStreamablePointer.Create( 'FileOpenDlg', TP3DDialogFile );
  Properties.Add( FFileOpenDlg );
  FFileOpenDlg.AsValue:= TP3DDialogFile.Create();
  FileOpenDlg.Files.ShowSymbols:= True;
  FileOpenDlg.Directories.ShowSymbols:= True;
  FileOpenDlg.Files.Mask:= '*.png';

  FNewTextureDlg:= TP3DStreamablePointer.Create( 'NewTextureDlg', TP3DDialogNewTexture );
  Properties.Add( FNewTextureDlg );
  FNewTextureDlg.AsValue:= TP3DDialogNewTexture.Create();

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
  PopupList.Items.Add( 'New Texture' );

  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.RegisterPopup( TP3DTexture, Popup );
    P3DMainIntf.RegisterPopup( TP3DTextureContainerList, PopupList );
  end;
end;

destructor TP3DEditorTexture.Destroy;
begin
  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.UnregisterPopup( TP3DTexture );
    P3DMainIntf.UnregisterPopup( TP3DTextureContainerList );
  end;

  inherited Destroy;
end;

procedure TP3DEditorTexture.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  if ( AButtonCaption = P3DSymbolTextureNew ) then
    NewTextureDlg.ShowModal( @NewTextureDlgCallback )
  else if ( AButtonCaption = P3DSymbolFileOpen ) then
    FileOpenDlg.ShowModal( @FileOpenDlgCallback )
  else if ( AButtonCaption = P3DSymbolTextureDelete ) then
    //P3DMainIntf.DeleteTexture( Self );
end;

function TP3DEditorTexture.GetProperty: TP3DStreamable;
begin
  Result:= P3DMainIntf.ActiveTexture;
end;

procedure TP3DEditorTexture.SetProperty(AValue: TP3DStreamable);
begin
  P3DMainIntf.SetActiveTexture( Self, AValue );
end;

function TP3DEditorTexture.GetPopupMenu: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopup.AsValue );
end;

function TP3DEditorTexture.GetPopupMenuList: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopupList.AsValue );
end;

function TP3DEditorTexture.GetNewTextureDlg: TP3DDialogNewTexture;
begin
  Result:= TP3DDialogNewTexture( FNewTextureDlg.AsValue );
end;

function TP3DEditorTexture.GetFileOpenDlg: TP3DDialogFile;
begin
  Result:= TP3DDialogFile( FFileOpenDlg.AsValue );
end;

procedure TP3DEditorTexture.FileOpenDlgCallback(Sender: TP3DModalWindow; Result: TP3DModalResult);
begin
  if ( FileOpenDlg.ModalResult = mrOk ) then
    P3DMainIntf.SetActiveTexture( Self, TP3DTexture.CreateFromFile( FileOpenDlg.FileName ));
end;

procedure TP3DEditorTexture.NewTextureDlgCallback(Sender: TP3DModalWindow; Result: TP3DModalResult);
var
  Tex: TP3DTexture;
begin
  if ( Result = mrOk ) then begin
    Tex:= TP3DTexture.CreateFromEmpty( NewTextureDlg.TextureWidth, NewTextureDlg.TextureHeight, 4, NewTextureDlg.DataType, P3DMainIntf.ActiveLibrary );
    Tex.Name:= NewTextureDlg.TextureName;
    P3DMainIntf.SetActiveTexture( Self, Tex );
  end;
end;


{ TP3DDialogNewTexture }

function TP3DDialogNewTexture.GetCreateBtn: TP3DButton;
begin
  Result:= TP3DButton( FCreateBtn.AsValue );
end;

procedure TP3DDialogNewTexture.SetCreateBtn(AValue: TP3DButton);
begin
  FCreateBtn.AsValue:= AValue;
end;

procedure TP3DDialogNewTexture.ButtonClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if ( Sender = CreateBtn ) then
    ModalResult:= mrOk
  else
    ModalResult:= mrCancel;
end;

constructor TP3DDialogNewTexture.Create(const AOwner: IP3DStreamableContainer);
var
  CancelBtn: TP3DButton;
begin
  inherited Create(AOwner);

  Caption:= 'Create texture...';

  Width:= 500;
  Height:= 400;

  FCreateBtn:= TP3DStreamablePointer.Create( 'OkBtn', TP3DButton );
  Properties.Add([ FCreateBtn ]);

  with ( TP3DGraphicControl.Create( Controls )) do begin
    Align:= alBottom;
    PageModeX:= pmStretch;
    CreateBtn:= TP3DButton.Create( Controls );
    CreateBtn.Caption:= 'Create';
    CreateBtn.OnMouseClick:= @ButtonClick;
    CancelBtn:= TP3DButton.Create( Controls );
    CancelBtn.Caption:= 'Cancel';
    CancelBtn.Left:= CreateBtn.Width;
    CancelBtn.OnMouseClick:= @ButtonClick;
  end;

  with ( TP3DPropertyEditorInt.Create( Controls )) do begin
     Prop:= TP3DPropAccessInt.CreateField( 'Width', @TextureWidth );
     Self.Properties.Add( Prop );
     Align:= alTop;
  end;

  with ( TP3DPropertyEditorInt.Create( Controls )) do begin
     Prop:= TP3DPropAccessInt.CreateField( 'Height', @TextureHeight );
     Self.Properties.Add( Prop );
     Align:= alTop;
  end;

  with ( TP3DPropertyEditorString.Create( Controls )) do begin
     Prop:= TP3DPropAccessString.CreateField( 'Texture name', @TextureName );
     Self.Properties.Add( Prop );
     Align:= alTop;
  end;

  with ( TP3DPropertyEditorEnum.Create( Controls )) do begin
     Prop:= TP3DPropAccessRTTI.Create( Self, 'DataType' );
     Self.Properties.Add( Prop );
     Align:= alTop;
  end;

  TextureWidth:= 512;
  TextureHeight:= 512;
  TextureName:= 'Untitled_Texture';
  DataType:= dtByte;
end;
{$ENDIF}

