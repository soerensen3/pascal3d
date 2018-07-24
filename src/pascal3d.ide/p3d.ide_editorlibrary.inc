{ p3d.ide_editorlibrary.inc }

{$IFDEF INTERFACE}
type

  { TP3DEditorLibrary }

  TP3DEditorLibrary = class ( TP3DEditor )
    protected
      FFileOpenDlg: TP3DStreamablePointer;
      FPopup: TP3DStreamablePointer;
      FPopupList: TP3DStreamablePointer;

      function GetFileOpenDlg: TP3DDialogFile;
      procedure OnFileOpenDlg( Sender: TObject );
      procedure FileOpenDlgCallback( Sender: TP3DModalWindow; Result: TP3DModalResult );
      procedure ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String); override;
      function GetProperty: TP3DStreamable; override;
      procedure SetProperty(AValue: TP3DStreamable); override;
      procedure PopupClick( Sender: TP3DMenuItem );
      function GetPopupMenu: TP3DPopupMenu;
      function GetPopupMenuList: TP3DPopupMenu;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      property FileOpenDlg: TP3DDialogFile read GetFileOpenDlg;
      property Popup: TP3DPopupMenu read GetPopupMenu;
      property PopupList: TP3DPopupMenu read GetPopupMenuList;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DEditorLibrary }

constructor TP3DEditorLibrary.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FName:= 'Libraries';
  FCaption:= P3DSymbolFile;
  FButtons:= P3DSymbolFileNew + LineEnding +
             P3DSymbolFileOpen + LineEnding +
             P3DSymbolFileSave + LineEnding +
             P3DSymbolFileClose;

  FFileOpenDlg:= TP3DStreamablePointer.Create( 'FileOpenDlg', TP3DDialogFile );
  Properties.Add( FFileOpenDlg );
  FFileOpenDlg.AsValue:= TP3DDialogFile.Create( P3DUI.Viewer.Controls );
  FileOpenDlg.Files.ShowSymbols:= True;
  FileOpenDlg.Directories.ShowSymbols:= True;
  FileOpenDlg.Files.Mask:= '*.p3d';

  FProp:= TP3DStreamablePointer.CreateGetSet( 'Active Library', @GetProperty, @SetProperty );
  FProp.FItemClass:= TP3DLibrary;

  Properties.Add( FProp );

  FPopup:= TP3DStreamablePointer.Create( 'Popup', TP3DPopupMenu );
  Properties.Add( FPopup );
  FPopup.AsValue:= TP3DPopupMenu.Create();
  Popup.OnItemClick:= @PopupClick;
  Popup.AutoWidth:= True;
  Popup.Items.Add( 'Save' );
  Popup.Items.Add( 'Close' );
  Popup.Items.Add( 'New Object' );
  Popup.Items.Add( 'New Scene' );
  Popup.Items.Add( 'New Node Material' );
  Popup.Items.Add( 'New Texture' );
  Popup.Items.Add( 'New Effect' );
  Popup.Items.Add( 'New Camera' );

  FPopupList:= TP3DStreamablePointer.Create( 'PopupList', TP3DPopupMenu );
  Properties.Add( FPopupList );
  FPopupList.AsValue:= TP3DPopupMenu.Create();
  PopupList.AutoWidth:= True;
  PopupList.Items.Add( 'New Library' );
  PopupList.Items.Add( 'Open Library' );

  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.AddFileOpenDlgHandler( @OnFileOpenDlg );
    P3DMainIntf.RegisterPopup( TP3DLibrary, Popup );
    P3DMainIntf.RegisterPopup( TP3DLibraryContainerList, PopupList );
  end;
end;

destructor TP3DEditorLibrary.Destroy;
begin
  if ( Assigned( P3DMainIntf )) then begin
    P3DMainIntf.UnregisterPopup( TP3DLibrary );
    P3DMainIntf.UnregisterPopup( TP3DLibraryContainerList );
    P3DMainIntf.RemoveFileOpenDlgHandler( @OnFileOpenDlg );
  end;
  inherited Destroy;
end;

function TP3DEditorLibrary.GetFileOpenDlg: TP3DDialogFile;
begin
  Result:= TP3DDialogFile( FFileOpenDlg.AsValue );
end;

procedure TP3DEditorLibrary.OnFileOpenDlg(Sender: TObject);
begin
  FileOpenDlg.ShowModal( @FileOpenDlgCallback );
end;

procedure TP3DEditorLibrary.FileOpenDlgCallback(Sender: TP3DModalWindow; Result: TP3DModalResult);
begin
  if ( FileOpenDlg.ModalResult = mrOk ) then
    P3DMainIntf.SetActiveLibrary( Self, TP3DLibrary.CreateFromFile( FileOpenDlg.FileName ));
end;

procedure TP3DEditorLibrary.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  if ( AButtonCaption = P3DSymbolFileOpen ) then
    P3DMainIntf.ShowFileOpenDlg( Self )
  else if ( AButtonCaption = P3DSymbolFileClose ) then
    //P3DMainIntf.;
  else if ( AButtonCaption = P3DSymbolFileSave ) then
    P3DMainIntf.ShowFileSaveDlg( Self )
end;

function TP3DEditorLibrary.GetProperty: TP3DStreamable;
begin
  Result:= P3DMainIntf.ActiveLibrary;
end;

procedure TP3DEditorLibrary.SetProperty(AValue: TP3DStreamable);
begin
  P3DMainIntf.SetActiveLibrary( Self, AValue );
end;

procedure TP3DEditorLibrary.PopupClick(Sender: TP3DMenuItem);
begin
  if ( Assigned( P3DMainIntf.ActiveLibrary )) then
    case Sender.Caption of
      'Save': P3DMainIntf.ShowFileSaveDlg( Self );
      'Close': ;
      'New Object': P3DMainIntf.ActiveLibrary.CreateNew( TP3DObject );
      'New Scene': P3DMainIntf.ActiveLibrary.CreateNew( TP3DScene );
      'New Effect': P3DMainIntf.ActiveLibrary.CreateNew( TP3DEffect );
      'New Node Material': P3DMainIntf.ActiveLibrary.CreateNew( TP3DMaterialNode );
      'New Texture': ;
      'New Camera': P3DMainIntf.ActiveLibrary.CreateNew( TP3DCamera );
    end;
  Popup.Hide;
end;

function TP3DEditorLibrary.GetPopupMenu: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopup.AsValue );
end;

function TP3DEditorLibrary.GetPopupMenuList: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FPopupList.AsValue );
end;


{$ENDIF}

