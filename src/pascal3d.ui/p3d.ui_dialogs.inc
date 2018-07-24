{ p3dgui_dialogs.inc }

{$IFDEF INTERFACE}
type
  { TP3DDialogStreamableClass }

  TP3DDialogStreamableClass = class( TP3DModalWindow )
    protected
      FClassFactory: TP3DStreamablePointer;
      FOkBtn: TP3DStreamablePointer;

      procedure SetSelection( var NewSelection: TP3DTreeNode );
      function GetClassFactory: TP3DTreeViewClassFactory;
      function GetOkBtn: TP3DButton;
      procedure SetClassFactory(AValue: TP3DTreeViewClassFactory);
      procedure SetOkBtn(AValue: TP3DButton);
      procedure ButtonClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      property ClassFactory: TP3DTreeViewClassFactory read GetClassFactory write SetClassFactory;
      property OkBtn: TP3DButton read GetOkBtn write SetOkBtn;
  end;

  { TP3DDialogStreamableClass }

  { TP3DDialogStreamableBrowseInstances }

  TP3DDialogStreamableBrowseInstances = class( TP3DModalWindow )
    protected
      FObjects: TP3DStreamablePointer;
      FOkBtn: TP3DStreamablePointer;

      function GetObjects: TP3DTreeViewObjects;
      procedure SetObjects(AValue: TP3DTreeViewObjects);
      procedure SetSelection( var NewSelection: TP3DTreeNode );
      function GetOkBtn: TP3DButton;
      procedure SetOkBtn(AValue: TP3DButton);
      procedure ButtonClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      property Objects: TP3DTreeViewObjects read GetObjects write SetObjects;
      property OkBtn: TP3DButton read GetOkBtn write SetOkBtn;
  end;

  { TP3DDialogFile }

  TP3DDialogFile = class( TP3DModalWindow )
    protected
      FFileName: String;
      FDirectories: TP3DStreamablePointer;
      FFiles: TP3DStreamablePointer;
      FOkBtn: TP3DStreamablePointer;

      function GetDirectories: TP3DTreeViewDirectory;
      function GetFiles: TP3DListViewFile;
      procedure SetDirectories(AValue: TP3DTreeViewDirectory);
      procedure SetFiles(AValue: TP3DListViewFile);
      procedure SetSelection( AListView: TP3DListView; var NewSelection: TP3DListItem );
      function GetOkBtn: TP3DButton;
      procedure SetOkBtn(AValue: TP3DButton);
      procedure ButtonClick( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      property Directories: TP3DTreeViewDirectory read GetDirectories write SetDirectories;
      property Files: TP3DListViewFile read GetFiles write SetFiles;

      property OkBtn: TP3DButton read GetOkBtn write SetOkBtn;
      property FileName: String read FFileName write FFileName;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DDialogStreamableClass }

function TP3DDialogStreamableClass.GetClassFactory: TP3DTreeViewClassFactory;
begin
  Result:= TP3DTreeViewClassFactory( FClassFactory.AsValue );
end;

function TP3DDialogStreamableClass.GetOkBtn: TP3DButton;
begin
  Result:= TP3DButton( FOkBtn.AsValue );
end;

procedure TP3DDialogStreamableClass.SetClassFactory(AValue: TP3DTreeViewClassFactory);
begin
  FClassFactory.AsValue:= AValue;
end;

procedure TP3DDialogStreamableClass.SetOkBtn(AValue: TP3DButton);
begin
  FOkBtn.AsValue:= AValue;
end;

procedure TP3DDialogStreamableClass.ButtonClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if ( Sender = OkBtn ) then
    ModalResult:= mrOk
  else
    ModalResult:= mrCancel;
end;

procedure TP3DDialogStreamableClass.SetSelection(var NewSelection: TP3DTreeNode);
begin
  OkBtn.Enabled:= Assigned( NewSelection );
end;

constructor TP3DDialogStreamableClass.Create(const AOwner: IP3DStreamableContainer);
var
  CancelBtn: TP3DButton;
begin
  inherited Create(AOwner);
  Caption:= 'Select base class';
  FClassFactory:= TP3DStreamablePointer.Create( 'ClassFactory', TP3DTreeViewClassFactory );
  FOkBtn:= TP3DStreamablePointer.Create( 'OkBtn', TP3DButton );
  Properties.Add([ FOkBtn, FClassFactory ]);
  ClassFactory:= TP3DTreeViewClassFactory.Create( Controls );
  ClassFactory.Align:= alClient;
  ClassFactory.OnSelectionChange:= @SetSelection;
  with ( TP3DGraphicControl.Create( Controls )) do
    begin
      Align:= alBottom;
      PageModeX:= pmStretch;
      OkBtn:= TP3DButton.Create( Controls );
      OkBtn.Caption:= 'OK';
      OkBtn.OnMouseClick:= @ButtonClick;
      CancelBtn:= TP3DButton.Create( Controls );
      CancelBtn.Caption:= 'Cancel';
      CancelBtn.Left:= OkBtn.Width;
      CancelBtn.OnMouseClick:= @ButtonClick;
    end;
end;

function TP3DDialogStreamableBrowseInstances.GetObjects: TP3DTreeViewObjects;
begin
  Result:= TP3DTreeViewObjects( FObjects.AsValue );
end;

procedure TP3DDialogStreamableBrowseInstances.SetObjects(AValue: TP3DTreeViewObjects);
begin
  FObjects.AsValue:= AValue;
end;

procedure TP3DDialogStreamableBrowseInstances.SetSelection(var NewSelection: TP3DTreeNode);
begin
  OkBtn.Enabled:= Assigned( NewSelection );
end;

function TP3DDialogStreamableBrowseInstances.GetOkBtn: TP3DButton;
begin
  Result:= TP3DButton( FOkBtn.AsValue );
end;

procedure TP3DDialogStreamableBrowseInstances.SetOkBtn(AValue: TP3DButton);
begin
  FOkBtn.AsValue:= AValue;
end;

procedure TP3DDialogStreamableBrowseInstances.ButtonClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X,
  Y: Float);
begin
  if ( Sender = OkBtn ) then
    ModalResult:= mrOk
  else
    ModalResult:= mrCancel;
end;

constructor TP3DDialogStreamableBrowseInstances.Create(const AOwner: IP3DStreamableContainer);
var
  CancelBtn: TP3DButton;
begin
  inherited Create(AOwner);
  Caption:= 'Select base class';
  FObjects:= TP3DStreamablePointer.Create( 'Objects', TP3DTreeViewObjects );
  FOkBtn:= TP3DStreamablePointer.Create( 'OkBtn', TP3DButton );
  Properties.Add([ FOkBtn, FObjects ]);
  Objects:= TP3DTreeViewObjects.Create( Controls );
  Objects.Align:= alClient;
  Objects.OnSelectionChange:= @SetSelection;
  with ( TP3DGraphicControl.Create( Controls )) do
    begin
      Align:= alBottom;
      PageModeX:= pmStretch;
      OkBtn:= TP3DButton.Create( Controls );
      OkBtn.Caption:= 'OK';
      OkBtn.OnMouseClick:= @ButtonClick;
      CancelBtn:= TP3DButton.Create( Controls );
      CancelBtn.Caption:= 'Cancel';
      CancelBtn.Left:= OkBtn.Width;
      CancelBtn.OnMouseClick:= @ButtonClick;
    end;
end;



{ TP3DDialogFile }

function TP3DDialogFile.GetDirectories: TP3DTreeViewDirectory;
begin
  Result:= TP3DTreeViewDirectory( FDirectories.AsValue );
end;

function TP3DDialogFile.GetFiles: TP3DListViewFile;
begin
  Result:= TP3DListViewFile( FFiles.AsValue );
end;

procedure TP3DDialogFile.SetDirectories(AValue: TP3DTreeViewDirectory);
begin
  FDirectories.AsValue:= AValue;
end;

procedure TP3DDialogFile.SetFiles(AValue: TP3DListViewFile);
begin
  FFiles.AsValue:= AValue;
end;

procedure TP3DDialogFile.SetSelection(AListView: TP3DListView; var NewSelection: TP3DListItem);
begin
  OkBtn.Enabled:= Assigned( NewSelection );
  if ( Assigned( NewSelection )) then
    FFileName:= Files.Directory + DirectorySeparator + NewSelection.Caption;
end;

function TP3DDialogFile.GetOkBtn: TP3DButton;
begin
  Result:= TP3DButton( FOkBtn.AsValue );
end;

procedure TP3DDialogFile.SetOkBtn(AValue: TP3DButton);
begin
  FOkBtn.AsValue:= AValue;
end;

procedure TP3DDialogFile.ButtonClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if ( Sender = OkBtn ) then
    ModalResult:= mrOk
  else
    ModalResult:= mrCancel;
end;

constructor TP3DDialogFile.Create(const AOwner: IP3DStreamableContainer);
var
  CancelBtn: TP3DButton;
begin
  inherited Create(AOwner);

  Caption:= 'Select file';

  Width:= 500;
  Height:= 400;

  FDirectories:= TP3DStreamablePointer.Create( 'Directories', TP3DTreeViewDirectory );
  FFiles:= TP3DStreamablePointer.Create( 'Files', TP3DListViewFile );
  FOkBtn:= TP3DStreamablePointer.Create( 'OkBtn', TP3DButton );
  Properties.Add([ FOkBtn, FDirectories, FFiles ]);

  with ( TP3DGraphicControl.Create( Controls )) do begin
    Align:= alBottom;
    PageModeX:= pmStretch;
    OkBtn:= TP3DButton.Create( Controls );
    OkBtn.Caption:= 'OK';
    OkBtn.OnMouseClick:= @ButtonClick;
    CancelBtn:= TP3DButton.Create( Controls );
    CancelBtn.Caption:= 'Cancel';
    CancelBtn.Left:= OkBtn.Width;
    CancelBtn.OnMouseClick:= @ButtonClick;
  end;

  Directories:= TP3DTreeViewDirectory.Create( Controls );
  Directories.Align:= alLeft;
  Directories.Width:= 200;
  TP3DSplitter.Create( Controls ).Align:= alLeft;
  Files:= TP3DListViewFile.Create( Controls );
  Files.Align:= alClient;
  Files.OnSelectionChange:= @SetSelection;
  Directories.ListViewFile:= Files;
end;
{$ENDIF}

