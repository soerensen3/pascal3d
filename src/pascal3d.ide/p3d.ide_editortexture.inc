{ p3d.ide_editortexture.inc }

{$IFDEF INTERFACE}

{ TP3DEditorTexture }

TP3DEditorTexture = class ( TP3DEditor )
  protected
    FFileOpenDlg: TP3DStreamablePointer;

    function GetFileOpenDlg: TP3DDialogFile;
    procedure FileOpenDlgCallback( Sender: TP3DModalWindow; Result: TP3DModalResult );
    function GetProperty: TP3DStreamable; override;
    procedure SetProperty(AValue: TP3DStreamable); override;

  public
    constructor Create( const AOwner: IP3DStreamableContainer=nil ); override;

    procedure ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String); override;

    property FileOpenDlg: TP3DDialogFile read GetFileOpenDlg;
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

  Properties.Add( FProp );
end;

procedure TP3DEditorTexture.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  if ( AButtonCaption = P3DSymbolTextureNew ) then
    //P3DMainIntf.NewTexture( Self )
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

function TP3DEditorTexture.GetFileOpenDlg: TP3DDialogFile;
begin
  Result:= TP3DDialogFile( FFileOpenDlg.AsValue );
end;

procedure TP3DEditorTexture.FileOpenDlgCallback(Sender: TP3DModalWindow; Result: TP3DModalResult);
begin
  if ( FileOpenDlg.ModalResult = mrOk ) then
    P3DMainIntf.SetActiveTexture( Self, TP3DTexture.CreateFromFile( FileOpenDlg.FileName ));
end;

{$ENDIF}

