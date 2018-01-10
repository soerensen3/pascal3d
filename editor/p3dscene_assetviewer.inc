{$IFDEF INTERFACE}
  { TP3DSAssetPanel }

  TP3DSAssetPanel = class( TP3DGraphicControl )
    private
      FAssetView: TP3DListViewFile;
      FDirectoryView: TP3DTreeViewDirectory;

    public
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

    published
      property DirectoryView: TP3DTreeViewDirectory read FDirectoryView write FDirectoryView;
      property AssetView: TP3DListViewFile read FAssetView write FAssetView;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}



{ TP3DSAssetPanel }

constructor TP3DSAssetPanel.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock( AData, AParent );
  Height:= 200;
  FDirectoryView:= TP3DTreeViewDirectory.CreateBlock( nil, Self );
  FAssetView:= TP3DListViewFile.CreateBlock( nil, Self );
  FDirectoryView.Directory:= P3DSearchPaths.BaseDir.FileName;
  FDirectoryView.ShowSymbols:= True;
  FDirectoryView.ShowFiles:= True;
  FDirectoryView.Mask:= '*.p3d';
  FDirectoryView.Align:= alLeft;
  FDirectoryView.Width:= 200;
  FDirectoryView.AllowDrag:= True;
  FAssetView.ShowSymbols:= True;
  FAssetView.TreeViewDirectory:= FDirectoryView;
  FAssetView.ShowFolderUp:= True;
  FAssetView.Align:= alClient;
  FAssetView.AllowDrag:= True;
  FAssetView.Mask:= '*.p3d';
  FAssetView.AllowOpenP3D:= True;
end;

destructor TP3DSAssetPanel.Destroy;
begin
  FDirectoryView.Free;
  FAssetView.Free;
  inherited Destroy;
end;

{$ENDIF}
