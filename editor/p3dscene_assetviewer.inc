{$IFDEF INTERFACE}
  { TP3DSAssetPanel }

  TP3DSAssetPanel = class( TP3DGraphicControl )
    private
      FAssetView: TP3DListViewFile;
      FDirectoryView: TP3DTreeViewDirectory;

    public
      constructor Create(const AData: TP3DData = nil; const AParent: TP3DGraphicControl=nil); override;

      destructor Destroy; override;

    published
      property DirectoryView: TP3DTreeViewDirectory read FDirectoryView write FDirectoryView;
      property AssetView: TP3DListViewFile read FAssetView write FAssetView;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}



{ TP3DSAssetPanel }

constructor TP3DSAssetPanel.Create(const AData: TP3DData;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  Height:= 200;
  FDirectoryView:= TP3DTreeViewDirectory.Create( nil, Self );
  FAssetView:= TP3DListViewFile.Create( nil, Self );
  FDirectoryView.Directory:= P3DSearchPaths.BaseDir;
  FDirectoryView.ShowSymbols:= True;
  FDirectoryView.Align:= alLeft;
  FDirectoryView.Width:= 200;
  FAssetView.ShowSymbols:= True;
  FAssetView.TreeViewDirectory:= FDirectoryView;
  FAssetView.ShowFolderUp:= True;
  FAssetView.Align:= alClient;
  FAssetView.AllowDrag:= True;
  FAssetView.Mask:= '*.p3d';
end;

destructor TP3DSAssetPanel.Destroy;
begin
  FDirectoryView.Free;
  FAssetView.Free;
  inherited Destroy;
end;

{$ENDIF}
