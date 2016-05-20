unit p3dscenegui;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  p3devents,
  p3dutils,
  p3dgraphics,
  p3dgui;

type

  { TP3DSToolbar }

  TP3DSToolbar = class ( TP3DGraphicControl )
    private
      FButtons: TP3DControlList;

    public
      constructor Create(const AOwner: TP3DObjectList = nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

      function AddButton( Text: String ): TP3DButton;

    published
      property Buttons: TP3DControlList read FButtons write FButtons;
  end;

  { TP3DSAssetPanel }

  TP3DSAssetPanel = class( TP3DGraphicControl )
    private
      FAssetView: TP3DListViewFile;
      FDirectoryView: TP3DTreeViewDirectory;

    public
      constructor Create(const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

    published
      property DirectoryView: TP3DTreeViewDirectory read FDirectoryView write FDirectoryView;
      property AssetView: TP3DListViewFile read FAssetView write FAssetView;
  end;

  procedure P3DSceneGUIInit;

var
  ToolbarFile: TP3DSToolbar;
  AssetView: TP3DSAssetPanel;
  SceneTree: TP3DTreeView;

implementation

procedure CreateToolbarFile;
begin
  ToolbarFile:= TP3DSToolbar.Create();
  ToolbarFile.AddButton( 'O' );
  ToolbarFile.AddButton( 'X' );
  ToolbarFile.Width:= 128;
end;

procedure CreateSceneTree;
var
  ListView: TP3DListView;
begin
  AssetView:= TP3DSAssetPanel.Create();
  AssetView.Align:= alBottom;
{  SceneTree:= TP3DTreeViewDirectory.Create();
  SceneTree.Width:= 200;
  SceneTree.Align:= alLeft;
  ListView:= TP3DListViewFile.Create();
  ListView.Align:= alClient;

  ListView.Children.Add( 'testnode1', nil );
  ListView.Children.Add( 'testnode2', nil );
  ListView.Children.Add( 'testnode3', nil );
  ListView.Children.Add( 'testnode4', nil );
  ListView.Children.Add( 'testnode5', nil );
  ListView.Children.Add( 'testnode6', nil );
  ListView.Children.Add( 'testnode7', nil );
  ListView.Children.Add( 'testnode8', nil );
  ListView.Children.Add( 'testnode9', nil );
  ListView.Children.Add( 'testnode10', nil );}

{  SceneTree.Children.Add( 'testnode1', nil, True );
  SceneTree.Children.Add( 'testnode2', nil, True );
  with SceneTree.Children[ SceneTree.Children.Add( 'testnode3', nil, True )] do
    begin
      Children.Add( 'testnode4', nil, True );
      Children.Add( 'testnode5', nil, True );
      Children.Add( 'testnode6', nil, True );
      with Children[ Children.Add( 'testnode3', nil, False )] do
        begin
          Children.Add( 'testnode4', nil, True );
          Children.Add( 'testnode5', nil, True );
          Children.Add( 'testnode6', nil, True );
          Children.Add( 'testnode7', nil, True );
        end;
      Children.Add( 'testnode7', nil, True );
    end;}
  //TP3DTreeViewDirectory( SceneTree ).ShowFiles:= True;
  {TP3DTreeViewDirectory( SceneTree ).Directory:= P3DSearchPaths.BaseDir;
  TP3DTreeViewDirectory( SceneTree ).ShowSymbols:= True;
  TP3DListViewFile( ListView ).Directory:= P3DSearchPaths.BaseDir;
  TP3DListViewFile( ListView ).ShowSymbols:= True;
  TP3DListViewFile( ListView ).TreeViewDirectory:= TP3DTreeViewDirectory( SceneTree );
  TP3DListViewFile( ListView ).ShowFolderUp:= True;}
end;

procedure P3DSceneGUIInit;
begin
  //CreateToolbarFile;
  CreateSceneTree;
  P3DGUIManager.Controls.Realign;
  P3DGUIManager.UpdateExtents;
end;

{ TP3DSAssetPanel }

constructor TP3DSAssetPanel.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
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
end;

destructor TP3DSAssetPanel.Destroy;
begin
  FDirectoryView.Free;
  FAssetView.Free;
  inherited Destroy;
end;

{ TP3DSToolbar }

constructor TP3DSToolbar.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Buttons:= TP3DControlList.Create( Self );
  Height:= 32;
end;

destructor TP3DSToolbar.Destroy;
begin
  Buttons.Clear( True );
  Buttons.Free;
  inherited Destroy;
end;

function TP3DSToolbar.AddButton( Text: String ): TP3DButton;
begin
  Result:= TP3DButton.Create( nil, Self );
  Buttons.Add( Result );
  Result.Align:= alLeft;
  Result.Width:= 32;
  Result.Caption:= Text;
end;

end.

