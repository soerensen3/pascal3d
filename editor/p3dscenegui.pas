unit p3dscenegui;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  LazFileUtils,
  SDL2,
  Math,
  dglOpenGL,
  p3devents,
  p3dutils,
  p3dgraphics,
  p3dMath,
  p3dgui;

type
  {$DEFINE INTERFACE}
    {$INCLUDE p3dscene_sceneviewer.inc}
    {$INCLUDE p3dscene_dataviewer.inc}
    {$INCLUDE p3dscene_assetviewer.inc}
    {$INCLUDE p3dscene_objectinspector.inc}
  {$UNDEF INTERFACE}


  procedure P3DSceneGUIInit;
  procedure P3DSceneGUIFinish;

var
  AssetView: TP3DSAssetPanel;
  DataView: TP3DDataPanel;
  SceneView: TP3DScenePanel;
  TestScene: TP3DScene;
  OIPanel: TP3DOIPanel;

  MeshArrows: TP3DActor;
  MeshGrid: TP3DActor;

  SymbolActor, SymbolMesh,
    SymbolScene, SymbolFont,
    SymbolCamera, SymbolTexture,
    SymbolMaterial, SymbolShader,
    SymbolLight, SymbolLibrary: TP3DText;

implementation

{$DEFINE IMPLEMENTATION}
  {$INCLUDE p3dscene_sceneviewer.inc}
  {$INCLUDE p3dscene_dataviewer.inc}
  {$INCLUDE p3dscene_assetviewer.inc}
  {$INCLUDE p3dscene_objectinspector.inc}
{$UNDEF IMPLEMENTATION}


procedure CreateSceneTree;
var
  ListView: TP3DListView;
  i, j, GridId, o: Integer;

  procedure AddLib( FName: String );
  var
    n, k: Integer;
  begin
    n:= OpenLibrary( FName );
    for k:= 0 to P3DData.Libraries[ n ].Scenes.Count - 1 do
      TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Meshes.Add( P3DData.Libraries[ n ].Scenes[ k ]);
  end;

begin
  //P3DSymbols:= P3DCreateSymbols( 'Pascal3D-Symbols', 48 );
  TestScene:= TP3DScene.Create();
  AssetView:= TP3DSAssetPanel.Create();
  AssetView.Align:= alBottom;
  SceneView:= TP3DScenePanel.Create();
  SceneView.Align:= alClient;
  DataView:= TP3DDataPanel.Create();
  DataView.Align:= alLeft;

  SceneView.Scene:= TestScene;
  OIPanel:= TP3DOIPanel.Create();
  OIPanel.Align:= alRight;
  OIPanel.ObjectInspector.Obj:= OIPanel;

  {TestScene.AppendFile( 'armature_test/media/colorwheel.p3d' );
  with ( TP3DComboBoxDataBlock.Create()) do
    begin
      Align:= alBottom;
      TypeFilter:= TP3DScene;
    end;}

  {n:= 1;
  for i:= 0 to ( Random( 10 ) + 5 ) do
    with ( TestScene.Objects[ TestScene.Objects.Add( TP3DActor.Create())]) do
      begin
        Name:= 'TestActor' + IntToStr( n );
        Inc( n );
        case random( 3 ) of
          0: Data:= TP3DMesh.Create();
          1: Data:= TP3DCamera.Create();
          2: Data:= TP3DScene.Create();
        end;;
        for j:= 0 to Random( 5 ) do
          with ( Children[ Children.Add( TP3DActor.Create())]) do
            begin
              Name:= 'TestActor' + IntToStr( n );
              Inc( n );
            end;
      end;}

  //ActorList.ActorList:= TestScene.Objects;
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



  TestScene.AppendFile( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/sun.p3d' );

  GridId:= P3DData.CreateNew( TP3DTileGrid, True );
  TestScene.Objects.Add( P3DData.Objects[ GridId ]);

  TP3DTileGrid( P3DData.Objects[ GridId ].Data ).GridWorldUnit:= 2;
  AddLib( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/tiles_rock.p3d' );

  {TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Meshes.Add( nil );


  AddLib( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/box.p3d' );

  AddLib( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/wall.p3d' );

  AddLib( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/wall_corner.p3d' );}

  //n:= OpenLibrary( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/conveyour.p3d' );
  //TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Meshes.Add( P3DData.Libraries[ n ].Scenes[ 0 ]);

  TP3DTileGrid( P3DData.Objects[ GridId ].Data ).UpdateArrays;
  //for i:= 0 to TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Width * TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Height - 1 do
  //  TP3DTileGrid( P3DData.Objects[ GridId ].Data ).GridData[ i ]:= Random( TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Meshes.Count );
  OIPanel.ObjectInspector.Obj:= P3DData.Objects[ GridId ];
  SceneView.Selection:= P3DData.Objects[ GridId ];
end;

procedure P3DSceneGUIInit;
begin
  CreateSceneTree;
  CreateEditorScenes;
  CreateEditModes;
  P3DGUIManager.Controls.Realign;
  P3DGUIManager.UpdateExtents;
  P3DGUIManager.ShowCursor:= True;
  SDL_ShowCursor( 0 );
end;

procedure P3DSceneGUIFinish;
//var
//  Lib: TP3DData;
begin
  //WriteLn( 'P3DData.Objects: ' + P3DData.DataBlocks.DumpObjectList );
  //for Lib in P3DData.Libraries do
  //  WriteLn( '<' + ExtractFileNameOnly( Lib.FileWatch.FileName ) + '>.Objects: ' + Lib.DataBlocks.DumpObjectList );

  DestroyEditModes;
  FreeAndNil( DataView );
  FreeAndNil( AssetView );
  FreeAndNil( SceneView );
  FreeAndNil( SymbolActor );
  FreeAndNil( SymbolMesh );
  FreeAndNil( SymbolScene );
  FreeAndNil( SymbolFont );
  FreeAndNil( SymbolCamera );
  FreeAndNil( SymbolTexture );
  FreeAndNil( SymbolMaterial );
  FreeAndNil( SymbolShader );
  FreeAndNil( SymbolLight );
  FreeAndNil( SymbolLibrary );
end;


finalization
  //P3DSceneGUIFinish;

end.

