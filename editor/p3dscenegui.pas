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
  TP3DDataViewerMode = ( dvmData, dvmLibrary, dvmScene );

  {$DEFINE INTERFACE}
    {$INCLUDE p3dscene_sceneviewer.inc}
    {$INCLUDE p3dscene_dataviewer.inc}
    {$INCLUDE p3dscene_assetviewer.inc}
    {$INCLUDE p3dscene_objectinspector.inc}
    {$INCLUDE p3dscene_main.inc}
  {$UNDEF INTERFACE}


  { TP3DSceneApplication }
  TP3DSceneApplication = class ( TP3DApplication )
    private
      procedure SetClearColor(AValue: TVec4);

    protected
      procedure Events; override;
      procedure Render; override;
      procedure InitGL;
      procedure MouseMotion(Event: TSDL_MouseMotionEvent); override;
      procedure Keyboard(Event: TSDL_KeyboardEvent); override;
      procedure LoadShaders;

    public
      procedure Initialize; override;
      procedure Finalize; override;
  end;

  procedure P3DSceneGUIInit;
  procedure P3DSceneGUIFinish;

var
  MeshArrows: TP3DActor;
  MeshGrid: TP3DActor;

  SceneMain: TP3DSceneMain;
  AssetView: TP3DSAssetPanel;
  DataView: TP3DDataPanel;
  SceneView: TP3DScenePanel;
  OIPanel: TP3DOIPanel;



  {SymbolActor, SymbolMesh,
    SymbolScene, SymbolFont,
    SymbolCamera, SymbolTexture,
    SymbolMaterial, SymbolShader,
    SymbolLight, SymbolLibrary: TP3DText; }

implementation

{ TP3DSceneApplication }


{$DEFINE IMPLEMENTATION}
  {$INCLUDE p3dscene_main.inc}
  {$INCLUDE p3dscene_sceneviewer.inc}
  {$INCLUDE p3dscene_dataviewer.inc}
  {$INCLUDE p3dscene_assetviewer.inc}
  {$INCLUDE p3dscene_objectinspector.inc}
{$UNDEF IMPLEMENTATION}

{$INCLUDE initscene.inc}

{$INCLUDE renderscene.inc}

{$INCLUDE inputscene.inc}


procedure CreateSceneTree;
var
  ListView: TP3DListView;
  i, j, GridId, o: Integer;
  Lib: TP3DLibrary;
  fn: String;

  procedure AddLib( FName: String );
  var
    n, k: Integer;
  begin
    n:= P3DData.OpenLibrary( FName );
    for k:= 0 to P3DData.Libraries[ n ].Scenes.Count - 1 do
      TP3DTileGrid( P3DData.Objects[ GridId ].Data ).AddScene( P3DData.Libraries[ n ].Scenes[ k ]);
  end;

begin
  //P3DSymbols:= P3DCreateSymbols( 'Pascal3D-Symbols', 48 );

  SceneMain:= TP3DSceneMain.Create();
  SceneMain.Align:= alClient;
  DataView:= TP3DDataPanel.Create();
  DataView.Parent:= SceneMain;
  DataView.Align:= alLeft;
  AssetView:= TP3DSAssetPanel.Create();
  AssetView.Parent:= SceneMain;
  AssetView.Align:= alBottom;
  SceneView:= TP3DScenePanel.Create();
  SceneView.Parent:= SceneMain;
  SceneView.Align:= alClient;
  DataView.BringToFront;
  //DataView.ActorList:= TestScene.Objects;

  SceneMain.ActiveLibrary:= SceneMain.NewLibrary();
  SceneMain.ActiveScene:= SceneMain.NewScene();
  SceneMain.ViewMode:= dvmScene;
  OIPanel:= TP3DOIPanel.Create();
  OIPanel.Parent:= SceneMain;
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



  {TestScene.AppendFile( '../../engine_runtime/assets/sun.p3d' );

  GridId:= P3DData.CreateNew( TP3DTileGrid, True );
  TestScene.Objects.Add( P3DData.Objects[ GridId ]);

  TP3DTileGrid( P3DData.Objects[ GridId ].Data ).GridWorldUnit:= 2;
  //AddLib( '../../engine_runtime/assets/tiles_rock.p3d' );}

  {TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Meshes.Add( nil );


  AddLib( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/box.p3d' );

  AddLib( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/wall.p3d' );

  AddLib( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/wall_corner.p3d' );}

  //n:= OpenLibrary( '/home/johannes/Documents/dev/Lazarus/p3d/pascal3d/engine_runtime/assets/conveyour.p3d' );
  //TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Meshes.Add( P3DData.Libraries[ n ].Scenes[ 0 ]);
  //TP3DTileGrid( P3DData.Objects[ GridId ].Data ).UpdateArrays;
  //for i:= 0 to TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Width * TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Height - 1 do
  //  TP3DTileGrid( P3DData.Objects[ GridId ].Data ).GridData[ i ]:= TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Scenes[ Random( TP3DTileGrid( P3DData.Objects[ GridId ].Data ).Scenes.Count )];

  //OIPanel.ObjectInspector.Obj:= P3DData.Objects[ GridId ];
  //SceneView.Selection:= P3DData.Objects[ GridId ];
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
  if ( Assigned( P3DData )) then
    begin
      if ( P3DData.IsValid( DataView )) then
        DataView.Free;
      DataView:= nil;
      if ( P3DData.IsValid( AssetView )) then
        AssetView.Free;
      AssetView:= nil;
      if ( P3DData.IsValid( SceneView )) then
        SceneView.Free;
      SceneView:= nil;
      if ( P3DData.IsValid( OIPanel )) then
        OIPanel.Free;
      OIPanel:= nil;
      if ( P3DData.IsValid( SceneMain )) then
        SceneMain.Free;
      SceneMain:= nil;
    end;
end;


finalization
  //P3DSceneGUIFinish;

end.

