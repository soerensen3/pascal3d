{$IFDEF INTERFACE}
type

  { TP3DSceneMain }

  TP3DSceneMain = class( TPersistent )
    private
      FActiveLibrary: TP3DLibrary;
      FActiveScene: TP3DScene;
      FLastUpdate: Integer;
      FNeedsUpdate: Boolean;
      FViewMode: TP3DDataViewerMode;
      FUpdating: Boolean;

      procedure SetActiveLibrary(AValue: TP3DLibrary);
      procedure SetActiveScene(AValue: TP3DScene);
      procedure SetViewMode(AValue: TP3DDataViewerMode);
      procedure BeginUpdate;
      procedure EndUpdate;

    public
      function NewLibrary( const FName: String = 'Untitled%d.p3d' ): TP3DLibrary;
      function NewScene( const Name: String = 'NewScene%d' ): TP3DScene;
      procedure CheckLastChange;
      procedure Update;

    published
      property ActiveScene: TP3DScene read FActiveScene write SetActiveScene;
      property ActiveLibrary: TP3DLibrary read FActiveLibrary write SetActiveLibrary;
      property ViewMode: TP3DDataViewerMode read FViewMode write SetViewMode;
      property LastUpdate: Integer read FLastUpdate;
      property NeedsUpdate: Boolean read FNeedsUpdate;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DSceneMain }

procedure TP3DSceneMain.SetActiveLibrary(AValue: TP3DLibrary);
begin
  if FActiveLibrary=AValue then Exit;
  FActiveLibrary:=AValue;

  if ( Assigned( ActiveLibrary ) and ( ActiveLibrary.Scenes.Count > 0 )) then
    ActiveScene:= ActiveLibrary.Scenes[ 0 ];

  if ( Assigned( DataView )) then
    DataView.UpdateButtons;
  if ( Assigned( SceneView )) then
    SceneView.ToolBar.UpdateButtons( ViewMode );
end;

procedure TP3DSceneMain.SetActiveScene(AValue: TP3DScene);
begin
  if FActiveScene=AValue then Exit;
  FActiveScene:=AValue;

  if ( Assigned( SceneMain.ActiveScene )) then
    begin
      SceneView.Scene:= SceneMain.ActiveScene;
      SceneMain.ActiveLibrary:= SceneMain.ActiveScene.Owner;
    end;
  if (( SceneMain.ViewMode in [ dvmLibrary, dvmScene ]) and Assigned( SceneView )) then
    SceneView.ToolBar.UpdateButtons( ViewMode );
end;

procedure TP3DSceneMain.SetViewMode(AValue: TP3DDataViewerMode);
begin
  if FViewMode=AValue then Exit;
  FViewMode:=AValue;
  if ( Assigned( DataView )) then
    DataView.UpdateViewMode( AValue );
  if ( Assigned( SceneView )) then
    SceneView.ToolBar.UpdateButtons( AValue );
end;

procedure TP3DSceneMain.BeginUpdate;
begin
  FUpdating:= True;
end;

procedure TP3DSceneMain.EndUpdate;
begin
  FUpdating:= False;
end;

function TP3DSceneMain.NewLibrary(const FName: String): TP3DLibrary;
var
  i: Integer;
  FN: String;
begin
  SetCurrentDirUTF8( P3DSearchPaths.BaseDir );
  Result:= TP3DLibrary.Create;
  i:= 1;
  if ( Pos( '%d', FName ) > 0 ) then
    begin
      while (( P3DData.Libraries.FindByFileName( Format( FName, [ i ])) > -1 ) or ( FileExists( Format( FName, [ i ])))) do
        Inc( i );
      FN:= Format( FName, [ i ]);
    end
  else
    FN:= FName;

  Result.FileWatch.FileName:= FN;
  P3DData.Libraries.Add( Result );
end;

function TP3DSceneMain.NewScene(const Name: String): TP3DScene;
var
  i: Integer;
  Lib: TP3DLibrary;
  N: String;
begin
  if ( Assigned( ActiveLibrary )) then
    Lib:= ActiveLibrary
  else
    Lib:= P3DData;

  i:= 1;
  if ( Pos( '%d', Name ) > 0 ) then
    begin
      while ( Lib.Scenes.FindByName( Format( Name, [ i ])) > -1 ) do
        Inc( i );
      N:= Format( Name, [ i ]);
    end
  else
    N:= Name;

  Result:= TP3DScene.Create( ActiveLibrary );
  Result.Name:= N;
end;

procedure TP3DSceneMain.CheckLastChange;
var
  DataLastUpdate: Integer;
begin
  DataLastUpdate:= P3DData.CheckLastChange;
  if ( LastUpdate < DataLastUpdate ) then
    FNeedsUpdate:= True;
end;

procedure TP3DSceneMain.Update;
begin
  if ( FUpdating ) then
    exit;
  BeginUpdate;
  if ( Assigned( DataView )) then
    DataView.UpdateViewMode( ViewMode );
  if ( Assigned( SceneView )) then
    SceneView.ToolBar.UpdateButtons( ViewMode );
  FLastUpdate:= SDL_GetTicks;
  FNeedsUpdate:= False;
  EndUpdate;
end;

{$ENDIF}
