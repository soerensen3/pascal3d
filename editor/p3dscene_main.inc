{$IFDEF INTERFACE}
type

  { TP3DSceneMain }

  TP3DSceneMain = class( TP3DGraphicControl )
    private
      FActiveDataBlock: TP3DDataBlock;
      FActiveLibrary: TP3DLibrary;
      FActiveScene: TP3DScene;
      FLastUpdate: Integer;
      FNeedsUpdate: Boolean;
      FViewMode: TP3DDataViewerMode;
      FUpdating: Boolean;

      PropEd: TP3DPropertyEditorString;
      Edit: TP3DEdit;

      procedure SetActiveDataBlock(AValue: TP3DDataBlock);
      procedure SetActiveLibrary(AValue: TP3DLibrary);
      procedure SetActiveScene(AValue: TP3DScene);
      procedure SetViewMode(AValue: TP3DDataViewerMode);
      procedure BeginUpdate;
      procedure EndUpdate;

      procedure OnEditKeyDown( Sender: TObject );

    public
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;

      function NewLibrary( const AFileName: String = 'Untitled%d.p3d' ): TP3DLibrary;
      function NewScene( const AName: String = 'NewScene%d' ): TP3DScene;
      procedure Paint(); override;
      procedure CheckLastChange;
      procedure Update;
      function FindFirstLib: TP3DLibrary;
      function FindFirstScene: TP3DScene;

    published
      property ActiveScene: TP3DScene read FActiveScene write SetActiveScene;
      property ActiveLibrary: TP3DLibrary read FActiveLibrary write SetActiveLibrary;
      property ActiveDataBlock: TP3DDataBlock read FActiveDataBlock write SetActiveDataBlock;
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
    ActiveScene:= ActiveLibrary.Scenes[ 0 ]
  else if ( Assigned( SceneView )) then
    SceneView.ToolBar.UpdateButtonsFast;

  if ( Assigned( DataView )) then
    DataView.UpdateButtons;
end;

procedure TP3DSceneMain.SetActiveDataBlock(AValue: TP3DDataBlock);
begin
  if FActiveDataBlock=AValue then Exit;
  FActiveDataBlock:=AValue;

  //current workaround
  ActionEditor.UpdateList;
end;

procedure TP3DSceneMain.SetActiveScene(AValue: TP3DScene);
begin
  if FActiveScene=AValue then Exit;
  FActiveScene:=AValue;

  if ( Assigned( SceneMain.ActiveScene )) then
    begin
      SceneView.Scene:= SceneMain.ActiveScene;
      SceneMain.ActiveLibrary:= SceneMain.ActiveScene.OwnerLib;
    end;
  if ( Assigned( SceneView )) then
    SceneView.ToolBar.UpdateButtonsFast;
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

procedure TP3DSceneMain.OnEditKeyDown(Sender: TObject);
begin
  if ( P3DInput.Keyboard.DKeys[ P3DK_RETURN ] = ksPressed ) then
    if ( OIPanel.ObjectInspector.Obj is TP3DStreamable ) then
      PropEd.Prop:= TP3DStreamable( OIPanel.ObjectInspector.Obj ).Properties.GetPropByPath( Edit.Text );
end;

constructor TP3DSceneMain.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited CreateBlock(AData);
  PageModeX:= pmStretch;
  PageModeY:= pmStretch;
end;

function TP3DSceneMain.NewLibrary(const AFileName: String): TP3DLibrary;
var
  i: Integer;
  FN: String;
begin
  SetCurrentDirUTF8( P3DSearchPaths.BaseDir.FileName );
  Result:= TP3DLibrary.Create;
  i:= 1;
  if ( Pos( '%d', AFileName ) > 0 ) then  // Give name like Untitled1.p3d, Untitled2.p3d but make sure that each name only exists once
    begin
      while (( P3DData.Libraries.FindByFileName( Format( AFileName, [ i ])) > -1 ) or ( FileExists( Format( AFileName, [ i ])))) do
        Inc( i );
      FN:= Format( AFileName, [ i ]);
    end
  else
    FN:= AFileName;

  Result.JSONLoader.FileName:= FN;
  P3DData.Libraries.Add( Result );
end;

function TP3DSceneMain.NewScene(const AName: String): TP3DScene;
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
  if ( Pos( '%d', AName ) > 0 ) then // Give name like NewScene1, NewScene2 but make sure that each name only exists once per file
    begin
      while ( Lib.Scenes.FindByName( Format( AName, [ i ])) > -1 ) do
        Inc( i );
      N:= Format( AName, [ i ]);
    end
  else
    N:= AName;

  Result:= TP3DScene.CreateBlock( ActiveLibrary );
  Result.Name:= N;
end;

procedure TP3DSceneMain.Paint();
begin
  inherited Paint();
  if ( not Enabled ) then
    begin
      Canvas.Lock;
      Canvas.RenderRect( vec2( 0 ), vec2( Width, Height ), vec4( vec3( 0 ), 0.4 ));
      Canvas.Unlock();
    end;
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
  if ( Assigned( ActiveLibrary ) and ( P3DData.Libraries.IndexOf( ActiveLibrary ) < 0 )) then
    FActiveLibrary:= FindFirstLib;
  if ( Assigned( ActiveScene ) and ( not P3DDataBlockCache.IsValid( ActiveScene ))) then
    begin
      FActiveScene:= FindFirstScene;
      SceneView.Scene:= ActiveScene;
    end;
  if ( Assigned( DataView )) then
    begin
      DataView.Viewer.Update;
      DataView.Viewer.Children.ExpandAll();
    end;
  if ( Assigned( SceneView )) then
    SceneView.ToolBar.UpdateButtons( ViewMode );
  FLastUpdate:= SDL_GetTicks;
  FNeedsUpdate:= False;
  EndUpdate;
end;

function TP3DSceneMain.FindFirstLib: TP3DLibrary;
var
  i: Integer;
begin
  Result:= P3DData;
  i:= 0;
  while ( i < P3DData.Libraries.Count ) do
    if ( not P3DData.Libraries[ i ].IsDesignerPackage ) then
      begin
        Result:= P3DData.Libraries[ i ];
        break;
      end
    else
      Inc( i );
end;

function TP3DSceneMain.FindFirstScene: TP3DScene;
var
  Lib: TP3DLibrary;
begin
  if ( Assigned( ActiveLibrary )) then
    Lib:= ActiveLibrary
  else
    Lib:= FindFirstLib;

  if ( Lib.Scenes.Count > 0 ) then
    Result:= Lib.Scenes[ 0 ]
  else
    Result:= nil;
end;

{$ENDIF}
