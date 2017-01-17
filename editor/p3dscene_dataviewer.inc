{$IFDEF INTERFACE}
type

  { TP3DDataPanel }

  TP3DDataPanel = class( TP3DGraphicControl )
    private
      FDataButton: TP3DGroupButton;
      FLibraryButton: TP3DGroupButton;
      FLibraryPopup: TP3DPopupMenu;
      FSceneButton: TP3DGroupButton;
      FToolBar: TP3DGraphicControl;
      FViewer: TP3DTreeViewActors;

      procedure UpdateActiveScene;
      procedure UpdateButtons;
      function UpdateViewMode(AViewMode: TP3DDataViewerMode): Boolean;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure ButtonMouseClick(Sender:TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
      procedure DataMouseClick(Sender:TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
      procedure PopupMouseClick( Sender: TP3DMenuItem );
      procedure SelectionChange( var NewSelection: TP3DTreeNode );

    published
      property LibraryPopup: TP3DPopupMenu read FLibraryPopup write FLibraryPopup;
      property ToolBar: TP3DGraphicControl read FToolBar write FToolBar;
      property DataButton: TP3DGroupButton read FDataButton write FDataButton;
      property LibraryButton: TP3DGroupButton read FLibraryButton write FLibraryButton;
      property SceneButton: TP3DGroupButton read FSceneButton write FSceneButton;
      property Viewer: TP3DTreeViewActors read FViewer write FViewer;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DDataPanel }

{
procedure TP3DDataPanel.SetViewMode(AValue: TP3DDataViewerMode);
begin
  if ( FViewMode = AValue ) then Exit;

  if ( UpdateViewMode( AValue )) then
    FViewMode:= AValue;

  UpdateButtons;
end;
}
procedure TP3DDataPanel.UpdateActiveScene;
begin
  SceneView.Scene:= SceneMain.ActiveScene;
  if ( Assigned( SceneMain.ActiveScene )) then
    SceneMain.ActiveLibrary:= SceneMain.ActiveScene.Owner;
  if (( SceneMain.ViewMode in [ dvmLibrary, dvmScene ]) and Assigned( SceneView )) then
    SceneView.ToolBar.UpdateButtonsFast;
end;
{
procedure TP3DDataPanel.SetActiveScene(AValue: TP3DScene);
begin
  if FActiveScene=AValue then Exit;
  FActiveScene:=AValue;
  if ( Assigned( SceneMain.ActiveScene )) then
    SceneMain.ActiveLibrary:= SceneMain.ActiveScene.Owner;
  UpdateActiveScene;
end;

procedure TP3DDataPanel.SetActiveLibrary(AValue: TP3DLibrary);
begin
  if FActiveLibrary=AValue then Exit;
  FActiveLibrary:=AValue;

  if (( SceneMain.ViewMode = dvmData ) and Assigned( SceneView )) then
    SceneView.ToolBar.UpdateButtonsFast;
end;
}
procedure TP3DDataPanel.UpdateButtons;
begin
  case SceneMain.ViewMode of
    dvmData: DataButton.Down:= True;
    dvmLibrary: LibraryButton.Down:= True;
    dvmScene: SceneButton.Down:= True;
  end;
end;

function TP3DDataPanel.UpdateViewMode( AViewMode: TP3DDataViewerMode ): Boolean;
begin
  Result:= False;
  case AViewMode of
    dvmData:
      begin
        Viewer.Obj:= P3DData;
        Result:= True;
      end;
    dvmLibrary:
      if ( Assigned( SceneMain.ActiveScene )) then
        begin
          Viewer.Obj:= SceneMain.ActiveScene.Owner;
          Result:= True;
        end;
    dvmScene:
      if ( Assigned( SceneMain.ActiveScene )) then
        begin
          Viewer.Obj:= SceneMain.ActiveScene;
          Result:= True;
        end;
  end;
end;

constructor TP3DDataPanel.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
  procedure CreatePopup;
  begin
    LibraryPopup:= TP3DPopupMenu.Create( AData );
    LibraryPopup.OnItemClick:= @PopupMouseClick;
    LibraryPopup.Items[ LibraryPopup.Items.Add( 'Action' )].Enabled:= False;
    LibraryPopup.Items.Add( '  Reload' );
    LibraryPopup.Items.Add( '  Remove' );
    LibraryPopup.Items[ LibraryPopup.Items.Add( 'Add' )].Enabled:= False;
    LibraryPopup.Items.Add( '  Light' );
    LibraryPopup.Items.Add( '  Camera' );
    LibraryPopup.Items.Add( '  TileGrid' );
  end;

  function CreateButton( Symbol: String ): TP3DGroupButton;
  begin
    Result:= TP3DGroupButton.Create( AData, ToolBar );
    Result.Align:= alLeft;
    Result.HAlignment:= haCenter;
    Result.VAlignment:= vaCenter;
    Result.Font.Name:= P3DSymbolFontName;
    Result.Group:= 'DataViewer';
    Result.Font.Size:= 24;
    Result.Caption:= Symbol;
    Result.Width:= 32;
    Result.Height:= 32;
    Result.OnMouseClick:= @ButtonMouseClick;
  end;

begin
  inherited Create( AData, AParent );
  Viewer:= TP3DTreeViewActors.Create( Owner, Self );
  Viewer.Align:= alClient;
  Viewer.ShowSymbols:= True;
  Width:= 200;
  Viewer.OnSelectionChange:= @SelectionChange;
  CreatePopup;

  ToolBar:= TP3DGraphicControl.Create( Owner, Self );
  ToolBar.Height:= 32;
  ToolBar.Align:= alTop;
  DataButton:= CreateButton( P3DSymbolOptions );
  LibraryButton:= CreateButton( P3DSymbolLibrary );
  LibraryButton.Left:= DataButton.Width;
  SceneButton:= CreateButton( P3DSymbolScene );
  SceneButton.Left:= DataButton.Width + LibraryButton.Width;
  Viewer.OnMouseClick:= @DataMouseClick;
  UpdateViewMode( SceneMain.ViewMode );
  UpdateButtons;
end;

destructor TP3DDataPanel.Destroy;
begin
  if (( Assigned( ToolBar )) and ( P3DData.IsValid( ToolBar ))) then
    ToolBar.Free;
  if (( Assigned( Viewer )) and ( P3DData.IsValid( Viewer ))) then
    Viewer.Free;
  if (( Assigned( LibraryPopup )) and ( P3DData.IsValid( LibraryPopup ))) then
    LibraryPopup.Free;
  if (( Assigned( LibraryButton )) and ( P3DData.IsValid( LibraryButton ))) then
    LibraryButton.Free;
  if (( Assigned( SceneButton )) and ( P3DData.IsValid( SceneButton ))) then
    SceneButton.Free;
  inherited Destroy;
end;

procedure TP3DDataPanel.ButtonMouseClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  if ( Sender = DataButton ) then
    SceneMain.ViewMode:= dvmData
  else if ( Sender = LibraryButton ) then
    SceneMain.ViewMode:= dvmLibrary
  else if ( Sender = SceneButton ) then
    SceneMain.ViewMode:= dvmScene;
end;

procedure TP3DDataPanel.DataMouseClick(Sender: TP3DGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);
begin
  if ( mb3 ) then
    if ( Assigned( DataView.Viewer.Selection )) then
      if ( TPersistent( DataView.Viewer.Selection.Data ) is TP3DLibrary or TPersistent( DataView.Viewer.Selection.Data ) is TP3DDataBlock ) then
        LibraryPopup.PopUp( X, Y );
end;

procedure TP3DDataPanel.PopupMouseClick(Sender: TP3DMenuItem);
  procedure AddToParentScene( Data: Pointer; TP: TP3DDataBlockType );
    function FindParentScene( Block: TP3DDataBlock ): TP3DScene;
    var
      User: TP3DDataBlock;
    begin
      if ( Block is TP3DScene ) then
        Result:= TP3DScene( Block )
      else if ( Assigned( Block ) and ( Block is TP3DActor ) and ( TP3DActor( Block ).Users.Count > 0 )) then
        for User in TP3DActor( Block ).Users do
          begin
            Result:= FindParentScene( User );
            if ( Assigned( Result )) then
              break;
          end
      else
        Result:= nil;
    end;

  var
    scene: TP3DScene;
    Obj: TP3DActor;
  begin
    if ( TObject( DataView.Viewer.Selection.Data ) is TP3DDataBlock ) then
      Obj:= TP3DDataBlock( DataView.Viewer.Selection.Data ).Owner.Objects[ TP3DDataBlock( DataView.Viewer.Selection.Data ).Owner.CreateNew( TP )]
    else if ( TObject( DataView.Viewer.Selection.Data ) is TP3DLibrary ) then
      Obj:= TP3DData( DataView.Viewer.Selection.Data ).Objects[ TP3DData( DataView.Viewer.Selection.Data ).CreateNew( TP )];

    if ( TObject( Data ) is TP3DDataBlock ) then
      begin
        scene:= FindParentScene( TP3DDataBlock( Data ));
        if ( Assigned( scene )) then
          scene.Objects.Add( TP3DActor( Obj ));
      end;

  end;

begin
  if ( Sender is TP3DButton ) then
    with ( TP3DButton( Sender )) do
      begin
        case Caption of
          '  Reload': if ( TObject( DataView.Viewer.Selection.Data ) is TP3DDataBlock ) then
            TP3DDataBlock( DataView.Viewer.Selection.Data ).Reload;
          '  Remove':
              if ( TObject( DataView.Viewer.Selection.Data ) is TP3DLibrary ) then
                begin
                  P3DData.Libraries.Remove( TP3DLibrary( DataView.Viewer.Selection.Data ));
                  TP3DLibrary( DataView.Viewer.Selection.Data ).Free;
                  if ( Viewer.Selection.Data = DataView.Viewer.Selection.Data ) then
                    Viewer.Selection:= nil;
                  DataView.Viewer.Update;
                  DataView.Viewer.Children.ExpandAll();
                end
              else if ( TObject( DataView.Viewer.Selection.Data ) is TP3DDataBlock ) then
                begin
                  TP3DDataBlock( DataView.Viewer.Selection.Data ).Free;
                  if ( Viewer.Selection.Data = DataView.Viewer.Selection.Data ) then
                    Viewer.Selection:= nil;
                  DataView.Viewer.Update;
                  DataView.Viewer.Children.ExpandAll();
                end;
          '  Light': AddToParentScene( DataView.Viewer.Selection.Data, TP3DLight );
          '  Camera': AddToParentScene( DataView.Viewer.Selection.Data, TP3DCamera );
          '  TileGrid': AddToParentScene( DataView.Viewer.Selection.Data, TP3DTileGrid );
        end;
        LibraryPopup.Hide;
      end;
end;

procedure TP3DDataPanel.SelectionChange(var NewSelection: TP3DTreeNode);
begin
  if (( Assigned( NewSelection ) and ( TObject( NewSelection.Data ) is TPersistent ))) then
    begin
      OIPanel.ObjectInspector.Obj:= TInterfacedPersistent( NewSelection.Data );
      if ( TInterfacedPersistent( NewSelection.Data ) is TP3DDataBlock ) then
        SceneMain.ActiveLibrary:= TP3DDataBlock( NewSelection.Data ).Owner;

      if ( TPersistent( NewSelection.Data ) is TP3DActor ) then
        begin
          SceneView.FSelection:= TP3DActor( NewSelection.Data );
          SceneMain.ActiveScene:= TP3DActor( NewSelection.Data ).Scene;
        end
      else if ( TPersistent( NewSelection.Data ) is TP3DLibrary ) then
        begin
          SceneView.FSelection:= nil;
          SceneMain.ActiveLibrary:= TP3DLibrary( NewSelection.Data );
        end
      else
        begin
          SceneView.FSelection:= nil;
        end;
    end
  else
    begin
      OIPanel.ObjectInspector.Obj:= nil;
      SceneView.FSelection:= nil;
    end;
end;


{$ENDIF}
