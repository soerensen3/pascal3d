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
      FViewer: TP3DTreeViewObjects;
      FSelectionParent: TPersistent;

      procedure SetEmptyParent;
      procedure UpdateActiveScene;
      procedure UpdateButtons;
      function UpdateViewMode(AViewMode: TP3DDataViewerMode): Boolean;

    public
      constructor CreateBlock(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure ButtonMouseClick(Sender:TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
      procedure DataMouseClick(Sender:TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
      procedure PopupMouseClick( Sender: TP3DMenuItem );
      procedure SelectionChange( var NewSelection: TP3DTreeNode );
      procedure DataDragDrop( Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean );

    published
      property LibraryPopup: TP3DPopupMenu read FLibraryPopup write FLibraryPopup;
      property ToolBar: TP3DGraphicControl read FToolBar write FToolBar;
      property DataButton: TP3DGroupButton read FDataButton write FDataButton;
      property LibraryButton: TP3DGroupButton read FLibraryButton write FLibraryButton;
      property SceneButton: TP3DGroupButton read FSceneButton write FSceneButton;
      property Viewer: TP3DTreeViewObjects read FViewer write FViewer;
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
    SceneMain.ActiveLibrary:= SceneMain.ActiveScene.OwnerLib;
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
          Viewer.Obj:= SceneMain.ActiveScene.OwnerLib;
          Result:= True;
        end;
    dvmScene:
      if ( Assigned( SceneMain.ActiveScene )) then
        begin
          Viewer.Obj:= SceneMain.ActiveScene;
          Result:= True;
        end;
  end;

  UpdateButtons;

  if ( Assigned( Viewer.Selection )) then
    Viewer.Selection:= nil
  else
    SetEmptyParent;
end;

constructor TP3DDataPanel.CreateBlock(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
  procedure CreatePopup;
  begin
    LibraryPopup:= TP3DPopupMenu.CreateBlock( AData );
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
    Result:= TP3DGroupButton.CreateBlock( AData, ToolBar );
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
  inherited CreateBlock( AData, AParent );
  ToolBar:= TP3DGraphicControl.CreateBlock( OwnerLib, Self );
  ToolBar.Height:= 32;
  ToolBar.Align:= alTop;

  Viewer:= TP3DTreeViewObjects.CreateBlock( OwnerLib, Self );
  Viewer.Align:= alClient;
  Viewer.ShowSymbols:= True;
  Width:= 200;
  Viewer.OnSelectionChange:= @SelectionChange;
  CreatePopup;

  DataButton:= CreateButton( P3DSymbolOptions );
  LibraryButton:= CreateButton( P3DSymbolLibrary );
  LibraryButton.Left:= DataButton.Width;
  SceneButton:= CreateButton( P3DSymbolScene );
  SceneButton.Left:= DataButton.Width + LibraryButton.Width;
  Viewer.OnMouseClick:= @DataMouseClick;
  Viewer.AllowDrag:= True;
  Viewer.OnDragDrop:= @DataDragDrop;

  SetEmptyParent;
  UpdateViewMode( SceneMain.ViewMode );
end;

destructor TP3DDataPanel.Destroy;
begin
  if (( Assigned( ToolBar )) and ( P3DDataBlockCache.IsValid( ToolBar ))) then
    ToolBar.Free;
  if (( Assigned( Viewer )) and ( P3DDataBlockCache.IsValid( Viewer ))) then
    Viewer.Free;
  if (( Assigned( LibraryPopup )) and ( P3DDataBlockCache.IsValid( LibraryPopup ))) then
    LibraryPopup.Free;
  if (( Assigned( LibraryButton )) and ( P3DDataBlockCache.IsValid( LibraryButton ))) then
    LibraryButton.Free;
  if (( Assigned( SceneButton )) and ( P3DDataBlockCache.IsValid( SceneButton ))) then
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
var
  SelParent: TPersistent;
begin
  if ( mb3 ) then
    LibraryPopup.PopUp( X + Sender.Canvas.Left, Y + Sender.Canvas.Top );
end;

procedure TP3DDataPanel.PopupMouseClick(Sender: TP3DMenuItem);

  procedure AddToParent( TP: TP3DDataBlockType );
  var
    Obj: TP3DObject;
  begin
    if ( FSelectionParent is TP3DObjectList ) then
      begin
        Obj:= SceneMain.ActiveLibrary.CreateNewWithObj( TP );
        TP3DObjectList( FSelectionParent ).Add( Obj );
      end
    else if ( FSelectionParent is TP3DLibrary ) then
      TP3DLibrary( FSelectionParent ).CreateNew( TP );
  end;

begin
  case Sender.Caption of
//    '  Reload': if ( TObject( DataView.Viewer.Selection.Data ) is TP3DDataBlock ) then
//      TP3DDataBlock( DataView.Viewer.Selection.Data ).Reload;
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
    '  Light': AddToParent( TP3DLight );
    '  Camera': AddToParent( TP3DCamera );
    '  TileGrid': AddToParent( TP3DTileGrid );
  end;
  LibraryPopup.Hide;
end;

procedure TP3DDataPanel.SetEmptyParent;
begin
  case SceneMain.ViewMode of
    dvmData: FSelectionParent:= P3DData;
    dvmLibrary: FSelectionParent:= SceneMain.ActiveLibrary;
    dvmScene: if ( Assigned( SceneMain.ActiveScene )) then FSelectionParent:= SceneMain.ActiveScene.Objects;
  end;
end;

procedure TP3DDataPanel.SelectionChange(var NewSelection: TP3DTreeNode);

var
  Sel: TPersistent;
begin
  if (( Assigned( NewSelection ) and ( TObject( NewSelection.Data ) is TPersistent ))) then
    begin
      Sel:= TPersistent( NewSelection.Data );
      OIPanel.ObjectInspector.Obj:= TInterfacedPersistent( NewSelection.Data );
      if ( TInterfacedPersistent( NewSelection.Data ) is TP3DDataBlock ) then
        SceneMain.ActiveLibrary:= TP3DDataBlock( NewSelection.Data ).OwnerLib;

      if ( Sel is TP3DObject ) then
        begin
          //SceneView.FSelection:= TP3DObject( Sel );
          SceneMain.ActiveScene:= TP3DObject( Sel ).Scene;
          FSelectionParent:= TP3DObject( Sel ).Children;
        end
      else if ( Sel is TP3DLibrary ) then
        begin
          //SceneView.FSelection:= nil;
          SceneMain.ActiveLibrary:= TP3DLibrary( NewSelection.Data );
          FSelectionParent:= SceneMain.ActiveLibrary;
        end
      else if ( Sel is TP3DScene) then
        begin
          //SceneView.FSelection:= nil;
          SceneMain.ActiveScene:= TP3DScene( Sel );
          FSelectionParent:= SceneMain.ActiveScene.Objects;
        end
      else if ( Sel is TP3DDataBlock ) then
        begin
          //SceneView.FSelection:= nil;
          SceneMain.ActiveLibrary:= TP3DDataBlock( NewSelection.Data ).OwnerLib;
          FSelectionParent:= SceneMain.ActiveLibrary;
        end
      else
        begin
          SetEmptyParent;
          SceneView.FSelection:= nil;
          exit;
        end;
      SceneMain.ActiveDataBlock:= TP3DDataBlock( Sel );
    end
  else
    begin
      SetEmptyParent;
      OIPanel.ObjectInspector.Obj:= nil;
      SceneView.FSelection:= nil;
    end;

end;

procedure TP3DDataPanel.DataDragDrop(Sender, Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean);
begin
  if ( Source is TP3DListViewFile ) then
    with ( TP3DListViewFile( Source )) do
      begin
        if ( Assigned( DragItem )) then
          if ( P3DMatchFileName( DragItem.Caption, '*.p3d' )) then
            begin
              try
                SceneMain.ActiveLibrary:= P3DData.Libraries[ P3DData.OpenLibrary( AppendPathDelim( Directory ) + DragItem.Caption )];
              except
                on E: Exception do
                  SDL_ShowSimpleMessageBox( SDL_MESSAGEBOX_ERROR, PChar( 'Error loading file ' + DragItem.Caption ), PChar( E.Message ), Nil );
              end;
            end;
      end
  else if ( Source is TP3DTreeViewDirectory ) then
    with ( TP3DTreeViewDirectory( Source )) do
      begin
        if ( Assigned( DragItem )) then
          if ( P3DMatchFileName( DragItem.Caption, '*.p3d' )) then
            begin
              try
                SceneMain.ActiveLibrary:= P3DData.Libraries[ P3DData.OpenLibrary( AppendPathDelim( Directory ) + DragItem.Caption )];
              except
                on E: Exception do
                  SDL_ShowSimpleMessageBox( SDL_MESSAGEBOX_ERROR, PChar( 'Error loading file ' + DragItem.Caption ), PChar( E.Message ), Nil );
              end;
            end;
      end;
  Accept:= False;
end;


{$ENDIF}
