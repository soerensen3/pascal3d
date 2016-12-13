{$IFDEF INTERFACE}
type

  { TP3DDataPanel }

  TP3DDataPanel = class( TP3DTreeViewActors )
    private
      FLibraryPopup: TP3DPopupMenu;

    public
      constructor Create(const AData: TP3DData = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

      procedure DataMouseClick(Sender:TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
      procedure PopupMouseClick( Sender: TP3DMenuItem );
      procedure SelectionChange( var NewSelection: TP3DTreeNode );

    published
      property LibraryPopup: TP3DPopupMenu read FLibraryPopup write FLibraryPopup;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DDataPanel }

constructor TP3DDataPanel.Create(const AData: TP3DData;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  ShowSymbols:= True;
  Width:= 200;
  Update;
  OnSelectionChange:= @SelectionChange;
  LibraryPopup:= TP3DPopupMenu.Create( AData );
  LibraryPopup.OnItemClick:= @PopupMouseClick;
  LibraryPopup.Items[ LibraryPopup.Items.Add( 'Action' )].Enabled:= False;
  LibraryPopup.Items.Add( '  Reload' );
  LibraryPopup.Items.Add( '  Remove' );
  LibraryPopup.Items[ LibraryPopup.Items.Add( 'Add' )].Enabled:= False;
  LibraryPopup.Items.Add( '  Light' );
  LibraryPopup.Items.Add( '  Camera' );
  LibraryPopup.Items.Add( '  TileGrid' );
  OnMouseClick:= @DataMouseClick;
end;

destructor TP3DDataPanel.Destroy;
begin
  if (( Assigned( LibraryPopup )) and ( P3DData.IsValid( LibraryPopup ))) then
    LibraryPopup.Free;
  inherited Destroy;
end;

procedure TP3DDataPanel.DataMouseClick(Sender: TP3DGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);
begin
  if ( mb3 ) then
    if ( Assigned( DataView.Selection )) then
      if ( TPersistent( DataView.Selection.Data ) is TP3DData or TPersistent( DataView.Selection.Data ) is TP3DDataBlock ) then
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
    if ( TObject( DataView.Selection.Data ) is TP3DDataBlock ) then
      Obj:= TP3DDataBlock( DataView.Selection.Data ).Owner.Objects[ TP3DDataBlock( DataView.Selection.Data ).Owner.CreateNew( TP )]
    else if ( TObject( DataView.Selection.Data ) is TP3DData ) then
      Obj:= TP3DData( DataView.Selection.Data ).Objects[ TP3DData( DataView.Selection.Data ).CreateNew( TP )];

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
          '  Reload': if ( TObject( DataView.Selection.Data ) is TP3DDataBlock ) then
            TP3DDataBlock( DataView.Selection.Data ).Reload;
          '  Remove':
              if ( TObject( DataView.Selection.Data ) is TP3DData ) then
                begin
                  P3DData.Libraries.Remove( TP3DData( DataView.Selection.Data ));
                  TP3DData( DataView.Selection.Data ).Free;
                  if ( Selection.Data = DataView.Selection.Data ) then
                    Selection:= nil;
                  DataView.Update;
                  DataView.Children.ExpandAll();
                end
              else if ( TObject( DataView.Selection.Data ) is TP3DDataBlock ) then
                begin
                  TP3DDataBlock( DataView.Selection.Data ).Free;
                  if ( Selection.Data = DataView.Selection.Data ) then
                    Selection:= nil;
                  DataView.Update;
                  DataView.Children.ExpandAll();
                end;
          '  Light': AddToParentScene( DataView.Selection.Data, TP3DLight );
          '  Camera': AddToParentScene( DataView.Selection.Data, TP3DCamera );
          '  TileGrid': AddToParentScene( DataView.Selection.Data, TP3DTileGrid );
        end;
        LibraryPopup.Hide;
      end;
end;

procedure TP3DDataPanel.SelectionChange(var NewSelection: TP3DTreeNode);
begin
  if (( Assigned( NewSelection ) and ( TPersistent( NewSelection.Data ) is TInterfacedPersistent ))) then
    begin
      OIPanel.ObjectInspector.Obj:= TInterfacedPersistent( NewSelection.Data );
      if ( TInterfacedPersistent( NewSelection.Data ) is TP3DActor ) then
        SceneView.FSelection:= TP3DActor( NewSelection.Data )
      else
        SceneView.FSelection:= nil;
    end
  else
    begin
      OIPanel.ObjectInspector.Obj:= nil;
      SceneView.FSelection:= nil;
    end;
end;


{$ENDIF}
