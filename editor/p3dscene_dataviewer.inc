{$IFDEF INTERFACE}
type

  { TP3DDataPanel }

  TP3DDataPanel = class( TP3DTreeViewActors )
    private
      FLibraryPopup: TP3DPopupMenu;

    public
      constructor Create(const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

      procedure DataMouseClick(Sender:TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
      procedure PopupMouseClick(Sender:TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer);
      procedure SelectionChange( var NewSelection: TP3DTreeNode );

    published
      property LibraryPopup: TP3DPopupMenu read FLibraryPopup write FLibraryPopup;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DDataPanel }

constructor TP3DDataPanel.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  ShowSymbols:= True;
  Width:= 200;
  Update;
  OnSelectionChange:= @SelectionChange;
  LibraryPopup:= TP3DPopupMenu.Create();
  LibraryPopup.Items[ LibraryPopup.Items.Add( 'Category' )].Enabled:= False;
  LibraryPopup.Items[ LibraryPopup.Items.Add( '  Reload' )].OnMouseClick:= @PopupMouseClick;
  LibraryPopup.Items[ LibraryPopup.Items.Add( '  Remove' )].OnMouseClick:= @PopupMouseClick;
  OnMouseClick:= @DataMouseClick;
end;

destructor TP3DDataPanel.Destroy;
begin
  LibraryPopup.Free;
  DataView.Free;
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

procedure TP3DDataPanel.PopupMouseClick(Sender: TP3DGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);
begin
  if ( Sender is TP3DButton ) then
    with ( TP3DButton( Sender )) do
      begin
        case Caption of
          'Reload': if ( TObject( DataView.Selection.Data ) is TP3DDataBlock ) then
            TP3DDataBlock( DataView.Selection.Data ).Reload;
          'Remove':
              if ( TObject( DataView.Selection.Data ) is TP3DDataBlock ) then
                begin
                  TP3DDataBlock( DataView.Selection.Data ).Free;
                  if ( Selection.Data = DataView.Selection.Data ) then
                    Selection:= nil;
                  DataView.Update;
                  DataView.Children.ExpandAll();
                end;
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
