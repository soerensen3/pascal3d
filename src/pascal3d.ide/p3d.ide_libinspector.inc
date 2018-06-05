{ p3d.ide_libinspector.inc }

{$IFDEF INTERFACE}
type

  { TP3DLibInspector }

  TP3DLibInspector = class( TP3DTreeViewObjects )
    protected
      FTextLib, FTextMaterial, FTextMesh,
        FTextScene, FTextTexture: TP3DTextBmp;

      function DrawNode(p: TVec2; Node: TP3DTreeNode; var MaxX: Float): TVec2; override;
      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float); override;

    public
      destructor Destroy; override;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DLibInspector }

function TP3DLibInspector.DrawNode(p: TVec2; Node: TP3DTreeNode; var MaxX: Float): TVec2;
var
  oldCl: TP3DColorRGBA;
begin
  Result:= inherited DrawNode( p, Node, MaxX );
  p-= vec2( ScrollBarH.Position, ScrollBarV.Position );
  oldCl:= Canvas.Font.Color;
  Canvas.Font.Color:= Red500;
  if ( TObject( Node.Data ) = P3DMainIntf.ActiveLibrary ) then begin
    Canvas.GetValidText( P3DSymbolLibrary, FTextLib, P3DFontManager.Fonts[ P3DSymbolFontName, Canvas.Font.Size ]);
    Canvas.RenderText( FTextLib, vec2( ClippedRect.Width - FTextLib.Width - 3, p.y ));

  end else if ( TObject( Node.Data ) = P3DMainIntf.ActiveMaterial ) then begin
    Canvas.GetValidText( P3DSymbolMaterial, FTextMaterial, P3DFontManager.Fonts[ P3DSymbolFontName, Canvas.Font.Size ]);
    Canvas.RenderText( FTextMaterial, vec2( ClippedRect.Width - FTextMaterial.Width - 3, p.y ));

  end else if ( TObject( Node.Data ) = P3DMainIntf.ActiveMesh ) then begin
    Canvas.GetValidText( P3DSymbolMesh, FTextMesh, P3DFontManager.Fonts[ P3DSymbolFontName, Canvas.Font.Size ]);
    Canvas.RenderText( FTextMesh, vec2( ClippedRect.Width - FTextMesh.Width - 3, p.y ));

  end else if ( TObject( Node.Data ) = P3DMainIntf.ActiveScene ) then begin
    Canvas.GetValidText( P3DSymbolScene, FTextScene, P3DFontManager.Fonts[ P3DSymbolFontName, Canvas.Font.Size ]);
    Canvas.RenderText( FTextScene, vec2( ClippedRect.Width - FTextScene.Width - 3, p.y ));

  end else if ( TObject( Node.Data ) = P3DMainIntf.ActiveTexture ) then begin
    Canvas.GetValidText( P3DSymbolTexture, FTextTexture, P3DFontManager.Fonts[ P3DSymbolFontName, Canvas.Font.Size ]);
    Canvas.RenderText( FTextTexture, vec2( ClippedRect.Width - FTextTexture.Width - 3, p.y ));
  end;

  Canvas.Font.Color:= oldCl;
end;

procedure TP3DLibInspector.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
type
  TP3DEditorClass = class of TP3DEditor;
var
  Popup: TP3DPopupMenu = nil;

  function FindPopup( EditorClass: TP3DEditorClass ): TP3DPopupMenu;
  var
    Ed: TP3DEditor;
  begin
    Ed:= TP3DEditor( P3DMainIntf.Editors.First( EditorClass ));
    if ( Assigned( Ed )) then
      Result:= TP3DPopupMenu( Ed.GetPopupMenu )
    else
      Result:= nil;
  end;

  function FindPopupList( EditorClass: TP3DEditorClass ): TP3DPopupMenu;
  var
    Ed: TP3DEditor;
  begin
    Ed:= TP3DEditor( P3DMainIntf.Editors.First( EditorClass ));
    if ( Assigned( Ed )) then
      Result:= TP3DPopupMenu( Ed.GetPopupMenuList )
    else
      Result:= nil;
  end;

begin
  inherited MouseClick( mb1, mb2, mb3, X, Y );
  if ( mb3 ) then begin
    inherited MouseClick( True, False, False, X, Y ); //Select with right click
    if ( Assigned( Selection )) then begin
      if ( TObject( Selection.Data ) is TP3DLibrary ) then
        Popup:= FindPopup( TP3DEditorLibrary )
      else if ( TObject( Selection.Data ) is TP3DLibraryContainerList ) then
        Popup:= FindPopupList( TP3DEditorLibrary );
      if ( Assigned( Popup )) then
        Popup.PopUp( ClippedRect.Left + X, ClippedRect.Top + Y );
    end;
  end;
end;

destructor TP3DLibInspector.Destroy;
begin
  FreeAndNil( FTextLib );
  FreeAndNil( FTextMaterial );
  FreeAndNil( FTextMesh );
  FreeAndNil( FTextScene );
  FreeAndNil( FTextTexture );
  inherited Destroy;
end;


{$ENDIF}

