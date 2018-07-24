{ p3d.ide_libinspector.inc }

{$IFDEF INTERFACE}
type

  { TP3DLibInspector }

  TP3DLibInspector = class( TP3DTreeViewObjects )
    protected
      function DrawNode(p: TVec2; Node: TP3DTreeNode; var MaxX: Float): TVec2; override;

    public
      procedure MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float); override;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DLibInspector }

function TP3DLibInspector.DrawNode(p: TVec2; Node: TP3DTreeNode; var MaxX: Float): TVec2;
begin
  if (( TObject( Node.Data ) = P3DMainIntf.ActiveLibrary ) or
      ( TObject( Node.Data ) = P3DMainIntf.ActiveMaterial ) or
      ( TObject( Node.Data ) = P3DMainIntf.ActiveMesh ) or
      ( TObject( Node.Data ) = P3DMainIntf.ActiveScene ) or
      ( TObject( Node.Data ) = P3DMainIntf.ActiveEffect )) then
    SymbolColor:= Red900
  else
    SymbolColor:= Grey900;

  Result:= inherited DrawNode( p, Node, MaxX );
end;

procedure TP3DLibInspector.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
var
  Popup: TP3DPopupMenu = nil;
begin
  inherited MouseClick( mb1, mb2, mb3, X, Y );
  if ( mb3 ) then begin
    if ( Assigned( Selection )) then begin
      if ( Assigned( Selection.Data ) and ( P3DMainIntf.Popups.FindClassPopup( TObject( Selection.Data ).ClassType, Popup ))) then
        Popup.PopUp( ClippedRect.Left + X, ClippedRect.Top + Y );
    end;
  end;
end;


{$ENDIF}

