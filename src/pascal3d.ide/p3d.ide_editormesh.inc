{ p3d.ide_editorMesh.inc }

{$IFDEF INTERFACE}

{ TP3DEditorMesh }

TP3DEditorMesh = class ( TP3DEditor )
   public
     constructor Create( const AOwner: IP3DStreamableContainer=nil ); override;
     procedure ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String); override;
     function GetProperty: TP3DStreamable; override;
     procedure SetProperty(AValue: TP3DStreamable); override;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DEditorMesh }

constructor TP3DEditorMesh.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FName:= 'Mesh';
  FCaption:= P3DSymbolMesh;
  FButtons:= P3DSymbolMeshNew + LineEnding +
             P3DSymbolMeshDelete + LineEnding +
             P3DSymbolMaterial;
  FProp:= TP3DStreamablePointer.CreateGetSet( 'Active Mesh', @GetProperty, @SetProperty );
  FProp.FItemClass:= TP3DMesh;

  Properties.Add( FProp );
end;

procedure TP3DEditorMesh.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  if ( AButtonCaption = P3DSymbolMeshNew ) then
//    P3DMainIntf.NewMesh( Self )
  else if ( AButtonCaption = P3DSymbolMeshDelete ) then
//    P3DMainIntf.DeleteMesh( Self );
  else if ( AButtonCaption = P3DSymbolMaterial ) then
    P3DMainIntf.AssignMeshMaterial( Self );
end;

function TP3DEditorMesh.GetProperty: TP3DStreamable;
begin
  Result:= P3DMainIntf.ActiveMesh;
end;

procedure TP3DEditorMesh.SetProperty(AValue: TP3DStreamable);
begin
  P3DMainIntf.SetActiveMesh( Self, AValue );
end;

{$ENDIF}

