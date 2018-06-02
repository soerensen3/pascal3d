{ p3d.ide_editormaterial.inc }

{$IFDEF INTERFACE}

{ TP3DEditorMaterial }

TP3DEditorMaterial = class ( TP3DEditor )
   public
     constructor Create( const AOwner: IP3DStreamableContainer=nil ); override;
     procedure ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String); override;
     function GetProperty: TP3DStreamable; override;
     procedure SetProperty(AValue: TP3DStreamable); override;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DEditorMaterial }

constructor TP3DEditorMaterial.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FName:= 'Material';
  FCaption:= P3DSymbolMaterial;
  FButtons:= P3DSymbolMaterialNew + LineEnding +
             P3DSymbolMaterialDelete + LineEnding +
             P3DSymbolTextureNew;
  FProp:= TP3DStreamablePointer.CreateGetSet( 'Active Material', @GetProperty, @SetProperty );
  FProp.FItemClass:= TP3DMaterial;

  Properties.Add( FProp );
end;

procedure TP3DEditorMaterial.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  if ( AButtonCaption = P3DSymbolMaterialNew ) then
    P3DMainIntf.NewMaterial( Self )
  else if ( AButtonCaption = P3DSymbolMaterialDelete ) then
    P3DMainIntf.DeleteMaterial( Self )
  else if ( AButtonCaption = P3DSymbolTextureNew ) then
    P3DMainIntf.NewMaterialMap( Self );
end;

function TP3DEditorMaterial.GetProperty: TP3DStreamable;
begin
  Result:= P3DMainIntf.ActiveMaterial;
end;

procedure TP3DEditorMaterial.SetProperty(AValue: TP3DStreamable);
begin
  P3DMainIntf.SetActiveMaterial( Self, AValue );
end;

{$ENDIF}

