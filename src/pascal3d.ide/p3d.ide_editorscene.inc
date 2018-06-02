{ p3d.ide_editorscene.inc }

{$IFDEF INTERFACE}

{ TP3DEditorScene }

TP3DEditorScene = class ( TP3DEditor )
   public
     constructor Create( const AOwner: IP3DStreamableContainer=nil ); override;
     procedure ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String); override;
     function GetProperty: TP3DStreamable; override;
     procedure SetProperty(AValue: TP3DStreamable); override;
end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DEditorScene }

constructor TP3DEditorScene.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FName:= 'Scene';
  FCaption:= P3DSymbolScene;
  FButtons:= P3DSymbolSceneNew + LineEnding +
             P3DSymbolSceneDelete + LineEnding +
             P3DSymbolObject;
  FProp:= TP3DStreamablePointer.CreateGetSet( 'Active Scene', @GetProperty, @SetProperty );
  FProp.FItemClass:= TP3DScene;

  Properties.Add( FProp );
end;

procedure TP3DEditorScene.ToolbarBtnClick(AButtonIdx: Integer; AButtonCaption: String);
begin
  if ( AButtonCaption = P3DSymbolSceneNew ) then
//    P3DMainIntf.NewScene( Self )
  else if ( AButtonCaption = P3DSymbolSceneDelete ) then
//    P3DMainIntf.DeleteScene( Self );
  else if ( AButtonCaption = P3DSymbolObject ) then
    if ( Assigned( P3DMainIntf.ActiveScene )) then
      P3DMainIntf.ActiveScene.Objects.Add( TP3DObject.CreateBlock( P3DMainIntf.ActiveScene.OwnerLib ));
end;

function TP3DEditorScene.GetProperty: TP3DStreamable;
begin
  Result:= P3DMainIntf.ActiveScene;
end;

procedure TP3DEditorScene.SetProperty(AValue: TP3DStreamable);
begin
  P3DMainIntf.SetActiveScene( Self, AValue );
end;

{$ENDIF}

