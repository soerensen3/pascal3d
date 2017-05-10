{$IFDEF INTERFACE}
  { TP3DOIPanel }

  TP3DOIPanel = class( TP3DGraphicControl )
    private
      FObjectInspector: TP3DObjectInspector;

      procedure SetObj( Sender: TP3DObjectInspector; AValue: TInterfacedPersistent );

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

    published
      property ObjectInspector: TP3DObjectInspector read FObjectInspector write FObjectInspector;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DOIPanel }

procedure TP3DOIPanel.SetObj(Sender: TP3DObjectInspector;
  AValue: TInterfacedPersistent);
begin
  if ( not ( Assigned( DataView ) and Assigned( DataView.Viewer ))) then
    exit;
  if ( Assigned( AValue )) then
    DataView.Viewer.Selection:= DataView.Viewer.Children.FindByData( Pointer( AValue ), True )
  else
    DataView.Viewer.Selection:= nil;
end;

constructor TP3DOIPanel.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  ObjectInspector:= TP3DObjectInspector.Create( AData, Self );
  Width:= 250;
  ObjectInspector.Align:= alClient;
  ObjectInspector.OnSetObj:= @SetObj;
end;

destructor TP3DOIPanel.Destroy;
begin
  inherited Destroy;
end;
{$ENDIF}
