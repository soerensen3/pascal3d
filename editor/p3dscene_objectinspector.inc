{$IFDEF INTERFACE}
  { TP3DOIPanel }

  TP3DOIPanel = class( TP3DGraphicControl )
    private
      FObjectInspector: TP3DObjectInspector;

    public
      constructor Create(const AOwner: TP3DObjectList=nil; const AParent: TP3DGraphicControl=nil);
      destructor Destroy; override;

    published
      property ObjectInspector: TP3DObjectInspector read FObjectInspector write FObjectInspector;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DOIPanel }

constructor TP3DOIPanel.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  ObjectInspector:= TP3DObjectInspector.Create( nil, Self );
  Width:= 250;
  ObjectInspector.Align:= alClient;
end;

destructor TP3DOIPanel.Destroy;
begin
  inherited Destroy;
end;
{$ENDIF}
