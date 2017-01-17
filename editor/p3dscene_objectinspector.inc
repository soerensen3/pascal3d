{$IFDEF INTERFACE}
  { TP3DOIPanel }

  TP3DOIPanel = class( TP3DGraphicControl )
    private
      FObjectInspector: TP3DObjectInspector;

    public
      constructor Create(const AData: TP3DLibrary = nil; const AParent: TP3DGraphicControl=nil); override;
      destructor Destroy; override;

    published
      property ObjectInspector: TP3DObjectInspector read FObjectInspector write FObjectInspector;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DOIPanel }

constructor TP3DOIPanel.Create(const AData: TP3DLibrary; const AParent: TP3DGraphicControl);
begin
  inherited Create( AData, AParent );
  ObjectInspector:= TP3DObjectInspector.Create( AData, Self );
  Width:= 250;
  ObjectInspector.Align:= alClient;
end;

destructor TP3DOIPanel.Destroy;
begin
  inherited Destroy;
end;
{$ENDIF}
