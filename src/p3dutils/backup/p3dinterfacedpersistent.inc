{$IFDEF INTERFACE}
{ TP3DInterfacedPersistent }

TP3DPropertyAccessList = class;

TP3DInterfacedPersistent = class ( TInterfacedPersistent )
  private
    FProperties: TP3DPropertyAccessList;
    function GetProperties: TP3DPropertyAccessList;

  public
    constructor Create;
    destructor Destroy; override;

    function SaveToDOM( AParent: TDOMElement ): TDOMElement; virtual;
    procedure LoadFromDOMNew( ADOMNode: TDOMElement ); virtual;

    class function DOMNodeName: String; virtual; abstract;

    property Properties: TP3DPropertyAccessList read FProperties write FProperties;
end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DInterfacedPersistent }

function TP3DInterfacedPersistent.GetProperties: TP3DPropertyAccessList;
begin
  Result:= FProperties;
end;

constructor TP3DInterfacedPersistent.Create;
begin
  inherited Create;
  Properties:= TP3DPropertyAccessList.Create( Self );
end;

destructor TP3DInterfacedPersistent.Destroy;
begin
  Properties.Clear( True );
  Properties.Free;
  inherited Destroy;
end;

function TP3DInterfacedPersistent.SaveToDOM( AParent: TDOMElement ): TDOMElement;
var
  i: Integer;
begin
  Result:= AParent.OwnerDocument.CreateElement( DOMNodeName );
  AParent.AppendChild( Result );

  for i:= 0 to Properties.Count - 1 do
    Properties[ i ].SaveToDOM( Result );
end;

procedure TP3DInterfacedPersistent.LoadFromDOMNew(ADOMNode: TDOMElement);
var
  i, propI: Integer;
begin
  for i:= 0 to ADOMNode.Attributes.Length - 1 do
    begin
      //WriteLn( ADOMNode.Attributes[ i ].NodeName, ' = ', ADOMNode.Attributes[ i ].NodeValue );
      propI:= Properties.FindByName( ADOMNode.Attributes[ i ].NodeName );
      if ( propI > -1 ) then
        Properties[ propI ].AsString:= ADOMNode.Attributes[ i ].NodeValue;
    end;
end;

{$ENDIF}
