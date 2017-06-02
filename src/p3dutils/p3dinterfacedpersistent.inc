{$IFDEF INTERFACE}
{ TP3DInterfacedPersistent }

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


IP3DPropertyAccessInterfacedPersistent = interface
  ['{CCCF4A09-B5A9-4EF6-AC58-D9C453D6055B}']
  function GetProperties: TP3DPropertyAccessList;
  function GetObjectValue: TP3DInterfacedPersistent;
  property Properties: TP3DPropertyAccessList read GetProperties;
end;

{ gP3DPropertyAccessInterfacedPersistent }

generic gP3DPropertyAccessInterfacedPersistent < TItem: TP3DInterfacedPersistent > = class( specialize gP3DPropertyAccess < TItem >, IP3DPropertyAccessInterfacedPersistent )
  protected
    function GetProperties: TP3DPropertyAccessList;
    function GetDefaultValue: TItem; override;
    function GetChildPropByPath(APath: String): TP3DPropertyAccess; override;
    function GetObjectValue: TP3DInterfacedPersistent;

  public
    function SaveToDOM( AParentNode: TDOMElement ): Boolean; override;
    procedure LoadFromDOM(AParentNode: TDOMElement); override;

    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    property Properties: TP3DPropertyAccessList read GetProperties;
end;

{ gP3DInterfacedPersistentList }

generic gP3DInterfacedPersistentList < TItem: TP3DInterfacedPersistent > = class( specialize gP3DCustomObjectList < TItem >)
  public
    procedure SaveToDOM( AParent: TDOMElement ); virtual;
    procedure LoadFromDOM( ADOMNode: TDOMElement ); virtual;
end;

TP3DInterfacedPersistentList = specialize gP3DInterfacedPersistentList < TP3DInterfacedPersistent >;


TP3DPropertyAccessInterfacedPersistent = specialize gP3DPropertyAccessInterfacedPersistent < TP3DInterfacedPersistent >;

TP3DPropertyAccessInterfacedPersistentList = specialize gP3DListPropertyAccessObject < TP3DInterfacedPersistentList, TP3DInterfacedPersistent, TP3DPropertyAccessInterfacedPersistent >;

{ gP3DTransformPersistent }

generic gP3DTransformPersistent < TCustomItem: TP3DInterfacedPersistent > = class ( specialize gP3DTransform < TCustomItem > )
  public
    procedure Create;

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
        begin
          if ( Properties[ propI ].StoreMode = smAttribute ) then
            Properties[ propI ].AsString:= ADOMNode.Attributes[ i ].NodeValue;
          //WriteLn( 'Set to : ', Properties[ propI ].AsString );
        end;
    end;
end;

{ gP3DPropertyAccessInterfacedPersistent }

function gP3DPropertyAccessInterfacedPersistent.GetProperties: TP3DPropertyAccessList;
begin
  if ( Assigned( Value )) then
    Result:= Value.Properties
  else
    Result:= nil;
end;

function gP3DPropertyAccessInterfacedPersistent.GetDefaultValue: TItem;
begin
  Result:= nil;
end;

function gP3DPropertyAccessInterfacedPersistent.GetChildPropByPath(APath: String): TP3DPropertyAccess;
begin
  if ( Assigned( Value )) then
    Result:= Value.Properties.GetPropByPath( APath );
end;

function gP3DPropertyAccessInterfacedPersistent.GetObjectValue: TP3DInterfacedPersistent;
begin
  Result:= Value;
end;

function gP3DPropertyAccessInterfacedPersistent.SaveToDOM(
  AParentNode: TDOMElement): Boolean;
begin
  Result:= inherited SaveToDOM( AParentNode );
  if (( Result ) and ( Assigned( Value ))) then
    Value.SaveToDOM( AParentNode );
end;

procedure gP3DPropertyAccessInterfacedPersistent.LoadFromDOM(
  AParentNode: TDOMElement);
begin
  inherited LoadFromDOM(AParentNode);
end;

function gP3DPropertyAccessInterfacedPersistent.GetAsString: String;
var
  Prop: TP3DPropertyAccess;
begin
  if ( Assigned( Value )) then
    begin
      Prop:= Value.Properties.GetPropByName( 'Name' );
      if ( Assigned( Prop )) then
        Result:= Prop.AsString + ':' + Value.ClassName + ' = <' + Value.Properties.ToStr() + '>'
      else
        Result:= Value.ClassName + ' = <' + Value.Properties.ToStr() + '>';
    end
  else
    Result:= Prop.AsString + ':' + Value.ClassName + ' = <None>';
end;

procedure gP3DPropertyAccessInterfacedPersistent.SetAsString(AValue: String);
begin

end;

{$ENDIF}
