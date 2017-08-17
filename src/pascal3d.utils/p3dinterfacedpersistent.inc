{$IFDEF INTERFACE}
type
  { TP3DInterfacedPersistent }

  TP3DInterfacedPersistent = class ( TInterfacedPersistent )
    private
      FProperties: TP3DPropertyList;
      function GetProperties: TP3DPropertyList;

    public
      constructor Create;
      constructor CreateFromJSONContext( AContext: TP3DJSONContext ); virtual;
      destructor Destroy; override;

      function SaveToDOM( AParent: TDOMElement ): TDOMElement; virtual; deprecated;
      procedure LoadFromDOMNew( ADOMNode: TDOMElement ); virtual; deprecated;
      procedure LoadFromJSONContext( AContext: TP3DJSONContext ); virtual;
      function Serialize: String;

      class function DOMNodeName: String; virtual; abstract; deprecated;

      property Properties: TP3DPropertyList read FProperties write FProperties;
  end;

  TP3DInterfacedPersistentType = class of TP3DInterfacedPersistent;


  IP3DPropertyAccessInterfacedPersistent = interface
    ['{CCCF4A09-B5A9-4EF6-AC58-D9C453D6055B}']
    function GetProperties: TP3DPropertyList;
    function GetObjectValue: TP3DInterfacedPersistent;
    property Properties: TP3DPropertyList read GetProperties;
  end;

  { gP3DPropertyAccessInterfacedPersistent }

  generic gP3DPropertyAccessInterfacedPersistent < TItem: TP3DInterfacedPersistent > = class( specialize gP3DPropertyAccess < TItem >, IP3DPropertyAccessInterfacedPersistent )
    protected
      function GetProperties: TP3DPropertyList;
      function GetDefaultValue: TItem; override;
      function GetChildPropByPath(APath: String): TP3DPropertyAccess; override;
      function GetObjectValue: TP3DInterfacedPersistent;

    public
      function SaveToDOM( AParentNode: TDOMElement ): Boolean; override;
      procedure LoadFromDOM(AParentNode: TDOMElement); override;
      procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
      procedure CreateValueFromJSONContext( AClass: TP3DInterfacedPersistentType; AContext: TP3DJSONContext ); virtual;
      procedure ValueCreateNew( ClTp: TP3DInterfacedPersistentType; AContext: TP3DJSONContext ); virtual;
      function Serialize: String; override;

      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      property Properties: TP3DPropertyList read GetProperties;
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

  {{ gP3DTransformPersistent }

  generic gP3DTransformPersistent < TCustomItem: TP3DInterfacedPersistent > = class ( specialize gP3DTransform < TCustomItem > )
    public
      procedure Create;

  end;}

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DInterfacedPersistent }

function TP3DInterfacedPersistent.GetProperties: TP3DPropertyList;
begin
  Result:= FProperties;
end;

constructor TP3DInterfacedPersistent.Create;
begin
  inherited Create;
  Properties:= TP3DPropertyList.Create( Self );
end;

constructor TP3DInterfacedPersistent.CreateFromJSONContext(AContext: TP3DJSONContext);
begin
  Create;
  LoadFromJSONContext( AContext );
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
      propI:= Properties.FindByName( ADOMNode.Attributes[ i ].NodeName );
      //WriteLn( ADOMNode.Attributes[ i ].NodeName, ' = ', ADOMNode.Attributes[ i ].NodeValue, ' @', propI );
      if ( propI > -1 ) then
        begin
          if ( Properties[ propI ].StoreMode = smAttribute ) then
            Properties[ propI ].AsString:= ADOMNode.Attributes[ i ].NodeValue;
          //WriteLn( 'Set to : ', Properties[ propI ].AsString );
        end;
    end;
end;

procedure TP3DInterfacedPersistent.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  Properties.LoadFromJSONContext( AContext );
end;

function TP3DInterfacedPersistent.Serialize: String;
begin
  Result:= '{ "ClassName" : "' + ClassName + '", ' + Properties.Serialize() + '}'
end;

{ gP3DPropertyAccessInterfacedPersistent }

function gP3DPropertyAccessInterfacedPersistent.GetProperties: TP3DPropertyList;
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

procedure gP3DPropertyAccessInterfacedPersistent.LoadFromJSONContext(AContext: TP3DJSONContext);
var
  ClName, Path: TJSONData;
  SPath: String;
  Cl: TP3DInterfacedPersistentType;
begin
  if ( AContext.Data.JSONType = jtString ) then
    begin
      SPath:= AContext.Data.AsString;
      if (( SPath = '' ) or ( SPath = 'None' )) then
        begin
          Value:= nil;
          exit;
        end;
      WriteLn( 'Load ' + TItem.ClassName + ' from path: ', SPath );
      Path:= AContext.JSONLoader.FindPath( SPath );
      if ( Assigned( Path )) then
        LoadFromJSONContext( P3DJSONContext( AContext.JSONLoader, Path, AContext.Owner, AContext.Parent ))
      else
        raise Exception.CreateFmt( 'Cannot load %s from path "%s" because the path is invalid!', [ TItem.ClassName, AContext.Data.AsString ]);
    end
  else if ( AContext.Data.JSONType = jtObject ) then
    begin
      try
        ClName:= AContext.Data.FindPath( 'ClassName' );
        if ( not Assigned( ClName )) then
          Cl:= TItem
        else if ( ClName.JSONType = jtString ) then
          begin
            WriteLn( '#: ', ClName.AsString );
            Cl:= P3DClassFactory.FindByName( ClName.AsString );
          end
        else
          raise Exception.CreateFmt( P3DERROR_JSON_CLASS_TYPE_NOT_FOUND, [ ClName.AsString ]);
        CreateValueFromJSONContext( Cl, AContext );
      except
        on E: Exception do
          Exception.CreateFmt( P3DERROR_JSON_CLASS_TYPE_NODE_NOT_FOUND, []);
      end;
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtObject )  + ' or as JSON path (' + JSONTypeName( jtString ) + ')', JSONTypeName( AContext.Data.JSONType )]);
end;

procedure gP3DPropertyAccessInterfacedPersistent.CreateValueFromJSONContext(AClass: TP3DInterfacedPersistentType; AContext: TP3DJSONContext);
begin
  Value:= TItem( AClass.CreateFromJSONContext( AContext ));
end;

procedure gP3DPropertyAccessInterfacedPersistent.ValueCreateNew(ClTp: TP3DInterfacedPersistentType; AContext: TP3DJSONContext);
begin
  if ( Assigned( ClTp )) then
    Value:= TItem( ClTp.Create());
end;

function gP3DPropertyAccessInterfacedPersistent.Serialize: String;
begin
  if ( Assigned( Value )) then
    Result:= Value.Serialize
  else
    Result:= '""';
end;

function gP3DPropertyAccessInterfacedPersistent.GetAsString: String;
var
  Prop: TP3DPropertyAccess;
begin
  if ( Assigned( Value )) then
    begin
      Prop:= Value.Properties.GetPropByName( 'Name' );
      if ( Assigned( Prop )) then
        Result:= '{ "' + Prop.AsString + ':' + Value.ClassName + '" : {' + Value.Properties.GetAsString() + '}}'
      else
        Result:= '{ "' + Value.ClassName + '" : {' + Value.Properties.GetAsString() + '}}';
    end
  else
    Result:= Prop.AsString + ':' + Value.ClassName + ' = <None>';
end;

procedure gP3DPropertyAccessInterfacedPersistent.SetAsString(AValue: String);
begin

end;

{ gP3DInterfacedPersistentList }

procedure gP3DInterfacedPersistentList.SaveToDOM(AParent: TDOMElement);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].SaveToDOM( AParent );
end;

procedure gP3DInterfacedPersistentList.LoadFromDOM(ADOMNode: TDOMElement);
begin

end;

{$ENDIF}

{$IFDEF INITIALIZATION}

{$ENDIF}

{$IFDEF FINALIZATION}
{$ENDIF}
