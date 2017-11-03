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

      procedure LoadFromJSONContext( AContext: TP3DJSONContext ); virtual;
      function Serialize: String;

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
      procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
      procedure CreateValueFromJSONContext( AClass: TP3DInterfacedPersistentType; AContext: TP3DJSONContext ); virtual;
      procedure ValueCreateNew( ClTp: TP3DInterfacedPersistentType; AContext: TP3DJSONContext ); virtual;
      function Serialize: String; override;

      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      property Properties: TP3DPropertyList read GetProperties;
  end;

  { gP3DPropertyAccessInterfacedPersistent_NoCreate }

  generic gP3DPropertyAccessInterfacedPersistent_NoCreate < TItem: TP3DInterfacedPersistent > = class( specialize gP3DPropertyAccessInterfacedPersistent < TItem >)
    public
      procedure ValueCreateNew(ClTp: TP3DInterfacedPersistentType; AContext: TP3DJSONContext); override;
  end;

  TP3DInterfacedPersistentList = specialize gP3DCustomObjectList < TP3DInterfacedPersistent >;


  TP3DPropertyAccessInterfacedPersistent = specialize gP3DPropertyAccessInterfacedPersistent < TP3DInterfacedPersistent >;
  TP3DPropertyAccessInterfacedPersistent_NoCreate = specialize gP3DPropertyAccessInterfacedPersistent_NoCreate < TP3DInterfacedPersistent >;

  TP3DPropertyAccessInterfacedPersistentList = specialize gP3DListPropertyAccessObject < TP3DInterfacedPersistentList, TP3DInterfacedPersistent, TP3DPropertyAccessInterfacedPersistent >;

  { gP3DListPropertyAccessInterfacedPersistent }

  generic gP3DListPropertyAccessInterfacedPersistent < TListProp, TListData, TPropType > = class ( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
    public
      procedure SetArrayCount(AValue: Integer); override;
      procedure ValueCreateNew( AContext: TP3DJSONContext ); virtual;
      procedure LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext); override;
  end;

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

procedure TP3DInterfacedPersistent.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  Properties.LoadFromJSONContext( AContext );
end;

function TP3DInterfacedPersistent.Serialize: String;
begin
  Result:= '{ "ClassName" : "' + ClassName + '"' + ifthen( Properties.Count > 0, ', ' + Properties.Serialize(), '' ) + ' }'
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
        if ( Assigned( Cl )) then
          CreateValueFromJSONContext( Cl, AContext )
        else if ( Assigned( Value )) then
          Value.LoadFromJSONContext( AContext );
      except
        on E: Exception do
          Exception.CreateFmt( P3DERROR_JSON_CLASS_TYPE_NODE_NOT_FOUND, []);
      end;
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE + ': ' + AContext.Data.AsJSON, [ JSONTypeName( jtObject )  + ' or as JSON path (' + JSONTypeName( jtString ) + ')', JSONTypeName( AContext.Data.JSONType )]);
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

{ gP3DListPropertyAccessInterfacedPersistent }

procedure gP3DListPropertyAccessInterfacedPersistent.SetArrayCount(AValue: Integer);
begin
  if ( Assigned( Value )) then
    Value.Count:= AValue;
end;

procedure gP3DListPropertyAccessInterfacedPersistent.ValueCreateNew(AContext: TP3DJSONContext);
begin
  //if ( not Assigned( Value )) then
  //  Value:= TItem.Create( AContext.Owner );
  //TODO: FINISH
end;

procedure gP3DListPropertyAccessInterfacedPersistent.LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext);
begin
  inherited LoadItemFromJSONContext(AIndex, AContext);
end;



{$ENDIF}

{$IFDEF INITIALIZATION}

{$ENDIF}

{$IFDEF FINALIZATION}
{$ENDIF}
