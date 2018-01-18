//p3dstreamable.inc
//..........................................................................

{$IFDEF INTERFACE}

  { IP3DStreamableContainer }

  IP3DStreamableContainer = interface ( IP3DPropAccessPath )
    ['{A760AED5-D12C-4BC9-BF32-432383751D1F}']

    procedure NotifyOnDelete( AItem: TP3DStreamable );
  end;

  { IP3DPropAccessStreamable }

  IP3DPropAccessStreamable = interface
    ['{CC1223F5-BC4C-4ED2-9406-239D4264617C}']
    function GetValueAsStreamable: TP3DStreamable;
    procedure SetValueAsStreamable( AValue: TP3DStreamable );
  end;

  { IP3DStreamableUser }

  IP3DStreamableUser = interface ( IP3DPropAccessStreamable )
    ['{12C827E2-C8EB-4386-AC66-ABB380F158C3}']
    procedure UpdateUser( Old, New: TP3DStreamable );
  end;

  { TP3DUserList }

  TP3DUserList = class ( specialize TFPGList < IP3DStreamableUser >)
    procedure UpdateAllUsers( Old, New: TP3DStreamable );
  end;

  { TP3DStreamable }

  TP3DStreamable = class ( TInterfacedPersistent, IP3DPropAccessPath )
    private
      FOwner: IP3DStreamableContainer;
      FProperties: TP3DPropertyList;
      FRoot: IP3DLoadJSON;
      FUsers: TP3DUserList;

      function GetJSON: TJSONData;
      function GetRoot: IP3DLoadJSON;
      procedure SetJSON(AValue: TJSONData);

    public
      constructor Create( const AOwner: IP3DStreamableContainer = nil; const ARoot: IP3DLoadJSON = nil ); virtual;
      destructor Destroy; override;

      function GetFullPath: String;

      property Properties: TP3DPropertyList read FProperties;
      property Owner: IP3DStreamableContainer read FOwner write FOwner;
      property Root: IP3DLoadJSON read GetRoot write FRoot;
      property AsJSON: TJSONData read GetJSON write SetJSON;
      property Users: TP3DUserList read FUsers;
  end;

  { gP3DPropAccessStreamable }

  generic gP3DPropAccessStreamable < T > = class ( specialize gP3DPropAccessCustomObject < T >, IP3DPropAccessStreamable, IP3DStreamableContainer )
    protected
      function GetAsJSON: TJSONData; override;
      function GetValueAsStreamable: TP3DStreamable;
      procedure SetValueAsStreamable( AValue: TP3DStreamable );
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      procedure NotifyOnDelete( AItem: TP3DStreamable );
      procedure SetAsValue(AValue: T); override;

    public
      destructor Destroy; override;

      function CreateObjectFromJSON( AJSON: TJSONData ): T; override;
      function GetChildPropByPath( var APath: String; out RestString: String;
       out LastFound: TP3DPropertyAccess): TP3DPropertyAccess; override;
  end;

  { gP3DPropAccessStreamablePointer }

  generic gP3DPropAccessStreamablePointer < T: TP3DStreamable > = class ( specialize gP3DPropAccess < T >, IP3DStreamableUser )
    protected
      function GetAsJSON: TJSONData; override;
      procedure LoadJSON( AValue: TJSONData ); override;
      procedure SetAsValue(AValue: T); override;
      procedure UpdateUser(Old, New: TP3DStreamable);
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      function GetValueAsStreamable: TP3DStreamable;
      procedure SetValueAsStreamable(AValue: TP3DStreamable);

      class function LoadDeferred: Boolean; override;

    public
      destructor Destroy; override;

      procedure Assign(AProp: TP3DPropertyAccess); override;
  end;

  { gP3DStreamableList }

  generic gP3DStreamableList < T : TP3DStreamable > = class( specialize gP3DCustomObjectList < T >)
    private
      FOwner: TP3DStreamable;

    public
      // Changing behaviour that on clear without parameters objects get freed by default. This also means objects get freed when the array is destroyed.
      procedure Clear(const FreeObjects: Boolean=True); override;

      property Root: TP3DStreamable read FOwner write FOwner;
  end;

  { gP3DPropAccessStreamableList }

  generic gP3DPropAccessStreamableList < TList; T: TP3DStreamable;
                                        TItemProp{: TP3DPropertyAccess } >
         = class( specialize gP3DPropAccessArray < TList >, IP3DStreamableContainer )
    protected
      FFakePropIdx: Integer;
      FFakeProp: TItemProp;

      function GetFakeProp: T;
      procedure SetFakeProp(AValue: T);

      procedure SetCount( AValue: Integer ); override;
      function GetCount: Integer; override;
      procedure SetAsValues( Index: Integer; AValue: T );
      function GetAsValues( Index: Integer ): T;
      procedure NotifyOnDelete(AItem: TP3DStreamable);

    public
      destructor Destroy; override;

      function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;
      function IndexOf(AItem: Pointer): Integer; override;
      function GetChildPropByPath(var APath: String; out RestString: String;
       out LastFound: TP3DPropertyAccess): TP3DPropertyAccess; override;

      property AsValues[ Index: Integer ]: T read GetAsValues write SetAsValues;
  end;

  { TP3DNamedStreamable }

  TP3DNamedStreamable = class ( TP3DStreamable )
    private
      FName: String;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil; const ARoot: IP3DLoadJSON= nil); override;
      procedure SetName( AValue: String );

    published
      property Name: String read FName write SetName;
  end;

  IP3DNamedStreamableList = interface ( IP3DPropAccessPath )
    ['{DD922AD9-983B-45D4-8C8E-A9012DE67E89}']
    function FindUniqueName( BaseStr: String ): String;
    function FindByName( AName: String ): Integer;
  end;

  { gP3DNamedStreamableList }

  generic gP3DNamedStreamableList < T: TP3DNamedStreamable > = class ( specialize gP3DStreamableList < T >)
    public
      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
      function Add(Item: TCustomItem): Integer; override;
  end;

  { gP3DPropAccessNamedStreamableList }

  generic gP3DPropAccessNamedStreamableList < TList; T: TP3DNamedStreamable;
                                        TItemProp{: TP3DPropertyAccess } >
         = class( specialize gP3DPropAccessStreamableList < TList, T, TItemProp >, IP3DNamedStreamableList )
    public
      function GetArrayItemAsProp( Idx: Integer ): TP3DPropertyAccess; override;
      function GetArrayIdxByString(Idx: String): Integer; override;

      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
  end;

  TP3DDebugList = specialize gP3DStreamableList < TP3DStreamable >;
  TP3DPropAccessStreamablePointer = specialize gP3DPropAccessStreamablePointer < TP3DStreamable >;
  // TP3DPropAccessStreamablePointerList = specialize gP3DPropAccessArray < TP3DDebugList, TP3DStreamable, TP3DPropAccessStreamablePointer >;

var
 DebugList: TP3DDebugList;
// DebugListProp: Tp3d


{$ENDIF}

{$IFDEF IMPLEMENTATION}
//p3dstreamable.inc
//..........................................................................
{ TP3DUserList }

procedure TP3DUserList.UpdateAllUsers(Old, New: TP3DStreamable);
var
  i: Integer;
begin
  for i:= Count - 1 downto 0 do
    Items[ i ].UpdateUser( Old, New );
end;

{ gP3DStreamableList }

procedure gP3DStreamableList.Clear(const FreeObjects: Boolean);
begin
  inherited Clear(FreeObjects);
end;

{ TP3DStreamable }

function TP3DStreamable.GetJSON: TJSONData;
begin
  Result:= Properties.AsJSON;
end;

function TP3DStreamable.GetRoot: IP3DLoadJSON;
begin
  Result:= FRoot;
end;

procedure TP3DStreamable.SetJSON(AValue: TJSONData);
begin
  Properties.AsJSON:= AValue;
end;

constructor TP3DStreamable.Create(const AOwner: IP3DStreamableContainer;
  const ARoot: IP3DLoadJSON);
begin
  inherited Create;
  FUsers:= TP3DUserList.Create;

  if ( AOwner is IP3DPropAccessPath ) then
    FOwner:= AOwner
  else if ( Assigned( Owner )) then
    raise Exception.Create( 'TP3DStreamable.Create: Wrong owner for streamable. It has to implement IP3DPropAccessPath!' )
  else
    FOwner:= nil;

  if (( not Assigned( Root )) and ( Self is IP3DLoadJSON )) then
    FRoot:= Self as IP3DLoadJSON
  else
    FRoot:= ARoot;

  FProperties:= TP3DPropertyList.Create( Self );

  DebugList.Add( Self );
end;

destructor TP3DStreamable.Destroy;
begin
  Users.UpdateAllUsers( Self, nil );

  if ( Assigned( Owner )) then
    Owner.NotifyOnDelete( Self );

  DebugList.Remove( Self );
  FProperties.Free;
  FUsers.Free;
  inherited Destroy;
end;

function TP3DStreamable.GetFullPath: String;
var
  Prop: TP3DPropertyAccess;
  n: Integer;
begin
  if ( Assigned( Owner )) then
    begin
      if ( Owner is IP3DPropAccessPathArray ) then
        begin
          n:= ( Owner as IP3DPropAccessPathArray ).IndexOf( Self );
          if ( n < 0 ) then
            raise Exception.Create( 'Internal error: The streamable could not be found in the owner!' );
          Prop:= ( Owner as IP3DPropAccessPathArray ).GetArrayItemAsProp( n );
          if ( Assigned( Prop )) then
            Result:= Prop.GetFullPath
          else
            Result:= '';
        end
      else
        Result:= ( Owner as IP3DPropAccessPath ).GetFullPath
    end
  else
    Result:= '';
end;

function gP3DPropAccessStreamable.CreateObjectFromJSON(AJSON: TJSONData): T;
var
  Owner: IP3DPropAccessPath;
begin
  if ( Context is IP3DPropAccessPathArray ) then
    Owner:= Context
  else
    Owner:= Self;
  if ( not ( Owner is IP3DStreamableContainer )) then
    raise Exception.Create( 'Cannot create object from JSON with this owner because it is not implementing IP3DStreamableContainer.' );
  Result:= T.Create( Owner as IP3DStreamableContainer, Context.GetRoot );
  Result.AsJSON:= AJSON;
end;

function gP3DPropAccessStreamable.GetAsString: String;
begin
  Result:= T.ClassName;
end;

procedure gP3DPropAccessStreamable.SetAsString(AValue: String);
begin
  raise Exception.Create( 'Error: Cannot set streamable as string!' );
end;

procedure gP3DPropAccessStreamable.NotifyOnDelete(AItem: TP3DStreamable);
begin
  SetValue( nil );
  //Do nothing. Only required because it needs to implement IP3DStreamableContainer
end;

procedure gP3DPropAccessStreamable.SetAsValue(AValue: T);
var
  V: T;
begin
  V:= GetAsValue;
  if ( AValue = V ) then
    exit;

  if ( Assigned( V )) then
    begin
      V.Users.UpdateAllUsers( V, AValue );
      V.Free;
    end;

  inherited SetAsValue(AValue);
end;

destructor gP3DPropAccessStreamable.Destroy;
begin
  if ( Assigned( AsValue )) then
    AsValue.Free;
  inherited Destroy;
end;

function gP3DPropAccessStreamable.GetChildPropByPath(var APath: String; out
  RestString: String; out LastFound: TP3DPropertyAccess): TP3DPropertyAccess;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.Properties.GetPropByPath( APath, RestString, LastFound )
  else
    Result:= nil;
end;

function gP3DPropAccessStreamable.GetAsJSON: TJSONData;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.AsJSON
  else
    Result:= TJSONNull.Create;
end;

function gP3DPropAccessStreamable.GetValueAsStreamable: TP3DStreamable;
begin
  Result:= AsValue;
end;

procedure gP3DPropAccessStreamable.SetValueAsStreamable(AValue: TP3DStreamable);
begin
  if ( AValue.InheritsFrom( T )) then
    AsValue:= T( AValue )
  else
    raise Exception.CreateFmt( 'gP3DPropAccessStreamable.SetValueAsStreamable: The assigned value is no descendant of the base class "%s" required for the property of type "%s"', [ T.ClassName, ClassName ]);
end;

procedure gP3DPropAccessStreamablePointer.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtString ) then
    AsString:= AValue.AsString
  else if ( AValue.JSONType = jtNull ) then
    AsValue:= nil
  else
    raise Exception.Create( 'Cannot load from StreamablePointer from JSON because the context was no pointer!' );
end;

procedure gP3DPropAccessStreamablePointer.SetAsValue(AValue: T);
var
  Obj: T;
begin
  Obj:= AsValue;
  if ( Obj = AValue ) then
    exit;

  if ( Assigned( Obj )) then
    Obj.Users.Remove( Self );

  inherited SetAsValue(AValue);

  if ( Assigned( AValue )) then
    AValue.Users.Add( Self );
end;

procedure gP3DPropAccessStreamablePointer.UpdateUser(Old, New: TP3DStreamable);
begin
  if ( AsValue = Old ) then
    SetValue( New );
end;

class function gP3DPropAccessStreamablePointer.LoadDeferred: Boolean;
begin
  Result:= True;
end;

destructor gP3DPropAccessStreamablePointer.Destroy;
begin
  SetAsValue( nil );
  inherited Destroy;
end;

function gP3DPropAccessStreamablePointer.GetAsString: String;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.GetFullPath
  else
    Result:= '<None>';
end;

procedure gP3DPropAccessStreamablePointer.SetAsString(AValue: String);
begin
  if ( Assigned( Context ) and Assigned( Context.GetRoot )) then
    Assign( Context.GetRoot.GetJSONLoader().GetPropByPath( AValue, True ));
end;

function gP3DPropAccessStreamablePointer.GetValueAsStreamable: TP3DStreamable;
begin
  Result:= AsValue;
end;

procedure gP3DPropAccessStreamablePointer.SetValueAsStreamable(
  AValue: TP3DStreamable);
begin
  if ( AValue.InheritsFrom( T )) then
    AsValue:= T( AValue )
  else
    raise Exception.CreateFmt( 'gP3DPropAccessStreamablePointer.SetValueAsStreamable: The assigned value is no descendant of the base class "%s" required for the property of type "%s"', [ T.ClassName, ClassName ]);
end;

function gP3DPropAccessStreamablePointer.GetAsJSON: TJSONData;
begin
  if ( Assigned( AsValue )) then
    Result:= TJSONString.Create( AsValue.GetFullPath )
  else
    Result:= TJSONNull.Create;
end;

procedure gP3DPropAccessStreamablePointer.Assign(AProp: TP3DPropertyAccess);
begin
  if ( AProp is IP3DPropAccessStreamable ) then
    AsValue:= ( AProp as IP3DPropAccessStreamable ).GetValueAsStreamable
  else
    inherited Assign( AProp );
end;

function gP3DPropAccessStreamableList.GetFakeProp: T;
begin
  Result:= AsValues[ FFakePropIdx ];
end;

procedure gP3DPropAccessStreamableList.SetFakeProp(AValue: T );
begin
  AsValues[ FFakePropIdx ]:= AValue;
end;

procedure gP3DPropAccessStreamableList.SetCount( AValue: Integer );
begin
  if ( Assigned( AsValue )) then
    AsValue.Count:= AValue;
end;

function gP3DPropAccessStreamableList.GetCount: Integer;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.Count
  else
    Result:= -1;
end;

procedure gP3DPropAccessStreamableList.SetAsValues( Index: Integer; AValue: T );
begin
  if ( Assigned( AsValue )) then
    AsValue[ Index ]:= AValue;
end;

function gP3DPropAccessStreamableList.GetAsValues( Index: Integer ): T;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue[ Index ]
  else
    Result:= nil;
end;

procedure gP3DPropAccessStreamableList.NotifyOnDelete(AItem: TP3DStreamable);
begin
  if ( Assigned( AsValue )) then
    AsValue.Remove( T( AItem ));
end;

destructor gP3DPropAccessStreamableList.Destroy;
var
  L: TList;
begin
  if ( Assigned( FFakeProp )) then
    begin
      FFakeProp.Getter:= nil;
      FFakeProp.Setter:= nil;
      FreeAndNil( FFakeProp );
    end;
  L:= AsValue;
  if ( Assigned( L )) then
    L.Clear( True );
  inherited Destroy;
end;

function gP3DPropAccessStreamableList.GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess;
var
  FakePropName: String;
begin
  FakePropName:= Name + '[' + IntToStr( Idx ) + ']';
  FFakePropIdx:= Idx;
  if ( not Assigned( FFakeProp )) then
    FFakeProp:= TItemProp.CreateGetSet( FakePropName, @GetFakeProp, @SetFakeProp, smDoNotStore )
  else
    FFakeProp.Name:= FakePropName;
  FFakeProp.Context:= Self;
  Result:= FFakeProp;
end;


function gP3DPropAccessStreamableList.IndexOf(AItem: Pointer): Integer;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.IndexOf( T( AItem ))
  else
    Result:= -1;
end;

function gP3DPropAccessStreamableList.GetChildPropByPath(var APath: String; out
  RestString: String; out LastFound: TP3DPropertyAccess): TP3DPropertyAccess;
var
  ArrIdx, Prop: String;
  tmpStr: String;
  i: LongInt;
begin
  RestString:= APath;

  Prop:= PathGetNextFragment( RestString, ArrIdx );

  if ( Prop = '' ) then
    begin
      i:= GetArrayIdxByString( ArrIdx );
      if ( i > -1 ) then
        begin
          Result:= GetArrayItemAsProp( i );
          tmpStr:= RestString;
          if ( RestString > '' ) then
            Result:= Result.GetChildPropByPath( tmpStr, RestString, LastFound )
          else
            LastFound:= Result;
        end
      else
        raise Exception.CreateFmt( 'Error in "%s.GetChildPropByPath". Invalid array index "%s".', [ ClassName, ArrIdx ]);
    end
  else
    raise Exception.CreateFmt( 'Error in "%s.GetChildPropByPath". The object does not have a property named "%s".', [ ClassName, Prop ]);


  if ( not Assigned( LastFound )) then
    LastFound:= Self;
end;

function gP3DNamedStreamableList.FindUniqueName( BaseStr: String ): String;
var
  I: Integer;
begin
  I:= 0;
  repeat
    Inc( I );
    Result:= BaseStr + IntToStr( I );
  until ( FindByName( Result ) = -1 );
end;

function gP3DNamedStreamableList.FindByName(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if (( Assigned( Items[ i ])) and // Items[ i ] can be nil for example if many values are loaded from JSON, the count is set to the final count before it loads all the values.
        ( Items[ i ].Name = AName )) then
      begin
        Result:= i;
        break;
      end;
end;

function gP3DNamedStreamableList.Add(Item: TCustomItem): Integer;
begin
  if ( not Assigned( Item )) then
    raise Exception.Create( 'gP3DNamedStreamableList.Add: Cannot add empty item to list.' )
  else if ( FindByName( Item.Name ) < 0 ) then
    Result:= inherited Add(Item)
  else
    raise Exception.CreateFmt( 'gP3DNamedStreamableList.Add: Cannot add "%s". An item with that name already exists!', [ Item.Name ]);
end;

{ gP3DPropAccessNamedStreamableList }

function gP3DPropAccessNamedStreamableList.GetArrayItemAsProp( Idx: Integer ): TP3DPropertyAccess;
begin
  Result:= inherited GetArrayItemAsProp( Idx );
  if ( Assigned( TItemProp( Result ).AsValue )) then
    Result.Name:= Name + '[ "' + TItemProp( Result ).AsValue.Name + '" ]'
  else
    Result.Name:= Name + '[ "" ]';
end;

function gP3DPropAccessNamedStreamableList.GetArrayIdxByString( Idx: String ): Integer;
begin
  if ( Enclosed( Idx, '"', '"' )) then
    Result:= FindByName( Idx )
  else
    Result:= inherited GetArrayIdxByString( Idx );
end;

function gP3DPropAccessNamedStreamableList.FindUniqueName( BaseStr: String ): String;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.FindUniqueName( BaseStr )
  else
    Result:= BaseStr;
end;

function gP3DPropAccessNamedStreamableList.FindByName(AName: String): Integer;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.FindByName( AName )
  else
    Result:= -1;
end;

{ TP3DNamedStreamable }

constructor TP3DNamedStreamable.Create(const AOwner: IP3DStreamableContainer;
  const ARoot: IP3DLoadJSON);
var
  BaseName: String;
begin
  inherited Create( AOwner, ARoot );
  Properties.Add( TP3DPropAccessString.CreateFieldSet( 'Name', @FName, @SetName, smText ));
  if ( ClassName[ 1 ] = 'T' ) then
    BaseName:= Copy( ClassName, 2, Length( ClassName ) - 1 )
  else
    BaseName:= ClassName;

  if ( Owner is IP3DNamedStreamableList ) then
    FName:= ( Owner as IP3DNamedStreamableList ).FindUniqueName( BaseName )
  else
    FName:= BaseName;
end;

procedure TP3DNamedStreamable.SetName(AValue: String);
begin
  if FName=AValue then Exit;

  if (( Owner is IP3DNamedStreamableList ) and (( Owner as IP3DNamedStreamableList ).FindByName( AValue ) > -1 )) then
    raise Exception.CreateFmt( 'TP3DNamedStreamable.SetName: A value with the name "%s" already exists in the same container!', [ AValue ])
  else
    FName:= AValue;
end;

{$ENDIF}

