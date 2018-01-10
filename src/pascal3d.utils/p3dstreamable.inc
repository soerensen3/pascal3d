//p3dstreamable.inc
//..........................................................................

{$IFDEF INTERFACE}

  { IP3DStreamableContainer }

  IP3DStreamableContainer = interface ( IP3DPropAccessPath )
    ['{ECFD6B13-A54F-4DF3-97CA-F00057B79525}']

    procedure NotifyOnDelete( AItem: TP3DStreamable );
    function NotifyOnAdd( AItem: TP3DStreamable ): Boolean;
    function GetContainer: TP3DPropertyAccess;
  end;

  { IP3DPropAccessStreamable }

  IP3DPropAccessStreamable = interface ( IP3DBaseInterface ) ['{CC1223F5-BC4C-4ED2-9406-239D4264617C}']
    function GetValueAsStreamable: TP3DStreamable;
    procedure SetValueAsStreamable( AValue: TP3DStreamable );
  end;

  { IP3DStreamableUser }

  IP3DStreamableUser = interface ( IP3DPropAccessStreamable ) ['{12C827E2-C8EB-4386-AC66-ABB380F158C3}']
    procedure UpdateUser( Old, New: TP3DStreamable );
    function GetUser: TP3DStreamable;
  end;

  { TP3DUserList }

  TP3DUserList = class ( specialize gP3DCustomList < IP3DStreamableUser >)
    procedure UpdateAllUsers( Old, New: TP3DStreamable );
  end;

  IP3DStreamableList = interface ( IP3DBaseInterface ) ['{4AECAA82-A758-48EF-AC2B-0384FC91CDE9}']
    function Add: TP3DPropertyAccess;
  end;

  { gP3DStreamableListBase }

  generic gP3DStreamableListBase < T; TItemProp{: TP3DPropertyAccess } > = class( TP3DPropertyAccessArray, IP3DStreamableList )
    public type
      TStreamableList = specialize TFPGList < TItemProp >;
      TStreamableNotifyEvent = procedure ( ASender: TObject; AItem: T; Action: TP3DListOnChangeAction ) of object;
      TStreamableListEnumerator = specialize gListEnumerator < T >;

    protected
      FItems: TStreamableList;
      FOnChange: TStreamableNotifyEvent;

      function GetValues( Index: Integer ): T;
      procedure SetValues( Index: Integer ; AValue: T);
      function GetAsJSON: TJSONData; override;
      function GetAsString: String; override;
      function GetCount: Integer; override;
      procedure LoadJSON(AValue: TJSONData); override;
      procedure SetAsString(AValue: String); override;
      procedure SetCount(AValue: Integer); override;
      function GetItems( Index: Integer ): TItemProp;
      procedure LoadItemFromJSON( Index: Integer; AData: TJSONData ); virtual;

      procedure Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction); virtual;
      function MoveNext( var AIndex: Integer; out AItem: T ): Boolean;
      function GetStrIdxOf( AIndex: Integer ): String;

    strict private
      function Add: TP3DPropertyAccess; virtual;

    private
      function Add( Item: T ): Integer; virtual;
      procedure Remove( AItem: T ); virtual;
      procedure Delete( Index: Integer ); virtual;
      procedure Insert( Index: Integer; Item: T ); virtual;

    public
      constructor Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
      destructor Destroy; override;

      function GetFullPath: String; override;
      function GetEnumerator: TStreamableListEnumerator;
      function SaveToJSON: TJSONData; override;

      function Find( Item: T ): Integer; virtual;
      procedure Clear;

      function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;
      function IndexOf(AItem: Pointer): Integer; override;
      function GetChildPropByPath(var APath: String; out RestString: String; out LastFound: TP3DPropertyAccess ): TP3DPropertyAccess; override;

      //property Items: TStreamableList read FItems;
      property Items[ Index: Integer ]: TItemProp read GetItems;
      property AsValues[ Index: Integer ]: T read GetValues write SetValues; default;
      property OnChange: TStreamableNotifyEvent read FOnChange write FOnChange;
  end;

  generic gP3DStreamableList < T: TP3DStreamable; TItemProp > = class ( specialize gP3DStreamableListBase < T, TItemProp >)
    public
      //function Add: TP3DPropertyAccess; override;
      function Add( Item: T ): Integer; override;
      procedure Remove( AItem: T ); override;
      procedure Delete( Index: Integer ); override;
      procedure Insert( Index: Integer; Item: T ); override;
  end;

  //TP3DStreamableList = specialize gP3DStreamableListBase < TP3DStreamable >;

  { gP3DStreamableContainerList }

  generic gP3DStreamableContainerList < T: TP3DStreamable; TItemProp > = class ( specialize gP3DStreamableListBase < T, TItemProp >, IP3DStreamableContainer )
    protected
      procedure NotifyOnDelete( AItem: TP3DStreamable );
      function NotifyOnAdd( AItem: TP3DStreamable ): Boolean;
      function GetContainer: TP3DPropertyAccess;

    public
      constructor Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
  end;

  { gP3DStreamableListBase }

  generic gP3DStreamableContainer < T : TP3DStreamable > = class ( TP3DPropertyAccess, IP3DPropAccessStreamable, IP3DStreamableContainer )
    {public type
      TStreamableContainer = specialize gP3DStreamableContainer < T >;
      TStreamableContainerList = specialize gP3DStreamableContainerList < T, TStreamableContainer >;}

    protected
      FAsValue: T;

      function GetAsJSON: TJSONData; override;
      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      procedure SetAsValue(AValue: T);
      procedure LoadJSON(AValue: TJSONData); override;
      procedure NotifyOnDelete( AItem: TP3DStreamable );
      function NotifyOnAdd( AItem: TP3DStreamable ): Boolean;
      function GetContainer: TP3DPropertyAccess;

    public
      constructor Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
      destructor Destroy; override;

      function SaveToJSON: TJSONData; override;

      function CreateObjectFromJSON( AJSON: TJSONData ): T;
      function GetValueAsStreamable: TP3DStreamable;
      procedure SetValueAsStreamable(AValue: TP3DStreamable);
      function GetChildPropByPath(var APath: String; out RestString: String; out LastFound: TP3DPropertyAccess ): TP3DPropertyAccess; override;

      property AsValue: T read FAsValue write SetAsValue;
  end;

  { gP3DPropAccessStreamable }

  generic gP3DPropAccessStreamable < T: TP3DStreamable > = class ( TP3DPropertyAccess, IP3DPropAccessStreamable, IP3DStreamableUser )
    type
      TGetter = function: T of object;
      TSetter = procedure ( AValue: T ) of object;

    protected
      FAsValue: T;
      FGetter: TGetter;
      FSetter: TSetter;

      function GetAsValue: T;
      function GetAsJSON: TJSONData; override;
      function GetValueAsStreamable: TP3DStreamable;
      procedure SetValueAsStreamable( AValue: TP3DStreamable );
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      procedure SetAsValue(AValue: T);
      function GetUser: TP3DStreamable;
      procedure UpdateUser(Old, New: TP3DStreamable);
      procedure LoadJSON(AValue: TJSONData); override;

      class function LoadDeferred: Boolean; override;

      {%H-}constructor CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore);

    public
      destructor Destroy; override;

      function SaveToJSON: TJSONData; override;
      procedure Assign(AProp: TP3DPropertyAccess); override;

      function GetChildPropByPath( var APath: String; out RestString: String;
       out LastFound: TP3DPropertyAccess): TP3DPropertyAccess; override;

      property AsValue: T read GetAsValue write SetAsValue;
  end;

  //TP3DPropAccessStreamable = specialize gP3DPropAccessStreamable < TP3DStreamable >;

  { gP3DPropAccessStreamablePointer }

  { gP3DStreamablePointer }

  generic gP3DStreamablePointer < T: TP3DStreamable > = class ( specialize gP3DPropAccessStreamable < T > )
    {public type
      TStreamablePointer = specialize gP3DPropAccessStreamablePointer < T >;
      TStreamablePointerList = specialize gP3DStreamableListBase < T, TStreamablePointer >;}
    protected
      function GetAsJSON: TJSONData; override;
      procedure LoadJSON( AValue: TJSONData ); override;
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;

    public
      function SaveToJSON: TJSONData; override;
  end;

  IP3DNamedStreamable = interface ( IP3DBaseInterface )
    ['{5915B652-4BB3-4ACA-B5CA-FD596E51E2B6}']
    function GetName: String;
    procedure SetName( AValue: String );
  end;

  { TP3DStreamable }

  TP3DStreamable = class ( TP3DInterfacedObject, IP3DPropAccessPath )
    protected
      FOwner: IP3DStreamableContainer;
      FProperties: TP3DPropertyList;
      FRoot: IP3DJSONStream;
      FUsers: TP3DUserList;

      function GetJSON: TJSONData; virtual;
      function GetRoot: IP3DJSONStream;
      procedure SetJSON(AValue: TJSONData); virtual;
      procedure SetOwner(AValue: IP3DStreamableContainer);

    public
      constructor Create( const AOwner: IP3DStreamableContainer = nil ); virtual;
      destructor Destroy; override;

      function GetFullPath: String;

      property Properties: TP3DPropertyList read FProperties;
      property Owner: IP3DStreamableContainer read FOwner write SetOwner;
      property Root: IP3DJSONStream read GetRoot write FRoot;
      property AsJSON: TJSONData read GetJSON write SetJSON;
      property Users: TP3DUserList read FUsers;
  end;

  TP3DPropAccessStreamableContainer = specialize gP3DStreamableContainer < TP3DStreamable >;
  TP3DStreamableContainerList = specialize gP3DStreamableContainerList < TP3DStreamable, TP3DPropAccessStreamableContainer >;

  { TP3DNamedStreamable }

  TP3DNamedStreamable = class ( TP3DStreamable, IP3DNamedStreamable )
    private
      FName: String;

      function GetName: String;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      procedure SetName( AValue: String ); virtual;

    published
      property Name: String read FName write SetName;
  end;

  IP3DNamedStreamableList = interface ( IP3DPropAccessPath )
    ['{DD922AD9-983B-45D4-8C8E-A9012DE67E89}']
    function FindUniqueName( BaseStr: String ): String;
    function FindByName( AName: String ): Integer;
  end;

  { gP3DNamedStreamableList }

  generic gP3DNamedStreamableList < T: TP3DNamedStreamable; TItemProp > = class ( specialize gP3DStreamableListBase < T, TItemProp >)
    public
      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
      procedure Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction); override;
      //function Add( Item: T ): Integer; override;
  end;

  { gP3DNamedStreamableContainerList }

  generic gP3DNamedStreamableContainerList < T: TP3DNamedStreamable;
                                        TItemProp{: TP3DPropertyAccess } >
         = class( specialize gP3DStreamableContainerList < T, TItemProp >, IP3DNamedStreamableList )
    protected
      procedure LoadItemFromJSON(Index: Integer; AData: TJSONData); override;
      function GetStrIdxOf(AIndex: Integer): String;

    public
      function GetArrayIdxByString(Idx: String): Integer; override;

      function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;

      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
  end;
  {
  generic gP3DNamedStreamableContainer < T: TP3DNamedStreamable > = class( specialize gP3DStreamableContainer < T >, IP3DPropAccessStreamable, IP3DStreamableContainer )
    public type
      TNamedStreamableContainer = specialize gP3DNamedStreamableContainer < T >;
      TNamedStreamableContainerList = specialize gP3DNamedStreamableContainerList < T, TNamedStreamableContainer >;
  end;

  generic gP3DPropAccessNamedStreamablePointer < T: TP3DNamedStreamable > = class ( specialize gP3DPropAccessStreamablePointer < T > )
    public type
      TNamedStreamablePointer = specialize gP3DPropAccessNamedStreamablePointer < T >;
      TNamedStreamablePointerList = specialize gP3DNamedStreamableList < T, TNamedStreamablePointer >;
  end;
  }


  TP3DPropAccessStreamablePointer = specialize gP3DStreamablePointer < TP3DStreamable >;
  TP3DStreamablePointerList = specialize gP3DStreamableListBase < TP3DStreamable, TP3DPropAccessStreamablePointer >;

//var
// DebugList: TP3DStreamableList;
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

{ TP3DStreamable }

procedure TP3DStreamable.SetOwner(AValue: IP3DStreamableContainer);
begin
  FOwner:= AValue;

  if ( Self is IP3DJSONStream ) then
    FRoot:= Self as IP3DJSONStream
  else if ( Assigned( Owner )) then
    FRoot:= Owner.GetRoot
  else
    FRoot:= nil;
end;

function TP3DStreamable.GetJSON: TJSONData;
begin
  Result:= Properties.AsJSON;
  ( Result as TJSONObject ).Add( 'ClassName', ClassName );
  ( Result as TJSONObject ).Add( 'Pointer', hexStr( Self ));
end;

function TP3DStreamable.GetRoot: IP3DJSONStream;
begin
  Result:= FRoot;
end;

procedure TP3DStreamable.SetJSON(AValue: TJSONData);
begin
  Properties.AsJSON:= AValue;
end;

constructor TP3DStreamable.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create;
  FUsers:= TP3DUserList.Create;

  if ( Assigned( AOwner ) and ( not AOwner.NotifyOnAdd( Self ))) then
    raise Exception.Create( 'Illegal owner for class ' + ClassName );
  WriteLn( 'Owner assigned: ', Assigned( AOwner ));
  Owner:= AOwner; //will also set root

  if ( not Assigned( FRoot )) then
    raise Exception.Create( ClassName + '.Create: Root for streamable is not assigned. Either the owner or the class itself has to implement IP3DJSONStream!' );

  FProperties:= TP3DPropertyList.Create( Self );

  //DebugList.Add( Self );
end;

destructor TP3DStreamable.Destroy;
begin
  Users.UpdateAllUsers( Self, nil );

  if ( Assigned( Owner )) then
    Owner.NotifyOnDelete( Self );

  //DebugList.Remove( Self );
  FProperties.Free;
  FUsers.Free;
  inherited Destroy;
end;

function TP3DStreamable.GetFullPath: String;
var
  Prop: TP3DPropertyAccess;
  PropArray: IP3DPropAccessPathArray;
  n: Integer;
  obj: IP3DJSONStream;
begin
  if ( Self.GetInterface( IP3DJSONStream, obj ) and ( obj = Root )) then
    Exit( '' );

  if ( Assigned( Owner )) then
    begin
      {if ( Owner.GetInterface( IP3DPropAccessPathArray, PropArray )) then
        begin
          n:= PropArray.IndexOf( Self );
          if ( n < 0 ) then
            raise Exception.Create( 'Internal error: The streamable could not be found in the owner!' + ifthen( Owner.GetInstance is TP3DPropertyAccess, ' [' + TP3DPropertyAccess( Owner.GetInstance ).GetFullPath + ']' ));
          Prop:= PropArray.GetArrayItemAsProp( n );
          if ( Assigned( Prop )) then
            Result:= Prop.GetFullPath
          else
            Result:= '';
        end
      else}
        Result:= Owner.GetFullPath
    end
  else
    Result:= '';
end;

procedure gP3DPropAccessStreamable.Assign(AProp: TP3DPropertyAccess);
var
  S: TP3DStreamable;
begin
  if ( AProp is IP3DPropAccessStreamable ) then
    begin
      S:= ( AProp as IP3DPropAccessStreamable ).GetValueAsStreamable;
      if ( S is T ) then
        AsValue:= T( S )
      else if ( not Assigned( S )) then
        AsValue:= nil;
    end
  else
    inherited Assign( AProp );
end;

function gP3DPropAccessStreamable.GetAsString: String;
begin
  Result:= T.ClassName;
end;

procedure gP3DPropAccessStreamable.SetAsString(AValue: String);
begin
  raise Exception.Create( 'Error: Cannot set streamable as string!' );
end;

procedure gP3DPropAccessStreamable.SetAsValue(AValue: T);
var
  Obj: T;
begin
  Obj:= AsValue;
  if ( Obj = AValue ) then
    exit;

  if ( Assigned( Obj )) then
    Obj.Users.Remove( Self );

  if ( Assigned( FSetter )) then
    FSetter( AValue )
  else
    FAsValue:= AValue;

  if ( Assigned( AValue )) then
    AValue.Users.Add( Self );
end;

function gP3DPropAccessStreamable.GetUser: TP3DStreamable;
begin
  Result:= AsValue;
end;

procedure gP3DPropAccessStreamable.UpdateUser(Old, New: TP3DStreamable);
begin
  if ( AsValue = Old ) then
    if ( New is T ) then
      FAsValue:= T( New );
end;

procedure gP3DPropAccessStreamable.LoadJSON(AValue: TJSONData);
begin
  if ( Assigned( AsValue ) and ( AValue.JSONType = jtObject )) then
    AsValue.AsJSON:= AValue
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtObject', JSONTypeName( AValue.JSONType )]);
end;

class function gP3DPropAccessStreamable.LoadDeferred: Boolean;
begin
  Result:= True;
end;

constructor gP3DPropAccessStreamable.CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter;
  const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FGetter:= AGetter;
  FSetter:= ASetter;
end;

destructor gP3DPropAccessStreamable.Destroy;
begin
  SetAsValue( nil );
  inherited Destroy;
end;

function gP3DPropAccessStreamable.SaveToJSON: TJSONData;
begin
  if ( StoreMode = smText ) then
    if ( Assigned( AsValue )) then
      Result:= AsValue.Properties.SaveToJSON
    else
      Result:= TJSONNull.Create
  else
    Result:= nil;
end;

function gP3DPropAccessStreamable.GetChildPropByPath(var APath: String; out
  RestString: String; out LastFound: TP3DPropertyAccess): TP3DPropertyAccess;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.Properties.GetPropByPath( APath, RestString, LastFound )
  else
    Result:= nil;
end;

function gP3DPropAccessStreamable.GetAsValue: T;
begin
  if ( Assigned( FGetter )) then
    Result:= FGetter()
  else
    Result:= FAsValue;
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


{ gP3DStreamableContainer }

procedure gP3DStreamableContainer.SetAsValue(AValue: T);
begin
  if ( FAsValue = AValue ) then
    Exit;

  if ( Assigned( FAsValue )) then
    begin
      FAsValue.Users.UpdateAllUsers( FAsValue, AValue );
      FAsValue.Free;
    end;

  FAsValue:= AValue;
  AValue.Owner:= Self;
end;

function gP3DStreamableContainer.GetAsJSON: TJSONData;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.AsJSON
  else
    Result:= TJSONNull.Create;
end;

function gP3DStreamableContainer.GetAsString: String;
begin
  Result:= T.ClassName;
end;

procedure gP3DStreamableContainer.SetAsString(AValue: String);
begin
  raise Exception.Create( 'Error: Cannot set streamable as string!' );
end;

procedure gP3DStreamableContainer.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtObject ) then
    CreateObjectFromJSON( AValue )
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtObject', JSONTypeName( AValue.JSONType )]);
end;

destructor gP3DStreamableContainer.Destroy;
begin
  if ( Assigned( AsValue )) then
    AsValue.Free;
  inherited Destroy;
end;

function gP3DStreamableContainer.SaveToJSON: TJSONData;
begin
  if ( StoreMode = smText ) then
    if ( Assigned( AsValue )) then
      Result:= AsValue.Properties.SaveToJSON
    else
      Result:= TJSONNull.Create
  else
    Result:= nil;
end;

function gP3DStreamableContainer.CreateObjectFromJSON(AJSON: TJSONData): T;
type
  TStreamableType = class of T;
var
  Tp: TStreamableType;
  TName: TJSONStringType;
begin
  Tp:= T;
  if ( AJSON.JSONType = jtObject ) then
    begin
      TName:= TJSONObject( AJSON ).Get( 'ClassName', '' );
      if ( not Assigned( Root )) then
        begin
          WriteLn( GetFullPath + ': Root is not assigned.' );
          raise Exception.Create( GetFullPath + ': Root is not assigned.' );
        end;
      if ( not Assigned( Root.GetClassFactory )) then
        begin
          WriteLn( GetFullPath + ': Root is not assigned.' );
          raise Exception.Create( GetFullPath + ': Root.GetClassFactory is not assigned.' );
        end;
      if ( TName > '' ) then
        Tp:= TStreamableType( Root.GetClassFactory.FindByName( TName ));
      if ( not Assigned( Tp )) then
        raise Exception.CreateFmt( 'The class "%s" could not be found in the class factory!', [ TName ]);
    end;
  Result:= Tp.Create( Self );
  Result.AsJSON:= AJSON;
  AsValue:= Result;
end;

function gP3DStreamableContainer.GetValueAsStreamable: TP3DStreamable;
begin
  Result:= AsValue;
end;

procedure gP3DStreamableContainer.SetValueAsStreamable(AValue: TP3DStreamable);
begin
  AsValue:= T( AValue );
end;

function gP3DStreamableContainer.GetChildPropByPath(var APath: String; out RestString: String; out
  LastFound: TP3DPropertyAccess): TP3DPropertyAccess;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.Properties.GetPropByPath( APath, RestString, LastFound )
  else
    Result:= nil;
end;

procedure gP3DStreamableContainer.NotifyOnDelete(AItem: TP3DStreamable);
begin
  FAsValue:= nil;
  //Do nothing. Only required because it needs to implement IP3DStreamableContainer
end;

function gP3DStreamableContainer.NotifyOnAdd(AItem: TP3DStreamable): Boolean;
begin
  Result:= AItem is T;
  if ( Result ) then
    AsValue:= T( AItem );
end;

function gP3DStreamableContainer.GetContainer: TP3DPropertyAccess;
begin
  Result:= Self;
end;

constructor gP3DStreamableContainer.Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FAsValue:= nil;
end;

procedure gP3DStreamablePointer.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtString ) then
    AsString:= AValue.AsString
  else if ( AValue.JSONType = jtNull ) then
    AsValue:= nil
  else
    raise Exception.Create( 'Cannot load from StreamablePointer from JSON because the context was no pointer!' );
end;

function gP3DStreamablePointer.GetAsString: String;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.GetFullPath
  else
    Result:= '<None>';
end;

procedure gP3DStreamablePointer.SetAsString(AValue: String);
begin
  if ( Assigned( Context ) and Assigned( Context.GetRoot )) then
    Assign( Context.GetRoot.GetJSONLoader().GetPropByPath( AValue, True ));
end;

function gP3DStreamablePointer.SaveToJSON: TJSONData;
begin
  Result:= GetAsJSON;
end;

function gP3DStreamablePointer.GetAsJSON: TJSONData;
begin
  if ( Assigned( AsValue )) then
    Result:= TJSONString.Create( AsValue.GetFullPath )
  else
    Result:= TJSONNull.Create;
end;

{ gP3DStreamableListBase }

function gP3DStreamableListBase.GetChildPropByPath(var APath: String; out
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


function gP3DStreamableListBase.GetAsJSON: TJSONData;
var
  a: TJSONArray;
  i: Integer;
begin
  a:= TJSONArray.Create();
  for i:= 0 to Count - 1 do
    a.Add( GetArrayItemAsProp( i ).AsJSON );
  Result:= a;
end;

function gP3DStreamableListBase.GetAsString: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to Count - 1 do
    Result+= ', ' + GetArrayItemAsProp( i ).AsString;
  Result:= '[' + Copy( Result, 3, Length( Result ) - 2 ) + ']';
end;

function gP3DStreamableListBase.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure gP3DStreamableListBase.LoadJSON(AValue: TJSONData);
var
  i: Integer;
begin
  if ( AValue.JSONType = jtArray ) then
    begin
      Count:= TJSONArray( AValue ).Count;
      for i:= 0 to TJSONArray( AValue ).Count - 1 do
        LoadItemFromJSON( i, TJSONArray( AValue )[ i ]);
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtArray', JSONTypeName( AValue.JSONType )]);
end;

procedure gP3DStreamableListBase.SetAsString(AValue: String);
var
  SItems: array of String;
  i: Integer;
begin
  if ( Enclosed( AValue, '[', ']' )) then
    begin
      SItems:= AValue.Split( ',' );
      Count:= Length( SItems );
      for i:= low( SItems ) to high( SItems ) do
        GetArrayItemAsProp( i ).AsString:= SItems[ i ];
    end
  else
    Exception.Create( 'Cannot set array as string as no array brackets where found in string: ' + AValue );
end;

procedure gP3DStreamableListBase.SetCount(AValue: Integer);
begin
  FItems.Count:= AValue;
end;

function gP3DStreamableListBase.GetEnumerator: TStreamableListEnumerator;
begin
  Result:= TStreamableListEnumerator.Create( -1, @MoveNext );
end;

function gP3DStreamableListBase.SaveToJSON: TJSONData;
var
  a: TJSONArray;
  i: Integer;
  json: TJSONData;
begin
  a:= TJSONArray.Create();
  for i:= 0 to Count - 1 do
    begin
      json:= GetArrayItemAsProp( i ).SaveToJSON;
      if ( Assigned( json )) then
        a.Add( GetArrayItemAsProp( i ).SaveToJSON );
    end;
  Result:= a;
end;

function gP3DStreamableListBase.Find(Item: T): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( TItemProp( GetArrayItemAsProp( i )).AsValue = Item ) then
      begin
        Result:= i;
        break;
      end;
end;

function gP3DStreamableListBase.Add(Item: T): Integer;
var
  Prop: TItemProp;
begin
  if ( Find( Item ) > -1 ) then
    exit;
  Notify( Self, Item, actAdd );
  Prop:= TItemProp.Create( '' );
  Prop.Context:= Self;
  Prop.Root:= Root;

  Result:= FItems.Add( Prop );
  Prop.AsValue:= Item;
  Prop.Name:= Name + '[' + GetStrIdxOf( Result ) + ']';
end;

function gP3DStreamableListBase.Add: TP3DPropertyAccess;
var
  n: Integer;
begin
  Notify( Self, nil, actAdd );
  Result:= TItemProp.Create( '', smText );
  Result.Context:= Self;
  Result.Root:= Root;

  n:= FItems.Add( TItemProp( Result ));
  Result.Name:= Name + '[' + GetStrIdxOf( n ) + ']';
  P3DLog.LogInfo( Self, 'gP3DStreamableListBase.Add: TP3DPropertyAccess - ' + Result.Name );
end;

procedure gP3DStreamableListBase.Remove(AItem: T);
var
  n: Integer;
  Prop: TItemProp;
begin
  Notify( Self, AItem, actDelete );
  n:= Find( AItem );
  if ( n > 0 ) then
    begin
      Prop:= FItems[ n ];
      Prop.Free;
      FItems.Delete( n );
    end;
end;

procedure gP3DStreamableListBase.Delete(Index: Integer);
var
  Prop: TItemProp;
begin
  Notify( Self, AsValues[ Index ], actDelete );
  Prop:= FItems[ Index ];
  Prop.Free;
  FItems.Delete( Index );
end;

procedure gP3DStreamableListBase.Clear;
begin
  Notify( Self, nil, actClear );
  FItems.Clear;
end;

procedure gP3DStreamableListBase.Insert(Index: Integer; Item: T);
var
  Prop: TItemProp;
begin
  if ( Find( Item ) > -1 ) then
    exit;
  Notify( Self, Item, actAdd );
  Prop:= TItemProp.Create( Name + '[' + GetStrIdxOf( Index ) + ']' );
  Prop.Context:= Self;
  Prop.Root:= Root;

  Prop.AsValue:= Item;
  FItems.Insert( Index, Prop );
  P3DLog.LogInfo( Self, 'Insert(Index: Integer; Item: T) - ' + Prop.Name );
end;

procedure gP3DStreamableListBase.Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction);
begin
  if ( Assigned( OnChange )) then
    OnChange( ASender, AItem, AAction );
end;

function gP3DStreamableListBase.MoveNext(var AIndex: Integer; out AItem: T): Boolean;
begin
  Inc( AIndex );
  Result:= AIndex < Count;
  if ( Result ) then
    AItem:= AsValues[ AIndex ]
  else
    AItem:= default( T );
end;

function gP3DStreamableListBase.GetStrIdxOf(AIndex: Integer): String;
begin
  Result:= IntToStr( AIndex );
end;

function gP3DStreamableListBase.GetItems(Index: Integer): TItemProp;
begin
  Result:= FItems[ Index ];
end;

function gP3DStreamableListBase.GetFullPath: String;
var
  obj: IP3DJSONStream;
begin
  if ( Self.GetInterface( IP3DJSONStream, obj ) and ( obj = Root )) then
    Exit( '' );

  Result:= Context.GetFullPath;
end;

function gP3DStreamableListBase.GetValues( Index: Integer ): T;
begin
  Result:= FItems[ Index ].AsValue;
end;

procedure gP3DStreamableListBase.SetValues( Index: Integer ; AValue: T);
begin
  Notify( Self, AsValues[ Index ], actDelete );
  if ( Find( AValue ) > -1 ) then
    Delete( Index )
  else
    begin
      FItems[ Index ].AsValue:= AValue;
      Notify( Self, AValue, actAdd );
      P3DLog.LogInfo( Self, 'SetItems( Index: Integer ; AValue: T) - ' + FItems[ Index ].Name );
    end;
end;

constructor gP3DStreamableListBase.Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FItems:= TStreamableList.Create;
end;

destructor gP3DStreamableListBase.Destroy;
begin
  FreeAndNil( FItems );
  inherited Destroy;
end;

procedure gP3DStreamableListBase.LoadItemFromJSON(Index: Integer; AData: TJSONData);
var
  Prop: TP3DPropertyAccess;
begin
  Prop:= GetArrayItemAsProp( Index );
  if ( Assigned( Prop )) then
    Prop.AsJSON:= AData;
end;


function gP3DStreamableListBase.GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess;
begin
  if ( not InRange( Idx, 0, Count - 1 )) then
    raise Exception.Create( 'Out of range!' );
  Result:= FItems[ Idx ];
  if ( not Assigned( Result )) then
    begin
      Result:= TItemProp.Create( Name + '[' + GetStrIdxOf( Idx ) + ']', smText );
      Result.Context:= Self;
      Result.Root:= Root;
      FItems[ Idx ]:= TItemProp( Result );
      P3DLog.LogInfo( Self, 'GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess - ' + Result.Name );
    end;
end;

function gP3DStreamableListBase.IndexOf(AItem: Pointer): Integer;
begin
  Result:= Find( T( AItem ));
end;

procedure gP3DStreamableContainerList.NotifyOnDelete(AItem: TP3DStreamable);
begin
  Remove( T( AItem ));
end;

function gP3DStreamableContainerList.NotifyOnAdd(AItem: TP3DStreamable): Boolean;
var
  n: Integer;
  Prop: TItemProp;
begin
  Result:= ( AItem is T );
  if ( Result ) then
    begin
      n:= Find( T( AItem ));
      if ( n < 0 ) then
        begin
          n:= Count;
          Count:= Count + 1;
          Prop:= TItemProp( GetArrayItemAsProp( n ));
          Prop.AsValue:= T( AItem );
        end
      else
        raise Exception.Create( 'Already added!' );
    end;
end;

function gP3DStreamableContainerList.GetContainer: TP3DPropertyAccess;
begin
  Result:= Self;
end;

constructor gP3DStreamableContainerList.Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  //FItems.OwnsObjects:= True;
end;

{ gP3DNamedStreamableList }

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

procedure gP3DNamedStreamableList.Notify(ASender: TObject; AItem: T; AAction: TP3DListOnChangeAction);
var
  n: Integer;
begin
  if ( AAction = actAdd ) then
    begin
      if ( not Assigned( AItem )) then
        raise Exception.Create( 'gP3DNamedStreamableList.Add: Cannot add empty item to list.' )
      else
        begin
          n:= FindByName( AItem.Name );
          if ( n < 0 ) then
            inherited Notify( ASender, AItem, AAction )
          else if ( AsValues[ n ] <> AItem ) then
            raise Exception.CreateFmt( 'gP3DNamedStreamableList.Add: Cannot add "%s". An item with that name already exists!', [ AItem.Name ]);
        end;
    end;
end;

{ gP3DNamedStreamableContainerList }

procedure gP3DNamedStreamableContainerList.LoadItemFromJSON(Index: Integer; AData: TJSONData);
var
  Prop: TP3DPropertyAccess;
begin
  Prop:= GetArrayItemAsProp( Index );
  if ( Assigned( Prop )) then
    begin
      if ( AData is TJSONObject ) then
        Prop.Name:= Name + '[ "' + TJSONObject( AData ).Get( 'Name', '' ) + '" ]';
      Prop.AsJSON:= AData;
      WriteLn( 'LoadItemFromJSON: ' + Prop.Name + ' = ' + AData.AsJSON );
    end;
end;

function gP3DNamedStreamableContainerList.GetStrIdxOf(AIndex: Integer): String;
var
  Item: T = nil;
  ItemProp: TItemProp;
begin
  ItemProp:= FItems[ AIndex ];
  if ( Assigned( ItemProp )) then
    Item:= ItemProp.AsValue;
  if ( Assigned( Item )) then
    Result:= '"' + Item.Name + '"'
  else
    Result:= inherited GetStrIdxOf( AIndex );
end;

function gP3DNamedStreamableContainerList.GetArrayIdxByString( Idx: String ): Integer;
begin
  if ( Enclosed( Idx, '"', '"' )) then
    Result:= FindByName( Idx )
  else
    Result:= inherited GetArrayIdxByString( Idx );
end;

function gP3DNamedStreamableContainerList.GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess;
begin
  if ( not InRange( Idx, 0, Count - 1 )) then
    raise Exception.Create( 'Out of range!' );
  Result:= FItems[ Idx ];
  if ( not Assigned( Result )) then
    begin
      Result:= TItemProp.Create( Name + '[' + GetStrIdxOf( Idx ) + ']', smText );
      Result.Context:= Self;
      Result.Root:= Root;
      FItems[ Idx ]:= TItemProp( Result );
    end
  else if ( Assigned( TItemProp( Result ).AsValue )) then
    Result.Name:= Name + '[ ' + GetStrIdxOf( Idx ) + ' ]';
end;

function gP3DNamedStreamableContainerList.FindUniqueName( BaseStr: String ): String;
var
  I: Integer;
begin
  I:= 0;
  repeat
    Inc( I );
    Result:= BaseStr + IntToStr( I );
  until ( FindByName( Result ) = -1 );
end;

function gP3DNamedStreamableContainerList.FindByName(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  WriteLn( ClassName + ': ', Count );
  for i:= 0 to Count - 1 do
    if ( Assigned( Items[ i ]) and // Items[ i ] can be nil for example if many values are loaded from JSON, the count is set to the final count before it loads all the values.
        ( Items[ i ].Name = AName )) then
      begin
        Result:= i;
        break;
      end;
end;

{ TP3DNamedStreamable }

function TP3DNamedStreamable.GetName: String;
begin
  Result:= FName;
end;

constructor TP3DNamedStreamable.Create(const AOwner: IP3DStreamableContainer);
var
  BaseName: String;
  StreamableList: IP3DNamedStreamableList;
begin
  inherited Create( AOwner );
  if ( ClassName[ 1 ] = 'T' ) then
    BaseName:= Copy( ClassName, 2, Length( ClassName ) - 1 )
  else
    BaseName:= ClassName;

  //if ( not Assigned( FOwner )) then
  //  raise Exception.Create( 'Cannot create NamedStreamable because the owner is not assigned!' );

  if ( Assigned( FOwner ) and FOwner.GetInterface( IP3DNamedStreamableList, StreamableList )) then
    FName:= StreamableList.FindUniqueName( BaseName )
  else
    FName:= BaseName;
  Properties.Add( TP3DPropAccessString.CreateFieldSet( 'Name', @FName, @SetName, smText ));
end;

procedure TP3DNamedStreamable.SetName(AValue: String);
var
  StreamableList: IP3DNamedStreamableList;
begin
  if FName=AValue then Exit;

  if (( Owner.GetInterface( IP3DNamedStreamableList, StreamableList ) and ( StreamableList.FindByName( AValue ) > -1 ))) then
    raise Exception.CreateFmt( 'TP3DNamedStreamable.SetName: A value with the name "%s" already exists in the same container!', [ AValue ])
  else
    FName:= AValue;
end;


{$ENDIF}

