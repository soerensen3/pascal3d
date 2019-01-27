//p3dstreamable.inc
//..........................................................................

{$IFDEF INTERFACE}

  { IP3DStreamableContainer }

  IP3DStreamableContainer = interface ( IP3DPropAccessPath ) ['{ECFD6B13-A54F-4DF3-97CA-F00057B79525}']
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

  { gP3DStreamableListEnumerator }

  generic gP3DStreamableListEnumerator < T: TP3DStreamable > = class
    private type
      TMoveNext = function ( var AIndex: Integer; out AItem: TP3DStreamable ): Boolean of object;
      TEnumerator = specialize gP3DStreamableListEnumerator < T >;
    private
      FCurrent: T;
      FCurrentIdx: Integer;
      FMoveNext: TMoveNext;

    public
      constructor Create( AStartIndex: Integer; AMoveNext: TMoveNext );
      function MoveNext: Boolean;
      function GetEnumerator: TEnumerator;

      property Current: T read FCurrent;
      property CurrentIdx: Integer read FCurrentIdx;
  end;


  { TP3DStreamableListBase }

  TP3DStreamableListBase = class abstract( TP3DPropertyAccessArray, IP3DStreamableList )
    public type
      TStreamableList = TObjectList;
      TStreamableNotifyEvent = procedure ( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction ) of object;
      TStreamableListEnumerator = specialize gP3DStreamableListEnumerator < TP3DStreamable >;

    protected
      FItems: TStreamableList;
      FOnListChange: TStreamableNotifyEvent;

      function GetValues( Index: Integer ): TP3DStreamable;
      procedure SetValues( Index: Integer ; AValue: TP3DStreamable);
      function GetAsJSON: TJSONData; override;
      function GetAsString: String; override;
      function GetCount: Integer; override;
      procedure LoadJSON(AValue: TJSONData); override;
      procedure SetAsString(AValue: String); override;
      procedure SetCount(AValue: Integer); override;
      function GetItems( Index: Integer ): TP3DPropertyAccess;
      procedure LoadItemFromJSON( Index: Integer; AData: TJSONData ); virtual;

      procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); virtual;
      procedure NotifyAfter(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); virtual;
      function MoveNext( var AIndex: Integer; out AItem: TP3DStreamable ): Boolean;
      function MoveLast( var AIndex: Integer; out AItem: TP3DStreamable ): Boolean;
      function GetStrIdxOf( AIndex: Integer ): String;

    strict private
      FAfterChange: TStreamableNotifyEvent;
      function Add: TP3DPropertyAccess; virtual;

    protected
      FItemClass: TP3DStreamableClass;
      FItemProp: TP3DPropertyAccessClass;

      function Add( Item: TP3DStreamable ): Integer; virtual;
      procedure Remove( AItem: TP3DStreamable ); virtual;
      procedure Delete( Index: Integer ); virtual;
      procedure Insert( Index: Integer; Item: TP3DStreamable ); virtual;

      property Items[ Index: Integer ]: TP3DPropertyAccess read GetItems;
      property AsValues[ Index: Integer ]: TP3DStreamable read GetValues write SetValues; default;

    public
      constructor Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
      constructor Create(AName: String; AItemClass: TP3DStreamableClass; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); virtual; overload;
      destructor Destroy; override;

      function GetFullPath: String; override;
      function GetRelativePath: String; override;

      function GetEnumerator: TStreamableListEnumerator;
      function Reversed: TStreamableListEnumerator;
      function SaveToJSON: TJSONData; override;

      function Find( Item: TP3DStreamable ): Integer; virtual;
      procedure Clear; virtual;

      function First( const AClassType: TP3DStreamableClass = nil ): TP3DStreamable;
      function Last( const AClassType: TP3DStreamableClass = nil ): TP3DStreamable;

      function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;
      function IndexOf(AItem: Pointer): Integer; override;
      function GetChildPropByPath(var APath: String; out RestString: String; out LastFound: TP3DPropertyAccess ): TP3DPropertyAccess; override;

      property ItemProp: TP3DPropertyAccessClass read FItemProp;
      property ItemClass: TP3DStreamableClass read FItemClass;
      property OnChange: TStreamableNotifyEvent read FOnListChange write FOnListChange;
      property AfterChange: TStreamableNotifyEvent read FAfterChange write FAfterChange;
  end;

  TP3DStreamableList = class ( TP3DStreamableListBase )
    public
      //function Add: TP3DPropertyAccess; override;
      FAllowEmpties: Boolean;
      FAllowDuplicates: Boolean;

      procedure SetAllowDuplicates(AValue: Boolean);
      procedure SetAllowEmpties(AValue: Boolean);
      function Add( Item: TP3DStreamable ): Integer; override;
      procedure Remove( AItem: TP3DStreamable ); override;
      procedure Delete( Index: Integer ); override;
      procedure Insert( Index: Integer; Item: TP3DStreamable ); override;

      property AllowEmpties: Boolean read FAllowEmpties write SetAllowEmpties;
      property AllowDuplicates: Boolean read FAllowDuplicates write SetAllowDuplicates;
  end;

  { TP3DStreamableContainerList }

  TP3DStreamableContainerListBase = class ( TP3DStreamableListBase, IP3DStreamableContainer )
    protected
      procedure NotifyOnDelete( AItem: TP3DStreamable );
      function NotifyOnAdd( AItem: TP3DStreamable ): Boolean;
      function GetContainer: TP3DPropertyAccess;

    public
      constructor Create(AName: String; AItemClass: TP3DStreamableClass; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
      function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;
      property Items;
  end;

  { gP3DStreamableContainerList }

  generic gP3DStreamableContainerList < T: TP3DStreamable > = class ( TP3DStreamableContainerListBase )
    protected
      function GetAsValues( Index: Integer ): T;
      procedure SetAsValues( Index: Integer ; AValue: T);

    public type
      TTypedStreamableListEnumerator = specialize gP3DStreamableListEnumerator < T >;

    public
      constructor Create( AName: String; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore ); override;

      function First( const AClassType: TP3DStreamableClass = nil ): T;
      function Last( const AClassType: TP3DStreamableClass = nil ): T;

      function GetEnumerator: TTypedStreamableListEnumerator;
      function Reversed: TTypedStreamableListEnumerator;

      property AsValues[ Index: Integer ]: T read GetAsValues write SetAsValues; default;
  end;

  TP3DStreamableContainerList = specialize gP3DStreamableContainerList < TP3DStreamable >;


  { TP3DStreamableContainer }

  TP3DStreamableContainer = class ( TP3DPropertyAccess, IP3DPropAccessStreamable, IP3DStreamableContainer )
    public type
      TOnChange = procedure ( ANewValue: TP3DStreamable ) of object;

    public
      FItemClass: TP3DStreamableClass;

    protected
      FAsValue: TP3DStreamable;
      FAfterChange: TOnChange;
      FOnValueChange: TOnChange;

      function GetAsJSON: TJSONData; override;
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      procedure SetAsValue( AValue: TP3DStreamable );
      procedure LoadJSON( AValue: TJSONData ); override;
      procedure NotifyOnDelete( AItem: TP3DStreamable );
      function NotifyOnAdd( AItem: TP3DStreamable ): Boolean;
      function GetContainer: TP3DPropertyAccess;
      function ValueIsDefault: Boolean; override;

      {%H-}constructor Create( AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore ); override;

    public
      constructor Create(AName: String; AItemClass: TP3DStreamableClass; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); reintroduce;
      destructor Destroy; override;

      function SaveToJSON: TJSONData; override;

      function CreateObjectFromJSON( AJSON: TJSONData ): TP3DStreamable;
      function GetValueAsStreamable: TP3DStreamable;
      procedure SetValueAsStreamable(AValue: TP3DStreamable);
      function GetChildPropByPath(var APath: String; out RestString: String; out LastFound: TP3DPropertyAccess ): TP3DPropertyAccess; override;

      property AsValue: TP3DStreamable read FAsValue write SetAsValue;
      property ItemClass: TP3DStreamableClass read FItemClass;
      property OnChange: TOnChange read FOnValueChange write FOnValueChange;
      property AfterChange: TOnChange read FAfterChange write FAfterChange;
  end;

  { TP3DPropAccessStreamable }

  TP3DPropAccessStreamable = class ( TP3DPropertyAccess, IP3DPropAccessStreamable, IP3DStreamableUser )
    type
      TGetter = function: TP3DStreamable of object;
      TSetter = procedure ( AValue: TP3DStreamable ) of object;
      TOnChange = procedure ( ANewValue: TP3DStreamable ) of object;

    public
      FItemClass: TP3DStreamableClass;

    protected
      FAfterChange: TOnChange;
      FOnValueChange: TOnChange;
      FAsValue: TP3DStreamable;
      FGetter: TGetter;
      FSetter: TSetter;

      function GetAsValue: TP3DStreamable;
      function GetAsJSON: TJSONData; override;
      function GetValueAsStreamable: TP3DStreamable;
      procedure SetValueAsStreamable( AValue: TP3DStreamable );
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      procedure SetAsValue(AValue: TP3DStreamable);
      function GetUser: TP3DStreamable;
      procedure UpdateUser(Old, New: TP3DStreamable);
      procedure LoadJSON(AValue: TJSONData); override;

      class function LoadDeferred: Boolean; override;
      function ValueIsDefault: Boolean; override;

    public
      constructor Create(AName: String; AItemClass: TP3DStreamableClass; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); reintroduce;
      {%H-}constructor CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore);
      destructor Destroy; override;

      function SaveToJSON: TJSONData; override;
      procedure Assign(AProp: TP3DPropertyAccess); override;

      function GetFullPath: String; override;
      function GetChildPropByPath( var APath: String; out RestString: String;
       out LastFound: TP3DPropertyAccess): TP3DPropertyAccess; override;

      property AsValue: TP3DStreamable read GetAsValue write SetAsValue;
      property ItemClass: TP3DStreamableClass read FItemClass;
      property OnChange: TOnChange read FOnValueChange write FOnValueChange;
      property AfterChange: TOnChange read FAfterChange write FAfterChange;
  end;

  { TP3DStreamablePointer }

  TP3DStreamablePointer = class ( TP3DPropAccessStreamable )
    protected
      FPathAbsolute: Boolean;

      function GetAsJSON: TJSONData; override;
      procedure LoadJSON( AValue: TJSONData ); override;
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;

    public
      function SaveToJSON: TJSONData; override;

      property PathAbsolute: Boolean read FPathAbsolute write FPathAbsolute;
  end;

  { gP3DStreamablePointerList }

  generic gP3DStreamablePointerList < T: TP3DStreamable > = class ( TP3DStreamableList )
    protected
      function GetAsValues( Index: Integer ): T;
      procedure SetAsValues( Index: Integer ; AValue: T);

    public type
      TTypedStreamableListEnumerator = specialize gP3DStreamableListEnumerator < T >;

    public
      constructor Create( AName: String; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore ); override;

      function First( const AClassType: TP3DStreamableClass = nil ): T;
      function Last( const AClassType: TP3DStreamableClass = nil ): T;

      function GetEnumerator: TTypedStreamableListEnumerator;
      function Reversed: TTypedStreamableListEnumerator;
      function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;

      property AsValues[ Index: Integer ]: T read GetAsValues write SetAsValues; default;
  end;

  TP3DStreamablePointerList = specialize gP3DStreamablePointerList < TP3DStreamable >;

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
      FNeedsUpdate: Boolean;
      FLastUpdate: QWord;

      function GetNeedsUpdate: Boolean; virtual;
      procedure SetNeedsUpdate(AValue: Boolean); virtual;
      function GetJSON: TJSONData; virtual;
      function GetRoot: IP3DJSONStream;
      procedure SetJSON(AValue: TJSONData); virtual;
      procedure SetOwner(AValue: IP3DStreamableContainer);

    public
      constructor CreateFromJSON( AJSON: TJSONData; const AOwner: IP3DStreamableContainer = nil ); virtual;
      constructor Create( const AOwner: IP3DStreamableContainer = nil ); virtual;
      destructor Destroy; override;

      function GetFullPath: String; virtual;
      function GetRelativePath: String;
      procedure Update; virtual;

      function SaveToJSON: TJSONData;
      function GetAsString: String; virtual;

      property Properties: TP3DPropertyList read FProperties;
      property Owner: IP3DStreamableContainer read FOwner write SetOwner;
      property Root: IP3DJSONStream read GetRoot write FRoot;
      property AsJSON: TJSONData read GetJSON write SetJSON;
      property Users: TP3DUserList read FUsers;
      property NeedsUpdate: Boolean read GetNeedsUpdate write SetNeedsUpdate;
      property LastUpdate: QWord read FLastUpdate write FLastUpdate;
  end;

  IP3DNamedStreamableList = interface ( IP3DStreamableContainer ) ['{9B199AA9-5110-4555-94EF-42C3258D9EEC}']
    function FindUniqueName( BaseStr: String ): String;
    function FindByName( AName: String ): Integer;
  end;

  { TP3DNamedStreamable }

  TP3DNamedStreamable = class ( TP3DStreamable )
    protected
      FName: String;
      FNamedStreamableList: IP3DNamedStreamableList;

      function GetName: String; virtual;
      function GetAsString: String; override;
      procedure SetName( AValue: String ); virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

    published
      property Name: String read GetName write SetName;
  end;

  { gP3DNamedStreamableList }

  TP3DNamedStreamableList = class ( TP3DStreamableListBase )
    public
      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
      procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;
      //function Add( Item: T ): Integer; override;
  end;


  { gP3DNamedStreamableListEnumerator }

  generic gP3DNamedStreamableListEnumerator < T: TP3DStreamable > = class
    private type
      TMoveNext = function ( var AIndex: Integer; out AItem: TP3DNamedStreamable; AFilter: String ): Boolean of object;
      TEnumerator = specialize gP3DNamedStreamableListEnumerator < T >;
    private
      FCurrent: T;
      FCurrentIdx: Integer;
      FFilter: String;
      FMoveNext: TMoveNext;

    public
      constructor Create( AStartIndex: Integer; AMoveNext: TMoveNext; AFilter: String );
      function MoveNext: Boolean;
      function GetEnumerator: TEnumerator;

      property Current: T read FCurrent;
      property CurrentIdx: Integer read FCurrentIdx;
      property Filter: String read FFilter;
  end;

  { gP3DNamedStreamableContainerList }

  generic gP3DNamedStreamableContainerList < T: TP3DNamedStreamable > = class( specialize gP3DStreamableContainerList < T >, IP3DNamedStreamableList )
    public
      type TNamedStreamableListEnumerator = specialize gP3DNamedStreamableListEnumerator < T >;

    protected
      procedure LoadItemFromJSON(Index: Integer; AData: TJSONData); override;
      function GetStrIdxOf(AIndex: Integer): String;

    public
      function GetArrayIdxByString(Idx: String): Integer; override;

      function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;

      function FindUniqueName( BaseStr: String ): String;
      function FindByName( AName: String ): Integer;
      function GetByName( AName: String ): T;

      function EnumerateWildcard( AWildCard: String ): TNamedStreamableListEnumerator;
      function MoveNextFilter( var AIndex: Integer; out AItem: TP3DNamedStreamable; AFilter: String ): Boolean;
  end;

  TP3DNamedStreamableContainerList = specialize gP3DNamedStreamableContainerList < TP3DNamedStreamable >;
  TP3DNamedStreamablePointerList = specialize gP3DStreamablePointerList < TP3DNamedStreamable >;


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
    if ( Assigned( Items[ i ])) then
      Items[ i ].UpdateUser( Old, New )
    else
      raise Exception.Create( 'Error: Empty user!' );
end;

{ TP3DStreamable }

procedure TP3DStreamable.SetOwner(AValue: IP3DStreamableContainer);
begin
  if ( Assigned( AValue )) then
    begin
      if ( FOwner = AValue ) then
        exit;

      //if (( Assigned( FOwner )) and ( FOwner.GetContainer is TP3DStreamableContainer )) then
      //  TP3DStreamableContainer( FOwner.GetContainer ).NotifyOnDelete( Self );

      if ( AValue.GetContainer is TP3DStreamableContainerListBase ) then
        begin
          TP3DStreamableContainerListBase( AValue.GetContainer ).Add( Self );
          exit;
        end
      else if ( not AValue.NotifyOnAdd( Self )) then
        raise Exception.CreateFmt( 'Illegal owner (%s) for class %s.', [ AValue.GetContainer.ClassName, ClassName ]);
    end;
  if ( Self is IP3DJSONStream ) then
    FRoot:= Self as IP3DJSONStream
  else if ( Assigned( AValue )) then
    FRoot:= AValue.GetRoot
  else
    raise Exception.CreateFmt( 'Owner for class %s can only be nil for classes that implement IP3DJSONStream!', [ ClassName ]);
  FOwner:= AValue;
end;

function TP3DStreamable.GetAsString: String;
begin
  Result:= ClassName;
end;

constructor TP3DStreamable.CreateFromJSON(AJSON: TJSONData; const AOwner: IP3DStreamableContainer);
begin
  Create( AOwner );
  AsJSON:= AJSON;
end;

function TP3DStreamable.GetNeedsUpdate: Boolean;
begin
  Result:= FNeedsUpdate;
end;

procedure TP3DStreamable.SetNeedsUpdate(AValue: Boolean);
begin
  //WriteLn( ClassName );
  FNeedsUpdate:= AValue;
end;

function TP3DStreamable.GetJSON: TJSONData;
begin
  Result:= Properties.AsJSON;
  ( Result as TJSONObject ).Add( 'ClassName', ClassName );
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

  Owner:= AOwner; //will also set root

  if ( not Assigned( FRoot )) then
    raise Exception.Create( ClassName + '.Create: Root for streamable is not assigned. Either the owner or the class itself has to implement IP3DJSONStream!' );

  FProperties:= TP3DPropertyList.Create( Self );

  //DebugList.Add( Self );
end;

destructor TP3DStreamable.Destroy;
var
  i: Integer;
begin
  Users.UpdateAllUsers( Self, nil );
  Users.Clear;
  //DebugList.Remove( Self );

  if ( Assigned( Owner )) then
    begin
      {$IFDEF VERBOSE}WriteLn( 'Notify the owner ', TP3DPropertyAccess( Owner.GetInstance ).Name );{$ENDIF}
      Owner.NotifyOnDelete( Self );
      FOwner:= nil;
    end;
  {$IFDEF VERBOSE}
  Write( 'Listing properties of ' );
  if ( Self is TP3DNamedStreamable ) then
    Write( TP3DNamedStreamable( Self ).Name, ': ' );
  WriteLn( ClassName );
  //Properties.Clear;
  for i:= Properties.Count - 1 downto 0 do
    begin
      if ( Assigned( Properties[ i ])) then
        begin WriteLn( '[', i, '] ', Properties[ i ].Name ); //WriteLn( ' = ', Properties[ i ].AsString );
        end
      else
        WriteLn( 'Warning: Empty prop' );
      Properties.Delete( i );
    end;
  {$ENDIF}
  FProperties.Free;
  FProperties:= nil;
  FreeAndNil( FUsers );
  inherited Destroy;
end;

function TP3DStreamable.GetFullPath: String;
begin
  if ( Assigned( Owner )) then
    Result:= Owner.GetFullPath
  else
    Result:= '';
end;

function TP3DStreamable.GetRelativePath: String;
begin
  if ( Assigned( Owner )) then
    Result:= Owner.GetRelativePath
  else
    Result:= '';
end;

procedure TP3DStreamable.Update;
begin
  LastUpdate:= GetTickCount64;
end;

function TP3DStreamable.SaveToJSON: TJSONData;
begin
  Result:= Properties.SaveToJSON;
  ( Result as TJSONObject ).Add( 'ClassName', ClassName );
end;

procedure TP3DPropAccessStreamable.Assign(AProp: TP3DPropertyAccess);
var
  S: TP3DStreamable;
  Prop: IP3DPropAccessStreamable;
begin
  if ( not Assigned( AProp )) then
    raise Exception.Create( 'Cannot assign null property to ' + ClassName );
  if ( AProp.GetInterface( IP3DPropAccessStreamable, Prop )) then
    begin
      S:= Prop.GetValueAsStreamable;
      if ( S.InheritsFrom( ItemClass )) then
        AsValue:= S
      else if ( not Assigned( S )) then
        AsValue:= nil
      else
        raise Exception.Create( 'Cannot assign this property to ' + ClassName + ' because it does not inherit from ' + ItemClass.ClassName );
    end
  else
    inherited Assign( AProp );
end;

function TP3DPropAccessStreamable.GetAsString: String;
begin
  if ( Assigned( FAsValue )) then
    Result:= FAsValue.GetAsString
  else
    Result:= '<None>: ' + ItemClass.ClassName;
end;

procedure TP3DPropAccessStreamable.SetAsString(AValue: String);
begin
  raise Exception.Create( 'Error: Cannot set streamable as string!' );
end;

procedure TP3DPropAccessStreamable.SetAsValue(AValue: TP3DStreamable);
var
  Obj: TP3DStreamable;
  Ct: TObject;
begin
  Obj:= AsValue;
  if ( Obj = AValue ) then
    exit;
  if (( Assigned( AValue )) and ( not AValue.InheritsFrom( ItemClass ))) then
    raise Exception.Create( 'Cannot set TP3DPropAccessStreamable.AsValue. The specified type ' + AValue.ClassName + ' does not inherit ' + ItemClass.ClassName );
  if ( Assigned( FOnValueChange )) then
    FOnValueChange( AValue );
  if ( Assigned( Obj )) then begin
    {$IFDEF VERBOSE}if ( Obj.frefcount < 0 ) then
      WriteLn( 'Invalid pointer in ' + GetFullPath )
    else begin
      try
        WriteLn( 'Deleting pointer in ' + GetFullPath );

      except
        On E: Exception do;
      end;{$ENDIF}
      Obj.Users.Remove( Self );
      {$IFDEF VERBOSE}end;{$ENDIF}
  end;

  if ( Assigned( FSetter )) then
    FSetter( AValue )
  else
    FAsValue:= AValue;

  if ( Assigned( FAfterChange )) then
    FAfterChange( AValue );
  if ( Assigned( FOnChange )) then
    FOnChange( Self );
  if ( Assigned( AValue )) then
    AValue.Users.Add( Self )
  else if ( Assigned( Context )) then
    begin
      Ct:= Context.GetInstance;
      if (( Ct is TP3DStreamableList ) and ( not TP3DStreamableList( Ct ).AllowEmpties )) then
        begin
          TP3DStreamableList( Ct ).Notify( Ct, AValue, actDelete );
          TP3DStreamableList( Ct ).FItems.Remove( Self );
          TP3DStreamableList( Ct ).NotifyAfter( Ct, AValue, actDelete );
        end;
    end;
end;

function TP3DPropAccessStreamable.GetUser: TP3DStreamable;
begin
  Result:= AsValue;
end;

procedure TP3DPropAccessStreamable.UpdateUser(Old, New: TP3DStreamable);
begin
  if (( AsValue = Old ) and ( Old <> New )) then
    begin {$IFDEF VERBOSE}WriteLn( 'updating user ' + Name + ' from ' + IntToHex( Int64( Old ), 8  ) + ' to ' + IntToHex( Int64( New ), 8 )); {$ENDIF}
      AsValue:= New;
      {Write( 'Updating user:', GetFullPath );
      if ( Old is TP3DNamedStreamable ) then
        Write( ' from: ', TP3DNamedStreamable( Old ).Name );
      if ( New is TP3DNamedStreamable ) then
        Write( ' to:', TP3DNamedStreamable( New ).Name );
      WriteLn();}
    end;
end;

procedure TP3DPropAccessStreamable.LoadJSON(AValue: TJSONData);
begin
  if ( Assigned( AsValue ) and ( AValue.JSONType = jtObject )) then
    AsValue.AsJSON:= AValue
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtObject', JSONTypeName( AValue.JSONType )]);
end;

function TP3DPropAccessStreamable.GetFullPath: String;
var
  obj: IP3DJSONStream;
begin
  if ( Assigned( AsValue ) and ( AsValue.GetInterface( IP3DJSONStream, obj )) and ( obj = Root )) then
    Result:= '.'
  else
    Result:= inherited GetFullPath;
end;

class function TP3DPropAccessStreamable.LoadDeferred: Boolean;
begin
  Result:= True;
end;

function TP3DPropAccessStreamable.ValueIsDefault: Boolean;
begin
  Result:= not Assigned( AsValue );
end;

constructor TP3DPropAccessStreamable.CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter;
  const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FGetter:= AGetter;
  FSetter:= ASetter;
end;

constructor TP3DPropAccessStreamable.Create(AName: String;
  AItemClass: TP3DStreamableClass; const AStoreMode: TP3DPropertyAccessStoreMode );
begin
  inherited Create( AName, AStoreMode );
  FItemClass:= AItemClass;
end;

destructor TP3DPropAccessStreamable.Destroy;
begin
  SetAsValue( nil );
  inherited Destroy;
end;

function TP3DPropAccessStreamable.SaveToJSON: TJSONData;
begin
  if ( StoreMode = smText ) then
    if ( Assigned( AsValue )) then
      Result:= AsValue.Properties.SaveToJSON
    else
      Result:= TJSONNull.Create
  else
    Result:= nil;
end;

function TP3DPropAccessStreamable.GetChildPropByPath(var APath: String; out
  RestString: String; out LastFound: TP3DPropertyAccess): TP3DPropertyAccess;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.Properties.GetPropByPath( APath, RestString, LastFound )
  else
    Result:= nil;
end;

function TP3DPropAccessStreamable.GetAsValue: TP3DStreamable;
begin
  if ( Assigned( FGetter )) then
    Result:= FGetter()
  else
    Result:= FAsValue;
end;

function TP3DPropAccessStreamable.GetAsJSON: TJSONData;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.AsJSON
  else
    Result:= TJSONNull.Create;
end;

function TP3DPropAccessStreamable.GetValueAsStreamable: TP3DStreamable;
begin
  Result:= AsValue;
end;

procedure TP3DPropAccessStreamable.SetValueAsStreamable(AValue: TP3DStreamable);
begin
  if ( AValue.InheritsFrom( ItemClass )) then
    AsValue:= AValue
  else
    raise Exception.CreateFmt( 'TP3DPropAccessStreamable.SetValueAsStreamable: The assigned value is no descendant of the base class "%s" required for the property of type "%s"', [ ItemClass.ClassName, ClassName ]);
end;


{ TP3DStreamableContainer }

procedure TP3DStreamableContainer.SetAsValue(AValue: TP3DStreamable);
var
  Obj: TP3DStreamable;
  Ct: TObject;
begin
  if ( FAsValue = AValue ) then
    Exit;

  if ( Assigned( FAsValue )) then
    begin
      FAsValue.Users.UpdateAllUsers( FAsValue, AValue );
      Obj:= FAsValue;
      Obj.FOwner:= nil;
      //Obj.FRoot:= nil;
      Obj.Free;
    end;

  if ( Assigned( FOnChange )) then
    FOnChange( AValue );

  FAsValue:= AValue;
  if ( Assigned( AValue )) then
    begin
      if ( Assigned( AValue.FOwner )) then
        AValue.FOwner.NotifyOnDelete( AValue ); // Shouldn't it be NotifyOnAdd?
      AValue.FOwner:= Self;
      AValue.Root:= Root;
    end;

  if ( Assigned( FAfterChange )) then
    FAfterChange( AValue );
end;

function TP3DStreamableContainer.GetAsJSON: TJSONData;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.AsJSON
  else
    Result:= TJSONNull.Create;
end;

function TP3DStreamableContainer.GetAsString: String;
begin
  Result:= ItemClass.ClassName;
end;

procedure TP3DStreamableContainer.SetAsString(AValue: String);
begin
  raise Exception.Create( 'Error: Cannot set streamable as string!' );
end;

procedure TP3DStreamableContainer.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtObject ) then
    CreateObjectFromJSON( AValue )
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtObject', JSONTypeName( AValue.JSONType )]);
end;

destructor TP3DStreamableContainer.Destroy;
begin
  if ( Assigned( AsValue )) then
    AsValue:= nil;
  inherited Destroy;
end;

function TP3DStreamableContainer.SaveToJSON: TJSONData;
begin
  if ( StoreMode = smText ) then
    if ( Assigned( AsValue )) then
      Result:= AsValue.SaveToJSON
    else
      Result:= TJSONNull.Create
  else
    Result:= nil;
end;

function TP3DStreamableContainer.CreateObjectFromJSON(AJSON: TJSONData): TP3DStreamable;
var
  Tp: TP3DStreamableClass;
  TName: TJSONStringType;
begin
  Tp:= ItemClass;
  if ( AJSON.JSONType = jtObject ) then
    begin
      TName:= TJSONObject( AJSON ).Get( 'ClassName', '' );
      if ( not Assigned( Root )) then
        begin
          {$IFDEF VERBOSE}WriteLn( Name + ': Root is not assigned.' );{$ENDIF}
          raise Exception.Create( Name + ': Root is not assigned.' );
        end;
      if ( not Assigned( Root.GetClassFactory )) then
        begin
          {$IFDEF VERBOSE}WriteLn( GetFullPath + ': Root.GetClassFactory is not assigned.' );{$ENDIF}
          raise Exception.Create( GetFullPath + ': Root.GetClassFactory is not assigned.' );
        end;
      if ( TName > '' ) then
        Tp:= TP3DStreamableClass( Root.GetClassFactory.FindByName( TName ));
      if ( not Assigned( Tp )) then
        raise Exception.CreateFmt( 'The class "%s" could not be found in the class factory!', [ TName ]);
    end;
  //WriteLn( AJSON.AsJSON );
  Result:= Tp.CreateFromJSON( AJSON, Self );
  if ( Assigned( OnChange )) then
    OnChange( Result );
end;

function TP3DStreamableContainer.GetValueAsStreamable: TP3DStreamable;
begin
  Result:= AsValue;
end;

procedure TP3DStreamableContainer.SetValueAsStreamable(AValue: TP3DStreamable);
begin
  AsValue:= AValue;
end;

function TP3DStreamableContainer.GetChildPropByPath(var APath: String; out RestString: String; out
  LastFound: TP3DPropertyAccess): TP3DPropertyAccess;
begin
  if ( Assigned( AsValue )) then begin
    //if ( AsValue is TP3DNamedStreamable ) then WriteLn( TP3DNamedStreamable( AsValue ).Name );
    Result:= AsValue.Properties.GetPropByPath( APath, RestString, LastFound )
  end else
    Result:= nil;
end;

procedure TP3DStreamableContainer.NotifyOnDelete(AItem: TP3DStreamable);
var
  Ct: TObject;
begin
  FAsValue:= nil;
  Ct:= Context.GetInstance;
  if ( Ct is TP3DStreamableListBase ) then
    begin
      TP3DStreamableListBase( Ct ).Notify( Ct, AItem, actDelete );
      TP3DStreamableListBase( Ct ).FItems.Remove( Self );
      TP3DStreamableListBase( Ct ).NotifyAfter( Ct, AItem, actDelete );
      {$IFDEF VERBOSE}WriteLn( TP3DStreamableListBase( Ct ).Name, '.Count = ', TP3DStreamableListBase( Ct ).Count ); {$ENDIF} end;
end;

function TP3DStreamableContainer.NotifyOnAdd(AItem: TP3DStreamable): Boolean;
begin
  Result:= ( not Assigned( AItem )) or ( AItem.InheritsFrom( ItemClass ));
  if ( Result ) then
    SetValueAsStreamable( AItem );
end;

function TP3DStreamableContainer.GetContainer: TP3DPropertyAccess;
begin
  Result:= Self;
end;

function TP3DStreamableContainer.ValueIsDefault: Boolean;
begin
  Result:= not Assigned( AsValue );
end;

constructor TP3DStreamableContainer.Create(AName: String;
  const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create(AName, AStoreMode);
  FAsValue:= nil;
end;

constructor TP3DStreamableContainer.Create(AName: String;
  AItemClass: TP3DStreamableClass; const AStoreMode: TP3DPropertyAccessStoreMode );
begin
  if ( not AItemClass.InheritsFrom( TP3DStreamable )) then
    raise Exception.CreateFmt( 'Cannot create a streamable with the class type "%s" because it is no descendant of TP3DStreamable!', [ AItemClass.ClassName ]);
  inherited Create( AName, AStoreMode );
  FItemClass:= AItemClass;
  FAsValue:= nil;
end;

procedure TP3DStreamablePointer.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtString ) then
    AsString:= AValue.AsString
  else if ( AValue.JSONType = jtNull ) then
    AsValue:= nil
  else
    raise Exception.Create( 'Cannot load from StreamablePointer from JSON because the context was no pointer!' );
end;

function TP3DStreamablePointer.GetAsString: String;
begin
  if ( Assigned( AsValue )) then
    if ( PathAbsolute or ( AsValue.Root <> Root ) ) then
      Result:= AsValue.GetFullPath
    else
      Result:= AsValue.GetRelativePath
  else
    Result:= '<None>';
end;

procedure TP3DStreamablePointer.SetAsString(AValue: String);
var
  Prop: TP3DPropertyAccess;
begin
  if ( Assigned( Context ) and Assigned( Context.GetRoot )) then
    begin
      Prop:= Context.GetRoot.GetJSONLoader().GetPropByPath( AValue, True );
      if ( Assigned( Prop )) then
        Assign( Prop )
      else
        raise Exception.CreateFmt( 'Error: Cannot set streamable pointer. The target "%s" was not found!', [ AValue ]);
    end
  else
    raise Exception.Create( 'Empty context for ' + Name );
end;

function TP3DStreamablePointer.SaveToJSON: TJSONData;
begin
  case StoreMode of
    smText: Result:= AsJSON;
    else
      Result:= nil;
  end;
end;

function TP3DStreamablePointer.GetAsJSON: TJSONData;
begin
  if ( Assigned( AsValue )) then
    if ( PathAbsolute or ( AsValue.Root <> Root )) then
      Result:= TJSONString.Create( AsValue.GetFullPath )
    else
      Result:= TJSONString.Create( AsValue.GetRelativePath )
  else
    Result:= TJSONNull.Create;
end;


{ TListEnumerator }

constructor gP3DStreamableListEnumerator.Create( AStartIndex: Integer; AMoveNext: TMoveNext );
begin
  inherited Create;
  FillByte( FCurrent, SizeOf( FCurrent ), 0 );
  FCurrentIdx:= AStartIndex;
  FMoveNext:= AMoveNext;
end;

function gP3DStreamableListEnumerator.MoveNext: Boolean;
begin
  Result:= FMoveNext( FCurrentIdx, FCurrent );
end;

function gP3DStreamableListEnumerator.GetEnumerator: TEnumerator;
begin
  Result:= Self;
  // Class needs this method when constructor for enumerator in enumerated class
  // is not named GetEnumerator (E.g. reversed, filtered, ...)
end;

{ TP3DStreamableListBase }

function TP3DStreamableListBase.GetChildPropByPath(var APath: String; out
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


function TP3DStreamableListBase.GetAsJSON: TJSONData;
var
  a: TJSONArray;
  i: Integer;
begin
  a:= TJSONArray.Create();
  for i:= 0 to Count - 1 do
    a.Add( GetArrayItemAsProp( i ).AsJSON );
  Result:= a;
end;

function TP3DStreamableListBase.GetAsString: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to Count - 1 do
    Result+= ', ' + GetArrayItemAsProp( i ).AsString;
  Result:= '[' + Copy( Result, 3, Length( Result ) - 2 ) + ']';
end;

function TP3DStreamableListBase.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure TP3DStreamableListBase.LoadJSON(AValue: TJSONData);
var
  i: Integer;
begin
  if ( AValue.JSONType = jtArray ) then begin
    Count:= TJSONArray( AValue ).Count;
    for i:= 0 to TJSONArray( AValue ).Count - 1 do
      LoadItemFromJSON( i, TJSONArray( AValue )[ i ]);
  end else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtArray', JSONTypeName( AValue.JSONType )]);
end;

procedure TP3DStreamableListBase.SetAsString(AValue: String);
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

procedure TP3DStreamableListBase.SetCount(AValue: Integer);
begin
  FItems.Count:= AValue;
end;

function TP3DStreamableListBase.GetEnumerator: TStreamableListEnumerator;
begin
  Result:= TStreamableListEnumerator.Create( -1, @MoveNext );
end;

function TP3DStreamableListBase.Reversed: TStreamableListEnumerator;
begin
  Result:= TStreamableListEnumerator.Create( Count, @MoveLast );
end;

function TP3DStreamableListBase.SaveToJSON: TJSONData;
var
  a: TJSONArray;
  i: Integer;
  json: TJSONData;
begin
  case StoreMode of
    smText:
      begin
        a:= TJSONArray.Create();
        for i:= 0 to Count - 1 do
          begin
            json:= GetArrayItemAsProp( i ).SaveToJSON;
            if ( Assigned( json )) then
              a.Add( json );
          end;
        Result:= a;
      end
    else
      Result:= nil;
  end;
end;

function TP3DStreamableListBase.Find(Item: TP3DStreamable): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( AsValues[ i ] = Item ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DStreamableListBase.Add(Item: TP3DStreamable): Integer;
var
  Prop: TP3DPropertyAccess;
begin
  Result:= Find( Item );
  if ( Result > -1 ) then
    exit;
  Notify( Self, Item, actAdd );
  Result:= FItems.Add( nil );
  Prop:= GetArrayItemAsProp( Result );

  ( Prop as IP3DPropAccessStreamable ).SetValueAsStreamable( Item );
  Prop.Name:= Name + '[' + GetStrIdxOf( Result ) + ']';
  NotifyAfter( Self, Item, actAdd );
end;

function TP3DStreamableListBase.Add: TP3DPropertyAccess;
var
  n: Integer;
begin
  Notify( Self, nil, actAdd );
  Result:= ItemProp.Create( '', smText );
  Result.Context:= Self;
  Result.Root:= Root;

  n:= FItems.Add( Result );
  Result.Name:= Name + '[' + GetStrIdxOf( n ) + ']';
  NotifyAfter( Self, nil, actAdd );
  //P3DLog.LogInfo( Self, 'TP3DStreamableListBase.Add: TP3DPropertyAccess - ' + Result.Name );
end;

procedure TP3DStreamableListBase.Remove(AItem: TP3DStreamable);
var
  n: Integer;
begin
  Notify( Self, AItem, actDelete );
  n:= Find( AItem );
  if ( n > 0 ) then
    FItems.Delete( n );
  NotifyAfter( Self, AItem, actDelete );
end;

procedure TP3DStreamableListBase.Delete(Index: Integer);
var
  Itm: TP3DStreamable;
begin
  Itm:= AsValues[ Index ];
  Notify( Self, Itm, actDelete );
  FItems.Delete( Index );
  NotifyAfter( Self, Itm, actDelete );
end;

procedure TP3DStreamableListBase.Clear;
begin
  Notify( Self, nil, actClear );
  FItems.Clear;
  NotifyAfter( Self, nil, actClear );
end;

function TP3DStreamableListBase.First(const AClassType: TP3DStreamableClass): TP3DStreamable;
var
  n: Integer;
begin
  Result:= nil;
  n:= 0;
  while ( not Assigned( Result ) and ( n < Count )) do begin
    if (( not Assigned( AClassType )) or ( AsValues[ n ] is AClassType )) then
      Result:= AsValues[ n ];
    Inc( n );
  end;
end;

function TP3DStreamableListBase.Last(const AClassType: TP3DStreamableClass): TP3DStreamable;
var
  n: Integer;
begin
  Result:= nil;
  n:= Count - 1;
  while ( not Assigned( Result ) and ( n > 0 )) do begin
    if (( not Assigned( AClassType )) or ( AsValues[ n ] is AClassType )) then
      Result:= AsValues[ n ];
    Dec( n );
  end;
end;

procedure TP3DStreamableListBase.Insert(Index: Integer; Item: TP3DStreamable);
var
  Prop: TP3DPropertyAccess;
begin
  if ( Find( Item ) > -1 ) then
    exit;
  Notify( Self, Item, actAdd );
  Prop:= ItemProp.Create( Name + '[' + GetStrIdxOf( Index ) + ']' );
  Prop.Context:= Self;
  Prop.Root:= Root;

  ( Prop as IP3DPropAccessStreamable ).SetValueAsStreamable( Item );
  FItems.Insert( Index, Prop );
  NotifyAfter( Self, Item, actAdd );

  //P3DLog.LogInfo( Self, 'Insert(Index: Integer; Item: T) - ' + Prop.Name );
end;

constructor TP3DStreamableListBase.Create(AName: String;
  const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create(AName, AStoreMode);
  FItemClass:= TP3DStreamable;
end;

procedure TP3DStreamableListBase.Notify(ASender: TObject;
  AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  if ( Assigned( OnChange )) then
    OnChange( ASender, AItem, AAction );
end;

procedure TP3DStreamableListBase.NotifyAfter(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  if ( Assigned( AfterChange )) then
    AfterChange( ASender, AItem, AAction );
end;

function TP3DStreamableListBase.MoveNext(var AIndex: Integer; out
  AItem: TP3DStreamable): Boolean;
begin
  Inc( AIndex );
  Result:= AIndex < Count;
  if ( Result ) then
    AItem:= AsValues[ AIndex ]
  else
    AItem:= nil;
end;

function TP3DStreamableListBase.MoveLast(var AIndex: Integer; out AItem: TP3DStreamable): Boolean;
begin
  Dec( AIndex );
  Result:= AIndex >= 0;
  if ( Result ) then
    AItem:= AsValues[ AIndex ]
  else
    AItem:= nil;
end;

function TP3DStreamableListBase.GetStrIdxOf(AIndex: Integer): String;
begin
  Result:= IntToStr( AIndex );
end;

function TP3DStreamableListBase.GetItems(Index: Integer): TP3DPropertyAccess;
begin
  Result:= TP3DPropertyAccess( FItems[ Index ]);
end;

function TP3DStreamableListBase.GetFullPath: String;
begin
  if ( Assigned( Context )) then
    Result:= Context.GetFullPath
  else
    Result:= '';
end;

function TP3DStreamableListBase.GetRelativePath: String;
var
  obj: IP3DJSONStream;
begin
  if ( Assigned( Context ) and not (( Context.GetInterface( IP3DJSONStream, obj ) and ( obj = Root )))) then
    Result:= Context.GetRelativePath
  else
    Result:= '';
end;

function TP3DStreamableListBase.GetValues(Index: Integer): TP3DStreamable;
begin
  if ( InRange( Index, 0, Count - 1 )) then
    Result:= ( GetArrayItemAsProp( Index ) as IP3DPropAccessStreamable ).GetValueAsStreamable
  else
    Result:= nil;
end;

procedure TP3DStreamableListBase.SetValues(Index: Integer;
  AValue: TP3DStreamable);
var
  Old: TP3DStreamable;
begin
  if ( not InRange( Index, 0, Count - 1 )) then
    exit;
  Old:= AsValues[ Index ];
  if ( Old = AValue ) then
    exit;
  Notify( Self, Old, actDelete );
  if ( Find( AValue ) > -1 ) then
    Delete( Index )
  else
    begin
      Notify( Self, AValue, actAdd );
      ( FItems[ Index ] as IP3DPropAccessStreamable ).SetValueAsStreamable( AValue );
      NotifyAfter( Self, AValue, actAdd );

      //P3DLog.LogInfo( Self, 'SetItems( Index: Integer ; AValue: T) - ' + TItemProp( FItems[ Index ]).Name );
    end;
  NotifyAfter( Self, Old, actDelete );
end;

constructor TP3DStreamableListBase.Create(AName: String;
  AItemClass: TP3DStreamableClass; const AStoreMode: TP3DPropertyAccessStoreMode );
begin
  inherited Create( AName, AStoreMode );
  FItems:= TStreamableList.Create;
  FItemClass:= AItemClass;
end;

destructor TP3DStreamableListBase.Destroy;
begin
  FItems.Free;
  FItems:= nil;
  inherited Destroy;
end;

procedure TP3DStreamableListBase.LoadItemFromJSON(Index: Integer; AData: TJSONData);
var
  Prop: TP3DPropertyAccess;
  Item: TP3DStreamable;
begin
  Prop:= GetArrayItemAsProp( Index );
  if ( Assigned( Prop )) then
    Prop.AsJSON:= AData
  else
    exit;
  Item:= ( Prop as IP3DPropAccessStreamable ).GetValueAsStreamable;
  if ( Assigned( Item )) then begin
    Notify( Self, Item, actAdd );
    NotifyAfter( Self, Item, actAdd );
  end else
    WriteLn( 'Could not load from data ' + AData.AsJSON );
end;


function TP3DStreamableListBase.GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess;
begin
  if ( not InRange( Idx, 0, Count - 1 )) then
    raise Exception.Create( 'Out of range!' );
  Result:= Items[ Idx ];
  if ( not Assigned( Result )) then
    begin
      Result:= ItemProp.Create( Name + '[' + GetStrIdxOf( Idx ) + ']', smText );
      Result.Context:= Self;
      Result.Root:= Root;
      FItems[ Idx ]:= Result;
      //P3DLog.LogInfo( Self, 'GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess - ' + Result.Name );
    end;
end;

function TP3DStreamableListBase.IndexOf(AItem: Pointer): Integer;
begin
  Result:= Find( TP3DStreamable( AItem ));
end;

procedure TP3DStreamableList.SetAllowDuplicates(AValue: Boolean);
begin
  if FAllowDuplicates=AValue then Exit;
  FAllowDuplicates:=AValue;
end;

procedure TP3DStreamableList.SetAllowEmpties(AValue: Boolean);
begin
  if FAllowEmpties=AValue then Exit;
  FAllowEmpties:=AValue;
end;

function TP3DStreamableList.Add(Item: TP3DStreamable): Integer;
begin
  if ( not Assigned( Item ) and ( not AllowEmpties )) then
    Result:= -1
  else
    Result:=inherited Add(Item);
  //P3DLog.LogInfo( Self, 'gP3DStreamableList.Add(Item: T): Integer;' );
end;

procedure TP3DStreamableList.Remove(AItem: TP3DStreamable);
begin
  inherited Remove(AItem);
end;

procedure TP3DStreamableList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

procedure TP3DStreamableList.Insert(Index: Integer; Item: TP3DStreamable);
begin
  if ( Assigned( Item ) or ( AllowEmpties )) then
    inherited Insert(Index, Item);
  //P3DLog.LogInfo( Self, 'gP3DStreamableList.Insert(Item: T): Integer;' );
end;

procedure TP3DStreamableContainerListBase.NotifyOnDelete(AItem: TP3DStreamable);
begin

end;


function TP3DStreamableContainerListBase.NotifyOnAdd(AItem: TP3DStreamable): Boolean;
begin
  Result:= False;
end;

function TP3DStreamableContainerListBase.GetContainer: TP3DPropertyAccess;
begin
  Result:= Self;
end;

constructor TP3DStreamableContainerListBase.Create(AName: String;
  AItemClass: TP3DStreamableClass; const AStoreMode: TP3DPropertyAccessStoreMode );
begin
  inherited Create( AName, AItemClass, AStoreMode );
  FItemProp:= TP3DStreamableContainer;
end;

function TP3DStreamableContainerListBase.GetArrayItemAsProp(Idx: Integer
  ): TP3DPropertyAccess;
begin
  Result:= inherited GetArrayItemAsProp(Idx);
  if ( Result is TP3DStreamableContainer ) then
    TP3DStreamableContainer( Result ).FItemClass:= ItemClass;
end;


{ gP3DNamedStreamableList }

function TP3DNamedStreamableList.FindUniqueName( BaseStr: String ): String;
var
  I: Integer;
begin
  I:= 0;
  repeat
    Inc( I );
    Result:= BaseStr + IntToStr( I );
  until ( FindByName( Result ) = -1 );
end;

function TP3DNamedStreamableList.FindByName(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if (( Assigned( Items[ i ])) and ( Assigned( AsValues[ i ])) and // Items[ i ] can be nil for example if many values are loaded from JSON, the count is set to the final count before it loads all the values.
        ( TP3DNamedStreamable( AsValues[ i ]).Name = AName )) then
      begin
        Result:= i;
        break;
      end;
end;

procedure TP3DNamedStreamableList.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
var
  n: Integer;
begin
  if ( AAction = actAdd ) then
    begin
      if ( not Assigned( AItem )) then
        raise Exception.Create( 'gP3DNamedStreamableList.Add: Cannot add empty item to list.' )
      else
        begin
          n:= FindByName( TP3DNamedStreamable( AItem ).Name );
          if ( n < 0 ) then
            inherited Notify( ASender, AItem, AAction )
          else if ( AsValues[ n ] <> AItem ) then
            raise Exception.CreateFmt( 'gP3DNamedStreamableList.Add: Cannot add "%s". An item with that name already exists!', [ TP3DNamedStreamable( AItem ).Name ]);
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
      Notify( Self, ( Prop as IP3DPropAccessStreamable ).GetValueAsStreamable, actAdd );
      //WriteLn( 'LoadItemFromJSON: ' + Prop.Name + ' = ' + AData.AsJSON );
    end;
end;

function gP3DNamedStreamableContainerList.GetStrIdxOf(AIndex: Integer): String;
var
  Item: TP3DNamedStreamable = nil;
  Prop: TP3DPropertyAccess;
begin
  Prop:= Items[ AIndex ];
  if ( Assigned( Prop )) then
    Item:= TP3DNamedStreamable(( Prop as IP3DPropAccessStreamable ).GetValueAsStreamable );
  if ( Assigned( Item )) then
    Result:= '"' + TP3DNamedStreamable( Item ).Name + '"'
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
  Result:= Items[ Idx ];
  if ( not Assigned( Result )) then
    begin
      Result:= ItemProp.Create( '', smText );
      Result.Context:= Self;
      Result.Root:= Root;
      FItems[ Idx ]:= Result;
      Result.Name:= Name + '[' + GetStrIdxOf( Idx ) + ']';
      if ( Result is TP3DStreamableContainer ) then
        TP3DStreamableContainer( Result ).FItemClass:= ItemClass;
    end
  else if ( Assigned(( Result as IP3DPropAccessStreamable ).GetValueAsStreamable )) then
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
  for i:= 0 to Count - 1 do begin
    //WriteLn( AsValues[ i ].Name );
    if ( Assigned( AsValues[ i ]) and // Items[ i ] can be nil for example if many values are loaded from JSON, the count is set to the final count before it loads all the values.
        ( AsValues[ i ].Name = AName )) then
      begin
        Result:= i;
        break;
      end;
  end;
end;

function gP3DNamedStreamableContainerList.GetByName(AName: String): T;
var
  n: Integer;
begin
  n:= FindByName( AName );
  if ( n > -1 ) then
    Result:= AsValues[ n ]
  else
    Result:= Default( T );
end;

function gP3DNamedStreamableContainerList.EnumerateWildcard(AWildCard: String): TNamedStreamableListEnumerator;
begin
  Result:= TNamedStreamableListEnumerator.Create( -1, @MoveNextFilter, AWildCard );
end;

function gP3DNamedStreamableContainerList.MoveNextFilter(var AIndex: Integer; out AItem: TP3DNamedStreamable; AFilter: String): Boolean;
var
  C: Integer;
begin
  C:= Count;
  while ( AIndex < C ) do begin
    Inc( AIndex );
    AItem:= AsValues[ AIndex ];
    if ( Assigned( AItem ) and ( IsWild( AItem.Name, AFilter, False ))) then
      break;
  end;

  Result:= AIndex < C;

  if ( not Result ) then
    AItem:= nil;
end;

{ TP3DNamedStreamable }

function TP3DNamedStreamable.GetName: String;
begin
  Result:= FName;
end;

function TP3DNamedStreamable.GetAsString: String;
begin
  Result:= Name + ': ' + inherited GetAsString;
end;

constructor TP3DNamedStreamable.Create(const AOwner: IP3DStreamableContainer);
var
  BaseName: String;
  obj: IP3DNamedStreamableList;
begin
  inherited Create( AOwner );
  if ( ClassName[ 1 ] = 'T' ) then
    BaseName:= Copy( ClassName, 2, Length( ClassName ) - 1 )
  else
    BaseName:= ClassName;

  //if ( not Assigned( FOwner )) then
  //  raise Exception.Create( 'Cannot create NamedStreamable because the owner is not assigned!' );

  if (( Assigned( AOwner )) and AOwner.GetInterface( IP3DNamedStreamableList, obj )) then
    begin
      FNamedStreamableList:= obj;
      FName:= obj.FindUniqueName( BaseName )
    end
  else
    begin
      FNamedStreamableList:= nil;
      FName:= BaseName;
    end;
  Properties.Add( TP3DPropAccessString.CreateGetSet( 'Name', @GetName, @SetName, smText ));
end;

procedure TP3DNamedStreamable.SetName(AValue: String);
begin
  if FName=AValue then Exit;

  if (( Assigned( FNamedStreamableList ) and ( FNamedStreamableList.FindByName( AValue ) > -1 ))) then
    raise Exception.CreateFmt( 'TP3DNamedStreamable.SetName: A value with the name "%s" already exists in the same container!', [ AValue ])
  else
    FName:= AValue;
end;

{ gP3DNamedStreamableListEnumerator }

constructor gP3DNamedStreamableListEnumerator.Create(AStartIndex: Integer; AMoveNext: TMoveNext; AFilter: String);
begin
  inherited Create;
  FillByte( FCurrent, SizeOf( FCurrent ), 0 );
  FCurrentIdx:= AStartIndex;
  FMoveNext:= AMoveNext;
  FFilter:= AFilter;
end;

function gP3DNamedStreamableListEnumerator.MoveNext: Boolean;
begin
  if ( Assigned( FMoveNext )) then
    Result:= FMoveNext( FCurrentIdx, FCurrent, FFilter );
end;

function gP3DNamedStreamableListEnumerator.GetEnumerator: TEnumerator;
begin
  Result:= Self; //Stupid but does not get recognized as an enumerator otherwise
end;


{ gP3DStreamablePointerList }

function gP3DStreamablePointerList.GetAsValues( Index: Integer ): T;
var
  Prop: TP3DPropertyAccess;
begin
  Prop:= GetArrayItemAsProp( Index );
  Result:= T( TP3DStreamablePointer( Prop ).AsValue );
end;

procedure gP3DStreamablePointerList.SetAsValues( Index: Integer ; AValue: T);
var
  Prop: TP3DPropertyAccess;
begin
  Prop:= GetArrayItemAsProp( Index );
  TP3DStreamablePointer( Prop ).AsValue:= AValue;
end;

constructor gP3DStreamablePointerList.Create(AName: String;
  const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, T {!}, AStoreMode );
  FItemProp:= TP3DStreamablePointer;
end;

function gP3DStreamablePointerList.First(const AClassType: TP3DStreamableClass): T;
begin
  Result:= T( inherited First( AClassType ));
end;

function gP3DStreamablePointerList.Last(const AClassType: TP3DStreamableClass): T;
begin
  Result:= T( inherited Last( AClassType ));
end;

function gP3DStreamablePointerList.GetEnumerator: TTypedStreamableListEnumerator;
begin
  Result:= TTypedStreamableListEnumerator.Create( -1, @MoveNext );
end;

function gP3DStreamablePointerList.Reversed: TTypedStreamableListEnumerator;
begin
  Result:= TTypedStreamableListEnumerator.Create( Count, @MoveLast );
end;

function gP3DStreamablePointerList.GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess;
begin
  Result:=inherited GetArrayItemAsProp(Idx);
  if ( Result is TP3DPropAccessStreamable ) then
    TP3DPropAccessStreamable( Result ).FItemClass:= T;
end;

{ gP3DStreamableContainerList }

function gP3DStreamableContainerList.GetAsValues( Index: Integer ): T;
begin
  Result:= T( GetValues( Index ));
end;

procedure gP3DStreamableContainerList.SetAsValues( Index: Integer ; AValue: T);
begin
  SetValues( Index, AValue );
end;

constructor gP3DStreamableContainerList.Create(AName: String;
  const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create(AName, T {!}, AStoreMode);
end;

function gP3DStreamableContainerList.First(const AClassType: TP3DStreamableClass): T;
begin
  Result:= T( inherited First( AClassType ));
end;

function gP3DStreamableContainerList.Last(const AClassType: TP3DStreamableClass): T;
begin
  Result:= T( inherited Last( AClassType ));
end;

function gP3DStreamableContainerList.GetEnumerator: TTypedStreamableListEnumerator;
begin
  Result:= TTypedStreamableListEnumerator.Create( -1, @MoveNext );
end;

function gP3DStreamableContainerList.Reversed: TTypedStreamableListEnumerator;
begin
  Result:= TTypedStreamableListEnumerator.Create( Count, @MoveLast );
end;


{$ENDIF}

