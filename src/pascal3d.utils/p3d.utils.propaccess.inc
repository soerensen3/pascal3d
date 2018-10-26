//p3dpropaccess.inc
//..........................................................................

{$IFDEF INTERFACE}
  const
    P3DERROR_JSON_WRONG_TYPE = 'Failed to load property from JSON Data: Data is of the wrong type. Expected: "%s" Found: "%s"';
    P3DERROR_JSON_PROP_LOADER_NOT_IMPLEMENTED = 'A loader for this property type "%s" was not yet implemented';
    P3DERROR_JSON_CLASS_TYPE_NOT_FOUND = 'Failed to load object from JSON Data: The specified class type "%s" is not registered!';
    P3DERROR_JSON_CLASS_TYPE_NODE_NOT_FOUND = 'Failed to load object from JSON Data: There was no class type specified!';

  type
    TP3DJSONLoader = class;
    TP3DClassFactory = class;

    { IP3DJSONStream }

    IP3DJSONStream = interface ( IP3DBaseInterface ) ['{1ED4CE6A-A993-4E1B-9B3B-C70FB5FEA3B4}']
      function GetJSONLoader(): TP3DJSONLoader;
      function GetClassFactory: TP3DClassFactory;
    end;

    IP3DJSONStreamBinary = interface ( IP3DBaseInterface ) ['{9A439E2A-413D-47A3-8122-9E791C71BDE2}']
      function GetBinaryStream: TStream;
    end;

    TP3DPropertyAccessStoreMode = ( smDoNotStore, smText, smExternalBinary );
    //TP3DStreamable = class;

    IP3DPropAccessPath = interface ( IP3DBaseInterface ) ['{DEAF160E-5AD6-496E-BFFB-E10DEE73DFAD}']
      function GetFullPath: String;
      function GetRelativePath: String;
      function GetRoot: IP3DJSONStream;
    end;


    TP3DPropertyAccessInterpolationMode = ( imInterpolate, imConstant, imExtrapolate );
    TP3DPropertyAccessInterpolationModes = set of TP3DPropertyAccessInterpolationMode;

    TP3DPropertyAccess = class;

    { TP3DPropertyPointer }

    TP3DPropertyPointer = class
      private
        FProp: TP3DPropertyAccess;

        procedure SetProp(AValue: TP3DPropertyAccess);

      public
        destructor Destroy; override;

      published
        property Prop: TP3DPropertyAccess read FProp write SetProp;
    end;

    TP3DPropertyPointerList = specialize gP3DCustomObjectList < TP3DPropertyPointer >;

    { TP3DPropertyAccess }

    TP3DPropertyAccess = class abstract( TP3DInterfacedObject, IP3DPropAccessPath )
      protected
        FOnChange: TNotifyEvent;
        FUsers: TP3DPropertyPointerList;
        FLastLoad: Integer;
        FCategory: String;
        FContext: IP3DPropAccessPath;
        FName: String;
        FStoreMode: TP3DPropertyAccessStoreMode;
        FRoot: IP3DJSONStream;

        function GetRoot: IP3DJSONStream;
        procedure SetAsString( AValue: String ); virtual; abstract;
        function GetAsString: String; virtual; abstract;
        function GetAsJSON: TJSONData; virtual; abstract;
        procedure SetAsJSON( AValue: TJSONData );
        procedure LoadJSON( AValue: TJSONData ); virtual; abstract;
        class function LoadDeferred: Boolean; virtual;
        class function GetSupportedInterpolationModes: TP3DPropertyAccessInterpolationModes; static;
        function ValueIsDefault: Boolean; virtual; abstract;

      public
        constructor Create( AName: String; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore ); virtual;
        destructor Destroy; override;

        function SaveToJSON: TJSONData; virtual;
        procedure Assign( AProp: TP3DPropertyAccess ); virtual; reintroduce;
        function GetChildPropByPath( APath: String ): TP3DPropertyAccess;
        function GetChildPropByPath( var APath: String; out RestString: String; out LastFound: TP3DPropertyAccess ): TP3DPropertyAccess; virtual;
        function GetRelativePath: String; virtual;
        function GetFullPath: String; virtual;
        function GetNames: TStringList; virtual;

        procedure SetInterpolate( Value1, Value2: String; AMix: Float; AInterpolationMode: TP3DPropertyAccessInterpolationMode ); virtual;
        class property SupportedInterpolationModes: TP3DPropertyAccessInterpolationModes read GetSupportedInterpolationModes;

        property AsString: String read GetAsString write SetAsString;
        property AsJSON: TJSONData read GetAsJSON write SetAsJSON;
        property Name: String read FName write FName;
        property StoreMode: TP3DPropertyAccessStoreMode read FStoreMode write FStoreMode;
        property Context: IP3DPropAccessPath read FContext write FContext;
        property Root: IP3DJSONStream read GetRoot write FRoot;
        property LastLoad: Integer read FLastLoad;
        property Category: String read FCategory write FCategory;
        property OnChange: TNotifyEvent read FOnChange write FOnChange;
        property IsDefault: Boolean read ValueIsDefault;
    end;

    { TP3DPropertyPointer }

    TP3DPropertyAccessClass = class of TP3DPropertyAccess;

    IP3DPropAccessPathArray = interface ( IP3DPropAccessPath ) ['{EB03BC8A-9A4C-49A8-B6E2-E91D0FD309C7}']
      function GetArrayItemAsProp( Idx: Integer ): TP3DPropertyAccess;
      function IndexOf( AItem: Pointer ): Integer;
      function GetCount: Integer;
      procedure SetCount( AValue: Integer );
    end;

    { TP3DPropertyAccessArray }

    TP3DPropertyAccessArray = class abstract( TP3DPropertyAccess, IP3DPropAccessPathArray )
      protected
        procedure SetCount( AValue: Integer ); virtual; abstract;
        function GetCount: Integer; virtual; abstract;
        function ValueIsDefault: Boolean; override;

      public
        function GetArrayIdxByString( Idx: String ): Integer; virtual;
        function GetArrayItemAsProp( Idx: Integer ): TP3DPropertyAccess; virtual; abstract;
        function IndexOf(AItem: Pointer): Integer; virtual; abstract;

        property Count: Integer read GetCount write SetCount;
    end;

    { TP3DPropertyList }

    TP3DStreamable = class;
    TP3DStreamableClass = class of TP3DStreamable;

    TP3DPropertyList = class ( specialize gP3DCustomObjectList < TP3DPropertyAccess >)
      private
        FContext: TP3DStreamable;

        function GetAsJSON: TJSONData;
        procedure SetAsJSON( AValue: TJSONData );
        //function GetAsString: String;

      public
        constructor Create( AContext: TP3DStreamable );

        function Add( Item: TP3DPropertyAccess ): Integer; override;
        function Add( Item: TP3DPropertyAccess; Category: String ): Integer;// inline;
        function Add( AItems: array of TP3DPropertyAccess; const Category: String = '' ): Integer;
        function FindByName( AName: String ): Integer;
        function SaveToJSON: TJSONData;

        function GetPropByName( AName: String ): TP3DPropertyAccess;
        function GetPropByPath( const APropPath: String ): TP3DPropertyAccess; //inline;
        function GetPropByPath( var APropPath: String; out RestString: String ): TP3DPropertyAccess;// inline;
        function GetPropByPath( var APropPath: String; out RestString: String; var LastFound: TP3DPropertyAccess): TP3DPropertyAccess;

        property Context: TP3DStreamable read FContext write FContext;
        property AsJSON: TJSONData read GetAsJSON write SetAsJSON;
    end;

    { gP3DPropAccess }

    generic gP3DPropAccess < T > = class ( TP3DPropertyAccess )
      type
        TGetter = function: T of object;
        TSetter = procedure ( AValue: T ) of object;
        pT = ^T;
        TProp = specialize gP3DPropAccess < T >;

      protected
        FField: pT;
        FGetter: TGetter;
        FSetter: TSetter;
        FOwnsField: Boolean;

        procedure SetAsValue( AValue: T ); virtual;
        procedure SetValue( AValue: T );
        function GetAsValue: T; virtual;
        function ValueIsDefault: Boolean; override;

      public
        constructor Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
        constructor CreateGetSet( AName:String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateGetField( AName:String; AGetter: TGetter; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateFieldSet( AName:String; AField: pT; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateField( AName:String; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        destructor Destroy; override;

        procedure Assign( AProp: TP3DPropertyAccess ); override;

        //class operator Implicit( AValue:T ): TProp;
        //class operator Implicit( Prop: TProp ): T;


        property AsValue: T read GetAsValue write SetAsValue;

        property Getter: TGetter read FGetter write FGetter;
        property Setter: TSetter read FSetter write FSetter;
        property Field: pT read FField write FField;
    end;

    { gP3DPropAccessArray }

    generic gP3DPropAccessArray < T > = class ( TP3DPropertyAccessArray )
      type
        TGetter = function: T of object;
        TSetter = procedure ( AValue: T ) of object;
        pT = ^T;

      protected
        FMaxStringElements: Integer;
        FField: pT;
        FStoredValue: T;
        FGetter: TGetter;
        FSetter: TSetter;
        FOwnsField: Boolean;

        procedure SetAsValue( AValue: T ); virtual;
        function GetAsValue: T; virtual;
        function GetAsJSON: TJSONData; override;
        procedure LoadJSON(AValue: TJSONData); override;
        function GetAsString: String; override;
        procedure SetAsString(AValue: String); override;

      public
        constructor Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
        constructor CreateGetSet( AName:String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateGetField( AName:String; AGetter: TGetter; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateFieldSet( AName:String; AField: pT; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateField( AName:String; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        destructor Destroy; override;

        function GetChildPropByPath( var APath: String; out RestString: String; out LastFound: TP3DPropertyAccess ): TP3DPropertyAccess; override;

        property AsValue: T read GetAsValue write SetAsValue;

        property Getter: TGetter read FGetter write FGetter;
        property Setter: TSetter read FSetter write FSetter;
        property Field: pT read FField write FField;
        property MaxStringElements: Integer read FMaxStringElements write FMaxStringElements;
    end;

    { gP3DPropAccessList }

    generic gP3DPropAccessList < TList, T, TItemProp > = class ( specialize gP3DPropAccessArray < TList > )
      protected
        FFakePropIdx: Integer;
        FFakeProp: TItemProp;
        FChildrenStoreMode: TP3DPropertyAccessStoreMode;
        procedure SetChildrenStoreMode(AValue: TP3DPropertyAccessStoreMode);

        function GetFakeProp: T;
        procedure SetFakeProp(AValue: T);
        function GetCount: Integer; override;
        procedure SetCount(AValue: Integer); override;
        function CreateObjectFromJSON( AJSON: TJSONData ): TList; virtual;
        procedure LoadJSON(AValue: TJSONData); override;
        function GetAsJSON: TJSONData; override;
        function GetAsString: String; override;
        procedure SetAsValues( Index: Integer; AValue: T );
        function GetAsValues( Index: Integer ): T;

      public
        constructor Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore); override;
        destructor Destroy; override;
        function SaveToJSON: TJSONData; override;

        function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;
        function IndexOf(AItem: Pointer): Integer; override;

        property AsValues[ Index: Integer ]: T read GetAsValues write SetAsValues;
        property ChildrenStoreMode: TP3DPropertyAccessStoreMode read FChildrenStoreMode write SetChildrenStoreMode;
    end;

    { gP3DPropAccessListBinaryStream }

    generic gP3DPropAccessListBinaryStream < TList, T, TItemProp > = class ( specialize gP3DPropAccessList < TList, T, TItemProp > )
      procedure LoadFromStream(S: TStream);
      procedure SaveToStream(S: TStream);
      function SaveToJSON: TJSONData; override;
      procedure LoadJSON(AValue: TJSONData); override;
      function GetAsJSON: TJSONData; override;
    end;

    { gP3DPropAccessCustomObject }

    generic gP3DPropAccessCustomObject < T > = class ( specialize gP3DPropAccess < T >)
      public
        function CreateObjectFromJSON( AJSON: TJSONData ): T; virtual; abstract;
        procedure LoadJSON(AValue: TJSONData); override;
    end;

    { TP3DDeferredLoad }

    TP3DDeferredLoad = class
      private
        FJSON: TJSONData;
        FProp: String;

      public
        constructor Create( AProp: String; AJSON: TJSONData );
        procedure DoLoad( Root: IP3DJSONStream );

        property Prop: String read FProp;
        property JSON: TJSONData read FJSON;
    end;

    TP3DDeferredLoadList = specialize gP3DCustomObjectList < TP3DDeferredLoad >;

    { TP3DJSONLoader }

    TP3DJSONLoader = class ( TP3DFilePointer )
      private
        FDeferredLoading: TP3DDeferredLoadList;
        FFileAge: LongInt;
        FIsLoading: Boolean;
        FJSON: TJSONData;
        FRoot: TP3DStreamable;

        procedure SetJSON(AValue: TJSONData);
        procedure BeginRead;
        procedure EndRead;

        property DeferredLoading: TP3DDeferredLoadList read FDeferredLoading;

      public
        constructor Create(AFileName: String; AUserPointer: Pointer=nil);
        destructor Destroy; override;

        function GetPropByPath( APath: String; const DoLoad: Boolean = False ): TP3DPropertyAccess;
        procedure ReadJSON;
        procedure ReadFile;
        procedure WriteFile( const AFileName: String = '' );

        property JSON: TJSONData read FJSON write SetJSON;
        property Root: TP3DStreamable read FRoot write FRoot;
        property FileAge: LongInt read FFileAge;
        property IsLoading: Boolean read FIsLoading;
    end;

    //..........................................................................



{$ENDIF}

{$IFDEF IMPLEMENTATION}

function TP3DPropertyAccess.GetFullPath: String;
begin
  if ( Assigned( Context )) then
    Result:= Context.GetFullPath
  else
    Result:= '';

  Result:= ifthen( Result > '', Result + '.' + Name, Name );
end;

function TP3DPropertyAccess.GetRelativePath: String;
var
  BasePath: String = '';
  obj: IP3DJSONStream;
begin
  if ( Assigned( Context )) then
    if ( Context.GetInterface( IP3DJSONStream, obj ) and ( obj = Root )) then
      BasePath:= '.'
    else
      BasePath:= Context.GetRelativePath + '.';
  Result:= BasePath + Name;
end;

function TP3DPropertyAccess.GetNames: TStringList;
begin
  Result:= nil;
end;

procedure TP3DPropertyAccess.SetInterpolate(Value1, Value2: String;
  AMix: Float; AInterpolationMode: TP3DPropertyAccessInterpolationMode);
begin
  // Always constant by default
  if ( AMix >= 1 ) then AsString:= Value2 else AsString:= Value1;
end;

class function TP3DPropertyAccess.GetSupportedInterpolationModes: TP3DPropertyAccessInterpolationModes; static;
begin
  Result:= [ imConstant ];
end;

function TP3DPropertyAccess.GetRoot: IP3DJSONStream;
begin
  Result:= FRoot;
end;

procedure TP3DPropertyAccess.SetAsJSON(AValue: TJSONData);
begin
  if ( LoadDeferred and Assigned( Root ) and ( Root.GetJSONLoader.IsLoading )) then
    Root.GetJSONLoader.DeferredLoading.Add( TP3DDeferredLoad.Create( GetRelativePath, AValue ))
  else
    LoadJSON( AValue );
  FLastLoad:= GetTickCount64;
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

function TP3DPropertyAccess.SaveToJSON: TJSONData;
begin
  case StoreMode of
    smText: Result:= AsJSON;
    else
      Result:= nil;
  end;
end;

class function TP3DPropertyAccess.LoadDeferred: Boolean;
begin
  Result:= False;
end;

constructor TP3DPropertyAccess.Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create;
  FName:= AName;
  StoreMode:= AStoreMode;
  FUsers:= TP3DPropertyPointerList.Create;
  FUsers.OwnsObjects:= False;
end;

destructor TP3DPropertyAccess.Destroy;
var
  i: Integer;
begin
  for i:= 0 to FUsers.Count - 1 do
    FUsers[ i ].FProp:= nil;
  FUsers.Free;
  inherited Destroy;
end;

procedure TP3DPropertyAccess.Assign( AProp: TP3DPropertyAccess );
begin
  if ( Assigned( AProp )) then
    AsString:= AProp.AsString;
end;

function TP3DPropertyAccess.GetChildPropByPath( APath: String ): TP3DPropertyAccess;
var
  tmp1: String;
  tmp2: TP3DPropertyAccess;
begin
  Result:= GetChildPropByPath( APath, tmp1, tmp2 );
end;

function TP3DPropertyAccess.GetChildPropByPath(var APath: String; out
  RestString: String; out LastFound: TP3DPropertyAccess): TP3DPropertyAccess;
begin
  Result:= nil;
end;


{ TP3DPropertyAccessArray }

function TP3DPropertyAccessArray.ValueIsDefault: Boolean;
begin
  Result:= Count = 0;
end;

function TP3DPropertyAccessArray.GetArrayIdxByString(Idx: String): Integer;
begin
  Result:= StrToIntDef( Idx, -1 );
end;

function TP3DPropertyList.GetAsJSON: TJSONData;
var
  obj: TJSONObject;
  i: Integer;
begin
  obj:= TJSONObject.Create();
  for i:= 0 to Count - 1 do
    obj.Add( Items[ i ].Name, Items[ i ].AsJSON );
  Result:= obj;
end;

procedure TP3DPropertyList.SetAsJSON(AValue: TJSONData);
var
  Item: TJSONEnum;
  n: Integer;
  KeyS: TJSONStringType;
begin
  if ( AValue.JSONType = jtObject ) then
    for Item in AValue do
      begin
        try
          KeyS:= JSONStringToString( Item.Key );
          if ( KeyS = 'ClassName' ) then
            Continue;
          if ( KeyS = 'DataPath' ) then
            Continue;
          n:= FindByName( KeyS );
          if ( n < 0 ) then
            raise Exception.CreateFmt( 'Failed to load object from JSON Data: The specified property "%s" was not found in the object of type "%s"', [ KeyS, Context.ClassName ]);
          if ( not Assigned( Items[ n ])) then
            raise Exception.CreateFmt( 'Failed to load object from JSON Data: The specified property "%s" was found but is empty', [ KeyS ]);
          if ( not Assigned( Item.Value )) then
            WriteLN( 'Error!' );
          Items[ n ].AsJSON:= Item.Value;
        finally
        end;
      end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtObject ), JSONTypeName( AValue.JSONType )]);
end;

function TP3DPropertyList.SaveToJSON: TJSONData;
var
  obj: TJSONObject;
  i: Integer;
  data: TJSONData;
begin
  obj:= TJSONObject.Create();
  for i:= 0 to Count - 1 do
    begin
      if (( Items[ i ].StoreMode = smDoNotStore ) or ( Items[ i ].ValueIsDefault )) then
        continue;
      //WriteLn( Items[ i ].GetRelativePath{, ': ', data.AsJSON} );
      data:= Items[ i ].SaveToJSON;
      if ( Assigned( data )) then
        obj.Add( Items[ i ].Name, data );
    end;
  Result:= obj;
end;

constructor TP3DPropertyList.Create(AContext: TP3DStreamable);
begin
  inherited Create;
  FContext:= AContext;
  if ( not Assigned( Context.Root )) then
    WriteLn( 'Context root is not assigned!' );
end;

function TP3DPropertyList.Add(Item: TP3DPropertyAccess): Integer;
begin
  if ( not Assigned( Item )) then
    Exception.Create( 'Cannot add property because it is not assigned!' );
  if ( FindByName( Item.Name ) > -1 ) then
    Exception.Create( 'Cannot add property because a property with the same name already exists!' );
  Result:= inherited Add( Item );
  Item.Context:= Context;
  Item.Root:= Context.Root;
end;

function TP3DPropertyList.Add(Item: TP3DPropertyAccess; Category: String): Integer;
begin
  Result:= Add( Item );
  Item.Category:= Category;
end;

function TP3DPropertyList.Add(AItems: array of TP3DPropertyAccess; const Category: String): Integer;
var
  i: Integer;
begin
  Result:= Count;
  for i:= low( AItems ) to high( AItems ) do
    Add( AItems[ i ], Category );
end;

function TP3DPropertyList.FindByName(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = AName ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DPropertyList.GetPropByName( AName: String ): TP3DPropertyAccess;
var
  Idx: Integer;
begin
  Idx:= FindByName( AName );
  if ( InRange( Idx, 0, Count - 1 )) then
    Result:= Items[ Idx ]
  else
    Result:= nil;
end;

function TP3DPropertyList.GetPropByPath(const APropPath: String): TP3DPropertyAccess;
var
  RestString: String;
  LastFound: TP3DPropertyAccess;
  Path: String;
begin
  Path:= APropPath;
  Result:= GetPropByPath( Path, RestString, LastFound );
end;

function TP3DPropertyList.GetPropByPath(var APropPath: String; out RestString: String): TP3DPropertyAccess;
var
  LastFound: TP3DPropertyAccess;
begin
  Result:= GetPropByPath( APropPath, RestString, LastFound );
end;

function TP3DPropertyList.GetPropByPath( var APropPath: String; out RestString: String; var LastFound: TP3DPropertyAccess): TP3DPropertyAccess;
var
  PropName, ArrayStr: String;
  ArrayIdx: LongInt;
  List: TP3DPropertyAccessArray absolute Result;
begin
  //WriteLn( APropPath );
  PropName:= PathGetNextFragment( APropPath, ArrayStr );

  Result:= GetPropByName( PropName );
  if ( Assigned( Result )) then
    LastFound:= Result;

  if (( Result is TP3DPropertyAccessArray ) and ( ArrayStr > '' )) then begin
    ArrayIdx:= List.GetArrayIdxByString( ArrayStr );
    //WriteLn( ArrayStr, '<=>', ArrayIdx );
    if ( ArrayIdx < 0 ) then
      Result:= nil
    else
      Result:= List.GetArrayItemAsProp( ArrayIdx );
  end;
  if ( Assigned( Result ) and ( APropPath > '' )) then
    Result:= Result.GetChildPropByPath( APropPath, RestString, LastFound );
  if ( Assigned( Result )) then
    RestString:= ''
  else
    RestString:= APropPath;
  APropPath:= PropName;
end;

{
function TP3DPropertyList.GetAsString: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to Count - 1 do
    begin
      Result += '"' + Items[ i ].Name + '" : "' + Items[ i ].AsString + '"';
      if ( i < Count - 1 ) then
        Result += ', ';
    end;
end;
}
{ gP3DPropAccess }

procedure gP3DPropAccess.Assign( AProp: TP3DPropertyAccess );
begin
  if ( Assigned( AProp )) then
    if ( AProp is ClassType ) then
      AsValue:= TProp( AProp ).AsValue
    else
      inherited Assign( AProp );
end;

{class operator gP3DPropAccess.Implicit(AValue: T): TProp;
begin

end;

class operator gP3DPropAccess.Implicit(Prop: TProp): T;
begin

end;}

procedure gP3DPropAccess.SetAsValue( AValue: T );
begin
  SetValue( AValue );
end;

procedure gP3DPropAccess.SetValue(AValue: T);
begin
  if ( Assigned( Setter )) then
    Setter( AValue )
  else if ( Assigned( Field )) then
    Field^:= AValue;
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

function gP3DPropAccess.GetAsValue: T;
begin
  if ( Assigned( Getter )) then
    Result:= Getter()
  else if ( Assigned( Field )) then
    Result:= Field^
  else
    Result:= Default( T );
end;

function gP3DPropAccess.ValueIsDefault: Boolean;
begin
  Result:= AsValue = Default( T );
end;

constructor gP3DPropAccess.Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create(AName, AStoreMode);
  FOwnsField:= True;
  New( FField );
  Field^:= default( T );
end;

constructor gP3DPropAccess.CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Getter:= AGetter;
  Setter:= ASetter;
end;

constructor gP3DPropAccess.CreateGetField(AName: String; AGetter: TGetter; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Getter:= AGetter;
  Field:= AField;
end;

constructor gP3DPropAccess.CreateFieldSet(AName: String; AField: pT; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Field:= AField;
  Setter:= ASetter;
end;

constructor gP3DPropAccess.CreateField(AName: String; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Field:= AField;
end;

destructor gP3DPropAccess.Destroy;
begin
  if ( FOwnsField ) then
    Dispose( Field );
  inherited Destroy;
end;

{ gP3DPropAccessList }

procedure gP3DPropAccessList.SetChildrenStoreMode(AValue: TP3DPropertyAccessStoreMode);
begin
  if FChildrenStoreMode=AValue then Exit;
  FChildrenStoreMode:=AValue;
end;

function gP3DPropAccessList.GetFakeProp: T;
begin
  Result:= AsValues[ FFakePropIdx ];
end;

procedure gP3DPropAccessList.SetFakeProp(AValue: T);
begin
  AsValues[ FFakePropIdx ]:= AValue;
end;

function gP3DPropAccessList.GetCount: Integer;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.Count
  else
    Result:= -1;
end;

procedure gP3DPropAccessList.SetCount(AValue: Integer);
begin
  if ( Assigned( AsValue )) then
    AsValue.Count:= AValue;
end;

function gP3DPropAccessList.CreateObjectFromJSON(AJSON: TJSONData): TList;
begin
  if ( Assigned( AsValue )) then
    AsValue.Clear
  else
    AsValue:= TList.Create;
  Result:= AsValue;
end;

procedure gP3DPropAccessList.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtArray ) then
    CreateObjectFromJSON( AValue )
  else
    raise Exception.CreateFmt( Name + ': ' + ClassName + ' ' + P3DERROR_JSON_WRONG_TYPE, [ 'jtArray', JSONTypeName( AValue.JSONType )]);
  inherited LoadJSON( AValue );
end;

function gP3DPropAccessList.GetAsJSON: TJSONData;
begin
  if ( Assigned( AsValue )) then
    Result:= inherited GetAsJSON
  else
    Result:= TJSONNull.Create;
end;

function gP3DPropAccessList.GetAsString: String;
begin
  Result:=inherited GetAsString;
end;

procedure gP3DPropAccessList.SetAsValues(Index: Integer; AValue: T);
begin
  if ( Assigned( AsValue )) then
    AsValue[ Index ]:= AValue;
end;

function gP3DPropAccessList.GetAsValues(Index: Integer): T;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue[ Index ]
  else
    Result:= Default( T );
end;

constructor gP3DPropAccessList.Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FChildrenStoreMode:= AStoreMode;
end;

function gP3DPropAccessList.IndexOf(AItem: Pointer): Integer;
begin
  if ( Assigned( AsValue )) then
    Result:= AsValue.IndexOf( T( AItem^ ))
  else
    Result:= -1;
end;

destructor gP3DPropAccessList.Destroy;
begin
  if ( Assigned( FFakeProp )) then
    begin
      FFakeProp.Getter:= nil;
      FFakeProp.Setter:= nil;
      FFakeProp.Field:= nil;
      FreeAndNil( FFakeProp );
    end;

  inherited Destroy;
end;

function gP3DPropAccessList.SaveToJSON: TJSONData;
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
              a.Add( GetArrayItemAsProp( i ).SaveToJSON );
          end;
        Result:= a;
      end
    else
      Result:= nil;
  end;
end;

function gP3DPropAccessList.GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess;
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
  FFakeProp.Root:= Root;
  FFakeProp.StoreMode:= ChildrenStoreMode;
  Result:= FFakeProp;
end;


{ gP3DPropAccessListBinaryStream }

procedure gP3DPropAccessListBinaryStream.LoadFromStream(S: TStream);
begin
  if ( not Assigned( AsValue )) then
    AsValue:= TList.Create;

  //WriteLn( ClassName + '.LoadFromStream' );
  AsValue.LoadFromStream( S );
end;

procedure gP3DPropAccessListBinaryStream.SaveToStream(S: TStream);
begin
  if ( Assigned( AsValue )) then
    AsValue.SaveToStream( S );
end;

function gP3DPropAccessListBinaryStream.SaveToJSON: TJSONData;
var
  Obj: IP3DJSONStreamBinary;
begin
  if ( StoreMode = smExternalBinary ) then
    begin
      if ( not Root.GetInterface( IP3DJSONStreamBinary, Obj )) then
        Exception.CreateFmt( 'Cannot save property of type "%s" to stream because the root does not support IP3DJSONStreamBinary!', [ ClassName ]);
      //WriteLn( GetRelativePath, ': ', Obj.GetBinaryStream.Position, ' - ', IntToStr( Obj.GetBinaryStream.Position ) );
      Result:= CreateJSON( '@' + IntToStr( Obj.GetBinaryStream.Position ));
      SaveToStream( Obj.GetBinaryStream );
    end
  else
    Result:= inherited SaveToJSON;
end;

procedure gP3DPropAccessListBinaryStream.LoadJSON(AValue: TJSONData);
var
  S: TJSONStringType;
  Obj: IP3DJSONStreamBinary;
  n: LongInt;
begin
  if ( not Assigned( AsValue )) then
    AsValue:= TList.Create;
  if (( AValue.JSONType = jtString ) and
      ( AValue.AsString[ 1 ] = '@' )) then
    begin
      S:= AValue.AsString;
      if ( not Root.GetInterface( IP3DJSONStreamBinary, Obj )) then
        Exception.CreateFmt( 'Cannot load property of type "%s" from stream because the root does not support IP3DJSONStreamBinary!', [ ClassName ]);
      n:= StrToInt( Copy( S, 2, Length( S ) - 1 ));
      Obj.GetBinaryStream.Position:= n;
      LoadFromStream( Obj.GetBinaryStream );
    end
  else
    inherited LoadJSON(AValue);
end;

function gP3DPropAccessListBinaryStream.GetAsJSON: TJSONData;
begin
  if ( StoreMode = smText ) then
    Result:= inherited GetAsJSON
  else
    Result:= nil;
end;

{ gP3DPropAccessCustomObject }

procedure gP3DPropAccessCustomObject.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtObject ) then
    CreateObjectFromJSON( AValue )
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtObject', JSONTypeName( AValue.JSONType )]);
end;

{ gP3DPropAccessArray }

procedure gP3DPropAccessArray.SetAsValue(AValue: T);
begin
  if ( Assigned( Setter )) then
    Setter( AValue )
  else if ( Assigned( Field )) then
    Field^:= AValue;
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

function gP3DPropAccessArray.GetAsValue: T;
begin
  if ( Assigned( Getter )) then
    Result:= Getter()
  else if ( Assigned( Field )) then
    Result:= Field^
  else
    Result:= Default( T );
end;

function gP3DPropAccessArray.GetAsJSON: TJSONData;
var
  a: TJSONArray;
  i: Integer;
begin
  a:= TJSONArray.Create();
  for i:= 0 to Count - 1 do
    a.Add( GetArrayItemAsProp( i ).AsJSON );
  Result:= a;
end;

procedure gP3DPropAccessArray.LoadJSON(AValue: TJSONData);
var
  i: Integer;
begin
  if ( AValue.JSONType = jtArray ) then
    begin
      Count:= TJSONArray( AValue ).Count;
      for i:= 0 to TJSONArray( AValue ).Count - 1 do
        GetArrayItemAsProp( i ).AsJSON:= TJSONArray( AValue ).Items[ i ];
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtArray', JSONTypeName( AValue.JSONType )]);
end;

function gP3DPropAccessArray.GetAsString: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to Min( MaxStringElements, Count ) - 1 do
    Result+= ', ' + GetArrayItemAsProp( i ).AsString;
  Result:= '[' + Copy( Result, 3, Length( Result ) - 2 ) + ']';
end;

procedure gP3DPropAccessArray.SetAsString(AValue: String);
var
  Items: array of String;
  i: Integer;
begin
  if ( Enclosed( AValue, '[', ']' )) then
    begin
      Items:= AValue.Split( ',' );
      Count:= Length( Items );
      for i:= low( Items ) to high( Items ) do
        GetArrayItemAsProp( i ).AsString:= Items[ i ];
      if ( Assigned( FOnChange )) then
        FOnChange( Self );
    end
  else
    Exception.Create( 'Cannot set array as string as no array brackets where found in string: ' + AValue );
end;

constructor gP3DPropAccessArray.Create(AName: String; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create(AName, AStoreMode);
  FOwnsField:= True;
  FMaxStringElements:= 20;
  FField:= @FStoredValue;
end;

constructor gP3DPropAccessArray.CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FMaxStringElements:= 20;
  Getter:= AGetter;
  Setter:= ASetter;
end;

constructor gP3DPropAccessArray.CreateGetField(AName: String; AGetter: TGetter; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FMaxStringElements:= 20;
  Getter:= AGetter;
  Field:= AField;
end;


constructor gP3DPropAccessArray.CreateFieldSet(AName: String; AField: pT; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FMaxStringElements:= 20;
  Field:= AField;
  Setter:= ASetter;
end;

constructor gP3DPropAccessArray.CreateField(AName: String; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  FMaxStringElements:= 20;
  Field:= AField;
end;

destructor gP3DPropAccessArray.Destroy;
begin
  //if ( FOwnsField ) then
    //FreeMemAndNil( FField );
  inherited Destroy;
end;

function gP3DPropAccessArray.GetChildPropByPath(var APath: String; out
  RestString: String; out LastFound: TP3DPropertyAccess): TP3DPropertyAccess;
var
  ArrIdxStr, PropPath: String;
  ArrIdx: LongInt;
begin
  PropPath:= PathGetNextFragment( APath, ArrIdxStr );
  if ( PropPath = '' ) then
    begin
      ArrIdx:= StrToIntDef( ArrIdxStr, -1 );
      if ( ArrIdx > -1 ) then
        begin
          Result:= GetArrayItemAsProp( ArrIdx );
          if ( APath > '' ) then
            Result:= Result.GetChildPropByPath( APath, RestString, LastFound );
        end
      else
        raise Exception.CreateFmt( 'Error in "%s.GetChildPropByPath". Invalid array index "%s".', [ ClassName, ArrIdxStr ]);
    end
  else
    raise Exception.CreateFmt( 'Error in "%s.GetChildPropByPath". The object does not have a property named "%s".', [ ClassName, PropPath ]);
end;


{ TP3DDeferredLoad }

constructor TP3DDeferredLoad.Create(AProp: String; AJSON: TJSONData);
begin
  inherited Create;
  FProp:= AProp;
  FJSON:= AJSON;
  //WriteLn( 'Putting deferred load on stack ', Prop, ' = ', JSON.AsJSON );
end;

procedure TP3DDeferredLoad.DoLoad(Root: IP3DJSONStream);
var
  P: TP3DPropertyAccess;
begin
  //WriteLn( 'Deferred loading ', Prop, ' = ', JSON.AsJSON );
  P:= Root.GetJSONLoader().GetPropByPath( Prop );
  if ( Assigned( P )) then
    P.AsJSON:= JSON
  else
    raise Exception.Create( 'Path for deferred load was not found: ' + Prop );
end;

procedure TP3DJSONLoader.SetJSON(AValue: TJSONData);
begin
  if FJSON=AValue then Exit;
  if ( Assigned( JSON )) then
    JSON.Free;

  FJSON:= AValue;
  FFileAge:= GetTickCount64;
end;

procedure TP3DJSONLoader.BeginRead;
begin
  FIsLoading:= True;
end;

procedure TP3DJSONLoader.EndRead;
var
  D: TP3DDeferredLoad;
begin
  FIsLoading:= False;
  for D in DeferredLoading do
    D.DoLoad( Root.Root );
  DeferredLoading.Clear;
end;

constructor TP3DJSONLoader.Create(AFileName: String; AUserPointer: Pointer);
begin
  inherited Create( AFileName, AUserPointer );
  FDeferredLoading:= TP3DDeferredLoadList.Create;
end;

destructor TP3DJSONLoader.Destroy;
begin
  FDeferredLoading.Free;
  JSON:= nil;
  inherited Destroy;
end;

// Try to find path in properties
// Exit if valid!
// Find JSONPath to next fragment
// raise Exception if not found
// Load last found property
// return
function TP3DJSONLoader.GetPropByPath( APath: String; const DoLoad: Boolean = False ): TP3DPropertyAccess;
var
  RestString, IdxString, PropString: String;
  LastFound: TP3DPropertyAccess;
  data: TJSONData;
  p: TP3DStreamable;
  Path: String;
begin
  if ( Assigned( Root ) and ( APath > '' )) then
    begin
      if ( APath[ 1 ] = '.' ) then
        begin
          Delete( APath, 1, 1 );
          Path:= APath;
          Result:= Root.Properties.GetPropByPath( Path, RestString, LastFound );
          if ( Assigned( Result ) or ( not DoLoad )) then
            exit;
          data:= JSON.FindPath( APath );
          if ( not Assigned( data )) then
            raise Exception.CreateFmt( 'The specified path ".%s" seems to be invalid.', [ APath ]);
          data:= JSON.FindPath( Path );

          while (( not Assigned( Result )) and ( RestString > '' )) do
            begin
              PropString:= PathGetNextFragment( RestString, IdxString );
              if ( not Assigned( LastFound )) then
                raise Exception.CreateFmt( 'The specified path "%s.%s" seems to be invalid.', [ Path, PropString ]);
              if ( not Assigned( data )) then
                raise Exception.CreateFmt( 'Could not find path "%s.%s" in json file.', [ Path, PropString ]);
              LastFound.AsJSON:= data;
              if ( IdxString > '' ) then
                PropString:= PropString + '[' + IdxString + ']';
              data:= data.FindPath( PropString );
              Result:= LastFound.GetChildPropByPath( PropString, RestString, LastFound );
            end;
        end
      else if ( Assigned( Root.Owner )) then
        begin
          p:= TP3DStreamable( Root.Owner.GetRoot.GetInstance );
          if ( Assigned( p )) then
            Result:= p.Properties.GetPropByPath( APath, RestString, LastFound )
          else
            Exit( nil );
        end
      else
        Result:= nil;
    end
  else
    Result:= nil;
end;

procedure TP3DJSONLoader.ReadJSON;
begin
  BeginRead;
  if ( Assigned( Root ) and ( Assigned( JSON ))) then
    Root.AsJSON:= JSON;
  EndRead;
end;

procedure TP3DJSONLoader.ReadFile;
begin
  JSON:= GetJSON( P3DLoadFileToString( FileName ));
end;

procedure TP3DJSONLoader.WriteFile(const AFileName: String);
var
  FN: String;
begin
  if ( AFileName > '' ) then
    FN:= AFileName
  else
    FN:= FileName;
  if ( Assigned( Root )) then
    JSON:= Root.SaveToJSON
  else
    JSON:= TJSONNull.Create;
  P3DSaveStringToFile( FN, JSON.FormatJSON());
end;

{ TP3DPropertyPointer }

procedure TP3DPropertyPointer.SetProp(AValue: TP3DPropertyAccess);
begin
  if FProp=AValue then Exit;

  if ( Assigned( FProp )) then
    FProp.FUsers.Remove( Self );

  FProp:=AValue;

  if ( Assigned( FProp )) then
    FProp.FUsers.Add( Self );
end;

destructor TP3DPropertyPointer.Destroy;
begin
  Prop:= nil;
  inherited Destroy;
end;


{$ENDIF}

//..........................................................................

