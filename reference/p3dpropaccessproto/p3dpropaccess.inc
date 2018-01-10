//p3dpropaccess.inc
//..........................................................................

{$IFDEF INTERFACE}
  const
    P3DERROR_JSON_WRONG_TYPE = 'Failed to load property from JSON Data: Data is of the wrong type. Expected: "%s" Found: "%s"';
    P3DERROR_JSON_PROP_LOADER_NOT_IMPLEMENTED = 'A loader for this property type "%s" was not yet implemented';
    P3DERROR_JSON_CLASS_TYPE_NOT_FOUND = 'Failed to load object from JSON Data: The specified class type "%s" is not registered!';
    P3DERROR_JSON_CLASS_TYPE_NODE_NOT_FOUND = 'Failed to load object from JSON Data: There was no class type specified!';

  type
    TP3DPropertyAccessStoreMode = ( smDoNotStore, smText, smExternalBinary );
    TP3DStreamable = class;

    IP3DPropAccessPath = interface
      ['{DEAF160E-5AD6-496E-BFFB-E10DEE73DFAD}']
      function GetFullPath: String;
      function GetRoot: IP3DLoadJSON;
    end;


    TP3DPropertyAccessInterpolationMode = ( imInterpolate, imConstant, imExtrapolate );
    TP3DPropertyAccessInterpolationModes = set of TP3DPropertyAccessInterpolationMode;

    { TP3DPropertyAccess }

    TP3DPropertyAccess = class abstract( TInterfacedPersistent, IP3DPropAccessPath )
      protected
        FLastLoad: Integer;
        FCategory: String;
        FContext: IP3DPropAccessPath;
        FName: String;
        FStoreMode: TP3DPropertyAccessStoreMode;
        FRoot: IP3DLoadJSON;

        function GetRoot: IP3DLoadJSON;
        procedure SetAsString( AValue: String ); virtual; abstract;
        function GetAsString: String; virtual; abstract;
        function GetAsJSON: TJSONData; virtual; abstract;
        procedure SetAsJSON( AValue: TJSONData );
        procedure LoadJSON( AValue: TJSONData ); virtual; abstract;
        class function LoadDeferred: Boolean; virtual;
        class function GetSupportedInterpolationModes: TP3DPropertyAccessInterpolationModes; static;
        procedure SaveToStream( S: TStream ); virtual;
        procedure LoadFromStream( S: TStream ); virtual;

      public
        constructor Create( AName: String; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );

        procedure Assign( AProp: TP3DPropertyAccess ); virtual; reintroduce;
        function GetChildPropByPath( APath: String ): TP3DPropertyAccess;
        function GetChildPropByPath( var APath: String; out RestString: String; out LastFound: TP3DPropertyAccess ): TP3DPropertyAccess; virtual;
        function GetFullPath: String; virtual;
        function GetNames: TStringList; virtual;

        procedure SetInterpolate( Value1, Value2: String; AMix: Float; AInterpolationMode: TP3DPropertyAccessInterpolationMode ); virtual;
        class property SupportedInterpolationModes: TP3DPropertyAccessInterpolationModes read GetSupportedInterpolationModes;

        property AsString: String read GetAsString write SetAsString;
        property AsJSON: TJSONData read GetAsJSON write SetAsJSON;
        property Name: String read FName write FName;
        property StoreMode: TP3DPropertyAccessStoreMode read FStoreMode write FStoreMode;
        property Context: IP3DPropAccessPath read FContext write FContext;
        property Root: IP3DLoadJSON read GetRoot write FRoot;
        property LastLoad: Integer read FLastLoad;
        property Category: String read FCategory write FCategory;
    end;

    IP3DPropAccessPathArray = interface ( IP3DPropAccessPath )
      ['{EB03BC8A-9A4C-49A8-B6E2-E91D0FD309C7}']

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

      public
        function GetArrayIdxByString( Idx: String ): Integer; virtual;
        function GetArrayItemAsProp( Idx: Integer ): TP3DPropertyAccess; virtual; abstract;
        function IndexOf(AItem: Pointer): Integer; virtual; abstract;

        property Count: Integer read GetCount write SetCount;
    end;

    { TP3DPropertyList }

    TP3DPropertyList = class ( specialize gP3DCustomObjectList < TP3DPropertyAccess >)
      private
        FContext: TP3DStreamable;

        function GetAsJSON: TJSONData;
        procedure SetAsJSON( AValue: TJSONData );
        //function GetAsString: String;

      public
        constructor Create( AContext: TP3DStreamable );
        destructor Destroy; override;
        procedure Clear(const FreeObjects: Boolean=True); override;

        function Add( Item: TCustomItem ): Integer; override;
        function Add( Item: TCustomItem; Category: String ): Integer; inline;
        function Add( AItems: array of TCustomItem; const Category: String = '' ): Integer;
        function FindByName( AName: String ): Integer;

        function GetPropByName( AName: String ): TP3DPropertyAccess;
        function GetPropByPath( var APropPath: String ): TP3DPropertyAccess; inline;
        function GetPropByPath( var APropPath: String; out RestString: String ): TP3DPropertyAccess; inline;
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

        procedure SetAsValue( AValue: T ); virtual;
        procedure SetValue( AValue: T );
        function GetAsValue: T; virtual;

      public
        constructor CreateGetSet( AName:String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateGetField( AName:String; AGetter: TGetter; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateFieldSet( AName:String; AField: pT; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateField( AName:String; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );

        procedure Assign( AProp: TP3DPropertyAccess ); override;

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
        FField: pT;
        FGetter: TGetter;
        FSetter: TSetter;

        procedure SetAsValue( AValue: T ); virtual;
        function GetAsValue: T; virtual;
        function GetAsJSON: TJSONData; override;
        procedure LoadJSON(AValue: TJSONData); override;
        function GetAsString: String; override;
        procedure SetAsString(AValue: String); override;

      public
        constructor CreateGetSet( AName:String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateGetField( AName:String; AGetter: TGetter; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateFieldSet( AName:String; AField: pT; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        constructor CreateField( AName:String; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
        function GetChildPropByPath( var APath: String; out RestString: String; out LastFound: TP3DPropertyAccess ): TP3DPropertyAccess; override;

        property AsValue: T read GetAsValue write SetAsValue;

        property Getter: TGetter read FGetter write FGetter;
        property Setter: TSetter read FSetter write FSetter;
        property Field: pT read FField write FField;
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
        FProp: TP3DPropertyAccess;

      public
        constructor Create( AProp: TP3DPropertyAccess; AJSON: TJSONData );
        procedure DoLoad;

        property Prop: TP3DPropertyAccess read FProp;
        property JSON: TJSONData read FJSON;
    end;

    TP3DDeferredLoadList = specialize TFPGObjectList < TP3DDeferredLoad >;

    { TP3DJSONLoader }

    TP3DJSONLoader = class ( TPersistent )
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
        constructor Create;
        destructor Destroy; override;

        function GetPropByPath( APath: String; const DoLoad: Boolean = False ): TP3DPropertyAccess;
        procedure ReadJSON;

        property JSON: TJSONData read FJSON write SetJSON;
        property Root: TP3DStreamable read FRoot write FRoot;
        property FileAge: LongInt read FFileAge;
        property IsLoading: Boolean read FIsLoading;
    end;
    //..........................................................................

    { IP3DLoadJSON }

    IP3DLoadJSON = interface
      ['{18740D8D-D2E9-4A76-A43D-DFC2057CD2FF}']
      function GetJSONLoader(): TP3DJSONLoader;
      function GetFileAge: LongInt;
    end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

function TP3DPropertyAccess.GetFullPath: String;
var
  BasePath: String = '';
begin
  if ( Assigned( Context )) then
    BasePath:= Context.GetFullPath;
  Result:= IfThen( BasePath > '', BasePath + '.', '' ) + Name;
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

procedure TP3DPropertyAccess.SaveToStream(S: TStream);
begin

end;

procedure TP3DPropertyAccess.LoadFromStream(S: TStream);
begin

end;

function TP3DPropertyAccess.GetRoot: IP3DLoadJSON;
begin
  Result:= FRoot;
end;

procedure TP3DPropertyAccess.SetAsJSON(AValue: TJSONData);
begin
  if ( LoadDeferred and Assigned( Root ) and ( Root.GetJSONLoader.IsLoading )) then
    Root.GetJSONLoader.DeferredLoading.Add( TP3DDeferredLoad.Create( Self, AValue ))
  else
    LoadJSON( AValue );
  FLastLoad:= GetTickCount64;
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
        KeyS:= JSONStringToString( Item.Key );
        if ( KeyS = 'ClassName' ) then
          Continue;
        if ( KeyS = 'DataPath' ) then
          Continue;
        n:= FindByName( KeyS );
        if ( n < 0 ) then
          raise Exception.CreateFmt( 'Failed to load object from JSON Data: The specified property "%s" was not found in the object of type "%s"', [ KeyS, Context.ClassName ]);

        Items[ n ].AsJSON:= Item.Value;
      end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtObject ), JSONTypeName( AValue.JSONType )]);
end;

constructor TP3DPropertyList.Create(AContext: TP3DStreamable);
begin
  inherited Create;
  FContext:= AContext;
end;

destructor TP3DPropertyList.Destroy;
begin
  Clear( True );
  inherited Destroy;
end;

procedure TP3DPropertyList.Clear(const FreeObjects: Boolean);
begin
  inherited Clear( FreeObjects );
end;

function TP3DPropertyList.Add(Item: TCustomItem): Integer;
begin
  Result:= inherited Add( Item );
  Item.Context:= Context;
  Item.Root:= Context.Root;
end;

function TP3DPropertyList.Add(Item: TCustomItem; Category: String): Integer;
begin
  //Item.Category:= Category;
  Result:= Add( Item );
end;

function TP3DPropertyList.Add(AItems: array of TCustomItem; const Category: String): Integer;
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

function TP3DPropertyList.GetPropByPath(var APropPath: String): TP3DPropertyAccess;
var
  RestString: String;
  LastFound: TP3DPropertyAccess;
begin
  Result:= GetPropByPath( APropPath, RestString, LastFound );
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
  PropName:= PathGetNextFragment( APropPath, ArrayStr );

  Result:= GetPropByName( PropName );
  if ( Assigned( Result )) then
    LastFound:= Result;

  if (( Result is TP3DPropertyAccessArray ) and ( ArrayStr > '' )) then
    begin
      if ( Enclosed( ArrayStr, '"', '"' )) then
        ArrayIdx:= List.GetArrayIdxByString( ArrayStr )
      else
        ArrayIdx:= StrToIntDef( ArrayStr, -1 );
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

{ gP3DPropAccessCustomObject }

procedure gP3DPropAccessCustomObject.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtObject ) then
    AsValue:= CreateObjectFromJSON( AValue )
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
  if ( Assigned( AsValue )) then
    begin
      a:= TJSONArray.Create();
      for i:= 0 to Count - 1 do
        a.Add( GetArrayItemAsProp( i ).AsJSON );
      Result:= a;
    end
  else
    Result:= TJSONNull.Create;
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
  for i:= 0 to Count - 1 do
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
    end
  else
    Exception.Create( 'Cannot set array as string as no array brackets where found in string: ' + AValue );
end;

constructor gP3DPropAccessArray.CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Getter:= AGetter;
  Setter:= ASetter;
end;

constructor gP3DPropAccessArray.CreateGetField(AName: String; AGetter: TGetter; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Getter:= AGetter;
  Field:= AField;
end;


constructor gP3DPropAccessArray.CreateFieldSet(AName: String; AField: pT; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Field:= AField;
  Setter:= ASetter;
end;

constructor gP3DPropAccessArray.CreateField(AName: String; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Field:= AField;
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

constructor TP3DDeferredLoad.Create( AProp: TP3DPropertyAccess; AJSON: TJSONData );
begin
  inherited Create;
  FProp:= AProp;
  FJSON:= AJSON;
end;

procedure TP3DDeferredLoad.DoLoad;
begin
  Prop.AsJSON:= JSON;
end;
{$ENDIF}

//..........................................................................

