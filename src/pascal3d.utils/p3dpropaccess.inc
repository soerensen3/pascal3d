{ <description>

  Copyright (c) <2016> <Johannes Rosleff Sörensen>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

{$IFDEF INTERFACE}
const
  P3DERROR_JSON_WRONG_TYPE = 'Failed to load property from JSON Data: Data is of the wrong type. Expected: "%s" Found: "%s"';
  P3DERROR_JSON_PROP_LOADER_NOT_IMPLEMENTED = 'A loader for this property type "%s" was not yet implemented';
  P3DERROR_JSON_CLASS_TYPE_NOT_FOUND = 'Failed to load object from JSON Data: The specified class type "%s" is not registered!';
  P3DERROR_JSON_CLASS_TYPE_NODE_NOT_FOUND = 'Failed to load object from JSON Data: There was no class type specified!';

type
  { TP3DPropertyAccess }

  TP3DPropertyAccessStoreMode = ( smDoNotStore, smAttribute, smNode, smExternalBinary );
  IP3DPropertyAccess = interface
    ['{05DADF8D-8437-49E9-8287-2620D1E71948}']
    function GetContext: TInterfacedPersistent;
    function GetStoreMode: TP3DPropertyAccessStoreMode;
    procedure SetContext(AValue: TInterfacedPersistent);
    procedure SetStoreMode(AValue: TP3DPropertyAccessStoreMode);
    function GetAsString: String;
    procedure SetAsString(AValue: String);
    function GetCategory: String;
    function GetName: String;
    procedure SetCategory(AValue: String);
    procedure SetName(AValue: String);
    function GetNames: TStringList;

    property Name: String read GetName write SetName;
    property Category: String read GetCategory write SetCategory;
    property AsString: String read GetAsString write SetAsString;
    property StoreMode: TP3DPropertyAccessStoreMode read GetStoreMode write SetStoreMode;
    property Context: TInterfacedPersistent read GetContext write SetContext;
  end;

  { TP3DPropertyAccess class is the base class (abstract) for any pascal3d property. You should never create an instance of TP3DPropertyAccess
    as this is an abstract class. There are different kind of properties provided for most pascal3d types. If possible an RTTI property
    should be used. To define a new kind of property one should inherit from gP3DPropertyAccess. E.g. TP3DPropertyAccessMyClass = class( specialize gP3DPropertyAccess < TMyClass > ) ...
    TP3DPropertyAccess also serves as a common base class for property variables. One does not need to know the exact type of the property
    class to get the name of a property and to read and write it's value as a string, for example.
  }
  TP3DPropertyAccess = class abstract ( TInterfacedPersistent, IP3DPropertyAccess )
    protected
      FContext: TInterfacedPersistent;
      FStoreMode: TP3DPropertyAccessStoreMode;
      FCategory: String;
      FName: String;

      function GetArrayCount: Integer; virtual;
      procedure SetArrayCount(AValue: Integer); virtual;
      function GetArrayAsProp( ArrayIndex: Integer ): TP3DPropertyAccess; virtual;
      function GetArrayAsString( ArrayIndex: Integer ): String;
      class function GetIsArray: Boolean; static; virtual;
      procedure SetArrayAsString( ArrayIndex: Integer ; AValue: String); virtual;
      function GetContext: TInterfacedPersistent;
      function GetStoreMode: TP3DPropertyAccessStoreMode;
      procedure SetContext(AValue: TInterfacedPersistent);
      procedure SetStoreMode(AValue: TP3DPropertyAccessStoreMode);
      function GetAsString: String; virtual; abstract;
      procedure SetAsString(AValue: String); virtual; abstract;
      function GetCategory: String;
      function GetName: String;
      procedure SetCategory(AValue: String);
      procedure SetName(AValue: String);
      function GetArrayIdxByString( StringIndex: String ): Integer; virtual;

    public
      { Properties can also be accessed by a path. The path should start with the property's name. If a property provides child properties (usually object properties), these can be accessed by a . behind the
        property's name. The root is always the current property. If a property is not found Nil is returned. GetChildPropByPath does not raise any exceptions. }
      function GetChildPropByPath( APath: String ): TP3DPropertyAccess; virtual;

      { The constructor of TP3DPropertyAccess should never be called directly as this is an abstract class.}
      constructor Create( AName: String; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      { Returns a string list with all possible values which is mainly used for enums.}
      function GetNames: TStringList; virtual;
      function SaveToDOM( AParentNode: TDOMElement ): Boolean; virtual; deprecated;
      procedure LoadFromDOM( AParentNode: TDOMElement ); virtual; deprecated;
      { Load a the value of the property from a json context. }
      procedure LoadFromJSONContext( AContext: TP3DJSONContext ); virtual;
      { Returns a json representation of the property as string. This will also return values of sub properties. }
      function Serialize(): String; virtual; abstract;
      { Returns an item of the given index as a property. The base property is assumed to be an array. }
      property ArrayAsProp[ ArrayIndex: Integer ]: TP3DPropertyAccess read GetArrayAsProp;
      { Reads or writes the value of the given index as a string. The base property is assumed to be an array. }
      property ArrayAsString[ ArrayIndex: Integer ]: String read GetArrayAsString write SetArrayAsString;
      { Reads or writes the length of an array. The base property is assumed to be an array. }
      property ArrayCount: Integer read GetArrayCount write SetArrayCount;
      { Class property to tell if a property class can be read as an array. }
      class property IsArray: Boolean read GetIsArray;

    published
      { Name of the property. }
      property Name: String read GetName write SetName;
      { Name of the category the property belongs to. This is for example used in the object inspector. }
      property Category: String read GetCategory write SetCategory;
      { Reads or writes the value of a property as string. }
      property AsString: String read GetAsString write SetAsString;
      property StoreMode: TP3DPropertyAccessStoreMode read GetStoreMode write SetStoreMode;
      property Context: TInterfacedPersistent read GetContext write SetContext;
  end;

  { TP3DPropertyList
    is used to make an objects properties accessible to through Pascal3D's property system. Properties have to be added manually and don't
    have to be real pascal properties. Instead of creating a property by oneself one should inherit TP3DInterfacedPersistent where possible.
  }

  TP3DPropertyList = class ( specialize gP3DCustomObjectList < TP3DPropertyAccess >)
    private
      FContext: TInterfacedPersistent;

    published
      function Add( Item: TCustomItem ): Integer; override;
      function Add( Item: TCustomItem; Category: String ): Integer; inline;
      function Add( AItems: array of TCustomItem; const Category: String = '' ): Integer;
      function FindByName( AName: String ): Integer;
      function GetPropByName( AName: String ): TP3DPropertyAccess;
      function GetPropByPath( APropPath: String ): TP3DPropertyAccess; virtual;
      procedure LoadFromJSONContext( AContext: TP3DJSONContext ); virtual;
      procedure Clear(const FreeObjects: Boolean=True); override;
      function GetAsString(): String;
      procedure Sort;
      function Serialize: String;

      constructor Create( AContext: TInterfacedPersistent );
      property Context: TInterfacedPersistent read FContext write FContext;
  end;

  { gP3DPropertyAccess

  Generic to be used to specialize new kind of properties. All pascal3d property classes are based on this. In the generic param TItem the
  type of the property's value should be passed. There are different kind of constructors to support all sort of combinations of getter and
  setter functions as well as fields. With the value property you can access a property's value directly. If you are not aware of a
  property's type you have to make a detour over AsString where the value is converted to string or from string depending on the direction.
  Thus the string access is slower.
  }

  generic gP3DPropertyAccess < TItem > = class ( TP3DPropertyAccess )
    type
      TGetter = function: TItem of object;
      TSetter = procedure ( AValue: TItem ) of object;
      pItem = ^TItem;

    private
      FField: pItem;
      FGetter: TGetter;
      FSetter: TSetter;

      function GetDefaultValue: TItem; virtual; abstract;
      function GetValue: TItem; virtual;
      procedure SetValue( AValue: TItem ); virtual;
      procedure SaveToStream( S: TStream ); virtual;
      procedure LoadFromStream( S: TStream ); virtual;

    public
      { The CreateGetSet constructor assumes you have a getter and a setter function for you property. It is allowed to pass Nil as a value
        here if you want to have a read or write only property for example. The store mode defines how a property is written when
        stored to a json file. }
      constructor CreateGetSet( AName:String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      { The CreateGetField constructor assumes you have a getter function and a field for write access for you property. It is allowed to pass Nil as a value
        here if you want to have a read or write only property for example. The store mode defines how a property is written when
        stored to a json file. }
      constructor CreateGetField( AName:String; AGetter: TGetter; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      { The CreateFieldSet constructor assumes you have a field for read access and a setter function for write access for you property.
        It is allowed to pass Nil as a value here if you want to have a read or write only property for example.
        The store mode defines how a property is written when stored to a json file. }
      constructor CreateFieldSet( AName:String; AField: pItem; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      { The CreateField constructor assumes you have a field for read and write access for you property. While it is allowed to pass Nil as
        a value here it does not really make sense in this case as you can neither read or write the property. If you want a read or
        write only property with a field you should use either CreateFieldSet or CreateFieldSet and pass nil for the setter or getter.
        The store mode defines how a property is written when stored to a json file. }
      constructor CreateField( AName:String; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );

      { With value you can access the value of a property directly. If a property does is write only the DefaultValue is returned on read
        access. If a property is read only nothing happens on write access. }
      property Value: TItem read GetValue write SetValue;
      property DefaultValue: TItem read GetDefaultValue;

      { If the property has a getter function set, this is called on read access. }
      property Getter: TGetter read FGetter write FGetter;
      { If the property has a getter function set, this is called on write access. }
      property Setter: TSetter read FSetter write FSetter;
      { If the property has a Field assigned, this is used on read or write access if no getter or setter is set. }
      property Field: pItem read FField write FField;
  end;


  { gP3DListPropertyAccess

    This generic can be used for properties that have child values in form of an array. TListProp is the type of the property's value (E.g.
    TStringList). TListData is the type of the child values (E.g. String). TPropType is the type of a TP3DPropertyAccess to be used for the
    child values (E.g. TP3DPropertyAccessString). TListProp must support the following: TListProp.Count must exist and return the length of
    the array as an integer. If you want the count to be writable as well, you have to override SetArrayCount in your descendant class.
    TListProp must also support indexing like this TListProp[ Index: Integer ]: TListData. If the childprop TListData has a pascal3d property
    Name: String then you can also use String indices for paths.}

  generic gP3DListPropertyAccess < TListProp, TListData, TPropType > = class( specialize gP3DPropertyAccess < TListProp >)
    protected
      FFakeProp: TPropType;
      FFakePropIdx: Integer;

      function GetArrayCount: Integer; override;
      function GetValues( idx: Integer ): TListData; virtual;
      procedure SetArrayCount( AValue: Integer ); override;
      procedure SetValues( idx: Integer ; AValue: TListData ); virtual;
      function GetFakeProp: TListData;
      procedure SetFakeProp( AValue: TListData );
      function GetArrayIdxByString(StringIndex: String): Integer; override;
      class function GetIsArray: Boolean; static; override;

    public
      { Will return a value of the given index as a property of type TPropType. You do not need to free the property after your call but
        however you should be aware that each use of this function will make the previously returned variable point to the new index. }
      function GetArrayAsProp(ArrayIndex: Integer): TP3DPropertyAccess; override;
      { This array can be used to directly access the child values of the array property. }
      property Values[ idx: Integer ]: TListData read GetValues write SetValues;

      destructor Destroy; override;

      function SaveToDOM( AParentNode: TDOMElement ): Boolean; override; deprecated;
      procedure SaveValueToDOM( AIndex: Integer; ADOMNode: TDOMElement ); virtual; deprecated;

      { This attempts to load the whole array from a json context. The previous content of the array is overwritten. If the context is an
        array each item is being appended to the list. If the context is a string and begins with @ followed by a number the binary file is
        set to this address and the list is loaded via LoadFromStream instead. }
      procedure LoadFromJSONContext( AContext: TP3DJSONContext ); override;
      { This attempts to load a single item of the array from a json context. The previous content of the item is overwritten. This function is
        also called by LoadFromJSONContext for each item. }
      procedure LoadItemFromJSONContext( AIndex: Integer; AContext: TP3DJSONContext ); virtual;
      { This attempts to load a single item of the array from stream. The previous content of the item is overwritten. This function is
        also called by LoadFromJSONContext for each item if the json context is a string pointer. }
      //procedure LoadItemFromStream( AIndex: Integer; S: Stream ); virtual;
      { This attempts to load the array from stream beginning at the current position of the binary file. }
      procedure LoadFromStream(S: TStream); override;
      { This attempts to save the array to a stream beginning at the current position of the binary file. }
      procedure SaveToStream(S: TStream); override;

      function Serialize: String; override;

    published
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
  end;

  { gP3DListPropertyAccessObject
    This makes the list itself an object. Value is only accessed when assigned. The value can be created when streaming the property from json }

  generic gP3DListPropertyAccessObject < TListProp, TListData, TPropType > = class( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
    protected
      { GetArrayCount is now performing a check if Value is assigned before access }
      function GetArrayCount: Integer; override;
      { GetValues is now performing a check if Value is assigned before access }
      function GetValues(idx: Integer): TListData; override;
      { SetValues is now performing a check if Value is assigned before access }
      procedure SetValues(idx: Integer; AValue: TListData); override;
      { SetArrayCount is attempting to set Value.Count after performing a check if Value is assigned. }
      procedure SetArrayCount(AValue: Integer); override;
      { ValueCreateNew is now performing a check if Value is assigned and the value will automatically be freed }
      procedure ValueCreateNew( AContext: TP3DJSONContext ); virtual;
  end;

  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  { TP3DPropertyAccessRTTI
    -------------------------
    This can be used for most properties that can be published. A published pascal property is accessed using RTTI. This is the only way to
    access enum properties. As a result you can only access published enums.
  }

  TP3DPropertyAccessRTTI   = class( TP3DPropertyAccess )
    private
      FOwner: TObject;
      FPropInfo: PPropInfo;
      FTypeName: String;

      function GetIntValue: string;
      function GetCharValue: string;
      function GetEnumValue: string;
      function GetFloatValue: string;
      function GetStringValue: string;
      function GetSetValue: string;
      function GetClassValue: string;
      function GetStringsValue: string;
      function GetLStringValue: string;
      function GetWCharValue: string;
      function GetVariantValue: string;

      procedure SetIntValue( AValue: String );
      procedure SetCharValue( AValue: String );
      procedure SetEnumValue( AValue: String );
      procedure SetFloatValue( AValue: String );
      procedure SetStringValue( AValue: String );
      procedure SetSetValue( AValue: String );
      procedure SetClassValue( AValue: String );
      procedure SetStringsValue( AValue: String );
      procedure SetLStringValue( AValue: String );
      procedure SetWCharValue( AValue: String );
      procedure SetVariantValue( AValue: String );

    public
      property PropInfo: PPropInfo read FPropInfo;

      function GetNames: TStringList; override;
      procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
      function Serialize: String; override;

    published
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;

      constructor Create( AOwner: TObject; AName: String; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      property TypeName: String read FTypeName;
      property Owner: TObject read FOwner write FOwner;
  end;

  { TP3DPropertyAccessInt
    ------------------------
    This is used to access an integer value.
  }

  TP3DPropertyAccessInt    = class( specialize gP3DPropertyAccess < Integer >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    function GetDefaultValue: Integer; override;
    procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
    procedure LoadFromStream(S: TStream); override;
    procedure SaveToStream(S: TStream); override;
    function Serialize: String; override;
  end;

  { TP3DPropertyAccessFloat
    ------------------------
    This is used to access a float value.
  }

  TP3DPropertyAccessFloat  = class( specialize gP3DPropertyAccess < Float >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    function GetDefaultValue: Float; override;
    procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
    procedure LoadFromStream(S: TStream); override;
    procedure SaveToStream(S: TStream); override;
    function Serialize: String; override;
  end;

  { TP3DPropertyAccessString
    ------------------------
    This is used to access a string value.
  }

  TP3DPropertyAccessString = class( specialize gP3DPropertyAccess < String >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    function GetDefaultValue: String; override;
    class function GetIsArray: Boolean; static; override;
    procedure LoadFromJSONContext(AContext: TP3DJSONContext); override;
    function Serialize: String; override;
  end;

  { gP3DPropertyAccessVec
    ------------------------
    This generic is used to access a vec as an array.
  }

  generic gP3DPropertyAccessVec < TVec, TListData, TPropType > = class( specialize gP3DListPropertyAccess < TVec, TListData, TPropType >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    procedure SetArrayAsString(ArrayIndex: Integer; AValue: String); override;
    function GetDefaultValue: TVec; override;
    procedure SetValues(idx: Integer; AValue: TListData); override;
    class function GetIsArray: Boolean; static; override;
    procedure LoadFromStream(S: TStream); override;
    procedure SaveToStream(S: TStream); override;
    function Serialize: String; override;
  end;

  TP3DPropertyAccessVec2      = specialize gP3DPropertyAccessVec  < TVec2, Float, TP3DPropertyAccessFloat >;
  TP3DPropertyAccessVec3      = specialize gP3DPropertyAccessVec  < TVec3, Float, TP3DPropertyAccessFloat >;
  TP3DPropertyAccessVec4      = specialize gP3DPropertyAccessVec  < TVec4, Float, TP3DPropertyAccessFloat >;
  TP3DPropertyAccessQuat      = specialize gP3DPropertyAccessVec  < TQuaternion, Float, TP3DPropertyAccessFloat >;
  TP3DPropertyAccessIVec2     = specialize gP3DPropertyAccessVec  < TIVec2, Int, TP3DPropertyAccessInt >;
  TP3DPropertyAccessIVec3     = specialize gP3DPropertyAccessVec  < TIVec3, Int, TP3DPropertyAccessInt >;
  TP3DPropertyAccessIVec4     = specialize gP3DPropertyAccessVec  < TIVec4, Int, TP3DPropertyAccessInt >;

  TP3DPropertyAccessVec2List  = specialize gP3DListPropertyAccess < TVec2List, TVec2, TP3DPropertyAccessVec2 >;
  TP3DPropertyAccessVec3List  = specialize gP3DListPropertyAccess < TVec3List, TVec3, TP3DPropertyAccessVec3 >;
  TP3DPropertyAccessVec4List  = specialize gP3DListPropertyAccess < TVec4List, TVec4, TP3DPropertyAccessVec4 >;
  TP3DPropertyAccessFloatList = specialize gP3DListPropertyAccess < TFloatList, Float, TP3DPropertyAccessFloat >;
  TP3DPropertyAccessIntList   = specialize gP3DListPropertyAccess < TIntList, Integer, TP3DPropertyAccessInt >;
  TP3DPropertyAccessIVec2List = specialize gP3DListPropertyAccess < TIVec2List, TIVec2, TP3DPropertyAccessIVec2 >;
  TP3DPropertyAccessIVec3List = specialize gP3DListPropertyAccess < TIVec3List, TIVec3, TP3DPropertyAccessIVec3 >;
  TP3DPropertyAccessIVec4List = specialize gP3DListPropertyAccess < TIVec4List, TIVec4, TP3DPropertyAccessIVec4 >;
  TP3DPropertyAccessQuatList  = specialize gP3DListPropertyAccess < TQuaternionList, TQuaternion, TP3DPropertyAccessQuat >;

  { TP3DPropertyAccessFilePointer
    -----------------------------
    Used to access a filepointer. }

  TP3DPropertyAccessFilePointer = class( specialize gP3DPropertyAccess < TP3DFilePointer >)
    private
      FAbsolutePaths: Boolean;

    public
      procedure LoadFromJSONContext( AContext: TP3DJSONContext ); override;
      function Serialize: String; override;

    published
      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      property AbsolutePaths: Boolean read FAbsolutePaths write FAbsolutePaths;
      function GetDefaultValue: TP3DFilePointer; override;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{$IF FPC_FULLVERSION < 30004}
function StringToJSONString( S: TJSONStringType; B: Boolean ): TJSONStringType;
begin
  Result:= fpjson.StringToJSONString( S );
end;

{$ENDIF}


{ gP3DOIEditorPropertyAccess }

function gP3DPropertyAccess.GetValue: TItem;
begin
  if ( Assigned( Getter )) then
    Result:= Getter()
  else if ( Assigned( Field )) then
    Result:= Field^
  else
    Result:= DefaultValue;
end;

procedure gP3DPropertyAccess.SetValue(AValue: TItem);
begin
  if ( Assigned( Setter )) then
    Setter( AValue )
  else if ( Assigned( Field )) then
    Field^:= AValue;
end;

procedure gP3DPropertyAccess.SaveToStream(S: TStream);
var
  str: String;
  c: Integer;
begin
  str:= AsString;
  c:= Length( str );
  S.Write( c, SizeOf( c ));
  S.Write( str[ 1 ], Length( str ));
end;

procedure gP3DPropertyAccess.LoadFromStream(S: TStream);
var
  c: Integer;
  str: String;
begin
  S.Read( c, SizeOf( Integer ));
  SetLength( str, c );
  S.Read( str[ 1 ], c );
end;

constructor gP3DPropertyAccess.CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Getter:= AGetter;
  Setter:= ASetter;
end;

constructor gP3DPropertyAccess.CreateGetField(AName: String; AGetter: TGetter; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Getter:= AGetter;
  Field:= AField;
end;

constructor gP3DPropertyAccess.CreateFieldSet(AName: String; AField: pItem; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Field:= AField;
  Setter:= ASetter;
end;

constructor gP3DPropertyAccess.CreateField(AName: String; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Field:= AField;
end;


{ TP3DPropertyList }

function TP3DPropertyList.Add(Item: TCustomItem): Integer;
begin
  Result:= inherited Add( Item );
  Item.Context:= Context;
end;

function TP3DPropertyList.Add(Item: TCustomItem; Category: String): Integer;
begin
  Item.Category:= Category;
  Result:= Add( Item );
end;

function TP3DPropertyList.Add( AItems: array of TCustomItem; const Category: String ): Integer;
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

function TP3DPropertyList.GetPropByName(AName: String): TP3DPropertyAccess;
var
  Idx: Integer;
begin
  Idx:= FindByName( AName );
  if ( InRange( Idx, 0, Count - 1 )) then
    Result:= Items[ Idx ]
  else
    Result:= nil;
end;

function Copy2SymbSkipString( var S: String; Ch: Char ): String;
var
  i: Integer;
begin
  i:= 1;
  while i <= Length( S ) do
    begin
      if ( S[ i ] = '"' ) then
        begin
          Inc( i );
          while ( i <= Length( S )) do
            begin
              if ( S[ i ] = '"' ) then
               if (( i = 1 ) or ( S[ i - 1 ] <> '\' )) then
                 break;
              Inc( i );
            end;
        end
      else if ( S[ i ] = Ch ) then
        break;
      Inc( i );
    end;
  Result:= Copy( S, 1, i - 1 );
  S:= Copy( S, i + 1, Length( S ));
end;

function TP3DPropertyList.GetPropByPath( APropPath: String ): TP3DPropertyAccess;

var
  PropName, ArrayStr: String;
  ArrayIdx: LongInt;
  n: SizeInt;
begin
  PropName:= Copy2SymbSkipString( APropPath, '.' );
  n:= Pos( '[', PropName );
  if ( n > 0 ) then
    begin
      ArrayStr:= PropName;
      PropName:= Copy2SymbDel( ArrayStr, '[' );
      ArrayStr:= Copy2Symb( ArrayStr, ']' );
      ArrayIdx:= 0;
    end
  else
    ArrayIdx:= -1;

  Result:= GetPropByName( PropName );
  if ( Assigned( Result ) and ( ArrayIdx > -1 )) then
    begin
      if ( Enclosed( ArrayStr, '"', '"' )) then
        ArrayIdx:= Result.GetArrayIdxByString( ArrayStr )
      else
        ArrayIdx:= StrToIntDef( ArrayStr, -1 );
      if ( ArrayIdx < 0 ) then
        Result:= nil
      else
        Result:= Result.GetArrayAsProp( ArrayIdx );
    end;
  if ( Assigned( Result ) and ( APropPath > '' )) then
    Result:= Result.GetChildPropByPath( APropPath );
end;

procedure TP3DPropertyList.LoadFromJSONContext(AContext: TP3DJSONContext);
var
  Item: TJSONEnum;
  n: Integer;
  KeyS: TJSONStringType;
begin
  if ( AContext.Data.JSONType = jtObject ) then
    for Item in AContext.Data do
      begin
        KeyS:= JSONStringToString( Item.Key );
        if ( KeyS = 'ClassName' ) then
          Continue;
        if ( KeyS = 'DataPath' ) then
          Continue;
        n:= FindByName( KeyS );
        if ( n < 0 ) then
          raise Exception.CreateFmt( 'Failed to load object from JSON Data: The specified property "%s" was not found in the object of type "%s"', [ KeyS, Context.ClassName ])
        //else
        //  WriteLn( 'Property "', KeyS, '" found: ', n );
        ;
        Items[ n ].LoadFromJSONContext( P3DJSONContext( AContext.JSONLoader, Item.Value, AContext.Owner, AContext.Data ));
        if ( Item.Value.JSONType in [ jtArray, jtObject ]) then
          WriteLn( KeyS )
        else
          WriteLn( KeyS, ': ', Item.Value.AsString );
      end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtObject ), JSONTypeName( AContext.Data.JSONType )]);
end;

procedure TP3DPropertyList.Clear(const FreeObjects: Boolean);
begin
  inherited Clear(FreeObjects);
end;

function TP3DPropertyList.GetAsString: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to Count - 1 do
    begin
      Result += Items[ i ].Name + ' : ' + Items[ i ].AsString;
      if ( i < Count - 1 ) then
        Result += ', ';
    end;
end;

function TP3DPropertyList.Serialize: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to Count - 1 do
    begin
      if ( Items[ i ].StoreMode = smDoNotStore ) then
        continue;
      if ( Items[ i ].StoreMode = smNode ) then
        continue;
      try
        if ( Result > '' ) then
          Result += ', ';
        //WriteLn( Items[ i ].Name + ': ' + ClassName + ' < ' + Items[ i ].ClassName + ' >' );
        Result += '"' + StringToJSONString( Items[ i ].Name, False ) + '" : ' + Items[ i ].Serialize;
      except
        on E: Exception do
          WriteLn( ClassName + ' < ' + Items[ i ].ClassName + ' >' );
      end;
    end;
end;

procedure TP3DPropertyList.Sort;
begin

end;

constructor TP3DPropertyList.Create(AContext: TInterfacedPersistent);
begin
  inherited Create;
  FContext:= AContext;
end;

{ TP3DPropertyAccess }

function TP3DPropertyAccess.GetArrayCount: Integer;
begin
  Result:= 0;
end;

procedure TP3DPropertyAccess.SetArrayCount(AValue: Integer);
begin

end;

function TP3DPropertyAccess.GetArrayAsProp( ArrayIndex: Integer ): TP3DPropertyAccess;
begin
  Result:= nil;
end;

function TP3DPropertyAccess.GetArrayAsString( ArrayIndex: Integer ): String;
begin
  Result:= AsString;
end;

class function TP3DPropertyAccess.GetIsArray: Boolean; static;
begin
  Result:= False;
end;

procedure TP3DPropertyAccess.SetArrayAsString( ArrayIndex: Integer ;
  AValue: String);
begin
  AsString:= AValue;
end;

function TP3DPropertyAccess.GetContext: TInterfacedPersistent;
begin
  Result:= FContext;
end;

function TP3DPropertyAccess.GetStoreMode: TP3DPropertyAccessStoreMode;
begin
  Result:= FStoreMode;
end;

procedure TP3DPropertyAccess.SetContext(AValue: TInterfacedPersistent);
begin
  FContext:= AValue;
end;

procedure TP3DPropertyAccess.SetStoreMode(AValue: TP3DPropertyAccessStoreMode);
begin
  FStoreMode:= AValue;
end;

function TP3DPropertyAccess.GetCategory: String;
begin
  Result:= FCategory;
end;

function TP3DPropertyAccess.GetName: String;
begin
  Result:= FName;
end;

procedure TP3DPropertyAccess.SetCategory(AValue: String);
begin
  FCategory:= AValue;
end;

procedure TP3DPropertyAccess.SetName(AValue: String);
begin
  FName:= AValue;
end;

function TP3DPropertyAccess.GetChildPropByPath( APath: String ): TP3DPropertyAccess;
begin
  Result:= nil;
end;

function TP3DPropertyAccess.GetArrayIdxByString(StringIndex: String): Integer;
begin
  Result:= -1;
end;

constructor TP3DPropertyAccess.Create( AName: String; const AStoreMode: TP3DPropertyAccessStoreMode );
begin
  inherited Create;
  FName:= AName;
  StoreMode:= AStoreMode;
end;

function TP3DPropertyAccess.GetNames: TStringList;
begin
  Result:= nil;
end;

function TP3DPropertyAccess.SaveToDOM(AParentNode: TDOMElement): Boolean;
begin
  Result:= False;
  if ( Assigned( AParentNode )) then
    case StoreMode of
      smAttribute: AParentNode.AttribStrings[ Name ]:= AsString;
      else
        exit;
    end;
  Result:= True;
end;

procedure TP3DPropertyAccess.LoadFromDOM( AParentNode: TDOMElement );
begin
  if ( Assigned( AParentNode )) then
    AsString:= AParentNode.AttribStrings[ Name ];
end;

procedure TP3DPropertyAccess.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  raise Exception.CreateFmt( P3DERROR_JSON_PROP_LOADER_NOT_IMPLEMENTED, [ ClassName ]);
end;

{ TP3DPropertyAccessRTTI }

function TP3DPropertyAccessRTTI.GetIntValue: string;
begin
  Result := IntToStr( GetOrdProp( Owner, FPropInfo ));
end;

function TP3DPropertyAccessRTTI.GetCharValue: string;
begin
  Result := Char(GetOrdProp(Owner, FPropInfo));
end;

function TP3DPropertyAccessRTTI.GetEnumValue: string;
begin
  Result := GetEnumName( FPropInfo^.PropType, GetOrdProp(Owner, FPropInfo));
end;

function TP3DPropertyAccessRTTI.GetFloatValue: string;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 19);

begin
  Result := StringReplace(FloatToStrF(GetFloatProp(Owner, FPropInfo), ffGeneral,
    Precisions[ GetTypeData( FPropInfo^.PropType )^.FloatType], 0),
    DecimalSeparator, '.',[rfReplaceAll]);
end;

function TP3DPropertyAccessRTTI.GetStringValue: string;
begin
   Result := GetStrProp(Owner, FPropInfo);
end;

function TP3DPropertyAccessRTTI.GetSetValue: string;
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I: Integer;
begin
  Result := '[';
  W := GetOrdProp(Owner, FPropInfo);
  TypeInfo := GetTypeData( FPropInfo^.PropType )^.CompType;
  for I := 0 to SizeOf( TCardinalSet ) * 8 - 1 do
    if I in TCardinalSet(W) then begin
      if Length(Result) <> 1 then Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  Result := Result + ']';
end;

function TP3DPropertyAccessRTTI.GetClassValue: string;
begin
  //TODO: Implement GetClassValue
end;

function TP3DPropertyAccessRTTI.GetStringsValue: string;
var
  List: TObject;
  I: Integer;
  SectName: string;
begin
  Result := '';
  {List := TObject(GetObjectProp(Owner.Obj, FPropInfo));
  SectName := Format('%s.%s', [Section, GetItemName(FPropInfo^.Name)]);
  EraseSection(SectName);
  if (List is TStrings)
     and ((TStrings(List).Count > 0) or (psoAlwaysStoreStringsCount in Options)) then
    begin
    WriteString(SectName, sCount, IntToStr(TStrings(List).Count));
    for I := 0 to TStrings(List).Count - 1 do
      WriteString(SectName, Format(sItem, [I]), TStrings(List)[I]);
    end;}
end;


function TP3DPropertyAccessRTTI.GetLStringValue: string;
begin
  Result := GetStrProp(Owner, FPropInfo);
end;

function TP3DPropertyAccessRTTI.GetWCharValue: string;
begin
  Result := Char(GetOrdProp(Owner, FPropInfo));
end;

function TP3DPropertyAccessRTTI.GetVariantValue: string;
begin
  Result := GetVariantProp(Owner, FPropInfo);
end;

procedure TP3DPropertyAccessRTTI.SetIntValue(AValue: String);
begin
  SetOrdProp( Owner, FPropInfo, StrToIntDef( AValue, 0 ));
end;

procedure TP3DPropertyAccessRTTI.SetCharValue(AValue: String);
begin
  SetOrdProp( Owner, FPropInfo, Integer( AValue[ 1 ]));
end;

procedure TP3DPropertyAccessRTTI.SetEnumValue(AValue: String);
var
  I: Integer;
  EnumType: PTypeInfo;
begin
  EnumType := FPropInfo^.PropType;
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do
      if ( CompareText( GetEnumName( EnumType, I ), AValue ) = 0 ) then
      begin
        SetOrdProp( Owner, FPropInfo, I );
        Exit;
      end;
end;

procedure TP3DPropertyAccessRTTI.SetFloatValue(AValue: String);
begin
  SetFloatProp(Owner, FPropInfo, StrToFloat(StringReplace( AValue, '.',
    DecimalSeparator,[ rfReplaceAll ])));
end;

procedure TP3DPropertyAccessRTTI.SetStringValue(AValue: String);
begin
  SetStrProp( Owner, FPropInfo, AValue );
end;


procedure TP3DPropertyAccessRTTI.SetSetValue(AValue: String);
const
  Delims = [' ', ',', '[', ']'];
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I, N: Integer;
  Count: Integer;
  EnumName: string;
begin
  W := 0;
  TypeInfo := GetTypeData( FPropInfo^.PropType )^.CompType;
  Count := WordCount( AValue, Delims );
  for N := 1 to Count do begin
    EnumName := ExtractWord( N, AValue, Delims );
    try
      I := TypInfo.GetEnumValue( TypeInfo, EnumName );
      if ( I >= 0 ) then Include( TCardinalSet( W ), I );
    except
    end;
  end;
  SetOrdProp( Owner, FPropInfo, W );
end;

procedure TP3DPropertyAccessRTTI.SetClassValue(AValue: String);
begin

end;

procedure TP3DPropertyAccessRTTI.SetStringsValue(AValue: String);
begin

end;

procedure TP3DPropertyAccessRTTI.SetLStringValue(AValue: String);
begin
  SetStrProp( Owner, FPropInfo, AValue );
end;

procedure TP3DPropertyAccessRTTI.SetWCharValue(AValue: String);
begin
  SetOrdProp( Owner, FPropInfo, Longint( AValue[ 1 ]));
end;

procedure TP3DPropertyAccessRTTI.SetVariantValue(AValue: String);
begin
  SetVariantProp( Owner, FPropInfo, AValue );
end;

function TP3DPropertyAccessRTTI.GetNames: TStringList;
var
  i: Integer;
begin
  if ( FPropInfo^.PropType^.Kind in [ tkEnumeration, tkBool ]) then
    begin
      Result:= TStringList.Create;
      for i:= 0 to GetEnumNameCount( FPropInfo^.PropType ) - 1 do
        Result.Add( GetEnumName( FPropInfo^.PropType, i ));
    end
  else
    Result:= nil;
end;

procedure TP3DPropertyAccessRTTI.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  if ( Assigned( FPropInfo )) then
    case FPropInfo^.PropType^.Kind of
      tkInteger: SetIntValue( AContext.Data.AsString );
      tkChar: SetCharValue( AContext.Data.AsString );
      tkEnumeration: SetEnumValue( AContext.Data.AsString );
      tkFloat: SetFloatValue( AContext.Data.AsString );
      tkAstring: SetStringValue( AContext.Data.AsString );
      tkWString: SetStringValue( AContext.Data.AsString );
      tkWChar: SetWCharValue( AContext.Data.AsString );
      tkVariant: SetVariantValue( AContext.Data.AsString );
      tkInt64: SetIntValue( AContext.Data.AsString );
      tkString: SetStringValue( AContext.Data.AsString );
      tkSet: SetSetValue( AContext.Data.AsString );
      tkClass: SetClassValue( AContext.Data.AsString );
      tkBool: SetIntValue( AContext.Data.AsString );
    else
      Exit;
    end;
end;

function TP3DPropertyAccessRTTI.Serialize: String;
begin
  if ( Assigned( FPropInfo )) then
    if ( FPropInfo^.PropType^.Kind in [ tkAString, tkWString, tkWChar, tkString, tkEnumeration ]) then
      Result:= '"' + StringToJSONString( AsString, True ) + '"'
    else
      Result:= AsString;
end;

function TP3DPropertyAccessRTTI.GetAsString: String;
begin
  if ( Assigned( FPropInfo )) then
    case FPropInfo^.PropType^.Kind of
      tkInteger: Result := GetIntValue;
      tkChar: Result := GetCharValue;
      tkEnumeration: Result := GetEnumValue;
      tkFloat: Result:= GetFloatValue;
      tkAstring: Result := GetStringValue;
      tkWString: Result := GetStringValue;
      tkWChar: Result := GetWCharValue;
      tkVariant: Result := GetVariantValue;
      tkInt64: Result:= GetIntValue;
      tkString: Result:= GetStringValue;
      tkSet: Result:= GetSetValue;
      tkClass: Result:= GetClassValue;
      tkBool: Result:= GetIntValue
    else
      Exit;
    end;
end;

procedure TP3DPropertyAccessRTTI.SetAsString(AValue: String);
begin
  if ( Assigned( FPropInfo )) then
    case FPropInfo^.PropType^.Kind of
      tkInteger: SetIntValue( AValue );
      tkChar: SetCharValue( AValue );
      tkEnumeration: SetEnumValue( AValue );
      tkFloat: SetFloatValue( AValue );
      tkAstring: SetStringValue( AValue );
      tkWString: SetStringValue( AValue );
      tkWChar: SetWCharValue( AValue );
      tkVariant: SetVariantValue( AValue );
      tkInt64: SetIntValue( AValue );
      tkString: SetStringValue( AValue );
      tkSet: SetSetValue( AValue );
      tkClass: SetClassValue( AValue );
      tkBool: SetIntValue( AValue )
    else
      Exit;
    end;
end;

constructor TP3DPropertyAccessRTTI.Create(AOwner: TObject; AName: String; const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Owner:= AOwner;
  FPropInfo:= GetPropInfo( Owner, Name );
  if ( not Assigned( FPropInfo )) then
    raise Exception.Create( 'Cannot create access: The specified property was not found: ' + AName );
  FTypeName:= PropInfo^.PropType^.Name;
end;


{ TP3DPropertyAccessString }

function TP3DPropertyAccessString.GetAsString: String;
begin
  Result:= Value;
end;

procedure TP3DPropertyAccessString.SetAsString(AValue: String);
begin
  Value:= AValue;
end;

function TP3DPropertyAccessString.GetDefaultValue: String;
begin
  Result:= '';
end;

class function TP3DPropertyAccessString.GetIsArray: Boolean;
begin
  Result:= True;
end;

procedure TP3DPropertyAccessString.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  if ( AContext.Data.JSONType = jtString ) then
    Value:= AContext.Data.AsString
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtString ), JSONTypeName( AContext.Data.JSONType )]);
end;

function TP3DPropertyAccessString.Serialize: String;
begin
  Result:= '"' + StringToJSONString( AsString, True ) + '"';
end;

{ TP3DPropertyAccessFloat }

function TP3DPropertyAccessFloat.GetAsString: String;
begin
  Result:= FormatFloat( '#.##', Value );
end;

procedure TP3DPropertyAccessFloat.SetAsString(AValue: String);
begin
  Value:= StrToFloat( AValue );
end;

function TP3DPropertyAccessFloat.GetDefaultValue: Float;
begin
  Result:= 0.0;
end;

procedure TP3DPropertyAccessFloat.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  if ( AContext.Data.JSONType = jtNumber ) then
    Value:= AContext.Data.AsFloat
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtNumber ), JSONTypeName( AContext.Data.JSONType )]);
end;

procedure TP3DPropertyAccessFloat.LoadFromStream(S: TStream);
begin
  inherited LoadFromStream(S);
end;

procedure TP3DPropertyAccessFloat.SaveToStream(S: TStream);
begin
  inherited SaveToStream(S);
end;

function TP3DPropertyAccessFloat.Serialize: String;
begin
  Result:= AsString;
end;

{ TP3DPropertyAccessInt }

function TP3DPropertyAccessInt.GetAsString: String;
begin
  Result:= IntToStr( Value );
end;

procedure TP3DPropertyAccessInt.SetAsString(AValue: String);
begin
  Value:= StrToInt( AValue );
end;

function TP3DPropertyAccessInt.GetDefaultValue: Integer;
begin
  Result:= 0;
end;

procedure TP3DPropertyAccessInt.LoadFromJSONContext(AContext: TP3DJSONContext);
begin
  inherited LoadFromJSONContext(AContext);
end;

procedure TP3DPropertyAccessInt.LoadFromStream(S: TStream);
begin
  inherited LoadFromStream(S);
end;

procedure TP3DPropertyAccessInt.SaveToStream(S: TStream);
begin
  inherited SaveToStream(S);
end;

function TP3DPropertyAccessInt.Serialize: String;
begin
  Result:= AsString;
end;


{ gP3DPropertyAccessVec }

function gP3DPropertyAccessVec.GetAsString: String;
begin
  Result:= Value.ToString;
end;

procedure gP3DPropertyAccessVec.SetAsString(AValue: String);
var
  Val: TVec;
begin
  Val.FromString( AValue );
  Value:= Val;
end;

procedure gP3DPropertyAccessVec.SetArrayAsString(ArrayIndex: Integer; AValue: String);
var
  V: TListData;
begin
  if ( InRange( ArrayIndex, 0, Value.Count - 1 )) then
    try
      V.Parse( AValue );
      Value[ ArrayIndex ]:= V;
    except
      On E: Exception do
        WriteLn( 'Error while setting property ' + Name + '[' + IntToStr( ArrayIndex ) + ']: "' + AValue + '" is not a valid integer!' );
    end;
end;

function gP3DPropertyAccessVec.GetDefaultValue: TVec;
var
  i: Integer;
begin
  for i:= 0 to Result.Count -1 do
    Result[ i ]:= 0;
end;

procedure gP3DPropertyAccessVec.SetValues(idx: Integer; AValue: TListData);
var
  Val: TItem;
begin
  Val:= Value;
  Val[ idx ]:= AValue;
  Value:= Val;
end;

class function gP3DPropertyAccessVec.GetIsArray: Boolean;
begin
  Result:= True;
end;

procedure gP3DPropertyAccessVec.LoadFromStream(S: TStream);
begin
  S.ReadBuffer( Value.Ptr^, SizeOf( TVec ));
end;

procedure gP3DPropertyAccessVec.SaveToStream(S: TStream);
begin
  S.WriteBuffer( Value.Ptr^, SizeOf( TVec ));
end;

function gP3DPropertyAccessVec.Serialize: String;
begin
  Result:= '[' + AsString + ']';
end;

{ gP3DListPropertyAccess }

function gP3DListPropertyAccess.GetArrayCount: Integer;
begin
  Result:= Value.Count
end;

function gP3DListPropertyAccess.GetValues(idx: Integer): TListData;
begin
  Result:= Value[ idx ]
end;

procedure gP3DListPropertyAccess.SetArrayCount(AValue: Integer);
begin

end;

procedure gP3DListPropertyAccess.SetValues(idx: Integer; AValue: TListData);
begin
  Value[ idx ]:= AValue;
end;

function gP3DListPropertyAccess.GetFakeProp: TListData;
begin
  Result:= Values[ FFakePropIdx ];
end;

procedure gP3DListPropertyAccess.SetFakeProp(AValue: TListData);
begin
  Values[ FFakePropIdx ]:= AValue;
end;

function gP3DListPropertyAccess.GetArrayIdxByString(StringIndex: String): Integer;
var
  i: Integer;
  Prop, NameProp: TP3DPropertyAccess;
begin
  Result:= -1;
  for i:= 0 to ArrayCount -1 do
    begin
      Prop:= GetArrayAsProp( i );
      NameProp:= Prop.GetChildPropByPath( 'Name' );
      if ( Assigned( NameProp ) and ( NameProp.AsString = StringIndex )) then
        begin
          Result:= i;
          break;
        end;
    end;
end;

class function gP3DListPropertyAccess.GetIsArray: Boolean;
begin
  Result:= True;
end;

function gP3DListPropertyAccess.GetArrayAsProp( ArrayIndex: Integer ): TP3DPropertyAccess;
var
  FakePropName: String;
begin
  FakePropName:= Name + '[' + IntToStr( ArrayIndex ) + ']';
  FFakePropIdx:= ArrayIndex;
  //WriteLn( FakePropName, ':', FFakePropIdx );
  if ( not Assigned( FFakeProp )) then
    FFakeProp:= TPropType.CreateGetSet( FakePropName, @GetFakeProp, @SetFakeProp, smDoNotStore )
  else
    FFakeProp.Name:= FakePropName;
  Result:= FFakeProp;
end;

destructor gP3DListPropertyAccess.Destroy;
begin
  if ( Assigned( FFakeProp )) then
    FFakeProp.Free;
  inherited Destroy;
end;

function gP3DListPropertyAccess.SaveToDOM(AParentNode: TDOMElement): Boolean;
var
  Node: TDOMElement;
  i: Integer;
  Prop: TP3DPropertyAccess;
begin
  Result:= inherited SaveToDOM( AParentNode );
  case StoreMode of
    smNode:
      begin
        Node:= AParentNode.OwnerDocument.CreateElement( Name );
        AParentNode.AppendChild( Node );

        for i:= 0 to ArrayCount - 1 do
          begin
            Prop:= GetArrayAsProp( i );
            Prop.SaveToDOM( Node );
          end;
      end;
  end;
end;

procedure gP3DListPropertyAccess.SaveValueToDOM(AIndex: Integer; ADOMNode: TDOMElement);
begin
{  Val:= Values[ AIndex ];
  if ( Assigned( Val )) then
    Val.SaveToDOM( Node )
  else
    AParentNode.AppendChild( AParentNode.OwnerDocument.CreateElement( 'empty' ));}
end;

procedure gP3DListPropertyAccess.LoadFromJSONContext(AContext: TP3DJSONContext);
var
  i: Integer;
  S: TJSONStringType;
begin
  if ( AContext.Data.JSONType = jtArray ) then
    begin
      SetArrayCount( AContext.Data.Count );
      for i:= 0 to min( AContext.Data.Count, GetArrayCount ) - 1 do
        LoadItemFromJSONContext( i, P3DJSONContext( AContext.JSONLoader, AContext.Data.Items[ i ], AContext.Owner, AContext.Data ));
    end
  else if ( AContext.Data.JSONType = jtString ) then
    begin
      S:= AContext.Data.AsString;
      if ( S[ 1 ] = '@' ) then
        AContext.JSONLoader.BinaryFile.Stream.Position:= StrToInt( Copy( S, 2, Length( S ) - 1 ))
      else
        exit;
      LoadFromStream( AContext.JSONLoader.BinaryFile.Stream )
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtArray ), JSONTypeName( AContext.Data.JSONType )]);
end;

procedure gP3DListPropertyAccess.LoadItemFromJSONContext(AIndex: Integer; AContext: TP3DJSONContext);
var
  Prop: TP3DPropertyAccess;
begin
  Prop:= GetArrayAsProp( AIndex );
  Prop.LoadFromJSONContext( AContext );
end;

function TypeKindToStr( Tp: TTypeKind ): String;
begin
  case Tp of
    tkUnknown: Result:= 'Unknown';
    tkInteger: Result:= 'Integer';
    tkChar: Result:= 'Char';
    tkEnumeration: Result:= 'Enumeration';
    tkFloat: Result:= 'Float';
    tkSet: Result:= 'Set';
    tkMethod: Result:= 'Method';
    tkSString: Result:= 'SString';
    tkLString: Result:= 'LString';
    tkAString: Result:= 'AString';
    tkWString: Result:= 'WString';
    tkVariant: Result:= 'Variant';
    tkArray: Result:= 'Array';
    tkRecord: Result:= 'Record';
    tkInterface: Result:= 'Interface';
    tkClass: Result:= 'Class';
    tkObject: Result:= 'Object';
    tkWChar: Result:= 'WChar';
    tkBool: Result:= 'Bool';
    tkInt64: Result:= 'Int64';
    tkQWord: Result:= 'QWord';
    tkDynArray: Result:= 'DynArray';
    tkInterfaceRaw: Result:= 'InterfaceRaw';
    tkProcVar: Result:= 'ProcVar';
    tkUString: Result:= 'UString';
    tkUChar: Result:= 'UChar';
    tkHelper: Result:= 'Helper';
    tkFile: Result:= 'File';
    tkClassRef: Result:= 'ClassRef';
    tkPointer: Result:= 'Pointer';
  end;
end;

procedure gP3DListPropertyAccess.LoadFromStream(S: TStream);
begin
  Value.LoadFromStream( S );
end;

procedure gP3DListPropertyAccess.SaveToStream(S: TStream);
begin
  //WriteLn( '-> ', Value.ClassName );
  Value.SaveToStream( S );
end;

function gP3DListPropertyAccess.Serialize: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to ArrayCount - 1 do
    Result+= ', ' + GetArrayAsProp( i ).Serialize;
  Result:= '[' + Copy( Result, 3, Length( Result ) - 2 ) + ']';
end;

function gP3DListPropertyAccess.GetAsString: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to ArrayCount - 1 do
    Result+= ', ' + GetArrayAsProp( i ).AsString;
  Result:= '[' + Copy( Result, 3, Length( Result ) - 2 ) + ']';
end;

procedure gP3DListPropertyAccess.SetAsString(AValue: String);
begin

end;

{ gP3DListPropertyAccessObject }

function gP3DListPropertyAccessObject.GetArrayCount: Integer;
begin
  if ( Assigned( Value )) then
    Result:= inherited GetArrayCount
  else
    Result:= 0;
end;

function gP3DListPropertyAccessObject.GetValues(idx: Integer): TListData;
begin
  if ( Assigned( Value )) then
    Result:= TListData( Value.Items[ idx ])
  else
    Result:= Default( TListData );
end;

procedure gP3DListPropertyAccessObject.SetArrayCount(AValue: Integer);
begin
  if ( Assigned( Value )) then
    Value.Count:= AValue;
end;

procedure gP3DListPropertyAccessObject.SetValues(idx: Integer; AValue: TListData);
begin
  if ( Assigned( Value )) then
    Value.Items[ idx ]:= AValue;
end;

procedure gP3DListPropertyAccessObject.ValueCreateNew(AContext: TP3DJSONContext);
begin
  Value:= TItem.Create;
end;


{$ENDIF}

{$IFDEF INITIALIZATION}

{$ENDIF}

{$IFDEF FINALIZATION}
{$ENDIF}
