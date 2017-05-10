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

  TP3DPropertyAccess = class abstract ( TInterfacedPersistent, IP3DPropertyAccess )
    protected
      FContext: TInterfacedPersistent;
      FStoreMode: TP3DPropertyAccessStoreMode;
      FCategory: String;
      FName: String;

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
      function GetChildPropByPath( APath: String ): TP3DPropertyAccess; virtual;

      constructor Create( AName: String; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      function GetNames: TStringList; virtual; //can be used for enums
      function SaveToDOM( AParentNode: TDOMElement ): Boolean; virtual;
      procedure LoadFromDOM( AParentNode: TDOMElement ); virtual;
      property ArrayAsProp[ ArrayIndex: Integer ]: TP3DPropertyAccess read GetArrayAsProp;
      property ArrayAsString[ ArrayIndex: Integer ]: String read GetArrayAsString write SetArrayAsString;

      class property IsArray: Boolean read GetIsArray;

    published
      property Name: String read GetName write SetName;
      property Category: String read GetCategory write SetCategory;
      property AsString: String read GetAsString write SetAsString;
      property StoreMode: TP3DPropertyAccessStoreMode read GetStoreMode write SetStoreMode;
      property Context: TInterfacedPersistent read GetContext write SetContext;
  end;

  { TP3DPropertyAccessList }

  TP3DPropertyAccessList = class ( specialize gP3DCustomObjectList < TP3DPropertyAccess >)
    private
      FContext: TInterfacedPersistent;

    published
      function Add( Item: TCustomItem ): Integer; override;
      function Add( Item: TCustomItem; Category: String ): Integer; inline;
      function Add( AItems: array of TCustomItem; const Category: String = '' ): Integer;
      function FindByName( AName: String ): Integer;
      function GetPropByName( AName: String ): TP3DPropertyAccess;
      function GetPropByPath( APropPath: String ): TP3DPropertyAccess; virtual;
      procedure Sort;

      constructor Create( AContext: TInterfacedPersistent );
      property Context: TInterfacedPersistent read FContext write FContext;
  end;

  { gP3DPropertyAccess }

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

    public
      constructor CreateGetSet( AName:String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      constructor CreateGetField( AName:String; AGetter: TGetter; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      constructor CreateFieldSet( AName:String; AField: pItem; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      constructor CreateField( AName:String; AField: pItem; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );

      property Value: TItem read GetValue write SetValue;
      property DefaultValue: TItem read GetDefaultValue;

      property Getter: TGetter read FGetter write FGetter;
      property Setter: TSetter read FSetter write FSetter;
      property Field: pItem read FField write FField;
  end;


  { gP3DListPropertyAccess }

  generic gP3DListPropertyAccess < TListProp, TListData, TPropType > = class( specialize gP3DPropertyAccess < TListProp >)
    private
      FFakeProp: TPropType;
      FFakePropIdx: Integer;

      function GetCount: Integer; virtual;
      function GetValues( idx: Integer ): TListData; virtual;
      procedure SetCount( AValue: Integer ); virtual;
      procedure SetValues( idx: Integer ; AValue: TListData ); virtual;
      function GetFakeProp: TListData;
      procedure SetFakeProp( AValue: TListData );
      function GetArrayIdxByString(StringIndex: String): Integer; override;

    public
      function GetArrayAsProp(ArrayIndex: Integer): TP3DPropertyAccess; override;
      property Values[ idx: Integer ]: TListData read GetValues write SetValues;

      destructor Destroy; override;

      function SaveToDOM( AParentNode: TDOMElement ): Boolean; override;
      procedure SaveValueToDOM( AIndex: Integer; ADOMNode: TDOMElement ); virtual;

    published
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      property Count: Integer read GetCount write SetCount;
  end;

  { gP3DListPropertyAccessObject }

  generic gP3DListPropertyAccessObject < TListProp, TListData, TPropType > = class( specialize gP3DListPropertyAccess < TListProp, TListData, TPropType >)
    private
      function GetCount: Integer; override;
      function GetValues(idx: Integer): TListData; override;
      procedure SetCount(AValue: Integer); override;
      procedure SetValues(idx: Integer; AValue: TListData); override;


  end;

  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  { TP3DPropertyAccessRTTI }

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

    published
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;

      constructor Create( AOwner: TObject; AName: String; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore );
      property TypeName: String read FTypeName;
      property Owner: TObject read FOwner write FOwner;
  end;

  { TP3DPropertyAccessInt }

  TP3DPropertyAccessInt    = class( specialize gP3DPropertyAccess < Integer >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    function GetDefaultValue: Integer; override;
  end;

  { TP3DPropertyAccessFloat }

  TP3DPropertyAccessFloat  = class( specialize gP3DPropertyAccess < Float >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    function GetDefaultValue: Float; override;
  end;

  { TP3DPropertyAccessString }

  TP3DPropertyAccessString = class( specialize gP3DPropertyAccess < String >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    function GetDefaultValue: String; override;
    class function GetIsArray: Boolean; static; override;
  end;

  { gP3DPropertyAccessVec }

  generic gP3DPropertyAccessVec < TVec > = class( specialize gP3DListPropertyAccess < TVec, Float, TP3DPropertyAccessFloat >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    procedure SetArrayAsString(ArrayIndex: Integer; AValue: String); override;
    function GetDefaultValue: TVec; override;
    class function GetIsArray: Boolean; static; override;
  end;

  { TP3DPropertyAccessVec2 }

  TP3DPropertyAccessVec2   = specialize gP3DPropertyAccessVec < TVec2 >;
  TP3DPropertyAccessVec3   = specialize gP3DPropertyAccessVec < TVec3 >;
  TP3DPropertyAccessVec4   = specialize gP3DPropertyAccessVec < TVec4 >;
  TP3DPropertyAccessQuat   = specialize gP3DPropertyAccessVec < TQuaternion >;

{  { TP3DPropertyAccessVec3 }

  TP3DPropertyAccessVec3   = class( specialize gP3DListPropertyAccess < TVec3, Float, TP3DPropertyAccessFloat >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    procedure SetArrayAsString(ArrayIndex: Integer; AValue: String); override;
    function GetDefaultValue: TVec3; override;
    class function GetIsArray: Boolean; static; override;
  end;

  { TP3DPropertyAccessVec4 }

  TP3DPropertyAccessVec4   = class( specialize gP3DListPropertyAccess < TVec4, Float, TP3DPropertyAccessFloat >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    procedure SetArrayAsString(ArrayIndex: Integer; AValue: String); override;
    function GetDefaultValue: TVec4; override;
    class function GetIsArray: Boolean; static; override;
  end;

  { TP3DPropertyAccessQuat }

  TP3DPropertyAccessQuat   = class( specialize gP3DListPropertyAccess < TQuaternion, Float, TP3DPropertyAccessFloat >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
    procedure SetArrayAsString(ArrayIndex: Integer; AValue: String); override;
    function GetDefaultValue: TQuaternion; override;
    class function GetIsArray: Boolean; static; override;
  end;}

{$ENDIF}

{$IFDEF IMPLEMENTATION}

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

{ TP3DPropertyAccessList }

function TP3DPropertyAccessList.Add(Item: TCustomItem): Integer;
begin
  Result:= inherited Add( Item );
  Item.Context:= Context;
end;

function TP3DPropertyAccessList.Add(Item: TCustomItem; Category: String): Integer;
begin
  Item.Category:= Category;
  Add( Item );
end;

function TP3DPropertyAccessList.Add( AItems: array of TCustomItem; const Category: String ): Integer;
var
  i: Integer;
begin
  for i:= low( AItems ) to high( AItems ) do
    Add( AItems[ i ], Category );
end;

function TP3DPropertyAccessList.FindByName(AName: String): Integer;
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

function TP3DPropertyAccessList.GetPropByName(AName: String): TP3DPropertyAccess;
var
  Idx: Integer;
begin
  Idx:= FindByName( AName );
  if ( InRange( Idx, 0, Count - 1 )) then
    Result:= Items[ Idx ]
  else
    Result:= nil;
end;

function TP3DPropertyAccessList.GetPropByPath( APropPath: String ): TP3DPropertyAccess;
var
  PropName, ArrayStr: String;
  ArrayIdx: LongInt;
  n: SizeInt;
begin
  PropName:= Copy2SymbDel( APropPath, '.' );
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

procedure TP3DPropertyAccessList.Sort;
begin

end;

constructor TP3DPropertyAccessList.Create(AContext: TInterfacedPersistent);
begin
  inherited Create;
  FContext:= AContext;
end;

{ TP3DPropertyAccess }

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
begin
  inherited SetArrayAsString(ArrayIndex, AValue);
end;

function gP3DPropertyAccessVec.GetDefaultValue: TVec;
var
  i: Integer;
begin
  for i:= 0 to Result.Count -1 do
    Result[ i ]:= 0;
end;

class function gP3DPropertyAccessVec.GetIsArray: Boolean;
begin
  Result:=inherited GetIsArray;
end;

{ gP3DListPropertyAccess }

function gP3DListPropertyAccess.GetCount: Integer;
begin
  Result:= Value.Count;
end;

function gP3DListPropertyAccess.GetValues(idx: Integer): TListData;
begin
  Result:= Value[ idx ]
end;

procedure gP3DListPropertyAccess.SetCount(AValue: Integer);
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
  for i:= 0 to Count -1 do
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

function gP3DListPropertyAccess.GetArrayAsProp( ArrayIndex: Integer ): TP3DPropertyAccess;
var
  FakePropName: String;
begin
  FakePropName:= Name + '[' + IntToStr( ArrayIndex ) + ']';
  FFakePropIdx:= ArrayIndex;
  WriteLn( FakePropName, ':', FFakePropIdx );
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

        for i:= 0 to Count - 1 do
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

function gP3DListPropertyAccess.GetAsString: String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 0 to Count - 1 do
    Result+= ', ' + Values[ i ].ToString;
  Result:= '[' + Copy( Result, 3, Length( Result ) - 2 ) + ']';
end;

procedure gP3DListPropertyAccess.SetAsString(AValue: String);
begin

end;


{$ENDIF}
