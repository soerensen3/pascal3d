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

  TP3DPropertyAccess = class abstract ( TPersistent )
    protected
      FCategory: String;
      FName: String;

      function GetAsString: String; virtual; abstract;
      procedure SetAsString(AValue: String); virtual; abstract;

    public
      constructor Create( AName: String );
      function GetNames: TStringList; virtual; //can be used for enums

    published
      property Name: String read FName write FName;
      property Category: String read FCategory write FCategory;
      property AsString: String read GetAsString write SetAsString;
  end;

  { TP3DPropertyAccessList }

  TP3DPropertyAccessList = class ( specialize gP3DCustomObjectList < TP3DPropertyAccess >)
    published
      function FindByName( AName: String ): Integer;
      function GetPropByName( AName: String ): TP3DPropertyAccess;
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
      function GetValue: TItem;
      procedure SetValue( AValue: TItem );

    public
      constructor CreateGetSet( AName:String; AGetter: TGetter; ASetter: TSetter );
      constructor CreateGetField( AName:String; AGetter: TGetter; AField: pItem );
      constructor CreateFieldSet( AName:String; AField: pItem; ASetter: TSetter );
      constructor CreateField( AName:String; AField: pItem );

      property Value: TItem read GetValue write SetValue;

      property Getter: TGetter read FGetter write FGetter;
      property Setter: TSetter read FSetter write FSetter;
      property Field: pItem read FField write FField;
  end;


  { gP3DListPropertyAccess }

  generic gP3DListPropertyAccess < TListProp, TListData > = class( specialize gP3DPropertyAccess < TListProp >)
    private
      function GetCount: Integer;
      function GetValues( idx: Integer ): TListData;
      procedure SetCount( AValue: Integer );
      procedure SetValues( idx: Integer ; AValue: TListData );

    public
      property Values[ idx: Integer ]: TListData read GetValues write SetValues;

    published
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;
      property Count: Integer read GetCount write SetCount;
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
      procedure SetAsString(AValue: String); override;

      constructor Create( AOwner: TObject; AName: String);
      property TypeName: String read FTypeName;
      property Owner: TObject read FOwner write FOwner;
  end;

  { TP3DPropertyAccessInt }

  TP3DPropertyAccessInt    = class( specialize gP3DPropertyAccess < Integer >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
  end;

  { TP3DPropertyAccessFloat }

  TP3DPropertyAccessFloat  = class( specialize gP3DPropertyAccess < Float >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
  end;

  { TP3DPropertyAccessString }

  TP3DPropertyAccessString = class( specialize gP3DPropertyAccess < String >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
  end;

  { TP3DPropertyAccessVec2 }

  TP3DPropertyAccessVec2   = class( specialize gP3DPropertyAccess < TVec2 >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
  end;

  { TP3DPropertyAccessVec3 }

  TP3DPropertyAccessVec3   = class( specialize gP3DPropertyAccess < TVec3 >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
  end;

  { TP3DPropertyAccessVec4 }

  TP3DPropertyAccessVec4   = class( specialize gP3DPropertyAccess < TVec4 >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
  end;

  { TP3DPropertyAccessQuat }

  TP3DPropertyAccessQuat   = class( specialize gP3DPropertyAccess < TQuaternion >)
    function GetAsString: String; override;
    procedure SetAsString(AValue: String); override;
  end;

  { TP3DInterfacedPersistent }

  TP3DInterfacedPersistent = class ( TInterfacedPersistent )
    private
      FProperties: TP3DPropertyAccessList;

    public
      constructor Create;
      destructor Destroy; override;

      property Properties: TP3DPropertyAccessList read FProperties write FProperties;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ gP3DOIEditorPropertyAccess }

function gP3DPropertyAccess.GetValue: TItem;
begin
  if ( Assigned( Getter )) then
    Result:= Getter()
  else if ( Assigned( Field )) then
    Result:= Field^;
end;

procedure gP3DPropertyAccess.SetValue(AValue: TItem);
begin
  if ( Assigned( Setter )) then
    Setter( AValue )
  else if ( Assigned( Field )) then
    Field^:= AValue;
end;

constructor gP3DPropertyAccess.CreateGetSet( AName: String; AGetter: TGetter; ASetter: TSetter);
begin
  inherited Create( AName );
  Getter:= AGetter;
  Setter:= ASetter;
end;

constructor gP3DPropertyAccess.CreateGetField( AName: String; AGetter: TGetter; AField: pItem);
begin
  inherited Create( AName );
  Getter:= AGetter;
  Field:= AField;
end;

constructor gP3DPropertyAccess.CreateFieldSet( AName: String; AField: pItem; ASetter: TSetter);
begin
  inherited Create( AName );
  Field:= AField;
  Setter:= ASetter;
end;

constructor gP3DPropertyAccess.CreateField( AName: String; AField: pItem);
begin
  inherited Create( AName );
  Field:= AField;
end;

{ TP3DPropertyAccessList }


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

{ TP3DPropertyAccess }

constructor TP3DPropertyAccess.Create(AName: String);
begin
  inherited Create;
  FName:= AName;
end;

function TP3DPropertyAccess.GetNames: TStringList;
begin
  Result:= nil;
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

constructor TP3DPropertyAccessRTTI.Create(AOwner: TObject; AName: String);
begin
  inherited Create( AName );
  Owner:= AOwner;
  FPropInfo:= GetPropInfo( Owner, Name );
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

{ TP3DPropertyAccessFloat }

function TP3DPropertyAccessFloat.GetAsString: String;
begin
  Result:= FormatFloat( '#.##', Value );
end;

procedure TP3DPropertyAccessFloat.SetAsString(AValue: String);
begin
  Value:= StrToFloat( AValue );
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

{ TP3DPropertyAccessVec2 }

function TP3DPropertyAccessVec2.GetAsString: String;
begin
  Result:= Value.ToString();
end;

procedure TP3DPropertyAccessVec2.SetAsString(AValue: String);
var
  Val: TVec2;
begin
  Val.FromString( AValue );
  Value:= Val;
end;

{ TP3DPropertyAccessVec3 }

function TP3DPropertyAccessVec3.GetAsString: String;
begin
  Result:= Value.ToString();
end;

procedure TP3DPropertyAccessVec3.SetAsString(AValue: String);
var
  Val: TVec3;
begin
  Val.FromString( AValue );
  Value:= Val;
end;

{ TP3DPropertyAccessVec4 }

function TP3DPropertyAccessVec4.GetAsString: String;
begin
  Result:= Value.ToString();
end;

procedure TP3DPropertyAccessVec4.SetAsString(AValue: String);
var
  Val: TVec4;
begin
  Val.FromString( AValue );
  Value:= Val;
end;

{ TP3DPropertyAccessQuat }

function TP3DPropertyAccessQuat.GetAsString: String;
begin
  Result:= Value.ToString();
end;

procedure TP3DPropertyAccessQuat.SetAsString(AValue: String);
var
  Val: TQuaternion;
begin
  Val.FromString( AValue );
  Value:= Val;
end;

{ gP3DListPropertyAccess }

function gP3DListPropertyAccess.GetCount: Integer;
begin
  Result:= Value.Count;
end;

function gP3DListPropertyAccess.GetValues(idx: Integer): TListData;
begin
  Result:= Value[ idx ];
end;

procedure gP3DListPropertyAccess.SetCount(AValue: Integer);
begin
  Value.Count:= AValue;
end;

procedure gP3DListPropertyAccess.SetValues(idx: Integer; AValue: TListData);
begin
  Value[ idx ]:= AValue;
end;

function gP3DListPropertyAccess.GetAsString: String;
begin

end;

procedure gP3DListPropertyAccess.SetAsString(AValue: String);
begin

end;




{$ENDIF}
