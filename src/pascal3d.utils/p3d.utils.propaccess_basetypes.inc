{ p3dpropaccess_basetypes.inc }

{$IFDEF INTERFACE}
type

  { TP3DPropAccessString }

  TP3DPropAccessString = class ( specialize gP3DPropAccess < String >)
    protected
      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      procedure LoadJSON( AValue: TJSONData ); override;
      function GetAsJSON: TJSONData; override;
  end;

  { TP3DPropAccessInt }

  TP3DPropAccessInt = class ( specialize gP3DPropAccess < Integer >)
    protected
      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      procedure LoadJSON( AValue: TJSONData ); override;
      function GetAsJSON: TJSONData; override;
  end;

  { TP3DPropAccessFloat }

  TP3DPropAccessFloat = class ( specialize gP3DPropAccess < Float >)
    protected
      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      procedure LoadJSON( AValue: TJSONData ); override;
      function GetAsJSON: TJSONData; override;
      procedure SetInterpolate(Value1, Value2: String; AMix: Float; AInterpolationMode: TP3DPropertyAccessInterpolationMode); override;
  end;

  { TP3DPropAccessBoolean }

  TP3DPropAccessBoolean = class ( specialize gP3DPropAccess < Boolean >)
    protected
      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      procedure LoadJSON( AValue: TJSONData ); override;
      function GetAsJSON: TJSONData; override;
  end;


  { TP3DPropAccessRTTI }

  TP3DPropAccessRTTI = class( TP3DPropertyAccess )
    protected
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
      function GetAsJSON: TJSONData; override;

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
      function ValueIsDefault: Boolean; override;

    public
      property PropInfo: PPropInfo read FPropInfo;

      function GetNames: TStringList; override;
      procedure LoadJSON(AValue: TJSONData); override;

    published
      function GetAsString: String; override;
      procedure SetAsString( AValue: String ); override;

      constructor Create( AOwner: TObject; AName: String; const AStoreMode: TP3DPropertyAccessStoreMode = smDoNotStore ); reintroduce;
      property TypeName: String read FTypeName;
      property Owner: TObject read FOwner write FOwner;
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DPropAccessString }

function TP3DPropAccessString.GetAsString: String;
begin
  Result:= AsValue;
end;

procedure TP3DPropAccessString.SetAsString(AValue: String);
begin
  AsValue:= AValue;
end;

procedure TP3DPropAccessString.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtString ) then
    AsValue:= AValue.AsString
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtString', JSONTypeName( AValue.JSONType )]);
end;

function TP3DPropAccessString.GetAsJSON: TJSONData;
begin
  Result:= TJSONString.Create( AsValue );
end;


  { TP3DPropAccessInt }

function TP3DPropAccessInt.GetAsString: String;
begin
  Result:= IntToStr( AsValue );
end;

procedure TP3DPropAccessInt.SetAsString(AValue: String);
begin
  AsValue:= StrToInt( AValue );
end;

procedure TP3DPropAccessInt.LoadJSON(AValue: TJSONData);
begin
  if (( AValue.JSONType = jtNumber ) and ( TJSONNumber( AValue ).NumberType = ntInteger )) then
    AsValue:= AValue.AsInteger
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtNumber, ntInteger', JSONTypeName( AValue.JSONType )]);;
end;

function TP3DPropAccessInt.GetAsJSON: TJSONData;
begin
  Result:= TJSONIntegerNumber.Create( AsValue );
end;

{ TP3DPropAccessBoolean }

function TP3DPropAccessBoolean.GetAsString: String;
begin
  Result:= IntToStr( Integer( AsValue ));
end;

procedure TP3DPropAccessBoolean.SetAsString(AValue: String);
begin
  AsValue:= Boolean( StrToInt( AValue ));
end;

procedure TP3DPropAccessBoolean.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtBoolean ) then
    AsValue:= AValue.AsBoolean
  else
    raise Exception.CreateFmt( Name + ': ' + P3DERROR_JSON_WRONG_TYPE, [ 'jtBoolean, ntInteger', JSONTypeName( AValue.JSONType )]);;
end;

function TP3DPropAccessBoolean.GetAsJSON: TJSONData;
begin
  Result:= TJSONBoolean.Create( AsValue );
end;


{ TP3DPropAccessFloat }

function TP3DPropAccessFloat.GetAsString: String;
begin
  Result:= FormatFloat( '0.0###', AsValue );
end;

procedure TP3DPropAccessFloat.SetAsString(AValue: String);
begin
  AsValue:= StrToFloat( AValue );
end;

procedure TP3DPropAccessFloat.LoadJSON(AValue: TJSONData);
begin
  if ( AValue.JSONType = jtNumber ) then
    begin   //WriteLn( Name, ' = ', AValue.AsFloat );
    AsValue:= AValue.AsFloat end
  else
    raise Exception.CreateFmt( Name + ': ' + P3DERROR_JSON_WRONG_TYPE, [ 'jtNumber, ntInteger', JSONTypeName( AValue.JSONType )]);;
end;

function TP3DPropAccessFloat.GetAsJSON: TJSONData;
begin
  Result:= TJSONFloatNumber.Create( AsValue );
end;

procedure TP3DPropAccessFloat.SetInterpolate(Value1, Value2: String; AMix: Float; AInterpolationMode: TP3DPropertyAccessInterpolationMode);
var
  v1, v2: Float;
  a: Float;
begin
  if ( AInterpolationMode = imConstant ) then
    inherited SetInterpolate(Value1, Value2, AMix, AInterpolationMode)
  else
    begin
      v1:= Float.Parse( Value1 );
      v2:= Float.Parse( Value2 );
      case AInterpolationMode of
        imInterpolate: a:= clamp( AMix, 0, 1 );
        imExtrapolate: a:= AMix;
      end;
      AsValue:= ( v1 * ( 1 - a ) + v2 * a );
    end;
end;

{ TP3DPropAccessRTTI }

function TP3DPropAccessRTTI.GetIntValue: string;
begin
  Result := IntToStr( GetOrdProp( Owner, FPropInfo ));
end;

function TP3DPropAccessRTTI.GetCharValue: string;
begin
  Result := Char(GetOrdProp(Owner, FPropInfo));
end;

function TP3DPropAccessRTTI.GetEnumValue: string;
begin
  Result := GetEnumName( FPropInfo^.PropType, GetOrdProp(Owner, FPropInfo));
end;

function TP3DPropAccessRTTI.GetFloatValue: string;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 19);

begin
  Result := StringReplace(FloatToStrF(GetFloatProp(Owner, FPropInfo), ffGeneral,
    Precisions[ GetTypeData( FPropInfo^.PropType )^.FloatType], 0),
    DecimalSeparator, '.',[rfReplaceAll]);
end;

function TP3DPropAccessRTTI.GetStringValue: string;
begin
   Result := GetStrProp(Owner, FPropInfo);
end;

function TP3DPropAccessRTTI.GetSetValue: string;
type
  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;
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

function TP3DPropAccessRTTI.GetClassValue: string;
begin
  //TODO: Implement GetClassValue
end;

function TP3DPropAccessRTTI.GetStringsValue: string;
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


function TP3DPropAccessRTTI.GetLStringValue: string;
begin
  Result := GetStrProp(Owner, FPropInfo);
end;

function TP3DPropAccessRTTI.GetWCharValue: string;
begin
  Result := Char(GetOrdProp(Owner, FPropInfo));
end;

function TP3DPropAccessRTTI.GetVariantValue: string;
begin
  Result := GetVariantProp(Owner, FPropInfo);
end;

function TP3DPropAccessRTTI.GetAsJSON: TJSONData;
begin
  if ( Assigned( FPropInfo )) then
    case FPropInfo^.PropType^.Kind of
      tkInteger: Result:= TJSONIntegerNumber.Create( GetOrdProp( Owner, FPropInfo ));
      tkChar: Result:= TJSONString.Create( GetCharValue );
      tkEnumeration: Result:= TJSONString.Create( GetEnumValue );
      tkFloat: Result:= TJSONFloatNumber.Create( GetFloatProp( Owner, FPropInfo ));
      tkAstring: Result:= TJSONString.Create( GetStringValue );
      tkWString: Result:= TJSONString.Create( GetStringValue );
      tkWChar: Result:= TJSONString.Create( GetWCharValue );
      tkVariant: Result:= TJSONString.Create( GetVariantValue );
      tkInt64: Result:= TJSONInt64Number.Create( GetOrdProp( Owner, FPropInfo ));
      tkString: Result:= TJSONString.Create( GetStringValue );
      tkSet: Result:= TJSONString.Create( GetSetValue );
      tkClass: Result:= TJSONString.Create( GetClassValue );
      tkBool: Result:= TJSONBoolean.Create( Boolean( GetOrdProp( Owner, FPropInfo )));
    else
      Exit;
    end;
end;

procedure TP3DPropAccessRTTI.SetIntValue(AValue: String);
begin
  SetOrdProp( Owner, FPropInfo, StrToIntDef( AValue, 0 ));
end;

procedure TP3DPropAccessRTTI.SetCharValue(AValue: String);
begin
  SetOrdProp( Owner, FPropInfo, Integer( AValue[ 1 ]));
end;

procedure TP3DPropAccessRTTI.SetEnumValue(AValue: String);
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

procedure TP3DPropAccessRTTI.SetFloatValue(AValue: String);
begin
  SetFloatProp(Owner, FPropInfo, StrToFloat(StringReplace( AValue, '.',
    DecimalSeparator,[ rfReplaceAll ])));
end;

procedure TP3DPropAccessRTTI.SetStringValue(AValue: String);
begin
  try
    SetStrProp( Owner, FPropInfo, AValue );
  except
    On E: Exception do
      raise Exception.Create( 'Error while setting property ' + GetFullPath + ': ' + E.Message );
  end;
end;


procedure TP3DPropAccessRTTI.SetSetValue(AValue: String);
type
  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;
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

procedure TP3DPropAccessRTTI.SetClassValue(AValue: String);
begin

end;

procedure TP3DPropAccessRTTI.SetStringsValue(AValue: String);
begin

end;

procedure TP3DPropAccessRTTI.SetLStringValue(AValue: String);
begin
  SetStrProp( Owner, FPropInfo, AValue );
end;

procedure TP3DPropAccessRTTI.SetWCharValue(AValue: String);
begin
  SetOrdProp( Owner, FPropInfo, Longint( AValue[ 1 ]));
end;

procedure TP3DPropAccessRTTI.SetVariantValue(AValue: String);
begin
  SetVariantProp( Owner, FPropInfo, AValue );
end;

function TP3DPropAccessRTTI.ValueIsDefault: Boolean;
begin
  Result:= not IsStoredProp( FOwner, PropInfo );
end;

function TP3DPropAccessRTTI.GetNames: TStringList;
var
  PS : PShortString;
  i, Count: Integer;
  s: String;
begin
  case ( FPropInfo^.PropType^.Kind ) of
    tkEnumeration:
      with GetTypeData( FPropInfo^.PropType )^ do begin
        PS:= @NameList;
        Count:= 0;

        Result:= TStringList.Create;
        While ( PByte( PS )^ <> 0 ) do begin
          Result.Add( PS^ );
          PS:= PShortString( Pointer( PS ) + PByte( PS )^ + 1 );
          Inc( Count );
        end;

        Result.Delete( Result.Count - 1 ); // Last line is the unit name
      end;
    tkSet:
      with GetTypeData(GetTypeData( FPropInfo^.PropType )^.CompType )^ do begin
        Result:= TStringList.Create;
        for I := MinValue to MaxValue do
          Result.Add( GetEnumName( GetTypeData( FPropInfo^.PropType )^.CompType, I ));
      end;
    tkBool: begin
      Result:= TStringList.Create;
      Result.Text:= 'False' + LineEnding + 'True';
    end
  else
    Result:= nil;
  end;
end;

procedure TP3DPropAccessRTTI.LoadJSON(AValue: TJSONData);
begin
  if ( Assigned( FPropInfo )) then
    case FPropInfo^.PropType^.Kind of
      tkInteger: SetOrdProp( Owner, FPropInfo, AValue.AsInteger );
      tkChar: SetCharValue( AValue.AsString );
      tkEnumeration: SetEnumValue( AValue.AsString );
      tkFloat: SetFloatProp( Owner, FPropInfo, AValue.AsFloat );
      tkAstring: SetStringValue( AValue.AsString );
      tkWString: SetStringValue( AValue.AsString );
      tkWChar: SetWCharValue( AValue.AsString );
      tkVariant: SetVariantValue( AValue.AsString );
      tkInt64: SetOrdProp( Owner, FPropInfo, AValue.AsInt64 );
      tkString: SetStringValue( AValue.AsString );
      tkSet: SetSetValue( AValue.AsString );
      tkClass: SetClassValue( AValue.AsString );
      tkBool: SetOrdProp( Owner, FPropInfo, AValue.AsInteger );
    else
      Exit;
    end;
end;

function TP3DPropAccessRTTI.GetAsString: String;
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

procedure TP3DPropAccessRTTI.SetAsString(AValue: String);
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

constructor TP3DPropAccessRTTI.Create(AOwner: TObject; AName: String;
  const AStoreMode: TP3DPropertyAccessStoreMode);
begin
  inherited Create( AName, AStoreMode );
  Owner:= AOwner;
  FPropInfo:= GetPropInfo( Owner, Name );
  if ( not Assigned( FPropInfo )) then
    raise Exception.Create( 'Cannot create access: The specified property was not found: ' + AName );
  FTypeName:= PropInfo^.PropType^.Name;
end;


{$ENDIF}

