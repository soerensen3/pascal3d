unit p3dRTTI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, typinfo, strutils, p3dutils;

type

  TCardinalSet = set of 0..SizeOf(Cardinal) * 8 - 1;

  { TP3DProperty }

  //paNone - The access method is not provided. Data is ignored.
  //paField - Data is a pointer to the field
  //paMethod - Data is a pointer to a method to query or write the field
  TP3DPropertyAccessType = ( paNone, paField, paMethod );
  //If Read is specified then the property is readable
  //If Write is specified the property is writable
  //If Next is specified the property is considered as an array, which is read until Next returns 0.
  TP3DFakePropertyInfo = record
    PropType: TTypeKind;
    PropTypeName: String;
    PropName: String;
    ReaderType: TP3DPropertyAccessType;
    ReadData: Pointer;
    WriterType: TP3DPropertyAccessType;
    WriteData: Pointer;
    NextType: TP3DPropertyAccessType;
    NextData: Pointer;
  end;

  TP3DPropList = class;
  TP3DProperty = class ( TPersistent )
    private
      FCanHaveChilds: Boolean;
      FChilds: TP3DPropList;
      FIsReadOnly: Boolean;
      FOwner: TP3DPropList;
      FName: String;

      function GetTypeKind: TTypeKind; virtual; abstract;
      function GetTypeName: String; virtual; abstract;
      function GetValue: String; virtual; abstract;
      procedure SetValue( AValue: String ); virtual; abstract;

    public
      constructor Create( AOwner: TP3DPropList; AName: String );
      function CreateChilds: Boolean; virtual;
      destructor Destroy; override;

    published
      property Name: String read FName;
      property TypeKind: TTypeKind read GetTypeKind;
      property TypeName: String read GetTypeName;
      property Owner: TP3DPropList read FOwner write FOwner;
      property Value: String read GetValue write SetValue;
      property Childs: TP3DPropList read FChilds write FChilds;
      property CanHaveChilds: Boolean read FCanHaveChilds;
      property IsReadOnly: Boolean read FIsReadOnly;
  end;

  { TP3DRTTIProperty }

  TP3DRTTIProperty = class ( TP3DProperty )
    private
      FPropInfo: PPropInfo;

      function GetTypeKind: TTypeKind; override;
      function GetTypeName: String; override;

    public
      constructor Create( AOwner: TP3DPropList; APropInfo: PPropInfo );
  end;


  { TP3DRTTIDefaultProperty }

  //THANKS TO rttiutils.pas from fpc!
  TP3DRTTIDefaultProperty = class ( TP3DRTTIProperty )
    private
      function GetValue: String; override;
      procedure SetValue( AValue: String );

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
      constructor Create( AOwner: TP3DPropList; APropInfo: PPropInfo );
      function CreateChilds: Boolean; override;
  end;

  { TP3DFakeProperty }

  TP3DFakeProperty = class ( TP3DProperty )
    private
      FPropInfo: TP3DFakePropertyInfo;

      function GetTypeKind: TTypeKind; override;
      function GetTypeName: String; override;
      function GetValue: String; override;
      procedure SetValue(AValue: String); override;

    public
      constructor Create( AOwner: TP3DPropList; APropInfo: TP3DFakePropertyInfo );
  end;


  TP3DCustomPropList = specialize gP3DCustomObjectList < TP3DProperty >;

  { TP3DPropList }

  TP3DPropList = class ( TP3DCustomPropList )
    private
      FObj: TPersistent;

      procedure SetObj( AValue: TPersistent );

    public
      procedure Update;
      destructor Destroy; override;

    published
      property Obj: TPersistent read FObj write SetObj;
  end;

  //function P3DPropertyInfo( APropName: String; APropType: TTypeKind; Read:


implementation

{ TP3DFakeProperty }

function TP3DFakeProperty.GetTypeKind: TTypeKind;
begin
  Result:= FPropInfo.PropType;
end;

function TP3DFakeProperty.GetTypeName: String;
begin
  Result:= FPropInfo.PropName;
end;

function TP3DFakeProperty.GetValue: String;
begin

end;

procedure TP3DFakeProperty.SetValue(AValue: String);
begin

end;

constructor TP3DFakeProperty.Create(AOwner: TP3DPropList;
  APropInfo: TP3DFakePropertyInfo);
begin
  inherited Create( AOwner, APropInfo.PropName );
  FPropInfo:= APropInfo;
end;

{ TP3DRTTIDefaultProperty }

function TP3DRTTIDefaultProperty.GetValue: String;
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

procedure TP3DRTTIDefaultProperty.SetValue(AValue: String);
begin

end;

function TP3DRTTIDefaultProperty.GetIntValue: string;
begin
  Result := IntToStr( GetOrdProp( Owner.Obj, FPropInfo ));
end;

function TP3DRTTIDefaultProperty.GetCharValue: string;
begin
  Result := Char(GetOrdProp(Owner.Obj, FPropInfo));
end;

function TP3DRTTIDefaultProperty.GetEnumValue: string;
begin
  Result := GetEnumName( FPropInfo^.PropType, GetOrdProp(Owner.Obj, FPropInfo));
end;

function TP3DRTTIDefaultProperty.GetFloatValue: string;
const
  Precisions: array[TFloatType] of Integer = (7, 15, 18, 18, 19);

begin
  Result := StringReplace(FloatToStrF(GetFloatProp(Owner.Obj, FPropInfo), ffGeneral,
    Precisions[ GetTypeData( FPropInfo^.PropType )^.FloatType], 0),
    DecimalSeparator, '.',[rfReplaceAll]);
end;

function TP3DRTTIDefaultProperty.GetStringValue: string;
begin
   Result := GetStrProp(Owner.Obj, FPropInfo);
end;

function TP3DRTTIDefaultProperty.GetSetValue: string;
var
  TypeInfo: PTypeInfo;
  W: Cardinal;
  I: Integer;
begin
  Result := '[';
  W := GetOrdProp(Owner.Obj, FPropInfo);
  TypeInfo := GetTypeData( FPropInfo^.PropType )^.CompType;
  for I := 0 to SizeOf(TCardinalSet) * 8 - 1 do
    if I in TCardinalSet(W) then begin
      if Length(Result) <> 1 then Result := Result + ',';
      Result := Result + GetEnumName(TypeInfo, I);
    end;
  Result := Result + ']';
end;

function TP3DRTTIDefaultProperty.GetClassValue: string;
begin
  //TODO: Implement GetClassValue
end;

function TP3DRTTIDefaultProperty.GetStringsValue: string;
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


function TP3DRTTIDefaultProperty.GetLStringValue: string;
begin
  Result := GetStrProp(Owner.Obj, FPropInfo);
end;

function TP3DRTTIDefaultProperty.GetWCharValue: string;
begin
  Result := Char(GetOrdProp(Owner.Obj, FPropInfo));
end;

function TP3DRTTIDefaultProperty.GetVariantValue: string;
begin
  Result := GetVariantProp(Owner.Obj, FPropInfo);
end;

procedure TP3DRTTIDefaultProperty.SetIntValue(AValue: String);
begin
  SetOrdProp( Owner.Obj, FPropInfo, StrToIntDef( AValue, 0 ));
end;

procedure TP3DRTTIDefaultProperty.SetCharValue(AValue: String);
begin
  SetOrdProp( Owner.Obj, FPropInfo, Integer( AValue[ 1 ]));
end;

procedure TP3DRTTIDefaultProperty.SetEnumValue(AValue: String);
var
  I: Integer;
  EnumType: PTypeInfo;
begin
  EnumType := FPropInfo^.PropType;
  with GetTypeData(EnumType)^ do
    for I := MinValue to MaxValue do
      if ( CompareText( GetEnumName( EnumType, I ), AValue ) = 0 ) then
      begin
        SetOrdProp( Owner.Obj, FPropInfo, I );
        Exit;
      end;
end;

procedure TP3DRTTIDefaultProperty.SetFloatValue(AValue: String);
begin
  SetFloatProp(Owner.Obj, FPropInfo, StrToFloat(StringReplace( AValue, '.',
    DecimalSeparator,[ rfReplaceAll ])));
end;

procedure TP3DRTTIDefaultProperty.SetStringValue(AValue: String);
begin
  SetStrProp( Owner.Obj, FPropInfo, AValue );
end;


procedure TP3DRTTIDefaultProperty.SetSetValue(AValue: String);
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
  SetOrdProp( Owner.Obj, FPropInfo, W );
end;

procedure TP3DRTTIDefaultProperty.SetClassValue(AValue: String);
begin

end;

procedure TP3DRTTIDefaultProperty.SetStringsValue(AValue: String);
begin

end;

procedure TP3DRTTIDefaultProperty.SetLStringValue(AValue: String);
begin
  SetStrProp( Owner.Obj, FPropInfo, AValue );
end;

procedure TP3DRTTIDefaultProperty.SetWCharValue(AValue: String);
begin
  SetOrdProp( Owner.Obj, FPropInfo, Longint( AValue[ 1 ]));
end;

procedure TP3DRTTIDefaultProperty.SetVariantValue(AValue: String);
begin
  SetVariantProp( Owner.Obj, FPropInfo, AValue );
end;

constructor TP3DRTTIDefaultProperty.Create(AOwner: TP3DPropList;
  APropInfo: PPropInfo);
begin
  inherited;
  FCanHaveChilds:= APropInfo^.PropType^.Kind = tkClass;
end;

function TP3DRTTIDefaultProperty.CreateChilds: Boolean;
var
  Obj: TObject;
begin
  Result:= False;
  case FPropInfo^.PropType^.Kind of
    tkClass:
      begin
        Obj:= GetObjectProp( Owner.Obj, FPropInfo );
        if ( Obj is TPersistent ) then
          begin
            Childs.Obj:= TPersistent( Obj );
            Result:= True;
          end;
      end;
  end;
end;

{ TP3DRTTIProperty }

function TP3DRTTIProperty.GetTypeKind: TTypeKind;
begin
  Result:= FPropInfo^.PropType^.Kind;
end;

function TP3DRTTIProperty.GetTypeName: String;
begin
  Result:= FPropInfo^.PropType^.Name;
end;

constructor TP3DRTTIProperty.Create( AOwner: TP3DPropList; APropInfo: PPropInfo );
begin
  inherited Create( AOwner, APropInfo^.Name );
  FPropInfo:= APropInfo;
  FIsReadOnly:= not Assigned( APropInfo^.SetProc );
end;

{ TP3DPropList }

procedure TP3DPropList.SetObj(AValue: TPersistent);
begin
  if ( FObj = AValue ) then
    Exit;

  FObj:= AValue;
  Update;
end;

procedure TP3DPropList.Update;
var
  Size, i, Cnt: Integer;
  FList: PPropList;
begin
  Clear( True );
  FList:= nil;
  if ( Assigned( Obj )) then
    begin
      Cnt:= GetPropList( PTypeInfo( Obj.ClassInfo ), tkAny, {Filter,} nil );
      Size := Cnt * SizeOf(Pointer);
      GetMem( FList, Size );
      GetPropList( Obj.ClassInfo, tkAny, {Filter,} FList);
      for i:= 0 to Cnt - 1 do
        Add( TP3DRTTIDefaultProperty.Create( Self, FList^[ i ]));
      FreeMem( FList, Size );
      FList:= nil;
    end;
end;

destructor TP3DPropList.Destroy;
begin
  Clear( True );
  inherited Destroy;
end;

{ TP3DProperty }

constructor TP3DProperty.Create( AOwner: TP3DPropList; AName: String );
begin
  inherited Create;
  FOwner:= AOwner;
  FName:= AName;
  Childs:= TP3DPropList.Create;
end;

function TP3DProperty.CreateChilds: Boolean;
begin
  Result:= False;
end;

destructor TP3DProperty.Destroy;
begin
  Childs.Clear( True );
  Childs.Free;
  inherited Destroy;
end;

end.

