{ p3dpropaccess_p3dtypes.inc }

{$IFDEF INTERFACE}

type
  TP3DPathMode = ( pmAbsolute, pmRelativeToBinary, pmRelativeToBaseBath, pmRelativeToJSONFile, pmRelativeToCurrentPath );

  { TP3DPropAccessFilePointer }

  TP3DPropAccessFilePointer = class ( specialize gP3DPropAccess < TP3DFilePointer >)
    protected
      FPathMode: TP3DPathMode;
      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      procedure LoadJSON(AValue: TJSONData); override;
      function GetAsJSON: TJSONData; override;

    public
      constructor CreateField(AName: String; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore; APathMode: TP3DPathMode = pmRelativeToBaseBath ); reintroduce;
      constructor CreateFieldSet(AName: String; AField: pT; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore; APathMode: TP3DPathMode = pmRelativeToBaseBath ); reintroduce;
      constructor CreateGetField(AName: String; AGetter: TGetter; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore; APathMode: TP3DPathMode = pmRelativeToBaseBath ); reintroduce;
      constructor CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode=smDoNotStore; APathMode: TP3DPathMode = pmRelativeToBaseBath ); reintroduce;

      property PathMode: TP3DPathMode read FPathMode write FPathMode;
  end;

  { TP3DPropAccessFilePointerList }

  TP3DPropAccessFilePointerList = class( specialize gP3DPropAccessList < TP3DFilePointerList, TP3DFilePointer, TP3DPropAccessFilePointer >)
    protected
      FPathMode: TP3DPathMode;
      procedure LoadJSON(AValue: TJSONData); override;

    public
      function IndexOf(AItem: Pointer): Integer; override;

      property PathMode: TP3DPathMode read FPathMode write FPathMode;
    end;

  { gP3DPropAccessVec }

  generic gP3DPropAccessVec < TVec; TListData; TItemProp > = class( specialize gP3DPropAccessArray < TVec >)
    protected
      FFakeProp: TItemProp;
      FFakePropIdx: Integer;
      function GetFakeProp: TListData;
      procedure SetFakeProp(AValue: TListData);
      function GetAsString: String; override;
      procedure SetAsString(AValue: String); override;
      procedure SetCount(AValue: Integer); override;
      function GetCount: Integer; override;

      class function GetSupportedInterpolationModes: TP3DPropertyAccessInterpolationModes; static;
      function ValueIsDefault: Boolean; override;

    public
      destructor Destroy; override;

      procedure SetInterpolate(Value1, Value2: String; AMix: Float; AInterpolationMode: TP3DPropertyAccessInterpolationMode); override;
      function GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess; override;
      function IndexOf(AItem: Pointer): Integer; override;
  end;

  TP3DPropAccessVec2      = specialize gP3DPropAccessVec  < TVec2, Float, TP3DPropAccessFloat >;
  TP3DPropAccessVec3      = specialize gP3DPropAccessVec  < TVec3, Float, TP3DPropAccessFloat >;
  TP3DPropAccessVec4      = specialize gP3DPropAccessVec  < TVec4, Float, TP3DPropAccessFloat >;
  TP3DPropAccessQuat      = specialize gP3DPropAccessVec  < TQuat, Float, TP3DPropAccessFloat >;
  TP3DPropAccessIVec2     = specialize gP3DPropAccessVec  < TIVec2, Int, TP3DPropAccessInt >;
  TP3DPropAccessIVec3     = specialize gP3DPropAccessVec  < TIVec3, Int, TP3DPropAccessInt >;
  TP3DPropAccessIVec4     = specialize gP3DPropAccessVec  < TIVec4, Int, TP3DPropAccessInt >;

  TP3DPropAccessVec2List  = specialize gP3DPropAccessListBinaryStream < TVec2List, TVec2, TP3DPropAccessVec2 >;
  TP3DPropAccessVec3List  = specialize gP3DPropAccessListBinaryStream < TVec3List, TVec3, TP3DPropAccessVec3 >;
  TP3DPropAccessVec4List  = specialize gP3DPropAccessListBinaryStream < TVec4List, TVec4, TP3DPropAccessVec4 >;
  TP3DPropAccessFloatList = specialize gP3DPropAccessListBinaryStream < TFloatList, Float, TP3DPropAccessFloat >;
  TP3DPropAccessIntList   = specialize gP3DPropAccessListBinaryStream < TIntList, Integer, TP3DPropAccessInt >;
  TP3DPropAccessIVec2List = specialize gP3DPropAccessListBinaryStream < TIVec2List, TIVec2, TP3DPropAccessIVec2 >;
  TP3DPropAccessIVec3List = specialize gP3DPropAccessListBinaryStream < TIVec3List, TIVec3, TP3DPropAccessIVec3 >;
  TP3DPropAccessIVec4List = specialize gP3DPropAccessListBinaryStream < TIVec4List, TIVec4, TP3DPropAccessIVec4 >;
  TP3DPropAccessQuatList  = specialize gP3DPropAccessListBinaryStream < TQuatList, TQuat, TP3DPropAccessQuat >;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DPropertyAccessFilePointer }

constructor TP3DPropAccessFilePointer.CreateField(AName: String; AField: pT;
  const AStoreMode: TP3DPropertyAccessStoreMode; APathMode: TP3DPathMode);
begin
  inherited CreateField( AName, AField, AStoreMode );
  PathMode:= APathMode;
end;

constructor TP3DPropAccessFilePointer.CreateFieldSet(AName: String; AField: pT;
  ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode;
  APathMode: TP3DPathMode);
begin
  inherited CreateFieldSet( AName, AField, ASetter, AStoreMode );
  PathMode:= APathMode;
end;

constructor TP3DPropAccessFilePointer.CreateGetField(AName: String;
  AGetter: TGetter; AField: pT; const AStoreMode: TP3DPropertyAccessStoreMode;
  APathMode: TP3DPathMode);
begin
  inherited CreateGetField( AName, AGetter, AField, AStoreMode );
  PathMode:= APathMode;
end;

constructor TP3DPropAccessFilePointer.CreateGetSet(AName: String; AGetter: TGetter; ASetter: TSetter; const AStoreMode: TP3DPropertyAccessStoreMode; APathMode: TP3DPathMode);
begin
  inherited CreateGetSet( AName, AGetter, ASetter, AStoreMode );
  PathMode:= APathMode;
end;

function TP3DPropAccessFilePointer.GetAsString: String;
  function GetJSONFilePath: String;
  begin
    Result:= ExtractFilePath( Root.GetJSONLoader().FileName );
    WriteLn( AsValue.FileName, ': ', Result );
  end;

var
  FileName: String;

begin
  if ( Assigned( AsValue )) then
    begin
      FileName:= ExtractFileName( AsValue.FileName );
      case PathMode of
        pmAbsolute: Result:= AsValue.FileName;
        pmRelativeToBinary: Result:= ExtractRelativepath( ExtractFilePath( ParamStr( 0 )), AsValue.FileName ) + FileName;
        pmRelativeToBaseBath: Result:= ExtractRelativepath( P3DSearchPaths.BaseDir.FileName, AsValue.FileName ) + FileName;
        pmRelativeToJSONFile: Result:= ExtractRelativepath( GetJSONFilePath, AsValue.FileName );
        pmRelativeToCurrentPath: Result:= ExtractRelativepath( AppendPathDelim( GetCurrentDir()), AsValue.FileName ) + FileName;
      end;
    end;
end;

procedure TP3DPropAccessFilePointer.SetAsString(AValue: String);
  function GetJSONFilePath: String;
  begin
    Result:= ExtractFilePath( Root.GetJSONLoader().FileName );
  end;
var
  FN: String;
begin
  if ( FilenameIsAbsolute( AValue )) then
    FN:= AValue
  else
    case FPathMode of
      pmAbsolute: FN:= CreateAbsolutePath( AValue, ExtractFilePath( AsValue.FileName ));
      pmRelativeToBaseBath: FN:= CreateAbsolutePath( AValue, P3DSearchPaths.BaseDir.FileName );
      pmRelativeToBinary: FN:= CreateAbsolutePath( AValue, ExtractFilePath( ParamStr( 0 )));
      pmRelativeToJSONFile: FN:= CreateAbsolutePath( AValue, GetJSONFilePath );
      pmRelativeToCurrentPath: FN:= CreateAbsolutePath( AValue, GetCurrentDir );
    end;
  if ( not Assigned( AsValue )) then
    AsValue:= TP3DFilePointer.Create( FN )
  else
    AsValue.FileName:= FN;
end;

procedure TP3DPropAccessFilePointer.LoadJSON(AValue: TJSONData);
var
  FN: String;
begin
  if ( AValue.JSONType = jtString ) then
    begin
      FN:= AValue.AsString;
      SetAsString( FN );
    end
  else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ JSONTypeName( jtString ), JSONTypeName( AValue.JSONType )]);
end;

function TP3DPropAccessFilePointer.GetAsJSON: TJSONData;
begin
  Result:= TJSONString.Create( AsString );
end;

{ TP3DPropAccessFilePointerList }

procedure TP3DPropAccessFilePointerList.LoadJSON(AValue: TJSONData);
begin
  inherited LoadJSON(AValue);
end;

function TP3DPropAccessFilePointerList.IndexOf(AItem: Pointer): Integer;
begin
  Result:= AsValue.IndexOf( TP3DFilePointer( AItem ));
end;

{ gP3DPropAccessVec }

function gP3DPropAccessVec.GetFakeProp: TListData;
begin
  Result:= AsValue[ FFakePropIdx ];
end;

procedure gP3DPropAccessVec.SetFakeProp(AValue: TListData);
var
  Vec: TVec;
begin
  if ( FFakePropIdx < Count ) then begin
    Vec:= AsValue;
    Vec[ FFakePropIdx ]:= AValue;
    AsValue:= Vec;
  end;
end;

function gP3DPropAccessVec.GetAsString: String;
begin
  Result:= AsValue.ToString;
end;

procedure gP3DPropAccessVec.SetAsString(AValue: String);
var
  Val: TVec;
begin
  Val.FromString( AValue );
  AsValue:= Val;
end;
{
procedure gP3DPropAccessVec.LoadFromStream(S: TStream);
begin
  S.ReadBuffer( AsValue.Ptr^, SizeOf( TVec ));
end;

procedure gP3DPropAccessVec.SaveToStream(S: TStream);
begin
  S.WriteBuffer( AsValue.Ptr^, SizeOf( TVec ));
end;
}
procedure gP3DPropAccessVec.SetCount(AValue: Integer);
begin
  //Can't set count so do nothing
end;

function gP3DPropAccessVec.GetCount: Integer;
begin
  Result:= AsValue.Count;
end;

class function gP3DPropAccessVec.GetSupportedInterpolationModes: TP3DPropertyAccessInterpolationModes;
begin
  Result:= [ imConstant, imInterpolate, imExtrapolate ];
end;

function gP3DPropAccessVec.ValueIsDefault: Boolean;
begin
  Result:= AsValue = Default( TVec );
end;

destructor gP3DPropAccessVec.Destroy;
begin
  if ( Assigned( FFakeProp )) then
    FreeAndNil( FFakeProp );
  inherited Destroy;
end;

procedure gP3DPropAccessVec.SetInterpolate(Value1, Value2: String;
  AMix: Float; AInterpolationMode: TP3DPropertyAccessInterpolationMode);
var
  v1, v2: TVec;
  a: Float;
begin
  if ( AInterpolationMode = imConstant ) then
    inherited SetInterpolate(Value1, Value2, AMix, AInterpolationMode)
  else
    begin
      v1.FromString( Value1 );
      v2.FromString( Value2 );
      case AInterpolationMode of
        imInterpolate: a:= clamp( AMix, 0, 1 );
        imExtrapolate: a:= AMix;
      end;
      AsValue:= TVec( v1 * ( 1 - a ) + v2 * a );
    end;
end;

function gP3DPropAccessVec.GetArrayItemAsProp(Idx: Integer): TP3DPropertyAccess;
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

function gP3DPropAccessVec.IndexOf(AItem: Pointer): Integer;
begin
  Result:= -1;
end;

{$ENDIF}

