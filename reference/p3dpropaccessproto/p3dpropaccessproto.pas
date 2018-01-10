program p3dpropaccesproto;

uses p3dpropaccess, fpjson, jsonparser, sysutils, Classes, fgl;

type

  { TTestObj }

  TTestObj = class ( TP3DNamedStreamable )
    private
      FTestObj: TP3DStreamable;
      FTestObjPointer: TP3DPropAccessStreamablePointer;
      FX: String;
      FY: String;

      procedure SetTestObjPointer(AValue: TP3DStreamable);

    public
      constructor Create( const AOwner: IP3DStreamableContainer = nil; const ARoot: IP3DLoadJSON = nil ); override;

    published
      property X: String read FX write FX;
      property Y: String read FY write FY;
      property TestObjPointer: TP3DStreamable read FTestObj write SetTestObjPointer;
  end;

  TTestObjList = specialize gP3DNamedStreamableList < TTestObj >;
  TP3DPropAccessTestObj = specialize gP3DPropAccessStreamable < TTestObj >;

  { TTestObj2 }

  TTestObj2 = class ( TP3DNamedStreamable )
    private
      FObjList: TTestObjList;

    public
      constructor Create( const AOwner: IP3DStreamableContainer = nil; const ARoot: IP3DLoadJSON = nil ); override;
      destructor Destroy; override;

    published
      property ObjList: TTestObjList read FObjList write FObjList;
  end;

  TP3DPropAccessTestObj2 = specialize gP3DPropAccessStreamable < TTestObj2 >;

  TP3DPropAccessStreamable = specialize gP3DPropAccessStreamable < TP3DStreamable >;
  TP3DPropAccessStreamablePointer = specialize gP3DPropAccessStreamablePointer < TP3DStreamable >;
  TP3DPropAccessTestObjectList = specialize gP3DPropAccessNamedStreamableList < TTestObjList, TTestObj, TP3DPropAccessTestObj >;

{ TTestObj2 }

constructor TTestObj2.Create(const AOwner: IP3DStreamableContainer;
  const ARoot: IP3DLoadJSON);
var
  Prop: TP3DPropertyAccess;
begin
  inherited Create( AOwner, ARoot );
  ObjList:= TTestObjList.Create;
  Prop:= TP3DPropAccessTestObjectList.CreateField( 'ObjList', @FObjList, smText );
  Properties.Add( Prop );
end;

destructor TTestObj2.Destroy;
begin
  ObjList.Clear( True );
  FreeAndNil( FObjList );
  inherited Destroy;
end;

procedure TTestObj.SetTestObjPointer(AValue: TP3DStreamable);
begin
  if FTestObj=AValue then Exit;
  //FTestObjPointer.AsValue:= AValue;
  FTestObjPointer.AsValue:= AValue;
end;

constructor TTestObj.Create(const AOwner: IP3DStreamableContainer;
  const ARoot: IP3DLoadJSON);
begin
  inherited Create( AOwner, ARoot );
  FTestObj:= nil;
  FTestObjPointer:= TP3DPropAccessStreamablePointer.CreateField( 'TestObjPointer', @FTestObj, smText );
  Properties.Add([ TP3DPropAccessString.CreateField( 'X', @X, smText ),
                   TP3DPropAccessString.CreateField( 'Y', @Y, smText ),
                   FTestObjPointer ]);
end;

type
  IP3DTest= interface
    ['{17E7FF7E-EB98-43A9-A513-BB1B1DB5F44B}']
    function Abc123: String;
  end;

  IP3DTest2= interface
    ['{E25759FF-84B9-4F3F-877B-3B4815E82FC4}']
    function AbcDEF: String;
  end;

  { TestClass }

  TestClass = class( TInterfacedPersistent, IP3DTest, IP3DTest2 )
    function Abc123: String;
    function AbcDEF: String;
  end;

  { TJSONRoot }

  TJSONRoot = class ( TP3DLibrary )
    public
      TestObj1: TTestObj;
      TestObj1Prop, TestObj2Prop: TP3DPropAccessTestObj;
      TestObj2: TTestObj;
      TestObj3Prop: TP3DPropAccessTestObj2;
      TestObj3: TTestObj2;

      constructor Create( const AOwner: IP3DStreamableContainer = nil; const ARoot: IP3DLoadJSON = nil ); override;
      destructor Destroy; override;
  end;

const
  SampleJSON =
  '{' + LineEnding +
  '  "TestObj1" : {' + LineEnding +
  '    "X" : "",' + LineEnding +
  '    "Y" : "",' + LineEnding +
  '    "TestObjPointer" : "TestObj3.ObjList[4]"' + LineEnding +
  '  },' + LineEnding +
  '  "TestObj2" : {' + LineEnding +
  '    "X" : "",' + LineEnding +
  '    "Y" : "",' + LineEnding +
  '    "TestObjPointer" : null' + LineEnding +
  '  },' + LineEnding +
  '  "TestObj3" : {' + LineEnding +
  '    "ObjList" : [' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      },' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      },' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      },' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      },' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      },' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      },' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      },' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      },' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      },' + LineEnding +
  '      {' + LineEnding +
  '        "X" : "",' + LineEnding +
  '        "Y" : "",' + LineEnding +
  '        "TestObjPointer" : null' + LineEnding +
  '      }' + LineEnding +
  '    ]' + LineEnding +
  '  }' + LineEnding +
  '}';

var
  JSONRoot: TJSONRoot;
  json: TJSONData;
  Prop: TP3DPropertyAccess;
  i: Integer;

{ TJSONRoot }

constructor TJSONRoot.Create(const AOwner: IP3DStreamableContainer;
  const ARoot: IP3DLoadJSON);
begin
  inherited Create( AOwner, ARoot );

  TestObj1Prop:= TP3DPropAccessTestObj.CreateField( 'TestObj1', @TestObj1, smText );
  TestObj2Prop:= TP3DPropAccessTestObj.CreateField( 'TestObj2', @TestObj2, smText );
  TestObj3Prop:= TP3DPropAccessTestObj2.CreateField( 'TestObj3', @TestObj3, smText );
  Properties.Add([ TestObj1Prop, TestObj2Prop, TestObj3Prop ]);
end;

destructor TJSONRoot.Destroy;
begin
  inherited Destroy;
end;


{ TestClass }

function TestClass.Abc123: String;
begin
  Result:= 'Hans Wurst!';
end;

function TestClass.AbcDEF: String;
begin
  Result:= 'Wurstbrot!';
end;

begin
  JSONRoot:= TJSONRoot.Create( nil );
  with ( JSONRoot ) do
    begin
      {T:= TestClass.Create;
      P:= T;

      if ( P is IP3DTest ) then
        WriteLn( 'ABC: ' + ( P as IP3DTest ).Abc123 );
      if ( P is IP3DTest2 ) then
        WriteLn( 'DEF: ' + ( P as IP3DTest2 ).AbcDEF );
      FreeAndNil( T );
      P:= nil;}
      {TestObj1:= TTestObj.Create( TestObj1Prop, JSONRoot );
      TestObj2:= TTestObj.Create( TestObj2Prop, JSONRoot );
      TestObj3:= TTestObj2.Create( TestObj3Prop, JSONRoot );
      for i:= 0 to 9 do
        TestObj3.ObjList.Add( TTestObj.Create( TestObj3.Properties.GetPropByName( 'ObjList' ), JSONRoot ));}

      {{

      WriteLn( TestObj1Prop.AsString );

      WriteLn( TestObj1.GetFullPath );
      WriteLn( TestObj1.Properties[ 2 ].GetFullPath );}
      //WriteLn( '{' + JSONRoot.Properties.GetJSON + '}' );
      json:= JSONRoot.Properties.GetJSON; //GetJSON( '{' + JSONRoot.Properties.GetAsString + '}' );
      WriteLn( json.FormatJSON());
      json.Free;}
      JSONLoader.Root:= JSONRoot;
      JSONLoader.JSON:= GetJSON( SampleJSON );
      JSONLoader.ReadJSON;
      TestObj2.TestObjPointer:= TestObj3.ObjList[ 2 ];
      //TestObj3.Free;
      {Prop:= TestObj2Prop.AsValue.Properties.GetPropByName( 'TestObjPointer' );
      Prop.AsString:= 'TestObj3.ObjList[5]';
      Prop:= TP3DPropertyAccess( TestObj3Prop ).GetChildPropByPath( 'ObjList' );
      Prop:= Prop.GetChildPropByPath( '[2].X' );
      if ( Assigned( Prop )) then
        Prop.AsString:= '10'; }
      json:= JSONRoot.AsJSON;
      WriteLn( json.FormatJSON());
      json.Free;
    end;
  {WriteLn( 'Number of unfreed streamables: ', IntToStr( DebugList.Count ));
  for i:= 0 to DebugList.Count - 1 do
    WriteLn( IntToHex( Integer( Pointer( DebugList[ i ])), 8 ));}
  JSONRoot.Free;
  {WriteLn( 'Number of unfreed streamables: ', DebugList.Count );
  for i:= 0 to DebugList.Count - 1 do
    WriteLn( IntToHex( Integer( Pointer( DebugList[ i ])), 8 ));}
end.

