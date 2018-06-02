program managedlists;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  sysutils, fpjson,
  p3d.utils
  { you can add units after this };

type

  { TTestClass }

  { TTestStreamable }

  TTestStreamable = class ( TP3DNamedStreamable )
    TestVal: TP3DPropAccessInt;
    procedure SetOwner(AValue: IP3DStreamableContainer);
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
    destructor Destroy; override;
  end;

  TTestStreamableContainerList = specialize gP3DNamedStreamableContainerList < TTestStreamable >;
  TTestStreamablePointerList = specialize gP3DStreamablePointerList < TTestStreamable >;

  TTestClass = class( TP3DJSONRoot )
    Cnt1: TTestStreamableContainerList;
    Cnt2: TTestStreamableContainerList;
    Pnt: TTestStreamablePointerList;
    Pnt1: TP3DStreamablePointer;
    constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
    destructor Destroy; override;
  end;

var
  i: Integer;
  TestClass: TTestClass;
  json: TJSONData;

{ TTestStreamable }

procedure TTestStreamable.SetOwner(AValue: IP3DStreamableContainer);
begin
  WriteLn( Name + ' switching owner to ' + AValue.GetContainer.Name );
  inherited SetOwner( AValue );
end;

constructor TTestStreamable.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  TestVal:= TP3DPropAccessInt.Create( 'TestVal' );
  Properties.Add( TestVal );
end;

destructor TTestStreamable.Destroy;
begin
  WriteLn( Name + ' Destroy' );
  inherited;
end;

{ TTestClass }

constructor TTestClass.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  Cnt1:= TTestStreamableContainerList.Create( 'Cnt1' );
  Cnt2:= TTestStreamableContainerList.Create( 'Cnt2' );
  Pnt:= TTestStreamablePointerList.Create( 'Pnt' );
  Pnt1:= TP3DStreamablePointer.Create( 'Pnt', TTestStreamable );

  Properties.Add([ Cnt1, Cnt2, Pnt, Pnt1 ]);
end;

destructor TTestClass.Destroy;
begin
  WriteLn( 'Destroy TestClass' );
  inherited Destroy;
end;

begin
  DeleteFile( 'trace.trc' );
  SetHeapTraceOutput('trace.trc');
  P3DUtilsInit;
  TestClass:= TTestClass.Create();

  for i:= 0 to 9 do
    with ( TTestStreamable.Create( TestClass.Cnt1 )) do
      begin
        Name:= 'Streamable' + IntToStr( i );
        TestVal.AsValue:= 1;
      end;
  for i:= 5 to 15 do
    with ( TTestStreamable.Create( TestClass.Cnt2 )) do
      begin
        Name:= 'Streamable' + IntToStr( i );
        TestVal.AsValue:= 2;
      end;
  TestClass.Pnt1.AsValue:= TestClass.Cnt1[ 7 ];

  for i:= 0 to TestClass.Cnt1.Count - 1 do
    TestClass.Pnt.Add( TestClass.Cnt1[ i ]);
  for i:= 0 to TestClass.Cnt2.Count - 1 do
    TestClass.Pnt.Add( TestClass.Cnt2[ i ]);

  TestClass.Cnt1[ 5 ]:= nil;
  TestClass.Cnt1[ 5 ].Free;
  for i:= TestClass.Cnt1.Count - 1 downto 0 do
    if ( TestClass.Cnt2.GetByName( TestClass.Cnt1[ i ].Name ) = nil ) then
      TestClass.Cnt1[ i ].Free;
  while TestClass.Cnt2.Count > 0 do
    begin
      i:= TestClass.Cnt1.FindByName( TestClass.Cnt2[ 0 ].Name );
      if ( i > -1 ) then
        TP3DStreamableContainer( TestClass.Cnt1.Items[ i ]).AsValue:= TestClass.Cnt2[ 0 ]
      else
        TestClass.Cnt2[ 0 ].Owner:= TestClass.Cnt1;
    end;

  json:= TestClass.Cnt1.AsJSON;
  WriteLn( 'TestClass.Cnt1 = ', json.FormatJSON());
  json.Free;

  json:= TestClass.Cnt2.AsJSON;
  WriteLn( 'TestClass.Cnt2 = ', json.FormatJSON());
  json.Free;

  json:= TestClass.Pnt.AsJSON;
  WriteLn( 'TestClass.Pnt = ', json.FormatJSON());
  json.Free;

  //json:= TestClass.Pnt1.AsValue.AsJSON;
  //WriteLn( json.FormatJSON());
  //json.Free;

  TestClass.Free;
  P3DUtilsFinish;
end.

