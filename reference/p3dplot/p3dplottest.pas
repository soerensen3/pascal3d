{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by Maciej Izak aka hnb (NewPascal project)
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

unit P3DPlotTest;

{$MODE DELPHI}

interface

uses
  SysUtils, Classes;

type

  TLinkedList = class;

  TSmartObj = record
    // similar as overloading [] operators for property x[v: string]: integer read gx write sx; default;
    Instance: TLinkedList;// default; // default keyword for non property.
    RefCount: PLongint;

    procedure SmartFinalize();

    class operator Initialize(var aRec: TSmartObj );
    class operator Finalize(var aRec: TSmartObj );
    class operator AddRef(var aRec: TSmartObj );
    class operator Copy(constref aSource: TSmartObj; var aDest: TSmartObj );

    // implicit or explicit operator is used before "default" field
    //class operator Implicit(aObj: TLinkedList): TSmartObj;
    class operator Add( a, b: TSmartObj ): TSmartObj;
    procedure Assign(const aValue: TLinkedList);
  end;

  TTestClass = class
    index: Integer;
    constructor Create( i: Integer );
    destructor Destroy; override;
    function Clone: TTestClass;
  end;

  { TLinkedListItem }

  TLinkedListItem = class
    Obj: TTestClass;
    Next: TLinkedListItem;

    constructor Create( AObj: TTestClass );
    destructor Destroy; override;
  end;

  TLinkedList = class
    First: TLinkedListItem;

    procedure Concatenate( AList: TSmartObj );
    destructor Destroy; override;
  end;

  procedure DoTest( var aList: TSmartObj );
  function DoTest2: TSmartObj;

implementation


procedure TLinkedList.Concatenate(AList: TSmartObj);
var
  p,p2: TLinkedListItem;
begin
  p:= First;
  while( Assigned( p.Next )) do
    p:= p.Next;

  if ( Assigned( p )) then begin
    p2:= AList.Instance.First;
    while( Assigned( p2 )) do begin
      p.Next:= TLinkedListItem.Create( p2.Obj.Clone );
      p:= p.Next;
      p2:= p2.Next;
    end;
  end;
end;

destructor TLinkedList.Destroy;
begin
  FreeAndNil( First );
  inherited Destroy;
end;

{ TLinkedListItem }

constructor TLinkedListItem.Create(AObj: TTestClass);
begin
  Obj:= AObj;
end;

destructor TLinkedListItem.Destroy;
begin
  FreeAndNil( Next );
  FreeAndNil( Obj );
  inherited Destroy;
end;

{ TTestClass }

constructor TTestClass.Create(i: Integer);
begin
  index:= i;
  WriteLn( 'Creating class ', index );
end;

destructor TTestClass.Destroy;
begin
  WriteLn( 'Destroying class ', index );
  inherited Destroy;
end;

function TTestClass.Clone: TTestClass;
begin
  Result:= TTestClass.Create( index );
end;

procedure DoTest( var aList: TSmartObj );
var
  P: TLinkedListItem;
begin
  P:= aList.Instance.First;
  while Assigned( P ) do begin
    WriteLn( 'Test ', P.Obj.index );
    P:= P.Next;
  end;
end;

function DoTest2: TSmartObj;
var
  Itm: TLinkedListItem;
  i: Integer;
begin
  Result.Assign( TLinkedList.Create );
  Result.Instance.First:= TLinkedListItem.Create( TTestClass.Create( 10 ));
  Itm:= Result.Instance.First;
  for i:= 1 to 9 do begin
    Itm.Next:= TLinkedListItem.Create( TTestClass.Create( i + 10 ));
    Itm:= Itm.Next;
  end;
end;

procedure TSmartObj.SmartFinalize();
begin
  if RefCount <> nil then
    if InterLockedDecrement(RefCount^)=0 then
    begin
      FreeMem(RefCount);
      Instance.Free;
    end;
end;

class operator TSmartObj.Initialize(var aRec: TSmartObj);
begin
  aRec.RefCount := nil;
end;

class operator TSmartObj.Finalize(var aRec: TSmartObj);
begin
  aRec.SmartFinalize();
end;

class operator TSmartObj.AddRef(var aRec: TSmartObj);
begin
  if aRec.RefCount <> nil then
    InterLockedIncrement(aRec.RefCount^);
end;

class operator TSmartObj.Copy(constref aSource: TSmartObj; var aDest: TSmartObj);
begin
  if aDest.RefCount <> nil then
    aDest.SmartFinalize();
  if aSource.RefCount <> nil then
    InterLockedIncrement(aSource.RefCount^);
  aDest.RefCount := aSource.RefCount;
  aDest.Instance := aSource.Instance;
end;
{
class operator TSmartObj.Implicit(aObj: T): TSmartObj;
begin
  Result.Assign(aObj);
end;
}
procedure TSmartObj.Assign(const aValue: TLinkedList);
begin
  if RefCount <> nil then
    SmartFinalize();

  GetMem(RefCount, SizeOf(LongInt));
  RefCount^ := 0;

  InterLockedIncrement(RefCount^);
  Instance := aValue;
end;

class operator TSmartObj.Add( a, b: TSmartObj): TSmartObj;
begin
  a.Instance.Concatenate( b );
  Result:= a;
end;

end.
