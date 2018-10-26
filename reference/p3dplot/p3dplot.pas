program p3dplot;

{$MODE DELPHI}

uses sysutils, P3DPlotTest;


var
  List: TSmartObj;
  Itm: TLinkedListItem;
  i: Integer;
begin
  List.Assign( TLinkedList.Create );

  List.Instance.First:= TLinkedListItem.Create( TTestClass.Create( 0 ));
  Itm:= List.Instance.First;
  for i:= 1 to 9 do begin
    Itm.Next:= TLinkedListItem.Create( TTestClass.Create( i ));
    Itm:= Itm.Next;
  end;
  DoTest( List );
  List:= List + DoTest2;
end.

