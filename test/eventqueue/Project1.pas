program Project1;

uses
  Classes,
  SysUtils;

type
  TP3DNodeEventType = ( netNode, netSocket );
  PP3DNodeEvent = ^TP3DNodeEvent;
  TP3DNodeEvent = record
    Next: PP3DNodeEvent;
    Obj: TObject;
    Method: TNotifyEvent;
  end;

  { TTestClass }

  TTestClass = class
    private type

      { TP3DNodeEventHandler }

      TP3DNodeEventHandler = object
        private
          FFirst: PP3DNodeEvent;
          FLast: PP3DNodeEvent;

        public
          property First: PP3DNodeEvent read FFirst write FFirst;
          property Last: PP3DNodeEvent read FLast write FLast;

          procedure PushEvent(AMethod: TNotifyEvent; Obj: TObject);
          function HandleFirstEvent: Boolean;
      end;
    public
      EH: TP3DNodeEventHandler;

      constructor Create;

      procedure HandleEvents;

      procedure AddNode( Node: TObject );
      procedure RemoveNode( Node: TObject );
      procedure AddSocket( Socket: TObject );
      procedure RemoveSocket( Socket: TObject );

      procedure AddNodeEvent( Node: TObject );
      procedure RemoveNodeEvent( Node: TObject );
      procedure AddSocketEvent( Socket: TObject );
      procedure RemoveSocketEvent( Socket: TObject );

  end;

var
  TC: TTestClass;

{ TTestClass.TP3DNodeEventHandler }

procedure TTestClass.TP3DNodeEventHandler.PushEvent(AMethod: TNotifyEvent; Obj: TObject);
var
  Event: PP3DNodeEvent;
begin
  WriteLn( 'Pushin Event' );
  Event:= GetMem( SizeOf( TP3DNodeEvent) );
  Event^.Method:= AMethod;
  Event^.Next:= nil;
  Event^.Obj:= Obj;
  if ( Assigned( FLast )) then
    FLast^.Next:= Event
  else if ( Assigned( First )) then
    First^.Next:= Event
  else
    First:= Event;
  Last:= Event;
end;

function TTestClass.TP3DNodeEventHandler.HandleFirstEvent: Boolean;
var
  _First: PP3DNodeEvent;
begin
  if ( not Assigned( First )) then
    exit( False );

  Result:= True;

  if ( Last = First ) then
    Last:= nil;
  First^.Method( First^.Obj );
  _First:= First;
  First:= First^.Next;
  Freemem( _First );
end;

{ TTestClass }

constructor TTestClass.Create;
begin

end;

procedure TTestClass.HandleEvents;
var
  n: Integer;
begin
  n:= 0;
  while EH.HandleFirstEvent do
    Inc( n );
  WriteLn( 'Handled ', n, ' events' );
end;

procedure TTestClass.AddNode(Node: TObject);
begin
  WriteLn( 'Add Node' );
end;

procedure TTestClass.RemoveNode(Node: TObject);
begin
  WriteLn( 'Remove Node' );
end;

procedure TTestClass.AddSocket(Socket: TObject);
begin
  WriteLn( 'Add Socket' );
end;

procedure TTestClass.RemoveSocket(Socket: TObject);
begin
  WriteLn( 'Remove Socket' );
end;

procedure TTestClass.AddNodeEvent(Node: TObject);
begin
  EH.PushEvent( @AddNode, Node );
end;

procedure TTestClass.RemoveNodeEvent(Node: TObject);
begin
  EH.PushEvent( @RemoveNode, Node );
end;

procedure TTestClass.AddSocketEvent(Socket: TObject);
begin
  EH.PushEvent( @AddSocket, Socket );
end;

procedure TTestClass.RemoveSocketEvent(Socket: TObject);
begin
  EH.PushEvent( @RemoveSocket, Socket );
end;

var
  C: Integer;
  HE: Boolean = False;

begin
  TC:= TTestClass.Create;

  repeat
    if ( HE ) then
      TC.HandleEvents;
    readln( C );
    case C of
      1:  TC.EH.PushEvent( @TC.AddNode, nil );
      2:  TC.EH.PushEvent( @TC.RemoveNode, nil );
      3:  TC.EH.PushEvent( @TC.AddSocket, nil );
      4:  TC.EH.PushEvent( @TC.RemoveSocket, nil );
      5: HE:= not HE;
    end;
  until C = 0;

  TC.Free;
end.

