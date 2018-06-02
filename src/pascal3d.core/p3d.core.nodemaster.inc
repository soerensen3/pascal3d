{ p3d.nodemaster.inc }

{$IFDEF INTERFACE}
type
  TP3DNodeMasterHandlerType = (
    nmhtNodeAdd,
    nmhtNodeRemove,
    nmhtNodeUpdatePosition,
    nmhtNodeClearSockets,
    nmhtSocketAdd,
    nmhtSocketRemove,
    nmhtSocketSetValue,
    nmhtSocketSetConnected
  );

  TP3DNodeMasterNodeHandler = procedure ( Node: TP3DNodeClone ) of object;
  TP3DNodeMasterSocketHandler = procedure ( Socket: TP3DNodeSocketClone ) of object;

  { TP3DNodeMaster }

  TP3DNodeMaster = class ( TObject )
    protected
      FHandlers: array[ TP3DNodeMasterHandlerType ] of TMethodList;

      procedure AddHandler( HandlerType: TP3DNodeMasterHandlerType;
                            const AMethod: TMethod; AsLast: Boolean = False );
      procedure RemoveHandler( HandlerType: TP3DNodeMasterHandlerType;
                               const AMethod: TMethod );

      procedure DoCallNodeHandlers( HandlerType: TP3DNodeMasterHandlerType; Node: TP3DNodeClone );
      procedure DoCallSocketHandlers( HandlerType: TP3DNodeMasterHandlerType; Socket: TP3DNodeSocketClone );

    public
      constructor Create;
      destructor Destroy; override;

      procedure AddNodeAddHandler( const AHandler: TP3DNodeMasterNodeHandler; AsLast: Boolean = False );
      procedure RemoveNodeAddHandler( const AHandler: TP3DNodeMasterNodeHandler );
      procedure AddNodeRemoveHandler( const AHandler: TP3DNodeMasterNodeHandler; AsLast: Boolean = False );
      procedure RemoveNodeRemoveHandler( const AHandler: TP3DNodeMasterNodeHandler );

      procedure AddNodeSocketsClearHandler( const AHandler: TP3DNodeMasterNodeHandler; AsLast: Boolean = False );
      procedure RemoveNodeSocketsClearHandler( const AHandler: TP3DNodeMasterNodeHandler );

      procedure NodeClearSockets( ANode: TP3DNodeClone );

      procedure AddNode( ANode: TP3DNodeClone );
      procedure RemoveNode( ANode: TP3DNodeClone );

      procedure AddNodeUpdatePositionHandler( const AHandler: TP3DNodeMasterNodeHandler; AsLast: Boolean = False );
      procedure RemoveNodeUpdatePositionHandler( const AHandler: TP3DNodeMasterNodeHandler );
      procedure UpdateNodePosition( ANode: TP3DNodeClone );

      procedure AddSocketAddHandler( const AHandler: TP3DNodeMasterSocketHandler; AsLast: Boolean = False );
      procedure RemoveSocketAddHandler( const AHandler: TP3DNodeMasterSocketHandler );
      procedure AddSocketRemoveHandler( const AHandler: TP3DNodeMasterSocketHandler; AsLast: Boolean = False );
      procedure RemoveSocketRemoveHandler( const AHandler: TP3DNodeMasterSocketHandler );

      procedure AddSocket( ASocket: TP3DNodeSocketClone );
      procedure RemoveSocket( ASocket: TP3DNodeSocketClone );

      procedure AddSocketSetValueHandler( const AHandler: TP3DNodeMasterSocketHandler; AsLast: Boolean = False );
      procedure RemoveSocketSetValueHandler( const AHandler: TP3DNodeMasterSocketHandler );
      procedure SetSocketValue( ASocket: TP3DNodeSocketClone );

      procedure AddSocketSetConnectedHandler( const AHandler: TP3DNodeMasterSocketHandler; AsLast: Boolean = False );
      procedure RemoveSocketSetConnectedHandler( const AHandler: TP3DNodeMasterSocketHandler );
      procedure SetSocketConnected( ASocket: TP3DNodeSocketClone );
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DNodeMaster }

procedure TP3DNodeMaster.AddHandler(HandlerType: TP3DNodeMasterHandlerType; const AMethod: TMethod; AsLast: Boolean );
begin
  if ( FHandlers[ HandlerType ] = nil ) then
    FHandlers[ HandlerType ]:= TMethodList.Create;
  FHandlers[ HandlerType ].Add( AMethod, AsLast );
end;

procedure TP3DNodeMaster.RemoveHandler(HandlerType: TP3DNodeMasterHandlerType; const AMethod: TMethod);
begin
  FHandlers[ HandlerType ].Remove( AMethod );
end;

procedure TP3DNodeMaster.DoCallNodeHandlers(HandlerType: TP3DNodeMasterHandlerType; Node: TP3DNodeClone);
var
  i: LongInt;
  m: TMethodList;
begin
  m:= FHandlers[ HandlerType ];
  if ( not Assigned( m )) then
    exit;
  i:= m.Count;
  while m.NextDownIndex( i ) do
    TP3DNodeMasterNodeHandler( m[ i ])( Node );
end;

procedure TP3DNodeMaster.DoCallSocketHandlers(HandlerType: TP3DNodeMasterHandlerType;
  Socket: TP3DNodeSocketClone);
var
  i: LongInt;
  m: TMethodList;
begin
  m:= FHandlers[ HandlerType ];
  if ( not Assigned( m )) then
    exit;
  i:= m.Count;
  while m.NextDownIndex( i ) do
    TP3DNodeMasterSocketHandler( m[ i ])( Socket );
end;

constructor TP3DNodeMaster.Create;
begin
  FillByte( FHandlers, SizeOf( FHandlers ), 0 );
end;

destructor TP3DNodeMaster.Destroy;
var
  h: TP3DNodeMasterHandlerType;
begin
  for h:= Low( FHandlers ) to High( FHandlers ) do
    FreeAndNil( FHandlers[ h ]);
  inherited Destroy;
end;

procedure TP3DNodeMaster.AddNodeAddHandler(const AHandler: TP3DNodeMasterNodeHandler; AsLast: Boolean);
begin
  AddHandler( nmhtNodeAdd, TMethod( AHandler ), AsLast );
end;

procedure TP3DNodeMaster.RemoveNodeAddHandler(const AHandler: TP3DNodeMasterNodeHandler);
begin
  RemoveHandler( nmhtNodeRemove, TMethod( AHandler ));
end;

procedure TP3DNodeMaster.AddNodeRemoveHandler(const AHandler: TP3DNodeMasterNodeHandler; AsLast: Boolean);
begin
  AddHandler( nmhtNodeRemove, TMethod( AHandler ), AsLast );
end;

procedure TP3DNodeMaster.RemoveNodeRemoveHandler(const AHandler: TP3DNodeMasterNodeHandler);
begin
  RemoveHandler( nmhtNodeRemove, TMethod( AHandler ));
end;

procedure TP3DNodeMaster.AddNodeSocketsClearHandler(
  const AHandler: TP3DNodeMasterNodeHandler; AsLast: Boolean);
begin
  AddHandler( nmhtNodeClearSockets, TMethod( AHandler ), AsLast );
end;

procedure TP3DNodeMaster.RemoveNodeSocketsClearHandler(
  const AHandler: TP3DNodeMasterNodeHandler);
begin
  RemoveHandler( nmhtNodeClearSockets, TMethod( AHandler ));
end;

procedure TP3DNodeMaster.NodeClearSockets(ANode: TP3DNodeClone);
begin
  DoCallNodeHandlers( nmhtNodeClearSockets, ANode );
end;

procedure TP3DNodeMaster.AddNode(ANode: TP3DNodeClone);
begin
  DoCallNodeHandlers( nmhtNodeAdd, ANode );
  ANode.NodeMaster:= Self;
end;

procedure TP3DNodeMaster.RemoveNode(ANode: TP3DNodeClone);
begin
  if ( Assigned( ANode )) then
    ANode.NodeMaster:= nil;
  DoCallNodeHandlers( nmhtNodeRemove, ANode );
end;

procedure TP3DNodeMaster.AddNodeUpdatePositionHandler(const AHandler: TP3DNodeMasterNodeHandler;
  AsLast: Boolean);
begin
  AddHandler( nmhtNodeUpdatePosition, TMethod( AHandler ), AsLast );
end;

procedure TP3DNodeMaster.RemoveNodeUpdatePositionHandler(const AHandler: TP3DNodeMasterNodeHandler);
begin
  RemoveHandler( nmhtNodeUpdatePosition, TMethod( AHandler ));
end;

procedure TP3DNodeMaster.UpdateNodePosition(ANode: TP3DNodeClone);
begin
  DoCallNodeHandlers( nmhtNodeUpdatePosition, ANode );
end;

procedure TP3DNodeMaster.AddSocketAddHandler(const AHandler: TP3DNodeMasterSocketHandler; AsLast: Boolean);
begin
  AddHandler( nmhtSocketAdd, TMethod( AHandler ), AsLast );
end;

procedure TP3DNodeMaster.RemoveSocketAddHandler(const AHandler: TP3DNodeMasterSocketHandler);
begin
  RemoveHandler( nmhtSocketAdd, TMethod( AHandler ));
end;

procedure TP3DNodeMaster.AddSocketRemoveHandler(const AHandler: TP3DNodeMasterSocketHandler; AsLast: Boolean );
begin
  AddHandler( nmhtSocketRemove, TMethod( AHandler ), AsLast );
end;

procedure TP3DNodeMaster.RemoveSocketRemoveHandler(const AHandler: TP3DNodeMasterSocketHandler);
begin
  RemoveHandler( nmhtSocketRemove, TMethod( AHandler ));
end;

procedure TP3DNodeMaster.AddSocket(ASocket: TP3DNodeSocketClone);
begin
  DoCallSocketHandlers( nmhtSocketAdd, ASocket );
  ASocket.NodeMaster:= Self;
end;

procedure TP3DNodeMaster.RemoveSocket(ASocket: TP3DNodeSocketClone);
begin
  if ( Assigned( ASocket )) then
    ASocket.NodeMaster:= nil;

  DoCallSocketHandlers( nmhtSocketRemove, ASocket );
end;

procedure TP3DNodeMaster.AddSocketSetValueHandler(const AHandler: TP3DNodeMasterSocketHandler;
  AsLast: Boolean);
begin
  AddHandler( nmhtSocketSetValue, TMethod( AHandler ), AsLast );
end;

procedure TP3DNodeMaster.RemoveSocketSetValueHandler(const AHandler: TP3DNodeMasterSocketHandler);
begin
  RemoveHandler( nmhtSocketSetValue, TMethod( AHandler ));
end;

procedure TP3DNodeMaster.SetSocketValue(ASocket: TP3DNodeSocketClone);
begin
  DoCallSocketHandlers( nmhtSocketSetValue, ASocket );
end;

procedure TP3DNodeMaster.AddSocketSetConnectedHandler(const AHandler: TP3DNodeMasterSocketHandler;
  AsLast: Boolean);
begin
  AddHandler( nmhtSocketSetConnected, TMethod( AHandler ), AsLast );
end;

procedure TP3DNodeMaster.RemoveSocketSetConnectedHandler(const AHandler: TP3DNodeMasterSocketHandler);
begin
  RemoveHandler( nmhtSocketSetConnected, TMethod( AHandler ));
end;

procedure TP3DNodeMaster.SetSocketConnected(ASocket: TP3DNodeSocketClone);
begin
  DoCallSocketHandlers( nmhtSocketSetConnected, ASocket );
end;

{$ENDIF}

