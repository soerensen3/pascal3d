//p3dnodes.inc
//part of p3dutils.pas
{$IFDEF INTERFACE}

type

  {$MACRO ON}

  TP3DNodeSocketDirection = ( nsdInput, nsdOutput );
  TP3DNode = class;

  { TP3DNodeSocket }

  TP3DNodeSocket = class ( TPersistent )
    protected
      FDirection: TP3DNodeSocketDirection;
      FConnected: TP3DNodeSocket;
      FName: String;
      FNode: TP3DNode;
      FOnConnect: TNotifyEvent;
      FUserData: Pointer;
      class function GetSocketType: String; static; virtual;
      procedure SetConnected(AValue: TP3DNodeSocket); virtual;
      procedure SetNode(AValue: TP3DNode);

    public
      constructor Create( ANode: TP3DNode; ADirection: TP3DNodeSocketDirection ); virtual;
      procedure AcceptConnection( ATarget: TP3DNodeSocket; const Accept: Boolean = False ); virtual;
  { - Procedure to check if socket connection is accepted  }
      class property SocketType: String read GetSocketType;

  { - Can be of various types (Int, Float, Vector, Color, ...)
      The types must be registered the NodeTree
    - Same types can be connected but also some different types
      can be connected like vector and color.
  }
      property Name: String read FName write FName;
      property Connected: TP3DNodeSocket read FConnected write SetConnected;
      property Node: TP3DNode read FNode write SetNode;
      property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
      property UserData: Pointer read FUserData write FUserData;
      property Direction: TP3DNodeSocketDirection read FDirection;
  end;

  TP3DCustomNodeSocketList = specialize gP3DCustomObjectList < TP3DNodeSocket >;

  { TP3DNodeSocketList }

  TP3DNodeSocketList = class ( TP3DCustomNodeSocketList )
    function FindSocketByName( Name: String ): Integer;
  end;

  TP3DNode = class (TPersistent )
    private
      FInputs: TP3DNodeSocketList;
      FName: String;
      FOnChange: TNotifyEvent;
      FOutputs: TP3DNodeSocketList;
      FUserData: Pointer;
      FX: Single;
      FY: Single;
      procedure SetName(AValue: String);
      procedure SetX(AValue: Single);
      procedure SetY(AValue: Single);

      procedure SocketsChange( Sender: TObject ); virtual;

    public
      constructor Create;
      destructor Destroy; override;

      procedure ConnectToNodeSocket(ANode: TP3DNode; SrcSocket, DestSocket: String);

      property Inputs: TP3DNodeSocketList read FInputs;
      property Outputs: TP3DNodeSocketList read FOutputs;
  //- Sockets for Inputs and Outputs
      property Name: String read FName write SetName;
      property X: Single read FX write SetX;
      property Y: Single read FY write SetY;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property UserData: Pointer read FUserData write FUserData;
  end;

  TP3DNodeList = specialize gP3DCustomObjectList < TP3DNode >;

  { TP3DNodeTree }

  TP3DNodeTree = class ( TPersistent ) //NodeTree
    private
      FNodes: TP3DNodeList;

    public
      constructor Create;
      destructor Destroy; override;

    published
      property Nodes: TP3DNodeList read FNodes; // List of all the nodes.
      //NodeTree is also the class to manage the creation and
      //destruction of all the nodes
  { - Technically the outputs of all nodes are strings which are
      merged by NodeTree
    - Multiple inputs/outputs
    - Outputs for ShaderNodes are the shaders text files
    - Contains different sections (Output Names), the Node Tree will look for
      the section in connected nodes}
  end;


{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DNodeSocketList }

function TP3DNodeSocketList.FindSocketByName(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = Name ) then
      begin
        Result:= i;
        break;
      end;
end;

{ TP3DNodeTree }

constructor TP3DNodeTree.Create;
begin
  inherited;
  FNodes:= TP3DNodeList.Create;
end;

destructor TP3DNodeTree.Destroy;
begin
  Nodes.Clear( True );
  inherited Destroy;
end;

{ TP3DNode }

procedure TP3DNode.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

procedure TP3DNode.SetX(AValue: Single);
begin
  if FX=AValue then Exit;
  FX:=AValue;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

procedure TP3DNode.SetY( AValue: Single );
begin
  if FY=AValue then Exit;
  FY:=AValue;
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

procedure TP3DNode.SocketsChange(Sender: TObject);
begin
  if ( Assigned( OnChange )) then
    OnChange( Self );
end;

constructor TP3DNode.Create;
begin
  inherited;
  FInputs:= TP3DNodeSocketList.Create;
  FOutputs:= TP3DNodeSocketList.Create;
  FInputs.OnChange:= @SocketsChange;
  FOutputs.OnChange:= @SocketsChange;
end;

destructor TP3DNode.Destroy;
begin
  Inputs.Free;
  Outputs.Free;
  inherited Destroy;
end;

procedure TP3DNode.ConnectToNodeSocket( ANode: TP3DNode; SrcSocket, DestSocket: String );
var
  Socket1n, Socket2n: Integer;
  Socket1, Socket2: TP3DNodeSocket;
begin
  Socket1n:= Inputs.FindSocketByName( SrcSocket );

  if ( Socket1n < 0 ) then
    Socket1n:= Outputs.FindSocketByName( SrcSocket )
  else
    Socket1:= Inputs[ Socket1n ];

  if ( Socket1n < 0 ) then
    raise Exception.Create( 'Cannot connect to socket: Source socket "' + SrcSocket + '" not found!' )
  else
    Socket1:= Outputs[ Socket1n ];

  if ( Socket1.Direction = nsdInput ) then
    begin
      Socket2n:= ANode.Outputs.FindSocketByName( DestSocket );
      Socket2:= ANode.Outputs[ Socket1n ];
    end
  else
    begin
      Socket2n:= ANode.Inputs.FindSocketByName( DestSocket );
      Socket2:= ANode.Inputs[ Socket1n ];
    end;

  if ( Socket2n < 0 ) then
    raise Exception.Create( 'Cannot connect to socket: Destination socket "' + DestSocket + '" not found!' );

  Socket1.Connected:= Socket2;
end;

{ TP3DNodeSocket }

class function TP3DNodeSocket.GetSocketType: String; static;
begin
  Result:= 'Null';
end;

procedure TP3DNodeSocket.SetConnected(AValue: TP3DNodeSocket);
begin
  if FConnected=AValue then Exit;
  FConnected:=AValue;
  if ( Assigned( OnConnect )) then
    OnConnect( Self );
end;

procedure TP3DNodeSocket.SetNode(AValue: TP3DNode);
begin
  if FNode=AValue then Exit;
  FNode:=AValue;
end;

constructor TP3DNodeSocket.Create(ANode: TP3DNode;
			ADirection: TP3DNodeSocketDirection);
begin
  inherited Create;
  FDirection:= ADirection;
  FNode:= ANode;
end;

procedure TP3DNodeSocket.AcceptConnection(ATarget: TP3DNodeSocket;
  const Accept: Boolean);
begin

end;

{$ENDIF}

