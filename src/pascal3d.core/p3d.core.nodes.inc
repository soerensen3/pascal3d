//p3dnodes.inc
//part of p3dutils.pas
{$IFDEF INTERFACE}

type

  {$MACRO ON}

  TP3DNodeSocketDirection = ( nsdInput, nsdOutput );
  TP3DNode = class;
  TP3DNodeSocketClone = class;
  TP3DNodeMaster = class;

  { TP3DNodeSocket }

  TP3DNodeSocket = class ( TP3DNamedStreamable )
    protected type
      TP3DNodeSocketCloneClass = class of TP3DNodeSocketClone;

    protected
      FVisible: Boolean;
      FDefault: String;
      FDirection: TP3DNodeSocketDirection;
      FNode: TP3DNode;
      FSocketType: String;

      procedure SetNode(AValue: TP3DNode);
      function GetSocketClass: TP3DNodeSocketCloneClass; virtual;

    public
      //constructor Create( ANode: TP3DNode; ADirection: TP3DNodeSocketDirection ); virtual;
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
  { - Can be of various types (Int, Float, Vector, Color, ...)
      The types must be registered the NodeTree
    - Same types can be connected but also some different types
      can be connected like vector and color.
  }
      function Clone( AOwner: IP3DStreamableContainer ): TP3DNodeSocketClone;

      property Node: TP3DNode read FNode write SetNode;

    published
      property Direction: TP3DNodeSocketDirection read FDirection; // Either input or output
      property Visible: Boolean read FVisible write FVisible; // Is used by the node editor only.
      property Default: String read FDefault write FDefault; // Is used by the node editor only.
      property SocketType: String read FSocketType;
  end;

  TP3DNodeClone = class;

  { TP3DNodeSocketClone }

  TP3DNodeSocketClone = class ( TP3DNamedStreamable )
    protected
      FNode: TP3DStreamablePointer;
      FValueProp: TP3DPropertyAccess;
      FUserData: Pointer;
      FConnected: TP3DStreamablePointer;
      FOnConnect: TNotifyEvent;
      FCloneOf: TP3DStreamablePointer;
      FUpdating: Boolean;
      FNodeMaster: TP3DNodeMaster;

      function GetNode: TP3DNodeClone;
      procedure ChangeCloneOf( ANewValue: TP3DStreamable );
      function GetCloneOf: TP3DNodeSocket;
      procedure SetConnected(AValue: TP3DNodeSocketClone); virtual;
      function GetConnected: TP3DNodeSocketClone;
      procedure SetValueProp( AValue: TP3DPropertyAccess );
      { - Procedure to check if socket connection is accepted  }
      procedure AcceptConnection( ATarget: TP3DNodeSocket; const Accept: Boolean = False ); virtual;
      procedure ChangePropValue( Sender: TObject );
      function GetName: String; override;
      procedure SetName(AValue: String); override;
      function GetIsDefault: Boolean;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor CreateFromJSON(AJSON: TJSONData; const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      property Connected: TP3DNodeSocketClone read GetConnected write SetConnected;
      property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
      property UserData: Pointer read FUserData write FUserData;
      property CloneOf: TP3DNodeSocket read GetCloneOf;
      property ValueProp: TP3DPropertyAccess read FValueProp write SetValueProp;
      property Node: TP3DNodeClone read GetNode;

      property NodeMaster: TP3DNodeMaster read FNodeMaster write FNodeMaster;
      property IsDefault: Boolean read GetIsDefault;
  end;

  TP3DNodeSocketContainerList = specialize gP3DNamedStreamableContainerList < TP3DNodeSocket >;
  TP3DNodeSocketPointerList = specialize gP3DStreamablePointerList < TP3DNodeSocket >;

  { TP3DNodeSocketCloneContainerList }

  TP3DNodeSocketCloneContainerList = class ( specialize gP3DNamedStreamableContainerList < TP3DNodeSocketClone >)
    public
      function SaveToJSON: TJSONData; override;
      procedure LoadJSON(AValue: TJSONData); override;
      procedure LoadItemFromJSON(Index: Integer; AData: TJSONData); override;

      procedure DisconnectAll;
  end;
  TP3DNodeSocketClonePointerList = specialize gP3DStreamablePointerList < TP3DNodeSocketClone >;

  TP3DNodeCloneClass = class of TP3DNodeClone;

  TP3DNode = class ( TP3DNamedStreamable )
    protected
      FInputs: TP3DNodeSocketContainerList;
      FOutputs: TP3DNodeSocketContainerList;

      property Inputs: TP3DNodeSocketContainerList read FInputs;
      property Outputs: TP3DNodeSocketContainerList read FOutputs;

      procedure SocketsChange( ASender: TObject; AItem: TP3DStreamable; Action: TP3DListOnChangeAction );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

      function Clone( AOwner: IP3DStreamableContainer; const AClass: TP3DNodeCloneClass = nil ): TP3DNodeClone; virtual;
  end;

  { TP3DNodeClone }

  TP3DNodeClone = class( TP3DStreamable )
    protected
      FNodeMaster: TP3DNodeMaster;
      FInputs: TP3DNodeSocketCloneContainerList;
      FOutputs: TP3DNodeSocketCloneContainerList;

      FUserData: Pointer;
      FX: Single;
      FY: Single;
      FOnChange: TNotifyEvent;
      FCloneOf: TP3DStreamablePointer;

      procedure SetNodeMaster(AValue: TP3DNodeMaster);
      procedure SetX(AValue: Single);
      procedure SetY(AValue: Single);
      function GetCloneOf: TP3DNode;
      procedure ChangeCloneOf( ANewValue: TP3DStreamable ); virtual;

      procedure SocketsChange( Sender: TObject; Item: TP3DStreamable; Action: TP3DListOnChangeAction ); virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      constructor CreateFromJSON(AJSON: TJSONData; const AOwner: IP3DStreamableContainer=nil); override;

      procedure ConnectToNodeSocket(ANode: TP3DNodeClone; SrcSocket, DestSocket: String);
      function FindInput( AName: String ): TP3DNodeSocketClone;
      function FindInputIndex( AName: String ): Integer;
      function FindOutput( AName: String ): TP3DNodeSocketClone;
      function FindOutputIndex( AName: String ): Integer;

      property Inputs: TP3DNodeSocketCloneContainerList read FInputs;
      property Outputs: TP3DNodeSocketCloneContainerList read FOutputs;
  //- Sockets for Inputs and Outputs
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property UserData: Pointer read FUserData write FUserData;
      property CloneOf: TP3DNode read GetCloneOf;
      property NodeMaster: TP3DNodeMaster read FNodeMaster write SetNodeMaster;

    published
      property X: Single read FX write SetX;
      property Y: Single read FY write SetY;
  end;

  { TP3DNodeList }

  TP3DNodeContainerList = class( specialize gP3DNamedStreamableContainerList < TP3DNode >)
    public
      procedure SortByName;
  end;

  TP3DNodeCloneContainerList = specialize gP3DStreamableContainerList < TP3DNodeClone >;
  TP3DNodeClonePointerList = specialize gP3DStreamablePointerList < TP3DNodeClone >;

  TP3DNodePointerList = specialize gP3DStreamablePointerList < TP3DNode >;

  {TP3DNodeSocketString = class ( TP3DNodeSocketClone )
    public
      function GetOutput( Buffer: TP3DShaderBuffer ): String; override;

      property Value: String read GetValue;
  end;}




{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DNode }

procedure TP3DNode.SocketsChange(ASender: TObject; AItem: TP3DStreamable;
  Action: TP3DListOnChangeAction);
begin
  if (( Action = actAdd ) and ( AItem is TP3DNodeSocket )) then
    TP3DNodeSocket( AItem ).Node:= Self;
end;

constructor TP3DNode.Create(const AOwner: IP3DStreamableContainer);
begin
  FInputs:= TP3DNodeSocketContainerList.Create( 'Inputs', smText );
  FOutputs:= TP3DNodeSocketContainerList.Create( 'Outputs', smText );
  inherited Create( AOwner );
  FInputs.OnChange:= @SocketsChange;
  FOutputs.OnChange:= @SocketsChange;
  Properties.Add([ FInputs, FOutputs ]);
end;

function TP3DNode.Clone(AOwner: IP3DStreamableContainer; const AClass: TP3DNodeCloneClass): TP3DNodeClone;
begin
  if ( not Assigned( AClass )) then
    Result:= TP3DNodeClone.Create( AOwner )
  else
    Result:= AClass.Create( AOwner );
  Result.FCloneOf.AsValue:= Self;
end;

{ TP3DNodeClone }

constructor TP3DNodeClone.Create(const AOwner: IP3DStreamableContainer);
begin
  FInputs:= TP3DNodeSocketCloneContainerList.Create( 'Inputs', smText );
  FOutputs:= TP3DNodeSocketCloneContainerList.Create( 'Outputs' );
  FCloneOf:= TP3DStreamablePointer.Create( 'CloneOf', TP3DNode, smText );
  FCloneOf.OnChange:= @ChangeCloneOf;
  inherited Create( AOwner );
  Properties.Add([ FInputs, FOutputs ]);
  FInputs.OnChange:= @SocketsChange;
  FOutputs.OnChange:= @SocketsChange;
  Properties.Add( FCloneOf );
  Properties.Add([ TP3DPropAccessRTTI.Create( Self, 'X', smText ), TP3DPropAccessRTTI.Create( Self, 'Y', smText )]);
  if ( Assigned( CloneOf )) then
    ChangeCloneOf( CloneOf );
end;

constructor TP3DNodeClone.CreateFromJSON(AJSON: TJSONData; const AOwner: IP3DStreamableContainer);
var
  path: TJSONUnicodeStringType;
  prop, O: TP3DPropertyAccess;
  obj: TObject;
begin
  Create( AOwner );
  if ( AJSON is TJSONObject ) then begin
    path:= TJSONObject( AJSON ).Get( 'CloneOf', '' );
    prop:= Root.GetJSONLoader().GetPropByPath( path );
    if ( not Assigned( prop )) then
      raise Exception.CreateFmt( 'The property "%s" was not found in "%s"!', [ path, Root.GetJSONLoader.FileName ]);
    FCloneOf.Assign( prop );
    AsJSON:= AJSON;
  end;
end;

procedure TP3DNodeClone.ConnectToNodeSocket( ANode: TP3DNodeClone; SrcSocket, DestSocket: String );
var
  SocketInp, SocketOutp: TP3DNodeSocketClone;
begin
  SocketInp:= FindInput( SrcSocket );
  if ( not Assigned( SocketInp )) then
    begin
      SocketOutp:= FindOutput( SrcSocket );
      if ( not Assigned( SocketOutp )) then
        raise Exception.Create( 'Cannot connect to socket: Source socket "' + SrcSocket + '" not found!' );
      SocketInp:= ANode.FindInput( DestSocket );
      if ( not Assigned( SocketInp )) then
        raise Exception.Create( 'Cannot connect to socket: Dest socket "' + DestSocket + '" not found!' );
    end
  else
    begin
      SocketOutp:= ANode.FindOutput( DestSocket );
      if ( not Assigned( SocketOutp )) then
        raise Exception.Create( 'Cannot connect to socket: Dest socket "' + DestSocket + '" not found!' );
    end;
  SocketInp.Connected:= SocketOutp;
end;

function TP3DNodeClone.FindInput(AName: String): TP3DNodeSocketClone;
var
  i: Integer;
begin
  Result:= nil;
  if ( not Assigned( Inputs )) then
    exit;
  for i:= 0 to Inputs.Count - 1 do
    if ( Inputs[ i ].Name = AName ) then
      begin
        Result:= Inputs[ i ];
        break;
      end;
end;

function TP3DNodeClone.FindInputIndex(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  if ( not Assigned( Inputs )) then
    exit;
  for i:= 0 to Inputs.Count - 1 do
    if ( Inputs[ i ].Name = AName ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DNodeClone.FindOutput(AName: String): TP3DNodeSocketClone;
var
  i: Integer;
begin
  Result:= nil;
  for i:= 0 to Outputs.Count - 1 do
    if ( Outputs[ i ].Name = AName ) then
      begin
        Result:= Outputs[ i ];
        break;
      end;
end;

function TP3DNodeClone.FindOutputIndex(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Outputs.Count - 1 do
    if ( Outputs[ i ].Name = AName ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DNodeClone.GetCloneOf: TP3DNode;
begin
  if ( not Assigned( FCloneOf )) then
    raise Exception.Create( ClassName );
  Result:= TP3DNode( FCloneOf.AsValue );
end;

procedure TP3DNodeClone.ChangeCloneOf(ANewValue: TP3DStreamable);
var
  Socket: TP3DNodeSocket;
  SocketC, SocketC2: TP3DNodeSocketClone;
  jinputs: TJSONData = nil;
  Marker, n: Integer;
begin
  if ( Assigned( CloneOf ) and Assigned( ANewValue )) then
    if ( CloneOf.Name = TP3DNode( ANewValue ).Name ) then begin
      jinputs:= Inputs.SaveToJSON;
      {for SocketC in Outputs do begin
        SocketC2:= TP3DNode( ANewValue ).Outputs.GetByName( Socket.Name );
        Socket.Users.UpdateAllUsers( Socket, Socket2 );
      end;}
    end;
  Inputs.Clear;
  //Outputs.Clear;
  if ( ANewValue is TP3DNode ) then
    begin
      for Socket in TP3DNode( ANewValue ).Inputs do
        Socket.Clone( Inputs ).FNode.AsValue:= Self;

      Marker:= Outputs.Count;
      for Socket in TP3DNode( ANewValue ).Outputs do begin
        SocketC:= FindOutput( Socket.Name );
        SocketC2:= Socket.Clone( Outputs );
        if ( Assigned( SocketC )) then begin
          SocketC.Users.UpdateAllUsers( SocketC, SocketC2 );
          SocketC.Free;
        end;
        SocketC2.FNode.AsValue:= Self;
      end;
    end;
  if ( Assigned( jinputs )) then try
    WriteLn( CloneOf.Name, '.Inputs <- ', jinputs.AsJSON );
    Inputs.AsJSON:= jinputs;
  finally
    jinputs.Free;
  end;
end;

procedure TP3DNodeClone.SetNodeMaster(AValue: TP3DNodeMaster);
var
  S: TP3DNodeSocketClone;
begin
  //if FNodeMaster=AValue then Exit;
  FNodeMaster:=AValue;
  for S in Inputs do
    S.NodeMaster:= NodeMaster;
  for S in Outputs do
    S.NodeMaster:= NodeMaster;
end;

procedure TP3DNodeClone.SetX(AValue: Single);
begin
  if FX=AValue then Exit;
  FX:=AValue;
  //WriteLn( 'Setting node ' + CloneOf.Name + ' X to ', FloatToStr( AValue ));
  {if ( Assigned( OnChange )) then
    OnChange( Self );}
end;

procedure TP3DNodeClone.SetY( AValue: Single );
begin
  if FY=AValue then Exit;
  FY:=AValue;
  //WriteLn( 'Setting node ' + CloneOf.Name + ' Y to ', FloatToStr( AValue ));
  {if ( Assigned( OnChange )) then
    OnChange( Self );}
end;

procedure TP3DNodeClone.SocketsChange(Sender: TObject; Item: TP3DStreamable; Action: TP3DListOnChangeAction);
var
  S: TP3DNodeSocketClone absolute Item;
begin
  if ( Assigned( OnChange )) then
    OnChange( Self );

  if ( Assigned( NodeMaster )) then
    case Action of
      actAdd: begin NodeMaster.AddSocket( S ); S.FNode.AsValue:= Self; end;
      actDelete,actExtract: NodeMaster.RemoveSocket( S );
      actClear: NodeMaster.NodeClearSockets( Self );
    end;
end;


{ TP3DNodeSocket }

procedure TP3DNodeSocket.SetNode(AValue: TP3DNode);
begin
  if FNode=AValue then Exit;
  FNode:=AValue;
end;

function TP3DNodeSocket.GetSocketClass: TP3DNodeSocketCloneClass;
begin
  Result:= TP3DNodeSocketClone;
end;

constructor TP3DNodeSocket.Create(const AOwner: IP3DStreamableContainer);
var
  Cnt: TP3DPropertyAccess;
  Nd: TObject;
begin
  inherited Create( AOwner );

  Cnt:= AOwner.GetContainer;
  if ( Cnt.Name = 'Inputs' ) then
    FDirection:= nsdInput
  else if ( Cnt.Name = 'Outputs' ) then
    FDirection:= nsdOutput
  else
    raise Exception.Create( 'Cannot detect node direction!' );

  Nd:= Cnt.Context.GetInstance;
  if ( Nd is TP3DNode ) then
    FNode:= TP3DNode( Nd )
  else
    raise Exception.Create( 'The context of the owner has to be a descendant of TP3DNode!' );

  Properties.Add( TP3DPropAccessRTTI.Create( Self, 'Direction', smText ));
end;

function TP3DNodeSocket.Clone(AOwner: IP3DStreamableContainer): TP3DNodeSocketClone;
begin
  Result:= GetSocketClass.Create( AOwner );
  Result.FUpdating:= True;
  Result.FCloneOf.AsValue:= Self;
  Result.FUpdating:= False;
end;

{ TP3DNodeSocketClone }

function TP3DNodeSocketClone.GetConnected: TP3DNodeSocketClone;
begin
  Result:= TP3DNodeSocketClone( FConnected.AsValue );
end;

procedure TP3DNodeSocketClone.AcceptConnection(ATarget: TP3DNodeSocket; const Accept: Boolean);
begin

end;

procedure TP3DNodeSocketClone.ChangePropValue(Sender: TObject);
begin
  if ( Assigned( NodeMaster )) then
    NodeMaster.SetSocketValue( Self );
end;

function TP3DNodeSocketClone.GetName: String;
begin
  if ( Assigned( FCloneOf ) and  Assigned( CloneOf )) then
    Result:= CloneOf.Name
  else
    Result:= 'Untitled';
end;

procedure TP3DNodeSocketClone.SetName(AValue: String);
begin
  //
end;

constructor TP3DNodeSocketClone.Create(const AOwner: IP3DStreamableContainer);
begin
  FNode:= TP3DStreamablePointer.Create( 'Node', TP3DNodeClone );
  inherited Create( AOwner );
  Properties.Add( FNode );
  FCloneOf:= TP3DStreamablePointer.Create( 'CloneOf', TP3DNodeSocket );
  FCloneOf.AfterChange:= @ChangeCloneOf;
  Properties.Add( FCloneOf );
  FConnected:= TP3DStreamablePointer.Create( 'Connected', TP3DNodeSocketClone, smText );
  Properties.Add( FConnected );
end;

constructor TP3DNodeSocketClone.CreateFromJSON(AJSON: TJSONData; const AOwner: IP3DStreamableContainer);
var
  path: TJSONUnicodeStringType;
  prop, O: TP3DPropertyAccess;
  obj: TObject;
begin
  Create( AOwner );
  if ( AJSON is TJSONObject ) then begin
    FUpdating:= True;
    path:= TJSONObject( AJSON ).Get( 'CloneOf', '' );
    prop:= Root.GetJSONLoader().GetPropByPath( path );
    if ( not Assigned( prop )) then
      raise Exception.CreateFmt( 'The property "%s" was not found in "%s"!', [ path, Root.GetJSONLoader.FileName ]);
    FCloneOf.Assign( prop );
    AsJSON:= AJSON;
  end;
end;

destructor TP3DNodeSocketClone.Destroy;
begin
  if ( Assigned( FCloneOf )) then
    FCloneOf.AfterChange:= nil;
  if ( Assigned( FValueProp )) then
    ValueProp:= nil;
  inherited Destroy;
end;

procedure TP3DNodeSocketClone.SetConnected(AValue: TP3DNodeSocketClone);
begin
  if ( FConnected.AsValue = AValue ) then
    Exit;
  if ( Assigned( AValue ) and ( AValue.NodeMaster <> Self.NodeMaster )) then
    raise Exception.CreateFmt( 'Trying to connect two sockets (%s.%s to %s.%s) from different materials!',
          [ CloneOf.Node.Name, CloneOf.Name, AValue.CloneOf.Node.Name, AValue.CloneOf.Name ]);
  if ( Assigned( AValue ) and ( CloneOf.Direction = nsdOutput )) then
    AValue.Connected:= Self
  else
    begin
      FConnected.AsValue:= AValue;
      if ( Assigned( OnConnect )) then
        OnConnect( Self );
      if ( Assigned( NodeMaster )) then
        NodeMaster.SetSocketConnected( Self )
      else
        WriteLn( Self.GetFullPath );
    end;
end;

procedure TP3DNodeSocketClone.SetValueProp(AValue: TP3DPropertyAccess);
begin
  if FValueProp=AValue then Exit;
  if ( Assigned( FValueProp )) then
    FValueProp.OnChange:= nil;

  FValueProp:=AValue;

  if ( Assigned( FValueProp ) and ( CloneOf.Node.Name[1..8] <> 'uniform_' )) then
    FValueProp.OnChange:= @ChangePropValue;
  if ( Assigned( NodeMaster )) then
    NodeMaster.SetSocketValue( Self );
end;

function TP3DNodeSocketClone.GetIsDefault: Boolean;
begin
  Result:= ValueProp.IsDefault and ( not Assigned( Connected ));
end;

function TP3DNodeSocketClone.GetNode: TP3DNodeClone;
begin
  Result:= TP3DNodeClone( FNode.AsValue );
end;

procedure TP3DNodeSocketClone.ChangeCloneOf(ANewValue: TP3DStreamable);
begin
  if ( not FUpdating ) then
    exit;
  if ( Assigned( ValueProp )) then
    Properties.Remove( ValueProp );
  if ( Assigned( CloneOf )) then
    begin
      case CloneOf.SocketType of
        'vec4': ValueProp:= TP3DPropAccessVec4.Create( CloneOf.Name, smText );
        'vec3': ValueProp:= TP3DPropAccessVec3.Create( CloneOf.Name, smText );
        'vec2': ValueProp:= TP3DPropAccessVec2.Create( CloneOf.Name, smText );
        'color4': ValueProp:= TP3DPropAccessColorRGBA.Create( CloneOf.Name, smText );
        'text': ValueProp:= TP3DPropAccessString.Create( CloneOf.Name, smText );
        'int', 'sampler1D', 'sampler2D', 'sampler3D': ValueProp:= TP3DPropAccessInt.Create( CloneOf.Name, smText );
        'float': ValueProp:= TP3DPropAccessFloat.Create( CloneOf.Name, smText );
        else
          ValueProp:= TP3DPropAccessString.Create( CloneOf.Name, smText );
      end;
      Properties.Add( ValueProp );
      if ( CloneOf.Default > '' ) then
        try
          ValueProp.AsString:= CloneOf.Default;
        except
          On E: Exception do
          log_exception( message_header( Self ) + Format( 'Error setting default value of %s.%s (%s) <= %s: "%s"', [ Node.CloneOf.Name, CloneOf.Name, CloneOf.SocketType, CloneOf.Default, E.Message ]));
            //P3DLog.LogException( Self, Format( 'Error setting default value of %s.%s (%s) <= %s: "%s"', [ Node.CloneOf.Name, CloneOf.Name, CloneOf.SocketType, CloneOf.Default, E.Message ]));
        end;
    end;
  if ( Assigned( Node )) then
    Node.NeedsUpdate:= True;
end;

function TP3DNodeSocketClone.GetCloneOf: TP3DNodeSocket;
begin
  Result:= TP3DNodeSocket( FCloneOf.AsValue );
end;



{ TP3DNodeList }

function CompareNodes(Item1, Item2: Pointer): Integer;
var
  n1, n2: String;
begin
  n1:= TP3DNode( TP3DStreamableContainer( Item1 ).AsValue ).Name;
  n2:= TP3DNode( TP3DStreamableContainer( Item2 ).AsValue ).Name;
  Result:= CompareStr( n1, n2 );
end;

procedure TP3DNodeContainerList.SortByName;
  procedure swap( a, b: Integer );
  var
    Element: TP3DNode;
  begin
    Element:= Self[ a ];
    FItems[ a ]:= Self[ b ];
    FItems[ b ]:= Element;
  end;

  function partition( lo, hi: Integer ): Integer;
  var
    i, j: Integer;
  begin
    i:= lo;        // place for swapping
    for j := lo to hi - 1 do
      if ( Self[ j ].Name <= Self[ hi ].Name ) then
        begin
          swap( i, j );
          Inc( i );
        end;
    swap( i, hi );
    Result:= i;
  end;

  procedure quicksort( lo, hi: Integer );
  var
    p: Integer;
  begin
    if lo < hi then
      begin
        p:= partition( lo, hi );
        quicksort( lo, p - 1 );
        quicksort( p + 1, hi );
      end;
  end;

begin
  FItems.Sort( @CompareNodes );
  //quicksort( 0, Count - 1 );

end;

{ TP3DNodeSocketCloneContainerList }

function TP3DNodeSocketCloneContainerList.SaveToJSON: TJSONData;
var
  a: TJSONArray;
  i: Integer;
  json: TJSONData;
begin
  case StoreMode of
    smText:
      begin
        a:= TJSONArray.Create();
        for i:= 0 to Count - 1 do
          begin
            if ( not AsValues[ i ].IsDefault ) then begin
              json:= GetArrayItemAsProp( i ).SaveToJSON;
              if ( Assigned( json )) then
                a.Add( json );
            end;
          end;
        Result:= a;
      end
    else
      Result:= nil;
  end;
end;

procedure TP3DNodeSocketCloneContainerList.LoadJSON(AValue: TJSONData);
var
  i: Integer;
  Item: TJSONData;
begin
  if ( AValue.JSONType = jtArray ) then begin
    for i:= 0 to TJSONArray( AValue ).Count - 1 do
      LoadItemFromJSON( 0, AValue.Items[ i ]);
  end else
    raise Exception.CreateFmt( P3DERROR_JSON_WRONG_TYPE, [ 'jtArray', JSONTypeName( AValue.JSONType )]);
end;

procedure TP3DNodeSocketCloneContainerList.LoadItemFromJSON(Index: Integer; AData: TJSONData);
var
  PropName: TJSONStringType;
  i: Integer;
begin
  PropName:= AData.FindPath( 'Name' ).AsString;
  for i:= 0 to Count - 1 do
    if ( AsValues[ i ].Name = PropName ) then begin
      AsValues[ i ].AsJSON:= AData;
      break;
    end;
end;

procedure TP3DNodeSocketCloneContainerList.DisconnectAll;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    AsValues[ i ].Connected:= nil;
end;

{$ENDIF}

