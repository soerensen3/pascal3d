unit p3dNodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Math, p3dMath, DOM, XMLRead, strutils, p3dfileutil, p3dgenerics;

type

  {$MACRO ON}

  { TP3DNodeSocket }

  TP3DNode = class;
  TP3DNodeSocket = class
    private
      FConnected: TP3DNodeSocket;
      FName: String;
      FNode: TP3DNode;
      FOnConnect: TNotifyEvent;
      FUserData: Pointer;
      class function GetSocketType: String; static;
      procedure SetConnected(AValue: TP3DNodeSocket);
      procedure SetNode(AValue: TP3DNode);

    public
      constructor Create( ANode: TP3DNode );
      procedure AcceptConnection( ATarget: TP3DNodeSocket; const Accept: Boolean = False ); virtual;
  { - Procedure to check if socket connection is accepted  }
      class property SocketType: String read GetSocketType;

  { - Can be of various types (Int, Float, Vector, Color)
      The types must be registered the NodeTree
    - Same types can be connected but also some different types
      can be connected like vector and color.
  }
      property Name: String read FName write FName;
      property Connected: TP3DNodeSocket read FConnected write SetConnected;
      property Node: TP3DNode read FNode write SetNode;
      property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
      property UserData: Pointer read FUserData write FUserData;
  end;

  TP3DNodeSocketList = specialize TP3DCustomObjectList < TP3DNodeSocket >;

  TP3DNode = class
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

    public
      constructor Create;
      destructor Destroy; override;

      property Inputs: TP3DNodeSocketList read FInputs;
      property Outputs: TP3DNodeSocketList read FOutputs;
  //- Sockets for Inputs and Outputs
      property Name: String read FName write SetName;
      property X: Single read FX write SetX;
      property Y: Single read FY write SetY;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property UserData: Pointer read FUserData write FUserData;
  end;

  TP3DNodeList = specialize TP3DCustomObjectList < TP3DNode >;

  { TP3DNodeTree }

  TP3DNodeTree = class( TP3DNode ) //NodeTree
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

implementation

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

constructor TP3DNode.Create;
begin
  inherited;
  FInputs:= TP3DNodeSocketList.Create;
  FOutputs:= TP3DNodeSocketList.Create;
end;

destructor TP3DNode.Destroy;
begin
  Inputs.Free;
  Outputs.Free;
  inherited Destroy;
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

constructor TP3DNodeSocket.Create(ANode: TP3DNode);
begin
  inherited Create;
  FNode:= ANode;
end;

procedure TP3DNodeSocket.AcceptConnection(ATarget: TP3DNodeSocket;
  const Accept: Boolean);
begin

end;

end.

