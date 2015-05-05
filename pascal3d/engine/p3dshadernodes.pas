unit p3dshadernodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Math, DOM, XMLRead, strutils, p3dfileutil;

type

  { TP3DShaderNode }
  TP3DShaderNode = class;
  TP3DShaderNodeVariableLink = class;

  { TP3DShaderNodeVariable }
  // Base Type for all Nodes
  TP3DShaderNodeVariable = class
    private
      FVarType: String;
      FNode: TP3DShaderNode;

      constructor Create( ANode: TP3DShaderNode );

      // Parse the node and return string output
      function GetStringOutput( const Link: TP3DShaderNodeVariableLink = nil ): String; virtual;

    published
      property Node: TP3DShaderNode read FNode;
      property VarType: String read FVarType write FVarType;
  end;

  {$MACRO ON}
  {$DEFINE TCustomList:= TP3DCustomShaderNodeVariableList}
  {$DEFINE TCustomListEnumerator:= TP3DShaderNodeVariableEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderNodeVariable}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DShaderNodeVariableList }

  TP3DShaderNodeVariableList = class( TP3DCustomShaderNodeVariableList )
    public
      function GetStringOutput( const Link: TP3DShaderNodeVariableLink = nil ): String;
      //function FindVariableLink( Name: String ): Integer;
  end;

  { TP3DShaderNodeVariableInline }
  {
   inline text is just written as a child element of any other node
   ...
   <node name="fshader">
   #version 330
   </node>
   ...
  }

  TP3DShaderNodeVariableInline = class( TP3DShaderNodeVariable )
    private
      FText: String;

    public
      function GetStringOutput( const Link: TP3DShaderNodeVariableLink = nil ): String; override;

      property Text: String read FText write FText;
  end;

  { TP3DShaderNodeVariableLink }

  TP3DShaderNodeVariableLink = class( TP3DShaderNodeVariable )
    private
      FFragments: TP3DShaderNodeVariableList;
      FList: Boolean;
      FTarget: String;

    public
      constructor Create( ANode: TP3DShaderNode );
      destructor Destroy; override;

      function GetStringOutput( const Link: TP3DShaderNodeVariableLink = nil ): String; override;

    published
      property Target: String read FTarget write FTarget;
      property Fragments: TP3DShaderNodeVariableList read FFragments write FFragments;
      property List: Boolean read FList write FList;
  end;

  { TP3DShaderNodeInput }

  TP3DShaderNodeInput = class
    private
      FName: String;
      FTarget: String;
      FVarType: String;

    published
      property Name: String read FName;
      property Target: String read FTarget write FTarget;
      property VarType: String read FVarType write FVarType;
  end;

  {$DEFINE TCustomList:= TP3DCustomShaderNodeInputList}
  {$DEFINE TCustomListEnumerator:= TP3DShaderNodeInputEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderNodeInput}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DShaderNodeInputList }

  TP3DShaderNodeInputList = class ( TP3DCustomShaderNodeInputList )
    public
      function Find( Name: String ): Integer;
  end;

  TP3DShaderNode = class
    private
      FInputs: TP3DShaderNodeInputList;
      FModule: String;
      FName: String;
      FFragments: TP3DShaderNodeVariableList;

    public
      constructor Create;
      destructor Destroy; override;

      function GetCode( const Link: TP3DShaderNodeVariableLink = nil ): String;

    published
      property Name: String read FName write FName;
      property Fragments: TP3DShaderNodeVariableList read FFragments write FFragments;
      property Module: String read FModule write FModule;
      property Inputs: TP3DShaderNodeInputList read FInputs write FInputs;
  end;

  {$DEFINE TCustomList:= TP3DShaderNodeList}
  {$DEFINE TCustomListEnumerator:= TP3DShaderNodeListEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderNode}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DShaderNodeLibrary }

  TP3DShaderNodeLibrary = class( TP3DShaderNodeList )
    procedure LoadLibrary( FileName: String );
    procedure LoadLibraryPath( PathName: String; const Ext: String = '*.*' );
    function FindReference( Ref: String ): TP3DShaderNode;
    function FindNode( Name: String ): TP3DShaderNode;
  end;

var
  P3DShaderLib: TP3DShaderNodeLibrary;

  procedure ProcessFile( NodeList: TP3DShaderNodeList; FN: String );
  procedure LoadVariablesFromDOMNode( Module: String; SNV: TP3DShaderNodeVariableList; Node: TDOMNode; SN: TP3DShaderNode );

implementation

{ TP3DShaderNodeInputList }

function TP3DShaderNodeInputList.Find(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if ( Name = Items[ i ].Name ) then
      Result:= i;
end;

{ TP3DShaderNodeVariableList }
function TP3DShaderNodeVariableList.GetStringOutput(
  const Link: TP3DShaderNodeVariableLink): String;
var
  Item: TP3DShaderNodeVariable;
begin
  Result:= '';
  for Item in Self do
    Result:= Result + Item.GetStringOutput( Link );
end;

{ TP3DShaderNodeLibrary }

procedure TP3DShaderNodeLibrary.LoadLibrary(FileName: String);
begin
  ProcessFile( Self, FileName );
end;

procedure TP3DShaderNodeLibrary.LoadLibraryPath(PathName: String; const Ext: String
  );
var
  Files: TStringList;
  line: String;
begin
  PathName:= IncludeTrailingPathDelimiter( PathName );
  Files:= P3DListFolderFiles( PathName + Ext, True, True );
  for line in Files do
    LoadLibrary( line );
end;

function TP3DShaderNodeLibrary.FindReference( Ref: String ): TP3DShaderNode;
var
  namemodule: String;
  namenode: String;
  Node: TP3DShaderNode;
begin
  Result:= nil;
  if ( WordCount( Ref, [ '.' ]) <> 2 ) then
    exit;
  namemodule:= ExtractWord( 1, Ref, [ '.' ]);
  namenode:= ExtractWord( 2, Ref, [ '.' ]);
  for Node in Self do
    if (( Node.Module = namemodule ) AND ( Node.Name = namenode )) then
      Result:= Node;
end;

function TP3DShaderNodeLibrary.FindNode(Name: String): TP3DShaderNode;
var
  Item: TP3DShaderNode;
begin
  Result:= nil;
  for Item in Self do
    if ( Item.Name = Name ) then
      begin
        Result:= Item;
        Break;
      end;
end;

{ TP3DShaderNodeVariableLink }

constructor TP3DShaderNodeVariableLink.Create(ANode: TP3DShaderNode);
begin
  inherited;
  Fragments:= TP3DShaderNodeVariableList.Create;
end;

destructor TP3DShaderNodeVariableLink.Destroy;
begin
  Fragments.Free;

  inherited Destroy;
end;

function TP3DShaderNodeVariableLink.GetStringOutput(
  const Link: TP3DShaderNodeVariableLink): String;
//TODO: Cleanup Check Reference
  function CheckReference( N: String ): String;

    function CheckInput( Name: String ): String;
    var
      i: Integer;
      S: String;
      Ref: TP3DShaderNode;
      Inp: TP3DShaderNodeInput;
    begin
      Result:= '';
      S:= Copy( N, 2, Min( Length( N ) - 1, Pos( '.', N ) - 2 ));
      if (( Pos( '*', S ) > 0 ) or ( Pos( '?', S ) > 0 )) then
        begin
          for i:= 0 to Node.Inputs.Count - 1 do
            if ( IsWild( Node.Inputs[ i ].Name, S, False )) then
              begin
                Ref:= P3DShaderLib.FindReference( ReplaceStr( N, ':' + S, Node.Inputs[ i ].Target ));
                if ( Assigned( Ref )) then
                  Result+= Ref.GetCode( Self );
              end;
        end
      else
        begin
          i:= Node.Inputs.Find( S );
          if ( i < 0 ) then
            begin
              Result:= '- Warning: Input "' + S + '" was not found -';
              exit;
            end;
          Ref:= P3DShaderLib.FindReference( ReplaceStr( N, ':' + S, Node.Inputs[ i ].Target ));
          Result:= Ref.GetCode( Self );
          Inp:= Node.Inputs[ i ];
          if ( Inp.Target = '' ) then
            Result:= '- Warning: Input "' + Inp.Name + '" was not connected -'
          else
            Result:= Ref.GetCode( Self );
      end;
    end;

  var
    Ref: TP3DShaderNode;
    S: String;
    Inp: TP3DShaderNodeInput;
    i: Integer;
    R: String;

  begin
    Ref:= nil;
    if ( IsEmptyStr( N, [ $8, $10, $13, ' ', ';' ])) then
      Result:= '- Warning: Reference was empty -'
    else if ( N[ 1 ] = ':' ) then
      Result:= CheckInput( N )
    else
      begin
        Ref:= P3DShaderLib.FindReference( N );
        if ( Assigned( Ref )) then
          Result:= Ref.GetCode( Self )
        else
          Result:= '- Warning: Reference was not found "' + N + '" -';
      end;
  end;

  function Explode( Input: String; Delim: Char ): TStringList;
  begin
    Result:= TStringList.Create;
    Result.Delimiter:= Delim;
    Result.DelimitedText:= Input;
  end;

var
  Targets: TStringList;
  ThisTarget: String;
begin
  inherited;
  Result:= '';

  Targets:= Explode( Target, ';' );
  for ThisTarget in Targets do
    Result+= CheckReference( ThisTarget );
end;

{ TP3DShaderNodeVariableInline }

function TP3DShaderNodeVariableInline.GetStringOutput(
  const Link: TP3DShaderNodeVariableLink): String;
begin
  inherited;
  Result:= Text;
  WriteLn( Text );
end;

{ TP3DShaderNodeVariable }

constructor TP3DShaderNodeVariable.Create(ANode: TP3DShaderNode);
begin
  inherited Create;
  FNode:= ANode;
end;

function TP3DShaderNodeVariable.GetStringOutput(const Link: TP3DShaderNodeVariableLink
  ): String;
begin
  Write( 'Class: ', ClassName );
  if ( Assigned( Self )) then
    Write( ' Self: ', Integer( @Self ));
  if ( Assigned( Link )) then
    Write( ' Link: ', Link.Target );
  WriteLn();
end;

{ TP3DShaderNode }

constructor TP3DShaderNode.Create;
begin
  inherited;
  FFragments:= TP3DShaderNodeVariableList.Create;
  FInputs:= TP3DShaderNodeInputList.Create;
end;

destructor TP3DShaderNode.Destroy;
begin
  FFragments.Free;
  FInputs.Free;
  inherited Destroy;
end;

function TP3DShaderNode.GetCode(const Link: TP3DShaderNodeVariableLink): String;
var
  Item: TP3DShaderNodeVariable;
begin
  Result:= Fragments.GetStringOutput( Link );
end;

{$DEFINE OBJECTLIST}
{$DEFINE TCustomList:= TP3DShaderNodeList}
{$DEFINE TCustomListEnumerator:= TP3DShaderNodeListEnumerator}
{$DEFINE TCustomItem:= TP3DShaderNode}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TP3DCustomShaderNodeVariableList}
{$DEFINE TCustomListEnumerator:= TP3DShaderNodeVariableEnumerator}
{$DEFINE TCustomItem:= TP3DShaderNodeVariable}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TP3DCustomShaderNodeInputList}
{$DEFINE TCustomListEnumerator:= TP3DShaderNodeInputEnumerator}
{$DEFINE TCustomItem:= TP3DShaderNodeInput}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$INCLUDE p3dshadernodes_load.inc}

initialization
  P3DShaderLib:= TP3DShaderNodeLibrary.Create;

finalization
  P3DShaderLib.Free;

end.

