unit p3dshadernodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, DOM, XMLRead, strutils, p3dfileutil;

type

  { TP3DShaderNode }
  TP3DShaderNode = class;
  TP3DShaderNodeVariableLink = class;

  { TP3DShaderNodeVariable }
  // Base Type for all Nodes
  TP3DShaderNodeVariable = class
    private
      FVarType: String;
      // Parse the node and return string output
      function GetStringOutput( const Link: TP3DShaderNodeVariableLink = nil ): String; virtual;

    published
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
      function FindVariableInput( Name: String ): Integer;
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

  { TP3DShaderNodeVariableInput }
  {
   input is just put as an empty element tag or with a child element which is
   only visible if it is linked to another node.
   ...
   gl_Position = <input type="vec4" name="gl_position" required="yes"/>;<br/>
   </node>
   ...
  }

  TP3DShaderNodeVariableInput = class( TP3DShaderNodeVariable )
    private
      FFragments: TP3DShaderNodeVariableList;
      FName: String;
      FRequired: Boolean;

    public
      constructor Create;
      destructor Destroy; override;

      function GetStringOutput( const Link: TP3DShaderNodeVariableLink = nil ): String; override;

      property Name: String read FName write FName;
      property Required: Boolean read FRequired write FRequired;

      property Fragments: TP3DShaderNodeVariableList read FFragments write FFragments;
  end;

  { TP3DShaderNodeVariableLink }

  TP3DShaderNodeVariableLink = class( TP3DShaderNodeVariable )
    private
      FFragments: TP3DShaderNodeVariableList;
      FTarget: String;

    public
      constructor Create;
      destructor Destroy; override;

      function GetStringOutput( const Link: TP3DShaderNodeVariableLink = nil ): String; override;

    published
      property Target: String read FTarget write FTarget;
      property Fragments: TP3DShaderNodeVariableList read FFragments write FFragments;
  end;

  { TP3DShaderNodeVariableExists }

  TP3DShaderNodeVariableExists = class( TP3DShaderNodeVariable )
    private
      FFragments: TP3DShaderNodeVariableList;
      FName: String;

    public
      constructor Create;
      destructor Destroy; override;

      function GetStringOutput( const Link: TP3DShaderNodeVariableLink = nil ): String; override;

    published
      property Name: String read FName write FName;
      property Fragments: TP3DShaderNodeVariableList read FFragments write FFragments;
  end;

  TP3DShaderNode = class
    private
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

  TP3DNodeShaderBase = class( TP3DShaderNodeLibrary )
    //function Connect( LinkName: String; Target: String ): Boolean;
  end;

var
  P3DShaderLib: TP3DShaderNodeLibrary;

  procedure ProcessFile( NodeList: TP3DShaderNodeList; FN: String );
  procedure LoadVariablesFromDOMNode( Module: String; SNV: TP3DShaderNodeVariableList; Node: TDOMNode );

implementation

{ TP3DShaderNodeVariableExists }

constructor TP3DShaderNodeVariableExists.Create;
begin
  inherited;
  Fragments:= TP3DShaderNodeVariableList.Create;
end;

destructor TP3DShaderNodeVariableExists.Destroy;
begin
  Fragments.Free;
  inherited Destroy;
end;

function TP3DShaderNodeVariableExists.GetStringOutput(
  const Link: TP3DShaderNodeVariableLink): String;
var
  Item: TP3DShaderNodeVariable;
  Exists: Boolean;
begin
  inherited;
  if ( Assigned( Link )) then
    begin
      Exists:= Link.Fragments.FindVariableInput( Name ) > -1;
      if ( Exists ) then
        Result:= Fragments.GetStringOutput();
    end
  else
    Result:= '';
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

function TP3DShaderNodeVariableList.FindVariableInput(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
     if ( Items[ i ] is TP3DShaderNodeVariableInput ) then
       if ( TP3DShaderNodeVariableInput( Items[ i ]).Name = Name ) then
         begin
           Result:= i;
           break;
         end;
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

constructor TP3DShaderNodeVariableLink.Create;
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
var
  Item: TP3DShaderNodeVariable;
  Ref: TP3DShaderNode;
begin
  inherited;
  Result:= '';

  Ref:= P3DShaderLib.FindReference( Target );
  if ( Assigned( Ref )) then
    Result:= Ref.GetCode( Self )
  else
    Result:= '- Warning: Reference not found "' + Target + '" -';
end;

{ TP3DShaderNodeVariableInput }

constructor TP3DShaderNodeVariableInput.Create;
begin
  inherited;
  FFragments:= TP3DShaderNodeVariableList.Create;
end;

destructor TP3DShaderNodeVariableInput.Destroy;
begin
  FFragments.Free;
  inherited Destroy;
end;

function TP3DShaderNodeVariableInput.GetStringOutput(
  const Link: TP3DShaderNodeVariableLink): String;
var
  n: Integer;
begin
  inherited;
  Result:= '';
  if ( not Assigned( Link )) then
    Result:= Fragments.GetStringOutput()
  else
    begin
      n:= Link.Fragments.FindVariableInput( Name );
      if ( n > -1 ) then
        Result:= Link.Fragments[ n ].GetStringOutput();
    end;
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
end;

destructor TP3DShaderNode.Destroy;
begin
  FFragments.Free;
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

{$INCLUDE p3dshadernodes_load.inc}

initialization
  P3DShaderLib:= TP3DShaderNodeLibrary.Create;

finalization
  P3DShaderLib.Free;

end.

