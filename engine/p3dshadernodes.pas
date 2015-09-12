unit p3dshadernodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Math, p3dMath, DOM, XMLRead, strutils, p3dfileutil, p3dNodes;

type
  { TP3DShaderNode }
  TP3DShaderNode = class;
  TP3DShaderNodeFragmentLink = class;


  TP3DShaderCompiled = class;
  TP3DInputCallback = function ( Sender: TP3DShaderNode; Ref: String; Context: TP3DShaderCompiled ): String of object;

  { TP3DShaderNodeFragment }
  // Base Type for all Nodes
  TP3DShaderNodeFragment = class
    private
      FVarType: String;
      FNode: TP3DShaderNode;

    public
      constructor Create( ANode: TP3DShaderNode );

      // Parse the node and return string output
      function GetStringOutput( InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled ): String; virtual;

    published
      property Node: TP3DShaderNode read FNode;
      property VarType: String read FVarType write FVarType;
  end;

  {$MACRO ON}
  {$DEFINE TCustomList:= TP3DCustomShaderNodeFragmentList}
//  {$DEFINE TCustomListEnumerator:= TP3DShaderNodeFragmentEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderNodeFragment}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DShaderNodeFragmentList }

  TP3DShaderNodeFragmentList = class( TP3DCustomShaderNodeFragmentList )
    public
      function GetStringOutput( InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled ): String;
      //function FindVariableLink( Name: String ): Integer;
  end;

  { TP3DShaderNodeFragmentInline }
  {
   inline text is just written as a child element of any other node
   ...
   <node name="fshader">
   #version 330
   </node>
   ...
  }

  TP3DShaderNodeFragmentInline = class( TP3DShaderNodeFragment )
    private
      FText: String;

    public
      function GetStringOutput( InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled ): String; override;

      property Text: String read FText write FText;
  end;

  { TP3DShaderNodeFragmentLink }

  TP3DShaderNodeFragmentLink = class( TP3DShaderNodeFragment )
    private
      FFragments: TP3DShaderNodeFragmentList;
      FList: Boolean;
      FTarget: String;

    public
      constructor Create( ANode: TP3DShaderNode );
      destructor Destroy; override;

      function GetStringOutput( InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled ): String; override;

    published
      property Target: String read FTarget write FTarget;
      property Fragments: TP3DShaderNodeFragmentList read FFragments write FFragments;
      property List: Boolean read FList write FList;
  end;

  TP3DShaderNodeOutlineFragmentList = class;
  { TP3DShaderNodeInput }

  TP3DShaderNodeInput = class  //NodeSocket - in Blender Input and Output at the same time
    private
      FFragments: TP3DShaderNodeOutlineFragmentList;
      FName: String;
      //FTarget: String;
      FVarType: String;

    public
      constructor Create;
      destructor Destroy; override;

    published
      property Name: String read FName;
      //property Target: String read FTarget write FTarget;
      property VarType: String read FVarType write FVarType;
      property Fragments: TP3DShaderNodeOutlineFragmentList read FFragments write FFragments;
  end;

  {$DEFINE TCustomList:= TP3DCustomShaderNodeInputList}
  {.$DEFINE TCustomListEnumerator:= TP3DShaderNodeInputEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderNodeInput}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DShaderNodeInputList }

  TP3DShaderNodeInputList = class ( TP3DCustomShaderNodeInputList )
    public
      function Find( Name: String ): Integer;
  end;

  TP3DShaderModule = class;
  TP3DShaderNode = class
    private
      //FInputs: TP3DShaderNodeInputList;
      FModule: TP3DShaderModule;
      FName: String;
      FFragments: TP3DShaderNodeFragmentList;
      FUnique: Boolean;

    public
      constructor Create;
      destructor Destroy; override;

      function GetStringOutput( InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled ): String; virtual;

    published
      property Name: String read FName write FName;
      property Fragments: TP3DShaderNodeFragmentList read FFragments write FFragments;
      property Module: TP3DShaderModule read FModule write FModule;
      property Unique: Boolean read FUnique write FUnique;
      //property Inputs: TP3DShaderNodeInputList read FInputs write FInputs;
  end;

  {$DEFINE TCustomList:= TP3DCustomShaderNodeList}
  {.$DEFINE TCustomListEnumerator:= TP3DShaderNodeListEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderNode}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DShaderNodeList }

  TP3DShaderNodeList = class ( TP3DCustomShaderNodeList )
    public
      function FindNode( Name: String ): TP3DShaderNode;
  end;

  { TP3DShaderModule }

  TP3DShaderModule = class
    private
      FInputs: TP3DShaderNodeInputList;
      FName: String;
      FNodes: TP3DShaderNodeList;

    public
      constructor Create;
      destructor Destroy; override;

    published
      property Nodes: TP3DShaderNodeList read FNodes write FNodes;
      property Name: String read FName write FName;
      property Inputs: TP3DShaderNodeInputList read FInputs write FInputs;
  end;

  {$DEFINE TCustomList:= TP3DShaderModuleList}
  {.$DEFINE TCustomListEnumerator:= TP3DShaderModuleListEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderModule}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DShaderNodeLibrary }

  TP3DShaderNodeLibrary = class( TP3DShaderModuleList )
    procedure LoadLibrary( FileName: String );
    procedure LoadLibraryPath( PathName: String; const Ext: String = '*.*' );
    function FindReference( Ref: String ): TP3DShaderNode;
    function FindModule( Name: String ): TP3DShaderModule;
  end;

  { TP3DShaderBuffer }

  TP3DShaderBuffer = class
    private
      FCode: String;
      FName: String;

    published
      property Code: String read FCode write FCode;
      property Name: String read FName write FName;
  end;

  {$DEFINE TCustomList:= TP3DShaderBufferList}
  {.$DEFINE TCustomListEnumerator:= TP3DShaderBufferListEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderBuffer}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DShaderCompiled }

  TP3DShaderCompiled = class
    private
      FBuffers: TP3DShaderBufferList;
      BlackList: TP3DShaderNodeList;

    public
      constructor Create;
      destructor Destroy; override;

      function FindBuffer( Name: String ): TP3DShaderBuffer;

    published
      property Buffers: TP3DShaderBufferList read FBuffers write FBuffers;
  end;

  { TP3DShaderNodeOutlineFragment }

  TP3DShaderNodeOutlineFragment = class
    private
      FInputs: TP3DShaderNodeInputList;
      FName: String;
      FOnChange: TNotifyEvent;
      FOnUpdateChild: TNotifyEvent;
      FX: Float;
      FY: Float;

      procedure LoadInputsFromDOMNode( Node: TDOMNode );
      procedure LoadAttributesFromDOMNode( Node: TDOMNode );
      procedure SetName(AValue: String);
      procedure SetX(AValue: Float);
      procedure SetY(AValue: Float);
      function InputCallback( Sender: TP3DShaderNode; Ref: String; Context: TP3DShaderCompiled ): String;

    public
      constructor CreateFromDOMNode( Node: TDOMNode );
      constructor Create;
      destructor Destroy; override;

      function Compile: TP3DShaderCompiled;

    published
      property Name: String read FName write SetName;
      property Inputs: TP3DShaderNodeInputList read FInputs write FInputs;
      property X: Float read FX write SetX;
      property Y: Float read FY write SetY;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
      property OnUpdateChild: TNotifyEvent read FOnUpdateChild write FOnUpdateChild;
  end;

  {$DEFINE TCustomList:= TP3DCustomShaderNodeOutlineFragmentList}
  {.$DEFINE TCustomListEnumerator:= TP3DShaderNodeOutlineFragmentListEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderNodeOutlineFragment}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DShaderNodeOutlineFragmentList }

  TP3DShaderNodeOutlineFragmentList = class ( TP3DCustomShaderNodeOutlineFragmentList )
    public
      procedure LoadFromDOMNode( Node: TDOMNode );
  end;

  TP3DShaderNodeOutline = class
    private
      FFragments: TP3DShaderNodeOutlineFragmentList;
      FName: String;

    public
      constructor Create;
      constructor CreateFromDOMElement( Element: TDOMElement );
      constructor CreateFromFile( FileName: String );

      destructor Destroy; override;

      function Compile: TP3DShaderCompiled;

    published
      property Name: String read FName write FName;
      property Fragments: TP3DShaderNodeOutlineFragmentList read FFragments write FFragments;
  end;

  {$DEFINE TCustomList:= TP3DShaderNodeOutlineList}
  {.$DEFINE TCustomListEnumerator:= TP3DShaderNodeOutlineListEnumerator}
  {$DEFINE TCustomItem:= TP3DShaderNodeOutline}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}
  {.$DEFINE INTERFACE}
  {.$INCLUDE p3dshader_core.inc}

  {$UNDEF INTERFACE}

var
  P3DShaderLib: TP3DShaderNodeLibrary;

  function ProcessFile( FN: String ): TP3DShaderModule;
  procedure LoadVariablesFromDOMNode( Module: TP3DShaderModule; SNV: TP3DShaderNodeFragmentList; Node: TDOMNode; SN: TP3DShaderNode );

implementation

{ TP3DShaderModule }

constructor TP3DShaderModule.Create;
begin
  inherited;
  FInputs:= TP3DShaderNodeInputList.Create;
  FNodes:= TP3DShaderNodeList.Create;
end;

destructor TP3DShaderModule.Destroy;
begin
  FInputs.Free;
  FNodes.Free;
  inherited Destroy;
end;

{ TP3DShaderCompiled }

constructor TP3DShaderCompiled.Create;
begin
  inherited;
  FBuffers:= TP3DShaderBufferList.Create;
end;

destructor TP3DShaderCompiled.Destroy;
begin
  FBuffers.Free;
  inherited Destroy;
end;

function TP3DShaderCompiled.FindBuffer(Name: String): TP3DShaderBuffer;
var
  buffer: TP3DShaderBuffer;
begin
  Result:= nil;
  for buffer in Buffers do
    if ( buffer.Name = Name ) then
      begin
        Result:= buffer;
        Break;
      end;
end;

{ TP3DShaderNodeInput }

constructor TP3DShaderNodeInput.Create;
begin
  inherited;
  FFragments:= TP3DShaderNodeOutlineFragmentList.Create;
end;

destructor TP3DShaderNodeInput.Destroy;
begin
  FFragments.Free;
  inherited Destroy;
end;

{ TP3DShaderNodeOutline }

constructor TP3DShaderNodeOutline.Create;
begin
  inherited;
  FFragments:= TP3DShaderNodeOutlineFragmentList.Create;
end;

constructor TP3DShaderNodeOutline.CreateFromDOMElement(Element: TDOMElement);
var
  sno: TP3DShaderNodeOutlineFragment;
  i: Integer;
begin
  Create;
  if ( Element.NodeName <> 'p3dshader' ) then
    raise Exception.Create( 'Cannot create shader node outline from element. Not a shader outline file!' );
  for i:= 0 to Element.ChildNodes.Count - 1 do
    begin
      if ( Element.ChildNodes[ i ].NodeName <> ':input' ) then
        begin
          sno:= TP3DShaderNodeOutlineFragment.CreateFromDOMNode( Element.ChildNodes[ i ]);
          Fragments.Add( sno );
        end
      else
        raise Exception.Create( 'Cannot create shader node outline from element. Input not allowed here!' );
    end;
end;

constructor TP3DShaderNodeOutline.CreateFromFile(FileName: String);
var
  F: TXMLDocument;
begin
  XMLRead.ReadXMLFile( F, FileName );

  CreateFromDOMElement( F.DocumentElement );
  Name:= ExtractFileNameOnly( FileName );
  F.Free;
end;

destructor TP3DShaderNodeOutline.Destroy;
begin
  FFragments.Free;
  inherited Destroy;
end;

function TP3DShaderNodeOutline.Compile: TP3DShaderCompiled;
var
  Node: TP3DShaderNode;
  Buffer: TP3DShaderBuffer;
  Module: TP3DShaderModule;
  frag: TP3DShaderNodeOutlineFragment;
begin
   Result:= TP3DShaderCompiled.Create;
   Result.BlackList:= TP3DShaderNodeList.Create;
   for frag in Fragments do
     begin
       Module:= P3DShaderLib.FindModule( frag.Name );
       if ( Assigned( Module )) then
         for Node in Module.Nodes do
           begin
             Buffer:= TP3DShaderBuffer.Create;
             Buffer.Name:= Node.Name;
             Buffer.Code:= Node.GetStringOutput( @frag.InputCallback, Result );
             Result.Buffers.Add( Buffer );
             Result.BlackList.Empty();
           end
       else
         raise Exception.Create( 'Error: Module "' + Name + '" was not found!' );
     end;
   Result.BlackList.Free;
end;

{ TP3DShaderNodeOutlineFragment }

procedure TP3DShaderNodeOutlineFragment.LoadInputsFromDOMNode(Node: TDOMNode);
  procedure LoadInputFromDOMNode( Node: TDOMNode );
  var
    itm: TP3DShaderNodeInput;
    InpName: TDOMNode;
  begin
    InpName:= Node.Attributes.GetNamedItem( 'name' );
    if ( not Assigned( InpName )) then
      raise Exception.Create( 'Error: inputs must have a name!' );
    itm:= TP3DShaderNodeInput.Create;
    itm.FName:= InpName.NodeValue;
    {$IFDEF DEBUG}
    WriteLn( 'input: ' + itm.Name );
    {$ENDIF}
    if ( Node.HasChildNodes ) then
      itm.Fragments.LoadFromDOMNode( Node );
    Inputs.Add( itm );
  end;

var
  Child: TDOMNode;
  i: Integer;
begin
  for i:= 0 to Node.ChildNodes.Count - 1 do
    begin
      Child:= Node.ChildNodes[ i ];
      if ( Child.NodeName = ':input' ) then
        LoadInputFromDOMNode( Child )
      else
        raise Exception.Create( 'Can only define inputs here!' );
    end;
end;

procedure TP3DShaderNodeOutlineFragment.LoadAttributesFromDOMNode(Node: TDOMNode
  );
var
  Child: TDOMNode;
  i: Integer;
begin
  for i:= 0 to Node.Attributes.Length - 1 do
    begin
      Child:= Node.Attributes[ i ];
      if ( Child.NodeName = 'X' ) then
        X:= StrToFloat( Child.NodeValue )
      else if ( Child.NodeName = 'Y' ) then
        Y:= StrToFloat( Child.NodeValue )
      else
        raise Exception.Create( 'Unknown attribute: ' + Child.NodeName );
    end;
end;

procedure TP3DShaderNodeOutlineFragment.SetName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

procedure TP3DShaderNodeOutlineFragment.SetX(AValue: Float);
begin
  if FX=AValue then Exit;
  FX:=AValue;
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

procedure TP3DShaderNodeOutlineFragment.SetY(AValue: Float);
begin
  if FY=AValue then Exit;
  FY:=AValue;
  if ( Assigned( FOnChange )) then
    OnChange( Self );
end;

function TP3DShaderNodeOutlineFragment.InputCallback(Sender: TP3DShaderNode;
  Ref: String; Context: TP3DShaderCompiled): String;
  function CheckInput( Name: String ): String;
  var
    i: Integer;
    S: String;
    Ref: TP3DShaderNode;
    Inp: TP3DShaderNodeInput;
    frag: TP3DShaderNodeOutlineFragment;
  begin
    Result:= '';
    S:= Copy( Name, 2, Min( Length( Name ) - 1, Pos( '.', Name ) - 2 ));
    if (( Pos( '*', S ) > 0 ) or ( Pos( '?', S ) > 0 )) then
      begin
        for i:= 0 to Inputs.Count - 1 do
          if ( IsWild( Inputs[ i ].Name, S, False )) then
            for frag in Inputs[ i ].Fragments do
              begin
                Ref:= P3DShaderLib.FindReference( ReplaceStr( Name, ':' + S, frag.Name ));
                if ( Assigned( Ref )) then
                  Result+= Ref.GetStringOutput( @frag.InputCallback, Context );
              end;
      end
    else
      begin
        i:= Inputs.Find( S );
        if ( i < 0 ) then
          begin
            Result:= '- Warning: Input "' + S + '" was not found -';
            exit;
          end;
        Inp:= Inputs[ i ];

        if ( Inp.Fragments.Count = 0 ) then
          Result:= '- Warning: Input "' + Inp.Name + '" was not connected -'
        else
          for frag in Inputs[ i ].Fragments do
            begin
              Ref:= P3DShaderLib.FindReference( ReplaceStr( Name, ':' + S, frag.Name ));
              if ( Assigned( Ref )) then
                Result+= Ref.GetStringOutput( @frag.InputCallback, Context )
              else
                Result:= '- Warning: Node "' + Sender.Name + '" was not found in module "' + frag.Name + '" -';
            end;
    end;
  end;

begin
  Result:= CheckInput( Ref );
end;

constructor TP3DShaderNodeOutlineFragment.CreateFromDOMNode( Node: TDOMNode );
begin
  Create;

  if ( Node.NodeName <> ':input' ) then
    begin
      if ( Node.HasAttributes ) then
        LoadAttributesFromDOMNode( Node );

      Name:= Node.NodeName;
      {$IFDEF DEBUG}
      WriteLn( 'outline: ' + Name );
      {$ENDIF}
      if ( Node.HasChildNodes ) then
        LoadInputsFromDOMNode( Node );
    end
  else
    raise Exception.Create( 'Error: Cannot create outline fragment from input node!' );
end;

constructor TP3DShaderNodeOutlineFragment.Create;
begin
  inherited;
  FInputs:= TP3DShaderNodeInputList.Create;
end;

destructor TP3DShaderNodeOutlineFragment.Destroy;
begin
  FInputs.Free;
  inherited Destroy;
end;

function TP3DShaderNodeOutlineFragment.Compile: TP3DShaderCompiled;
var
  Node: TP3DShaderNode;
  Buffer: TP3DShaderBuffer;
  Module: TP3DShaderModule;
begin
   Result:= TP3DShaderCompiled.Create;
   Module:= P3DShaderLib.FindModule( Name );
   Result.BlackList:= TP3DShaderNodeList.Create;
   if ( Assigned( Module )) then
     for Node in Module.Nodes do
       begin
         Buffer:= TP3DShaderBuffer.Create;
         Buffer.Name:= Node.Name;
         Buffer.Code:= Node.GetStringOutput( @InputCallback, Result );
         Result.Buffers.Add( Buffer );
         Result.BlackList.Empty();
       end
   else
     raise Exception.Create( 'Error: Module "' + Name + '" was not found!' );
   Result.BlackList.Free;
end;

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

{ TP3DShaderNodeFragmentList }
function TP3DShaderNodeFragmentList.GetStringOutput(
  InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled ): String;
var
  Item: TP3DShaderNodeFragment;
begin
  Result:= '';
  for Item in Self do
    Result:= Result + Item.GetStringOutput( InputCallback, Context );
end;

function TP3DShaderNodeList.FindNode(Name: String): TP3DShaderNode;
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

{ TP3DShaderNodeLibrary }

procedure TP3DShaderNodeLibrary.LoadLibrary(FileName: String);
var
  Module: TP3DShaderModule;
begin
  Module:= ProcessFile( FileName );
  Add( Module );
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
  Module: TP3DShaderModule;
begin
  Result:= nil;
  if ( WordCount( Ref, [ '.' ]) <> 2 ) then
    exit;
  namemodule:= ExtractWord( 1, Ref, [ '.' ]);
  namenode:= ExtractWord( 2, Ref, [ '.' ]);
  Module:= FindModule( namemodule );
  if ( Assigned( Module )) then
    begin
      Node:= Module.Nodes.FindNode( namenode );
      if ( Assigned( Node )) then
        Result:= Node;
    end;
end;

function TP3DShaderNodeLibrary.FindModule(Name: String): TP3DShaderModule;
var
  Item: TP3DShaderModule;
begin
  Result:= nil;
  for Item in Self do
    if ( Item.Name = Name ) then
      begin
        Result:= Item;
        Break;
      end;
end;

{ TP3DShaderNodeFragmentLink }

constructor TP3DShaderNodeFragmentLink.Create(ANode: TP3DShaderNode);
begin
  inherited;
  Fragments:= TP3DShaderNodeFragmentList.Create;
end;

destructor TP3DShaderNodeFragmentLink.Destroy;
begin
  Fragments.Free;

  inherited Destroy;
end;

function TP3DShaderNodeFragmentLink.GetStringOutput(
  InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled): String;
//TODO: Cleanup Check Reference
  function CheckReference( N: String ): String;
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
      Result:= InputCallback( Node, N, Context )
    else
      begin
        Ref:= P3DShaderLib.FindReference( N );
        if ( Assigned( Ref )) then
          Result:= Ref.GetStringOutput( InputCallback, Context )
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

{ TP3DShaderNodeFragmentInline }

function TP3DShaderNodeFragmentInline.GetStringOutput(
  InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled ): String;
begin
  inherited;
  Result:= Text;
end;

{ TP3DShaderNodeFragment }

constructor TP3DShaderNodeFragment.Create(ANode: TP3DShaderNode);
begin
  inherited Create;
  FNode:= ANode;
end;

function TP3DShaderNodeFragment.GetStringOutput(
  InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled): String;
begin
  Result:= '';
end;

{ TP3DShaderNode }

constructor TP3DShaderNode.Create;
begin
  inherited;
  FFragments:= TP3DShaderNodeFragmentList.Create;
end;

destructor TP3DShaderNode.Destroy;
begin
  FFragments.Free;
  inherited Destroy;
end;

function TP3DShaderNode.GetStringOutput( InputCallback: TP3DInputCallback; Context: TP3DShaderCompiled ): String;
begin
  if ( Context.BlackList.FItems.IndexOf( Self ) > -1 ) then
    Result:= ''
  else
    begin
      Result:= Fragments.GetStringOutput( InputCallback, Context );
      if ( Unique ) then
        Context.BlackList.Add( Self );
    end;
end;

procedure TP3DShaderNodeOutlineFragmentList.LoadFromDOMNode(Node: TDOMNode);
var
  i: Integer;
begin
  for i:= 0 to Node.ChildNodes.Count - 1 do
    Add( TP3DShaderNodeOutlineFragment.CreateFromDOMNode( Node.ChildNodes[ i ]));
end;


{$DEFINE OBJECTLIST}
{$DEFINE TCustomList:= TP3DCustomShaderNodeList}
{.$DEFINE TCustomListEnumerator:= TP3DShaderNodeListEnumerator}
{$DEFINE TCustomItem:= TP3DShaderNode}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TP3DCustomShaderNodeFragmentList}
{.$DEFINE TCustomListEnumerator:= TP3DShaderNodeFragmentEnumerator}
{$DEFINE TCustomItem:= TP3DShaderNodeFragment}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TP3DCustomShaderNodeInputList}
{.$DEFINE TCustomListEnumerator:= TP3DShaderNodeInputEnumerator}
{$DEFINE TCustomItem:= TP3DShaderNodeInput}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TP3DCustomShaderNodeOutlineFragmentList}
{.$DEFINE TCustomListEnumerator:= TP3DShaderNodeOutlineFragmentListEnumerator}
{$DEFINE TCustomItem:= TP3DShaderNodeOutlineFragment}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TP3DShaderNodeOutlineList}
{.$DEFINE TCustomListEnumerator:= TP3DShaderNodeOutlineListEnumerator}
{$DEFINE TCustomItem:= TP3DShaderNodeOutline}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TP3DShaderBufferList}
{.$DEFINE TCustomListEnumerator:= TP3DShaderBufferListEnumerator}
{$DEFINE TCustomItem:= TP3DShaderBuffer}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TP3DShaderModuleList}
{.$DEFINE TCustomListEnumerator:= TP3DShaderModuleListEnumerator}
{$DEFINE TCustomItem:= TP3DShaderModule}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$INCLUDE p3dshadernodes_load.inc}

{.$DEFINE IMPLEMENTATION}
{.$INCLUDE p3dshader_core.inc}

initialization
  P3DShaderLib:= TP3DShaderNodeLibrary.Create;

finalization
  P3DShaderLib.Free;

end.

