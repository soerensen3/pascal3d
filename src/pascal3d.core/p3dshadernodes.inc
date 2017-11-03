//p3dshadernodes.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

type
  { TP3DShaderNode }
  TP3DShaderNode = class;
  TP3DShaderNodeTree = class;


  TP3DShaderCompiled = class;
  TP3DShaderBuffer = class;
  TP3DShaderNodeSocket = class;
  TP3DInputCallback = function ( Sender: TP3DShaderNode; Ref: String; Context: TP3DShaderCompiled ): String of object;

  TP3DCustomShaderNodeSocketList = specialize gP3DCustomObjectList < TP3DShaderNodeSocket >;

  { TP3DShaderNodeSocketList }

  TP3DShaderNodeSocketList = class ( TP3DCustomShaderNodeSocketList )
    function GetStringOutput(Buffer: TP3DShaderBuffer): String;
    function FindSocketByName( Name: String ): Integer;
  end;

  { TP3DShaderNodeFragment }
  // Base Type for all Fragments
  TP3DShaderNodeFragment = class
    private
      FVarType: String;

    public
      constructor Create;

      // Parse the node and return string output
      function GetStringOutput( ANode: TP3DShaderNode; Buffer: TP3DShaderBuffer ): String; virtual;
      function MakeCopy( ANode: TP3DShaderNode ): TP3DShaderNodeFragment; virtual;

    published
      property VarType: String read FVarType write FVarType;
  end;

  TP3DShaderNodeFragmentList = specialize gP3DCustomObjectList < TP3DShaderNodeFragment >;

  { TP3DShaderNodeFragmentInline }
  {
   inline text is just written as a child element of any other node
   ...
   <node name="fshader">
   #version 330
   </node>
   ...
  }

  { TP3DShaderNodeFragmentInput }

  TP3DShaderNodeFragmentInput = class( TP3DShaderNodeFragment )
    private
      FInputName: String;

    public
      function GetStringOutput( ANode: TP3DShaderNode; Buffer: TP3DShaderBuffer ): String; override;

      function MakeCopy(ANode: TP3DShaderNode): TP3DShaderNodeFragment; override;

      property InputName: String read FInputName write FInputName;
  end;

  TP3DShaderNodeFragmentInline = class( TP3DShaderNodeFragment )
    private
      FText: String;

    public
      function GetStringOutput( ANode: TP3DShaderNode; Buffer: TP3DShaderBuffer ): String; override;

      function MakeCopy(ANode: TP3DShaderNode): TP3DShaderNodeFragment; override;

      property Text: String read FText write FText;
  end;

  { TP3DShaderNodeFragmentIfDef }

  TP3DShaderNodeFragmentIfDef = class( TP3DShaderNodeFragment )
    private
      FCaption: String;
      FFragments: TP3DShaderNodeFragmentList;

    public
      constructor Create;
      destructor Destroy; override;

      function GetStringOutput( ANode: TP3DShaderNode; Buffer: TP3DShaderBuffer ): String; override;

      property Caption: String read FCaption write FCaption;
      property Fragments: TP3DShaderNodeFragmentList read FFragments write FFragments;
  end;

  TP3DShaderNode = class ( TP3DNode )
    public
      constructor CreateFromDOM(DomNode: TDOMElement);

      function MakeCopy: TP3DShaderNode;
  end;

  { TP3DShaderNodeSocket }

  TP3DShaderNodeSocket = class ( TP3DNodeSocket )
    private
      FFragments: TP3DShaderNodeFragmentList;
      FIsCopy: Boolean;
      procedure SetConnected( AValue: TP3DNodeSocket); override;

    public
      constructor Create( ANode: TP3DNode; ADirection: TP3DNodeSocketDirection ); override;
      destructor Destroy; override;

      //function GetMainOutput( Buffer: TP3DShaderBuffer ): String; virtual;
      //function GetDeclarationOutput( Buffer: TP3DShaderBuffer ): String; virtual;
      function GetOutput( Buffer: TP3DShaderBuffer ): String; virtual;

      function MakeCopy( ANode: TP3DNode ): TP3DShaderNodeSocket;

    published
      property Fragments: TP3DShaderNodeFragmentList read FFragments write FFragments;
      property IsCopy: Boolean read FIsCopy;
  end;

  { TP3DShaderNodeLibrary }

  TP3DShaderNodeLibrary = class( TP3DNodeTree )
    private
      FPaths: TP3DFilePointerList;

      procedure OnChange( Sender: TPersistent; Item: TP3DFilePointer; Action: TP3DCustomListOnChangeAction );

    public
      constructor Create;
      destructor Destroy; override;

      procedure LoadLibrary( FileName: String );
      procedure LoadLibraryPath( PathName: String; const Ext: String = '*.pmd' );
      function FindNode( ANode: String ): TP3DShaderNode;

      class function DOMNodeName: String; override;
      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      procedure UpdatePaths( const Index: Integer = -1 );

      property Paths: TP3DFilePointerList read FPaths write FPaths;
  end;

  { TP3DShaderBuffer }

  TP3DShaderBuffer = class
    private
      FCode: String;
      FDeclarations: TP3DShaderNodeSocketList;
      FDefines: TStringList;
      FName: String;

    public
      constructor Create;
      destructor Destroy; override;

      procedure RegisterSocket( Socket: TP3DShaderNodeSocket );

    published
      property Code: String read FCode write FCode;
      property Name: String read FName write FName;
      property Defines: TStringList read FDefines write FDefines;
      property Declarations: TP3DShaderNodeSocketList read FDeclarations write FDeclarations;
  end;

  TP3DShaderBufferList = specialize gP3DCustomObjectList < TP3DShaderBuffer >;

  { TP3DShaderCompiled }

  TP3DShaderCompiled = class
    private
      FBuffers: TP3DShaderBufferList;

    public
      constructor Create;
      destructor Destroy; override;

      function FindBuffer( Name: String ): TP3DShaderBuffer;

    published
      property Buffers: TP3DShaderBufferList read FBuffers write FBuffers;
  end;

  { TP3DShaderNodeTree }

  TP3DShaderNodeTree = class ( TP3DNodeTree )
    private
      function GetShaderOutputFromNodes( Buffer: TP3DShaderBuffer ): String;

    public
      function AddCopy( Item: TP3DShaderNode ): Integer; virtual;
      function AddCopy( Name: String ): Integer; virtual;
      function AddCopyEx( Name: String ): TP3DShaderNode; inline;

      function Compile: TP3DShaderCompiled;
      //function CompileToBuffer( Definitions: TStringList ): TP3DShaderBuffer;
  end;
  {$INCLUDE p3dshadernodes_markdown.inc}
  {$INCLUDE p3dshadernodes_core.inc}

  const P3DGLSLVersion: String = '120';

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DShaderNodeTree }

function TP3DShaderNodeTree.GetShaderOutputFromNodes(Buffer: TP3DShaderBuffer ): String;
var
  Node: TP3DNode;
  Socket: TP3DNodeSocket;
  Code: String;
begin
  Result:= '';
  for Node in Nodes do
    if ( Node is TP3DShaderNode ) then
      for Socket in Node.Outputs do
        if ( Socket.SocketType = 'shader' ) then
          begin
            //WriteLn( 'Processing ', Node.Name, '.', Socket.Name, ' defines=', Buffer.Defines.CommaText );
            Code:= TP3DShaderNodeSocket( Socket ).GetOutput( Buffer );
            if ( Code > '' ) then
              Result+= '/*' + Node.Name + '-->*/' +
                       Code
                     + '/*<--' + Node.Name + '*/';

            Buffer.RegisterSocket( TP3DShaderNodeSocket( Socket ));
          end;
end;

function TP3DShaderNodeTree.Compile: TP3DShaderCompiled;
var
  Defines: TStringList;
  Buffer: TP3DShaderBuffer;
  Node: TP3DNode;
  Socket: TP3DNodeSocket;
begin
  Result:= TP3DShaderCompiled.Create;
  for Node in Nodes do
    if ( Node is TP3DShaderNode ) then
      for Socket in Node.Outputs do
        if ( Socket.SocketType = 'shader' ) then
          begin
            Buffer:= TP3DShaderBuffer.Create;
            Buffer.Name:= Socket.Name;
            Buffer.Code:= TP3DShaderNodeSocket( Socket ).GetOutput( Buffer );
            Buffer.RegisterSocket( TP3DShaderNodeSocket( Socket ));
            Result.Buffers.Add( Buffer );
            P3DLog.LogInfoXML( Self, '<details><summary>Built ' + Buffer.Name + ' from chunks <br/></summary><pre><code class="glsl">' + P3DTextToHTML( Buffer.Code ) + '</code></pre></details>' );
          end;
  {Defines:= TStringList.Create;
  Defines.Text:= 'vshader';
  Result.Buffers[ Result.Buffers.Add( CompileToBuffer( Defines ))].Name:= 'vshader';
  Defines.Text:= 'fshader';
  Result.Buffers[ Result.Buffers.Add( CompileToBuffer( Defines ))].Name:= 'fshader';
  Defines.Free;}
end;
{
function TP3DShaderNodeTree.CompileToBuffer( Definitions: TStringList ): TP3DShaderBuffer;
var
  s, declaration, extensions: String;
begin
  Result:= TP3DShaderBuffer.Create;

  for Socket in Node.Outputs do
    if ( Socket.SocketType = 'shader' ) then
      begin
        F.Text:= TP3DShaderNodeSocket( Socket ).GetOutput( Result );
        Result.RegisterSocket( TP3DShaderNodeSocket( Socket ));
        F.SaveToFile( '/tmp/' + Socket.Name + '.glsl' );
      end;
  {Result.Defines.Text:= Definitions.Text;
  Result.Defines.Add( 'main' );
  s:= GetShaderOutputFromNodes( Result );


  Result.Defines.Text:= Definitions.Text;
  Result.Defines.Add( 'extensions' );
  extensions:= Result.Declarations.GetStringOutput( Result );

  Result.Defines.Text:= Definitions.Text;
  Result.Defines.Add( 'declaration' );
  declaration:= Result.Declarations.GetStringOutput( Result );

  Result.Code:= '#version ' + P3DGLSLVersion + LineEnding
                + extensions + LineEnding + declaration
                + 'void main(){' + LineEnding
                + s + '}';}
  P3DLog.LogInfo( Self, 'Built shader from chunks &gt;'  + LineEnding + Result.Code + LineEnding + '&lt; End of Shader' );
end;
}

function TP3DShaderNodeTree.AddCopy(Item: TP3DShaderNode): Integer;
var
  Node: TP3DShaderNode;
begin
  Node:= Item.MakeCopy;
  Result:= Nodes.Add( Node );
end;

function TP3DShaderNodeTree.AddCopy(Name: String): Integer;
var
  Node: TP3DShaderNode;
begin
  Node:= P3DShaderNodeLib.FindNode( Name );
  if ( not Assigned( Node )) then
    raise Exception.Create( 'Cannot add copy of shader node: Node "' + Name + '" not found!' );
  Result:= AddCopy( Node );
end;

function TP3DShaderNodeTree.AddCopyEx(Name: String): TP3DShaderNode;
begin
  Result:= TP3DShaderNode( Nodes[ AddCopy( Name )]);
end;

{ TP3DShaderNodeSocketList }

function TP3DShaderNodeSocketList.GetStringOutput( Buffer: TP3DShaderBuffer ): String;
var
  Socket: TP3DShaderNodeSocket;
  noduplicate: Boolean;
  S: String;
begin
  Result:= '';
  noduplicate:= ( Buffer.Defines.IndexOf( 'declaration' ) > -1 ) or ( Buffer.Defines.IndexOf( 'extensions' ) > -1 );
  for Socket in Self do
    begin
      S:= Socket.GetOutput( Buffer );
      if (( noduplicate ) and ( Pos( S, Result ) = 0 )) then
        Result+= S;
    end;
end;

function TP3DShaderNodeSocketList.FindSocketByName(Name: String): Integer;
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

{ TP3DShaderBuffer }

constructor TP3DShaderBuffer.Create;
begin
  inherited;
  Defines:= TStringList.Create;
  Defines.Sorted:= True;
  Defines.Duplicates:= dupIgnore;
  Declarations:= TP3DShaderNodeSocketList.Create;
end;

destructor TP3DShaderBuffer.Destroy;
begin
  Defines.Free;
  //Declarations.Clear( True );
  FDeclarations.Free;
  inherited Destroy;
end;

procedure TP3DShaderBuffer.RegisterSocket(Socket: TP3DShaderNodeSocket);
begin
  if ( Declarations.IndexOf( Socket ) < 0 ) then
    Declarations.Add( Socket );
end;

{ TP3DShaderNodeFragmentIfDef }

constructor TP3DShaderNodeFragmentIfDef.Create;
begin
  inherited;
  Fragments:= TP3DShaderNodeFragmentList.Create;
end;

destructor TP3DShaderNodeFragmentIfDef.Destroy;
begin
  Fragments.Clear( True );
  Fragments.Free;
  inherited Destroy;
end;

function TP3DShaderNodeFragmentIfDef.GetStringOutput(ANode: TP3DShaderNode;
  Buffer: TP3DShaderBuffer): String;
var
  Frag: TP3DShaderNodeFragment;
begin
  Result:= '';
  //WriteLn( 'IFDEF ', Caption, ' defines=', Buffer.Defines.CommaText, ' defined?=', Buffer.Defines.IndexOf( Caption ) > -1 );
  if ( Buffer.Defines.IndexOf( Caption ) > -1 ) then
    for Frag in Fragments do
      Result+= Frag.GetStringOutput( ANode, Buffer )
end;

{ TP3DShaderNodeLibrary }

procedure TP3DShaderNodeLibrary.OnChange(Sender: TPersistent; Item: TP3DFilePointer; Action: TP3DCustomListOnChangeAction);
begin
  UpdatePaths();
end;

constructor TP3DShaderNodeLibrary.Create;
var
  fpl: TP3DPropertyAccessFilePointerList;
begin
  inherited Create;
  FPaths:= TP3DFilePointerList.Create;
  FPaths.OnChange:= @OnChange;
  fpl:= TP3DPropertyAccessFilePointerList.CreateField( 'Paths', @FPaths, smAttribute );
  fpl.PathMode:= pmRelativeToBaseBath;
  Properties.Add( fpl );
  if ( Assigned( P3DConfig )) then
    P3DConfig.Config.Add( Self );
end;

destructor TP3DShaderNodeLibrary.Destroy;
begin
  if ( Assigned( P3DConfig )) then
    P3DConfig.Config.Remove( Self );
  FPaths.Clear( True );
  FPaths.Free;
  inherited Destroy;
end;

procedure TP3DShaderNodeLibrary.LoadLibrary(FileName: String);
begin
  ParseMarkdownFileAppend( FileName, Self );
end;

{
//XML deprecated
var
  F: TXMLDocument;
  i: Integer;
  Node: TP3DShaderNode;
begin
  XMLRead.ReadXMLFile( F, FileName );

  for i:= 0 to F.DocumentElement.ChildNodes.Count - 1 do
    if ( F.DocumentElement.ChildNodes[ i ].NodeName = 'node' ) then
      begin
        Node:= TP3DShaderNode.CreateFromDOM( TDOMElement( F.DocumentElement.ChildNodes[ i ]));
        Nodes.Add( Node );
      end;

  F.Free;
end;
}

procedure TP3DShaderNodeLibrary.LoadLibraryPath( PathName: String; const Ext: String );
var
  Files: TStringList;
  line: String;
  Path: String;
begin
  Path:= IncludeTrailingPathDelimiter( PathName ); //P3DSearchPaths.FindFileEx( PathName, GetCurrentDir()));
  //if ( Path = DirectorySeparator ) then
  if ( not DirectoryExists( Path )) then
    raise Exception.Create( Format( 'Could not load shader library from path. The specified path "%s" was not found', [ PathName ]));
  Files:= P3DListFolderFiles( Path + Ext, True, True );
  for line in Files do
    LoadLibrary( line );
  Files.Free;
end;

function TP3DShaderNodeLibrary.FindNode(ANode: String): TP3DShaderNode;
var
  Node: TP3DNode;
begin
  Result:= nil;
  for Node in Nodes do
    if ( Node.Name = ANode ) then
      begin
        Result:= TP3DShaderNode( Node );
        break;
      end;
end;

class function TP3DShaderNodeLibrary.DOMNodeName: String;
begin
  Result:= 'shadernodelibrary';
end;

procedure TP3DShaderNodeLibrary.LoadFromDOMNew(ADOMNode: TDOMElement);
var
  Path: TDOMElement;
  value: DOMString;
begin
  inherited LoadFromDOMNew(ADOMNode);

  Path:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Path )) do
    if ( Path.NodeName = 'path' ) then
      begin
        value:= Path.GetAttribute( 'value' );
        Paths.Add( TP3DFilePointer.Create( value ));
        Path:= TDOMElement( Path.NextSibling );
      end;
end;

procedure TP3DShaderNodeLibrary.UpdatePaths(const Index: Integer);
var
  Path: TP3DFilePointer;
begin
  if ( Index < 0 ) then
    begin
      Nodes.Clear( True );
      for Path in Paths do
        LoadLibraryPath( Path.FileName );
    end
  else
    //No idea..
end;


{ TP3DShaderNodeFragmentInput }

function TP3DShaderNodeFragmentInput.GetStringOutput(ANode: TP3DShaderNode;
  Buffer: TP3DShaderBuffer): String;
var
  S, n: Integer;
begin
  //Buffer.Defines.Add( Self.InputName );
  S:= ANode.Inputs.FindSocketByName( InputName );
  if ( S > -1 ) then
    if ( ANode.Inputs[ S ] is TP3DShaderNodeSocket ) then
      Result:= TP3DShaderNodeSocket( ANode.Inputs[ S ]).GetOutput( Buffer );
  //if ( Buffer.Defines.Find( Self.InputName, n )) then
  //  Buffer.Defines.Delete( n );
end;

function TP3DShaderNodeFragmentInput.MakeCopy(ANode: TP3DShaderNode
  ): TP3DShaderNodeFragment;
begin
  Result:=inherited MakeCopy(ANode);
  TP3DShaderNodeFragmentInput( Result ).InputName:= InputName;
end;

{ TP3DShaderNodeSocket }

procedure TP3DShaderNodeSocket.SetConnected(AValue: TP3DNodeSocket);
begin
  inherited SetConnected(AValue);
  {if ( Node is TP3DShaderNodeDynamic ) then
    TP3DShaderNodeDynamic( Node ).SocketChanged( Self );}
end;

constructor TP3DShaderNodeSocket.Create(ANode: TP3DNode;
			ADirection: TP3DNodeSocketDirection);
begin
  inherited;
  FFragments:= TP3DShaderNodeFragmentList.Create;
end;

destructor TP3DShaderNodeSocket.Destroy;
begin
  if ( not FIsCopy ) then
    begin
      Fragments.Clear( True );
      FFragments.Free
    end
  else
    FFragments:= nil;
  inherited Destroy;
end;

function TP3DShaderNodeSocket.GetOutput( Buffer: TP3DShaderBuffer ): String;
var
  i, n: Integer;
  s: String;
begin
  Result:= '';
  //Buffer.Defines.Add( Self.Name );
  if ( Assigned( Connected )) then
    begin
      if ( Connected is TP3DShaderNodeSocket ) then
        Result:= TP3DShaderNodeSocket( Connected ).GetOutput( Buffer );
      {s:= '';
      if ( Fragments.Count > 0 ) then
        for i:= 0 to Fragments.Count - 1 do
          s+= Fragments[ i ].GetStringOutput( TP3DShaderNode( Node ), Buffer );
      if ( s > '' ) then
        Result:= ReplaceStr( s, '%i', Result );}
    end
  else
    begin
      Buffer.RegisterSocket( Self );
      for i:= 0 to Fragments.Count - 1 do
        Result+= Fragments[ i ].GetStringOutput( TP3DShaderNode( Node ), Buffer );
    end;
  //if ( Buffer.Defines.Find( Self.Name, n )) then
  //  Buffer.Defines.Delete( n );
end;

function TP3DShaderNodeSocket.MakeCopy(ANode: TP3DNode): TP3DShaderNodeSocket;
type
  TP3DShaderNodeSocketClass = class of TP3DShaderNodeSocket;
begin
  Result:= TP3DShaderNodeSocketClass( ClassType ).Create( ANode, Direction );
  Result.Name:= Name;
  Result.Fragments.Free;
  Result.FIsCopy:= True;
  Result.Fragments:= Fragments;
end;

{ TP3DShaderNodeSocketShader }

class function TP3DShaderNodeSocketShader.GetSocketType: String;
begin
  Result:= 'shader'
end;

function TP3DShaderNodeSocketShader.GetOutput(Buffer: TP3DShaderBuffer): String;
begin
  if (( Direction = nsdOutput ) or Assigned( Connected )) then
    Result:= inherited GetOutput( Buffer )
  else
    Result:= Value;
end;



{ TP3DShaderCompiled }

constructor TP3DShaderCompiled.Create;
begin
  inherited;
  FBuffers:= TP3DShaderBufferList.Create;
end;

destructor TP3DShaderCompiled.Destroy;
begin
  FBuffers.Clear( True );
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

{ TP3DShaderNodeFragmentInline }

function TP3DShaderNodeFragmentInline.GetStringOutput(ANode: TP3DShaderNode; Buffer: TP3DShaderBuffer): String;
begin
  inherited;
  Result:= Text;
end;

function TP3DShaderNodeFragmentInline.MakeCopy(ANode: TP3DShaderNode): TP3DShaderNodeFragment;
begin
  Result:=inherited MakeCopy(ANode);
  TP3DShaderNodeFragmentInline( Result ).Text:= Text;
end;

{ TP3DShaderNodeFragment }

constructor TP3DShaderNodeFragment.Create;
begin
  inherited Create;
end;

function TP3DShaderNodeFragment.GetStringOutput(ANode: TP3DShaderNode; Buffer: TP3DShaderBuffer): String;
begin
  Result:= '';
end;

function TP3DShaderNodeFragment.MakeCopy(ANode: TP3DShaderNode
  ): TP3DShaderNodeFragment;
type
  TP3DShaderNodeFragmentClass = class of TP3DShaderNodeFragment;
begin
  Result:= TP3DShaderNodeFragmentClass( ClassType ).Create();
  Result.VarType:= VarType;
end;

{ TP3DShaderNode }

constructor TP3DShaderNode.CreateFromDOM( DomNode: TDOMElement );
  procedure CreateFragmentsFromDom( AFragments: TP3DShaderNodeFragmentList; Node: TDOMElement );
  var
    Item: TP3DShaderNodeFragment;
    i: Integer;
  begin
    for i:= 0 to Node.ChildNodes.Count - 1 do
      begin
        case ( Node.ChildNodes[ i ].NodeName ) of //only manual line breaks
          '#text':
            begin
              Item:= TP3DShaderNodeFragmentInline.Create();
              TP3DShaderNodeFragmentInline( Item ).Text:= StringReplace( Node.ChildNodes[ i ].NodeValue, LineEnding, '', [rfReplaceAll]);
            end;
          'br':
            begin
              Item:= TP3DShaderNodeFragmentInline.Create();
              TP3DShaderNodeFragmentInline( Item ).Text:= LineEnding;
            end;
          'input':
            begin
              Item:= TP3DShaderNodeFragmentInput.Create();
              TP3DShaderNodeFragmentInput( Item ).InputName:= TDOMElement( Node.ChildNodes[ i ]).AttribStrings[ 'name' ];
            end;
          else
            begin
              Item:= TP3DShaderNodeFragmentIfDef.Create();
              TP3DShaderNodeFragmentIfDef( Item ).Caption:= TDOMElement( Node.ChildNodes[ i ]).NodeName;
              CreateFragmentsFromDom( TP3DShaderNodeFragmentIfDef( Item ).Fragments, TDOMElement( Node.ChildNodes[ i ]));
            end;


            //raise Exception.Create( 'Error: Unknown section for output socket''s code: "' + Node.ChildNodes[ i ].NodeName + '". Plain text, "input" or "br" expected.' );
        end;
        if ( Assigned( Item )) then
          AFragments.Add( Item );
      end;
  end;

  function CreateSocketFromDOM( Node: TDOMElement ): TP3DShaderNodeSocket;
  var
    AName: TDOMAttr;
    AType: TDOMAttr;
    i: Integer;
		Dir: TP3DNodeSocketDirection;
  begin
    if ( lowercase( Node.NodeName ) = 'input' ) then
      Dir:= nsdInput
    else
      Dir:= nsdOutput;
    AName:= Node.GetAttributeNode( 'name' );
    if ( not Assigned( AName )) then
      raise Exception.Create( 'Error: Sockets must have a name.' );
    AType:= Node.GetAttributeNode( 'type' );
    if ( Assigned( AType )) then
      case AType.NodeValue of
        'vec4': Result:= TP3DShaderNodeSocketVector.Create( Self, Dir );
        'float': Result:= TP3DShaderNodeSocketFloat.Create( Self, Dir );
        'int': Result:= TP3DShaderNodeSocketInt.Create( Self, Dir );
        'shader': Result:= TP3DShaderNodeSocketShader.Create( Self, Dir );
        'mat4': Result:= TP3DShaderNodeSocketMatrix.Create( Self, Dir );
      else
        Result:= TP3DShaderNodeSocket.Create( Self, Dir );
      end;
    Result.Name:= AName.NodeValue;

    CreateFragmentsFromDom( Result.Fragments, TDOMElement( Node ))
  end;

var
  AName: TDOMAttr;
  i: Integer;
begin
  AName:= DomNode.GetAttributeNode( 'name' );
  if ( not Assigned( AName )) then
    raise Exception.Create( 'Error: Nodes must have a name.' );
  Create;

  Name:= AName.NodeValue;
  for i:= 0 to DomNode.ChildNodes.Count - 1 do
    case lowercase( DomNode.ChildNodes[ i ].NodeName ) of
      'input': Inputs.Add(CreateSocketFromDOM( TDOMElement( DomNode.ChildNodes[ i ])));
      'output': Outputs.Add(CreateSocketFromDOM( TDOMElement( DomNode.ChildNodes[ i ])));
      else
        raise Exception.Create( 'Error: Unknown section for nodes: "' + DomNode.ChildNodes[ i ].NodeName + '". "input" or "output" expected.' );
    end;
end;

function TP3DShaderNode.MakeCopy: TP3DShaderNode;
var
  Socket: TP3DNodeSocket;
begin
  Result:= TP3DShaderNode.Create;
  Result.Name:= Name;
  for Socket in Inputs do
    Result.Inputs.Add( TP3DShaderNodeSocket( Socket ).MakeCopy( Result ));
  for Socket in Outputs do
    Result.Outputs.Add( TP3DShaderNodeSocket( Socket ).MakeCopy( Result ));
end;

{$INCLUDE p3dshadernodes_core.inc}
{$INCLUDE p3dshadernodes_markdown.inc}

{$ENDIF}


{$IFDEF INITIALIZATION}
  if ( not Assigned( P3DShaderNodeLib )) then
    P3DShaderNodeLib:= TP3DShaderNodeLibrary.Create;
{$ENDIF}

{$IFDEF FINALIZATION}
  if ( Assigned( P3DShaderNodeLib )) then
    FreeAndNil( P3DShaderNodeLib );
{$ENDIF}
