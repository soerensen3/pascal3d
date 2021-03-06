//p3dshadernodes.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

type
  { TP3DShaderNode }
  TP3DShaderNodeTree = class;


  TP3DShaderCompiled = class;
  TP3DShaderBuffer = class;
  TP3DShaderNodeSocket = class;
  TP3DInputCallback = function ( Sender: TP3DNode; Ref: String; Context: TP3DShaderCompiled ): String of object;

  TP3DCustomShaderNodeSocketList = specialize gP3DCustomObjectList < TP3DShaderNodeSocket >;

  {$INCLUDE p3d.core.shadernodes_fragments.inc}
  { TP3DShaderNodeSocket }

  TP3DShaderNodeSocket = class ( TP3DNodeSocket )
    protected
      FFragments: TP3DShaderNodeFragmentList;

      function GetSocketClass: TP3DNodeSocketCloneClass; override;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      function GetOutput( Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone ): String; virtual;
      //function GetOutputAll( Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone; Filter: String ): String;
      function GetOutputAsType( Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone ): String; virtual;
      procedure GetAllInputs( Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone; AFilter: String; AInputs: TP3DNodeSocketClonePointerList; ATypes: TP3DNodeSocketClonePointerList; ALibs: TP3DNodeSocketPointerList ); virtual;

    published
      property Fragments: TP3DShaderNodeFragmentList read FFragments write FFragments;
  end;

  { TP3DShaderNodeSocketClone }

  TP3DShaderNodeSocketClone = class ( TP3DNodeSocketClone )
    public
      function GetOutput( Buffer: TP3DShaderBuffer ): String; virtual;
      //function GetOutputAll( Buffer: TP3DShaderBuffer; Filter: String ): String;
      procedure GetAllInputs( Buffer: TP3DShaderBuffer; Filter: String; Inputs, Types: TP3DNodeSocketClonePointerList; ALibs: TP3DNodeSocketPointerList );
  end;

  { TP3DShaderNodeType }

  TP3DShaderNodeType = class ( TP3DNode )
    public
      function GetOutputAsType( Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone; ATypeCast: String = 'constructor' ): String;
  end;

  TP3DShaderNodeLibrary = class;

  { TP3DMarkdownFile }

  TP3DMarkdownFile = class( TP3DNamedStreamable )
    private
      FFilePointer: TP3DFilePointer;
      FNodes: TP3DNodePointerList;
      FShaderLib: TP3DStreamablePointer;
      FTypes: TP3DNodePointerList;
      procedure SetName(AValue: String); override;

      procedure FileChanged( Sender: TP3DFilePointer; out DoReload: Boolean );
      procedure FileNameChange( Sender: TP3DFilePointer; ANewValue: String );
      function GetShaderLib: TP3DShaderNodeLibrary;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;
      procedure Reload;

      property Nodes: TP3DNodePointerList read FNodes;
      property Types: TP3DNodePointerList read FTypes;
      property FilePointer: TP3DFilePointer read FFilePointer write FFilePointer;
      property ShaderLib: TP3DShaderNodeLibrary read GetShaderLib;
  end;

  TP3DMarkdownFileContainerList = specialize gP3DNamedStreamableContainerList < TP3DMarkdownFile >;

  TP3DShaderNodeTypeContainerList = specialize gP3DNamedStreamableContainerList < TP3DShaderNodeType >;


  { TP3DShaderNodeLibrary }

  TP3DShaderNodeLibrary = class( TP3DJSONRoot )
    private
      FFiles: TP3DMarkdownFileContainerList;
      FPaths: TP3DFilePointerList;
      FTypes: TP3DShaderNodeTypeContainerList;
      FNodes: TP3DNodeContainerList;

      procedure OnChange( ASender: TObject; AItem: TP3DFilePointer; AAction: TP3DListOnChangeAction );
      procedure OnFileChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure LoadLibrary( FileName: String );
      procedure LoadLibraryPath( PathName: String; const Ext: String = '*.pmd' );
      function FindNode( ANode: String ): TP3DNode;
      function FindType( AType: String ): TP3DShaderNodeType;
      function GetOutputAsType( Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone; const ATypeCast: String = 'constructor' ): String;
      function DebugOutput: String;

      procedure UpdatePaths( const Index: Integer = -1 );

      property Paths: TP3DFilePointerList read FPaths write FPaths;
      property Types: TP3DShaderNodeTypeContainerList read FTypes;
      property Nodes: TP3DNodeContainerList read FNodes;
      property Files: TP3DMarkdownFileContainerList read FFiles;
  end;

  { TP3DShaderBuffer }

  TP3DShaderBuffer = class
    private
      FCode: String;
      FDeclarations: TStringList;
      FDefines: TStringList;
      FName: String;

    public
      constructor Create;
      destructor Destroy; override;

      procedure RegisterFragment( Fragment: TP3DShaderNodeFragmentSingleOnly; ACloneSocket: TP3DShaderNodeSocketClone );

    published
      property Code: String read FCode write FCode;
      property Name: String read FName write FName;
      property Defines: TStringList read FDefines write FDefines;
      property Declarations: TStringList read FDeclarations write FDeclarations;
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

  { gP3DShaderNodeTree }

  generic gP3DShaderNodeTree < T: TP3DNodeClone > = class ( specialize gP3DStreamableContainerList < T > )
    protected
      FNodeMaster: TP3DNodeMaster;
      function GetShaderOutputFromNodes( Buffer: TP3DShaderBuffer ): String;

      procedure Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction); override;
      procedure CompileNode( Node: TP3DNodeClone; Shader: TP3DShaderCompiled );

    public
      function AddCopy( AItem: TP3DNode): T; virtual;
      function AddCopy( AName: String ): T; virtual;
      function FindByName( AName: String ): T;

      function Compile( const ARootNode: TP3DNodeClone = nil ): TP3DShaderCompiled;
      procedure SortByName;
      property NodeMaster: TP3DNodeMaster read FNodeMaster write FNodeMaster;
  end;

  TP3DShaderNodeTree = class( specialize gP3DShaderNodeTree < TP3DNodeClone >);


  {TP3DShaderNodeMaster = class ( TP3DNodeMaster )
    protected
      FAutoRecompile: Boolean;
      FRootNode: TP3DStreamableContainer;
      FGeometryNode: TP3DStreamableContainer;
      FNodeTree: TP3DShaderNodeTree;
      FUniforms: TP3DShaderNodeTree;

      function GetGeometryNode: TP3DNodeClone;
      procedure SetGeometryNode(AValue: TP3DNodeClone);
      function GetRootNode: TP3DNodeClone;
      procedure SetRootNode(AValue: TP3DNodeClone);
      procedure SetupRootNode;
      procedure MapsChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );
      procedure NodesChange( ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction );
      procedure NodeChange( ANewValue: TP3DStreamable );
      procedure UpdateTexNodes;
      function GetShader: TP3DShader; override;
      procedure SocketHandler( Sender: TP3DNodeSocketClone );

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;

      procedure MakeRootNode( AName: String );
      procedure MakeRootNode( ANode: TP3DNode );
      procedure BuildShader; override;
      procedure PassToShader(const ActiveSubset: String=''); override;
      function FindUniform( AName: String ): TP3DNodeClone;
      function AddMap( ATexture: TP3DTexture; ATexChannel: Integer ): TP3DMaterialMap;

      property RootNode: TP3DNodeClone read GetRootNode write SetRootNode;
      property GeometryNode: TP3DNodeClone read GetGeometryNode write SetGeometryNode;
      property NodeTree: TP3DShaderNodeTree read FNodeTree;
      property Uniforms: TP3DShaderNodeTree read FUniforms;
      property Libraries: TP3DShaderNodeTree read FLibraries;
      property AutoRecompile: Boolean read FAutoRecompile write FAutoRecompile;
      property OnPassUniform: TOnPassUniform read FOnPassUniform write FOnPassUniform;
  end;}


  { TP3DShaderBuilder }

  TP3DShaderBuilder = class ( TP3DJSONRoot )
    private
      FLibraries: TP3DShaderNodeTree;
      FNodeMaster: TP3DNodeMaster;
      FRootNode: TP3DStreamableContainer;
      FGeometryNode: TP3DStreamableContainer;
      FNodeTree: TP3DShaderNodeTree;
      FUniforms: TP3DShaderNodeTree;
      function GetGeometryNode: TP3DNodeClone;
      function GetRootNode: TP3DNodeClone;
      procedure SetGeometryNode(AValue: TP3DNodeClone);
      procedure SetRootNode(AValue: TP3DNodeClone);
      procedure SetupRootNode;

    public
      constructor Create;
      destructor Destroy; override;

      procedure MakeRootNode( AName: String );
      procedure MakeRootNode( ANode: TP3DNode );
      procedure BuildShader( AExtensions: TStringList; AOwner: TP3DStreamableContainer );

      property RootNode: TP3DNodeClone read GetRootNode write SetRootNode;
      property GeometryNode: TP3DNodeClone read GetGeometryNode write SetGeometryNode;
      property NodeTree: TP3DShaderNodeTree read FNodeTree;
      property Uniforms: TP3DShaderNodeTree read FUniforms;
      property Libraries: TP3DShaderNodeTree read FLibraries;
      property NodeMaster: TP3DNodeMaster read FNodeMaster;
  end;

  {$INCLUDE p3d.core.shadernodes_markdown.inc}
  {.$INCLUDE p3dshadernodes_core.inc}

  const P3DGLSLVersion: String = '120';

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ gP3DShaderNodeTree }

function gP3DShaderNodeTree.GetShaderOutputFromNodes(Buffer: TP3DShaderBuffer ): String;
var
  Node: TP3DNodeClone;
  Socket: TP3DNodeSocketClone;
  Code: String;
begin
  Result:= '';
  for Node in Self do
    for Socket in Node.Outputs do
      if ( Socket.CloneOf.SocketType = 'text' ) then
        begin
          //WriteLn( 'Processing ', Node.Name, '.', Socket.Name, ' defines=', Buffer.Defines.CommaText );
          Code:= TP3DShaderNodeSocketClone( Socket ).GetOutput( Buffer );
          if ( Code > '' ) then
            Result+= {'/*' + Node.CloneOf.Name + '-->*/' + LineEnding + }
                     Code
                   {+ '/*<--' + Node.CloneOf.Name + '*/'};
        end;
end;

procedure gP3DShaderNodeTree.Notify(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  inherited Notify( ASender, AItem, AAction );
  if (( AAction = actAdd ) and ( AItem is TP3DNodeClone )) then
    TP3DNodeClone( AItem ).NodeMaster:= NodeMaster;
end;

procedure gP3DShaderNodeTree.CompileNode(Node: TP3DNodeClone; Shader: TP3DShaderCompiled);
var
  Socket: TP3DNodeSocketClone;
  Buffer: TP3DShaderBuffer;
begin
  for Socket in Node.Outputs do
    if ( Socket.CloneOf.SocketType = 'text' ) then
      begin
        Buffer:= TP3DShaderBuffer.Create;
        Buffer.Name:= Socket.CloneOf.Name;
        Buffer.Code:= TP3DShaderNodeSocketClone( Socket ).GetOutput( Buffer );
        Shader.Buffers.Add( Buffer );
        log_info( message_header( Self ) + '<details><summary>Built ' + Buffer.Name + ' from chunks <br/></summary><pre><code class="glsl">' + P3DTextToHTML( Buffer.Code ) + '</code></pre></details>' );
      end;
end;

function gP3DShaderNodeTree.Compile(const ARootNode: TP3DNodeClone): TP3DShaderCompiled;
var
  Node: TP3DNodeClone;

begin
  Result:= TP3DShaderCompiled.Create;
  if ( Assigned( ARootNode )) then
    CompileNode( ARootNode, Result )
  else for Node in Self do
    CompileNode( Node, Result );
end;

function CompareShaderNode(Item1, Item2: Pointer): Integer;
begin
  Result:= CompareStr( TP3DNodeClone( TP3DStreamableContainer( Item1 ).AsValue ).CloneOf.Name, TP3DNodeClone( TP3DStreamableContainer( Item2 ).AsValue ).CloneOf.Name );
end;

procedure gP3DShaderNodeTree.SortByName;
begin
  //FItems.Sort( @CompareShaderNode );
end;

function gP3DShaderNodeTree.AddCopy(AItem: TP3DNode): T;
begin
  Result:= T( AItem.Clone( Self, TP3DNodeCloneClass( ItemClass )));
end;

function gP3DShaderNodeTree.AddCopy(AName: String): T;
var
  Node: TP3DNode;
begin
  Node:= P3DShaderNodeLib.FindNode( AName );
  if ( not Assigned( Node )) then
    raise Exception.Create( 'Cannot add copy of shader node: Node "' + AName + '" not found!' );
  Result:= AddCopy( Node );
end;

function gP3DShaderNodeTree.FindByName(AName: String): T;
var
  Item: T;
begin
  Result:= default( T );
  for Item in Self do
    if ( Item.CloneOf.Name = AName ) then begin
      Result:= Item;
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
  Declarations:= TStringList.Create;
end;

destructor TP3DShaderBuffer.Destroy;
begin
  Defines.Free;
  //Declarations.Clear( True );
  FDeclarations.Free;
  inherited Destroy;
end;

procedure TP3DShaderBuffer.RegisterFragment(Fragment: TP3DShaderNodeFragmentSingleOnly;
  ACloneSocket: TP3DShaderNodeSocketClone);
var
  S: String;
begin
  S:= Fragment.Serialize( ACloneSocket );
  if ( Declarations.IndexOf( S ) < 0 ) then
    Declarations.Add( S );
end;

{$INCLUDE p3d.core.shadernodes_fragments.inc}


{ TP3DShaderNodeLibrary }

procedure TP3DShaderNodeLibrary.OnChange(ASender: TObject; AItem: TP3DFilePointer; AAction: TP3DListOnChangeAction);
begin
  UpdatePaths();
end;

procedure TP3DShaderNodeLibrary.OnFileChange(ASender: TObject; AItem: TP3DStreamable; AAction: TP3DListOnChangeAction);
begin
  if ( AAction = actAdd ) then
    TP3DMarkdownFile( AItem ).FShaderLib.AsValue:= Self
  else if ( AAction = actExtract ) then
    TP3DMarkdownFile( AItem ).FShaderLib.AsValue:= nil;
end;

constructor TP3DShaderNodeLibrary.Create(const AOwner: IP3DStreamableContainer);
var
  fpl: TP3DPropAccessFilePointerList;
begin
  inherited Create( AOwner );
  FPaths:= TP3DFilePointerList.Create;
  FPaths.OnChange:= @OnChange;
  fpl:= TP3DPropAccessFilePointerList.CreateField( 'Paths', @FPaths, smText );
  fpl.PathMode:= pmRelativeToBaseBath;
  Properties.Add( fpl );

  FNodes:= TP3DNodeContainerList.Create( 'Nodes' );
  FTypes:= TP3DShaderNodeTypeContainerList.Create( 'Types' );
  FFiles:= TP3DMarkdownFileContainerList.Create( 'Files' );
  FFiles.OnChange:= @OnFileChange;
  Properties.Add([ FTypes, FNodes, FFiles ]);
  if ( Assigned( P3DConfig )) then
    P3DConfig.AddStreamable( 'P3DShaderNodeLib', Self );
end;

destructor TP3DShaderNodeLibrary.Destroy;
begin
  if ( Assigned( P3DConfig )) then
    P3DConfig.RemoveStreamable( 'P3DShaderNodeLib' );
  FPaths.OnChange:= nil;
  FPaths.Clear;
  FPaths.Free;
  inherited Destroy;
end;

procedure TP3DShaderNodeLibrary.LoadLibrary(FileName: String);
begin
  ParseMarkdownFileAppend( FileName, Self );
end;

procedure TP3DShaderNodeLibrary.LoadLibraryPath( PathName: String; const Ext: String );
var
  AllFiles: TStringList;
  line: String;
  Path: String;
begin
  Path:= IncludeTrailingPathDelimiter( PathName ); //P3DSearchPaths.FindFileEx( PathName, GetCurrentDir()));
  //if ( Path = DirectorySeparator ) then
  if ( not DirectoryExists( Path )) then
    raise Exception.Create( Format( 'Could not load shader library from path. The specified path "%s" was not found', [ PathName ]));
  AllFiles:= P3DListFolderFiles( Path + Ext, True, True );
  for line in AllFiles do
    LoadLibrary( line );
  AllFiles.Free;
end;

function TP3DShaderNodeLibrary.FindNode(ANode: String): TP3DNode;
var
  Node: TP3DNode;
begin
  Result:= nil;
  for Node in Nodes do
    if ( Node.Name = ANode ) then
      begin
        Result:= TP3DNode( Node );
        break;
      end;
end;

function TP3DShaderNodeLibrary.FindType(AType: String): TP3DShaderNodeType;
var
  Node: TP3DNode;
begin
  Result:= nil;
  for Node in Types do
    if ( Node.Name = AType ) then
      begin
        Result:= TP3DShaderNodeType( Node );
        break;
      end;
end;

function TP3DShaderNodeLibrary.GetOutputAsType(Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone;
  const ATypeCast: String): String;
var
  T: TP3DShaderNodeType;
begin
  T:= FindType( ACloneSocket.CloneOf.SocketType );
  if ( Assigned( T )) then
    Result:= T.GetOutputAsType( Buffer, ACloneSocket, ATypeCast )
  else
    raise Exception.CreateFmt( 'Could not find type "%s" for "%s"!', [ ACloneSocket.CloneOf.SocketType, ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name ]);
end;

function TP3DShaderNodeLibrary.DebugOutput: String;
  function CheckFragmentRestrictions( Frag: TP3DShaderNodeFragment; var restrictions: String ): Boolean;
  var
    restr: String;
  begin
    restr:= restrictions;
    if ( Frag is TP3DShaderNodeFragmentIfDef ) then
      restrictions:= TP3DShaderNodeFragmentIfDef( Frag ).CheckDefs.CommaText;
    Result:= ( restr <> restrictions );
  end;

  function ProcessFragments( Socket: TP3DNodeSocket; Frags: TP3DShaderNodeFragmentList; var restrictions: String ): String;
  var
    Fragment: TP3DShaderNodeFragment;

  begin
    Result:= '';
    for Fragment in Frags do
      begin
        if ( CheckFragmentRestrictions( Fragment, restrictions )) then
          begin
            if ( Fragment <> Frags[ 0 ]) then
              Result += LineEnding + '```' + LineEnding + LineEnding;

            //if ( not ( NoHeader and ( Fragment = Frags[ 0 ]))) then
              Result += Format( '``` { name: "%s", type: "%s"%s%s }' + LineEnding, [ Socket.Name, Socket.SocketType,
                                                                                     {BoolToStr( Fragment.SingleOnly, ', single: "yes"', '' )}'',
                                                                                     BoolToStr( restrictions > '', ', restrict: "' + restrictions + '"', '' )]);
          end;

        if ( Fragment is TP3DShaderNodeFragmentInline ) then
          Result += TP3DShaderNodeFragmentInline( Fragment ).Text
        else if ( Fragment is TP3DShaderNodeFragmentIfDef ) then
          Result += ProcessFragments( Socket, TP3DShaderNodeFragmentIfDef( Fragment ).Fragments, restrictions )
        else if ( Fragment is TP3DShaderNodeFragmentSelector ) then
          Result += Format( '$%s: %s$', [ TP3DShaderNodeFragmentSelector( Fragment ).InputName,
                                          TP3DShaderNodeFragmentSelector( Fragment ).Defines.CommaText ]);
    end;
  end;

var
  Node: TP3DNode;
  Socket: TP3DNodeSocket;
  restrictions: String;
begin
  Result:= '';
  for Node in Nodes do
    begin
      Result += '---' + LineEnding;
      Result += 'node: ' + Node.Name + LineEnding;
      for Socket in Node.Inputs do
        Result += Format( 'input: { name: "%s", type: "%s"%s }' + LineEnding, [ Socket.Name, Socket.SocketType, BoolToStr( Socket.Visible, ', visible: "yes"', '' )]);
      Result += '---' + LineEnding + LineEnding;
      restrictions:= '@'; //Something unlikely so first fragments always get header
      for Socket in Node.Outputs do
        begin
          with ( Socket as TP3DShaderNodeSocket ) do
            Result += ProcessFragments( Socket, Fragments, restrictions );
          Result += LineEnding + '```' + LineEnding + LineEnding + LineEnding;
        end;
    end;
end;

procedure TP3DShaderNodeLibrary.UpdatePaths(const Index: Integer);
var
  Path: TP3DFilePointer;
begin
  if ( Index < 0 ) then
    begin
      Nodes.Clear;
      for Path in Paths do
        if ( Assigned( Path )) then
          LoadLibraryPath( Path.FileName );
    end
  else
    //No idea..
end;



{ TP3DShaderNodeSocket }

function TP3DShaderNodeSocket.GetSocketClass: TP3DNodeSocketCloneClass;
begin
  Result:= TP3DShaderNodeSocketClone;
end;

constructor TP3DShaderNodeSocket.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FFragments:= TP3DShaderNodeFragmentList.Create;
end;

destructor TP3DShaderNodeSocket.Destroy;
begin
  FFragments.Free;
  inherited Destroy;
end;

function TP3DShaderNodeSocket.GetOutput(Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone): String;
var
  i: Integer;
  Nd: TP3DNode;
  Socket: TP3DShaderNodeSocket;
begin
  Result:= '';

  if (( Direction = nsdOutput ) or ( not Assigned( ACloneSocket.Connected ))) then
    begin
      if ( Direction = nsdOutput ) then
        Result:= Fragments.GetStringOutput( ACloneSocket, Buffer )
      else case ( SocketType ) of
        'text':
          Result:= ACloneSocket.ValueProp.AsString
        else
          Result:= P3DShaderNodeLib.GetOutputAsType( Buffer, ACloneSocket );
      end;
    end
  else if ( Assigned( ACloneSocket.Connected )) then begin
    if ( ACloneSocket.Connected.CloneOf.SocketType = 'text' ) then begin
      if ( ACloneSocket.Connected.CloneOf is TP3DShaderNodeSocket ) then
        begin Result:= TP3DShaderNodeSocket( ACloneSocket.Connected.CloneOf ).GetOutput( Buffer, ACloneSocket.Connected ); {P3DLog.LogInfo( Self, 'getting connected text output of: ' + ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name + ' = "' + Result + '"' ); } end
      else
        raise Exception.CreateFmt( 'Cannot get output of socket "%s". The connected socket is not inherited from TP3DShaderNodeSocket!', [ Name ]);
    end else begin
      Result:= P3DShaderNodeLib.GetOutputAsType( Buffer, ACloneSocket, ACloneSocket.Connected.CloneOf.SocketType );
    end;
  end;
end;
{
function TP3DShaderNodeSocket.GetOutputAll(Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone; Filter: String ): String;
{  procedure FindAllSockets( Node: TP3DNodeClone );
  var
    Sock: TP3DNodeSocketClone;
  begin
    if ( Filter > '' ) then
//      FAllSockets.Add( Node.FindOutput( Filter ))
    else for Sock in Node.Outputs do
//      FAllSockets.Add( Sock );
    for Sock in Node.Inputs do
      if ( Assigned( Sock.Connected ) and ( Sock.Connected.CloneOf.Direction = nsdOutput )) then
        FindAllSockets( Sock.Connected.Node );
  end;
}
var
  Sock: TP3DNodeSocketClone;
  i: Integer;
begin
  Result:= '';
  //WriteLn( 'Output All, Node: ', Node.Name, ' Socket: ', Name, ' Filter: ', Filter );
{  if ( not Assigned( FAllSockets )) then
    begin
      FAllSockets:= TP3DNodeSocketClonePointerList.Create( 'AllSockets' );
      Properties.Add( FAllSockets );
    end
  else
    FAllSockets.Clear;
  if ( Assigned( ACloneSocket.Connected ) and ( ACloneSocket.Connected.CloneOf.Direction = nsdOutput )) then
    FindAllSockets( ACloneSocket.Connected.Node );
  WriteLn( FAllSockets.AsString );
  //for Sock in FAllSockets.GetReverseEnumerator do
  for i:= FAllSockets.Count - 1 downto 0 do
    begin
      Sock:= FAllSockets[ i ];
      Result += TP3DShaderNodeSocket( Sock.CloneOf ).GetOutput( Buffer, Sock );
    end;}
end;
}
function TP3DShaderNodeSocket.GetOutputAsType(Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone): String;
var
  i: Integer;
begin
  Result:= '';
  if ( Direction = nsdOutput ) then
    for i:= 0 to Fragments.Count - 1 do
      Result+= Fragments[ i ].GetStringOutput( ACloneSocket, Buffer );
end;

procedure TP3DShaderNodeSocket.GetAllInputs(Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone; AFilter: String;
  AInputs: TP3DNodeSocketClonePointerList; ATypes: TP3DNodeSocketClonePointerList; ALibs: TP3DNodeSocketPointerList);

  procedure CheckAddType( Socket: TP3DNodeSocketClone );
  var
    T: TP3DShaderNodeType;
    S: TP3DShaderNodeSocket;
  begin
    T:= P3DShaderNodeLib.FindType( Socket.CloneOf.SocketType );
    if ( Assigned( T )) then begin
      S:= TP3DShaderNodeSocket( T.Outputs.GetByName( AFilter ));
      if ( Assigned( S )) then
        ATypes.Add( Socket );
    end;
  end;

begin
  if ( Assigned( ACloneSocket.Connected ) and ( ACloneSocket.Connected.CloneOf.Direction = nsdOutput )) then
    ACloneSocket.Connected.Node.FindAllSockets( AFilter, AInputs, ATypes, ALibs )
  else if ( not Assigned( ACloneSocket.Connected ) and ( ACloneSocket.CloneOf.Direction = nsdInput )) then
    CheckAddType( ACloneSocket );  //Maybe check for Libs?
end;

{ TP3DShaderNodeSocketShader }
{
function TP3DShaderNodeSocketString.GetOutput(Buffer: TP3DShaderBuffer; CloneSocket: TP3DNodeSocketClone): String;
begin
  if (( Direction = nsdOutput ) or Assigned( CloneSocket.Connected )) then
    Result:= inherited GetOutput( Buffer, CloneSocket )
  else
    Result:= Value;
end;
}


{ TP3DShaderCompiled }

constructor TP3DShaderCompiled.Create;
begin
  inherited;
  FBuffers:= TP3DShaderBufferList.Create;
end;

destructor TP3DShaderCompiled.Destroy;
begin
  FBuffers.Clear;
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

{ TP3DShaderNodeType }

function TP3DShaderNodeType.GetOutputAsType(Buffer: TP3DShaderBuffer; ACloneSocket: TP3DNodeSocketClone;
  ATypeCast: String): String;
var
  o: TP3DNodeSocket;
begin

  o:= Outputs.GetByName( ATypeCast );
  if ( o is TP3DShaderNodeSocket ) then
    Result:= TP3DShaderNodeSocket( o ).GetOutputAsType( Buffer, ACloneSocket )
  else
    raise Exception.CreateFmt( 'Cannot find type conversion "%s" for node %s with type %s!', [ ATypeCast, ACloneSocket.Node.CloneOf.Name + '.' + ACloneSocket.CloneOf.Name, ACloneSocket.CloneOf.SocketType ]);
end;



{ TP3DMarkdownFile }

procedure TP3DMarkdownFile.SetName(AValue: String);
begin

end;

procedure TP3DMarkdownFile.FileChanged(Sender: TP3DFilePointer; out DoReload: Boolean);
begin
  Reload;
  DoReload:= True;
end;

procedure TP3DMarkdownFile.FileNameChange(Sender: TP3DFilePointer; ANewValue: String);
begin
  FName:= ExtractFileNameOnly( ANewValue );
end;

function TP3DMarkdownFile.GetShaderLib: TP3DShaderNodeLibrary;
begin
  Result:= TP3DShaderNodeLibrary( FShaderLib.AsValue );
end;

constructor TP3DMarkdownFile.Create(const AOwner: IP3DStreamableContainer);
begin
  FShaderLib:= TP3DStreamablePointer.Create( 'ShaderLib', TP3DShaderNodeLibrary );

  inherited Create( AOwner );
  Properties.Add( FShaderLib );

  FFilePointer:= TP3DFilePointer.Create('');
  FilePointer.OnFileChange:= @FileChanged;
  FilePointer.OnFileNameChange:= @FileNameChange;
  Properties.Add( TP3DPropAccessFilePointer.CreateField( 'FilePointer', @FFilePointer ));

  FNodes:= TP3DNodePointerList.Create( 'Nodes' );
  FTypes:= TP3DNodePointerList.Create( 'Types' );
  Properties.Add( FNodes );
  Properties.Add( FTypes );
end;

destructor TP3DMarkdownFile.Destroy;
var
  json: TJSONData;
begin
  {WriteLn( 'Deleting ' + FilePointer.FileName );
  json:= Nodes.AsJSON;
  WriteLn( json.FormatJSON());
  json.Free;}
  FreeAndNil( FFilePointer );
  inherited Destroy;
end;

procedure TP3DMarkdownFile.Reload;
  procedure ReplaceNode( Cnt: TP3DStreamableContainer; ReplaceWith: TP3DNode );
  var
    Nd: TP3DNode;
    Socket, Socket2: TP3DNodeSocket;
  begin
    {Nd:= Cnt.AsValue as TP3DNode;
    for Socket in Nd.Inputs do
      begin
        Socket2:= ReplaceWith.Inputs.GetByName( Socket.Name );
        Socket.Users.UpdateAllUsers( Socket, Socket2 );
      end;
    for Socket in Nd.Outputs do
      begin
        Socket2:= ReplaceWith.Outputs.GetByName( Socket.Name );
        Socket.Users.UpdateAllUsers( Socket, Socket2 );
      end;}
    Cnt.AsValue:= ReplaceWith;
  end;

var
  Lib: TP3DShaderNodeLibrary;
  i: Integer;
  json: TJSONData;
  Nd: TP3DNode;
  TmpLibCont: TP3DStreamableContainer;
begin
  TmpLibCont:= TP3DStreamableContainer.Create( 'TmpLibCont', TP3DShaderNodeLibrary );
  TmpLibCont.Root:= Root;
  TmpLibCont.Context:= Self;
  ParseMarkdownFile( FilePointer.FileName, TmpLibCont );
  Lib:= TP3DShaderNodeLibrary( TmpLibCont.AsValue );
  for i:= Nodes.Count - 1 downto 0 do
    if ( Lib.Nodes.GetByName( Nodes[ i ].Name ) = nil ) then
      Nodes[ i ].Free;
  Nodes.Clear;
  while Lib.Nodes.Count > 0 do
    begin
      Nd:= Lib.Nodes[ 0 ];
      i:= ShaderLib.Nodes.FindByName( Nd.Name );
      if ( i > -1 ) then
        ReplaceNode( TP3DStreamableContainer( ShaderLib.Nodes.Items[ i ]), Nd )
      else
        Nd.Owner:= ShaderLib.Nodes;
      Nodes.Add( Nd );
    end;
  for i:= 0 to Nodes.Count - 1 do
    begin
      json:= Nodes[ i ].AsJSON;
      WriteLn( 'Nodes[', i, ']: ' + json.FormatJSON());
      json.Free;
    end;
  json:= Lib.Nodes.AsJSON;
  WriteLn( Lib.Nodes.GetFullPath + ': ' + json.FormatJSON());
  json.Free;
  Lib.Free;
  TmpLibCont.Free;
end;

{ TP3DShaderNodeSocketClone }

function TP3DShaderNodeSocketClone.GetOutput(Buffer: TP3DShaderBuffer): String;
begin
  if ( CloneOf is TP3DShaderNodeSocket ) then
    Result:= TP3DShaderNodeSocket( CloneOf ).GetOutput( Buffer, Self )
  else
    Result:= ValueProp.AsString;
end;

{
function TP3DShaderNodeSocketClone.GetOutputAll(Buffer: TP3DShaderBuffer; Filter: String ): String;
begin
  if ( CloneOf is TP3DShaderNodeSocket ) then
    Result:= TP3DShaderNodeSocket( CloneOf ).GetOutputAll( Buffer, Self, Filter )
  else
    Result:= ValueProp.AsString;
end;
}

procedure TP3DShaderNodeSocketClone.GetAllInputs(Buffer: TP3DShaderBuffer; Filter: String; Inputs, Types: TP3DNodeSocketClonePointerList;
  ALibs: TP3DNodeSocketPointerList);
begin
  if ( CloneOf is TP3DShaderNodeSocket ) then begin
    TP3DShaderNodeSocket( CloneOf ).GetAllInputs( Buffer, Self, Filter, Inputs, Types, ALibs );
    Node.FindLibsOutput( Filter, ALibs );
  end;
end;


{.$INCLUDE p3dshadernodes_core.inc}
{$INCLUDE p3d.core.shadernodes_markdown.inc}

{$ENDIF}


{$IFDEF INITIALIZATION}
  if ( not Assigned( P3DShaderNodeLib )) then
    P3DShaderNodeLib:= TP3DShaderNodeLibrary.Create;
{$ENDIF}

{$IFDEF FINALIZATION}
  if ( Assigned( P3DShaderNodeLib )) then
    FreeAndNil( P3DShaderNodeLib );
{$ENDIF}
