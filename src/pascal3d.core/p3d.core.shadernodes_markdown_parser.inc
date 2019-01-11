{ p3d.core.shadernodes_markdown_parser.inc }

{$IFDEF INTERFACE}

{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure ParseNode;
var
  NameSet: Boolean = False;
  breakat: SizeInt;

  procedure ParseInput;
  var
    SocketName: String = '';
    SocketType: String = '';
    SocketDefault: String = '';
    SocketVisible: Boolean = True;
    symb: String;
    IsString: Boolean;
  begin
    SocketType:= ParseWord;
    SocketName:= ParseWord;
    symb:= ParseSymbol();
    if ( symb = '=' ) then begin
      SocketDefault:= ParseString( IsString );
    end else if ( symb <> '' ) then
      Error( 'Unexpected symbol in input statement!' );

    if ( GlobalActiveNode is TP3DShaderNodeType ) then
      Error( 'Cannot define inputs for types!' );

    SocketVisible:= SocketName[ 1..2 ] = '__';

    CreateSocketFromType( GlobalActiveNode, nsdInput, SocketType, SocketName, False, SocketVisible, SocketDefault );
  end;

  procedure ParseLib;
  var
    SocketName: String = '';
  begin
    SocketName:= ParseWord;
    CreateSocketFromType( GlobalActiveNode, nsdLib, 'text', SocketName, False, False );
  end;

  procedure ParseNode;
  var
    name: String;
  begin
    name:= ParseWord( True );

    if ( GlobalLibrary.Nodes.FindByName( name ) > -1 ) then
      Error( 'A node with the name ' + name + ' already exists!' );
    GlobalActiveNode:= TP3DNode.Create( GlobalLibrary.Nodes );
    GlobalMarkdown.Nodes.Add( GlobalActiveNode );
    GlobalActiveNode.Name:= name;
    GlobalActiveOutput:= nil;
  end;

  procedure ParseType;
  var
    name: String;
  begin
    name:= ParseWord( True );

    if ( GlobalLibrary.Types.FindByName( name ) > -1 ) then
      Error( 'A type with the name ' + name + ' already exists!' );
    GlobalActiveNode:= TP3DShaderNodeType.Create( GlobalLibrary.Types );
    GlobalMarkdown.Types.Add( GlobalActiveNode );
    GlobalActiveNode.Name:= name;
    GlobalActiveOutput:= nil;
    CreateSocketFromType( GlobalActiveNode, nsdInput, 'text', 'inject', False, False, '' );
  end;

  procedure NodeCase( Name: String );
  begin
    WriteLn( Name );
    SkipChars();
    case ( Name ) of
      'node': ParseNode;
      'input': ParseInput;
      'type': ParseType;
      'lib': ParseLib;
    else
      Error( 'Unknown node attribute: "' + Name + '"' );
    end;
  end;

  function NodeDebugOutput: String;
  var
    socket: TP3DNodeSocket;
  begin
    if ( not Assigned( GlobalActiveNode )) then
      begin
        Result:= 'Error: Node not Assigned';
        exit;
      end;
    Result:= 'Node ' + GlobalActiveNode.Name + LineEnding;
    for socket in GlobalActiveNode.Inputs do
      Result+= 'input: ' + socket.Name + ' : ' + socket.SocketType + LineEnding;
  end;

begin
  //Inc( GlobalCursor, Length( '---' ));
  if ( Length( IfStack ) > 0 ) then
    Error( 'Missing EndIf(''s): level = ' + IntToStr( Length( IfStack )));
  SkipChars();
  breakat:= PosEx( '---', GlobalFile, GlobalCursor );
  while ( not CompWordAtCursor( '---' )) do
    begin
      NodeCase( ParseWord );
      //ParseNameValuePairs( LineEnding, ':', @NodeCallback, breakat, '{', '}' );
      SkipChars();
    end;
  Inc( GlobalCursor, Length( '---' ));
end;

procedure ParseComment;
begin
  GlobalCursor:= PosEx( LineEnding, GlobalFile, GlobalCursor );
end;

procedure ParseChunk;
var
  EndChunk: Integer;

    procedure AddInline( S: String; Trim: Boolean );
    var
      chunk: TP3DShaderNodeFragmentInline;
    begin
      chunk:= TP3DShaderNodeFragmentInline.Create;
      GlobalActiveFragments.Add( chunk );

      if ( Trim ) then
        if ( RightStr( S, Length( LineEnding )) = LineEnding ) then
          SetLength( S, Length( S ) - Length( LineEnding ));
      //Delete one trailing line-ending if present
      //so the chunk terminator does not have to be in the same line
      //which is unintuive

      chunk.Text:= S;
    end;

begin
  if ( not Assigned( GlobalActiveNode )) then
    Error( 'Cannot define chunk. You have to define a node first!' );
  if ( not Assigned( GlobalActiveOutput )) then
    Error( 'Cannot define chunk. You have to define an output first!' );

  //Inc( GlobalCursor, Length( ChunkDelim ));
  EndChunk:= PosEx( ChunkDelim, GlobalFile, GlobalCursor );
  if ( EndChunk = 0 ) then
    Error( 'Unterminated Chunk' );

  AddInline( CopyBetween( GlobalCursor, EndChunk ), False );

  GlobalCursor:= EndChunk + Length( ChunkDelim );
end;

procedure ParseOutput;
var
  SocketName: String = '';
  SocketType: String = '';
  SocketVisible: Boolean = True;
begin
  if ( not Assigned( GlobalActiveNode )) then
    Error( 'Cannot define output before defining a node first!' );
  if ( Length( IfStack ) > 0 ) then
    Error( 'Missing EndIf(''s): level = ' + IntToStr( Length( IfStack )));
  SocketType:= ParseWord;
  SocketName:= ParseWord;

  SocketVisible:= SocketName[ 1..2 ] = '__';

  GlobalActiveOutput:= CreateSocketFromType( GlobalActiveNode, nsdOutput, SocketType, SocketName, False, SocketVisible );
  GlobalActiveFragments:= GlobalActiveOutput.Fragments;
end;

procedure ParseSelector;
var
  SocketName: String = '';
  OutputOverride: String = '';
  IsSymbol, Stop: Boolean;
  FirstWord: String;
  chunk: TP3DShaderNodeFragmentSelector;
begin
  if ( not Assigned( GlobalActiveNode )) then
    Error( 'Cannot use select before defining a node first!' );
  if ( not Assigned( GlobalActiveFragments )) then
    Error( 'Cannot use select before defining a node''s output first!' );

  SocketName:= ParseWord( Identifier + [ '*' ]);

  SkipChars();
  if ( Ch = '.' ) then begin
    Inc( GlobalCursor );
    OutputOverride:= ParseWord;
    SkipChars();
  end;

  chunk:= TP3DShaderNodeFragmentSelector.Create;
  if ( Ch = ':' ) then begin
    Inc( GlobalCursor );
    FirstWord:= ParseWord();
    SkipChars();
    Stop:= False;
    if ( FirstWord > '' ) then
      repeat
        if ( FirstWord > '' ) then
          chunk.Defines.Add( FirstWord )
        else
          Error( 'Identifier expected' );

        Stop:= not CompWordAtCursor( ',' );
        if ( not Stop ) then begin
          Inc( GlobalCursor );
          FirstWord:= ParseWord();
        end;
      until stop;
  end;
  if ( Pos( '*', SocketName ) = 0 ) then
    if ( GlobalActiveNode.Inputs.FindByName( SocketName ) < 0 ) then
      Error( Format( 'Select with non existing input "%s" found in "%s"', [ SocketName, GlobalActiveNode.Name ]));
  GlobalActiveFragments.Add( chunk );
  chunk.InputName:= SocketName;
  chunk.OutputOverride:= OutputOverride;
end;

{procedure AddInput( S, defs: String );
  function GetNextDef( out ADef: String ): Boolean;
  begin
    ADef:= Trim( Copy2SymbDel( defs, ',' ));
    Result:= ADef > '';
  end;

var
  chunk: TP3DShaderNodeFragmentInput;
  def: String;
begin
  chunk:= TP3DShaderNodeFragmentInput.Create;
  while GetNextDef( def ) do
    chunk.Defines.Add( def );
  TopFragments.Add( chunk );
  chunk.InputName:= S;
end;}

procedure ParseIfDef( Negate: Boolean );
var
  IsString: Boolean;
  IfDefs: String;
  IfDef: TP3DShaderNodeFragmentIfDef;
begin
  IfDefs:= ParseString( IsString );
  if ( not Assigned( GlobalActiveFragments )) then
    Error( 'Cannot define ifdef before defining an output!' );

  IfDef:= TP3DShaderNodeFragmentIfDef.Create;
  IfDef.CheckDefs.Text:= ReplaceStr( IfDefs, ',', LineEnding );
  IfDef.FInvert:= Negate;

  SetLength( IfStack, Length( IfStack ) + 1 );
  IfStack[ high( IfStack )]:= IfDef;
  GlobalActiveFragments.Add( IfDef );
  GlobalActiveFragments:= IfDef.Fragments;
end;

procedure ParseIfConn( Negate: Boolean );
var
  IsString: Boolean;
  IfConnS: String;
  IfConn: TP3DShaderNodeFragmentIfConn;
begin
  IfConnS:= ParseString( IsString );
  if ( not Assigned( GlobalActiveFragments )) then
    Error( 'Cannot define ifconn before defining an output!' );

  IfConn:= TP3DShaderNodeFragmentIfConn.Create;
  IfConn.FInputName:= IfConnS;
  IfConn.FInvert:= Negate;

  SetLength( IfStack, Length( IfStack ) + 1 );
  IfStack[ high( IfStack )]:= IfConn;
  GlobalActiveFragments.Add( IfConn );
  GlobalActiveFragments:= IfConn.Fragments;
end;

procedure ParseEndIf;
var
  l: Integer;
begin
  l:= Length( IfStack ) - 1;
  if ( l < 0 ) then
    Error( 'endif without matching if!' )
  else begin
    SetLength( IfStack, l );
    Dec( l );
    if ( l >= 0 ) then
      GlobalActiveFragments:= IfStack[ l ].Fragments
    else
      GlobalActiveFragments:= GlobalActiveOutput.Fragments;
  end;
end;

procedure Parse;
var
  FirstWord: String;
begin
  GlobalCursor:= 1;
  GlobalActiveNode:= nil;
  SetLength( IfStack, 0 );

  while ( GlobalCursor < EOF ) do
    begin
      if ( Ch in Identifier ) then begin
        FirstWord:= ParseWord();
        case( FirstWord ) of
          'output': ParseOutput;
          'select': ParseSelector;
          'ifdef': ParseIfDef( False );
          'ifndef': ParseIfDef( True );
          'ifconn': ParseIfConn( False );
          'ifnconn': ParseIfConn( True );
          'endif': ParseEndIf;
          else Error( 'Unknown identifier found: "' + FirstWord + '"' );
        end;
      end else begin
        FirstWord:= ParseWord([ ChunkDelim, NodeDelim[ 1 ], StartCommentDelim ]);
        case( FirstWord ) of
          NodeDelim: ParseNode;
          ChunkDelim: ParseChunk;
          StartCommentDelim: ParseComment;
          else Error( 'Unknown symbol found: "' + FirstWord + '"' );
        end;
      end;
      SkipChars();
    end;
end;

{$ENDIF}

