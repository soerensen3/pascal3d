{ p3d.core.shadernodes_markdown_parser.inc }

{$IFDEF INTERFACE}

{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure ParseNode;
var
  NameSet: Boolean = False;
  SocketName: String = '';
  SocketType: String = '';
  SocketDefault: String = '';
  SocketVisible: Boolean = True;
  breakat: SizeInt;

  procedure SocketCallback( Name, Value: String; ValueStart: Integer );
  begin
    case Name of
      'name':
        SocketName:= Value;
      'type':
        SocketType:= Value;
      'default':
        SocketDefault:= Value;
      'visible':
        SocketVisible:= StrToBool( Value );
      else
        Error( 'Unknown socket attribute: "' + Name + '"' );
    end;
  end;

  procedure NodeCallback( Name, Value: String; ValueStart: Integer );
  var
    EOF_Value: Integer;
  begin
    if (( Name = 'node' ) or ( Name = 'type' )) then
      NameSet:= True;
    if ( not NameSet ) then
      Error( 'Either node or type has to be the first node''s attribute' );
    case Name of
      'type':
        begin
          if ( GlobalLibrary.Types.FindByName( Value ) > -1 ) then
            Error( 'A type with the name ' + Value + ' already exists!' );
          GlobalActiveNode:= TP3DShaderNodeType.Create( GlobalLibrary.Types );
          GlobalMarkdown.Types.Add( GlobalActiveNode );
          GlobalActiveNode.Name:= Value;
          GlobalActiveOutput:= nil;
          CreateSocketFromType( GlobalActiveNode, nsdInput, 'text', 'inject', False, False, '' );
        end;
      'node':
        begin
          if ( GlobalLibrary.Nodes.FindByName( Value ) > -1 ) then
            Error( 'A node with the name ' + Value + ' already exists!' );
          GlobalActiveNode:= TP3DNode.Create( GlobalLibrary.Nodes );
          GlobalMarkdown.Nodes.Add( GlobalActiveNode );
          GlobalActiveNode.Name:= Value;
          GlobalActiveOutput:= nil;
        end;
      'input':
        begin
          if ( GlobalActiveNode is TP3DShaderNodeType ) then
            Error( 'Cannot define inputs for types!' );
          EOF_Value:= GlobalCursor;
          GlobalCursor:= ValueStart;
          //while ( GlobalCursor < EOF_Value - 1 ) do
          ParseNameValuePairs( ',', ':', @SocketCallback, EOF_Value - 1 );
          GlobalCursor:= EOF_Value;
          CreateSocketFromType( GlobalActiveNode, nsdInput, SocketType, SocketName, False, SocketVisible, SocketDefault );
          SocketName:= '';
          SocketType:= '';
          SocketDefault:= '';
          SocketVisible:= True;
        end
      else
        Error( 'Unknown node attribute' );
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
  Inc( GlobalCursor, Length( '---' ));
  SkipChars( False );
  if ( not ( EOL )) then
    Error( 'invalid argument to unary operator' )
  else
    begin
      SkipChars();
      breakat:= PosEx( '---', GlobalFile, GlobalCursor );
      //while ( not CompWordAtCursor( '---' )) do
        begin
          ParseNameValuePairs( LineEnding, ':', @NodeCallback, breakat, '{', '}' );
          //SkipChars();
        end;
    end;
  Inc( GlobalCursor, Length( '---' ));
end;

procedure ParseComment;
begin
  GlobalCursor:= PosEx( LineEnding, GlobalFile, GlobalCursor );
end;

procedure ParseChunk;
var
  TopFragments: TP3DShaderNodeFragmentList = nil;
  EndChunk: Integer;

  procedure ParseChunkOptions;
  var
    ChunkName: String = '';
    ChunkType: String = '';
    ChunkRestrict: String = '';
    ChunkVisible: Boolean = True;
    ChunkSingleOnly: Boolean = False;

    procedure Callback( Name, Value: String; ValueStart: Integer );
    begin
      case Name of
        'name':
          ChunkName:= Value;
        'type':
          ChunkType:= Value;
        'restrict':
          ChunkRestrict:= Value;
        'single':
          ChunkSingleOnly:= StrToBool( Value );
        'visible':
          ChunkVisible:= StrToBool( Value );
        else
          Error( 'Unknown node attribute' );
      end;
    end;

  var
    idx: Integer;
    breakat: SizeInt;

    procedure CreateSingleOnly;
    var
      single: TP3DShaderNodeFragmentSingleOnly;
    begin
      single:= TP3DShaderNodeFragmentSingleOnly.Create;
      TopFragments.Add( single );
      TopFragments:= single.Fragments;
    end;

    procedure CreateRestrictions;
    var
      i, LocalCursor: Integer;
      ifdef: TP3DShaderNodeFragmentIfDef;
    begin
      LocalCursor:= 1;
      if ( Trim( ChunkRestrict ) > '' ) then
        begin
          ifdef:= TP3DShaderNodeFragmentIfDef.Create;
          ifdef.CheckDefs.CommaText:= ChunkRestrict;
          TopFragments.Add( ifdef );
          TopFragments:= ifdef.Fragments;
        end;
      {for i:= 1 to WordCount( ChunkRestrict, [ ',' ]) do
        begin
          ifdef:= TP3DShaderNodeFragmentIfDef.Create;
          ifdef.Caption:= Trim( ExtractSubstr( ChunkRestrict, LocalCursor, [ ',' ]));
          TopFragments.Add( ifdef );
          TopFragments:= ifdef.Fragments;
        end;}
    end;

  begin
    SkipChars( False );
    if (( Ch = '{' ) and before( '}', LineEnding )) then
      begin
        Inc( GlobalCursor );
        breakat:= PosEx( '}', GlobalFile, GlobalCursor );
        //while ( not CompWordAtCursor( '}' )) do
        ParseNameValuePairs( ',', ':', @Callback, breakat );

        if ( ChunkName = '' ) then
          begin
            if ( GlobalActiveOutput = nil ) then
              Error( GlobalActiveNode.Name + ': First chunk in node has to have a name!' );
          end
        else
          begin
            idx:= GlobalActiveNode.Outputs.FindByName( ChunkName );
            if ( idx >= 0 ) then
              GlobalActiveOutput:= TP3DShaderNodeSocket( GlobalActiveNode.Outputs[ idx ])
            else
              GlobalActiveOutput:= CreateSocketFromType( GlobalActiveNode, nsdOutput, ChunkType, ChunkName, ChunkSingleOnly, ChunkVisible );
          end;

        TopFragments:= GlobalActiveOutput.Fragments;
        if ( ChunkSingleOnly ) then
          CreateSingleOnly;
        CreateRestrictions;
      end;
    //GlobalCursor:= PosEx( LineEnding, GlobalFile, GlobalCursor ) + 1;
    Inc( GlobalCursor );
    SkipChars( False );
    if ( CompWordAtCursor( LineEnding )) then
      Inc( GlobalCursor, Length( LineEnding ));
  end;

  function ChunkDebugOutput: String;
  var
    socket: TP3DNodeSocket;
  begin
    if ( not Assigned( GlobalActiveNode )) then
      begin
        Result:= 'Error: Node not Assigned';
        exit;
      end;
    Result:= '';
    for socket in GlobalActiveNode.Outputs do
      Result+= 'output: ' + socket.Name + ' : ' + socket.SocketType + LineEnding;
  end;

  procedure ProcessChunkOutput;

    procedure AddInline( S: String; Trim: Boolean );
    var
      chunk: TP3DShaderNodeFragmentInline;
    begin
      chunk:= TP3DShaderNodeFragmentInline.Create;
      TopFragments.Add( chunk );

      if ( Trim ) then
        if ( RightStr( S, Length( LineEnding )) = LineEnding ) then
          SetLength( S, Length( S ) - Length( LineEnding ));
      //Delete one trailing line-ending if present
      //so the chunk terminator does not have to be in the same line
      //which is unintuive

      chunk.Text:= S;
    end;

    procedure AddInput( S, defs: String );
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
    end;

    procedure GetInputNameDef( S: String; out AName, ADefs: String );
    begin
      AName:= Trim( Copy2SymbDel( S, ':' ));
      ADefs:= Trim( S );
    end;

  var
    p2, p3, inp: Integer;
    name, defs: String;
  begin
    p2:= GlobalCursor;
    while (( p2 > 0 ) and ( p2 < EndChunk )) do
      begin
        p2:= PosEx( ChunkInpDelim, GlobalFile, p2 );
        if (( p2 = 0 ) or ( p2 > EndChunk )) then
          AddInline( CopyBetween( GlobalCursor, EndChunk ), True )
        else
          begin
            p3:= PosEx( ChunkInpDelim, GlobalFile, p2 + Length( ChunkInpDelim ));
            if ( p3 < EndChunk ) then
              begin
                GetInputNameDef( CopyBetween( p2 + Length( ChunkInpDelim ), p3 ), name, defs );

                inp:= GlobalActiveNode.Inputs.FindByName( name );
                if (( inp >= 0 ) or ( ExtractWord( 1, name, SkippingChars ) = 'all' )) then
                  begin
                    if ( p2 > GlobalCursor ) then
                      AddInline( CopyBetween( GlobalCursor, p2 ), False );
                    AddInput( name, defs );
                    p2:= p3 + Length( ChunkInpDelim );
                    GlobalCursor:= p2;
                  end
                else
                  Error( Format( 'Cannot find input of name "%s" in node "%s".', [ name, GlobalActiveNode.Name ]));
                  //p2:= p3 + Length( ChunkInpDelim );
              end
            else
              Error( 'Unterminated input!' );
          end;
      end;
  end;

begin
  if ( not Assigned( GlobalActiveNode )) then
    Error( 'Cannot define chunk. You have to define a node first!' );
  Inc( GlobalCursor, Length( '```' ));
  EndChunk:= PosEx( '```', GlobalFile, GlobalCursor );
  if ( EndChunk = 0 ) then
    Error( 'Unterminated Chunk' );
  ParseChunkOptions;
  ProcessChunkOutput;

  //TODO: ReplaceInputs;
  GlobalCursor:= EndChunk + Length( '```' );
end;

procedure Parse;
begin
  GlobalCursor:= 1;
  GlobalActiveNode:= nil;

  while ( GlobalCursor < EOF ) do
    begin
      SkipChars();
      if ( GlobalCursor >= EOF ) then
        break;
      if ( CompWordAtCursor( '---' )) then
        ParseNode
      else if ( CompWordAtCursor( '```' )) then
        ParseChunk
      else if ( CompWordAtCursor( StartCommentDelim )) then
        ParseComment
      else
        Error( 'Syntax Error! Only ---, ``` or # allowed outside of chunks and nodes' );
    end;
end;

{$ENDIF}

