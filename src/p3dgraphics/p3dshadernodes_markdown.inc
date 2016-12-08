//p3dshadernodes.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}

function ParseMarkdown( S: String ): TP3DShaderNodeLibrary;
procedure ParseMarkdownAppend( S: String; Lib: TP3DShaderNodeLibrary );
function ParseMarkdownFile( FN: String ): TP3DShaderNodeLibrary;
procedure ParseMarkdownFileAppend( FN: String; Lib: TP3DShaderNodeLibrary );

{$ENDIF}

{$IFDEF IMPLEMENTATION}

var
  GlobalFile: String;
  GlobalCursor: Integer;
  GlobalActiveNode: TP3DShaderNode;
  GlobalActiveOutput: TP3DShaderNodeSocket;
  GlobalLibrary: TP3DShaderNodeLibrary;

const
  SkippingChars: set of Char = [ ' ', #8  ];
  WordDelims = [#0..' ',',','.',';','/','\',':','''','"','`','='] + Brackets;
  ChunkInpDelim = '$';
  StartCommentDelim = '#';

{$MACRO ON}
{$DEFINE Ch:= GlobalFile[ GlobalCursor ]}
{$DEFINE EOF:= Length( GlobalFile )}
{$DEFINE EOL:= GlobalFile[ GlobalCursor ] in [ #13, #10 ]}

type
  TParserCallBack = procedure ( Name, Value: String; ValueStart: Integer ) is nested;

function GetCaretPos: TPoint;
var
  n, i: Integer;
begin
  i:= 0;
  n:= 1;
  repeat
    Result.y:= n;
    n:= PosEx( LineEnding, GlobalFile, n + 1 );
    Inc( i );
  until ( n = 0 ) or ( n > GlobalCursor );
  Result:= Point( GlobalCursor - Result.y, i );
end;

procedure Error( Msg: String );
var
  LocalCursor: TPoint;
begin
  LocalCursor:= GetCaretPos;
  raise Exception.Create( Format( '[%d, %d, Ch=%s] Error while parsing markdown: "%s"', [ LocalCursor.y, LocalCursor.x, Ch, Msg ]));
end;

procedure SkipChars( const SkipEOL: Boolean = True );
begin
  while (( GlobalFile[ GlobalCursor ] in SkippingChars )
     or ( SkipEOL and ( GlobalFile[ GlobalCursor ] in [ #13,#10 ]))) do
    Inc( GlobalCursor );
end;

function CopyBetween( p1, p2: Integer ): String;
begin
  if (( p1 > 0 ) and ( p2 > p1 )) then
    Result := Copy( GlobalFile, p1, p2 - p1 );
end;

function Between( S1, S2: String ): String;
var
  p1, p2: Integer;
begin
  p1:= PosEx( S1, GlobalFile, GlobalCursor );
  p2:= PosEx( S2, GlobalFile, p1 + 1 );

  if (( p1 > 0 ) and ( p2 > p1 )) then
    Result := Copy( GlobalFile, p1 + Length( S1 ), p2 - p1 - Length( S1 ));
  GlobalCursor:= p2 + Length( S2 );
end;

function FirstWord: String;
var
  tmpCursor: Integer;
begin
  SkipChars( False );
  tmpCursor:= GlobalCursor;
  Result:= ExtractSubstr( GlobalFile, tmpCursor, WordDelims );
  Inc( GlobalCursor, Length( Result ));
end;


function ParseString( const Delim1: String = '"'; const Delim2: String = '"'): String;
begin
  SkipChars;
  if ( Ch = Delim1 ) then
    Result:= Between( Delim1, Delim2 )
  else
    Result:= FirstWord;
end;

function before( S, bef: String ): Boolean;
begin
  Result:= PosEx( S, GlobalFile, GlobalCursor ) < PosEx( bef, GlobalFile, GlobalCursor );
end;


function CompWordAtCursor( Word: String ): Boolean;
var
  Cmp: String;
begin
  Cmp:= Copy( GlobalFile, GlobalCursor, Length( Word ));
  Result:= Cmp = Word;
end;

procedure ParseNameValuePair( Separator, Assignment: String; Callback: TParserCallBack; const SDelim1: String = '"'; const SDelim2: String = '"' );
var
  Name, Value: String;
  ValueStart: Integer;
begin
  Name:= FirstWord;
  SkipChars( False );
  if ( Ch = Separator ) then
    begin
      Inc( GlobalCursor );
      Callback( Name, '', GlobalCursor );
    end
  else if ( Ch = Assignment ) then
    begin
      Inc( GlobalCursor );
      SkipChars( False );
      if ( CompWordAtCursor( SDelim1 )) then
        ValueStart:= GlobalCursor + Length( SDelim1 )
      else
        ValueStart:= GlobalCursor;
      Value:= ParseString( SDelim1, SDelim2 );
      Callback( Name, Value, ValueStart );
      if ( Ch = Separator ) then
        Inc( GlobalCursor, Length( Separator ));
    end
  else
    Error( 'Assignment or Separator expected' );
end;


function CreateSocketFromType( Node: TP3DShaderNode; Direction: TP3DNodeSocketDirection; SocketType: String; Name: String; const SingleOnly: Boolean = False ): TP3DShaderNodeSocket;
begin
  case SocketType of
    'vec4': Result:= TP3DShaderNodeSocketVector.Create( Node, Direction );
    'float': Result:= TP3DShaderNodeSocketFloat.Create( Node, Direction );
    'int': Result:= TP3DShaderNodeSocketInt.Create( Node, Direction );
    'shader': Result:= TP3DShaderNodeSocketShader.Create( Node, Direction );
    'mat4': Result:= TP3DShaderNodeSocketMatrix.Create( Node, Direction );
  else
    Result:= TP3DShaderNodeSocket.Create( GlobalActiveNode, Direction );
  end;
  Result.Name:= Name;
  Result.FSingleOnly:= SingleOnly;
  case Direction of
    nsdInput: Node.Inputs.Add( Result );
    nsdOutput: Node.Outputs.Add( Result );
  end;
end;

procedure ParseNode;
var
  NameSet: Boolean = False;
  SocketName: String = '';
  SocketType: String = '';

  procedure SocketCallback( Name, Value: String; ValueStart: Integer );
  begin
    case Name of
      'name':
        SocketName:= Value;
      'type':
        SocketType:= Value;
      else
        Error( 'Unknown socket attribute' );
    end;
  end;

  procedure NodeCallback( Name, Value: String; ValueStart: Integer );
  var
    EOF_Value: Integer;
  begin
    if ( Name = 'node' ) then
      NameSet:= True;
    if ( not NameSet ) then
      Error( 'Name has to be the first node''s attribute' );
    case Name of
      'node':
        begin
          if ( GlobalLibrary.Nodes.Find( Value ) > -1 ) then
            Error( 'A node with the name ' + Value + ' already exists!' );
          GlobalActiveNode:= TP3DShaderNode.Create;
          GlobalActiveNode.Name:= Value;
          GlobalActiveOutput:= nil;
          GlobalLibrary.Nodes.Add( GlobalActiveNode );
        end;
      'input':
        begin
          EOF_Value:= GlobalCursor;
          GlobalCursor:= ValueStart;
          while ( GlobalCursor < EOF_Value - 1 ) do
            ParseNameValuePair( ',', '=', @SocketCallback );
          GlobalCursor:= EOF_Value;
          CreateSocketFromType( GlobalActiveNode, nsdInput, SocketType, SocketName );
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
      while ( not CompWordAtCursor( '---' )) do
        begin
          ParseNameValuePair( LineEnding, ':', @NodeCallback, '{', '}' );
          SkipChars();
        end;
    end;
  Inc( GlobalCursor, Length( '---' ));
end;

procedure ParseComment;
begin
  GlobalCursor:= PosEx( LineEnding, GlobalFile, GlobalCursor );
end;

function StrToBool( const S: string ): Boolean;
begin
  if ( S = 'yes' ) then
    Result:= True
  else if ( S = 'no' ) then
    Result:= False
  else
    Raise EConvertError.CreateFmt( '"%s" is not a valid markdown boolean. It has to be either yes or no!', [ S ] );
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
    ChunkSingle: Boolean = False;

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
          ChunkSingle:= StrToBool( Value );
        else
          Error( 'Unknown node attribute' );
      end;
    end;

  var
    idx: Integer;

    procedure CreateRestrictions;
    var
      i, LocalCursor: Integer;
      ifdef: TP3DShaderNodeFragmentIfDef;
    begin
      LocalCursor:= 1;
      for i:= 1 to WordCount( ChunkRestrict, [ ',' ]) do
        begin
          ifdef:= TP3DShaderNodeFragmentIfDef.Create;
          ifdef.Caption:= Trim( ExtractSubstr( ChunkRestrict, LocalCursor, [ ',' ]));
          TopFragments.Add( ifdef );
          TopFragments:= ifdef.Fragments;
        end;
    end;

  begin
    SkipChars( False );
    if (( Ch = '{' ) and before( '}', LineEnding )) then
      begin
        Inc( GlobalCursor );
        while ( not CompWordAtCursor( '}' )) do
          ParseNameValuePair( ',', '=', @Callback );

          if ( ChunkName = '' ) then
            begin
              if ( GlobalActiveOutput = nil ) then
                Error( 'First chunk in node has to have a name!' );
            end
          else
            begin
              idx:= GlobalActiveNode.Outputs.FindSocketByName( ChunkName );
              if ( idx >= 0 ) then
                GlobalActiveOutput:= TP3DShaderNodeSocket( GlobalActiveNode.Outputs[ idx ])
              else
                GlobalActiveOutput:= CreateSocketFromType( GlobalActiveNode, nsdOutput, ChunkType, ChunkName, ChunkSingle );
            end;

        TopFragments:= GlobalActiveOutput.Fragments;
        CreateRestrictions;
      end;
    GlobalCursor:= PosEx( LineEnding, GlobalFile, GlobalCursor ) + 1;
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

    procedure AddInline( S: String );
    var
      chunk: TP3DShaderNodeFragmentInline;
    begin
      chunk:= TP3DShaderNodeFragmentInline.Create;
      TopFragments.Add( chunk );

      if ( Copy( S, Length( S ) - Length( LineEnding ) - 1, Length( LineEnding )) = LineEnding ) then
        Delete( S, Length( S ) - Length( LineEnding ) - 1, Length( LineEnding ));
      //Delete one trailing line-ending if present
      //so the chunk terminator does not have to be in the same line
      //which is unintuive

      chunk.Text:= S;
    end;

    procedure AddInput( S: String );
    var
      chunk: TP3DShaderNodeFragmentInput;
    begin
      chunk:= TP3DShaderNodeFragmentInput.Create;
      TopFragments.Add( chunk );
      chunk.InputName:= S;
    end;

  var
    p2, p3, inp: Integer;
    name: String;
  begin
    p2:= GlobalCursor;
    while (( p2 > 0 ) and ( p2 < EndChunk )) do
      begin
        p2:= PosEx( ChunkInpDelim, GlobalFile, p2 + Length( ChunkInpDelim ));
        if (( p2 = 0 ) or ( p2 > EndChunk )) then
          AddInline( CopyBetween( GlobalCursor, EndChunk ))
        else
          begin
            p3:= PosEx( ChunkInpDelim, GlobalFile, p2 + Length( ChunkInpDelim ));
            if ( p3 < EndChunk ) then
              begin
                name:= CopyBetween( p2 + Length( ChunkInpDelim ), p3 );
                inp:= GlobalActiveNode.Inputs.FindSocketByName( name );
                if ( inp >= 0 ) then
                  begin
                    AddInline( CopyBetween( GlobalCursor, p2 ));
                    AddInput( name );
                    p2:= p3 + Length( ChunkInpDelim );
                    GlobalCursor:= p2;
                  end
                else
                  p2:= p3 + Length( ChunkInpDelim );
              end;
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

function ParseMarkdown( S: String ): TP3DShaderNodeLibrary;
begin
  GlobalFile:= S;

  GlobalLibrary:= TP3DShaderNodeLibrary.Create;
  Result:= GlobalLibrary;

  Parse;
end;

procedure ParseMarkdownAppend(S: String; Lib: TP3DShaderNodeLibrary);
begin
  GlobalFile:= S;

  GlobalLibrary:= Lib;

  Parse;
end;

function ParseMarkdownFile( FN: String ): TP3DShaderNodeLibrary;
var
  F: TStringList;
  FileName: String;
begin
  FileName:= P3DSearchPaths.Shaders.FindFileEx( FN, GetCurrentDir());
  if ( FileName = '' ) then
    raise Exception.Create( Format( 'Error while parsing markdown file. The file "%f" could not be found', [ FN ]));
  F:= TStringlist.Create;
  F.LoadFromFile( FileName );
  Result:= ParseMarkdown( F.Text );
  F.Free;
end;

procedure ParseMarkdownFileAppend(FN: String; Lib: TP3DShaderNodeLibrary);
var
  F: TStringList;
  FileName: String;
begin
  FileName:= P3DSearchPaths.Shaders.FindFileEx( FN, GetCurrentDir());
  if ( FileName = '' ) then
    raise Exception.Create( Format( 'Error while parsing markdown file. The file "%f" could not be found', [ FN ]));
  F:= TStringlist.Create;
  F.LoadFromFile( FileName );
  ParseMarkdownAppend( F.Text, Lib );
  F.Free;
end;

{$ENDIF}
