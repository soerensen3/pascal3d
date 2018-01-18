//P3D Markdown Parser
//PSEUDOCODE

unit p3dMarkdown;

interface

implementation

var 
  GlobalCursor: Integer;
  GlobalActiveNode: TNode;
  GlobalActiveOutput: TNodeOutput;

function ParseString: Integer;
begin
  SkipChars;
  if ( '"' ) then
    begin
      MidStr( '"', '"' );
      Result:= GlobalCursor + Len;
    end
  else
    begin
      FirstWord;
      Result:= GlobalCursor + Len;
    end;
end;

procedure ParseNameValuePair( Separator, Assignment, Callback );
begin
  LocalCursor:= GlobalCursor;
  Name:= FirstWord;
  Inc( LocalCursor );
  if ( ',' ) then
    Inc( GlobalCursor ); 
    Callback( Name, '' );
    continue;
  if ( '=' ) then
    Value:= ParseString;
    Callback( Name, Value );
    Inc( LocalCursor, ...);
  else 
    error = 'Assignment or Separator expected';
end;

procedure ParseNode;
var
  LocalSocket: TSocket;
  NameSet: Boolean = False;

  procedure SocketCallback( Name, Value ); 
  begin
    case Name of
      'name':
        LocalSocket.Name:= Value;
      'type:
        LocalSocket.Type:= Value;
      else
        error = 'Unknown socket attribute';
    end;
  end;

  procedure NodeCallback( Name, Value );
  begin
    if Name = Title then
      NameSet:= True;
    if ( not NameSet ) then
      error = 'Name has to be the first node''s attribute'
    case Name of
      'name': 
        GlobalActiveNode:= TNode.Create;
        GlobalActiveNode.Name:= Value;
        GlobalActiveOutput:= nil;
      'input':
        LocalSocket:= GlobalActiveNode.AddInput;
        while not EOF ( Value ) do
          ParseNameValuePair( ',', '=', SocketCallback );
      else
        error = 'Unknown node attribute';
    end;
  end;
begin
  SkipChars( not EOL );
  if ( not EOL ) then
    error = 'invalid argument to unary operator'
  if '{', '}' before EOL then
    OptionsS:= MidStr( '{', '}' );
    while not EOF (OptionsS) do
      ParseNameValuePair( ',', EOL, NodeCallback );
end;

procedure ParseChunkOptions
var
  FirstAttrib: Boolean = True;
  procedure Callback( Name, Value );
  begin
    if Name not 'name' and not GlobalActiveOutput then
      error = 'First chunk in node has to have a name as first attribute';
    case Name of
      'name':
        if ( not FirstAttrib ) then
          error = 'Name has to be the first attribute';
        GlobalActiveOutput:= TNode.Create;
        GlobalActiveOutput.Name:= Value;
      'type:
        GlobalActiveOutput.Type:= Value;
      'restrict':
        CheckActiveNode or Error;
        GlobalActiveOutput.ClearRestrict;
        for word in Words( ',' ) do
          GlobalActiveOutput.Restrict( Trim( word ));
        while not EOF ( Value ) do
          ParseNameValuePair( ',', '=', SocketCallback );
      else
        error = 'Unknown node attribute';
    end;
    FirstAttrib:= False;
  end;

begin
  if '{', '}' before EOL then
    OptionsS:= MidStr( '{', '}' );
    while not EOF (OptionsS) do
      ParseNameValuePair( ',', '=', Callback );
end;

procedure ParseChunk
begin
  end = find( '```' );
  if end = 0 then
    error = 'Unterminated Chunk';
  ParseChunkOptions;
  Buffer:= MidStr( start, end );
  ReplaceInputs;
end;

procedure Parse
begin
  GlobalActiveNode:= nil;
  while not EOF do
    SkipChars
    if ( '---' ) then
      ParseNode
    if ( '```' ) then
      ParseChunk
end;


end.
