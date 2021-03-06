{ p3d.core.shadernodes_markdown_utils.inc }

{$IFDEF INTERFACE}
type
  TParserCallBack = procedure ( Name, Value: String; ValueStart: Integer ) is nested;

//procedure ParseNameValuePair( Separator, Assignment: String; Callback: TParserCallBack; const SDelim1: String = '"'; const SDelim2: String = '"' );
function ParseWord( AllowedDelimiters: TSysCharSet; const RaiseException: Boolean = False ): String;
function ParseWord( const RaiseException: Boolean = False ): String;
function ParseSymbol( const RaiseException: Boolean = False ): String;
function CompWordAtCursor( Word: String ): Boolean;
function ParseString( out IsString: Boolean; const Delim1: String = '"'; const Delim2: String = '"' ): String;
procedure SkipChars( const SkipEOL: Boolean = True );

var
  GlobalFile: String;
  GlobalMarkdown: TP3DMarkdownFile;
  GlobalFileName: String;
  GlobalCursor: Integer;
  GlobalActiveNode: TP3DNode = nil;
  GlobalActiveOutput: TP3DShaderNodeSocket = nil;
  GlobalActiveFragments: TP3DShaderNodeFragmentList = nil;
  GlobalLibrary: TP3DShaderNodeLibrary;
  IfStack: array of TP3DShaderNodeFragmentWithFragments;

{$ENDIF}

{$IFDEF IMPLEMENTATION}


const
  SkippingChars: set of Char = [ ' ', #9  ];
  WordDelims = [#0..' ',',','.',';','/','\',':','''','"','`','=', '+', '-', '*', '#' ] + Brackets;
  Identifier = ['a'..'z','A'..'Z','1'..'9','0', '_' ];
  ChunkDelim = '''';
  NodeDelim = '---';
  StartCommentDelim = '#';

{$MACRO ON}
{$DEFINE Ch:= GlobalFile[ GlobalCursor ]}
{$DEFINE EOF:= Length( GlobalFile )}
{$DEFINE EOL:= GlobalFile[ GlobalCursor ] in [ #13, #10 ]}


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
  raise Exception.Create( Format( '%s[%d, %d, Ch=%s] Error while parsing markdown: "%s"' + LineEnding + '%s...', [ IfThen( GlobalMarkdown.FilePointer.FileName > '', GlobalMarkdown.FilePointer.FileName + ': ' ), LocalCursor.y, LocalCursor.x, Ch, Msg, Copy( GlobalFile, GlobalCursor, 10 )]));
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
  if ( p2 < p1 ) then
    Error( Format( 'Matching %s was not found!', [ S2 ]));

  if (( p1 > 0 ) and ( p2 > p1 )) then
    Result := Copy( GlobalFile, p1 + Length( S1 ), p2 - p1 - Length( S1 ));
  GlobalCursor:= p2 + Length( S2 );
end;

function ParseString( out IsString: Boolean; const Delim1: String = '"'; const Delim2: String = '"' ): String;
begin
  SkipChars;
  IsString:= Ch = Delim1;
  if ( IsString ) then
    Result:= Between( Delim1, Delim2 )
  else
    Result:= ParseWord;
end;

function before( S, bef: String ): Boolean;
begin
  Result:= PosEx( S, GlobalFile, GlobalCursor ) < PosEx( bef, GlobalFile, GlobalCursor );
end;

{function DelimiterToStr( Delimiters: TSysCharSet ): String;
var
  i: AnsiChar;
begin
  Result:= '[';
  for i:= 0 to high( Delimiters ) do begin
    if ( i > 0 ) then
      Result:= Result + ', ';
    Result:= Result + Delimiters[ i ];
  end;
  Result:= Result + ']';
end;}

function ParseWord( AllowedDelimiters: TSysCharSet; const RaiseException: Boolean = False ): String;
begin
  SkipChars();
  Result:= '';
  while( Ch in AllowedDelimiters ) do begin
    Result:= Result + Ch;
    Inc( GlobalCursor )
  end;
  if ( RaiseException and ( Result = '' )) then
    Error( {DelimiterToStr( AllowedDelimiters ) + }'Identifier expected!' );
end;

function ParseWord( const RaiseException: Boolean = False ): String;
var
  tmpCursor: Integer;
begin
  SkipChars();
  tmpCursor:= GlobalCursor;
  Result:= ExtractSubstr( GlobalFile, tmpCursor, WordDelims );
  Inc( GlobalCursor, Length( Result ));
  if ( RaiseException and ( Result = '' )) then
    Error( 'Identifier expected!' );
end;

function ParseSymbol( const RaiseException: Boolean = False ): String;
begin
  SkipChars( False );
  Result:= '';
  while( Ch in WordDelims - SkippingChars - [ #10, #13 ]) do begin
    Result:= Result + Ch;
    Inc( GlobalCursor )
  end;
  if ( RaiseException and ( Result = '' )) then
    Error( 'Symbol expected!' );
end;

function ParseSymbolOrWord( out IsSymbol: Boolean ): String;
begin
  SkipChars();
  IsSymbol:= Ch in WordDelims;
  if ( IsSymbol ) then
    Result:= ParseSymbol
  else
    Result:= ParseWord;
end;

function CompWordAtCursor( Word: String ): Boolean;
var
  Cmp: String;
begin
  Cmp:= Copy( GlobalFile, GlobalCursor, Length( Word ));
  Result:= Cmp = Word;
end;



procedure ParseNameValuePairs( Separator, Assignment: String; Callback: TParserCallBack; breakat: Integer; const SDelim1: String = '"'; const SDelim2: String = '"' );
var
  Name, Value: String;
  ValueStart: Integer;
  IsString: Boolean;
begin
  while GlobalCursor < breakat do
    begin
      Name:= ParseWord;
      SkipChars( False );
      if ( Name = '' ) then
        Error( Format( GlobalActiveNode.Name + ': Name expected but "%s" found.', [ Ch ]));
      if ( ch = Separator ) then // Skip Separator
        begin
          Inc( GlobalCursor );
          Callback( Name, '', GlobalCursor );
          Continue;
        end
      else if ( Ch = Assignment ) then
        begin
          Inc( GlobalCursor );
          SkipChars( False );
          if ( CompWordAtCursor( SDelim1 )) then
            ValueStart:= GlobalCursor + Length( SDelim1 )
          else
            ValueStart:= GlobalCursor;
          Value:= ParseString( IsString, SDelim1, SDelim2 );
          Callback( Name, Value, ValueStart );
          SkipChars( False );
          if ( GlobalCursor >= breakat ) then
            break;
          if ( Ch = Separator ) then
            Inc( GlobalCursor, Length( Separator ))
          else
            Error( Format( GlobalActiveNode.Name + ': "%s" expected', [ Separator ]));
          Continue;
        end
      else
        Error( Format( GlobalActiveNode.Name + ': "%s" or "%s" expected', [ Assignment, Separator ]));
    end;
end;

{procedure ParseNameValuePair( Separator, Assignment: String; Callback: TParserCallBack; const SDelim1: String = '"'; const SDelim2: String = '"' );
var
  Name, Value: String;
  ValueStart: Integer;
  IsString: Boolean;
begin
  Name:= FirstWord;
  SkipChars( False );
  if ( Ch = Separator ) then // Skip Separator
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
      Value:= ParseString( IsString, SDelim1, SDelim2 );
      if ( Value = '' ) then
        FirstWord;
      Callback( Name, Value, ValueStart );
      SkipChars( False );
      if ( Ch = Separator ) then
        Inc( GlobalCursor, Length( Separator ));
    end
  else
    begin
      //WriteLn( Copy( GlobalFile, GlobalCursor, 10 ));
      Error( GlobalActiveNode.Name + ': Assignment or Separator expected' );
    end;
end;}

function FindCntByDir( Node: TP3DNode; Direction: TP3DNodeSocketDirection ): IP3DStreamableContainer;
begin
  case Direction of
    nsdInput: Result:= Node.Inputs;
    nsdOutput: Result:= Node.Outputs;
    nsdLib: Result:= Node.Libs;
  end;
end;

function CreateSocketFromType( Node: TP3DNode; Direction: TP3DNodeSocketDirection; SocketType: String; Name: String; const SingleOnly: Boolean = False; const Visible: Boolean = True; const SocketDefault: String = '' ): TP3DShaderNodeSocket;
var
  Cnt: TP3DNodeSocketContainerList;
begin
  Cnt:= FindCntByDir( Node, Direction ).GetInstance as TP3DNodeSocketContainerList;
  if ( Cnt.FindByName( Name ) > -1 ) then
    Error( 'A socket with that name already exists!' );
  Result:= TP3DShaderNodeSocket.Create( Cnt );
  Result.Name:= Name;
  Result.FSocketType:= SocketType;
  Result.Visible:= Visible;
  Result.Default:= SocketDefault;
end;

{function CreateSocketLibFromType( Node: TP3DNode; LibName: String ): TP3DShaderNodeSocketCloneLib;
var
  Cnt: TP3DNodeSocketContainerList;
  Name: String;
begin
  Cnt:= Node.Libs;
  Name:= Cnt.FindUniqueName( 'Lib' );
  Result:= TP3DShaderNodeSocketCloneLib.Create( Cnt );
  Result.Name:= Name;
  Result.FLibName:= LibName;
  Result.Visible:= False;
end;}

function StrToBool( const S: string ): Boolean;
begin
  if ( S = 'true' ) then
    Result:= True
  else if ( S = 'false' ) then
    Result:= False
  else
    Raise EConvertError.CreateFmt( '"%s" is not a valid markdown boolean. It has to be either true or false!', [ S ] );
end;




{$ENDIF}

