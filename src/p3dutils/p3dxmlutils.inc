//p3dfileutil.inc
//part of p3dutils.pas

{$IFDEF INTERFACE}

type TP3DXMLPath = string;

function P3DXMLFindPath( ABase: TDOMElement; APath: TP3DXMLPath ): TDOMElement;
function P3DXMLExtractRelativePath( APath: TP3DXMLPath; out FileName: String ): TP3DXMLPath;
procedure P3DXMLWrite( Element: TDOMNode; const AFileName: String );
function P3DHTMLEncode( const Data: String ): String; //http://stackoverflow.com/questions/2968082/is-there-a-delphi-standard-function-for-escaping-html
function P3DHTMLDecode( const Data: String ): String;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

function P3DHTMLEncode( const Data: String ): String; //http://stackoverflow.com/questions/2968082/is-there-a-delphi-standard-function-for-escaping-html
var
  iPos, i: Integer;

  procedure Encode(const AStr: String);
  begin
    Move(AStr[1], result[iPos], Length(AStr) * SizeOf(Char));
    Inc(iPos, Length(AStr));
  end;

begin
  SetLength(result, Length(Data) * 6);
  iPos := 1;
  for i := 1 to length(Data) do
    case Data[i] of
      '<': Encode('&lt;');
      '>': Encode('&gt;');
      '&': Encode('&amp;');
      '"': Encode('&quot;');
    else
      result[iPos] := Data[i];
      Inc(iPos);
    end;
  SetLength(result, iPos - 1);
end;

function CopyBetween( S: String; p1, p2: Integer ): String;
begin
  if (( p1 > 0 ) and ( p2 > p1 )) then
    Result := Copy( S, p1, p2 - p1 );
end;

function P3DHTMLDecode( const Data: String ): String;
var
  iPos, i: Integer;
  Wd: String;

begin
  iPos := 1;
  for i := 1 to Length( Data ) do
    if ( Data[ i ] = '&' ) then
      begin
        iPos:= PosEx( ';', Data, i );
        if ( iPos > 0 ) then
          begin
            Wd:= CopyBetween( Data, i, iPos );
            case Wd of
              '&lt;': Result+= '<';
              '&gt;': Result+= '>';
              '&amp;': Result+= '&';
              '&quot;': Result+= '"';
            end;
          end
        else
          Result+= Data[ i ];
      end
    else
      Result+= Data[ i ];
end;


function P3DXMLFindPath( ABase: TDOMElement; APath: TP3DXMLPath ): TDOMElement;
var
  Child: TDOMElement;
  RestPath, CurPath: String;
  ChildName: WideString;
begin
  Result:= nil;
  Child:= TDOMElement( ABase.FirstChild );

  RestPath:= APath;
  CurPath:= Copy2SymbDel( RestPath, '/' );
  if ( CurPath = '' ) then //paths usually have the initial "/"
    CurPath:= Copy2SymbDel( RestPath, '/' );
  if ( CurPath = '' ) then //still empty? exit!
    exit;

  while Assigned( Child ) do
    begin
      ChildName:= TDOMElement( Child ).AttribStrings[ 'Name' ];
      if ( ChildName = '' ) then
        ChildName:= TDOMElement( Child ).AttribStrings[ 'name' ]; //temporary

      if ( ChildName = CurPath ) then
        begin
          if ( RestPath = '' ) then
            Result:= Child
          else
            Result:= P3DXMLFindPath( Child, RestPath );
          break;
        end;
      Child:= TDOMElement( Child.NextSibling );
    end;
end;

function P3DXMLExtractRelativePath( APath: TP3DXMLPath; out FileName: String ): TP3DXMLPath;
begin
  Result:= APath;
  FileName:= Copy2SymbDel( Result, '$' );
  if ( Result = '' ) then
    begin
      Result:= FileName;
      FileName:= '';
    end;
end;

procedure P3DXMLWrite( Element: TDOMNode; const AFileName: String );
var
  F: TextFile;
  indent: Integer = 0;

  procedure WriteNode( Element: TDOMNode );
  var
    i: Integer;
    indentStr: String;
  begin
    if ( not Assigned( Element )) then
      exit;
    indentStr:= StringOfChar( ' ', indent * 2 );
    if ( Element.NodeName = '#text' ) then
      Write( F, Element.TextContent )
    else if ( Element.NodeName = '#comment' ) then
      WriteLn( F, indentStr + '<!--', StringReplace( Element.TextContent, LineEnding, LineEnding + indentStr, [ rfReplaceAll ]), '-->' )
    else
      begin
        Write( F, indentStr + '<' + Element.NodeName );
        if ( Assigned( Element.Attributes )) then
          for i:= 0 to Element.Attributes.Length - 1 do
            begin
//              if ( i = 0 ) then
//                WriteLn( F );
              Write( F, LineEnding + indentStr +
                     Format( '   %s = "%s"', [ Element.Attributes[ i ].NodeName, P3DHTMLEncode( Element.Attributes[ i ].NodeValue )]));
            end;

        if ( Element.ChildNodes.Count > 0 ) then
          begin
            WriteLn( F, '>' );
            Inc( indent, 1 );
            for i:= 0 to Element.ChildNodes.Count - 1 do
              WriteNode( Element.ChildNodes[ i ]);
            Dec( indent, 1 );
            WriteLn( F, indentStr + '</' + Element.NodeName + '>' );
          end
        else
          WriteLn( F, ' />' );
      end;
  end;

begin
  Assign( F, AFileName );
  Rewrite( F );
  WriteLn( F, '<?xml version="1.0" encoding="utf-8"?>' );
  if ( Assigned( Element )) then
    WriteNode( Element );
  CloseFile( F );
end;

{$ENDIF}
