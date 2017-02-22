//p3dwindow.inc
//part of p3devents.pas

{$IFDEF INTERFACE}

type
  { TP3DLogger }

  TP3DLogger = class ( TP3DInterfacedPersistent )
    private
      FFileName: String;
      FLevel: TDOMElement;
      FLogFile: TXMLDocument;

      procedure SetFileName( AValue: String );
      function CreateMessageHeader( Sender: TObject ): TDOMElement;

    public
      constructor Create;
      destructor Destroy; override;

      procedure WriteFile;
      procedure AddChild( ADOM: TDOMElement );
      function LogInfoXML( Sender: TObject; Message: String ): TDOMElement;
      function LogInfo( Sender: TObject; Message: String ): TDOMElement;
      function LogException( Sender: TObject; Message: String ): TDOMElement;
      function LogException( Sender: TObject; Message: Exception ): TDOMElement;

      class function DOMNodeName: String; override;

      property LogFile: TXMLDocument read FLogFile write FLogFile;
      property Level: TDOMElement read FLevel write FLevel;

    published
      property FileName: String read FFileName write SetFileName;
  end;

const
  P3DLogOutput =
  '<html>' + LineEnding +
  '  <head>' + LineEnding +
  '    <link rel="stylesheet" href="tomorrow.css" />' + LineEnding +
  '    <script src="highlight.pack.js"></script>' + LineEnding +
  '    <script src="highlightjs-line-numbers.min.js"></script>' + LineEnding +
  '    <script>hljs.initHighlightingOnLoad();hljs.initLineNumbersOnLoad();</script>' + LineEnding +
  '     <style>' + LineEnding +
  '      table{' +  LineEnding +
  '        border-collapse: separate;' +  LineEnding +
  '        border-spacing: 0px 5px;' +  LineEnding +
  '        width: 100%;' +  LineEnding +
  '      }' + LineEnding +
  '      h1{' + LineEnding +
  '        font-family: Open Sans, Arial;' + LineEnding +
  '        font-size: 32px;' + LineEnding +
  '        border-bottom: 4px solid sienna;' + LineEnding +
  '        background-color: linen;' + LineEnding +
  '        box-shadow: 0px 5px 5px 0px rgba(0,0,0,0.1);' + LineEnding +
  '      }' + LineEnding +
  '      td,th{' + LineEnding +
  '        font-family: Open Sans, Arial;' + LineEnding +
  '        white-space: pre-wrap;' + LineEnding +
  '        text-align:left;' + LineEnding +
  '        vertical-align: top;' + LineEnding +
  '      }' + LineEnding +
  '      .hljs{' + LineEnding +
  '        border-bottom: 2px solid darkkhaki;' + LineEnding +
  '      }' + LineEnding +
  '      .messageheader{' + LineEnding +
  '        background-color: khaki;' + LineEnding +
  '        border-bottom: 2px solid darkkhaki;' + LineEnding +
  '      }' + LineEnding +
  '      .messageheader span{' + LineEnding +
  '        padding-right: 10px;' + LineEnding +
  '        padding-left: 10px;' + LineEnding +
  '        font-weight: bold;' + LineEnding +
  '        color: sienna;' + LineEnding +
  '      }' + LineEnding +
  '      .whitebox{' + LineEnding +
  '        border-bottom: 2px solid darkkhaki;' + LineEnding +
  '        background-color: white;' + LineEnding +
  '        padding: 5px;' + LineEnding +
  '      }' + LineEnding +
  '      .info{' + LineEnding +
  '        background-color: aliceblue;' + LineEnding +
  '      }' + LineEnding +
  '      .info td{' + LineEnding +
  '        border-bottom: 5px solid slateblue;' + LineEnding +
  '      }' + LineEnding +
  '      .exception{' + LineEnding +
  '        background-color: salmon;' + LineEnding +
  '      }' + LineEnding +
  '      .exception td{' + LineEnding +
  '        border-bottom: 5px solid crimson;' + LineEnding +
  '      }' + LineEnding +
  '     </style>' + LineEnding +
  '  </head>' + LineEnding +
  '  <body>' + LineEnding +
  '    <h1></h1>' + LineEnding +
  '    <table>' + LineEnding +
  '      <tr><th>Time</th><th>Message</th></tr>' + LineEnding +
  '    </table>' + LineEnding +
  '  </body>' + LineEnding +
  '</html>';

{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure WriteHTML( Element: TDOMNode; const AFileName: String );
var
  F: TextFile;

  procedure WriteNode( Element: TDOMNode );
  var
    i: Integer;
  begin
    if ( not Assigned( Element )) then
      exit;
    if ( Element.NodeName = '#text' ) then
      Write( F, Element.TextContent )
    else
      begin
        Write( F, '<' + Element.NodeName );
        if ( Assigned( Element.Attributes )) then
          for i:= 0 to Element.Attributes.Length - 1 do
            Write( F, Format( ' %s="%s"', [ Element.Attributes[ i ].NodeName, Element.Attributes[ i ].NodeValue ]));

        if ( Element.ChildNodes.Count > 0 ) then
          begin
            Write( F, '>' );
            for i:= 0 to Element.ChildNodes.Count - 1 do
              WriteNode( Element.ChildNodes[ i ]);
            Write( F, '</' + Element.NodeName + '>' );
          end
        else
          case ( Element.NodeName ) of
            'br', 'img', 'link': Write( F, '>' );
          else
            Write( F, '></', Element.NodeName, '>' );
          end;
      end;
  end;

begin
  Assign( F, AFileName );
  Rewrite( F );
  Write( F, '<!DOCTYPE html>' );
  if ( Assigned( Element )) then
    WriteNode( Element );
  CloseFile( F );
end;

{ TP3DLogger }

procedure TP3DLogger.SetFileName(AValue: String);
var
  FullPath: RawByteString;
begin
  FullPath:= ExpandFileName( AValue );
  if ( FFileName = FullPath ) then
    Exit;
  FFileName:= FullPath;
  WriteFile();
end;

function TP3DLogger.CreateMessageHeader( Sender: TObject ): TDOMElement;
var
  Txt: string;
  N: TComponentName;
  S: TStringStream;
begin
  Result:= LogFile.CreateElement( 'p' );
  Result.AttribStrings[ 'class' ]:= 'messageheader';

  if ( Sender is TP3DDataBlock ) then
    N:= '{' + TP3DDataBlock( Sender ).Owner.Name + '}.' + TP3DObject( Sender ).Name + ': '
  else if ( Sender is TP3DObject ) then
    N:= TP3DObject( Sender ).Name + ': '
  else
    N:= 'Instance of ';

  Txt:= '<span>Unit</span> ' + Sender.UnitName + ' <span>Object</span> ' + N + Sender.ClassName;
  S:= TStringStream.Create( Txt );
  ReadXMLFragment( Result, S );
  S.Free;
end;

constructor TP3DLogger.Create;
var
  F: TStringStream;
begin
  inherited Create;
  F:= TStringStream.Create( P3DLogOutput );
  XMLRead.ReadXMLFile( FLogFile, F );
  F.Free;
  Level:= TDOMElement( FLogFile.DocumentElement.FindNode( 'body' ).FindNode( 'h1'));
  Level.AppendChild( LogFile.CreateTextNode( 'Logfile for ' + ExtractFileName( ParamStr( 0 ))));

  Level:= TDOMElement( FLogFile.DocumentElement.FindNode( 'body' ).FindNode( 'table'));
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'FileName', smAttribute ));
end;

destructor TP3DLogger.Destroy;
begin
  FLevel:= nil;
  FLogFile.Free;
  inherited Destroy;
end;

procedure TP3DLogger.WriteFile;
begin
  if ( FileName <> '' ) then
    WriteHTML( LogFile.DocumentElement, FileName ); //XMLWrite.WriteXMLFile( LogFile, FileName );
end;

procedure TP3DLogger.AddChild(ADOM: TDOMElement);
begin
  if ( not Assigned( Level )) then
    begin
      exit;
      ADOM.Free;
    end;
  Level.AppendChild( ADOM );
  WriteFile;
end;

function TP3DLogger.LogInfoXML(Sender: TObject; Message: String): TDOMElement;
var
  Node, cell: TDOMElement;
  NodeText: TDOMText;
  S: TStringStream;
begin
  Node:= LogFile.CreateElement( 'tr' );
  Node.SetAttribute( 'class', 'info' );
  cell:= LogFile.CreateElement( 'td' );
  NodeText:= LogFile.CreateTextNode( IntToStr( SDL_GetTicks ));
  cell.AppendChild( NodeText );
  Node.AppendChild( cell );

  cell:= LogFile.CreateElement( 'td' );

  if ( Assigned( Sender )) then
     cell.AppendChild( CreateMessageHeader( Sender ));

  S:= TStringStream.Create( Message );
  ReadXMLFragment( cell, S );
  S.Free;
  Node.AppendChild( cell );
  AddChild( Node );
  Result:= Node;
end;

function TP3DLogger.LogInfo(Sender: TObject; Message: String): TDOMElement;
var
  Node, cell, header: TDOMElement;
  NodeText: TDOMText;
  N: TComponentName;
begin
  Node:= LogFile.CreateElement( 'tr' );
  Node.SetAttribute( 'class', 'info' );
  cell:= LogFile.CreateElement( 'td' );
  NodeText:= LogFile.CreateTextNode( IntToStr( SDL_GetTicks ));
  cell.AppendChild( NodeText );
  Node.AppendChild( cell );

  cell:= LogFile.CreateElement( 'td' );

  if ( Assigned( Sender )) then
     cell.AppendChild( CreateMessageHeader( Sender ));

  NodeText:= LogFile.CreateTextNode( Message );
  cell.AppendChild( NodeText );
  Node.AppendChild( cell );
  AddChild( Node );
  Result:= Node;
end;

function TP3DLogger.LogException(Sender: TObject; Message: String): TDOMElement;
var
  Node, cell: TDOMElement;
  NodeText: TDOMText;
begin
  Node:= LogFile.CreateElement( 'tr' );
  Node.SetAttribute( 'class', 'exception' );
  cell:= LogFile.CreateElement( 'td' );
  NodeText:= LogFile.CreateTextNode( IntToStr( SDL_GetTicks ));
  cell.AppendChild( NodeText );
  Node.AppendChild( cell );

  cell:= LogFile.CreateElement( 'td' );

  if ( Assigned( Sender )) then
     cell.AppendChild( CreateMessageHeader( Sender ));

  NodeText:= LogFile.CreateTextNode( Message {StringReplace( Message, LineEnding, '<br />', [ rfReplaceAll ])});
  cell.AppendChild( NodeText );
  Node.AppendChild( cell );
  AddChild( Node );
  Result:= Node;
end;

function TP3DLogger.LogException(Sender: TObject; Message: Exception ): TDOMElement;
begin //TODO: Sender might be invalid - in this case this would cause another exception
  Result:= LogException( Sender, Message.Message );//StringReplace( Message.Message, LineEnding, '<br />', [ rfReplaceAll ]));
end;

class function TP3DLogger.DOMNodeName: String;
begin
  Result:= 'logging';
end;


{$ENDIF}

