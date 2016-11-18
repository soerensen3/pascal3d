//p3dwindow.inc
//part of p3devents.pas

{$IFDEF INTERFACE}

type
  { TP3DLogger }

  TP3DLogger = class
  private
    FFileName: String;
    FLevel: TDOMElement;
    FLogFile: TXMLDocument;

    procedure SetFileName( AValue: String );

  public
    constructor Create;
    destructor Destroy; override;

    procedure WriteFile;
    procedure AddChild( ADOM: TDOMElement );
    function LogInfoXML( Sender: TObject; Message: String ): TDOMElement;
    function LogInfo( Sender: TObject; Message: String ): TDOMElement;
    function LogException( Sender: TObject; Message: String ): TDOMElement;
    function LogException( Sender: TObject; Message: Exception ): TDOMElement;

    property LogFile: TXMLDocument read FLogFile write FLogFile;
    property FileName: String read FFileName write SetFileName;
    property Level: TDOMElement read FLevel write FLevel;
  end;

const
  P3DLogStylesheet =
  '<xsl:stylesheet id="stylesheet"' + LineEnding +
  '                version="1.0"' + LineEnding +
  '                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">' + LineEnding +
  '  <xsl:template match="xsl:stylesheet"/>' + LineEnding +
  '  <xsl:template match="/">' + LineEnding +
  '  <html>' + LineEnding +
  '  <head>' + LineEnding +
  '    <link rel="stylesheet" href="tomorrow.css" />' + LineEnding +
  '    <script src="highlight.pack.js"/>' + LineEnding +
  '    <script>hljs.initHighlightingOnLoad();</script>' + LineEnding +
  '     <style>' + LineEnding +
  '      table{' +  LineEnding +
  '        border-collapse: separate;' +  LineEnding +
  '        border-spacing: 0px 5px;' +  LineEnding +
  '        width: 100%;' +  LineEnding +
  '      }' + LineEnding +
  '      h1{' + LineEnding +
  '        font-family: Open Sans, Arial;' + LineEnding +
  '        font-size: 32px;' + LineEnding +
  '        border-bottom: 4px solid black;' + LineEnding +
  '        box-shadow: 0px 5px 5px 0px rgba(0,0,0,0.1);' + LineEnding +
  '      }' + LineEnding +
  '      td,th{' + LineEnding +
  '        font-family: Open Sans, Arial;' + LineEnding +
  '        white-space: pre-wrap;' + LineEnding +
  '        text-align:left;' + LineEnding +
  '        vertical-align: top;' + LineEnding +
  '      }' + LineEnding +
  '      .info{' + LineEnding +
  '        background-color: aliceblue;' + LineEnding +
  '      }' + LineEnding +
  '      .exception{' + LineEnding +
  '        background-color: rgb( 255, 87, 51 );' + LineEnding +
  '      }' + LineEnding +
  '     </style>' + LineEnding +
  '  </head>' + LineEnding +
  '  <body>' + LineEnding +
  '  <h1>Logfile</h1>' + LineEnding +
  '  <table>' + LineEnding +
  '  <tr><th>Time</th><th>Message</th></tr>' + LineEnding +
  '  <xsl:apply-templates />' + LineEnding +
  '  </table>' + LineEnding +
  '  </body>' + LineEnding +
  '  </html>' + LineEnding +
  '  </xsl:template>' + LineEnding +
  '  <xsl:template match="info">' + LineEnding +
  '    <tr class="info"><td><xsl:value-of select="@Time"/></td><td><xsl:value-of select="."/></td></tr>' + LineEnding +
  '  </xsl:template>' + LineEnding +
  '  <xsl:template match="exception">' + LineEnding +
  '    <tr class="exception"><td><xsl:value-of select="@Time"/></td><td><xsl:value-of select="."/></td></tr>' + LineEnding +
  '  </xsl:template>' + LineEnding +
  '</xsl:stylesheet>';

{$ENDIF}

{$IFDEF IMPLEMENTATION}

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

constructor TP3DLogger.Create;
var
  Instr: TDOMProcessingInstruction;
  F: TStringStream;
begin
  FLogFile:= TXMLDocument.Create;
  Instr:= FLogFile.CreateProcessingInstruction( 'xml-stylesheet', 'type="text/xml" href="#stylesheet"' );
  LogFile.AppendChild( Instr );
  Level:= FLogFile.CreateElement( 'p3dlog');
  F:= TStringStream.Create( P3DLogStylesheet );
  XMLRead.ReadXMLFragment( Level, F );
  F.Free;
  LogFile.AppendChild( Level );
end;

destructor TP3DLogger.Destroy;
begin
  FLogFile.Free;
  inherited Destroy;
end;

procedure TP3DLogger.WriteFile;
begin
  if ( FileName <> '' ) then
    XMLWrite.WriteXMLFile( LogFile, FileName );
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
  Node: TDOMElement;
  NodeText: TDOMText;
  S: TStringStream;
begin
  Node:= LogFile.CreateElement( 'td' );
  Node.AttribStrings[ 'Time' ]:= IntToStr( SDL_GetTicks );
  if ( Assigned( Sender )) then
    begin
      Node.AttribStrings[ 'Unit' ]:= Sender.UnitName;
      Node.AttribStrings[ 'Sender' ]:= Sender.ClassName;
    end;
  //Message:= StringReplace( Message, LineEnding, '<br />', [ rfReplaceAll ]);
  S:= TStringStream.Create( Message );
  ReadXMLFragment( Node, S );
  S.Free;
  AddChild( Node );
  Result:= Node;
end;

function TP3DLogger.LogInfo(Sender: TObject; Message: String): TDOMElement;
var
  Node: TDOMElement;
  NodeText: TDOMText;
begin
  Node:= LogFile.CreateElement( 'info' );
  Node.AttribStrings[ 'Time' ]:= IntToStr( SDL_GetTicks );
  if ( Assigned( Sender )) then
    begin
      Node.AttribStrings[ 'Unit' ]:= Sender.UnitName;
      Node.AttribStrings[ 'Sender' ]:= Sender.ClassName;
    end;
  //Message:= StringReplace( Message, LineEnding, '<br />', [ rfReplaceAll ]);
  NodeText:= LogFile.CreateTextNode( Message );
  Node.AppendChild( NodeText );
  AddChild( Node );
  Result:= Node;
end;

function TP3DLogger.LogException(Sender: TObject; Message: String): TDOMElement;
var
  Node: TDOMElement;
  NodeText: TDOMText;
begin
  Node:= LogFile.CreateElement( 'exception' );
  Node.AttribStrings[ 'Time' ]:= IntToStr( SDL_GetTicks );
  if ( Assigned( Sender )) then
    begin
      Node.AttribStrings[ 'Unit' ]:= Sender.UnitName;
      Node.AttribStrings[ 'Sender' ]:= Sender.ClassName;
    end;
  NodeText:= LogFile.CreateTextNode( Message {StringReplace( Message, LineEnding, '<br />', [ rfReplaceAll ])});
  Node.AppendChild( NodeText );
  AddChild( Node );
  Result:= Node;
end;

function TP3DLogger.LogException(Sender: TObject; Message: Exception ): TDOMElement;
begin
  Result:= LogException( Sender, Message );//StringReplace( Message.Message, LineEnding, '<br />', [ rfReplaceAll ]));
end;


{$ENDIF}

