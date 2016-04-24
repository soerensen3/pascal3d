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
    function LogInfo( Sender: TObject; Message: String ): TDOMElement;
    function LogException( Sender: TObject; Message: String ): TDOMElement;
    function LogException( Sender: TObject; Message: Exception ): TDOMElement;

    property LogFile: TXMLDocument read FLogFile write FLogFile;
    property FileName: String read FFileName write SetFileName;
    property Level: TDOMElement read FLevel write FLevel;
  end;

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
begin
  FLogFile:= TXMLDocument.Create;
  Level:= FLogFile.CreateElement( 'p3dlog');
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

function TP3DLogger.LogException(Sender: TObject; Message: Exception
  ): TDOMElement;
begin
  Result:= LogException( Sender, Message );//StringReplace( Message.Message, LineEnding, '<br />', [ rfReplaceAll ]));
end;


{$ENDIF}

