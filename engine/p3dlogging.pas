unit p3dlogging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLWrite;

type

  { TP3DLogger }

  TP3DLogger = class
  private
    FFileName: String;
    FLevel: TDOMElement;
    FLogFile: TXMLDocument;
    procedure SetFileName(AValue: String);

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

var
  P3DLog: TP3DLogger;

implementation


{ TP3DLogger }

procedure TP3DLogger.SetFileName(AValue: String);
begin
  if ( FFileName = AValue ) then
    Exit;
  FFileName:= AValue;
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
  if ( Assigned( Sender )) then
    begin
      Node.AttribStrings[ 'Unit' ]:= Sender.UnitName;
      Node.AttribStrings[ 'Sender' ]:= Sender.ClassName;
    end;
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
  if ( Assigned( Sender )) then
    begin
      Node.AttribStrings[ 'Unit' ]:= Sender.UnitName;
      Node.AttribStrings[ 'Sender' ]:= Sender.ClassName;
    end;
  NodeText:= LogFile.CreateTextNode( Message );
  Node.AppendChild( NodeText );
  AddChild( Node );
  Result:= Node;
end;

function TP3DLogger.LogException(Sender: TObject; Message: Exception
  ): TDOMElement;
begin
  Result:= LogException( Sender, Message.Message );
end;

initialization
  P3DLog:= TP3DLogger.Create;
finalization
  P3DLog.Free;

end.

