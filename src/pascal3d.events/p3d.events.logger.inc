//unit p3d.logger;

{$IFDEF INTERFACE}

//{$mode objfpc}{$H+}

//interface
//uses
//  Classes, SysUtils, sdl2;

type
  TAttribute = record
    Name: String;
    Value: String;
  end;

  TAttributes = array of TAttribute;

  function attribute( Name: String; Value: String ): TAttribute;

  function attributes( attribs: TAttributes ): String;
  function tag( name: String; content: String; const attributes: String = '' ): String;
  function tag_begin( name: String; const attributes: String = '' ): String; inline;
  function tag_end( name: String ): String; inline;
  function message_header( Sender: TObject ): String;

  procedure log_file_open( filename: String );
  procedure log_file_close;

  procedure log_init;
  procedure log_end;
  procedure log_string( message: String );
  procedure log_info( message: String; const attribs: String = '' );
  procedure log_exception( message: String; const attribs: String = '' );



const
  header: String =
    '    <link rel="stylesheet" href="tomorrow.css" />' + LineEnding +
    '    <script src="highlight.pack.js"></script>' + LineEnding +
    '    <script src="highlightjs-line-numbers.min.js"></script>' + LineEnding +
    '    <script>hljs.initHighlightingOnLoad();hljs.initLineNumbersOnLoad();</script>' + LineEnding +
    '    <style>' + LineEnding +
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
    '    </style>' + LineEnding;


var
  Log: TStream = nil;
  LogEnd: String = '';
  LogTitle: String;



{$ENDIF}

{$IFDEF IMPLEMENTATION}
//implementation

function attribute(Name: String; Value: String): TAttribute;
begin
  Result.Name:= Name;
  Result.Value:= Value;
end;

function attributes(attribs: TAttributes): String;
var
  i: Integer;
begin
  Result:= '';
  for i:= low( attribs ) to high( attribs ) do
    Result:= Result + ' ' + attribs[ i ].Name + '= "' + attribs[ i ].Value + '"';
end;

function tag(name: String; content: String; const attributes: String): String;
begin
  Result:= tag_begin( name, attributes );
  Result:= Result + content;
  Result:= tag_end( name );
end;

function tag_begin(name: String; const attributes: String): String;
var
  i: Integer;
begin
  Result:= '<' + name;

  Result:= Result + attributes;

  Result:= Result + '>';
end;

function tag_end(name: String): String;
begin
  Result:= Result + '</' + name + '>';
end;

function message_header(Sender: TObject): String;
begin
  if ( Assigned( Sender )) then
    Result:= tag( 'p', Sender.UnitName + '.' + Sender.ToString, attributes([ attribute( 'class', 'messageheader' )]) )
  else
    Result:= '';
end;

procedure log_file_open(filename: String);
begin
  Log:= TFileStream.Create( filename, fmOpenWrite or fmCreate );
  log_end;
  log_init;
end;

procedure log_file_close;
begin
  FreeAndNil( Log );
end;


procedure log_init;
begin
  log_string( tag_begin( 'html' ));
  log_string( tag( 'title', LogTitle ));
  log_string( header );
  log_string( tag_begin( 'body' ));
  log_string( tag_begin( 'table' ));
end;

procedure log_end;
begin
  LogEnd:= tag_end( 'table' ) + LineEnding +
           tag_end( 'body' ) + LineEnding +
           tag_end( 'html' );
end;

procedure log_string(message: String);
var
  p: Int64;
begin
  if ( Assigned( Log )) then begin
    Log.Write( message[ 1 ], Length( message ));
    p:= Log.Position;
    Log.Write( LogEnd[ 1 ], Length( LogEnd ));
    Log.Position:= p;
  end;
end;

procedure log_info(message: String; const attribs: String);
var
  m: String;
begin
  m:= tag( 'tr',
        tag( 'td', IntToStr( SDL_GetTicks )) +
        tag( 'td', message ),
        attributes([ attribute( 'class', 'info' )]) + attribs
      );
  log_string( m );
end;

procedure log_exception(message: String; const attribs: String);
var
  m: String;
begin
  m:= tag( 'tr',
        tag( 'td', IntToStr( SDL_GetTicks )) +
        tag( 'td', message ),
        attributes([ attribute( 'class', 'exception' )]) + attribs
      );
  log_string( m );
end;


//end.
{$ENDIF}
