unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, strutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;


  function ParseMarkdownFile(FileName: String): String;
  function ParseMarkdown( Md: TStringList ): String;

implementation

uses Unit2;

function ParseMarkdownFile(FileName: String): String;
var
  F: TStringList;
begin
  F:= TStringList.Create;
  F.LoadFromFile( FileName );
  Result:= ParseMarkdown( F );
  F.Free;
end;



function ParseMarkdown(Md: TStringList): String;
var
  Outp: TStringList;
  i: Integer;

  procedure ParseOutput;
  var
    Params, words, name, value: String;
    tmp, i2: Integer;
  begin
    Params:= '';
    if ( Pos( '{', Md[ i ]) > 0 ) then
      Params:= ExtractText( Md[ i ], '{', '}' );
    Outp.Add( 'Parsing Output' );
    if ( Params > '' ) then
      begin
        for i2:=1 to WordCount( Params, [ ',' ]) do
          begin
            words:= ExtractWordPos( i2, Params, [ ',' ], tmp );
            ExtractNameValuePair( words, '=', name, value );

            Outp.Add( 'parameter: "' + name + '" = "' + value + '"' );
          end;
      end;
    repeat
      Inc( i );
    until ( i = Md.Count ) or ( Pos( '```', Md[ i ]) > 0 );
  end;

  procedure ParseNode;
  var
    name, value: String;
  begin
    Inc( i );
    repeat
      if ( Pos( ':', Md[ i ]) > 0 ) then
        begin
          ExtractNameValuePair( Md[ i ], ':', name, value );
          Outp.Add( 'prop: "' + name + '" : "' + value + '"' );
        end
      else
        Outp.Add( 'Ignoring line ' + IntToStr( i ));
      Inc( i );
    until ( i = Md.Count ) or ( Pos( '---', Md[ i ]) > 0 );
  end;

  procedure ScanLine;
  begin
    if ( Pos( '```', Md[ i ]) > 0 ) then
      ParseOutput
    else if ( Pos( '---', Md[ i ]) > 0 ) then
      ParseNode
    else
      Outp.Add( 'Ignoring line ' + IntToStr( i + 1 ));
    Inc( i );
  end;

begin
  Outp:= TStringList.Create;
  i:= 0;
  while i < Md.Count do
    ScanLine;

  Result:= Outp.Text;
  Outp.Free;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if ( OpenDialog1.Execute ) then
    Memo1.Text:= ParseMarkdownFile( OpenDialog1.FileName );
end;

end.

