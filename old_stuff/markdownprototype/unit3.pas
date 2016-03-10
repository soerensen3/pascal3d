unit unit3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, p3dmarkdown, p3dNodes, p3dshadernodes;

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

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);

var
  indent: Integer;

  procedure DebugFragments( Fragments: TP3DShaderNodeFragmentList );
  var
    frag: TP3DShaderNodeFragment;
  begin
    inc( indent );

    for frag in Fragments do
      case frag.ClassName of
        'TP3DShaderNodeFragmentIfDef': Memo1.Lines.Add( StringOfChar( ' ', indent * 2 ) + 'IfDef ' +
                                       TP3DShaderNodeFragmentIfDef( frag ).Caption );
        'TP3DShaderNodeFragmentInline': Memo1.Lines.Add( '°°°' +
                                       TP3DShaderNodeFragmentInline( frag ).Text + '°°°' );
        'TP3DShaderNodeFragmentInput': Memo1.Lines.Add( StringOfChar( ' ', indent * 2 ) + 'Input ' +
                                       TP3DShaderNodeFragmentInput( frag ).InputName );
        else
          Memo1.Lines.Add( StringOfChar( ' ', indent * 2 ) + frag.ClassName );
      end;
    dec( indent );
  end;

var
  socket: TP3DNodeSocket;
  Node: TP3DNode;
begin
  Memo1.Clear;
  if ( OpenDialog1.Execute ) then
    with ( ParseMarkdownFile( OpenDialog1.FileName )) do
      begin
        for Node in Nodes do
          begin
            Memo1.Lines.Add( 'Node ' + Node.Name );
            for socket in Node.Inputs do
              Memo1.Lines.Add( 'input: ' + socket.Name + ' : ' + socket.SocketType );
            for socket in Node.Outputs do
              begin
                Memo1.Lines.Add( 'output: ' + socket.Name + ' : ' + socket.SocketType );
                indent:= 1;
                DebugFragments( TP3DShaderNodeSocket( socket ).Fragments );
              end;
            Memo1.Lines.Add( '---' + LineEnding );
          end;
      end;
end;

end.

