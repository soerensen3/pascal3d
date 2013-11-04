unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, UTF8Process, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, LCLType, GameVars, Game, cmd;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Edit1: TEdit;
    IdleTimer1: TIdleTimer;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure Panel2Resize(Sender: TObject);
    procedure OnWrite( S: String );
  private
    FCurrentLine: Integer;
    FInput: TStringList;
    { private declarations }
  public
    property CurrentLine: Integer read FCurrentLine write FCurrentLine;
    property Input: TStringList read FInput write FInput;
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  Done:= False;
end;

procedure TForm1.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ( Key = VK_RETURN ) then
    begin
      Memo1.Lines.Add( Edit1.Text );
      Input.Add( Edit1.Text );
      Memo1.Lines.Add( ParseCmd( Edit1.Text ));
      CurrentLine:= Input.Count;
      Edit1.Text:= '';
    end;
  if ( Input.Count > 0 ) then
    begin
      if ( Key = VK_UP ) then
        begin
          CurrentLine:= CurrentLine - 1;
          if ( CurrentLine < 0 ) then
            CurrentLine:= Input.Count - 1;
          Edit1.Text:= Input[ CurrentLine ];
        end;
      if ( Key = VK_DOWN ) then
        begin
          CurrentLine:= CurrentLine + 1;
          if ( CurrentLine = Input.Count ) then
            Edit1.Text:= ''
          else if ( CurrentLine > Input.Count ) then
            begin
              CurrentLine:= 0;
              Edit1.Text:= Input[ CurrentLine ];
            end;
        end;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cmd.OnWrite:= @OnWrite;
  TheGame:= TGame.Create;
  TheGame.Init;
  FInput:= TStringList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  cmd.OnWrite:= nil;
  FInput.Free;
  TheGame.Deinit;
  TheGame.Free;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  GamePainter.Screen.Width:= Panel2.Width;
  GamePainter.Screen.Height:= Panel2.Height;
end;

procedure TForm1.IdleTimer1Timer(Sender: TObject);
begin
  TheGame.Process;
  Caption:= FloatToStr( TheGame.Speed );
  Panel2.Canvas.Draw( 0, 0, GamePainter.Screen );
end;

procedure TForm1.Panel2Resize(Sender: TObject);
begin
  if ( Assigned( TheGame )) then
    begin
      GamePainter.Screen.Width:= Panel2.Width;
      GamePainter.Screen.Height:= Panel2.Height;
    end;
end;

procedure TForm1.OnWrite(S: String);
begin
  Memo1.Lines.Add( S );
end;

end.

