unit mouse;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLIntf, BGRAKnob;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAKnob1: TBGRAKnob;
    BGRAKnob2: TBGRAKnob;
    Timer1: TTimer;
    procedure BGRAKnob2ValueChanged(Sender: TObject; Value: single);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
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

procedure TForm1.Timer1Timer(Sender: TObject);
var
  dx,dy: Integer;
  scx, scy: Integer;
  p: TPoint;
begin
  GetCursorPos( p );
  scx:= Left + ClientWidth div 2;
  scy:= Top + ClientHeight div 2;
  dx:= scx - p.x;
  dy:= scy - p.y;
  BGRAKnob1.Value:= BGRAKnob1.Value + dx;
  BGRAKnob2.Value:= BGRAKnob2.Value + dy;
  SetCursorPos( scx, scy );
  Caption:= Format( 'dx=%d dy=%d', [ dx, dy ]);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if ( Key = 27 ) then
    Close;
end;

procedure TForm1.BGRAKnob2ValueChanged(Sender: TObject; Value: single);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

end.

