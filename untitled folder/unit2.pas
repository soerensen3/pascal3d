unit unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  math3d;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    procedure UpdateStatus;
    { public declarations }
  end;

var
  Form1: TForm1;
  List: TVec2List;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  List:= TVec2List.Create;
  UpdateStatus;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  List.Free;
  List:= nil;
  UpdateStatus;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  i: Integer;
begin
  if ( Assigned( List )) then
    for i:= 1 to 5 do
      List.Add( vec2( ( Random( 20 ) - 10 ) / 10, ( Random( 20 ) - 10 ) / 10 ));
  UpdateStatus;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TForm1.UpdateStatus;
var
  i: Integer;
begin
  Label1.Caption:= '$' + IntToHex( Word( Pointer( List )), 8 );

  ListBox1.Clear;
  if ( Assigned( List )) then
    for i:= 0 to List.Count - 1 do
      ListBox1.Items.Add( Format( 'x = %f, y = %f, Pointer = [%p]', [ List[ i ].X, List[ i ].Y, List.PtrTo( i )]));
end;

end.

