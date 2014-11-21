unit letterman_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, Math, p3dbmpfont, p3dbmpfontfile, p3dMath, p3dtexture;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    FontDialog1: TFontDialog;
    FontsEdt: TLabeledEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    SpinEdit1: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FontsEdtKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure PaintLetters;
    procedure InitializeFont;
    procedure SaveBMPToFile( FName: String );
  private
    BmpFont: TP3DFont;
    Image: TBitmap;
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
begin
  if ( FontDialog1.Execute ) then
    FontsEdt.Text:= FontDialog1.Font.Name;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Image.Width:= SpinEdit1.Value;
  Image.Height:= SpinEdit1.Value;
  InitializeFont;
  PaintLetters;
  Repaint;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  letter: TP3DFontLetter;
  fp: String;
  fn: String;
begin
  if ( SaveDialog1.Execute ) then
    begin
      fp:= ExtractFilePath( SaveDialog1.FileName );
      fn:= ExtractFileName( SaveDialog1.FileName );
      SetCurrentDir( fp );
      Image.SaveToFile( ChangeFileExt( fn, '.jpg' ));
      BmpFont.Textures.Add( TP3DTexture.Create( ChangeFileExt( fn, '.jpg' )));
      for letter in BmpFont.Letters do
        letter.Texture:= BmpFont.Textures[ 0 ];
      p3dbmpfontfile.SaveP3DFont( fn, BmpFont );
    end;
end;

procedure TForm1.FontsEdtKeyPress(Sender: TObject; var Key: char);
begin
  if ( Key = #13 ) then //RETURN
    FontDialog1.Font.Name:= FontsEdt.Text;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BmpFont:= TP3DFont.Create;
  Image:= TBitmap.Create();
  FontsEdt.Text:= FontDialog1.Font.Name;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Image.Free;
  BmpFont.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  Canvas.Brush.Style:= bsCross;
  Canvas.Brush.Color:= clGray;
  Canvas.Rectangle( 0, 0, ClientWidth, ClientHeight );
  Canvas.Draw( 0, 100, Image );
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  Label2.Caption:= 'x' + IntToStr( SpinEdit1.Value ) + 'px';
end;

procedure TForm1.PaintLetters;
var
  x: Integer;
  y: Integer;
  lineheight: Integer;
  i: Integer;
  c: WideChar;
  w: Integer;
  tw: Integer;
  th: Integer;
begin
  Form1.Caption:= '';

  lineheight:= 0;
  x:= 0;
  y:= 0;
  w:= SpinEdit1.Value;
  FontDialog1.Font.Name:= FontsEdt.Text;
  Image.Clear;
  Image.Canvas.Brush.Style:= bsClear;
  Image.Canvas.Font.Assign( FontDialog1.Font );
  Image.Canvas.Font.Color:= clWhite;
  Image.Canvas.Pen.Color:= clWhite;
  with ( Image.Canvas ) do
    for i:= 0 to BmpFont.Letters.Count - 1 do
      begin
        c:= BmpFont.Letters[ i ].Letter;
        tw:= TextWidth( c );
        if ( x + tw >= w ) then
          begin
            x:= 0;
            y:= y + lineheight;
          end;
        th:= TextHeight( c );
        TextOut( x, y, c );
        //Rectangle( x, y, x + tw, y + th );
        Form1.Caption:= Form1.Caption + c;
        BmpFont.Letters[ i ].uv1:= vec2( x + 0.5, y + 0.5 ) / w;
        BmpFont.Letters[ i ].uv2:= vec2( x + tw - 0.5, y + th - 0.5 ) / w;
        lineheight:= Max( lineheight, th );
        x:= x + tw;
      end;
end;

procedure TForm1.InitializeFont;
var
  c: WideChar;
  letter: TP3DFontLetter;
begin // +U0020 - +U007E
  BmpFont.Letters.Clear;
  for c := #$0020 to #$007E do
    begin
      letter:= TP3DFontLetter.Create;
      letter.Letter:= c;
      BmpFont.Letters.Add( letter );
    end;
end;

procedure TForm1.SaveBMPToFile(FName: String);
begin
  Image.SaveToFile( FName );
end;

end.

