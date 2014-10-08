unit imagedb;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ValEdit, ComCtrls, ExtCtrls, process, EXIF, Img;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    ListView1: TListView;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
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
  ImgObj: TImgObj;
begin
  ImgObj:= ReadImgToImgObj( '04-Pilze\Tintenfischpilz003_NSG_Friesheimer_Busch_Ulrich_G_Sander.JPG' );
  Memo1.Lines.Assign( ImgObj.Tags );
//  Memo1.Lines.Add( GetThumbnailName( '04-Pilze\Tintenfischpilz003_NSG_Friesheimer_Busch_Ulrich_G_Sander.JPG' ));
  //GenerateThumbnail( '04-Pilze\Tintenfischpilz003_NSG_Friesheimer_Busch_Ulrich_G_Sander.JPG' );
  ImgObj.Free;
end;

procedure FindAllFiles( output: TStrings; RootFolder: string; Mask: string = '*.*'; Recurse: Boolean = True );

var
  SR: TSearchRec;
begin
  // Implementation ab Delphi 5
  RootFolder := IncludeTrailingPathDelimiter(RootFolder);

  // * * * * *
  // nur bis einschließlich Delphi 4 benötigt
  if Mask = '' then
    Mask := '*.*';
  // * * * * *

  if Recurse then
    if FindFirst(RootFolder + '*.*', faAnyFile, SR) = 0 then
      try
        repeat
          if SR.Attr and faDirectory = faDirectory then
            // --> ein Verzeichnis wurde gefunden
            // der Verzeichnisname steht in SR.Name
            // der vollständige Verzeichnisname (inkl. darüberliegender Pfade) ist
            // RootFolder + SR.Name
            if (SR.Name <> '.') and (SR.Name <> '..') then
              FindAllFiles( output, RootFolder + SR.Name, Mask, Recurse);
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
  if FindFirst(RootFolder + Mask, faAnyFile, SR) = 0 then
    try
      repeat
        if SR.Attr and faDirectory <> faDirectory then
        begin
          // --> eine Datei wurde gefunden
          // der Dateiname steht in SR.Name
          // der vollständige Dateiname (inkl. Pfadangabe) ist
          // RootFolder + SR.Name

          // folgende Zeile schreibt den vollständigen Namen in eine Memo Feld des
          // Formulars Form1
          if ( Recurse ) then
            output.Add( RootFolder + SR.Name)
          else
            output.Add( ( SR.Name ));
        end;
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  Files: TStringList;
  i: Integer;
begin
  Files:= TStringList.Create;
  FindAllFiles( Files, '04-Pilze', '*.*', False );
  for i:= 1 to Files.Count do
    ListView1.Items.Add.Caption:= Files[ i - 1 ];
  Files.Free;
end;

procedure TForm1.FormClick(Sender: TObject);
begin

end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  fn,
    preview: String;
  ImgObj: TImgObj;
begin
  fn:= ( '04-Pilze' + DirectorySeparator + Item.Caption );
  ImgObj:= ReadImgToImgObj( fn );
  preview:= GetThumbnailName( fn );
  if ( not FileExists( preview )) then
    GenerateThumbnail( fn );

  try
    Image1.Picture.LoadFromFile( preview );

  finally
  end;
  Memo2.Lines.Assign( ImgObj.Tags );
  ImgObj.Free;
end;

end.
