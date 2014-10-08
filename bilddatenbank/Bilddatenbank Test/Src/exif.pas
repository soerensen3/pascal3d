unit EXIF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, Img;

procedure ProgramToStrLst( Cmd: String; Lst: TStrings );
procedure ProgramToFile( Cmd: String; outFile: String );
function ReadTags( FileName: String; List: TStrings ): Integer;
function ReadImgToImgObj( FileName: String ): TImgObj;
function GenerateThumbnail( FileName: String ): String;
function GetThumbnailName( FileName: String ): String;

var
  {$IFDEF WINDOWS}
  EXIF_FILENAME: String = 'exiftool.exe';
  {$ENDIF}
  {$IFDEF LINUX}
  EXIF_FILENAME: String = 'exiftool';
  {$ENDIF}
  CACHE_PATH: String = 'Cache';

implementation

procedure ProgramToStrLst( Cmd: String; Lst: TStrings );
const
  READ_BYTES = 2048;

var
  AProcess: TProcess;
  M: TMemoryStream;

  BytesRead, n: LongInt;
  i: Integer;

// Ab hier startet Ihr Programm
begin
  // Jetzt erzeugen wir das TProcess Objekt und
  // ordnen es der Variablen AProcess zu.
  AProcess := TProcess.Create(nil);
  BytesRead := 0;

  // Gibt an, welcher Befehl vom Prozess ausgeführt werden soll
  AProcess.CommandLine := Cmd;

  AProcess.ShowWindow:= swoHide;

  // Startet den Prozess, nachdem die Parameter entsprechend
  // gesetzt sind

  WriteLn( 'Running Command: ' + Cmd );
  if ( not Assigned( Lst )) then
    begin
      AProcess.Options := AProcess.Options + [ poWaitOnExit ];
      AProcess.Execute;
    end
  else
    begin
      AProcess.Options := AProcess.Options + [ poUsePipes ];
      AProcess.Execute;
      M := TMemoryStream.Create;

      while AProcess.Running do
        begin
          // stellt sicher, dass wir Platz haben
          M.SetSize(BytesRead + READ_BYTES);

          // versuche, es zu lesen
          n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
          if n > 0
          then
            Inc(BytesRead, n)
          else
            // keine Daten, warte 100 ms
            Sleep( 100 );
        end;
      // lese den letzten Teil
        repeat
          // stellt sicher, dass wir Platz haben
          M.SetSize( BytesRead + READ_BYTES );
          // versuche es zu lesen
          n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
          if ( n > 0 )then
            Inc(BytesRead, n);
        until n <= 0;

        M.SetSize( BytesRead );

        Lst.LoadFromStream( M );
        Lst.Delimiter:= ',';
        Lst.DelimitedText:= Lst.Text;
        M.Free;
    end;

  AProcess.Free;
end;

procedure ProgramToFile(Cmd: String; outFile: String);
const
  READ_BYTES = 2048;

var
  AProcess: TProcess;
  M: TMemoryStream;

  BytesRead, n: LongInt;
  i: Integer;

// Ab hier startet Ihr Programm
begin
  // Jetzt erzeugen wir das TProcess Objekt und
  // ordnen es der Variablen AProcess zu.
  AProcess := TProcess.Create(nil);
  BytesRead := 0;

  // Gibt an, welcher Befehl vom Prozess ausgeführt werden soll
  AProcess.CommandLine := Cmd;

  AProcess.ShowWindow:= swoHide;

  // Startet den Prozess, nachdem die Parameter entsprechend
  // gesetzt sind

  WriteLn( 'Running Command: ' + Cmd );
  AProcess.Options := AProcess.Options + [ poUsePipes ];
  AProcess.Execute;
  M:= TMemoryStream.Create;

  while AProcess.Running do
    begin
      // stellt sicher, dass wir Platz haben
      //F.SetSize(BytesRead + READ_BYTES);

      // versuche, es zu lesen
      n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
      if n > 0
      then
        Inc(BytesRead, n)
      else
        // keine Daten, warte 100 ms
        Sleep( 100 );
    end;
  // lese den letzten Teil
  repeat
    // stellt sicher, dass wir Platz haben
    M.SetSize( BytesRead + READ_BYTES );
    // versuche es zu lesen
    n := AProcess.Output.Read((M.Memory + BytesRead)^, READ_BYTES);
    if ( n > 0 )then
      Inc(BytesRead, n);
  until n <= 0;

  M.SetSize( BytesRead );

  M.SaveToFile( outFile );
  M.Free;

  AProcess.Free;
end;

function ReadTags( FileName: String; List: TStrings ): Integer;
begin
  Assert( List <> nil );
  ProgramToStrLst( EXIF_FILENAME + ' -keywords -T "' + FileName + '"', List );
  Result:= List.Count;
end;

function ReadImgToImgObj(FileName: String): TImgObj;
begin
  Result:= TImgObj.Create;
  ReadTags( FileName, Result.Tags );
end;

function GenerateThumbnail(FileName: String): String;
begin
  Result:= GetThumbnailName( FileName );

  ForceDirectories( ExtractFilePath( Result ));
//  ProgramToFile( EXIF_FILENAME + ' -b -ThumbnailImage "' + FileName + '"', Result );
  {$IfDef WINDOWS}
  ProgramToStrLst( 'cmd /c "' + EXIF_FILENAME + ' -b -ThumbnailImage "' + FileName + '" > "' + Result + '""', nil );
  {$EndIf}
  {$IfDef LINUX}
  ProgramToStrLst( '' + EXIF_FILENAME + ' -b -ThumbnailImage "' + FileName + '" > "' + Result + '""', nil );
  {$EndIf}
end;


function GetThumbnailName(FileName: String): String;
var
  Drive,
    Path,
    FN: String;
begin
  FileName:= ExpandFileName( FileName );
  Path:= ExtractFilePath( FileName );
  Drive:= ExtractFileDrive( FileName );
  Path:= Copy( Path, Length( Drive ) + 1, Length( Path ) - Length( Drive ));
  SetLength( Drive, Length( Drive ) - 1 );
  FN:= ExtractFileName( FileName );
  Result:= CACHE_PATH + DirectorySeparator + Drive + DirectorySeparator + ExcludeTrailingBackslash( ExcludeLeadingPathDelimiter( Path )) + DirectorySeparator + FN;
end;

end.
