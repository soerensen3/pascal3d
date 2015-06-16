unit p3dfileutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

  function P3DListFolderFiles( Folder: String; Recursive: Boolean; Relative: Boolean ): TStringList;

implementation

procedure FindAllFiles( output: TStrings; ext: String; RootFolder: string; Recurse: Boolean = True; Relative: Boolean = True );

var
  SR: TSearchRec;
  fileext: String;
begin
  //if ( Relative ) then
  //  RootFolder:= ExtractRelativepath( RootFolder, GetCurrentDir );
  RootFolder := IncludeTrailingPathDelimiter(RootFolder);
  if Recurse then
    if ( FindFirst(RootFolder + '*', faAnyFile or faDirectory, SR) = 0 ) then
      try
        repeat
          if SR.Attr and faDirectory = faDirectory then
            // --> ein Verzeichnis wurde gefunden
            // der Verzeichnisname steht in SR.Name
            // der vollständige Verzeichnisname (inkl. darüberliegender Pfade) ist
            // RootFolder + SR.Name
            if (SR.Name <> '.') and (SR.Name <> '..') then
              FindAllFiles( output, Ext, RootFolder + SR.Name, Recurse, Relative );
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
  if FindFirst(RootFolder + '*.*', faAnyFile, SR) = 0 then
    try
      repeat
        if SR.Attr and faDirectory <> faDirectory then
        begin
          // --> eine Datei wurde gefunden
          // der Dateiname steht in SR.Name
          // der vollständige Dateiname (inkl. Pfadangabe) ist
          // RootFolder + SR.Name

          fileext:= LowerCase( ExtractFileExt( SR.Name ));
          if ( ext = fileext ) then
            begin
              if ( Recurse ) then
                output.Add( RootFolder + SR.Name)
              else
                output.Add(( SR.Name ));
            end;
        end;
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
end;

function P3DListFolderFiles(Folder: String; Recursive: Boolean; Relative: Boolean
  ): TStringList;
var
  Ext: String;
  Path: String;
begin
  Result:= TStringList.Create;
  Path:= ExtractFilePath( Folder );
  Ext:= ExtractFileExt( Folder );
  FindAllFiles( Result, Ext, Path, Recursive, Relative );
end;

end.

