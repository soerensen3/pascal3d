unit p3dfileutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, p3dgenerics;

type

  { TP3DFileWatch }

  TP3DFileWatch = class;

  TP3DFileWatchChange = procedure ( Sender: TP3DFileWatch; out DoReload: Boolean ) of object;
  TP3DFileWatch = class ( TPersistent )
    private
      FLastFileAge: LongInt;
      FFileName: String;
      FOnFileChange: TP3DFileWatchChange;
      FUserPointer: Pointer;

      function GetFileAge: LongInt;
      procedure SetFileName(AValue: String);

    public
      constructor Create( AFileName: String; AUserPointer: Pointer = nil );

      function CheckForChange: Boolean;
      procedure Reload;

      property UserPointer: Pointer read FUserPointer write FUserPointer;

    published
      property FileName: String read FFileName write SetFileName;
      property LastFileAge: LongInt read FLastFileAge;
      property OnFileChange: TP3DFileWatchChange read FOnFileChange write FOnFileChange;
  end;

  TP3DFileWatchList = class( specialize gP3DCustomObjectList < TP3DFileWatch > )
    function AddWatch( AFileName: String; AUserPointer: Pointer = nil ): Integer;

    procedure CheckForChange;
  end;

  function P3DListFolderFiles( Folder: String; Recursive: Boolean; Relative: Boolean ): TStringList;

var
  FileWatches: TP3DFileWatchList;


implementation

{ TP3DFileWatchList }

function TP3DFileWatchList.AddWatch(AFileName: String; AUserPointer: Pointer
  ): Integer;
begin
  Result:= Add( TP3DFileWatch.Create( AFileName, AUserPointer ));
end;

procedure TP3DFileWatchList.CheckForChange;
var
  Watch: TP3DFileWatch;
  Reload: Boolean;
begin
  for Watch in Self do
    if ( Watch.CheckForChange ) then
      if ( Assigned( Watch.OnFileChange )) then
        begin
          Watch.OnFileChange( Watch, Reload );
          if ( Reload ) then
            Watch.Reload;
        end;
end;


{ TP3DFileWatch }

function TP3DFileWatch.GetFileAge: LongInt;
begin
  Result:= FileAge( FFileName );
end;

procedure TP3DFileWatch.SetFileName(AValue: String);
begin
  if ( FFileName = AValue ) then
    Exit;

  FFileName:= AValue;
  Reload;
end;

constructor TP3DFileWatch.Create(AFileName: String; AUserPointer: Pointer);
begin
  inherited Create;
  FFileName:= AFileName;
  Reload;
end;

function TP3DFileWatch.CheckForChange: Boolean;
begin
  Result:= ( GetFileAge <> FLastFileAge );
end;

procedure TP3DFileWatch.Reload;
begin
  FLastFileAge:= GetFileAge;
end;

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

function P3DListFolderFiles( Folder: String; Recursive: Boolean; Relative: Boolean ): TStringList;
var
  Ext: String;
  Path: String;
begin
  Result:= TStringList.Create;
  Path:= ExtractFilePath( Folder );
  Ext:= ExtractFileExt( Folder );
  FindAllFiles( Result, Ext, Path, Recursive, Relative );
end;


initialization
  FileWatches:= TP3DFileWatchList.Create;

finalization
  FileWatches.Free;

end.

