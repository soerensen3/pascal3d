unit filewatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

type

  { TFileWatch }

  TFileWatch = class;

  TFileWatchChange = procedure ( Sender: TFileWatch; out DoReload: Boolean ) of object;
  TFileWatch = class
    private
      FLastFileAge: LongInt;
      FFileName: String;
      FOnFileChange: TFileWatchChange;
      FUserPointer: Pointer;

      function GetFileAge: LongInt;
    public
      constructor Create( AFileName: String; AUserPointer: Pointer = nil );

      function CheckForChange: Boolean;
      procedure Reload;

      property FileName: String read FFileName;
      property LastFileAge: LongInt read FLastFileAge;
      property OnFileChange: TFileWatchChange read FOnFileChange write FOnFileChange;
      property UserPointer: Pointer read FUserPointer write FUserPointer;
  end;

  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomFileWatchList}
  {$DEFINE TCustomListEnumerator:= TFileWatchEnumerator}
  {$DEFINE TCustomItem:= TFileWatch}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}

  { TFileWatchList }

  TFileWatchList = class( TCustomFileWatchList )
    function AddWatch( AFileName: String; AUserPointer: Pointer = nil ): Integer;

    procedure CheckForChange;
  end;

var
  FileWatches: TFileWatchList;


implementation

{$MACRO ON}
{$DEFINE TCustomList:= TCustomFileWatchList}
{$DEFINE TCustomListEnumerator:= TFileWatchEnumerator}
{$DEFINE TCustomItem:= TFileWatch}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}

{ TFileWatchList }

function TFileWatchList.AddWatch(AFileName: String; AUserPointer: Pointer
  ): Integer;
begin
  Result:= Add( TFileWatch.Create( AFileName, AUserPointer ));
end;

procedure TFileWatchList.CheckForChange;
var
  Watch: TFileWatch;
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


{ TFileWatch }

function TFileWatch.GetFileAge: LongInt;
begin
  Result:= FileAge( FFileName );
end;

constructor TFileWatch.Create(AFileName: String; AUserPointer: Pointer);
begin
  inherited Create;
  FFileName:= AFileName;
  Reload;
end;

function TFileWatch.CheckForChange: Boolean;
begin
  Result:= ( GetFileAge <> FLastFileAge );
end;

procedure TFileWatch.Reload;
begin
  FLastFileAge:= GetFileAge;
end;


initialization
  FileWatches:= TFileWatchList.Create;

finalization
  FileWatches.Free;

end.

