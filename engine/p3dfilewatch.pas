unit p3dfilewatch;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil;

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

  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomFileWatchList}
  {$DEFINE TCustomListEnumerator:= TP3DFileWatchEnumerator}
  {$DEFINE TCustomItem:= TP3DFileWatch}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  { TP3DFileWatchList }

  TP3DFileWatchList = class( TCustomFileWatchList )
    function AddWatch( AFileName: String; AUserPointer: Pointer = nil ): Integer;

    procedure CheckForChange;
  end;

var
  FileWatches: TP3DFileWatchList;


implementation

{$MACRO ON}
{$DEFINE TCustomList:= TCustomFileWatchList}
{$DEFINE TCustomListEnumerator:= TP3DFileWatchEnumerator}
{$DEFINE TCustomItem:= TP3DFileWatch}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

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


initialization
  FileWatches:= TP3DFileWatchList.Create;

finalization
  FileWatches.Free;

end.

