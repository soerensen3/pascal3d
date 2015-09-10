unit p3dlcleditorfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, FileUtil;

type
  { TP3DEditorFile }

  TP3DEditorFile = class( TPersistent )
    private
      FChanged: Boolean;
      FFileName: String;
      FOnChange: TNotifyEvent;
      FPage: TTabSheet;
      FTitle: String;
    public
      constructor Create( AFileName: String; TabSheet: TTabSheet ); virtual;
      destructor Destroy; override;

      procedure FileIdle(); virtual;
      procedure FileRender(); virtual;
      procedure MakeActive();

    published
      property FileName: String read FFileName write FFileName;
      property Changed: Boolean read FChanged write FChanged;
      property Page: TTabSheet read FPage write FPage;
      property Title: String read FTitle write FTitle;
      property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TP3DEditorFileClass = class of TP3DEditorFile;

  { TP3DEditorFileExtension }

  TP3DEditorFileExtension = class ( TPersistent )
    public
      Ext: String;
      FileClass: TP3DEditorFileClass;
  end;

  {$MACRO ON}
  {$DEFINE TCustomList:= TP3DEditorFileList}
  {$DEFINE TCustomItem:= TP3DEditorFile}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  {$DEFINE TCustomList:= TP3DEditorFileExtensionList}
  {$DEFINE TCustomItem:= TP3DEditorFileExtension}
  {$DEFINE INTERFACE}
  {$INCLUDE p3dcustomlist.inc}

  function RegisterExtension( Ext: String; EditorFile: TP3DEditorFileClass ): Integer;
  function FindExtension( Ext: String ): TP3DEditorFileClass;

implementation

var
  P3DRegisteredExtensions: TP3DEditorFileExtensionList;


{$DEFINE TCustomList:= TP3DEditorFileExtensionList}
{$DEFINE TCustomItem:= TP3DEditorFileExtension}
{$DEFINE OBJECTLIST}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}

{$DEFINE TCustomList:= TP3DEditorFileList}
{$DEFINE TCustomItem:= TP3DEditorFile}
{$DEFINE OBJECTLIST}
{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dcustomlist.inc}


{ TP3DEditorFile }

constructor TP3DEditorFile.Create(AFileName: String; TabSheet: TTabSheet);
begin
  inherited Create;
  FPage:= TabSheet;
  FTitle:= ExtractFileNameOnly( AFileName );
end;

destructor TP3DEditorFile.Destroy;
begin
  inherited Destroy;
end;

procedure TP3DEditorFile.FileIdle;
begin

end;

procedure TP3DEditorFile.FileRender;
begin

end;

procedure TP3DEditorFile.MakeActive;
begin
  if (( Assigned( Page )) and ( Assigned( Page.PageControl ))) then
    Page.PageControl.ActivePage:= Page;
end;


function RegisterExtension(Ext: String; EditorFile: TP3DEditorFileClass
  ): Integer;
var
  E: TP3DEditorFileExtension;
begin
  if ( not Assigned( P3DRegisteredExtensions )) then
    P3DRegisteredExtensions:= TP3DEditorFileExtensionList.Create;
  E:= TP3DEditorFileExtension.Create;
  E.Ext:= LowerCase( Ext );
  E.FileClass:= EditorFile;
  Result:= P3DRegisteredExtensions.Add( E );
end;

function FindExtension(Ext: String): TP3DEditorFileClass;
var
  E: TP3DEditorFileExtension;
begin
  Result:= Nil;
  if ( not Assigned( P3DRegisteredExtensions )) then
    exit;
  for E in P3DRegisteredExtensions do
   if ( LowerCase( Ext ) = E.Ext ) then
     begin
       Result:= E.FileClass;
       break;
     end;
end;

end.

