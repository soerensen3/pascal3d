//p3d.core.shadernodes_markdown.inc
{$IFDEF INTERFACE}
{$INCLUDE p3d.core.shadernodes_markdown_utils.inc}

function ParseMarkdown( S: String; LibOwner: IP3DStreamableContainer ): TP3DShaderNodeLibrary;
procedure ParseMarkdownAppend( S: String; Lib: TP3DShaderNodeLibrary );
function ParseMarkdownFile( FN: String; LibOwner: IP3DStreamableContainer ): TP3DShaderNodeLibrary;
procedure ParseMarkdownFileAppend( FN: String; Lib: TP3DShaderNodeLibrary );


{$ENDIF}

{$IFDEF IMPLEMENTATION}



{$INCLUDE p3d.core.shadernodes_markdown_utils.inc}
{$INCLUDE p3d.core.shadernodes_markdown_parser.inc}



function ParseMarkdown( S: String; LibOwner: IP3DStreamableContainer ): TP3DShaderNodeLibrary;
begin
  GlobalFile:= S;

  GlobalLibrary:= TP3DShaderNodeLibrary.Create( LibOwner );
  GlobalMarkdown:= TP3DMarkdownFile.Create( GlobalLibrary.Files );
  GlobalMarkdown.FilePointer.FileName:= GlobalFileName;
  Result:= GlobalLibrary;

  Parse;
end;

procedure ParseMarkdownAppend(S: String; Lib: TP3DShaderNodeLibrary);
begin
  GlobalMarkdown:= TP3DMarkdownFile.Create( Lib.Files );
  GlobalMarkdown.FilePointer.FileName:= GlobalFileName;
  GlobalFile:= S;

  GlobalLibrary:= Lib;

  Parse;
end;

function ParseMarkdownFile( FN: String; LibOwner: IP3DStreamableContainer ): TP3DShaderNodeLibrary;
var
  F: TStringList;
  FileName: String;
begin
  FileName:= P3DSearchPaths.FindFileEx( FN, GetCurrentDir());
  if ( FileName = '' ) then
    raise Exception.Create( Format( 'Error while parsing markdown file. The file "%f" could not be found', [ FN ]));
  GlobalFileName:= FileName;
  F:= TStringlist.Create;
  F.LoadFromFile( FileName );
  Result:= ParseMarkdown( F.Text, LibOwner );
  F.Free;
end;

procedure ParseMarkdownFileAppend(FN: String; Lib: TP3DShaderNodeLibrary);
var
  F: TStringList;
  FileName: String;
begin
  FileName:= P3DSearchPaths.FindFileEx( FN, GetCurrentDir());
  if ( FileName = '' ) then
    raise Exception.Create( Format( 'Error while parsing markdown file. The file "%f" could not be found', [ FN ]));
  GlobalFileName:= FileName;
  F:= TStringlist.Create;
  F.LoadFromFile( FileName );
  ParseMarkdownAppend( F.Text, Lib );
  F.Free;
end;
{$ENDIF}
