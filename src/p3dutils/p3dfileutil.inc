//p3dfileutil.inc
//part of p3dutils.pas

{$IFDEF INTERFACE}

type

  { TP3DFileWatch }

  TP3DFileWatch = class;

  TP3DFileWatchOnFileChanged = procedure ( Sender: TP3DFileWatch; out DoReload: Boolean ) of object;
  TP3DFileWatchOnFileNameChanged = procedure ( Sender: TP3DFileWatch; ANewValue: String ) of object;
  TP3DFileWatch = class ( TPersistent )
    private
      FLastFileAge: LongInt;
      FFileName: String;
      FOnFileChange: TP3DFileWatchOnFileChanged;
      FOnFileNameChange: TP3DFileWatchOnFileNameChanged;
      FUserPointer: Pointer;

      function GetFileAge: LongInt;
      procedure SetFileName(AValue: String);

    public
      constructor Create( AFileName: String; AUserPointer: Pointer = nil );
      destructor Destroy; override;

      function CheckForChange: Boolean;
      procedure Reload;

      property UserPointer: Pointer read FUserPointer write FUserPointer;

    published
      property FileName: String read FFileName write SetFileName;
      property LastFileAge: LongInt read FLastFileAge;
      property OnFileChange: TP3DFileWatchOnFileChanged read FOnFileChange write FOnFileChange;
      property OnFileNameChange: TP3DFileWatchOnFileNameChanged read FOnFileNameChange write FOnFileNameChange;
  end;

  TP3DFileWatchList = class( specialize gP3DCustomObjectList < TP3DFileWatch > )
    function AddWatch( AFileName: String; AUserPointer: Pointer = nil ): Integer;

    procedure CheckForChange;
  end;


  { TP3DSearchPathContainer }

  TP3DSearchPathContainer = class( TP3DInterfacedPersistent )
    private
      FBaseDir: String;
      FPaths: TStringList;

      procedure SetBaseDir(AValue: String);
      function GetFullPath( Index: Integer ): String;

    public
      constructor Create;
      destructor Destroy; override;

      function FindFile( FName: String ): String;
      function FindFileEx( FName: String; PreferedPath: String ): String;

      procedure LoadFromDOMNew(ADOMNode: TDOMElement); override;
      class function DOMNodeName: String; override;

      property FullPath[ Index: Integer ]: String read GetFullPath;
      property Paths: TStringList read FPaths write FPaths;

    published
      property BaseDir: String read FBaseDir write SetBaseDir;
  end;

  function P3DListFolderFiles( Folder: String; Recursive: Boolean; Relative: Boolean ): TStringList;
  function P3DListFolderDirectories( Folder: String; Recursive: Boolean; Relative: Boolean ): TStringList;
  procedure P3DLoadConfig( FileName: String );
  function P3DMatchFileName( FileName: String; Mask: String ): Boolean;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DFileWatchList }

function TP3DFileWatchList.AddWatch( AFileName: String; AUserPointer: Pointer ): Integer;
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
var
  FN, FName: String;
begin
  if ( FFileName = AValue ) then
    Exit;

  FN:= AValue;
  if ( FN > '' ) then
    begin
      FName:= P3DSearchPaths.FindFileEx( AValue, GetCurrentDir ); //ExpandFileName( AValue );
      if ( FileExists( FName )) then
        FN:= FName;
    end;
  if ( Assigned( OnFileNameChange )) then
    OnFileNameChange( Self, FN );
  FFileName:= FN;
  Reload;
end;

constructor TP3DFileWatch.Create(AFileName: String; AUserPointer: Pointer);
begin
  inherited Create;
  FFileName:= AFileName;
  Reload;

  if ( Assigned( P3DFileWatch )) then
    P3DFileWatch.Add( Self );

  //Add Self to FileWatches
  //TP3DData has Filewatch
  //TP3DData has also a LibName which should be unique in Scope
  //Scope means the engine's searchpaths
  //TP3DDatablock has only a string to the corresponding object, <LibName> or similar will reference a different library
  //Descendants, like textures or meshes, can define their own filewatch if needed
end;

destructor TP3DFileWatch.Destroy;
begin
  if ( Assigned( P3DFileWatch )) then
    P3DFileWatch.Remove( Self );
  inherited Destroy;
end;

function TP3DFileWatch.CheckForChange: Boolean;
begin
  Result:= ( GetFileAge <> FLastFileAge );
end;

procedure TP3DFileWatch.Reload;
begin
  FLastFileAge:= GetFileAge;
end;

procedure FindAllFiles( output: TStrings; ext: String; RootFolder: string; Recurse: Boolean = True;
    Relative: Boolean = True );

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

            if ( P3DMatchFileName( SR.Name, ext )) then
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

procedure FindAllDirectories( output: TStrings; RootFolder: string; Recurse: Boolean = True;
    Relative: Boolean = True );
var
  SR: TSearchRec;
  fileext: String;
begin
  //if ( Relative ) then
  //  RootFolder:= ExtractRelativepath( RootFolder, GetCurrentDir );
  RootFolder := IncludeTrailingPathDelimiter(RootFolder);
  if ( FindFirst(RootFolder + '*', faAnyFile or faDirectory, SR) = 0 ) then
    try
      repeat
        if SR.Attr and faDirectory = faDirectory then
          // --> ein Verzeichnis wurde gefunden
          // der Verzeichnisname steht in SR.Name
          // der vollständige Verzeichnisname (inkl. darüberliegender Pfade) ist
          // RootFolder + SR.Name
          if (SR.Name <> '.') and (SR.Name <> '..') then
            begin
              output.Add(( SR.Name ));
              if ( Recurse ) then
                FindAllDirectories( output, RootFolder + SR.Name, Recurse, Relative );
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
  Ext:= ExtractFilename( Folder );
  FindAllFiles( Result, Ext, Path, Recursive, Relative );
  Result.Sort;
end;

function P3DListFolderDirectories( Folder: String; Recursive: Boolean; Relative: Boolean ): TStringList;
var
  Ext: String;
  Path: String;
begin
  Result:= TStringList.Create;
  Path:= ExtractFilePath( Folder );
  FindAllDirectories( Result, Path, Recursive, Relative );
  Result.Sort;
end;

procedure ReadGeneral( BaseNode: TDOMElement );
var
  rootdir: DOMString;
begin
  rootdir:= BaseNode.GetAttribute( 'rootdir' );
  if ( rootdir > '' ) then
    P3DSearchPaths.BaseDir:= rootdir;
end;

procedure ReadPaths( BaseNode: TDOMElement );
var
  Path: TDOMElement;
  sptype, value: DOMString;
begin
  Path:= TDOMElement( BaseNode.FirstChild );
  while ( Assigned( Path )) do
    begin
      sptype:= Path.GetAttribute( 'type' );
      value:= Path.GetAttribute( 'value' );
      P3DSearchPaths.Paths.Add( value );
      Path:= TDOMElement( Path.NextSibling );
    end;
end;

procedure ReadWindow( BaseNode: TDOMElement );
var
  Wnd: TDOMElement;
begin
  Wnd:= TDOMElement( BaseNode.FirstChild );
  //p3dapplication. Wnd.GetAttribute( 'title' );
end;

procedure P3DLoadConfig( FileName: String );
var
  F: TXMLDocument;
  BaseNode: TDOMNode;
  DocRoot: TDOMElement;
begin
  F:= nil;
  if ( not FileExists( FileName )) then
    raise Exception.Create( Format( 'Cannot load config. The file "%s" was not found!', [ FileName ]));
  try
    ReadXMLFile( F, FileName );
    DocRoot:= F.DocumentElement;
    BaseNode:= DocRoot.FindNode( 'general' );
    if ( Assigned( BaseNode )) then
      ReadGeneral( TDOMElement( BaseNode ));
    BaseNode:= DocRoot.FindNode( 'paths' );
    if ( Assigned( BaseNode )) then
      ReadPaths( TDOMElement( BaseNode ));
    BaseNode:= DocRoot.FindNode( 'window' );
    if ( Assigned( BaseNode )) then
      ReadWindow( TDOMElement( BaseNode ));

  finally
    FreeAndNil( F );
  end;

end;

function P3DMatchFileName(FileName: String; Mask: String): Boolean; //Credit to https://github.com/alrieckert/lazarus/blob/master/components/codetools/fileprocs.pas

const
  SpecialChar = '#';

  function FilenameIsMatching(const Mask, Filename: string; MatchExactly: boolean ): boolean; inline;
  (*
    check if Filename matches Mask
    if MatchExactly then the complete Filename must match, else only the
    start
    Filename matches exactly or is a file/directory in a subdirectory of mask.
    Mask can contain the wildcards * and ? and the set operator {,}.
    The wildcards will *not* match PathDelim.
    You can nest the {} sets.
    If you need the asterisk, the question mark or the PathDelim as character
    just put the SpecialChar character in front of it (e.g. #*, #? #/).
    Examples:
      /abc             matches /abc, /abc/, /abc/p, /abc/xyz/filename
                       but not /abcd
      /abc/            matches /abc, /abc/, /abc//, but not /abc/.
      /abc/x?z/www     matches /abc/xyz/www, /abc/xaz/www
                       but not /abc/x/z/www
      /abc/x*z/www     matches /abc/xz/www, /abc/xyz/www, /abc/xAAAz/www
                       but not /abc/x/z/www
      /abc/x#*z/www    matches /abc/x*z/www, /abc/x*z/www/ttt
      /a{b,c,d}e       matches /abe, /ace, /ade
      *.p{as,p,}       matches a.pas, unit1.pp, b.p but not b.inc
      *.{p{as,p,},inc} matches a.pas, unit1.pp, b.p, b.inc but not c.lfm
  *)
  {off $DEFINE VerboseFilenameIsMatching}

    function Check(MaskP, FileP: PChar): boolean;
    var
      Level: Integer;
      MaskStart: PChar;
      FileStart: PChar;
    begin
      {$IFDEF VerboseFilenameIsMatching}
      debugln(['  Check Mask="',MaskP,'" FileP="',FileP,'"']);
      {$ENDIF}
      Result:=false;
      repeat
        case MaskP^ of
        #0:
          begin
            // the whole Mask fits the start of Filename
            // trailing PathDelim in FileP are ok
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check END Mask="',MaskP,'" FileP="',FileP,'"']);
            {$ENDIF}
            if FileP^=#0 then exit(true);
            if FileP^<>PathDelim then exit(false);
            while FileP^=PathDelim do inc(FileP);
            Result:=(FileP^=#0) or (not MatchExactly);
            exit;
          end;
        SpecialChar:
          begin
            // match on character
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check specialchar Mask="',MaskP,'" FileP="',FileP,'"']);
            {$ENDIF}
            inc(MaskP);
            if MaskP^=#0 then exit;
            if MaskP^<>FileP^ then exit;
            inc(MaskP);
            inc(FileP);
          end;
        PathDelim:
          begin
            // match PathDelim(s) or end of filename
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check PathDelim Mask="',MaskP,'" FileP="',FileP,'"']);
            {$ENDIF}
            if not (FileP^ in [#0,PathDelim]) then exit;
            // treat several PathDelim as one
            while MaskP^=PathDelim do inc(MaskP);
            while FileP^=PathDelim do inc(FileP);
            if MaskP^=#0 then
              exit((FileP^=#0) or not MatchExactly);
          end;
        '?':
          begin
            // match any one character, but PathDelim
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check any one char Mask="',MaskP,'" FileP="',FileP,'"']);
            {$ENDIF}
            if FileP^ in [#0,PathDelim] then exit;
            inc(MaskP);
            inc(FileP,LazUTF8.UTF8CharacterLength(FileP));
          end;
        '*':
          begin
            // match 0 or more characters, but PathDelim
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check any chars Mask="',MaskP,'" FileP="',FileP,'"']);
            {$ENDIF}
            while MaskP^='*' do inc(MaskP);
            repeat
              if Check(MaskP,FileP) then exit(true);
              if FileP^ in [#0,PathDelim] then exit;
              inc(FileP);
            until false;
          end;
        '{':
          begin
            // OR options separated by comma
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check { Mask="',MaskP,'" FileP="',FileP,'"']);
            {$ENDIF}
            inc(MaskP);
            repeat
              if Check(MaskP,FileP) then begin
                {$IFDEF VerboseFilenameIsMatching}
                debugln(['  Check { option fits -> end']);
                {$ENDIF}
                exit(true);
              end;
              {$IFDEF VerboseFilenameIsMatching}
              debugln(['  Check { skip to next option ...']);
              {$ENDIF}
              // skip to next option in MaskP
              Level:=1;
              repeat
                case MaskP^ of
                #0: exit;
                SpecialChar:
                  begin
                    inc(MaskP);
                    if MaskP^=#0 then exit;
                    inc(MaskP);
                  end;
                '{': inc(Level);
                '}':
                  begin
                    dec(Level);
                    if Level=0 then exit; // no option fits
                  end;
                ',':
                  if Level=1 then break;
                end;
                inc(MaskP);
              until false;
              {$IFDEF VerboseFilenameIsMatching}
              debugln(['  Check { next option: "',MaskP,'"']);
              {$ENDIF}
              inc(MaskP)
            until false;
          end;
        '}':
          begin
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check } Mask="',MaskP,'" FileP="',FileP,'"']);
            {$ENDIF}
            inc(MaskP);
          end;
        ',':
          begin
            // OR option fits => continue behind the {}
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check Skipping to end of {} Mask="',MaskP,'" ...']);
            {$ENDIF}
            Level:=1;
            repeat
              inc(MaskP);
              case MaskP^ of
              #0: exit;
              SpecialChar:
                begin
                  inc(MaskP);
                  if MaskP^=#0 then exit;
                  inc(MaskP);
                end;
              '{': inc(Level);
              '}':
                begin
                  dec(Level);
                  if Level=0 then break;
                end;
              end;
            until false;
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check Skipped to end of {} Mask="',MaskP,'"']);
            {$ENDIF}
            inc(MaskP);
          end;
        #128..#255:
          begin
            // match UTF-8 characters
            {$IFDEF VerboseFilenameIsMatching}
            debugln(['  Check UTF-8 chars Mask="',MaskP,'" FileP="',FileP,'"']);
            {$ENDIF}
            MaskStart:=MaskP;
            FileStart:=FileP;
            while not (MaskP^ in [#0,SpecialChar,PathDelim,'?','*','{',',','}']) do
            begin
              if FileP^ in [#0,PathDelim] then exit;
              inc(MaskP,LazUTF8.UTF8CharacterLength(MaskP));
              inc(FileP,LazUTF8.UTF8CharacterLength(FileP));
            end;
            if LazFileUtils.CompareFilenames(MaskStart,MaskP-MaskStart,FileStart,FileP-FileStart)<>0 then
              exit;
          end;
        else
          // match ASCII characters
          repeat
            case MaskP^ of
            #0,SpecialChar,PathDelim,'?','*','{',',','}': break;
            {$IFDEF CaseInsensitiveFilenames}
            'a'..'z','A'..'Z':
              if FPUpChars[MaskP^]<>FPUpChars[FileP^] then exit;
            {$ENDIF}
            else
              if MaskP^<>FileP^ then exit;
            end;
            inc(MaskP);
            inc(FileP);
          until false;
        end;
      until false;
    end;

  begin
    if Filename='' then exit(false);
    if Mask='' then exit(true);
    {$IFDEF VerboseFilenameIsMatching}
    debugln(['FilenameIsMatching2 Mask="',Mask,'" File="',Filename,'" Exactly=',MatchExactly]);
    {$ENDIF}

    Result:=Check(PChar(Mask),PChar(Filename));
  end;
begin
  Result:= FilenameIsMatching( Mask, FileName, False );
end;

{ TP3DSearchPathCategory }

function TP3DSearchPathContainer.GetFullPath( Index: Integer ): String;
begin
  Result:= '';
  if ( InRange( Index, 0, Paths.Count - 1 )) then
    begin
      Result:= AppendPathDelim( Paths[ Index ]);
      if ( not FilenameIsAbsolute( Result )) then
        Result:= AppendPathDelim( BaseDir + Result );
    end;
end;

constructor TP3DSearchPathContainer.Create;
begin
  inherited Create;
  FPaths:= TStringList.Create;
  Properties.Add( TP3DPropertyAccessRTTI.Create( Self, 'BaseDir', smAttribute ));
end;

destructor TP3DSearchPathContainer.Destroy;
begin
  FPaths.Free;
  inherited Destroy;
end;

function TP3DSearchPathContainer.FindFile(FName: String): String;
begin
  Result:= FindFileEx( FName, '' );
end;

function TP3DSearchPathContainer.FindFileEx( FName: String; PreferedPath: String ): String;
var
  i: Integer;
  BasePath, FileName: String;
begin
  Result:= '';
  if ( FName = '' ) then
    exit;
  if ( FilenameIsAbsolute( FName ) and FileExistsUTF8( FName )) then
    begin
      Result:= FName;
      exit;
    end;
  if ( PreferedPath > '' ) then
    begin
      FileName:= CleanAndExpandDirectory( AppendPathDelim( PreferedPath )) + FName;
      if ( FileExists( FileName )) then
        begin
          Result:= FileName;
          exit;
        end;
    end;
  BasePath:= AppendPathDelim( BaseDir );
  FileName:= BasePath + FName;
  if ( FileExists( FileName )) then
    begin
      Result:= FileName;
      exit;
    end;
  for i:= Paths.Count - 1 downto 0 do
    begin
      FileName:= CleanAndExpandDirectory( BasePath + Paths[ i ]) + FName;
      if ( FileExists( FileName ) or DirectoryExists( FileName )) then
        begin
          Result:= FileName;
          break;
        end;
    end;
end;

procedure TP3DSearchPathContainer.LoadFromDOMNew(ADOMNode: TDOMElement);
var
  Path: TDOMElement;
  value: DOMString;
begin
  inherited LoadFromDOMNew(ADOMNode);

  Path:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Path )) do
    if ( Path.NodeName = 'path' ) then
      begin
        value:= Path.GetAttribute( 'value' );
        P3DSearchPaths.Paths.Add( value );
        Path:= TDOMElement( Path.NextSibling );
      end;
end;

class function TP3DSearchPathContainer.DOMNodeName: String;
begin
  Result:= 'paths';
end;

procedure TP3DSearchPathContainer.SetBaseDir(AValue: String);
begin
  if FBaseDir= AValue then Exit;
  FBaseDir:= AppendPathDelim( ExpandFileName( AValue ));
end;



{$ENDIF}

