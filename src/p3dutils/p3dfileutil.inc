//p3dfileutil.inc
//part of p3dutils.pas

{$IFDEF INTERFACE}

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

  { TP3DSearchPathCategory }

  TP3DSearchPaths = class;

  TP3DSearchPathCategory = ( p3dsc_Models, p3dsc_Shaders, p3dsc_Fonts );

  { TP3DSearchPathContainer }

  TP3DSearchPathContainer = class( TStringList )
    private
      FCategory: TP3DSearchPathCategory;
      FSearchPaths: TP3DSearchPaths;

    protected
      property SearchPaths: TP3DSearchPaths read FSearchPaths write FSearchPaths;

    public
      constructor Create( ASearchPaths: TP3DSearchPaths );

      function FindFile( FName: String ): String;
      function FindFileEx( FName: String; PrefferedPath: String ): String;

    published
      property Category: TP3DSearchPathCategory read FCategory write FCategory;
  end;

  { TP3DSearchPaths }

  TP3DSearchPaths = class ( TPersistent )
    private
      FBaseDir: String;
      SearchPaths: array [ p3dsc_Models..p3dsc_Fonts ] of TP3DSearchPathContainer;

      function GetItem( Index: TP3DSearchPathCategory ): TP3DSearchPathContainer;
      procedure SetBaseDir(AValue: String);
      procedure SetItem( Index: TP3DSearchPathCategory ; AValue: TP3DSearchPathContainer );

    public
      constructor Create;
      destructor Destroy; override;

      property Items[ Index: TP3DSearchPathCategory ]: TP3DSearchPathContainer
        read GetItem write SetItem; default;

    published
      property BaseDir: String read FBaseDir write SetBaseDir;
      property Fonts: TP3DSearchPathContainer read Searchpaths[ p3dsc_Fonts ] write Searchpaths[ p3dsc_Fonts ];
      property Models: TP3DSearchPathContainer read Searchpaths[ p3dsc_Models ] write Searchpaths[ p3dsc_Models ];
      property Shaders: TP3DSearchPathContainer read Searchpaths[ p3dsc_Shaders ] write Searchpaths[ p3dsc_Shaders ];
  end;

  function P3DListFolderFiles( Folder: String; Recursive: Boolean; Relative: Boolean ): TStringList;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

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

{ TP3DSearchPathCategory }

constructor TP3DSearchPathContainer.Create(ASearchPaths: TP3DSearchPaths);
begin
  inherited Create;
  SearchPaths:= ASearchPaths;
end;

function TP3DSearchPathContainer.FindFile(FName: String): String;
begin
  Result:= FindFileEx( FName, '' );
end;

function TP3DSearchPathContainer.FindFileEx( FName: String; PrefferedPath: String ): String;
var
  i: Integer;
  BasePath, FileName: String;
begin
  Result:= '';
  if ( FilenameIsAbsolute( FName ) and FileExistsUTF8( FName )) then
    begin
      Result:= FName;
      exit;
    end;
  if ( PrefferedPath > '' ) then
    begin
      FileName:= CleanAndExpandDirectory( AppendPathDelim( PrefferedPath )) + FName;
      if ( FileExists( FileName )) then
        begin
          Result:= FileName;
          exit;
        end;
    end;
  BasePath:= SearchPaths.BaseDir;
  for i:= Count - 1 downto 0 do
    begin
      FileName:= CleanAndExpandDirectory( AppendPathDelim( BasePath ) + Self[ i ]) + FName;
      if ( FileExists( FileName )) then
        begin
          Result:= FileName;
          break;
        end;
    end;
end;

{ TP3DSearchPaths }

function TP3DSearchPaths.GetItem(Index: TP3DSearchPathCategory): TP3DSearchPathContainer;
begin
  Result:= SearchPaths[ Index ];
end;

procedure TP3DSearchPaths.SetBaseDir(AValue: String);
begin
  if FBaseDir= AValue then Exit;
  FBaseDir:= AppendPathDelim( ExpandFileName( AValue ));
end;

procedure TP3DSearchPaths.SetItem(Index: TP3DSearchPathCategory; AValue: TP3DSearchPathContainer);
begin
  SearchPaths[ Index ]:= AValue;
end;

constructor TP3DSearchPaths.Create;
var
  Item: TP3DSearchPathCategory;
begin
  inherited;
  for Item in TP3DSearchPathCategory do
    Items[ Item ]:= TP3DSearchPathContainer.Create( Self );
end;

destructor TP3DSearchPaths.Destroy;
var
  Item: TP3DSearchPathCategory;
begin
  for Item in TP3DSearchPathCategory do
    Items[ Item ].Free;
  inherited Destroy;
end;



{$ENDIF}

