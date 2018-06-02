program p3dfontbmp;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
  {$R *.res}
{$ENDIF}

uses
  strutils,
  LCLIntf,
  sysutils,
  Classes,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  Interfaces, //crashes in linux without
  {$ENDIF}{$ENDIF}
  p3d.events,
  p3d.core,
  p3d.utils,
  p3d.ui,
  SDL2,
  main;

{type

  { TP3DConfig }

  TP3DConfig = class ( TP3DInterfacedPersistent )
    private
      FConfig: TP3DInterfacedPersistentList;

    public
      constructor Create;
      destructor Destroy; override;
      procedure SaveConfig( FileName: String );
      procedure LoadConfig( FileName: String );

    published
      property Config: TP3DInterfacedPersistentList read FConfig write FConfig;
  end;

  { TP3DPropertyAccessConfigItem }

  TP3DPropertyAccessConfigItem = class ( TP3DPropertyAccessInterfacedPersistent )
    procedure ValueCreateNew( ClTp: TP3DInterfacedPersistentType; AContext: TP3DJSONContext ); override;
  end;

  TP3DPropertyAccessConfig = specialize gP3DListPropertyAccessInterfacedPersistent < TP3DInterfacedPersistentList, TP3DInterfacedPersistent, TP3DPropertyAccessInterfacedPersistent_NoCreate >;



var
  config: TP3DConfig;}

{ TP3DPropertyAccessConfigItem }
  {

procedure TP3DPropertyAccessConfigItem.ValueCreateNew(ClTp: TP3DInterfacedPersistentType; AContext: TP3DJSONContext);
begin
  WriteLn( 'Wurst' );
end;

constructor TP3DConfig.Create;
begin
  inherited Create;
  Config:= TP3DInterfacedPersistentList.Create;
  Properties.Add( TP3DPropertyAccessConfig.CreateField( 'Config', @FConfig, smAttribute ));
end;

destructor TP3DConfig.Destroy;
begin
  Config.Free;
  inherited Destroy;
end;

procedure TP3DConfig.SaveConfig(FileName: String);
var
  loader: TP3DJSONLoader;
begin
  loader:= TP3DJSONLoader.Create( FileName, Self );
  loader.WriteFile;
  loader.Free;
end;

procedure TP3DConfig.LoadConfig(FileName: String);
var
  loader: TP3DJSONLoader;
  i: Integer;
begin
  loader:= TP3DJSONLoader.Create( FileName, Self );
  //for i:= 0 to Config.Count - 1 do
  //  Config[ i ].Find;
  loader.ReadFile;
  loader.Free;
end;
}
Begin
  try
    //Initialize all the Pascal3D subsystems
    P3DUtilsInit;
    P3DEventsInit;
    P3DCoreInit;

    //config:= TP3DConfig.Create;
    //Create the application from your class
    P3DApplication:= TP3DFontBitmap.Create;

    //Load the config which for example contains paths for assets and shaders and setups the main window
    //P3DApplication.LoadConfig( 'settings_default.xml' );

    {config.Config.Add( P3DApplication );
    config.Config.Add( P3DLog );
    config.Config.Add( P3DSearchPaths );
    config.Config.Add( P3DShaderNodeLib );
    //P3DShaderNodeLib.Paths.Add( '../../shaders' );
    //config.SaveConfig( 'settings_default.p3d' );}
    //config.LoadConfig( 'settings_default.p3d' );
    P3DConfig.LoadConfig( 'settings_default.p3d' );

    P3DUIInit;

    //Initialize everything and run the application
    P3DApplication.Initialize;
    //config.Free;
    if ( P3DApplication.MainWindow.Visible ) then
      P3DApplication.Run;

  except
    On E: Exception do
      P3DLog.LogException( nil, E ); // On Exception we add a message to the log.
      //This is saved to a html file after each message. The logpath is in the config.
  end;
  //Finalize Pascal3D subsystems
  //Note that this is optional (finalization section of each unit will call these functions
  //anyway) but this way we have more control in which order they are called
  P3DUIFinish;
  P3DCoreFinish;
  P3DEventsFinish;
  P3DUtilsFinish;
End.
