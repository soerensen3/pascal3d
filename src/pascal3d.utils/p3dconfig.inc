{$IFDEF INTERFACE}
type
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

  TP3DPropertyAccessConfig = specialize gP3DListPropertyAccessInterfacedPersistent < TP3DInterfacedPersistentList, TP3DInterfacedPersistent, TP3DPropertyAccessInterfacedPersistent_NoCreate >;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

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
begin
  FileName:= CreateAbsolutePath( FileName, ProgramDirectory );
  loader:= TP3DJSONLoader.Create( FileName, Self );
  loader.ReadFile;
  loader.Free;
end;

{$ENDIF}
