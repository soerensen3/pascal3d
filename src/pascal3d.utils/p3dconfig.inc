{$IFDEF INTERFACE}
type
  { TP3DConfig }

  TP3DConfig = class ( TP3DJSONRoot )
    public
      procedure AddStreamable( APropName: String; AObj: TP3DStreamable );
      procedure RemoveStreamable( APropName: String );

      procedure SaveConfig( FileName: String );
      procedure LoadConfig( FileName: String );
  end;

  TP3DStreamablePointerConfig = specialize gP3DPropAccessStreamable < TP3DStreamable >;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

procedure TP3DConfig.AddStreamable(APropName: String; AObj: TP3DStreamable);
var
  cfg: TP3DStreamablePointerConfig;
begin
  //Prop:= tp3dpropaccess
  if ( not Assigned( AObj )) then
    exit;
  cfg:= TP3DStreamablePointerConfig.Create( APropName );
  Properties.Add( cfg );
  cfg.AsValue:= AObj;
end;

procedure TP3DConfig.RemoveStreamable(APropName: String);
var
  n: Integer;
begin
  n:= Properties.FindByName( APropName );
  if ( n > -1 ) then
    Properties.Delete( n );
end;

procedure TP3DConfig.SaveConfig(FileName: String);
begin
  FileName:= CreateAbsolutePath( FileName, ProgramDirectory );
  JSONLoader.FileName:= FileName;
  JSONLoader.Root:= Self;
  JSONLoader.WriteFile;
end;

procedure TP3DConfig.LoadConfig(FileName: String);
begin
  FileName:= CreateAbsolutePath( FileName, ProgramDirectory );
  JSONLoader.FileName:= FileName;
  JSONLoader.Root:= Self;
  JSONLoader.ReadFile;
  JSONLoader.ReadJSON;
end;

{$ENDIF}
