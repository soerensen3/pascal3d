//p3dscene.inc
//part of p3dgraphics.pas
{$IFDEF INTERFACE}
type

  { TP3DScene }

  TP3DScene = class( TP3DDataBlock )
    private
      FCam: TP3DActor;
      FObjects: TP3DActorList;

      procedure SetCam(AValue: TP3DActor);

    public
      constructor Create( const AData: TP3DData = nil );
      constructor Create( AFileName: String; const AWatchFileForChanges: Boolean = False; const AData: TP3DData = nil );
      //constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil );
      destructor Destroy; override;

      procedure Clear;

//      function Debug: String;

      procedure Render; reintroduce;
      //procedure AppendFromDom(ADOMNode: TDOMElement; const AParentActor: TP3DActor = nil;
      //  const AutoNames: Boolean=False);
      procedure AppendFile( AFileName: String; const AParent: TP3DActor = nil; const AutoNames: Boolean = False );
      procedure AppendFromData( AData: TP3DData );
      procedure PassToShader( world: TMat4 ); virtual;
      procedure NotifyDeleteOfData(Sender: TP3DDataBlock); override;

      property Objects: TP3DActorList read FObjects;
      property Cam: TP3DActor read FCam write SetCam;
  end;
  TP3DSceneList = specialize gP3DCustomDatablockList < TP3DScene >;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DScene }

procedure TP3DScene.SetCam(AValue: TP3DActor);
begin
  if FCam=AValue then Exit;

  if ( Assigned( FCam )) then
    FCam.Users.Delete( FCam.Users.IndexOf( Self ));

  FCam:= AValue;

  if ( Assigned( FCam )) then
    FCam.Users.Add( Self );
end;

constructor TP3DScene.Create(const AData: TP3DData);
begin
  inherited Create( AData );
  FObjects:= TP3DActorList.Create;
  Parent.Scenes.Add( Self );
end;

constructor TP3DScene.Create(AFileName: String;
  const AWatchFileForChanges: Boolean; const AData: TP3DData);
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  inherited Create( AData );
  AppendFile( AFileName );
end;
{
constructor TP3DScene.CreateFromDOM(ADOMNode: TDOMElement; const AData: TP3DData );
begin
  if ( not Assigned( ADOMNode )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Not Assigned!' );

  if ( not ( ADOMNode.NodeName = 'p3dscene' )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Scenes must begin with p3dscene!' );

  Create( AData );

  AppendFromDom( ADOMNode );
end;}

destructor TP3DScene.Destroy;
begin
  Clear;
  Objects.Free;
  inherited Destroy;
end;

procedure TP3DScene.Clear;
begin
  Objects.Clear( True );
end;

procedure TP3DScene.Render;
begin
  TP3DCamera( Cam.Data ).View:= mat4translate( vec4( Cam.Position, 1 )) * mat4( mat3( Cam.Quaternion ));
                   //TODO:SHOULD BE INVERTED
  Objects.Render( Mat4Identity, Self );
end;

procedure TP3DScene.AppendFile(AFileName: String;
  const AParent: TP3DActor; const AutoNames: Boolean);
var
  xml: TXMLDocument;
  dir: String;
  n: Integer;
begin
{  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  ReadXMLFile( xml, AFileName );
  dir:= GetCurrentDir;}
  try
    //SetCurrentDir( ExtractFilePath( AFileName ));
    n:= OpenLibrary( AFileName );
    AppendFromData( P3DData.Libraries[ n ]);
    //AppendFromDom( xml.DocumentElement, AParent, AutoNames );
  finally
{    SetCurrentDir( dir );
    xml.Free;}
  end;
end;

procedure TP3DScene.AppendFromData(AData: TP3DData);
var
  Obj: TP3DActor;
begin
  for Obj in AData.Objects do
    begin
      Objects.Add( Obj );
      Obj.Users.Add( Self );
    end;
end;

procedure TP3DScene.PassToShader(world: TMat4);
var
  LightList: TP3DLightList;
  Obj: TP3DActor;
  decl: TP3DShaderDecl;
  matCam: TMat4;
begin
  if ( Assigned( Cam )) then
    begin
      LightList:= TP3DLightList.Create;
      for Obj in Objects do
        if ( Obj.Data is TP3DLight ) then
          LightList.Add( TP3DLight( Obj.Data ));
      if ( Cam.Data is TP3DCamera ) then
        begin
          TP3DCamera( Cam.Data ).PassToShader( world );
          LightList.PassToActiveShader( 0, Self, world, TP3DCamera( Cam.Data ).View, [ liPosition, liLightParams ]);
        end;
      if ( P3DShaderActive.Uniforms.FindByName( 'world', decl )) then
        decl.AsMat4:= world;
      LightList.Free;
    end;
end;

procedure TP3DScene.NotifyDeleteOfData(Sender: TP3DDataBlock);
begin
  inherited NotifyDeleteOfData( Sender );
  if ( Sender = Cam ) then
    Cam:= nil;
end;

{$ENDIF}
