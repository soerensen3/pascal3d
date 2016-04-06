{$IFDEF INTERFACE}
type

  { TP3DScene }

  TP3DScene = class( TP3DDataBlock )
    private
      FCam: TP3DCamera;
      FObjects: TP3DActorList;

      procedure SetCam(AValue: TP3DCamera);

    public
      constructor Create( const AData: TP3DData = nil );
      constructor Create( AFileName: String; const AWatchFileForChanges: Boolean = False; const AData: TP3DData = nil );
      constructor CreateFromDOM( ADOMNode: TDOMElement; const AData: TP3DData = nil );
      destructor Destroy; override;

      procedure Clear;

//      function Debug: String;

      procedure Render; reintroduce;
      procedure AppendFromDom(ADOMNode: TDOMElement;
        const AParent: TP3DActor = nil; const AutoNames: Boolean=False);
      procedure AppendFile( AFileName: String; const AParent: TP3DActor = nil; const AutoNames: Boolean = False );
      procedure PassToShader( world: TMat4 ); virtual;
      procedure NotifyDeleteOfData(Sender: TP3DDataBlock); override;

      property Objects: TP3DActorList read FObjects;
      property Cam: TP3DCamera read FCam write SetCam;
  end;
  TP3DSceneList = specialize gP3DCustomDatablockList < TP3DScene >;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DScene }

procedure TP3DScene.SetCam(AValue: TP3DCamera);
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
end;

constructor TP3DScene.Create(AFileName: String;
  const AWatchFileForChanges: Boolean; const AData: TP3DData);
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  inherited Create( AData );
  AppendFile( AFileName );
end;

constructor TP3DScene.CreateFromDOM(ADOMNode: TDOMElement; const AData: TP3DData );
begin
  if ( not Assigned( ADOMNode )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Not Assigned!' );

  if ( not ( ADOMNode.NodeName = 'p3dscene' )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Scenes must begin with p3dscene!' );

  Create( AData );

  AppendFromDom( ADOMNode );
end;

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

{
function TP3DScene.Debug: String;
var
  Indent: Integer;

  function DebugList( Items: TP3DActorList ): String;
    function WriteVertices( Mdl: TP3DMesh ): String;
    var
      i: Integer;
    begin
      Result:= StringOfChar( ' ', Indent * 2 ) + 'vertices' + #13#10;

      Inc( Indent );

      for i:= 0 to Mdl.Positions.Count - 1 do
        Result += StringOfChar( ' ', Indent * 2 ) +
          Format( '%9.4f, %9.4f, %9.4f;', [ Mdl.Positions[ i ].X, Mdl.Positions[ i ].Y, Mdl.Positions[ i ].Z ])
          + #13#10;

      Dec( Indent );

      Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
    end;

    function WriteNormals( Mdl: TP3DMesh ): String;
    var
      i: Integer;
    begin
      Result:= StringOfChar( ' ', Indent * 2 ) + 'normals' + #13#10;

      Inc( Indent );

      for i:= 0 to Mdl.Normals.Count - 1 do
        Result += StringOfChar( ' ', Indent * 2 ) +
          Format( '%9.4f, %9.4f, %9.4f;', [ Mdl.Normals[ i ].X, Mdl.Normals[ i ].Y, Mdl.Normals[ i ].Z ])
          + #13#10;

      Dec( Indent );

      Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
    end;

    function WriteFaces( Mdl: TP3DMesh ): String;
    var
      i: Integer;
      s: String;
      j: Integer;
      k: Integer;
    begin
      Result:= StringOfChar( ' ', Indent * 2 ) + 'faces' + #13#10;

      Inc( Indent );

      for i:= 0 to high( Mdl.Faces ) do
        begin
          s:= '';

          for j:= 0 to high( Mdl.Faces[ i ].verts ) do
            begin
              s += ', ' + IntToStr( Mdl.Faces[ i ].verts[ j ].v ) + '/' + IntToStr( Mdl.Faces[ i ].verts[ j ].n ) + '/';
              for k:= 0 to High( Mdl.Faces[ i ].verts[ j ].texc ) do
                begin
                  if ( k > 0 ) then
                    s += '|';
                  s += IntToStr( Mdl.Faces[ i ].verts[ j ].texc[ k ]);
                end;
            end;


          Delete( s, 1, 2 );
          Result += StringOfChar( ' ', Indent * 2 ) + s + #13#10;
        end;

      Dec( Indent );

      Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
    end;

  var
    i: Integer;
  begin
    Result:= '';
    for i:= 0 to Items.Count - 1 do
      begin
        Result += StringOfChar( ' ', Indent * 2 ) + 'object ' + Items[ i ].Name +  #13#10;
        Inc( Indent );
        Result += WriteVertices( Items[ i ] as TP3DMesh );
        Result += WriteNormals( Items[ i ] as TP3DMesh );
        Result += WriteFaces( Items[ i ] as TP3DMesh );
        Result += StringOfChar( ' ', Indent * 2 ) + 'children' + #13#10;
        Inc( Indent );
        Result += DebugList( ( Items[ i ] as TP3DMesh ).Children );
        Dec( Indent );
        Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
        Dec( Indent );
        Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
      end;
  end;

begin
  Indent:= 0;
  Result:= DebugList( Children );
end;
}
procedure TP3DScene.Render;
begin
  Objects.Render( Mat4Identity, Self );
end;

//TODO: FIXIT
procedure TP3DScene.AppendFromDom(ADOMNode: TDOMElement;
  const AParent: TP3DActor; const AutoNames: Boolean = False );
var
  Element: TDOMElement;
  Parent: TP3DActorList;
  lst: TDOMNodeList;
  i: Integer;

  procedure LoadData();
    function CreateObject: Integer;
    var
      DB: TP3DDataBlock;
    begin
      Result:= -1;
      case Element.NodeName of
        'mesh': begin DB:= TP3DMesh.Create( P3DData ); P3DData.Meshes.Add( TP3DMesh( DB )); end;
        'material': begin DB:= TP3DMaterial.Create( P3DData ); P3DData.Materials.Add( TP3DMaterial( DB )); end;
        'light': begin DB:= TP3DLight.Create( P3DData ); P3DData.Lights.Add( TP3DLight( DB )); end;
      end;
      Result:= P3DData.DataBlocks.IndexOf( DB );
    end;

  var
    AName: DOMString;
    Idx: Integer;
  begin
    AName:= Element.GetAttribute( 'name' );
    Idx:= P3DData.DataBlocks.FindByName( AName );
    WriteLn( 'IDX of Datablock "', AName, '": ', Idx );
    if ( Idx = -1 ) then
      Idx:= CreateObject;
    if ( TP3DDataBlock( P3DData.DataBlocks[ Idx ]).FileWatch.FileName = '<empty>' ) then
      TP3DDataBlock( P3DData.DataBlocks[ Idx ]).LoadFromDOM( Element, P3DData );
    WriteLn( Format( 'Created datablock of type "%s" and name "%s" with index %d', [ Element.NodeName, AName, Idx ]));
  end;

begin
  if ( not Assigned( ADOMNode )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Not Assigned!' );

  if ( not ( ADOMNode.NodeName = 'p3dscene' )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Scenes must begin with p3dscene!' );

  Element:= TDOMElement( ADOMNode.FirstChild );
  if ( Assigned( AParent )) then
    Parent:= AParent.Children
  else
    Parent:= Objects;

  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'object': ; //Postpone the loading of objects
        'mesh',
        'material',
        'light': LoadData()
      else
        WriteLn( Format( 'Warning unsupported type "%s". Skipping.', [ Element.NodeName ]));
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
  lst:= ADOMNode.GetElementsByTagName( 'object' );
  for i:= 0 to lst.Count - 1 do
    Parent.Add( TP3DActor.CreateFromDOM( P3DData.DataBlocks, TDOMElement( lst[ i ]), P3DData ));
end;

procedure TP3DScene.AppendFile(AFileName: String;
  const AParent: TP3DActor; const AutoNames: Boolean);
var
  xml: TXMLDocument;
  dir: String;
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  ReadXMLFile( xml, AFileName );
  dir:= GetCurrentDir;
  try
    SetCurrentDir( ExtractFilePath( AFileName ));
    AppendFromDom( xml.DocumentElement, AParent, AutoNames );
  finally
    SetCurrentDir( dir );
    xml.Free;
  end;
end;

procedure TP3DScene.PassToShader(world: TMat4);
begin
  P3DData.Lights.PassToActiveShader( 0, Self, Mat4Identity, Cam.View, [ liPosition, liLightParams ]);
end;

procedure TP3DScene.NotifyDeleteOfData(Sender: TP3DDataBlock);
begin
  inherited NotifyDeleteOfData( Sender );
  if ( Sender = Cam ) then
    Cam:= nil;
end;

{$ENDIF}
