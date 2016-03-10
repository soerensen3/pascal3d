{$IFDEF INTERFACE}
type

  { TP3DModelScene }

  TP3DModelScene = class( TP3DScene )
    private
      FDataBlocks: TP3DObjectList;
      FFileWatch: TFileWatch;
      FLights: TP3DLightList;
      FMeshes: TP3DMeshList;
      FObjects: TP3DRenderableObjectList;
      FMaterials: TP3DMaterialList;

    protected
      property DataBlocks: TP3DObjectList read FDataBlocks write FDataBlocks;

    public
      constructor Create();
      constructor Create( AFileName: String; const AWatchFileForChanges: Boolean = False );
      constructor CreateFromDOM( ADOMNode: TDOMElement );
      destructor Destroy; override;

      procedure Clear;

//      function Debug: String;

      procedure Render; override;
      procedure AppendFromDom(ADOMNode: TDOMElement;
        const AParent: TP3DRenderableObject = nil; const AutoNames: Boolean=False);
      procedure AppendFile( AFileName: String; const AParent: TP3DRenderableObject = nil; const AutoNames: Boolean = False );
      procedure PassToShader( world: TMat4 ); override;

      property Objects: TP3DRenderableObjectList read FObjects;
      property Materials: TP3DMaterialList read FMaterials;
      property FileWatch: TFileWatch read FFileWatch write FFileWatch;
      property Lights: TP3DLightList read FLights write FLights;
      property Meshes: TP3DMeshList read FMeshes write FMeshes;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DModelScene }

constructor TP3DModelScene.Create;
begin
  inherited Create();
  FObjects:= TP3DRenderableObjectList.Create;
  FMaterials:= TP3DMaterialList.Create;
  FLights:= TP3DLightList.Create;
  FMeshes:= TP3DMeshList.Create;
  FDataBlocks:= TP3DObjectList.Create;
end;

constructor TP3DModelScene.Create( AFileName: String;
  const AWatchFileForChanges: Boolean);
begin
  if ( not FileExists( AFileName )) then
    raise Exception.Create( ClassName + ': Cannot load scene from file "' + AFileName + '". The specified file does not exist!' );
  inherited Create;
  AppendFile( AFileName );
end;

constructor TP3DModelScene.CreateFromDOM( ADOMNode: TDOMElement );
begin
  if ( not Assigned( ADOMNode )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Not Assigned!' );

  if ( not ( ADOMNode.NodeName = 'p3dscene' )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Scenes must begin with p3dscene!' );

  Create();

  AppendFromDom( ADOMNode );
end;

destructor TP3DModelScene.Destroy;
begin
  Clear;
  Lights.Free;
  Objects.Free;
  Materials.Free;
  Meshes.Free;
  DataBlocks.Free;
  inherited Destroy;
end;

procedure TP3DModelScene.Clear;
begin
  Objects.Clear( True );
  Lights.Clear( False );
  Materials.Clear( False );
  Meshes.Clear( False );
  DataBlocks.Clear( True );
end;
{
function TP3DModelScene.Debug: String;
var
  Indent: Integer;

  function DebugList( Items: TP3DRenderableObjectList ): String;
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
procedure TP3DModelScene.Render;
begin
  inherited;
  Objects.Render( Mat4Identity, Self );
end;

procedure TP3DModelScene.AppendFromDom(ADOMNode: TDOMElement;
  const AParent: TP3DRenderableObject; const AutoNames: Boolean = False );
var
  Element: TDOMElement;
  Parent: TP3DRenderableObjectList;

  procedure LoadData();
    function CreateObject: Integer;
    var
      DB: TP3DDataBlock;
    begin
      Result:= -1;
      case Element.NodeName of
        'mesh': begin DB:= TP3DMesh.Create( DataBlocks ); Meshes.Add( TP3DMesh( DB )); end;
        'material': begin DB:= TP3DMaterial.Create( DataBlocks ); Materials.Add( TP3DMaterial( DB )); end;
        'light': begin DB:= TP3DLight.Create( DataBlocks ); Lights.Add( TP3DLight( DB )); end;
      end;
      Result:= DataBlocks.IndexOf( DB );
    end;

  var
    AName: DOMString;
    Idx: Integer;
  begin
    AName:= Element.GetAttribute( 'name' );
    Idx:= DataBlocks.FindByName( AName );
    WriteLn( 'IDX of Datablock "', AName, '": ', Idx );
    if ( Idx = -1 ) then
      Idx:= CreateObject;
    if ( TP3DDataBlock( DataBlocks[ Idx ]).FileName = '<empty>' ) then
      TP3DDataBlock( DataBlocks[ Idx ]).LoadFromDOM( Self, Element );
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
        'object': Parent.Add( TP3DRenderableObject.CreateFromDOM( Parent, Self, Element, AutoNames ));
        'mesh',
        'material',
        'light': LoadData()
      else
        WriteLn( Format( 'Warning unsupported type "%s". Skipping.', [ Element.NodeName ]));
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
end;

procedure TP3DModelScene.AppendFile(AFileName: String;
  const AParent: TP3DRenderableObject; const AutoNames: Boolean);
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

procedure TP3DModelScene.PassToShader(world: TMat4);
begin
  inherited PassToShader( world );
  Lights.PassToActiveShader( 0, Self, Mat4Identity, Cam.View, [ liPosition, liLightParams ]);
end;

{$ENDIF}
