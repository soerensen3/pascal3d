{$IFDEF INTERFACE}
type

  { TP3DScene }

  TP3DScene = class( TPersistent )
    private
      FFileWatch: TFileWatch;
      FModelList: TRenderableObjectList;
      FMaterials: TP3DMaterialList;

    public
      constructor Create();
      constructor Create( AFileName: String; const AWatchFileForChanges: Boolean = False );
      constructor CreateFromDOM( ADOMNode: TDOMElement );
      destructor Destroy; override;

      procedure Clear;

      function Debug: String;

      procedure Render( world: TMat4; const RenderFlag: TRenderFlags = []);

      property Children: TRenderableObjectList read FModelList;
      property Materials: TP3DMaterialList read FMaterials;
      property FileWatch: TFileWatch read FFileWatch write FFileWatch;
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{ TP3DScene }

constructor TP3DScene.Create;
begin
  inherited Create();
  FModelList:= TRenderableObjectList.Create;
  FMaterials:= TP3DMaterialList.Create;
end;

constructor TP3DScene.Create( AFileName: String;
  const AWatchFileForChanges: Boolean);
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
    CreateFromDOM( xml.DocumentElement );
  finally
    SetCurrentDir( dir );
    xml.Free;
  end;
end;

constructor TP3DScene.CreateFromDOM( ADOMNode: TDOMElement );
var
  Element: TDOMElement;

  procedure LoadMaterial;
  var
    AName: DOMString;
    matIdx: Integer;
  begin
    AName:= Element.GetAttribute( 'name' );
    matIdx:= Materials.FindByName( AName );
    if ( matIdx = -1 ) then
      matIdx:= Materials.Add( TP3DMaterial.Create );
    Materials[ matIdx ].LoadFromDOM( Element, Self );
  end;

begin
  if ( not Assigned( ADOMNode )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Not Assigned!' );

  if ( not ( ADOMNode.NodeName = 'p3dscene' )) then
    raise Exception.Create( ClassName + ': Cannot create from DOM Node. Scenes must begin with p3dscene!' );

  Create();

  Element:= TDOMElement( ADOMNode.FirstChild );
  while ( Assigned( Element )) do
    begin
      case Element.NodeName of
        'mesh': Children.Add( TP3DMesh.CreateFromDOM( Children, Self, Element ));
        'material': LoadMaterial;
      end;
      Element:= TDOMElement( Element.NextSibling );
    end;
end;

destructor TP3DScene.Destroy;
begin
  Clear;

  FModelList.Free;
  FMaterials.Free;
  inherited Destroy;
end;

procedure TP3DScene.Clear;
begin
  FModelList.Clear;
end;

function TP3DScene.Debug: String;
var
  Indent: Integer;

  function DebugList( Items: TRenderableObjectList ): String;
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

procedure TP3DScene.Render(world: TMat4; const RenderFlag: TRenderFlags);
begin
  Children.Render( world, RenderFlag );
end;
{$ENDIF}
