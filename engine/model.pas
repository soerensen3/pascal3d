unit Model;

{$mode objfpc}{$H+}

{$DEFINE VERBOSE}

interface
uses
  Classes, SysUtils, dglOpenGL, Math3D, strutils, shaders, zglHeader;

type

  { TMaterial }

  TMaterial = class
    Diff_Map: Integer;
    Amb: TVec3;
    Diff: TVec3;
    Spec: TVec3;
    alpha: Real;
    Name: String;

    constructor Create;
  end;

  TFaceVertex = record
    v, n: Integer;
    texc: array of Integer;
  end;

  TFace = record
    verts: array of TFaceVertex;
    mat: TMaterial;
  end;

  TVec2Array = array of TVec2;
  TVec3Array = array of TVec3;

  TFaceArray = array of TFace;
//  TMaterialArray = array of TMaterial;
  TModel = class;


  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomModelList}
  {$DEFINE TCustomItem:= TModel}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}

  {$DEFINE TCustomList:= TCustomMaterialList}
  {$DEFINE TCustomItem:= TMaterial}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}

  { TMaterialList }

  TMaterialList = class( TCustomMaterialList )
    public
      function FindByName( Name: String ): Integer;
      procedure Clear; override;
  end;

  { TModelList }

  TModelList = class( TCustomModelList )
    public
      procedure Clear; override;
      procedure Render( world, view, proj: TMat4 );
  end;

  { TModel }
  TModel = class( TPersistent )
    private
      FChildren: TModelList;

    public
      Vertices: TVec3Array;
      Normals: TVec3Array;
      Faces: TFaceArray;
      TexCoords: TVec2Array;
      Name: String;
      Matrix: TMat4;
      Material: TMaterial;
      testTex: Integer;
//      glBuffer: GLuint;
//      glIBuffer: GLuint;
//      glVertexArray: GLuint;
//      glIndexArray: GLuint;
//     BoundingBox: TBoundingBox;

//      constructor Create( FName: String );
      constructor Create;
      destructor Destroy; override;

      procedure Render( world, view, proj: TMat4 );
      procedure ClearChildren;

      property Children: TModelList read FChildren;
  end;

  { TModelFile }

  TModelFile = class( TPersistent )
    private
      FModelList: TModelList;
      FMaterials: TMaterialList;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Clear;

      function Debug: String;

      procedure Render( world, view, proj: TMat4 );

      property Children: TModelList read FModelList;
      property Materials: TMaterialList read FMaterials;
  end;

function LoadModelFile( F: TStringList ): TModelFile;
function LoadModelFileFromFile( FName: String ): TModelFile;
function LoadObject( MdlFile: TModelFile; var Mdl: TModel; F: TStringList; const strpos: Integer ): Integer;
function LoadMaterial( Mat: TMaterial; F: TStringList; strpos: Integer ): Integer;

implementation



function RemoveComment( S: String ): String;
var
  n: Integer;
begin
  n:= Pos( '//', S );
  if ( n > 0 ) then
    Result:= Copy( S, 1, n - 1 )
  else
    Result:= S;
end;

function GetCmd( S: String ): String;
begin
  Result:= ExtractWord( 1, lowercase( Trim( RemoveComment( S ))), [ ' ' ]);
end;

function GetParams( S: String ): String;
var
  n: Integer;
begin
  S:= Trim( RemoveComment( S ));
  n:= WordPosition( 2, S, [ ' ' ]);
  Result:= Copy( S, n, Length( S ) - n + 1 );
end;


function LoadMatrix( var Matrix: TMat4; F: TStringList; const strpos: Integer ): Integer;
  function ParseMatrix( S: String ): TMat4;
  var
    i, n: Integer;
  begin
    n:= WordCount( S, [ ',' ]);
    if ( n <> 16 ) then
      WriteLn( Format( 'Error in LoadMatrix [%d]: Wrong number of values specified. Expected 16; Found %d!', [ i, n ]))
    else
      begin
        S:= ExtractWord( 1, S, [ ';' ]);
        for i:= 0 to 15 do
          begin
            Result[ i ]:= StrToFloat( Trim( ExtractWord( i + 1, S, [ ',' ])));
          end;
      end;
  end;

var
  i: Integer;
  cmd: String;
  cat: String;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERYVERBOSE}
      writeln( 'LoadMatrix[', i, ']' );
      {$ENDIF}
      case cmd of
        'end;':
          begin
            Matrix:= ParseMatrix( cat );
            break;
          end
        else
          begin
            cat+= F[ i ];
            Inc( i );
          end;
      end;
    end;
  Result:= i + 1;
end;

function LoadUV( var UVs: TVec2Array; F: TStringList; const strpos: Integer ): Integer;
  function ParseUV( S: String ): TVec2;
  var
    i, n: Integer;
  begin
    n:= WordCount( S, [ ',' ]);
    if ( n <> 2 ) then
      WriteLn( Format( 'Error in LoadUV [%d]: Wrong number of values specified. Expected 2; Found %d!', [ i, n ]))
    else
      begin
        S:= ExtractWord( 1, S, [ ';' ]);
        for i:= 0 to 1 do
          Result[ i ]:= StrToFloat( Trim( ExtractWord( i + 1, S, [ ',' ])));
      end;
  end;
var
  i: Integer;
  cmd: String;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERYVERBOSE}
      writeln( 'LoadUV[', i, ']' );
      {$ENDIF}
      case cmd of
        'end;': break;
        else
          begin
            SetLength( UVs, Length( UVs ) + 1 );
            UVs[ high( UVs )]:= ParseUV( F[ i ]);
            inc( i );
          end;// Loading Vertices
      end;
    end;
  Result:= i + 1;
end;

function ParseVertex( S: String ): TVec3;
var
  i, n: Integer;
begin
  n:= WordCount( S, [ ',' ]);
  if ( n <> 3 ) then
    WriteLn( Format( 'Error in LoadVertex [%d]: Wrong number of values specified. Expected 3; Found %d!', [ i, n ]))
  else
    begin
      S:= ExtractWord( 1, S, [ ';' ]);
      for i:= 0 to 2 do
        Result.Coord[ i ]:= StrToFloat( Trim( ExtractWord( i + 1, S, [ ',' ])));
    end;
end;

function LoadVertices( var Vertices: TVec3Array; F: TStringList; const strpos: Integer ): Integer;
var
  i: Integer;
  cmd: String;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERBOSE}
      writeln( 'LoadVertices[', i, ']' );
      {$ENDIF}
      case cmd of
        'end;': break;
        else
          begin
            SetLength( Vertices, Length( Vertices ) + 1 );
            Vertices[ high( Vertices )]:= ParseVertex( F[ i ]);
            inc( i );
          end;// Loading Vertices
      end;
    end;
  Result:= i + 1;
end;

function LoadVerticesBin( var Vertices: TVec3Array; F: TFileStream; count: Integer ): Integer;
var
  i: Integer;
  base: Integer;
begin
  base:= Length( Vertices );
  SetLength( Vertices, base + count );
  for i:= 0 to count - 1 do
    begin
      F.Read( Vertices[ i + base ].Ptr^, 3*SizeOf( Single ));
    end;
end;

function LoadUVBin( var UVs: TVec2Array; F: TFileStream; count: Integer ): Integer;
var
  i: Integer;
  base: Integer;
begin
  base:= Length( UVs );
  SetLength( UVs, base + count );
  for i:= 0 to count - 1 do
    begin
      F.Read( UVs[ i + base ].Ptr^, 2 * SizeOf( Single ));
    end;
end;

function LoadFacesBin( var Faces: TFaceArray; F: TFileStream; count: Integer ): Integer;
var
  i: Integer;
  base: Integer;
  c, tc: Integer;
  j: Integer;
  k: Integer;
begin
  base:= Length( Faces );
  SetLength( Faces, base + count );
  for i:= 0 to count - 1 do
    begin
      F.Read( c, SizeOf( c ));
      F.Read( tc, SizeOf( tc ));

      SetLength( Faces[ i + base ].verts, c );
      for j:= 0 to c - 1 do
        begin
          SetLength( Faces[ i + base ].verts[ j ].texc, tc );
          F.Read( Faces[ i + base ].verts[ j ].v, SizeOf( Integer ));
          F.Read( Faces[ i + base ].verts[ j ].n, SizeOf( Integer ));
          for k:= 0 to tc - 1 do
            F.Read( Faces[ i + base ].verts[ j ].texc[ k ], SizeOf( Integer ));
        end;
    end;
end;

function LoadChildren( MdlFile: TModelFile; Children: TModelList; F: TStringList; const strpos: Integer ): Integer;
var
  i: Integer;
  cmd: String;
  Mdl: TModel;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERBOSE}
      writeln( 'LoadChildren[', i, ']' );
      {$ENDIF}
      case cmd of
        'object':
          begin
            Mdl:= TModel.Create;
            Children.Add( Mdl );
            Mdl.Name:= GetParams( F[ i ]);
            i:= LoadObject( MdlFile, Mdl, F, i + 1 );
          end;
        'end;': break;
        else
          begin
            inc( i );
          end;// Loading Vertices
      end;
    end;
  Result:= i + 1;
end;
{
function LoadMaterials( var Materials: TMaterialArray; F: TStringList; const strpos: Integer ): Integer;
  function ParseMaterial( S: String; var i: Integer ): TMaterial; inline;
  var
    cmd: String;
  begin
    while( i < F.Count ) do
      begin
        cmd:= GetCmd( F[ i ]);
        {$IFDEF VERYVERBOSE}
        writeln( 'LoadMaterials[', i, ']' );
        {$ENDIF}
        case cmd of
          'end;': break;
          else
            begin
              inc( i );
            end;
        end;
      end;
  end;
var
  i: Integer;
  cmd: String;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERYVERBOSE}
      writeln( 'LoadMaterials[', i, ']' );
      {$ENDIF}
      case cmd of
        'end;': break;
        else
          begin
            SetLength( Materials, Length( Materials ) + 1 );
            Materials[ high( Materials )]:= ParseMaterial( F[ i ], i );
            inc( i );
          end;// Loading Vertices
      end;
    end;
  Result:= i + 1;
end;
}

function LoadFaces( var Faces: TFaceArray; F: TStringList; const strpos: Integer ): Integer;
  function ParseFace( S: String ): TFace;
  var
    i, n: Integer;
    chunk: String;
    uv: String;
    uvcount: Integer;
    j: Integer;
  begin
    S:= Trim( ExtractWord( 1, S, [ ';' ]));
//    Result.mat:= StrToInt( ExtractWord( 1, S, [ ' ' ])); //FEHLER?
//    n:= WordPosition( 2, S, [ ' ' ]);
//    S:= Copy( S, n, Length( S ) - n + 1 );
    n:= WordCount( S, [ ',' ]);

    SetLength( Result.verts, n );
    for i:= 1 to n do
      begin
        chunk:= Trim( ExtractWord( i, S, [ ',' ]));
        Result.verts[ i - 1 ].v:= StrToInt( Trim( ExtractWord( 1, chunk, [ '/' ])));
        Result.verts[ i - 1 ].n:= StrToInt( Trim( ExtractWord( 2, chunk, [ '/' ])));
        uv:= Trim(ExtractWord( 3, chunk, [ '/' ]));
        uvcount:= WordCount( uv, [ '|' ]);
        SetLength( Result.verts[ i - 1 ].texc, uvcount );
        for j:= 0 to uvcount - 1 do
          Result.verts[ i - 1 ].texc[ j ]:= StrToInt( ExtractWord( j + 1, uv, [ '|' ]));
      end;
  end;
var
  i: Integer;
  cmd: String;
  s: String;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERYVERBOSE}
      writeln( 'LoadFaces[', i, ']' );
      {$ENDIF}
      case cmd of
        'end;':
          break;
        else
          begin
            SetLength( Faces, Length( Faces ) + 1 );
            s:= F[ i ];
            Faces[ high( Faces )]:= ParseFace( s );
            inc( i );
          end;
      end;
    end;
  Result:= i + 1;
end;

function LoadExternalObjectFile( var Mdl: TModel; fname: String; F: TStringList; const strpos: Integer ): Integer;
var
  i: Integer;
  cmd: String;
  bin: TFileStream;
  param: String;
begin
  i:= strpos;
  if ( not FileExists( fname )) then
    begin
       WriteLn( Format( 'Error in LoadExternalObjectFile [%d]: External model file not found "%s"', [ i, fname ]));
       exit;
    end;
  bin:= TFileStream.Create( fname, fmOpenRead );

  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERYVERBOSE}
      writeln( 'LoadExternalObjectFile[', i, ']' );
      {$ENDIF}
      param:= GetParams( F[ i ]);
      case cmd of
        'end;':
          break;
        'vertices': LoadVerticesBin( Mdl.Vertices, bin, StrToInt( param ));
        'normals': LoadVerticesBin( Mdl.Normals, bin, StrToInt( param ));
        'texcoords': LoadUVBin( Mdl.TexCoords, bin, StrToInt( param ));
        'faces': LoadFacesBin( Mdl.Faces, bin, StrToInt( param ));
      else
        begin
          WriteLn( Format( 'Error in LoadExternalObjectFile [%d]: unknown token "%s"', [ i, cmd ]));
        end;
      end;
      i:= i + 1;
    end;
  bin.Free;
  Result:= i + 1;
end;

function LoadObjMaterials( MdlFile: TModelFile; var Mdl: TModel; F: TStringList; const strpos: Integer ): Integer;
var
  i: Integer;
  cmd: String;
  param: String;
  Material: TMaterial;
  MatIdx: Integer;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      case cmd of
        'end;': break;
        'material':
          begin
            param:= ExtractWord( 1, GetParams( F[ i ]), [ '''' ]);
            MatIdx:= MdlFile.Materials.FindByName( param );
            if ( MatIdx >= 0 ) then
              Material:= MdlFile.Materials[ MatIdx ]
            else
              begin
                Material:= TMaterial.Create;
                Material.Name:= param;
                MdlFile.Materials.Add( Material );
              end;
            Mdl.Material:= Material;
          end
        else
          begin
            WriteLn( Format( 'Error in LoadObject [%d]: unknown token "%s"', [ i, cmd ]));
          end;
      end;
      i:= i + 1;
    end;
  Result:= i + 1;
end;

function LoadObject( MdlFile: TModelFile; var Mdl: TModel; F: TStringList; const strpos: Integer ): Integer;
var
  i: Integer;
  cmd, param: String;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERBOSE}
      writeln( 'LoadObject[', i, ']' );
      {$ENDIF}
      case cmd of
        'end;': break;
        'matrix': i:= LoadMatrix( Mdl.Matrix, F, i + 1 );
        'vertices': i:= LoadVertices( Mdl.Vertices, F, i + 1 );
        'normals': i:= LoadVertices( Mdl.Normals, F, i + 1 );
        'faces': i:= LoadFaces( Mdl.Faces, F, i + 1 );
//        'materials': i:= LoadMaterials( Mdl.Materials, F, i + 1 );
        'texcoords': i:= LoadUV( Mdl.TexCoords, F, i + 1 );
        'children': i:= LoadChildren( MdlFile, Mdl.Children, F, i + 1 );
        'materials': i:= LoadObjMaterials( MdlFile, Mdl, F, i + 1 );
        'external':
          begin
            param:= ExtractWord( 1, GetParams( F[ i ]), [ '''' ]);
            i:= LoadExternalObjectFile( Mdl, param, F, i + 1 );
          end
        else
          begin
            WriteLn( Format( 'Error in LoadObject [%d]: unknown token "%s"', [ i, cmd ]));
            i:= i + 1;
          end;
      end;
    end;
  Result:= i + 1;
end;

function LoadMaterial( Mat: TMaterial; F: TStringList; strpos: Integer ): Integer;
var
  i: Integer;
  cmd: String;
  param: String;
  tex: zglPTexture;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      case cmd of
        'ambient':
          begin
            Mat.Amb:= ParseVertex( GetParams( F[ i ]));
          end;
        'diffuse':
          begin
            Mat.Diff:= ParseVertex( GetParams( F[ i ]));
          end;
        'specular':
          begin
            Mat.Spec:= ParseVertex( GetParams( F[ i ]));
          end;
        'texture':
          begin
            param:= ExtractWord( 1, GetParams( F[ i ]), [ '''' ]);
            if ( not FileExists( param )) then
               WriteLn( 'Error: The specified texture "', param, '" could not be found!' )
            else
              begin
                tex:= tex_LoadFromFile( Param, TEX_NO_COLORKEY, TEX_CLAMP or TEX_FILTER_LINEAR or TEX_CALCULATE_ALPHA );
                if ( Assigned( tex )) then
                  Mat.Diff_Map:= tex^.ID
                else
                  Mat.Diff_Map:= -1;
              end;
          end;
        'end;':
          begin
            break;
          end;
      end;
      i:= i + 1;
    end;
  Result:= i + 1;
end;

function LoadModelFile(F: TStringList): TModelFile;
var
  i: Integer;
  cmd: String;
  Mdl: TModel;
  MatName: String;
  MatIdx: Integer;
  Material: TMaterial;
begin
  DecimalSeparator:= '.';
  i:= 0;
  Result:= TModelFile.Create;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      case cmd of
        'object':
          begin
            Mdl:= TModel.Create;
            Result.Children.Add( Mdl );
            Mdl.Name:= GetParams( F[ i ]);
            i:= LoadObject( Result, Mdl, F, i + 1 );
          end;
        'material':
          begin
            MatName:= ExtractWord( 1, GetParams( F[ i ]), [ '''' ]);
            MatIdx:= Result.Materials.FindByName( MatName );

            if ( MatIdx >= 0 ) then
              begin
                Material:= Result.Materials[ MatIdx ];
              end
            else
              begin
                Material:= TMaterial.Create;
                Material.Name:= ExtractWord( 1, GetParams( F[ i ]), [ '''' ]);
                Result.Materials.Add( Material );
              end;
            i:= LoadMaterial( Material, F, i + 1 );
          end
        else
          begin
            WriteLn( Format( 'Error in LoadModelFile [%d]: unknown token "%s"', [ i, cmd ]));
            i:= i + 1;
          end;
      end;
    end;
end;

function LoadModelFileFromFile(FName: String): TModelFile;
var
  F: TStringList;
begin
  if ( FileExists( FName )) then
    begin
      F:= TStringList.Create;
      F.LoadFromFile( FName );
      Result:= LoadModelFile( F );
      F.Free;
    end
  else
    raise Exception.Create( Format( 'TModel.Create: File %s not found', [ FName ]));
end;

{ TMaterial }

constructor TMaterial.Create;
begin
  inherited;
  Diff_Map:= -1;
end;

{ TMaterialList }

function TMaterialList.FindByName(Name: String): Integer;
var
  i: Integer;
begin
  Result:= -1;

  for i:= 0 to Count - 1 do
    if ( Items[ i ].Name = Name ) then
      begin
        Result:= i;
        break;
      end;
end;

procedure TMaterialList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

{ TModelList }

procedure TModelList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

procedure TModelList.Render(world, view, proj: TMat4);
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Render( world, view, proj );
end;

{ TModelFile }

constructor TModelFile.Create;
begin
  inherited;

  FModelList:= TModelList.Create;
  FMaterials:= TMaterialList.Create;
end;

destructor TModelFile.Destroy;
begin
  Clear;

  FModelList.Free;
  FMaterials.Free;
  inherited Destroy;
end;

procedure TModelFile.Clear;
begin
  FModelList.Clear;
end;

function TModelFile.Debug: String;
var
  Indent: Integer;

  function DebugList( Items: TModelList ): String;
    function WriteVertices( Mdl: TModel ): String;
    var
      i: Integer;
    begin
      Result:= StringOfChar( ' ', Indent * 2 ) + 'vertices' + #13#10;

      Inc( Indent );

      for i:= 0 to high( Mdl.Vertices ) do
        Result += StringOfChar( ' ', Indent * 2 ) +
          Format( '%9.4f, %9.4f, %9.4f;', [ Mdl.Vertices[ i ].X, Mdl.Vertices[ i ].Y, Mdl.Vertices[ i ].Z ])
          + #13#10;

      Dec( Indent );

      Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
    end;

    function WriteNormals( Mdl: TModel ): String;
    var
      i: Integer;
    begin
      Result:= StringOfChar( ' ', Indent * 2 ) + 'normals' + #13#10;

      Inc( Indent );

      for i:= 0 to high( Mdl.Normals ) do
        Result += StringOfChar( ' ', Indent * 2 ) +
          Format( '%9.4f, %9.4f, %9.4f;', [ Mdl.Normals[ i ].X, Mdl.Normals[ i ].Y, Mdl.Normals[ i ].Z ])
          + #13#10;

      Dec( Indent );

      Result += StringOfChar( ' ', Indent * 2 ) + 'end;' + #13#10;
    end;

    function WriteFaces( Mdl: TModel ): String;
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
        Result += WriteVertices( Items[ i ]);
        Result += WriteNormals( Items[ i ]);
        Result += WriteFaces( Items[ i ]);
        Result += StringOfChar( ' ', Indent * 2 ) + 'children' + #13#10;
        Inc( Indent );
        Result += DebugList( Items[ i ].Children );
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

procedure TModelFile.Render(world, view, proj: TMat4);
begin
  Children.Render( world, view, proj );
end;

{ TModel }

procedure TModel.ClearChildren;
begin
  FChildren.Clear;
end;

constructor TModel.Create;
var
  vertexLoc: GLuint;
begin
  inherited;
  FChildren:= TModelList.Create;
{  glGenBuffers( 1, @glBuffer );

  glGenVertexArrays(1, @glVertexArray );
  glBindVertexArray( glVertexArray );

  glGenBuffers( 1, @glIBuffer );}
end;

destructor TModel.Destroy;
begin
  ClearChildren;
  FChildren.Free;
  inherited Destroy;
end;

procedure TModel.Render( world, view, proj: TMat4 );
var
  i: Integer;
  j: Integer;
  matId: Integer;
  vNorm, Vec2: TVec3;
  _world: TMat4;
  k: Integer;
  normal, vertex, color: GLint;
begin
  for i:= 0 to high( Faces ) do
    begin
(*      // bind buffer for positions and copy data into buffer
      // GL_ARRAY_BUFFER is the buffer type we use to feed attributes
      glBindBuffer( GL_ARRAY_BUFFER, glBuffer );

      // feed the buffer, and let OpenGL know that we don't plan to
      // change it (STATIC) and that it will be used for drawing (DRAW)
      glBufferData( GL_ARRAY_BUFFER, sizeof( Vertices ), @Vertices[ 0 ], GL_STATIC_DRAW );

      // Enable the attribute at that location
      glEnableVertexAttribArray( glGetAttribLocation( ActiveShader, 'vertex' ) );
//      glEnableVertexAttribArray( glGetAttribLocation( ActiveShader, 'normal' ) );
      // Tell OpenGL what the array contains:
      // it is a set of 4 floats for each vertex
      glVertexAttribPointer( glGetAttribLocation( ActiveShader, 'vertex' ), 4, GL_FLOAT, False, 0, nil );
//      glVertexAttribPointer( glGetAttribLocation( ActiveShader, 'normal' ), 4, GL_FLOAT, False, 0, nil );*)

      if ( Assigned( Material )) then
        begin
//          glColor3f( Material.Diff.x, Material.Diff.y, Material.Diff.z );
          color:= glGetAttribLocation( ActiveShader, 'in_color');
          glVertexAttrib4f( ActShad.Attributes.AddrByName( 'in_color' ),
            Material.Diff.R, Material.Diff.G, Material.Diff.B, 1.0 );

          if ( Material.Diff_Map > -1 ) then
            begin
              glActiveTexture( GL_TEXTURE0 );
              glEnable( GL_TEXTURE_2D );
              glBindTexture( GL_TEXTURE_2D, Material.Diff_Map );
              glUniform1i( ActShad.Uniforms.AddrByName( 'tex0'), 0 );

              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
            end
          else
            begin
              glActiveTexture( GL_TEXTURE0 );
              {$IFDEF DEFAULT_TEX}
              glEnable( GL_TEXTURE_2D );
              glBindTexture( GL_TEXTURE_2D, DefaultTex^.ID );
              {$ENDIF}
              glDisable( GL_TEXTURE_2D );
              glBindTexture( GL_TEXTURE_2D, 0 );
            end;
        end
      else
        begin
          glColor3f( 1.0, 1.0, 1.0 );
          glActiveTexture( GL_TEXTURE0 );
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, testTex );
          glActiveTexture( GL_TEXTURE1 );
          glEnable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, testTex );
//          glDisable( GL_TEXTURE_2D );
//          glBindTexture( GL_TEXTURE_2D, 0 );
        end;
      _world:= world * Matrix;

      ShaderSetParameter4fv( ActiveShader, 'world', _world );

      glBegin( GL_TRIANGLE_FAN );

      for j:= 0 to high( Faces[ i ].verts ) do
        begin
          // bind buffer for positions and copy data into buffer
          //glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, glIBuffer );
          //glBufferData( GL_ELEMENT_ARRAY_BUFFER, SizeOf( Faces[ i ].verts ), @Faces[ i ].verts[ 0 ], GL_STATIC_DRAW);
//          glDrawElements(GL_TRIANGLE_FAN, Length( Faces[ i ].verts ), GL_UNSIGNED_INT, @Faces[ i ].verts[ 0 ]);

//          if ( Length( Faces[ i ].verts[ j ].texc ) > 0 ) then
//            for k:= 0 to high( Faces[ i ].verts[ j ].texc ) do
//              glMultiTexCoord2f( GL_TEXTURE0 + k, TexCoords[ Faces[ i ].verts[ j ].texc[ k ]].S, TexCoords[ Faces[ i ].verts[ j ].texc[ k ]].T );
          vertex:= glGetAttribLocation( ActiveShader, 'in_vertex' );
          glVertexAttrib4f( ActShad.Attributes.AddrByName( 'in_vertex' ), //glGetAttribLocation( ActiveShader, 'in_vertex' ),
                      vertices[ faces[ i ].verts[ j ].v ].x,
                      vertices[ faces[ i ].verts[ j ].v ].y,
                      vertices[ faces[ i ].verts[ j ].v ].z, 1 );

          normal:= glGetAttribLocation( ActiveShader, 'in_normal' );
          glVertexAttrib3f( ActShad.Attributes.AddrByName( 'in_normal' ),//glGetAttribLocation( ActiveShader, 'in_normal' ),
                      normals[ faces[ i ].verts[ j ].n ].x,
                      normals[ faces[ i ].verts[ j ].n ].y,
                      normals[ faces[ i ].verts[ j ].n ].z );
//          glNormal3f( normals[ faces[ i ].verts[ j ].n ].x,
//                      normals[ faces[ i ].verts[ j ].n ].y,
//                      normals[ faces[ i ].verts[ j ].n ].z );
          k:= 0;
          if ( Length( Faces[ i ].verts[ j ].texc ) > 0 ) then
//            for k:= 0 to high( Faces[ i ].verts[ j ].texc ) do
              glVertexAttrib2f( ActShad.Attributes.AddrByName( 'in_texc' + IntToStr( k )),
                TexCoords[ Faces[ i ].verts[ j ].texc[ k ]].S, TexCoords[ Faces[ i ].verts[ j ].texc[ k ]].T );
{          glVertex3f( vertices[ faces[ i ].verts[ j ].v ].x,
                      vertices[ faces[ i ].verts[ j ].v ].y,
                      vertices[ faces[ i ].verts[ j ].v ].z );}
        end;

      glEnd();
    end;
  Children.Render( _world, view, proj );
  ShaderSetParameter4fv( ActiveShader, 'world', _world );
end;

{$DEFINE TCustomList:= TCustomModelList}
{$DEFINE TCustomItem:= TModel}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}

{$DEFINE TCustomList:= TCustomMaterialList}
{$DEFINE TCustomItem:= TMaterial}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}


end.

