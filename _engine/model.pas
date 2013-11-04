unit Model;

{$mode objfpc}{$H+}

{.$DEFINE VERBOSE}

interface
uses
  Classes, SysUtils, dglOpenGL, Math3D, strutils;

type
  TFaceVertex = record
    v, n, texc: Integer;
  end;

  TFace = record
    verts: array of TFaceVertex;
    mat: Integer;
  end;

  TMaterial = record
    Diff_Map: Integer;
    Diff: TVec3;
    Spec: TVec3;
    alpha: Real;
    Name: String;
  end;

  TVec2Array = array of TVec2;
  TVec3Array = array of TVec3;

  TFaceArray = array of TFace;
  TMaterialArray = array of TMaterial;
  TModel = class;


  {$MACRO ON}
  {$DEFINE TCustomList:= TCustomModelList}
  {$DEFINE TCustomItem:= TModel}
  {$DEFINE INTERFACE}
  {$INCLUDE custom_list.inc}

  { TModelList }

  TModelList = class( TCustomModelList )
    public
      procedure Clear; override;
      procedure Render;
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
      Matrix: TMatrix4f;
      Materials: array of TMaterial;
//     BoundingBox: TBoundingBox;

//      constructor Create( FName: String );
      constructor Create;
      destructor Destroy; override;

      procedure Render;
      procedure ClearChildren;

      property Children: TModelList read FChildren;
  end;

  { TModelFile }

  TModelFile = class( TPersistent )
    private
      FModelList: TModelList;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Clear;

      function Debug: String;

      procedure Render;

      property Children: TModelList read FModelList;
  end;

function LoadModelFile( F: TStringList ): TModelFile;
function LoadModelFileFromFile( FName: String ): TModelFile;
function LoadObject( var Mdl: TModel; F: TStringList; const strpos: Integer ): Integer;

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

{
function LoadMatrix( var Matrix: TMatrix4f; F: TStringList; const strpos: Integer ): Integer;
  function ParseMatrix( S: String ): TMatrix4f; inline;
  var
    i, n: Integer;
    ptr: ^Float;
  begin
    n:= WordCount( S, [ ',' ]);
    if ( n <> 16 ) then
      WriteLn( Format( 'Error in LoadMatrix [%d]: Wrong number of values specified. Expected 16; Found %d!', [ i, n ]))
    else
      begin
        S:= ExtractWord( 1, S, [ ';' ]);
        ptr:= @Result._00;
        for i:= 1 to 16 do
          begin
            ptr^:= StrToFloat( Trim( ExtractWord( i, S, [ ',' ])));
            inc( ptr );
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

function LoadUV( var UVs: TUVArray; F: TStringList; const strpos: Integer ): Integer;
  function ParseUV( S: String ): TUV; inline;
  var
    i, n: Integer;
    ptr: ^Float;
  begin
    n:= WordCount( S, [ ',' ]);
    if ( n <> 2 ) then
      WriteLn( Format( 'Error in LoadUV [%d]: Wrong number of values specified. Expected 2; Found %d!', [ i, n ]))
    else
      begin
        S:= ExtractWord( 1, S, [ ';' ]);
        ptr:= @Result.u;
        for i:= 1 to 2 do
          begin
            ptr^:= StrToFloat( Trim( ExtractWord( i, S, [ ',' ])));
            inc( ptr );
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
}
function LoadVertices( var Vertices: TVec3Array; F: TStringList; const strpos: Integer ): Integer;
  function ParseVertex( S: String ): TVec3;
  var
    i, n: Integer;
    ptr: ^Float;
  begin
    n:= WordCount( S, [ ',' ]);
    if ( n <> 3 ) then
      WriteLn( Format( 'Error in LoadVertex [%d]: Wrong number of values specified. Expected 3; Found %d!', [ i, n ]))
    else
      begin
        S:= ExtractWord( 1, S, [ ';' ]);
        ptr:= @Result.X;
        for i:= 1 to 3 do
          begin
            ptr^:= StrToFloat( Trim( ExtractWord( i, S, [ ',' ])));
            inc( ptr );
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

function LoadChildren( Children: TModelList; F: TStringList; const strpos: Integer ): Integer;
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
            i:= LoadObject( Mdl, F, i + 1 );
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
        Result.verts[ i - 1 ].v:= StrToInt( Trim( chunk ));//StrToInt( Trim( ExtractWord( 1, chunk, [ '/' ])));
//        Result.verts[ i - 1 ].texc:= StrToInt( Trim(ExtractWord( 1, chunk, [ '/' ])));
//        Result.verts[ i - 1 ].n:= StrToInt( Trim( ExtractWord( 1, chunk, [ '/' ])));
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

function LoadObject( var Mdl: TModel; F: TStringList; const strpos: Integer ): Integer;
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
//        'matrix': i:= LoadMatrix( Mdl.Matrix, F, i + 1 );
        'vertices': i:= LoadVertices( Mdl.Vertices, F, i + 1 );
//        'normals': i:= LoadVertices( Mdl.Normals, F, i + 1 );
        'faces': i:= LoadFaces( Mdl.Faces, F, i + 1 );
//        'materials': i:= LoadMaterials( Mdl.Materials, F, i + 1 );
//        'texturecoords': i:= LoadUV( Mdl.TexCoords, F, i + 1 );
        'children': i:= LoadChildren( Mdl.Children, F, i + 1 );
        else
          begin
            WriteLn( Format( 'Error in LoadObject [%d]: unknown token "%s"', [ i, cmd ]));
            i:= i + 1;
          end;
      end;
    end;
  Result:= i + 1;
end;

function LoadModelFile(F: TStringList): TModelFile;
var
  i: Integer;
  cmd: String;
  Mdl: TModel;
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
            i:= LoadObject( Mdl, F, i + 1 );
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

{ TModelList }

procedure TModelList.Clear;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Free;
  inherited Clear;
end;

procedure TModelList.Render;
var
  i: Integer;
begin
  for i:= 0 to Count - 1 do
    Items[ i ].Render;
end;

{ TModelFile }

constructor TModelFile.Create;
begin
  inherited;
  FModelList:= TModelList.Create;
end;

destructor TModelFile.Destroy;
begin
  Clear;
  FModelList.Free;
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

    function WriteFaces( Mdl: TModel ): String;
    var
      i: Integer;
      s: String;
      j: Integer;
    begin
      Result:= StringOfChar( ' ', Indent * 2 ) + 'faces' + #13#10;

      Inc( Indent );

      for i:= 0 to high( Mdl.Faces ) do
        begin
          s:= '';

          for j:= 0 to high( Mdl.Faces[ i ].verts ) do
            s += ', ' + IntToStr( Mdl.Faces[ i ].verts[ j ].v );

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

procedure TModelFile.Render;
begin
  Children.Render;
end;

{ TModel }

procedure TModel.ClearChildren;
begin
  FChildren.Clear;
end;

constructor TModel.Create;
begin
  inherited;
  FChildren:= TModelList.Create;
end;

destructor TModel.Destroy;
begin
  ClearChildren;
  FChildren.Free;;
  inherited Destroy;
end;

procedure TModel.Render;
var
  i: Integer;
  j: Integer;
  matId: Integer;
  vNorm, Vec2: TVec3;
begin
  for i:= 0 to high( Faces ) do
    begin
{      matId:= Faces[ i ].mat;
      if ( matId >= 0 ) then
        begin
//          glColor3f( Materials[ matId ].Diff.x, Materials[ matId ].Diff.y, Materials[ matId ].Diff.z );
          glColor3f( 1.0, 1.0, 1.0 );
{          if ( Assigned( Materials[ matId ].Diff_Map )) then
            begin
              glActiveTexture( GL_TEXTURE0 );
              glEnable( GL_TEXTURE_2D );
              glBindTexture( GL_TEXTURE_2D, Materials[ matId ].Diff_Map^.ID );

              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
              glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
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
            end;}
        end
      else}
        begin
          glColor3f( 1.0, 1.0, 1.0 );
          glActiveTexture( GL_TEXTURE0 );
          glDisable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, 0 );
        end;
{      if ( Faces[ i ].count = 4 ) then
        glBegin( GL_QUADS )
      else}
        glBegin( GL_TRIANGLE_FAN );

      for j:= 0 to high( Faces[ i ].verts ) do
        begin
          //      glNormal3f(normals[faces[f].vn1 - 1].x, normals[faces[f].vn1 - 1].y, normals[faces[f].vn1 - 1].z);
{          glNormal3f( normals[ faces[ i ].verts[ j ].n ].x,
                      normals[ faces[ i ].verts[ j ].n ].y,
                      normals[ faces[ i ].verts[ j ].n ].z );}
//          if ( Faces[ i ].uv[ j ] > 0 ) then
{          glTexCoord2f( TexCoords[ Faces[ i ].verts[ j ].uv ].u,
                        TexCoords[ Faces[ i ].verts[ j ].uv ].v );}
//          glTexCoord2f( Vertices[ Faces[ i ].v[ j ]].x, Vertices[ Faces[ i ].v[ j ]].y );
          glVertex3f( vertices[ faces[ i ].verts[ j ].v ].x,
                      vertices[ faces[ i ].verts[ j ].v ].y,
                      vertices[ faces[ i ].verts[ j ].v ].z );
        end;

      glEnd();
    end;
  Children.Render;
end;

{$DEFINE TCustomList:= TCustomModelList}
{$DEFINE TCustomItem:= TModel}
{$DEFINE IMPLEMENTATION}
{$INCLUDE custom_list.inc}


end.

