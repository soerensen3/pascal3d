unit Model;

{$mode objfpc}{$H+}
{.$DEFINE VERYVERBOSE}

interface
uses
  Classes, SysUtils, dglOpenGL, Math3D, strutils;

type
  TUV = record
    u, v: Real;
  end;

  TFaceVertex = record
    v, n, uv: Integer;
  end;

  TFace = record
    verts: array [ 0..3 ] of TFaceVertex;
    count: Byte;
    mat: Integer;
  end;

  TMaterial = record
    Diff_Map: Integer;
    Diff: TVector;
    Spec: TVector;
    alpha: Real;
    Name: String;
  end;
  TVecArray = array of TVector;
  TFaceArray = array of TFace;
  TUVArray = array of TUV;
  TMaterialArray = array of TMaterial;

  { TModel }
  TModel = class( TPersistent )
    public
      Vertices: TVecArray;
      Normals: TVecArray;
      Faces: TFaceArray;
      TexCoords: TUVArray;
      Name: String;
      Matrix: TMatrix4f;
      Materials: array of TMaterial;
//     BoundingBox: TBoundingBox;

      constructor Create( FName: String );
      constructor Create;

      procedure Render;
  end;

function LoadModelFile( {tmp}var Mdl: TModel; F: TStringList; const strpos: Integer = 0 ): Integer;

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
  Result:= Copy( S, n + 1, Length( S ) - n - 1 );
end;

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

function LoadVertices( var Vertices: TVecArray; F: TStringList; const strpos: Integer ): Integer;
  function ParseVertex( S: String ): TVector; inline;
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
      {$IFDEF VERYVERBOSE}
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

function LoadFaces( var Faces: TFaceArray; F: TStringList; const strpos: Integer ): Integer;
  function ParseFace( S: String ): TFace; //inline;
  var
    i, n: Integer;
    chunk: String;
  begin
    S:= Trim( ExtractWord( 1, S, [ ';' ]));
    Result.mat:= StrToInt( ExtractWord( 1, S, [ ' ' ])); //FEHLER?
    n:= WordPosition( 2, S, [ ' ' ]);
    S:= Copy( S, n, Length( S ) - n + 1 );
    n:= WordCount( S, [ ',' ]);
    if ( n > 4 ) then
      begin
        WriteLn( Format( 'Error in LoadFaces [%d]: Too many vertices. Maximum 4; Found %d!', [ i, n ]));
        exit;
      end;

    Result.count:= n;
    for i:= 1 to n do
      begin
        chunk:= Trim( ExtractWord( i, S, [ ',' ]));
        Result.verts[ i - 1 ].v:= StrToInt( Trim( ExtractWord( 1, chunk, [ '/' ])));
        Result.verts[ i - 1 ].uv:= StrToInt( Trim(ExtractWord( 1, chunk, [ '/' ])));
        Result.verts[ i - 1 ].n:= StrToInt( Trim( ExtractWord( 1, chunk, [ '/' ])));
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
      {$IFDEF VERYVERBOSE}
      writeln( 'LoadObject[', i, ']' );
      {$ENDIF}
      param:= GetParams( F[ i ]);
      Mdl.Name:= param;
      case cmd of
        'end;': break;
        'matrix': i:= LoadMatrix( Mdl.Matrix, F, i + 1 );
        'vertices': i:= LoadVertices( Mdl.Vertices, F, i + 1 );
        'normals': i:= LoadVertices( Mdl.Normals, F, i + 1 );
        'faces': i:= LoadFaces( Mdl.Faces, F, i + 1 );
        'materials': i:= LoadMaterials( Mdl.Materials, F, i + 1 );
        'texturecoords': i:= LoadUV( Mdl.TexCoords, F, i + 1 );
        else
          begin
            WriteLn( Format( 'Error in LoadObject [%d]: unknown token "%s"', [ i, cmd ]));
            i:= i + 1;
          end;
      end;
    end;
  Result:= i + 1;
end;

function LoadModelFile( {tmp}var Mdl: TModel; F: TStringList; const strpos: Integer = 0 ): Integer;
var
  i: Integer;
  cmd: String;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERYVERBOSE}
      writeln( 'LoadModelFile[', i, ']' );
      {$ENDIF}
      case cmd of
        'object': i:= LoadObject( Mdl, F, i + 1 );
        else
          begin
            WriteLn( Format( 'Error in LoadModelFile [%d]: unknown token "%s"', [ i, cmd ]));
            i:= i + 1;
          end;
      end;
    end;
  Result:= i + 1;
end;

{ TModel }

constructor TModel.Create(FName: String);
var
  F: TStringList;
begin
  if ( FileExists( FName )) then
    begin
      Create;
      F:= TStringList.Create;
      F.LoadFromFile( FName );
      LoadModelFile( Self, F );
      F.Free;
    end
  else
    raise Exception.Create( Format( 'TModel.Create: File %s not found', [ FName ]));
end;

constructor TModel.Create;
begin
  inherited;
end;

procedure TModel.Render;
var
  i: Integer;
  j: Integer;
  matId: Integer;
  vNorm, Vec2: TVector;
begin
  for i:= 0 to high( Faces ) do
    begin
      matId:= Faces[ i ].mat;
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
      else
        begin
          glColor3f( 1.0, 1.0, 1.0 );
          glActiveTexture( GL_TEXTURE0 );
          glDisable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, 0 );
        end;
      if ( Faces[ i ].count = 4 ) then
        glBegin( GL_QUADS )
      else
        glBegin( GL_TRIANGLES );

      for j:= 0 to Faces[ i ].count - 1 do
        begin
          //      glNormal3f(normals[faces[f].vn1 - 1].x, normals[faces[f].vn1 - 1].y, normals[faces[f].vn1 - 1].z);
          glNormal3f( normals[ faces[ i ].verts[ j ].n ].x,
                      normals[ faces[ i ].verts[ j ].n ].y,
                      normals[ faces[ i ].verts[ j ].n ].z );
//          if ( Faces[ i ].uv[ j ] > 0 ) then
          glTexCoord2f( TexCoords[ Faces[ i ].verts[ j ].uv ].u,
                        TexCoords[ Faces[ i ].verts[ j ].uv ].v );
//          glTexCoord2f( Vertices[ Faces[ i ].v[ j ]].x, Vertices[ Faces[ i ].v[ j ]].y );
          glVertex3f( vertices[ faces[ i ].verts[ j ].v ].x,
                      vertices[ faces[ i ].verts[ j ].v ].y,
                      vertices[ faces[ i ].verts[ j ].v ].z );
        end;

      glEnd();
    end;
end;

end.

