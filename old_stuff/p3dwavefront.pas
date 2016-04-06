unit p3dwavefront;

{$mode objfpc}{$H+}

{.$DEFINE DEFAULT_TEX}

interface

uses
  Classes, SysUtils, strutils, dglOpenGL, {zgl_textures,} p3dMath;

type

  { TVec3 }


  TUV = record
    u, v: Real;
  end;

  TFace = record
    v: array of Integer;
    n: array of Integer;
    uv: array of Integer;
    mat: Integer;
  end;

  TMaterial = record
//    Diff_Map: zglPTexture;
    Diff: TVec3;
    alpha: Real;
    Name: String;
  end;

  TBoundingBox = record
    min,
    max,
    ctr: TVec3;
  end;

  { TModel }

  TModel = class ( TPersistent )
    public
      Vertices: array of TVec3;
      Normals: array of TVec3;
      Faces: array of TFace;
      TexCoords: array of TUV;
      Materials: array of TMaterial;
      BoundingBox: TBoundingBox;

      constructor Create;

      procedure Draw;
      procedure CalcBoundingBox;
  end;

  function LoadModelFromFile( FN: String ): TModel;

  {$IFDEF DEFAULT_TEX}
  var
    DefaultTex: zglPTexture;
  {$ENDIF}

implementation

function LoadModelFromFile(FN: String ): TModel;
var
  F: TStringList;
  i: Integer;
  mdl: TModel;
  matId: Integer;

  function RemoveComment( S: String ): String;
  var
    n: Integer;
  begin
    n:= Pos( '#', S );
    if ( n > 0 ) then
      Result:= Copy( S, 1, n - 1 )
    else
      Result:= S;
  end;

  procedure ParseLine( S: String );
  var
    cmd: String;
    ln: String;
    param: String;

    procedure ParseVertex( S: String; const Normal: Boolean = False );
    const
      Delim = [ ' ' ];
    var
      c: Integer;
      v: TVec3;
      n: Integer;
    begin
      c:= WordCount( S, Delim );
      if ( c < 3 ) then
        WriteLn( ln, 'Error: Number of coordinates (' , c, ') for vertex/normal should be 3!' )
      else if ( Normal ) then
        begin
          n:= Length( mdl.Normals );
          SetLength( mdl.Normals, n + 1 );
          mdl.Normals[ n ].x:= StrToFloat( ExtractWord( 1, S, Delim ));
          mdl.Normals[ n ].y:= StrToFloat( ExtractWord( 2, S, Delim ));
          mdl.Normals[ n ].z:= StrToFloat( ExtractWord( 3, S, Delim ));
        end
      else
        begin
          n:= Length( mdl.Vertices );
          SetLength( mdl.Vertices, n + 1 );
          mdl.Vertices[ n ].x:= StrToFloat( ExtractWord( 1, S, Delim ));
          mdl.Vertices[ n ].y:= StrToFloat( ExtractWord( 2, S, Delim ));
          mdl.Vertices[ n ].z:= StrToFloat( ExtractWord( 3, S, Delim ));
        end;
    end;

    procedure ParseFace( S: String );
    const
      Delim = [ ' ' ];
      Delim2 = [ '/' ];
    var
      c: Integer;
      n: Integer;
      j: Integer;
      vert: String;
      fi,
      ti: String;
      ni: String;
    begin
      c:= WordCount( S, Delim );

      n:= Length( mdl.Faces );
      SetLength( mdl.Faces, n + 1 );
      SetLength( mdl.Faces[ n ].v, c );

      for j:= 1 to c do
        begin
          vert:= ExtractWord( j, S, Delim );
          fi:= ExtractDelimited( 1, vert, Delim2 );
          ti:= ExtractDelimited( 2, vert, Delim2 );
          ni:= ExtractDelimited( 3, vert, Delim2 );
          if ( Trim( fi ) <> EmptyStr ) then
            mdl.Faces[ n ].v[ j - 1 ]:= StrToInt( fi );
          if ( Trim( ti ) <> EmptyStr ) then
            begin
              if ( j = 1 ) then
                SetLength( mdl.Faces[ n ].uv, c );
              mdl.Faces[ n ].uv[ j - 1 ]:= StrToInt( ti );
            end;
          if ( Trim( ni ) <> EmptyStr ) then
            begin
              if ( j = 1 ) then
                SetLength( mdl.Faces[ n ].n, c );
              mdl.Faces[ n ].n[ j - 1 ]:= StrToInt( ni );
            end;
          mdl.Faces[ n ].mat:= matId;
        end;
    end;

    procedure ParseTexCoords( S: String );
    const
      Delim = [ ' ' ];
    var
      n: Integer;
      c: Integer;
    begin
      c:= WordCount( S, Delim );
      if ( c <> 2 ) then
        WriteLn( ln, 'Error: Number of texture coordinates (' , c, ') should be 2!' )
      else
        begin
          n:= Length( mdl.TexCoords );
          SetLength( mdl.TexCoords, n + 1 );
          mdl.TexCoords[ n ].u:= StrToFloat( ExtractWord( 1, S, Delim ));
          mdl.TexCoords[ n ].v:= StrToFloat( ExtractWord( 2, S, Delim ));
        end;
    end;

    procedure LoadMaterialLib( Name: String );
    var
      matLib: TStringList;
      ln2: String;
      j: Integer;

      procedure ParseMaterial( S: String );
      var
        cmd2, param2: String;

        function ParseColor( S: String ): TVec3;
        const
          Delim = [ ' ' ];
        var
          c: Integer;
        begin
          c:= WordCount( S, Delim );
          if ( c < 3 ) then
            WriteLn( ln2, 'Error: Number of color components (' , c, ') for material should be 3!' )
          else
            begin
              Result.x:= StrToFloat( ExtractWord( 1, S, Delim ));
              Result.y:= StrToFloat( ExtractWord( 2, S, Delim ));
              Result.z:= StrToFloat( ExtractWord( 3, S, Delim ));
            end;
        end;

{        function ParseTexture( S: String ): zglPTexture;
        begin
          if ( not FileExists( S )) then
            WriteLn( ln2, 'Error: The specified texture "', S, '" could not be found!' )
          else
            begin
              Result:= tex_LoadFromFile( S );
            end;
        end;}

        procedure NewMtl( S: String );
        var
          n: Integer;
        begin
          n:= Length( mdl.Materials );
          SetLength( mdl.Materials, n + 1 );
          mdl.Materials[ n ].Name:= S;
        end;

      begin
        S:= RemoveComment( S );
        if ( not ( EmptyStr = S )) then
          begin
            cmd2:= ExtractWord( 1, S, [ ' ' ]);
            param2:= Trim( Copy( S, Length( cmd2 ) + 1, Length( S ) - Length( cmd2 )));
            ln2:= '[' + IntToStr( j + 1 ) + ']: ';

            case lowercase( cmd2 ) of
              'newmtl': NewMtl( param2 );
              'kd': mdl.Materials[ high( mdl.Materials )].Diff:= ParseColor( param2 );
              //'map_kd': mdl.Materials[ high( mdl.Materials )].Diff_Map:= ParseTexture( param2 );
              else
                WriteLn( ln2, 'Unknown command: "', cmd2, '"' );
            end;
          end;
      end;

    begin
      if ( not FileExists( Name )) then
        WriteLn( ln, 'Error loading material ', Name, '! The specified file does not exist!' )
      else
        begin
          matLib:= TStringList.Create;
          matLib.LoadFromFile( Name );
          for j:= 0 to matLib.Count - 1 do
            ParseMaterial( matLib[ j ]);

          matLib.Free;
        end;
    end;

    function GetMatId( S: String ): Integer;
    var
      j: Integer;
    begin
      Result:= -1;
      for j:= 0 to High( mdl.Materials ) do
        if ( mdl.Materials[ j ].Name = S ) then
          begin
            Result:= j;
            break;
          end;
    end;

  begin
    S:= RemoveComment( S );
    if ( not ( EmptyStr = S )) then
      begin
        cmd:= ExtractWord( 1, S, [ ' ' ]);
        param:= Trim( Copy( S, Length( cmd ) + 1, Length( S ) - Length( cmd )));
        ln:= '[' + IntToStr( i + 1 ) + ']: ';
        case lowercase( cmd ) of
          'usemtl': matId:= GetMatId( param );
          'mtllib': LoadMaterialLib( param );
          'o': WriteLn( ln, 'object ignored! Not supported yet!' );
          'v': ParseVertex( param );
          'vn': ParseVertex( param, True );
          'vt': ParseTexCoords( param );
          'f': ParseFace( param );
          's': WriteLn( ln, 'smoothing group ignored! Not supported yet!' );
        else
          WriteLn( 'Unknown command: "', cmd, '"' );
        end;
      end;
  end;

begin
  if ( not FileExists( FN )) then
    write( 'Error loading model ', FN, '! The specified file does not exist!' )
  else
    begin
      mdl:= TModel.Create;
      Result:= mdl;
      F:= TStringList.Create;
      F.LoadFromFile( FN );
      matId:= -1;
      DecimalSeparator:= '.';

      for i:= 0 to F.Count - 1 do
        ParseLine( F[ i ]);
      F.Free;
    end;
end;

{ TVec3 }
{
function TVec3.CrossProduct(v: TVec3): TVec3;
begin
  Result.x:= y * v.z - v.y * z;
  Result.y:= z * v.x - v.z * x;
  Result.z:= x * v.y - v.x * y;
end;

function TVec3.Length: Real;
begin
  Result:= sqrt( x*x + y*y + z*z );
end;

procedure TVec3.Normalize;
var
  len: Real;
begin
  len:= Length;
  x:= x / len;
  y:= y / len;
  z:= z / len;
end;

procedure TVec3.Add(Vec: TVec3);
begin
  x:= x + Vec.x;
  y:= y + Vec.y;
  z:= z + Vec.z;
end;

procedure TVec3.Subtract(Vec: TVec3);
begin
  x:= x - Vec.x;
  y:= y - Vec.y;
  z:= z - Vec.z;
end;
}
{ TModel }

constructor TModel.Create;
begin
  inherited;
  {$IFDEF DEFAULT_TEX}
    if ( not Assigned( DefaultTex )) then
      DefaultTex:= tex_CreateZero( 1, 1, $FFFFFFFF );
  {$ENDIF}
end;

procedure TModel.Draw;
var
  i: Integer;
  j: Integer;
  matId: Integer;
  vNorm: TVec3;
begin
  for i:= 0 to high( Faces ) do
    begin
      matId:= Faces[ i ].mat;
      if ( matId >= 0 ) then
        begin
          glColor3f( Materials[ matId ].Diff.x, Materials[ matId ].Diff.y, Materials[ matId ].Diff.z );
          {if ( Assigned( Materials[ matId ].Diff_Map )) then
            begin
              glActiveTexture( GL_TEXTURE0 );
              glEnable( GL_TEXTURE_2D );
              glBindTexture( GL_TEXTURE_2D, Materials[ matId ].Diff_Map^.ID );

              glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT );
              glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT );
            end
          else}
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
          glDisable( GL_TEXTURE_2D );
          glBindTexture( GL_TEXTURE_2D, 0 );
        end;
      if ( Length( Faces[ i ].v ) = 4 ) then
        glBegin( GL_QUADS )
      else
        glBegin( GL_TRIANGLE_FAN );

      for j:= 0 to High( Faces[ i ].v ) do
        begin
          //      glNormal3f(normals[faces[f].vn1 - 1].x, normals[faces[f].vn1 - 1].y, normals[faces[f].vn1 - 1].z);
          if ( Length( faces[ i ].n ) > 0 ) then
            glNormal3f( normals[ faces[ i ].n[ j ] - 1 ].x, normals[ faces[ i ].n[ j ] - 1 ].y, normals[ faces[ i ].n[ j ] - 1 ].z );
          if ( Length( faces[ i ].uv ) > 0 ) then
//          if ( Faces[ i ].uv[ j ] > 0 ) then
            glTexCoord2f( TexCoords[ Faces[ i ].uv[ j ] - 1].u, TexCoords[ Faces[ i ].uv[ j ] - 1 ].v );
//          glTexCoord2f( Vertices[ Faces[ i ].v[ j ]].x, Vertices[ Faces[ i ].v[ j ]].y );
          glVertex3f( vertices[ faces[ i ].v[ j ] - 1 ].x, vertices[ faces[ i ].v[ j ] - 1 ].y, vertices[ faces[ i ].v[ j ] - 1 ].z );
        end;

      glEnd();
    end;

end;

procedure TModel.CalcBoundingBox;
var
  i: Integer;
begin
  if ( Length( Vertices ) < 1 ) then
    exit;
  BoundingBox.min:= Vertices[ 0 ];
  BoundingBox.max:= Vertices[ 0 ];

  for i:= 0 to high( Vertices ) do
    begin
      if ( Vertices[ i ].x < BoundingBox.min.x ) then
        BoundingBox.min.x:= Vertices[ i ].x;
      if ( Vertices[ i ].y < BoundingBox.min.y ) then
        BoundingBox.min.y:= Vertices[ i ].y;
      if ( Vertices[ i ].z < BoundingBox.min.z ) then
        BoundingBox.min.z:= Vertices[ i ].z;

      if ( Vertices[ i ].x < BoundingBox.max.x ) then
        BoundingBox.max.x:= Vertices[ i ].x;
      if ( Vertices[ i ].y < BoundingBox.max.y ) then
        BoundingBox.max.y:= Vertices[ i ].y;
      if ( Vertices[ i ].z < BoundingBox.max.z ) then
        BoundingBox.max.z:= Vertices[ i ].z;
    end;
  BoundingBox.ctr:= BoundingBox.max - BoundingBox.min;
end;


end.
