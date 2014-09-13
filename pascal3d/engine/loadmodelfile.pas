unit loadmodelfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Model, strutils, Math3D, texture_sdl;


function LoadModelFile( F: TStringList ): TModelFile;
function LoadModelFileFromFile( FName: String ): TModelFile;


implementation


function LoadObject( MdlFile: TModelFile; var Mdl: TModel; F: TStringList; const strpos: Integer ): Integer; forward;


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
      {$IFDEF VERBOSE}
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

function SkipToNextEnd( F: TStringList; const strpos: Integer ): Integer;
var
  i: Integer;
  cmd: String;
begin
  i:= strpos;
  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERBOSE}
      writeln( 'SkipToNextEnd[', i, ']' );
      {$ENDIF}
      case cmd of
        'end;': break;
      else
        inc( i );
      end;
    end;
  Result:= i + 1;
end;

function LoadUV( var UVs: TVec2List; F: TStringList; const strpos: Integer ): Integer;
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
      {$IFDEF VERBOSE}
      writeln( 'LoadUV[', i, ']' );
      {$ENDIF}
      case cmd of
        'end;': break;
        else
          begin
            //SetLength( UVs, Length( UVs ) + 1 );
            UVs.Add( ParseUV( F[ i ]));
//            UVs[ high( UVs )]:= ParseUV( F[ i ]);
            inc( i );
          end;// Loading Vertices
      end;
    end;
  Result:= i + 1;
end;

function ParseVertex3f( S: String ): TVec3;
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

function ParseVertex4f( S: String ): TVec4;
var
  i, n: Integer;
begin
  n:= WordCount( S, [ ',' ]);
  if ( n <> 4 ) then
    WriteLn( Format( 'Error in LoadVertex [%d]: Wrong number of values specified. Expected 4; Found %d!', [ i, n ]))
  else
    begin
      S:= ExtractWord( 1, S, [ ';' ]);
      for i:= 0 to 3 do
        Result.Coord[ i ]:= StrToFloat( Trim( ExtractWord( i + 1, S, [ ',' ])));
    end;
end;

function LoadVertices( var Vertices: TVec3List; F: TStringList; const strpos: Integer ): Integer;
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
//            SetLength( Vertices, Length( Vertices ) + 1 );
            {Vertices[ high( Vertices )]:= }Vertices.Add( ParseVertex3f( F[ i ]));
            inc( i );
          end;// Loading Vertices
      end;
    end;
  Result:= i + 1;
end;

function LoadVerticesBin( var Vertices: TVec3List; F: TFileStream; count: Integer ): Integer;
var
  i: Integer;
  base: Integer;
  vert: TVec3;
begin
  base:= Vertices.Count;
  //SetLength( Vertices, base + count );
  //Vertices.Count:= base + count;
  for i:= 0 to count - 1 do
    begin
      F.Read( vert, 3*SizeOf( Single ));
      Vertices.Add( vert );
    end;
end;

function LoadUVBin( var UVs: TVec2List; F: TFileStream; count: Integer ): Integer;
var
  i: Integer;
  base: Integer;
  uv: TVec2;
begin
  base:= UVs.Count;
  //UVs.Count:= base + count;
  for i:= 0 to count - 1 do
    begin
      F.Read( uv, SizeOf( TVec2 ));
      UVs.Add( uv );
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
        uv:= Trim( ExtractWord( 3, chunk, [ '/' ]));
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
      {$IFDEF VERBOSE}
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
       Result:= SkipToNextEnd( F, i );
       exit;
    end;
  bin:= TFileStream.Create( fname, fmOpenRead );

  while( i < F.Count ) do
    begin
      cmd:= GetCmd( F[ i ]);
      {$IFDEF VERBOSE}
      writeln( 'LoadExternalObjectFile[', i, ']' );
      {$ENDIF}
      param:= GetParams( F[ i ]);
      case cmd of
        'end;':
          break;
        'vertices': LoadVerticesBin( Mdl.Positions, bin, StrToInt( param ));
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

      {$IFDEF VERBOSE}
      writeln( 'LoadObjMaterials[', i, ']' );
      {$ENDIF}

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
        'vertices': i:= LoadVertices( Mdl.Positions, F, i + 1 );
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
  {.$IFDEF BUFFERS}
  Mdl.Calc_Tangent_Binormal;
  Mdl.UnpackBuffers;
  {.$ENDIF}
  Result:= i + 1;
end;

function LoadTexture( Mat: TMaterial; TexName: String; F: TStringList; strpos: Integer ): Integer;
var
  i: Integer;
  cmd: String;
  tex: TSDLSurface;
  tmpVec: TVec4;
  Map: ^TMap;
begin
  i:= strpos;

  if ( not FileExists( TexName )) then
    begin
      WriteLn( 'Error: The specified texture "', TexName, '" could not be found!' );
      Result:= SkipToNextEnd( F, i + 1 );
      exit;
    end
  else
    begin
      tex:= TSDLSurface.Create( TexName );
      if ( Assigned( tex )) then
        begin
          Mat.Maps[ Mat.NumMaps ].Map:= tex;
          Map:= @Mat.Maps[ Mat.NumMaps ];
          Inc( Mat.NumMaps );
        end
      else
        begin
          WriteLn( 'Error: An unknown error occured while loading texture "', TexName, '"!' );
          Result:= SkipToNextEnd( F, i + 1 );
          exit;
        end;
    end;

  while( i < F.Count ) do
    begin
      {$IFDEF VERBOSE}
      writeln( 'LoadTexture[', i, ']' );
      {$ENDIF}

      cmd:= GetCmd( F[ i ]);
      case cmd of
        'diffuse':
          Map^.DiffuseFactor:= StrToFloat( GetParams( F[ i ]));
        'normal':
          Map^.NormalFactor:= StrToFloat( GetParams( F[ i ]));
        'end;':
          begin
            break;
          end;
        else
          begin
            WriteLn( Format( 'Error in LoadTexture [%d]: unknown token "%s"', [ i, cmd ]));
            i:= i + 1;
          end;
      end;
      i:= i + 1;
    end;
  Result:= i;
end;


function LoadMaterial( Mat: TMaterial; F: TStringList; strpos: Integer ): Integer;
var
  i: Integer;
  cmd: String;
  param: String;
  tex: TSDLSurface;
  tmpVec: TVec4;
begin
  i:= strpos;

  Mat.NumMaps:= 0;
  while( i < F.Count ) do
    begin
      {$IFDEF VERBOSE}
      writeln( 'LoadMaterial[', i, ']' );
      {$ENDIF}

      cmd:= GetCmd( F[ i ]);
      case cmd of
        'ambient':
          begin
//            Mat.Amb:= ParseVertex4f( GetParams( F[ i ]));
          end;
        'diffuse':
          begin
            Mat.Diff:= ParseVertex3f( GetParams( F[ i ]));
          end;
        'specular':
          begin
            tmpVec:= ParseVertex4f( GetParams( F[ i ]));
            Mat.Spec_Hardness:= tmpVec.X;
            Mat.Spec:= Vec3( tmpVec.y, tmpVec.z, tmpVec.w );
          end;
        'texture':
          begin
            param:= ExtractWord( 1, GetParams( F[ i ]), [ '''' ]);
            i:= LoadTexture( Mat, Param, F, i + 1 );
          end;
        'end;':
          begin
            break;
          end;
        else
          begin
            WriteLn( Format( 'Error in LoadMaterial [%d]: unknown token "%s"', [ i, cmd ]));
            i:= i + 1;
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
      {$IFDEF VERBOSE}
      writeln( 'LoadModelFile[', i, ']' );
      {$ENDIF}

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
          end;
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
      {$IFDEF VERBOSE}
      writeln( 'LoadModelFileFromFile ''', FName , ''']' );
      {$ENDIF}
      F:= TStringList.Create;
      F.LoadFromFile( FName );
      Result:= LoadModelFile( F );
      F.Free;
    end
  else
    raise Exception.Create( Format( 'TModel.Create: File %s not found', [ FName ]));
end;

end.

