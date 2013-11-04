unit terrain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, zgl_textures, zgl_types, dglOpenGL, Math3D;

type
  vertex_terrain = packed record
      x,y,z: Single;
      nx, ny, nz: Single;
  end;

  { TTerrain }

  TTerrain = class
    private
      FVertices: array of vertex_terrain;
      FIndices: array of Cardinal;
      vbo_vertices, vbo_indices: GLuint;
      ivertex, inormal,
        iprojection, iview, imodel,
        glProgram: GLuint;

    public
      constructor Create;
      destructor Destroy; override;

      function LoadHeightMap( tex: zglPTexture; const elevation: Float = 1 ): Boolean;
      procedure updateShader( Shader: GLuint );

      procedure Render( Proj, View, Model: TMatrix4f );
  end;

implementation

{ TTerrain }

constructor TTerrain.Create;
begin
  inherited;

  glGenBuffers( 1, @vbo_vertices );

  glGenBuffers( 1, @vbo_indices);
end;

destructor TTerrain.Destroy;
begin
  SetLength( FVertices, 0 );
  SetLength( FIndices, 0 );

  glDeleteBuffers( 1, @vbo_indices );
  glDeleteBuffers( 1, @vbo_vertices );

  inherited Destroy;
end;

function TTerrain.LoadHeightMap(tex: zglPTexture; const elevation: Float = 1 ): Boolean;
var
  data: PByteArray;
  y: Integer;
  x: Integer;
  idx: Integer;
  idx2: Integer;
begin
  tex_GetData( tex, data );
  SetLength( FVertices, ( tex^.Width ) * ( tex^.Height ));
  for x:= 0 to tex^.Width - 1 do
    for y:= 0 to tex^.Height - 1 do
      begin
        idx:= y * tex^.Width + x;
        FVertices[ idx ].y:= data^[ idx * 4 ] / 255 * elevation;
        FVertices[ idx ].x:= x - tex^.Width / 2;
        FVertices[ idx ].z:= y - tex^.Height / 2;
      end;
  WriteLn( idx );
  SetLength( FIndices, (( tex^.Width - 1 ) * ( tex^.Height - 1 ) * 4 ));
  for y:= 0 to tex^.Height - 2 do
    for x:= 0 to tex^.Width - 2 do
      begin
        idx:= ( y * ( tex^.Width - 1 ) + x ) * 4;
        idx2:= y * tex^.Width + x;
        FIndices[ idx + 0 ]:= idx2 + 0;
        FIndices[ idx + 1 ]:= idx2 + 1;
        FIndices[ idx + 2 ]:= idx2 + tex^.Width + 1;
        FIndices[ idx + 3 ]:= idx2 + tex^.Width + 0;
      end;
  WriteLn( idx );
  glBindBuffer( GL_ARRAY_BUFFER, vbo_vertices );
  glBufferData( GL_ARRAY_BUFFER, sizeof( vertex_terrain ) * Length( FVertices ), @FVertices[ 0 ], GL_DYNAMIC_DRAW );
  glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, vbo_indices );
  glBufferData( GL_ELEMENT_ARRAY_BUFFER, Length( FIndices ) * SizeOf( Cardinal ), @FIndices[ 0 ], GL_STATIC_DRAW );
end;

procedure TTerrain.updateShader(Shader: GLuint);
begin
  glProgram:= Shader;

  ivertex:= glGetAttribLocation(glProgram, 'vertex');
  inormal:= glGetAttribLocation(glProgram, 'normal');
  iprojection:= glGetUniformLocation(glProgram, 'Projection');
  iview:= glGetUniformLocation(glProgram, 'View');
  imodel:= glGetUniformLocation(glProgram, 'Model');
end;

procedure TTerrain.Render(Proj, View, Model: TMatrix4f);
  function MatrixToDGL( MatIn: TMatrix4f ): dglOpenGL.TMatrix4f;
  begin
    Result[ 0, 0 ]:= MatIn._00;
    Result[ 0, 1 ]:= MatIn._01;
    Result[ 0, 2 ]:= MatIn._02;
    Result[ 0, 3 ]:= MatIn._03;

    Result[ 1, 0 ]:= MatIn._10;
    Result[ 1, 1 ]:= MatIn._11;
    Result[ 1, 2 ]:= MatIn._12;
    Result[ 1, 3 ]:= MatIn._13;

    Result[ 2, 0 ]:= MatIn._20;
    Result[ 2, 1 ]:= MatIn._21;
    Result[ 2, 2 ]:= MatIn._22;
    Result[ 2, 3 ]:= MatIn._23;

    Result[ 3, 0 ]:= MatIn._30;
    Result[ 3, 1 ]:= MatIn._31;
    Result[ 3, 2 ]:= MatIn._32;
    Result[ 3, 3 ]:= MatIn._33;
  end;
var
  m: dglOpenGL.TMatrix4f;
begin
  m:= MatrixToDGL( Proj );
  glUniformMatrix4fv( iprojection, 1, ByteBool( GL_FALSE ), @m );
  m:= MatrixToDGL( View );
  glUniformMatrix4fv( iview, 1, ByteBool( GL_FALSE ), @m );
  m:= MatrixToDGL( Model );
  glUniformMatrix4fv( imodel, 1, ByteBool( GL_FALSE ), @m );

  glBindBuffer(GL_ARRAY_BUFFER, vbo_vertices);

  glBufferSubData( GL_ARRAY_BUFFER, 0, sizeof( vertex_terrain ) * Length( FVertices ), @FVertices[ 0 ]);
  glEnableVertexAttribArray( ivertex );
  glVertexAttribPointer( ivertex, 3, GL_FLOAT, ByteBool( 0 ), sizeof( vertex_terrain ), Pointer( 0 ));
  glEnableVertexAttribArray( inormal );
  glVertexAttribPointer( inormal, 3, GL_FLOAT, ByteBool( 0 ), sizeof( vertex_terrain ), Pointer( 12 ));

  glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, vbo_indices );

  glDrawElements( GL_QUADS, Length( FIndices ), GL_UNSIGNED_INT, nil );
end;

end.

