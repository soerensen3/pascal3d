unit p3dskybox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, p3dtexture, p3dshaders, p3dscene, p3dMath, dglOpenGL, p3dmodel, p3dbuffers;

type

  { TSkyBoxSimple }

  TSkyBoxSimple = class
    private
      FTextureRLFR: TP3DTexture;
      FTextureTB: TP3DTexture;
      Cube: TP3DMesh;

    public
      constructor Create;
      destructor Destroy; override;

      procedure Render( Cam: TP3DCamera );

    published
      property TextureRLFR: TP3DTexture read FTextureRLFR write FTextureRLFR;
      property TextureTB: TP3DTexture read FTextureTB write FTextureTB;
  end;

implementation

{ TSkyBoxSimple }

constructor TSkyBoxSimple.Create;
const
  s = 4;
var
  n: Integer;
begin
  inherited;

  Cube:= TP3DMesh.Create( nil );
  SetLength( Cube.TexCoords, 1 );
  Cube.TexCoords[ 0 ]:= TP3DVec2BufferGL.Create( True );

  n:= 0;
  Cube.Positions.Add( vec3( -1, -1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, 0.0 ));
  Cube.Positions.Add( vec3(  1, -1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( s, 0.0 ));
  Cube.Positions.Add( vec3(  1,  1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( s, s ));
  Cube.Positions.Add( vec3( -1,  1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, s ));
  Cube.Indices.Add([ 0 +n, 1 +n, 2 +n, 0 +n, 2 +n, 3 +n ]);

  Inc( n, 4 );
  Cube.Positions.Add( vec3(  1,  1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( s, s ));
  Cube.Positions.Add( vec3(  1, -1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( s, 0.0 ));
  Cube.Positions.Add( vec3( -1, -1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, 0.0 ));
  Cube.Positions.Add( vec3( -1,  1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, s ));
  Cube.Indices.Add([ 0 +n, 1 +n, 2 +n, 0 +n, 2 +n, 3 +n ]);

  Inc( n, 4 );
  Cube.Positions.Add( vec3(  1,  1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, s ));
  Cube.Positions.Add( vec3(  1, -1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, 0.0 ));
  Cube.Positions.Add( vec3(  1, -1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( s, 0.0 ));
  Cube.Positions.Add( vec3(  1,  1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( s, s ));
  Cube.Indices.Add([ 0 +n, 1 +n, 2 +n, 0 +n, 2 +n, 3 +n ]);

  Inc( n, 4 );
  Cube.Positions.Add( vec3( -1, -1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( s, s ));
  Cube.Positions.Add( vec3( -1, -1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( s, 0.0 ));
  Cube.Positions.Add( vec3( -1,  1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, 0.0 ));
  Cube.Positions.Add( vec3( -1,  1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, s ));
  Cube.Indices.Add([ 0 +n, 1 +n, 2 +n, 0 +n, 2 +n, 3 +n ]);

  Inc( n, 4 );
  Cube.Positions.Add( vec3( -1,  1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( s, s ));
  Cube.Positions.Add( vec3(  1,  1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( s, 0.0 ));
  Cube.Positions.Add( vec3(  1,  1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, 0.0 ));
  Cube.Positions.Add( vec3( -1,  1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, s ));
  Cube.Indices.Add([ 0 +n, 1 +n, 2 +n, 0 +n, 2 +n, 3 +n ]);

  Inc( n, 4 );
  Cube.Positions.Add( vec3(  1, -1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( s, s ));
  Cube.Positions.Add( vec3(  1, -1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( s, 0.0 ));
  Cube.Positions.Add( vec3( -1, -1, -1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, 0.0 ));
  Cube.Positions.Add( vec3( -1, -1,  1 )); Cube.TexCoords[ 0 ].Add( vec2( 0.0, s ));
  Cube.Indices.Add([ 0 +n, 1 +n, 2 +n, 0 +n, 2 +n, 3 +n ]);

  //Cube.Material.Maps[ 0 ].Map:= FTextureRLFR;
  //Cube.Material.Maps[ 1 ].Map:= FTextureTB;
end;

destructor TSkyBoxSimple.Destroy;
begin
  Cube.Free;
  inherited Destroy;
end;

procedure TSkyBoxSimple.Render( Cam: TP3DCamera );
var
  view: TMat4;
begin
  if ( Assigned( Cam )) then
    begin
      view:= mat4( Cam.MatNormal );

      glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'view'), 1, False, @view );
      glUniformMatrix4fv( ActShad.Uniforms.AddrByName( 'world'), 1, False, @Mat4Identity );
    end;

{  glActiveTexture( GL_TEXTURE0 );
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, FTextureRLFR.fGLTexture );
  glActiveTexture( GL_TEXTURE1 );
  glEnable( GL_TEXTURE_2D );
  glBindTexture( GL_TEXTURE_2D, FTextureTB.fGLTexture );}

  glUniform1i( ActShad.Uniforms.AddrByName( 'TextureRLFR' ), 0 );
  glUniform1i( ActShad.Uniforms.AddrByName( 'TextureTB' ), 1 );

  //Cube.Render( Mat4Identity, Scene ); TODO: FIX TP3DSkyBox Render procedure
end;

end.

