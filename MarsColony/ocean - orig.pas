unit ocean;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, shaders, Math, Math3D, UComplex, FFT;

type
  vertex_ocean = packed record
      x,y,z: Single;
      nx, ny, nz: Single;
      s, t: Single;
      htilde0x, htilde0y, htilde0z: Single;
      htilde0mkx, htilde0mky, htilde0mkz: Single;
      ox, oy, oz: Single;
  end;

  complex_vector_normal = record  // structure used with discrete fourier transform
      h: complex;      // wave height
      D: TVector;      // displacement
      n: TVector;      // normal
  end;

  { TOcean }

  TOcean = class
    private
      _geometry: Boolean;                      // flag to render geometry or surface

      g: Real;                                // gravity constant
      _N, Nplus1: Integer;                     // dimension -- N should be a power of 2
      _A: Real;                                // phillips spectrum parameter -- affects heights of waves
      _w: TVector;                             // wind parameter
      _length: Real;                           // length parameter
      h_tilde,                                // for fast fourier transform
      h_tilde_slopex, h_tilde_slopez,
      h_tilde_dx, h_tilde_dz: TComplexField;
      cFFT: TFFT;                             // fast fourier transform

      vertices: array of vertex_ocean;        // vertices for vertex buffer object
      indices: array of Cardinal;             // indicies for vertex buffer object
      indices_count: Cardinal;                // number of indices to render
      vbo_vertices, vbo_indices: GLuint;      // vertex buffer objects

      glProgram: GLuint; // shaders
      vertex, normal, texture, light_position,
        _view, _projection, _model: GLint; // attributes and uniforms

    public
      constructor Create( const N: Integer; const A: Real; const w: TVector; const length: Real; Shader: GLuint; geometry: Boolean );
      destructor Destroy; override;
//      void release();

      procedure updateShader( Shader: GLuint );
      function dispersion( n_prime, m_prime: Integer ): Real;     // deep water
      function phillips( n_prime, m_prime: Integer ): Real;       // phillips spectrum
      function hTilde_0( n_prime, m_prime: Integer ): Complex;
      function hTilde( t: Real; n_prime, m_prime: Integer ): Complex;
      function h_D_and_n( x: TVector; t: Real ): complex_vector_normal;
//      procedure evaluateWaves(t: Real );
      procedure evaluateWavesFFT(t: Real );
      procedure render( t: Real; light_pos: TVector; Proj, View, Model: TMatrix4f );
  end;

implementation

{ TOcean }

constructor TOcean.Create(const N: Integer; const A: Real; const w: TVector;
  const length: Real;  Shader: GLuint; geometry: Boolean);
var
  m_prime: Integer;
  n_prime: Integer;
  index: Integer;
  htilde0: complex;
  htilde0mk_conj: complex;
begin
  g:= 9.81;
  _geometry:= geometry;
  _N:= N;
  Nplus1:= N+1;
  _A:= A;
  _w:= w;
  _length:= length;

  SetLength( h_tilde, N*N );
  SetLength( h_tilde_slopex, N*N );
  SetLength( h_tilde_slopez, N*N );
  SetLength( h_tilde_dx, N*N );
  SetLength( h_tilde_dz, N*N );
  cfft:= TFFT.Create( N );
  SetLength( vertices, Nplus1*Nplus1 );
  SetLength( indices, Nplus1*Nplus1 * 10 );

//  complex htilde0, htilde0mk_conj;
  for m_prime:= 0 to N do
    for n_prime:= 0 to N do
      begin
        index:= m_prime * Nplus1 + n_prime;

  	htilde0:= hTilde_0( n_prime,  m_prime);
  	htilde0mk_conj:= cong( hTilde_0( -n_prime, -m_prime ));

  	vertices[ index ].htilde0x:= htilde0.re;
  	vertices[ index ].htilde0y:= htilde0.im;
  	vertices[ index ].htilde0mkx:= htilde0mk_conj.im;
  	vertices[ index ].htilde0mky:= htilde0mk_conj.re;

  	vertices[ index ].ox:= ( n_prime - N / 2.0 ) * length / N;
  	vertices[ index ].oy:= 0;
        vertices[ index ].oz:= ( m_prime - N / 2.0 ) * length / N;
        vertices[ index ].x:= vertices[index].ox;
        vertices[ index ].y:= vertices[index].oy;
        vertices[ index ].z:= vertices[index].oz;
        vertices[ index ].s:= vertices[ index ].ox;
        vertices[ index ].t:= vertices[ index ].oz;

  	vertices[index].nx:= 0;
  	vertices[index].ny:= 1;
  	vertices[index].nz:= 0;
      end;

  indices_count:= 0;
  for m_prime:= 0 to N - 1 do
    for n_prime:= 0 to N - 1 do
      begin
        index:= m_prime * Nplus1 + n_prime;

  	if ( geometry ) then // lines
          begin
            indices[ indices_count ]:= index; Inc( indices_count );
            indices[ indices_count ]:= index + 1; Inc( indices_count );
            indices[ indices_count ]:= index; Inc( indices_count );
            indices[ indices_count ]:= index + Nplus1; Inc( indices_count );
  	    indices[ indices_count ]:= index; Inc( indices_count );
  	    indices[ indices_count ]:= index + Nplus1 + 1; Inc( indices_count );
  	    if ( n_prime = N - 1 ) then
              begin
      	        indices[ indices_count ]:= index + 1; Inc( indices_count );
  	        indices[ indices_count ]:= index + Nplus1 + 1; Inc( indices_count );
              end;
  	    if (m_prime = N - 1) then
              begin
  	        indices[ indices_count ]:= index + Nplus1; Inc( indices_count );
  		indices[ indices_count ]:= index + Nplus1 + 1; Inc( indices_count );
  	      end;
          end
        else				// two triangles
          begin
  	    indices[ indices_count ]:= index; Inc( indices_count );
  	    indices[ indices_count ]:= index + Nplus1; Inc( indices_count );
  	    indices[ indices_count ]:= index + Nplus1 + 1; Inc( indices_count );
  	    indices[ indices_count ]:= index; Inc( indices_count );
  	    indices[ indices_count ]:= index + Nplus1 + 1; Inc( indices_count );
  	    indices[ indices_count ]:= index + 1; Inc( indices_count );
          end;
      end;

//  createProgram(glProgram, glShaderV, glShaderF, "src/oceanv.sh", "src/oceanf.sh");
  updateShader( Shader );

  glGenBuffers( 1, @vbo_vertices );
  glBindBuffer( GL_ARRAY_BUFFER, vbo_vertices );
  glBufferData( GL_ARRAY_BUFFER, sizeof( vertex_ocean ) * ( Nplus1 ) * ( Nplus1 ), @vertices[ 0 ], GL_DYNAMIC_DRAW );

  glGenBuffers(1, @vbo_indices);
  glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, vbo_indices );
  glBufferData( GL_ELEMENT_ARRAY_BUFFER, indices_count * sizeof( Cardinal ), @indices[ 0 ], GL_STATIC_DRAW );
end;

destructor TOcean.Destroy;
begin
  cFFT.Free;

  glDeleteBuffers( 1, @vbo_indices );
  glDeleteBuffers( 1, @vbo_vertices );

  inherited Destroy;
end;

procedure TOcean.updateShader(Shader: GLuint);
begin
  glProgram:= Shader;

  vertex:= glGetAttribLocation(glProgram, 'vertex');
  normal:= glGetAttribLocation(glProgram, 'normal');
  texture:= glGetAttribLocation(glProgram, 'texture');
  light_position:= glGetUniformLocation(glProgram, 'light_position');
  _projection:= glGetUniformLocation(glProgram, 'Projection');
  _view:= glGetUniformLocation(glProgram, 'View');
  _model:= glGetUniformLocation(glProgram, 'Model');
end;

function TOcean.dispersion(n_prime, m_prime: Integer): Real;
var
  w_0: Real;
  kz: Real;
  kx: Real;
begin
  w_0:= 2.0 * PI / 200.0;
  kx:= PI * ( 2 * n_prime - _N ) / _length;
  kz:= PI * ( 2 * m_prime - _N ) / _length;
  Result:= floor( sqrt( g * sqrt( kx * kx + kz * kz )) / w_0 ) * w_0;
end;

function TOcean.phillips(n_prime, m_prime: Integer): Real;
var
  k: TVector;
  k_length: Float;
  k_length2: Float;
  k_length4: Float;
  k_dot_w2: float;
  w_length: Float;
  L2: Float;
  damping: Float;
  l_2: Float;
begin
  Result:= 0;
  k:= Vector(PI * (2 * n_prime - _N) / _length, PI * (2 * m_prime - _N) / _length, 0 );
  k_length:= k.GetDist();

  if ( k_length < 0.000001 ) then
    exit;

  k_length2:= k_length  * k_length;
  k_length4:= k_length2 * k_length2;

  k_dot_w2:= power( VecNormalize( k ) * VecNormalize( _w ), 2 );

  w_length:= _w.GetDist();
  L2:= power( w_length * w_length / g, 2 );

  damping:= 0.001;
  l_2:= L2 * damping * damping;

  Result:= _A * exp( -1.0 / ( k_length2 * L2 )) / k_length4 * k_dot_w2 * exp( -k_length2 * l_2 );
end;

function TOcean.hTilde_0(n_prime, m_prime: Integer): Complex;

  function gaussianRandomVariable(): complex;
  var
    x1, x2, w: Float;
  begin
    repeat
      x1:= 2 * Random() - 1;
      x2:= 2 * Random() - 1;
      w:= x1 * x1 + x2 * x2;
    until ( w >= 1 );

    w:= sqrt( abs(( -2 * ln( w )) / w ));
    Result:= cinit( x1 * w, x2 * w );
  end;

begin
  Result:= gaussianRandomVariable();
  Result *= sqrt( phillips( n_prime, m_prime ) / 2 );
end;

function TOcean.hTilde(t: Real; n_prime, m_prime: Integer): Complex;
var
  index: Integer;
  htilde0,
  htilde0mkconj: complex;
  omegat: Real;
  c0, c1: complex;
begin
  index:= m_prime * Nplus1 + n_prime;

  htilde0.im:= vertices[ index ].htilde0x;
  htilde0.re:= vertices[ index ].htilde0y;
  htilde0mkconj.im:= vertices[ index ].htilde0mkx;
  htilde0mkconj.re:= vertices[ index ].htilde0mky;

  omegat:= dispersion( n_prime, m_prime ) * t;

  c0.im:= cos( omegat );
  c0.re:= sin( omegat );

  c1:= cong( c0 );

  Result:= htilde0 * c0 + htilde0mkconj * c1;
end;

function TOcean.h_D_and_n(x: TVector; t: Real): complex_vector_normal;
var
  h, c, htilde_c: complex;
  d, n, k: TVector;
  kx, kz, k_length, k_dot_x: Real;
  m_prime: Integer;
  n_prime: Integer;
begin
  for m_prime:= 0 to _N - 1 do
    begin
      kz:= 2 * PI * ( m_prime - _N / 2 ) / _length;
      for n_prime:= 0 to _N - 1 do
        begin
          kx:= 2 * PI * ( n_prime - _N / 2 ) / _length;
          k:= Vector( kx, kz, 0 );

          k_length:= k.GetDist();
          k_dot_x:= k * x;

          c.im:= cos( k_dot_x );
          c.re:= sin( k_dot_x );
          htilde_c:= hTilde( t, n_prime, m_prime ) * c;

          h:= h + htilde_c;

          n:= n + Vector( -kx * htilde_c.re, 0.0, -kz * htilde_c.re );

          if ( k_length < 0.000001 ) then
            continue;
          D:= D + Vector( kx / k_length * htilde_c.re, kz / k_length * htilde_c.re, 0 );
        end;
    end;

  n:= VecNormalize( Vector( 0, 1, 0 ) - n );

  Result.h:= h;
  Result.D:= D;
  Result.n:= n;
end;

procedure TOcean.evaluateWavesFFT(t: Real);
const
  signs: array[ 0..1 ] of Integer = ( 1, -1 );

var
   kx: Real = -1.0;
   kz: Real = -1.0;
   len: Real = -1.0;
   lambda: Real = -1.0;
   index, index1: Integer;
   m_prime: Integer;
   n_prime: Integer;
   sign: Integer;
   n: TVector;
begin
  for m_prime:= 0 to _N - 1 do
    begin
      kz:= PI * ( 2 * m_prime - _N ) / _length;
      for n_prime:= 0 to _N - 1 do
        begin
          kx:= PI * ( 2 * n_prime - _N ) / _length;
          len:= sqrt( kx * kx + kz * kz );
          index:= m_prime * _N + n_prime;

          h_tilde[ index ]:= hTilde( t, n_prime, m_prime );
          h_tilde_slopex[ index ]:= h_tilde[ index ] * cinit( 0, kx );
          h_tilde_slopez[ index ]:= h_tilde[ index ] * cinit( 0, kz );
          if ( len < 0.000001 ) then
            begin
              h_tilde_dx[ index ]:= cinit( 0.0, 0.0 );
              h_tilde_dz[ index ]:= cinit( 0.0, 0.0 );
            end
          else
            begin
              h_tilde_dx[ index ]:= h_tilde[ index ] * cinit( 0, -kx/len );
              h_tilde_dz[ index ]:= h_tilde[ index ] * cinit( 0, -kz/len );
            end;
        end;
    end;

  for m_prime:= 0 to _N - 1 do
    begin
      cFFT.fft( h_tilde, h_tilde, 1, m_prime * _N );
      cFFT.fft( h_tilde_slopex, h_tilde_slopex, 1, m_prime * _N );
      cFFT.fft( h_tilde_slopez, h_tilde_slopez, 1, m_prime * _N );
      cFFT.fft( h_tilde_dx, h_tilde_dx, 1, m_prime * _N );
      cFFT.fft( h_tilde_dz, h_tilde_dz, 1, m_prime * _N );
    end;

  for n_prime:= 0 to _N - 1 do
    begin
      cFFT.fft( h_tilde, h_tilde, _N, n_prime );
      cFFT.fft( h_tilde_slopex, h_tilde_slopex, _N, n_prime );
      cFFT.fft( h_tilde_slopez, h_tilde_slopez, _N, n_prime );
      cFFT.fft( h_tilde_dx, h_tilde_dx, _N, n_prime );
      cFFT.fft( h_tilde_dz, h_tilde_dz, _N, n_prime );
    end;

  for m_prime:= 0 to _N - 1 do
      for n_prime:= 0 to _N - 1 do
        begin
          index:= m_prime * _N + n_prime;     // index into h_tilde..
          index1:= m_prime * Nplus1 + n_prime;    // index into vertices

          sign:= signs[( n_prime + m_prime ) AND 1 ];

          h_tilde[ index ]:= h_tilde[ index ] * sign;

          // height
          vertices[ index1 ].Y:= h_tilde[ index ].im;

          // displacement
          h_tilde_dx[ index ]:= h_tilde_dx[ index ] * sign;
          h_tilde_dz[ index ]:= h_tilde_dz[ index ] * sign;
          vertices[ index1 ].X:= vertices[ index1 ].oX + h_tilde_dx[index].im * lambda;
          vertices[ index1 ].Z:= vertices[ index1 ].oZ + h_tilde_dz[index].im * lambda;

          // normal
          h_tilde_slopex[ index ]:= h_tilde_slopex[ index ] * sign;
          h_tilde_slopez[ index ]:= h_tilde_slopez[ index ] * sign;
          n:= Vector( 0.0 - h_tilde_slopex[index].im, 1.0, 0.0 - h_tilde_slopez[ index ].im ).Normalize;
          vertices[index1].nX:= n.X;
          vertices[index1].nY:= n.Y;
          vertices[index1].nZ:= n.Z;

          // for tiling
          if (( n_prime = 0 ) AND ( m_prime = 0 )) then
            begin
              vertices[ index1 + _N + Nplus1 * _N ].Y:= h_tilde[ index ].im;

              vertices[ index1 + _N + Nplus1 * _N ].X:= vertices[ index1 + _N + Nplus1 * _N ].oX + h_tilde_dx[ index ].im * lambda;
              vertices[ index1 + _N + Nplus1 * _N ].Z:= vertices[ index1 + _N + Nplus1 * _N ].oZ + h_tilde_dz[ index ].im * lambda;

              vertices[ index1 + _N + Nplus1 * _N ].nx:= n.X;
              vertices[ index1 + _N + Nplus1 * _N ].ny:= n.Y;
              vertices[ index1 + _N + Nplus1 * _N ].nz:= n.Z;
            end;
          if ( n_prime = 0 ) then
            begin
              vertices[ index1 + _N ].y:= h_tilde[ index ].im;

              vertices[ index1 + _N ].x:= vertices[ index1 + _N ].ox + h_tilde_dx[ index ].im * lambda;
              vertices[ index1 + _N ].z:= vertices[ index1 + _N ].oz + h_tilde_dz[ index ].im * lambda;

              vertices[ index1 + _N ].nx:=  n.X;
              vertices[ index1 + _N ].ny:=  n.Y;
              vertices[ index1 + _N ].nz:=  n.Z;
            end;
          if ( m_prime = 0 ) then
            begin
              vertices[ index1 + Nplus1 * _N ].y:= h_tilde[index].im;

              vertices[ index1 + Nplus1 * _N ].x:= vertices[ index1 + Nplus1 * _N ].ox + h_tilde_dx[ index ].im * lambda;
              vertices[ index1 + Nplus1 * _N ].z:= vertices[ index1 + Nplus1 * _N ].oz + h_tilde_dz[ index ].im * lambda;

              vertices[ index1 + Nplus1 * _N ].nx:=  n.X;
              vertices[ index1 + Nplus1 * _N ].ny:=  n.Y;
              vertices[ index1 + Nplus1 * _N ].nz:=  n.Z;
            end;
        end;
end;

procedure TOcean.render(t: Real; light_pos: TVector; Proj, View,
  Model: TMatrix4f);

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
  i, j: Integer;
  m: dglOpenGL.TMatrix4f;
begin
  evaluateWavesFFT( t );

  glBindBuffer(GL_ARRAY_BUFFER, vbo_vertices);

  glUniform3f( light_position, light_pos.x, light_pos.y, light_pos.z);
//  Proj:= MatrixTranspose( Proj );
//  View:= MatrixTranspose( Proj );
//  Model:= MatrixTranspose( Proj );
  m:= MatrixToDGL( Proj );
//  ShaderSetParameter4fv( ActiveShader, 'proj', m );
  glUniformMatrix4fv( _projection, 1, ByteBool( GL_FALSE ), @m );
  m:= MatrixToDGL( View );
//  ShaderSetParameter4fv( ActiveShader, 'view', m );
  glUniformMatrix4fv( _view, 1, ByteBool( GL_FALSE ), @m );
  m:= MatrixToDGL( Model );
//  ShaderSetParameter4fv( ActiveShader, 'world', m );
  glUniformMatrix4fv( _model, 1, ByteBool( GL_FALSE ), @m );

  glBufferSubData( GL_ARRAY_BUFFER, 0, sizeof( vertex_ocean ) * Nplus1 * Nplus1, @vertices[ 0 ]);
  glEnableVertexAttribArray( vertex );
  glVertexAttribPointer( vertex, 3, GL_FLOAT, ByteBool( 0 ), sizeof( vertex_ocean ), Pointer( 0 ));
  glEnableVertexAttribArray(normal);
  glVertexAttribPointer(normal, 3, GL_FLOAT, ByteBool( 0 ), sizeof( vertex_ocean ), Pointer( 12 ));
  glEnableVertexAttribArray(texture);
  glVertexAttribPointer(texture, 2, GL_FLOAT, ByteBool( 0 ), sizeof( vertex_ocean ), Pointer( 32 ));

{  glVertexPointer( 3, GL_FLOAT, sizeof( vertex_ocean ), Pointer( 0 ));
  glNormalPointer( GL_FLOAT, sizeof( vertex_ocean ), Pointer( 0 ));}

  glBindBuffer( GL_ELEMENT_ARRAY_BUFFER, vbo_indices );
  for j:= 0 to 9 do
    for i:= 0 to 9 do
      begin
        Model:= MatrixTranslate( Vector( _length * i - ( _length * 5 ), 0, _length * -j + ( _length * 5 )));
        Model:= Model * MatrixScale( Vector( 5, 5, 5 ));
        m:= MatrixToDGL( Model );
        glUniformMatrix4fv( _model, 1, ByteBool( GL_FALSE ), @m );
//        ShaderSetParameter4fv( ActiveShader, 'world', m );

        if ( _geometry ) then
          glDrawElements( GL_LINES, indices_count, GL_UNSIGNED_INT, nil )
        else
          glDrawElements( GL_TRIANGLES, indices_count, GL_UNSIGNED_INT, nil );
      end;
end;

end.

