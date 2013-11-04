unit FFT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Math,
  UComplex;

type
  TComplexField = array of complex;

  { TFFT }

  TFFT = class
    private
      _N, which, log_2_N: Cardinal;
      pi2: Real;
      reversed: array of Cardinal;
      _W: array of TComplexField;
      c: array [ 0..1 ] of TComplexField;

    public
      constructor Create( N: Cardinal );
      destructor Destroy; override;

      function reverse( i: Cardinal ): Cardinal;
      function w( x, N: Cardinal ): complex;
      procedure fft( var input: TComplexField; var output: TComplexField; stride: Integer; offset: Integer );
  end;
{
  private:
	unsigned int N, which;
	unsigned int log_2_N;
	float pi2;
	unsigned int *reversed;
	complex **W;
	complex *c[2];
  protected:
  public:
	cFFT(unsigned int N);
	~cFFT();
	unsigned int reverse(unsigned int i);
	complex w(unsigned int x, unsigned int N);
	void fft(complex* input, complex* output, int stride, int offset);
}

implementation

{ TFFT }

constructor TFFT.Create(N: Cardinal);
var
  i: Integer;
  j: Integer;
  pow2: Integer;
begin
  _N:= N;
  pi2:= 2 * PI;

  log_2_N:= Round( ln( N ) / ln( 2 )); //oder div?

//  reversed:= new unsigned int[N];		// prep bit reversals
  SetLength( reversed, N );
  for i:= 0 to N-1 do
    reversed[ i ]:= reverse( i );

  pow2:= 1;

//  W = new complex*[log_2_N];		// prep W
  SetLength( _W, log_2_N );
  for i:= 0 to log_2_N - 1 do
    begin
      SetLength( _W[ i ], pow2 );
      for j:= 0 to pow2 - 1 do
        _W[ i ][ j ]:= w( j, pow2 * 2 );
      pow2 *= 2;
    end;
  SetLength( c[ 0 ], N );
  SetLength( c[ 1 ], N );
  which:= 0;
end;

destructor TFFT.Destroy;
begin
  inherited Destroy;
end;

function TFFT.reverse(i: Cardinal): Cardinal;
var
  j: Integer;
begin
  Result:= 0;
  for j:= 0 to log_2_N - 1 do
    begin
      Result:= (Result shl 1) + (i AND 1);
      i:= i shr 1;
    end;
end;

function TFFT.w(x, N: Cardinal): complex;
begin
  Result.im:= cos( pi2 * x / N );
  Result.re:= sin( pi2 * x / N );
end;

procedure TFFT.fft(var input: TComplexField; var output: TComplexField;
  stride: Integer; offset: Integer);
var
  i: Integer;
  loops: Integer;
  size: Integer;
  size_over_2: Integer;
  w_: Integer;
  j: Integer;
  k: Integer;
begin
  for i:= 0 to _N - 1 do
    c[ which ][ i ]:= input[ reversed[ i ] * stride + offset ];

  loops:= _N shr 1;
  size:= 1 shl 1;
  size_over_2:= 1;
  w_:= 0;

  for i:= 1 to log_2_N do
    begin
      which:= which xor 1;

      for j:= 0 to loops - 1 do
        begin
          for k:= 0 to size_over_2 - 1 do
  	    c[ which ][ size * j + k ]:= c[ which xor 1 ][ size * j + k ] +
                                         c[ which xor 1 ][ size * j + size_over_2 + k ] * _W[ w_ ][ k ];

  	  for k:= size_over_2 to size - 1 do
  	    c[ which ][ size * j + k ]:= c[ which xor 1 ][ size * j - size_over_2 + k ] -
  	                                 c[ which xor 1 ][ size * j + k] * _W[ w_ ][ k - size_over_2 ];
        end;
  	loops:= loops shr 1;
  	size:= size shl 1;
  	size_over_2:= size_over_2 shl 1;
  	inc( w_ );
    end;
  for i:= 0 to _N - 1 do
    output[ i * stride + offset ]:= c[ which ][ i ];
end;

end.

