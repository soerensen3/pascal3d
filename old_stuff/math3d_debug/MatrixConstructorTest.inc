procedure PrintMat2( m: TMat2 );
begin
  WriteLn(         '┌          ┐');
  WriteLn( Format( '│%1.2f, %1.2f│', [ m[0], m[1] ]));
  WriteLn( Format( '│%1.2f, %1.2f│', [ m[2], m[3] ]));
  WriteLn(         '└          ┘' );
end;

procedure PrintMat4( m: TMat4 );
begin
  WriteLn(         '┌                      ┐');
  WriteLn( Format( '│%2.2f, %2.2f, %2.2f, %2.2f│', [ m[0], m[1], m[2], m[3] ]));
  WriteLn( Format( '│%2.2f, %2.2f, %2.2f, %2.2f│', [ m[4], m[5], m[6], m[7] ]));
  WriteLn( Format( '│%2.2f, %2.2f, %2.2f, %2.2f│', [ m[8], m[9], m[10], m[11] ]));
  WriteLn( Format( '│%2.2f, %2.2f, %2.2f, %2.2f│', [ m[12], m[13], m[14], m[15] ]));
  WriteLn(         '└                      ┘' );
end;

procedure ConstructorTestMat;
begin
  WriteLn( 'Constructor Test - Matrix' );
  WriteLn( '--------------------------------------------------------------------' );
  WriteLn( 'function mat2( F: Float ): TMat2;' );
  PrintMat2( mat2( 1 ));
  WriteLn( '--------------------------------------------------------------------' );
  WriteLn( 'function mat2( aa, ba, ab, bb ): TMat2;' );
  PrintMat2( mat2( 1.1, 1.2, 2.1, 2.2 ));
  WriteLn( '--------------------------------------------------------------------' );
  WriteLn( 'function mat4perspectiveFOVLH( FOV, Aspect, Near, Far ): TMat4;' );
  PrintMat4( mat4perspectiveFOVLH( 45, 4/3, 0.1, 10 ));
  WriteLn( '--------------------------------------------------------------------' );
end;

