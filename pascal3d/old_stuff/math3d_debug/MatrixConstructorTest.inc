procedure PrintMat2( m: TMat2 );
begin
  WriteLn(         '┌          ┐');
  WriteLn( Format( '│%1.2f, %1.2f│', [ m[0], m[1] ]));
  WriteLn( Format( '│%1.2f, %1.2f│', [ m[2], m[3] ]));
  WriteLn(         '└          ┘' );
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
end;
