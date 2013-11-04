function mat2( aa, ab, ba, bb: Float ): TMat2;
{$IFDEF IMPLEMENTATION}
begin
  Result._00:= aa;
  Result._01:= ab;
  Result._10:= ba;
  Result._11:= bb;
end;
{$ENDIF}

function mat2( c1, c2: TVec2 ): TMat2;
{$IFDEF IMPLEMENTATION}
begin
  Result:= mat2( c1.x, c1.y,
                 c2.x, c2.y );
end;
{$ENDIF}

function mat3( aa, ab, ac, ba, bb, bc, ca, cb, cc: Float ): TMat3;
{$IFDEF IMPLEMENTATION}
begin
  Result._00:= aa;
  Result._01:= ab;
  Result._02:= ac;

  Result._10:= ba;
  Result._11:= bb;
  Result._12:= bc;

  Result._20:= ca;
  Result._21:= cb;
  Result._22:= cc;
end;
{$ENDIF}

function mat3( c1, c2, c3: TVec3 ): TMat3;
{$IFDEF IMPLEMENTATION}
begin
  Result:= mat3( c1.x, c1.y, c1.z,
                 c2.x, c2.y, c2.z,
                 c3.x, c3.y, c3.z );
end;
{$ENDIF}

function mat4( aa, ab, ac, ad, ba, bb, bc, bd, ca, cb, cc, cd, da, db, dc, dd: Float ): TMat4;
{$IFDEF IMPLEMENTATION}
begin
  Result._00:= aa;
  Result._01:= ab;
  Result._02:= ac;
  Result._03:= ad;

  Result._10:= ba;
  Result._11:= bb;
  Result._12:= bc;
  Result._13:= bd;

  Result._20:= ca;
  Result._21:= cb;
  Result._22:= cc;
  Result._23:= cd;

  Result._30:= da;
  Result._31:= db;
  Result._32:= dc;
  Result._33:= dd;
end;
{$ENDIF}

function mat4( c1, c2, c3, c4: TVec4 ): TMat4;
{$IFDEF IMPLEMENTATION}
begin
  Result:= mat4( c1.x, c1.y, c1.z, c1.w,
                 c2.x, c2.y, c2.z, c2.w,
                 c3.x, c3.y, c3.z, c3.w,
                 c4.x, c4.y, c4.z, c4.w );
end;
{$ENDIF}

{$UNDEF IMPLEMENTATION}

