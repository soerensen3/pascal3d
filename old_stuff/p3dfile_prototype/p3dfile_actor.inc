//TP3DFile_Actor.inc
//-------------------------------------------------

{$IFDEF INTERFACE}

TP3DActor = class( TP3DDataBlock )
  Position: TVec3;
  Rotation: TVec3;
  Data: TP3DDataBlock;
  procedure LoadFromDom(...);
  ...
end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}
{$ENDIF}
