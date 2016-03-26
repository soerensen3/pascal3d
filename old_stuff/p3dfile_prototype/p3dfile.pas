unit p3dfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  //STUBS
  TP3DObject = class
  end;
  TP3DFileWatch = class
  end;

  //

  {$DEFINE INTERFACE}

  {$INCLUDE p3dfile_datablock}
  {$INCLUDE p3dfile_actor}

  {$UNDEF INTERFACE}

implementation

  {$DEFINE IMPLEMENTATION}

  {$INCLUDE p3dfile_datablock}

  {$UNDEF IMPLEMENTATION}

end.

