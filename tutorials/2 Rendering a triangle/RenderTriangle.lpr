program RenderTriangle;

uses
  MainUnit,
  SysUtils;

begin
  DeleteFile( 'heap.trc' );
  SetHeapTraceOutput( 'heap.trc' );
  MyApplication:= TMyApplication.Create();
  MyApplication.Run;
  FreeAndNil( MyApplication );
end.

