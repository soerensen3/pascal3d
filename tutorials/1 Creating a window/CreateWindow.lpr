program CreateWindow;

uses
  MainUnit,
  SysUtils;

begin
  MyApplication:= TMyApplication.Create();
  MyApplication.Run;
  FreeAndNil( MyApplication );
end.

