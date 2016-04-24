unit p3devents;

{.$DEFINE CHECKFORERRORS} //SLOWER BUT SAFER, FOR DEBUGGING PURPOSES
{.$DEFINE VERBOSE} //WILL OUTPUT DEBUGGING MESSAGES FOR CREATED OBJECTS

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,

  XMLWrite,
  DOM,

  SDL2
  ;

{$DEFINE INTERFACE}
{$INCLUDE p3dlogging.inc}
{$INCLUDE p3dinput.inc}
{$INCLUDE p3dwindow.inc}
{$INCLUDE p3dapplication.inc}
{$UNDEF INTERFACE}

var
  P3DLog: TP3DLogger;
  P3DInput: TP3DInputManager;
  P3DApplication: TP3DApplication;

procedure P3DEventsInit;
procedure P3DEventsFinish;

implementation

uses p3dgraphics;

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dlogging.inc}
{$INCLUDE p3dinput.inc}
{$INCLUDE p3dwindow.inc}
{$INCLUDE p3dapplication.inc}
{$UNDEF IMPLEMENTATION}


procedure P3DEventsInit;
begin
  if ( not Assigned( P3DLog )) then
    P3DLog:= TP3DLogger.Create;
  if ( not Assigned( P3DApplication )) then
    P3DApplication:= TP3DApplication.Create;
  if ( not Assigned( P3DInput )) then
    P3DInput:= TP3DInputManager.Create;
end;

procedure P3DEventsFinish;
begin
  if ( Assigned( P3DInput )) then
    FreeAndNil( P3DInput );
  if ( Assigned( P3DApplication )) then
    FreeAndNil( P3DApplication );
  if ( Assigned( P3DLog )) then
    FreeAndNil( P3DLog );
end;

finalization
  P3DEventsFinish;

end.

