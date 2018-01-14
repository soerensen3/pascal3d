unit pascal3d.events;

{.$DEFINE CHECKFORERRORS} //SLOWER BUT SAFER, FOR DEBUGGING PURPOSES
{.$DEFINE VERBOSE} //WILL OUTPUT DEBUGGING MESSAGES FOR CREATED OBJECTS

{$mode objfpc}{$H+}
{$interfaces CORBA}


interface

uses
  Classes,
  SysUtils,

  XMLWrite,
  XMLRead,
  DOM,
  fpjson,

  pascal3d.utils,

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
  P3DEventsContainers: TP3DJSONRootContainerList = nil;


procedure P3DEventsInit;
procedure P3DEventsFinish;

implementation

uses pascal3d.core;

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dlogging.inc}
{$INCLUDE p3dinput.inc}
{$INCLUDE p3dwindow.inc}
{$INCLUDE p3dapplication.inc}
{$UNDEF IMPLEMENTATION}


procedure P3DEventsInit;
begin
//  if ( not Assigned( P3DEventsContainers )) then
//    P3DEventsContainers:= TP3DJSONRootContainerList.Create( 'P3DEventsContainers' );
  if ( not Assigned( P3DLog )) then
    P3DLog:= TP3DLogger.Create();
  //if ( not Assigned( P3DApplication )) then
  //  P3DApplication:= TP3DApplication.Create;
  if ( not Assigned( P3DInput )) then
    P3DInput:= TP3DInputManager.Create;
  if ( Assigned( P3DClassFactory )) then
    P3DClassFactory.Add( TP3DWindow );
end;

procedure P3DEventsFinish;
begin
  if ( Assigned( P3DInput )) then
    FreeAndNil( P3DInput );
  if ( Assigned( P3DApplication )) then
    FreeAndNil( P3DApplication );
  if ( Assigned( P3DEventsContainers )) then
    FreeAndNil( P3DEventsContainers );
end;

finalization
  P3DEventsFinish;

end.

