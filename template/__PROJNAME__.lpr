program __PROJNAME__;

{$IFDEF WINDOWS}
  {$APPTYPE CONSOLE}
  {$R *.res}
{$ENDIF}

uses
  strutils,
  LCLIntf,
  sysutils,
  Classes,
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  Interfaces, //crashes in linux without
  {$ENDIF}{$ENDIF}
  p3devents,
  p3dgraphics,
  p3dutils,
  __MAINUNIT__;


Begin
  try
    //Initialize all the Pascal3D subsystems
    P3DUtilsInit;
    P3DEventsInit;
    P3DGraphicsInit;

    //Create the application from your class
    P3DApplication:= __appclassname__.Create;

    //Load the config which for example contains paths for assets and shaders and setups the main window
    P3DApplication.LoadConfig( 'settings_default.xml' );

    //Initialize everything and run the application
    P3DApplication.Initialize;
    P3DApplication.Run;
  except
    On E: Exception do
      P3DLog.LogException( nil, E ); // On Exception we add a message to the log.
      //This is saved to a html file after each message. The logpath is in the config.
  end;
  //Finalize Pascal3D subsystems
  //Note that this is optional (finalization section of each unit will call these functions
  //anyway) but this way we have more control in which order they are called
  P3DGraphicsFinish;
  P3DEventsFinish;
  P3DUtilsFinish;
End.
