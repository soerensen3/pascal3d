unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, RTTICtrls, Forms, Graphics,
  {$IFDEF LCLGTK2}
  gtk2, xlib, x,gdk2x,gdk2,
  {$ENDIF}
  Dialogs, ExtCtrls, ComCtrls, Buttons, Math, p3dgraphics, p3devents, dglOpenGL,
  OpenGLContext,
  p3dutils, rttiutils, typinfo, p3dRTTI, p3dMath, p3dgui, p3dgui_buttons,
  Controls;

type

  { TForm1 }

  TTabSelection = ( tsObjects, tsCameras, tsLights, tsMaterials, tsMeshes,
                    tsLibraries, tsScenes, tsTextures, tsShaders );

  TDataTab = object
    DataObject: TP3DData;
    Selection: TTabSelection;
  end;

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    ListView1: TListView;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    TabControl1: TTabControl;
    TabControl2: TTabControl;
    Timer1: TTimer;
    TIPropertyGrid1: TTIPropertyGrid;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure TabControl1Change(Sender: TObject);
    procedure TabControl2Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    Initialized: Boolean;
  public
    procedure UpdateList;
    procedure UpdateFiles;
    procedure Append( FName: String );
    procedure Open( FName: String );
    { public declarations }
  end;

var
  Form1: TForm1;
  DataTab: TDataTab;
  TestCanvas: TP3DCanvas2D;
  Button1: TP3DButton;
  TestText: TP3DText;
  TestScene: TP3DScene;
  TestCam: TP3DActor;
  CamRotation: TVec3;
  CamDistance: Single = 10;
  WindowGL: TOpenGLControl;

procedure Render( Sender: TP3DWindow );
procedure Input( Sender: TP3DApplication );

implementation



procedure Render(Sender: TP3DWindow);
begin
  WindowGL.MakeCurrent();
  P3DGUIManager.Input;
  P3DViewports[ 0 ]:= P3DViewport( 0, 0, WindowGL.Width, WindowGL.Height );

  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer

  //TestCanvas.Lock;
  //TestCanvas.RenderLineCircle( vec2( 100, 100 ), 25, 5, vec4( 1 ));
  //TestCanvas.Unlock();


  glEnable( GL_DEPTH_TEST );
  TestScene.Render;
  glDisable( GL_DEPTH_TEST );

  P3DGUIManager.Render;
  WindowGL.SwapBuffers;
  {TestCanvas.Lock;
  TestCanvas.RenderText( TestText, vec2( 25, 25 ));
  TestCanvas.Unlock();}
end;

procedure Input(Sender: TP3DApplication);
var
  m: TMat4;
begin
  if ( P3DInput.Mouse.Buttons[ 0 ]) then
    TestCam.Rotation:= TestCam.Rotation + vec3( P3DInput.Mouse.DY, 0, P3DInput.Mouse.DX ) * 0.5;
  if ( P3DInput.Mouse.Buttons[ 2 ]) then
    CamDistance:= Max( 0.1, CamDistance + P3DInput.Mouse.DY * 0.2 );
  m:= TP3DCamera( TestCam.Data ).View;
  mat4inverse( m, m );
  TestCam.Position:= -m.Row[ 2 ].xyz * CamDistance;
  //TestCam.Position:= -TP3DCamera( TestCam.Data ).View.Row[ 1 ].xyz * Distance;
end;

{$R *.lfm}


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  TabControl1.Tabs.Append( 'P3DData' );

  TabControl2.BeginUpdate;
  TabControl2.Tabs.Add( 'Objects' );
  TabControl2.Tabs.Add( 'Cameras' );
  TabControl2.Tabs.Add( 'Lights' );
  TabControl2.Tabs.Add( 'Materials' );
  TabControl2.Tabs.Add( 'Meshes' );
  TabControl2.Tabs.Add( 'Libraries' );
  TabControl2.Tabs.Add( 'Scenes' );
  TabControl2.Tabs.Add( 'Textures' );
  TabControl2.Tabs.Add( 'Shaders' );
  TabControl2.EndUpdate;
  Initialized:= False;
  //FormActivate( Self );
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  TestText.Free;
  P3DGUIFinish;
  P3DGraphicssFinish;
  P3DEventsFinish;
  //P3DApplication.MainWindow.Free;
end;


procedure Debug( Obj: TPersistent );
  procedure DumpList( L: TP3DPropList; const Indent: Integer = 0 );
  var
    i: Integer;
    List: specialize IP3DEnumerable < TPersistent >;
    L_New: TP3DPropList;
    Enum: specialize IP3DEnumerator < TPersistent >;
  begin
    for i:= 0 to L.Count - 1 do
      begin
        WriteLn( StringOfChar( ' ', Indent * 2 ), L[ i ].Name, ' : ', L[ i ].TypeName, ' RO:', L[ i ].IsReadOnly, ' = ', L[ i ].Value );
        if ( L[ i ].CanHaveChilds ) then
          if ( L[ i ].CreateChilds ) then
            begin
              DumpList( L[ i ].Childs, Indent + 1 );

              if (( Indent < 3 ) and Supports( L[ i ].Childs.Obj, IP3DEnumerable, List )) then
                begin
                  Enum:= List.GetEnumerator;
                  while Enum.MoveNext do
                    begin
                      L_New:= TP3DPropList.Create;
                      L_New.Obj:= Enum.GetCurrent;
                      DumpList( L_New, Indent + 2 );
                      L_New.Free;
                    end;
                end;
            end;
      end;
  end;

var
  Lst: TP3DPropList;
begin
  Lst:= TP3DPropList.Create;
  Lst.Obj:= Obj;
  DumpList( Lst, 0 );
  Lst.Free;
end;

procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if ( Selected ) then
    begin
      if ( Assigned( Item )) then
        begin
          Debug( TPersistent( Item.Data ));
          TIPropertyGrid1.TIObject:= TPersistent( Item.Data )
        end
      else
        TIPropertyGrid1.TIObject:= nil;
    end;
end;

procedure TForm1.TabControl1Change(Sender: TObject);
begin
  if ( TabControl1.TabIndex = 0 ) then
    DataTab.DataObject:= P3DData
  else
    DataTab.DataObject:= P3DData.Libraries[ TabControl1.TabIndex - 1 ];
  UpdateList;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if ( TabControl1.TabIndex > 0 ) then
    begin
      P3DData.Libraries[ TabControl1.TabIndex - 1 ].Free;
      P3DData.Libraries.Delete( TabControl1.TabIndex - 1 );
      UpdateFiles;
      UpdateList;
    end;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  if ( OpenDialog1.Execute ) then
    Open( OpenDialog1.FileName );
end;

function GetWindowPtr( Comp: TWinControl ): Pointer;
begin
  {$IFDEF LCLWIN32}
  Result:= Comp.Handle;
  {$ENDIF}
  {$IFDEF LCLGTK2}
    Result:= Pointer( GDK_WINDOW_XID( PGtkWidget( Comp.Handle )^.window ));
  {SDL_VERSION( @info.version );
  if ( Boolean( SDL_GetWindowWMInfo( P3DApplication.MainWindow.Window, @info ))) then
    WriteLn( GetEnumName( TypeInfo( TSDL_SYSWM_TYPE ), Ord( info.subsystem )));
  case info.subsystem of
    SDL_SYSWM_UNKNOWN: WriteLn( 'SDL_SYSWM_UNKNOWN' );
    SDL_SYSWM_WINDOWS: WriteLn( 'SDL_SYSWM_WINDOWS' );
    SDL_SYSWM_X11: WriteLn( 'SDL_SYSWM_X11' );
    SDL_SYSWM_DIRECTFB: WriteLn( 'SDL_SYSWM_DIRECTFB' );
    SDL_SYSWM_COCOA: WriteLn( 'SDL_SYSWM_COCOA' );
    SDL_SYSWM_UIKIT: WriteLn( 'SDL_SYSWM_UIKIT' );
    SDL_SYSWM_WAYLAND: WriteLn( 'SDL_SYSWM_WAYLAND' );
    SDL_SYSWM_MIR: WriteLn( 'SDL_SYSWM_MIR' );
    SDL_SYSWM_WINRT: WriteLn( 'SDL_SYSWM_WINRT' );
    SDL_SYSWM_ANDROID: WriteLn( 'SDL_SYSWM_ANDROID' );
  end;

  WriteLn( Ord( info.subsystem ));

  XReparentWindow( info.x11.display, info.x11.window, GDK_WINDOW_XID( PGtkWidget( Handle )^.window ), 0, 0 );}
  {WriteLn( Integer( gdk_window_lookup( info.x11.window )));
  WriteLn( Integer( PGtkWidget( Handle )^.window ));
  gdk_window_reparent( gdk_window_lookup( info.x11.window ), PGtkWidget( Handle )^.window, 0, 0 );}
  {$ENDIF}
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  if ( not Initialized ) then
    begin
      P3DEventsInit;
      P3DLog.FileName:= 'Logfile.xml';
      WindowGL:= TOpenGLControl.Create( Panel2 );
      WindowGL.Parent:= Panel2;
      WindowGL.Align:= alClient;
      P3DApplication.MainWindow:= TP3DWindow.CreateDummyWindow;//TP3DWindow.CreateFrom( GetWindowPtr( WindowGL ));
      P3DApplication.Initialize;
      P3DGraphicsInit;
      P3DUtilsInit;
      P3DSearchPaths.BaseDir:= '../../engine_runtime' ;
      P3DSearchPaths[ p3dsc_Fonts ].Add( 'fonts/' );

      P3DApplication.MainWindow.OnRender:= @Render;
      P3DApplication.OnInput:= @Input;
      P3DShaderNodeLib.LoadLibraryPath( '../../engine_runtime/shaders/nodes/core/', '*.pmd' );
      P3DGUIInit;

      TestText:= p3dTextSimple( 'ABCDEFG', P3DFontManager.Fonts['DejaVuSans', 16 ]);
      Timer1.Enabled:= True;

      DataTab.DataObject:= P3DData;
      TestCanvas:= TP3DCanvas2D.Create( P3DData.DataBlocks );
      TestCanvas.Width:= P3DApplication.MainWindow.Width;
      TestCanvas.Height:= P3DApplication.MainWindow.Height;

      glEnable( GL_LINE_SMOOTH );
      glEnable( GL_DEPTH_TEST );
      glCullFace( GL_NONE );

      P3DGUIManager.Window:= P3DApplication.MainWindow;
      P3DGUIManager.ShowCursor:= True;

      Button1:= TP3DButton.Create( P3DData.DataBlocks, P3DGUIManager );
      Button1.Width:= 200;
      Button1.Height:= 100;
      P3DGUIManager.Controls.Realign;

      TestScene:= TP3DScene.Create();
      TestScene.Cam:= TP3DActor.Create();
      TestScene.Cam.Data:= TP3DCamera.Create();
      TP3DCamera( TestScene.Cam.Data ).Far:= 2000;
      TestScene.Cam.Position:= vec3( 0, 1, -10 );
      TestScene.Cam.Rotation:= vec3( 90, 0, 180 );
      TestCam:= TestScene.Cam;
    end;
end;

procedure TForm1.TabControl2Change(Sender: TObject);
begin
  case TabControl2.Tabs[ TabControl2.TabIndex ] of
    'Objects':   DataTab.Selection:= tsObjects;
    'Cameras':   DataTab.Selection:= tsCameras;
    'Lights':    DataTab.Selection:= tsLights;
    'Materials': DataTab.Selection:= tsMaterials;
    'Meshes':    DataTab.Selection:= tsMeshes;
    'Libraries': DataTab.Selection:= tsLibraries;
    'Scenes':    DataTab.Selection:= tsScenes;
    'Textures':  DataTab.Selection:= tsTextures;
    'Shaders':   DataTab.Selection:= tsShaders;
  end;
  UpdateList;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  P3DApplication.HandleEvents_SDL;
  if ( Assigned( P3DApplication.MainWindow )) then
    P3DApplication.MainWindow.Render;
  P3DInput.NextCycle;
end;

procedure TForm1.UpdateList;
var
  i: Integer;
begin
  ListView1.Clear;
  case DataTab.Selection of
    tsObjects:
      for i:= 0 to DataTab.DataObject.Objects.Count - 1 do
        ListView1.AddItem( DataTab.DataObject.Objects[ i ].Name, DataTab.DataObject.Objects[ i ]);
    tsCameras:
      for i:= 0 to DataTab.DataObject.Cameras.Count - 1 do
        ListView1.AddItem( DataTab.DataObject.Cameras[ i ].Name, DataTab.DataObject.Cameras[ i ]);
    tsLights:
      for i:= 0 to DataTab.DataObject.Lights.Count - 1 do
        ListView1.AddItem( DataTab.DataObject.Lights[ i ].Name, DataTab.DataObject.Lights[ i ]);
    tsMaterials:
      for i:= 0 to DataTab.DataObject.Materials.Count - 1 do
        ListView1.AddItem( DataTab.DataObject.Materials[ i ].Name, DataTab.DataObject.Materials[ i ]);
    tsMeshes:
      for i:= 0 to DataTab.DataObject.Meshes.Count - 1 do
        ListView1.AddItem( DataTab.DataObject.Meshes[ i ].Name, DataTab.DataObject.Meshes[ i ]);
    tsLibraries:
      for i:= 0 to DataTab.DataObject.Libraries.Count - 1 do
        ListView1.AddItem( DataTab.DataObject.Libraries[ i ].FileWatch.FileName, DataTab.DataObject.Libraries[ i ]);
    tsScenes:
      for i:= 0 to DataTab.DataObject.Scenes.Count - 1 do
        ListView1.AddItem( DataTab.DataObject.Scenes[ i ].Name, DataTab.DataObject.Scenes[ i ]);
    tsTextures:
      for i:= 0 to DataTab.DataObject.Textures.Count - 1 do
        ListView1.AddItem( DataTab.DataObject.Textures[ i ].Name, DataTab.DataObject.Textures[ i ]);
    tsShaders:
      for i:= 0 to DataTab.DataObject.Shaders.Count - 1 do
        ListView1.AddItem( DataTab.DataObject.Shaders[ i ].Name, DataTab.DataObject.Shaders[ i ]);
  end;
end;

procedure TForm1.UpdateFiles;
var
  i: Integer;
begin
  TabControl1.Tabs.Clear;
  TabControl1.Tabs.Add( 'P3DData' );
  for i:= 0 to P3DData.Libraries.Count - 1 do
    TabControl1.Tabs.Add( ExtractFileName( P3DData.Libraries[ i ].FileWatch.Filename ));
end;

procedure TForm1.Append( FName: String );
begin
  DataTab.DataObject.AppendFile( FName );
  UpdateList;
end;

procedure TForm1.Open( FName: String );
var
  n: Integer;
  Obj: TP3DActor;
begin
  n:= OpenLibrary( FName );
  UpdateFiles;
  UpdateList;
  for Obj in P3DData.Libraries[ n ].Objects do
    begin
      {if ( Obj.Data is TP3DCamera ) then
        begin
          //TestCam:= Obj;
          TestScene.Cam:= Obj;//TP3DCamera( TestCam.Data );
        end;}
      TestScene.Objects.Add( Obj );
    end;
end;

end.

