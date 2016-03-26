unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, RTTIGrids, RTTICtrls, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, ComCtrls, Buttons, p3dmodel, p3dSDLApplication, dglOpenGL,
  p3dwindow, p3dshadernodes;

type

  { TForm1 }

  TTabSelection = ( tsObjects, tsCameras, tsLights, tsMaterials, tsMeshes, tsLibraries, tsScenes );

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

procedure Render( Sender: TSDLWindow );

implementation

procedure Render(Sender: TSDLWindow);
begin
  glClearColor($06 / 255, $2C / 255, $29 / 255, 1.0);                      // Set the Background colour of or scene
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);   // Clear the colour buffer
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  DataTab.DataObject:= P3DData;

  TabControl1.Tabs.Append( 'P3DData' );

  TabControl2.BeginUpdate;
  TabControl2.Tabs.Add( 'Objects' );
  TabControl2.Tabs.Add( 'Cameras' );
  TabControl2.Tabs.Add( 'Lights' );
  TabControl2.Tabs.Add( 'Materials' );
  TabControl2.Tabs.Add( 'Meshes' );
  TabControl2.Tabs.Add( 'Libraries' );
  TabControl2.Tabs.Add( 'Scenes' );
  TabControl2.EndUpdate;
  Initialized:= False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  //P3DApplication.MainWindow.Free;
end;

procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if ( Selected ) then
    begin
      if ( Assigned( Item )) then
        TIPropertyGrid1.TIObject:= TPersistent( Item.Data )
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

procedure TForm1.FormActivate(Sender: TObject);
begin
  if ( not Initialized ) then
    begin
      P3DApplication.Initialize;

      P3DApplication.MainWindow:= TSDLWindow.Create;

      P3DApplication.MainWindow.OnRender:= @Render;

      P3DShaderLib.LoadLibraryPath( '../../engine_runtime/shaders/nodes/core/', '*.pmd' );

      Timer1.Enabled:= True;
    end;
end;

procedure TForm1.TabControl2Change(Sender: TObject);
begin
  case TabControl2.Tabs[ TabControl2.TabIndex ] of
    'Objects': DataTab.Selection:= tsObjects;
    'Cameras': DataTab.Selection:= tsCameras;
    'Lights': DataTab.Selection:= tsLights;
    'Materials': DataTab.Selection:= tsMaterials;
    'Meshes': DataTab.Selection:= tsMeshes;
    'Libraries': DataTab.Selection:= tsLibraries;
    'Scenes': DataTab.Selection:= tsScenes;
  end;
  UpdateList;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  P3DApplication.HandleEvents_SDL;
  if ( Assigned( P3DApplication.MainWindow )) then
    P3DApplication.MainWindow.Render;
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

procedure TForm1.Open(FName: String);
begin
  OpenLibrary( FName );
  UpdateFiles;
  UpdateList;
end;

end.

