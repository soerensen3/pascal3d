unit P3DEditorMainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, ExtCtrls, Buttons, IDEWindowIntf, MenuIntf, p3dlcleditorfile,
  SDL2, p3dSDLApplication, typinfo, p3dinput,
  {$IFDEF LCLGTK2}
  gtk2, xlib, x,gdk2x,gdk2,
  {$ENDIF}

  p3dwindow;

type

  { TP3DEditorForm }

  TP3DEditorForm = class(TForm)
    published
      AppProps: TApplicationProperties;
      MMFileOpen: TMenuItem;
      MMFile: TMenuItem;
      MMnu: TMainMenu;
      OpenDlg: TOpenDialog;
      Pages: TPageControl;
      Panel1: TPanel;
      SpeedButton1: TSpeedButton;
      procedure AppPropsIdle(Sender: TObject; var Done: Boolean);
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure MMFileOpenClick(Sender: TObject);
      function CreateBuffer( AFileName: String ): TP3DEditorFile;
      procedure PagesChange(Sender: TObject);
      procedure PagesCloseTabClicked(Sender: TObject);
    private
      FActiveFileIdx: Integer;
      FFiles: TP3DEditorFileList;
      SDLNeedsInit: Boolean;
      procedure SetActiveFileIdx(AValue: Integer);
      { private declarations }
    public
      procedure InitSDL;
      property Files: TP3DEditorFileList read FFiles write FFiles;
      property ActiveFileIdx: Integer read FActiveFileIdx write SetActiveFileIdx;
  end;

  procedure Register;


var
  P3DEditorForm: TP3DEditorForm = nil;
  P3DEditorFormCreator: TIDEWindowCreator;


implementation

{$R *.lfm}


procedure CreateMyIDEWindow(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  // check the name
  if CompareText(aFormName,'P3DEditor')<>0 then
    begin
      //DebugLn(['ERROR: CreateFileBrowser: there is already a form with this name']);
      exit;
    end;
  // create the form if not already done and disable autosizing
  IDEWindowCreators.CreateForm(P3DEditorForm,TP3DEditorForm,DoDisableAutosizing,Application);
  AForm:= P3DEditorForm;
  AForm.Name:= aFormName;
end;

procedure ShowP3DEditor( Sender: TObject );
begin
  IDEWindowCreators.ShowForm(P3DEditorFormCreator.FormName,false);
end;

procedure Register;
begin
  P3DEditorFormCreator:= IDEWindowCreators.Add('P3DEditor',@CreateMyIDEWindow,nil,'100','50%','+300','+20%');
  RegisterIDEMenuCommand( itmSecondaryTools, 'P3DEditor','Pascal3D Editor',nil,@ShowP3DEditor );
  // the default boundsrect of the form is:
  // Left=100, Top=50% of Screen.Height, Width=300, Height=20% of Screen.Height
  // when the IDE needs an instance of this window it calls the procedure CreateMyIDEWindow.
end;


{ TP3DEditorForm }

procedure TP3DEditorForm.MMFileOpenClick(Sender: TObject);
begin
  if ( OpenDlg.Execute ) then
    CreateBuffer( OpenDlg.FileName ).MakeActive;
  if ( ActiveFileIdx <> Pages.ActivePageIndex ) then
    ActiveFileIdx:= Pages.ActivePageIndex;
end;

procedure TP3DEditorForm.FormDestroy(Sender: TObject);
var
  F: TP3DEditorFile;
begin
  for F in Files do
    F.Page.Free;
  Files.Free;
end;

procedure TP3DEditorForm.FormCreate(Sender: TObject);
begin
  Files:= TP3DEditorFileList.Create;
  ActiveFileIdx:= -1;
  Pages.ActivePageIndex:= -1;
  SDLNeedsInit:= True;  //No valid window XID/Handle yet
end;


procedure TP3DEditorForm.AppPropsIdle(Sender: TObject; var Done: Boolean);
var
  F: TP3DEditorFile;
begin
  Done:= False;
  //Application.ProcessMessages;
  P3DApplication.HandleEvents_SDL;
  if ( Assigned( Files )) then
    for F in Files do
      F.FileIdle();
  if ( ActiveFileIdx >= 0 ) then
    Files[ ActiveFileIdx ].FileRender();
  p3dinput.InputManager.NextCycle;
end;

function TP3DEditorForm.CreateBuffer(AFileName: String): TP3DEditorFile;
var
  Ext: String;
  FileClass: TP3DEditorFileClass;
begin
  if ( SDLNeedsInit ) then
    InitSDL;
  Ext:= ExtractFileExt( AFileName );
  FileClass:= FindExtension( Ext );
  if ( not Assigned( FileClass )) then
    raise Exception.Create( Format( 'Cannot open file "%s"! The specified extension "%s" is not registered', [ AFileName, Ext ]));
  if ( not Assigned( Files )) then
    Files:= TP3DEditorFileList.Create;
  Result:= FileClass.Create( AFileName, Pages.AddTabSheet );
  Result.Page.Caption:= Result.Title;
  Files.Add( Result );
end;

procedure TP3DEditorForm.PagesChange(Sender: TObject);
begin
  ActiveFileIdx:= Pages.ActivePageIndex;
end;

procedure TP3DEditorForm.PagesCloseTabClicked(Sender: TObject);
var
  i: Integer;
begin
  for i:= 0 to Files.Count - 1 do
    if ( Files[ i ].Page = Sender ) then
      begin
        Files[ i ].Free;
        Files.Delete( i );
        Sender.Free;
        break;
      end;
end;

procedure TP3DEditorForm.SetActiveFileIdx(AValue: Integer);
begin
  if FActiveFileIdx=AValue then Exit;
  if (( AValue < Files.Count ) and ( AValue >= -1 )) then
    FActiveFileIdx:=AValue;
end;

procedure TP3DEditorForm.InitSDL;
{$IFDEF LCLGTK2}
var
  info: TSDL_SysWMinfo;
{$ENDIF}
begin
  P3DApplication.Initialize;
  SDL_SetHint(SDL_HINT_RENDER_DRIVER, 'opengl');
  P3DApplication.MainWindow:= TSDLWindow.Create;
  SDLNeedsInit:= False;
  {$IFDEF LCLWIN32}
  //P3DApplication.MainWindow:= TSDLWindow.CreateFrom( Handle );
  {$ENDIF}
  {$IFDEF LCLGTK2}
  //P3DApplication.MainWindow:= TSDLWindow.CreateFrom( Pointer( GDK_WINDOW_XID( PGtkWidget( Handle )^.window )));
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

end.

