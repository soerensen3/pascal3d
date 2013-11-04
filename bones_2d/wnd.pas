unit wnd;

{$mode objfpc}{$H+}
{$DEFINE USE_ZENGL_STATIC}

interface

uses
  vars,
  compo,
  sysutils,
  bones,

  {$IFDEF USE_ZENGL_STATIC}
  // RU: При использовании статической компиляции необходимо подключать модули ZenGL содержащие необходимый функционал.
  // EN: Using static compilation needs to use ZenGL units with needed functionality.
  zgl_main,
  zgl_screen,
  zgl_window,
  zgl_timers,
  zgl_utils,
  zgl_mouse,
  zgl_font,
  zgl_text,
  zgl_primitives_2d,
  zgl_sprite_2d,
  zgl_textures
  {$ELSE}
  // RU: Используя ZenGL в качестве библиотеки(so, dll или dylib) нужен всего один заголовочный файл.
  // EN: Using ZenGL as a shared library(so, dll or dylib) needs only one header.
  zglHeader
  {$ENDIF}
  ;

type

{  { TMainForm }

  TMainForm = class( TForm )
    Panel1: TPanel;
  published
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure Panel1Resize(Sender: TObject);
    { private declarations }
  public
    { public declarations }
  end;
}

  { TMyProgram }

  TMyProgram = class
    procedure Init;
    procedure Draw;
    procedure Input;
    procedure Deinit;
  end;


  { TImg }

  TImg = record
    FName: String;
    Shortname: String;
    Tex: zglPTexture;
  end;

  { TBoneInterface }

  TBoneInterface = class
    ImgList: array of TImg;
    BonePainter: TBonePainter;
    LBImg: TListBox;
    BtnAddBone: TButton;
    Ctrls: TGraphicList;

    BoneList: TBoneList;

    procedure GenerateImgList( Dir: String );

    constructor Create;
    destructor Destroy; override;

    procedure Draw;
    procedure Input;

    procedure ButtonAddBone( Sender: TGraphicControl; X,Y: Integer );
    function LBGetCount( Sender: TListBox ): Integer;
    function LBGetItem( Sender: TListBox; Index: Integer ): String;
  end;

  { TBoneHud }

  TBoneHud = class
    Selection  : TSelection;
    Screen     : TScreen;

    procedure ButtonClickBoneImg( Sender: TGraphicControl; X,Y: Integer );
    procedure Draw;
    procedure Input;

    constructor Create;
    destructor Destroy; override;
  end;

var
//  MainForm: TMainForm;
  MyProgram: TMyProgram;

  BoneInterface: TBoneInterface;
  BoneHud: TBoneHud;

  zglInited  : Boolean;
  zglResized : Boolean;

implementation

uses GL;

{ TBoneHud }

procedure TBoneHud.ButtonClickBoneImg(Sender: TGraphicControl; X, Y: Integer);
begin
  Selection.Obj:= Sender as TImage;
end;

procedure TBoneHud.Draw;
begin
  Screen.Draw;
//  Selection.Draw;
end;

procedure TBoneHud.Input;
begin
  BoneHud.Screen.MouseMove( mouse_X(), mouse_Y());
  if ( mouse_Click( M_BLEFT )) then
    BoneHud.Screen.MouseClick( mouse_X(), mouse_Y());
end;

constructor TBoneHud.Create;
begin
  inherited;
  Screen:= TScreen.Create;
  Screen.W:= zgl_Get( VIEWPORT_WIDTH );
  Screen.H:= zgl_Get( VIEWPORT_HEIGHT );

  Screen.Items:= TGraphicList.Create;
  Selection:= TSelection.Create;
  Screen.Items.Add( Selection );
end;

destructor TBoneHud.Destroy;
begin
//  Selection.Free;
  FreeList( Screen.Items );
  Screen.Free;
  inherited Destroy;
end;

{ TBoneInterface }

procedure TBoneInterface.GenerateImgList(Dir: String);
  var
    SR: TSearchRec;
begin
  if ( FindFirst( Dir + '*.*', faAnyFile, SR) = 0 ) then
    begin
      repeat
        if ( SR.Attr <> faDirectory ) then
          begin
            SetLength( ImgList, Length( ImgList ) + 1 );
            with( ImgList[ high( ImgList )]) do
              begin
                FName:= Dir + '/' + SR.Name;
                Shortname:= ExtractFileName( FName );
                Tex:= tex_LoadFromFile( FName );
              end;
          end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
end;

constructor TBoneInterface.Create;
begin
  Ctrls:= TGraphicList.Create;

  LBImg:= TListBox.Create;
  LBImg.W:= 200;
  LBImg.H:= 300;
  LBImg.GetCount:= @LBGetCount;
  LBImg.GetItem:= @LBGetItem;
  Ctrls.Add( LBImg );

  GenerateImgList( ExtractFilePath( ParamStr( 0 )) + '../' + dirRes + 'img'  + '/' );

  BoneList:= TBoneList.Create;

  BtnAddBone:= TButton.Create;
  with ( BtnAddBone ) do
    begin
      W:= 100;
      H:= 30;
      X:= 10;
      Y:= 310;
      Caption:= 'Add Bone';
      OnClick:= @ButtonAddBone;
    end;
  Ctrls.Add( BtnAddBone );

  BonePainter:= TBonePainter.Create;
  BonePainter.DrawLines:= True;
  BonePainter.Bones:= BoneList;
  Ctrls.Add( BonePainter );
end;

destructor TBoneInterface.Destroy;
begin
  BoneList.Free;
  FreeList( Ctrls );
  inherited Destroy;
end;

procedure TBoneInterface.Draw;
begin
  DrawList( Ctrls );
end;

procedure TBoneInterface.Input;
begin
  if ( mouse_Click( 0 )) then
    WriteLn( 'Mouse Click' );
  MouseList( Ctrls, False, True );
  if ( mouse_Click( 0 )) then
    MouseList( Ctrls, True, False );
end;


procedure TBoneInterface.ButtonAddBone(Sender: TGraphicControl; X, Y: Integer);
begin
  if (( LBImg.Selection >= 0 ) AND ( LBImg.Selection < Length( ImgList ))) then
    with ( BoneList[ BoneList.Add( TBone.Create )]) do
      begin
        Graphic:= BoneHud.Screen.Items[ BoneHud.Screen.Items.Add( TImage.Create )] as TImage;
        with Graphic , (ImgList[ LBImg.Selection ]) do
          begin
            Img:= Tex;
            W:= Tex^.Width;
            H:= Tex^.Height;
            X:= Random( 1000 );
            Y:= Random( 1000 );
            OnClick:= @BoneHud.ButtonClickBoneImg;
          end;
        BoneLen:= 10;
        Pos.x:= X;
        Pos.y:= Y;
      end;
end;

function TBoneInterface.LBGetCount(Sender: TListBox): Integer;
begin
  if ( Sender = LBImg ) then
    Result:= Length( ImgList );
end;

function TBoneInterface.LBGetItem(Sender: TListBox; Index: Integer): String;
begin
  if ( Sender = LBImg ) then
    Result:= ImgList[ Index ].Shortname;
end;

{.$R *.lfm}

procedure TMyProgram.Init;
begin
  {
  // RU: Вертикальная синхронизация поможет избежать загрузки процессора.
  // EN: Vertical synchronization will decrease a CPU loading.
  scr_SetVSync( TRUE );

  // RU: Перед стартом необходимо настроить viewport.
  // EN: Before the start need to configure a viewport.
  wnd_SetPos( MainForm.Panel1.Left, MainForm.Panel1.Top );
  wnd_SetSize( MainForm.Panel1.Width, MainForm.Panel1.ClientHeight );

  MainForm.BringToFront();
  }
  fntMain := font_LoadFromFile( '../' + dirRes + 'Calibri-Regular-36pt.zfi' );
  //background:= tex_LoadFromFile( dirres + 'background.png', TEX_NO_COLORKEY, TEX_FILTER_ANISOTROPY );


  Buttons:= TGraphicList.Create;

{  with ( Buttons[ Buttons.Add( TButton.Create )] as TButton ) do
    begin
      W:= 100;
      H:= 40;
      X:= 100;
      Y:= 100;
      Caption:= 'Btn1';
    end;

  with ( Buttons[ Buttons.Add( TButton.Create )] as TButton ) do
    begin
      W:= 100;
      H:= 40;
      X:= 100;
      Y:= 200;
      Caption:= 'Btn2';
    end;}

  BoneHud:= TBoneHud.Create;

{  with ( BoneHud.Screen.Items[ BoneHud.Screen.Items.Add( TImage.Create )] as TImage ) do
    begin
      W:= 100;
      H:= 40;
      X:= 200;
      Y:= 100;
      if ( FileExists( ExtractFilePath( ParamStr( 0 )) + '../' + dirRes + 'img'  + '/body.png' )) then
        Img:= tex_LoadFromFile( ExtractFilePath( ParamStr( 0 )) + '../' + dirRes + 'img'  + '/body.png' )
      else
        WriteLn( 'File not found: ' + ExtractFilePath( ParamStr( 0 )) + '../' + dirRes + 'img'  + '/body.png' );
    end;}

  Hud:= TGraphicList.Create;

//  BoneHud.Screen.Selection.Obj:= TImage( BoneHud.Screen.Items[ 0 ]);

  BoneInterface:= TBoneInterface.Create;
//  Selection:= TSelection.Create;
{  with ( Hud[ Hud.Add( Selection )] as TSelection ) do
    begin
      Obj:= TImage( Screen.Items[ 0 ]);
    end;}

//  joyCount:= joy_Init();
end;

procedure TMyProgram.Draw;
begin
  // RU: Необходимо обновлять viewport как только изменились размеры контрола, куда был инициализирован ZenGL.
  // EN: Viewport should be updated as soon as size of control was changed.
{  if zglResized Then
    begin
      zglResized := FALSE;
      wnd_SetPos( MainForm.Panel1.Left, MainForm.Panel1.Top );
      wnd_SetSize( MainForm.Panel1.ClientWidth, MainForm.Panel1.ClientHeight );
      XRes:= MainForm.Panel1.ClientWidth;
      YRes:= MainForm.Panel1.ClientHeight;
      HalfX:= XRes div 2;
      HalfY:= YRes div 2;
    end;}

  BoneHud.Draw;
  DrawList( Hud );
  DrawList( Buttons );

  BoneInterface.Draw;

//  text_Draw( fntMain, 0, 0, IntToStr( mouse_X() - mx) + ':' + IntToStr( mouse_Y() - my));
//  text_Draw( fntMain, 0, 0, BoolToStr( mouse_Down( 0 )) + ' ' + IntToStr( mouse_X()));
//  ssprite2d_Draw( background, 0,0, XRes, YRes, 0, 255 );

  // RU: Т.к. ZenGL перехватывает "управление" нужно выполнять обработку интерфейса вручную.
  // EN: Because ZenGL intercepts "control" you need to call process of GUI manually.
//  Application.ProcessMessages();
end;

procedure TMyProgram.Input;
begin
//  MainForm.Caption := '01 - Initialization [ FPS: ' + u_IntToStr( zgl_Get( RENDER_FPS ) ) + ' ]';
  MouseList( Buttons );
  MouseList( Hud );
  BoneHud.Input;
  BoneInterface.Input;
  mx:= mouse_X;
  my:= mouse_Y;
  mouse_ClearState();
end;

procedure TMyProgram.Deinit;
begin
  BoneInterface.Free;
  FreeList( Buttons );
  FreeList( Hud );
  BoneHud.Free;
end;


end.

