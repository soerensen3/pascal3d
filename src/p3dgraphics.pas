unit p3dgraphics;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

{.$DEFINE DEBUG_DATABLOCKS}

interface

uses
  Classes,
  SysUtils,
  LazFileUtils,
  strutils,
  Math,

  dglOpenGL,
  p3dMath,
  p3dutils,
  p3devents,


  DOM,
  XMLRead,
  XMLWrite,

  sdl2,
  sdl2_image,
  sdl2_ttf;



//forward
type
  TP3DLibrary = class;
  TP3DScene = class;
  TP3DActor = class;

{$MACRO ON}
{$IfDef CHECKFORERRORS}{$Assertions ON}{$EndIf}

{$DEFINE INTERFACE}
  {$INCLUDE p3dgraphics_lib.inc}
{$UNDEF INTERFACE}

var
  P3DViewports: TP3DViewportStack = nil;
  P3DShaderActive: TP3DShader = nil;
  P3DShaderNodeLib: TP3DShaderNodeLibrary = nil;
  P3DData: TP3DData = nil;
  P3DFontManager: TP3DFontManager = nil;
  P3DFontManagerBmp: TP3DFontManagerBmp = nil;
  P3DCanvasMaterialDefault: TP3DMaterialBase = nil;
  P3DDataBlockCache: TP3DDataBlockCache = nil;
  P3DAttributes: TP3DAttributeList = nil;

procedure P3DGraphicsInit;
procedure P3DGraphicssFinish;
function P3DCheckLastError( Sender: TObject; const AddMsg: String = '' ): Boolean; inline;
procedure P3DDumpGraphicsInfo;

implementation

procedure cle( Sender: TObject; const AddMsg: String = ''  ); inline;
begin
  P3DCheckLastError( Sender, AddMsg );
end;


var
  P3DAssertSender: TObject = nil;

procedure P3DAssert(const M, F: ShortString; L: LongInt; E: Pointer);
begin
  P3DLog.LogException( P3DAssertSender, Format( '%s(%d): %s', [F,L,M]));
end;


function P3DCheckLastError(Sender: TObject; const AddMsg: String): Boolean;
var
  Code: Cardinal;
  S: TAssertErrorProc;
  Msg: String;
begin
  Code:= glGetError();
  Result:= Code = GL_NO_ERROR;
  if ( not Result ) then
    try
      S:= AssertErrorProc;
      AssertErrorProc:= @P3DAssert;
      P3DAssertSender:= Sender;
      Msg:= 'OpenGL Error: ' + gluErrorString( Code );
      if ( AddMsg > '' ) then
        Msg+= ': ' + AddMsg;
      //Assert( False, Msg );
    finally
      AssertErrorProc := S;
    end;
end;



{ TP3DGridSceneList }

function TP3DGridSceneList.FindByScene(AScene: TP3DScene): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count -1 do
    if ( Items[ i ].Scene = AScene ) then
      begin
        Result:= i;
        break;
      end;
end;

function TP3DGridSceneList.FindBySceneName(AName: String): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count -1 do
    if ( Items[ i ].Scene.Name = AName ) then
      begin
        Result:= i;
        break;
      end;
end;


{ TP3DFontLetter }

destructor TP3DFontLetter.Destroy;
begin
  if ( Assigned( Texture ) and P3DData.IsValid( Texture )) then
    FreeAndNil( FTexture );
  inherited Destroy;
end;


{$DEFINE IMPLEMENTATION}
  {$INCLUDE p3dgraphics_lib.inc}
{$UNDEF IMPLEMENTATION}

procedure P3DDumpGraphicsInfo;
var
  debuginfo: String;
begin
  debuginfo:= '<span>GPU vendor</span>' + glGetString( GL_VENDOR );
  debuginfo+= '<br /><span>Renderer</span>' + glGetString( GL_RENDERER );
  debuginfo+= '<br /><span>GL version</span>' + glGetString( GL_VERSION );
  debuginfo+= '<br /><span>GLSL version</span>' + glGetString( GL_SHADING_LANGUAGE_VERSION );
  debuginfo+= '<br /><details><summary>Extensions</summary><div class="whitebox">' + glGetString( GL_EXTENSIONS ) + '</div></details>';
  P3DLog.LogInfoXML( nil, 'Initialized OpenGL <p class="messageheader">' + debuginfo + '</p>');
end;

procedure P3DGraphicsInit;
begin
  InitOpenGL();
  ReadExtensions;

  // Some OpenGL configurations
  glClearColor( 0.0, 0.5, 1.0, 1.0 );
  glClearDepth( 1.0 );
  //glEnable( GL_DEPTH_TEST ); cle( nil );
  glDepthFunc( GL_LESS ); cle( nil );
  glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST ); cle( nil );
  glEnable( GL_BLEND ); cle( nil );
  glBlendFunc( GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA ); cle( nil );

  if ( not Assigned( P3DAttributes )) then
    P3DAttributes:= TP3DAttributeList.Create;
  if ( not Assigned( P3DDataBlockCache )) then
    P3DDataBlockCache:= TP3DDataBlockCache.Create;
  if ( not Assigned( P3DViewports )) then
    P3DViewports:= TP3DViewportStack.Create;
  if ( not Assigned( P3DData )) then
    P3DData:= TP3DData.Create;
  if ( not Assigned( P3DShaderNodeLib )) then
    P3DShaderNodeLib:= TP3DShaderNodeLibrary.Create;

  if ( TTF_Init() <> 0 ) then
    raise Exception.Create( 'Cannot initialize sdl2_text!' );
  if ( not Assigned( P3DFontManager )) then
    P3DFontManager:= TP3DFontManager.Create;
  if ( not Assigned( P3DFontManagerBmp )) then
    P3DFontManagerBmp:= TP3DFontManagerBmp.Create;
  {$DEFINE INITIALIZATION}
    {$INCLUDE p3dgraphics_lib.inc}
  {$UNDEF INITIALIZATION}
end;

procedure P3DGraphicssFinish;
begin
  if ( Assigned( P3DFontManagerBmp )) then
    FreeAndNil( P3DFontManagerBmp );
  if ( Assigned( P3DFontManager )) then
    FreeAndNil( P3DFontManager );
  if ( Assigned( P3DViewports )) then
    FreeAndNil( P3DViewports );
  if ( Assigned( P3DShaderNodeLib )) then
    FreeAndNil( P3DShaderNodeLib );
  if ( Assigned( P3DData )) then
    begin
      P3DData.Free;
      P3DData:= nil;
    end;
  if ( Assigned( P3DDataBlockCache )) then
    FreeAndNil( P3DDataBlockCache );
  if ( Assigned( P3DAttributes )) then
    FreeAndNil( P3DAttributes );
  TTF_Quit();
  {$DEFINE FINALIZATION}
    {$INCLUDE p3dgraphics_lib.inc}
  {$UNDEF FINALIZATION}
end;

finalization
  P3DGraphicssFinish;

end.

