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

  TP3DRenderListFlag = ( rlfMaterials, rlfIndexColors, rlfWireFrame, rlfDepthTest, rlfLighting, rlfRenderEdges, rlfActors, rlfMeshes, rlfScenes, rlfTileGrids );
  TP3DRenderListFlags = set of TP3DRenderListFlag;

 const
   P3DRenderListFlagsDefault = [ rlfMaterials, rlfDepthTest, rlfLighting, rlfMeshes, rlfScenes, rlfTileGrids ];

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
procedure P3DGraphicsFinish;
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

{ TP3DLightRenderList }

function TP3DLightRenderList.PushRenderObject(AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial): Integer;
begin
  if ( AData is TP3DLight ) then
    Result:= inherited PushRenderObject(AData, AMatrix, AMaterial)
  else
    Result:= -1;
end;

procedure TP3DLightRenderList.PassToActiveShader( matView: TMat4; Params: TP3DLightInformationSet );
var
  numLights: Integer;
  i, j: Integer;
  l: String;
  pos: TVec4;
  obj: TP3DDataBlock;

begin
  if ( not Assigned( P3DShaderActive )) then
    exit;

  numLights:= 0;
  for i:= 0 to Count - 1 do
    for j:= 0 to Items[ i ].Positions.Count - 1 do
      with ( Items[ i ].Data as TP3DLight ) do
        begin
          l:= 'LightSource[' + IntToStr( numLights ) + ']';

          if ( liLightParams in Params ) then
            begin
              //Color
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.color' ), Color.R, Color.G, Color.B, Energy ); cle( Self );

              //linear Attenuation
              glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.linearAttenuation' ), LinearAttenuation ); cle( Self );

              //quadratic Attenuation
              glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.quadraticAttenuation' ), .QuadraticAttenuation ); cle( Self );

              //range
              glUniform1f( P3DShaderActive.Uniforms.AddrByName( l + '.range' ), GetLightRange ); cle( Self );

              //type
              glUniform1i( P3DShaderActive.Uniforms.AddrByName( l + '.type' ), Ord( LightType )); cle( Self );
            end;

          if ( liPosition in Params ) then
            begin
              //position in viewspace
              pos:= matView * vec4( Positions[ j ], 1 );
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.position' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
              pos:= vec4( normalize( mat3( matView ) * {mat3( matWorld ) * }Matrix.Row[ 2 ].XYZ ), 0 );
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.direction' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
              pos:= normalize( -pos + vec4( 0, 0, 1, 0 ));
              glUniform4f( P3DShaderActive.Uniforms.AddrByName( l + '.halfvector' ), pos.X, pos.Y, pos.Z, pos.W ); cle( Self );
              //light_vect = light_position - face_center_position
              //cam_vect = cam_position - face_center_position
              //halfangle_vect = (light_vect.normal() + cam_vect.normal()).normal()
            end;
          Inc( numLights );
          //WriteLn( P3DShaderActive.DumpUniforms );
        end;
  glUniform1i( P3DShaderActive.Uniforms.AddrByName( 'numLightSource'), numLights ); cle( Self );
end;


{ TP3DCustomRenderList }

function TP3DCustomRenderList.PushRenderObject(AData: TP3DDataBlock; AMatrix: TMat4; AMaterial: TP3DMaterial): Integer;
var
  Obj: TP3DRenderObject;
  q: TQuaternion;
  i: Integer;
  loc, scale: TVec3;
  rot: TMat3;
begin
  Result:= Find( AData, AMaterial );
  {if ( Result > -1 ) then
    begin
      Obj:= Items[ Result ];
      mat4decompose( AMatrix, loc, rot, scale );
      q:= quat( rot );
      Obj.Rotations.Add( q );
      Obj.Positions.Add( loc );
      Obj.Scalings.Add( scale );
      Obj.Matrices.Add( AMatrix );
      Obj.InstanceCount:= Obj.InstanceCount + 1;
    end
  else}
    begin
      Obj:= TP3DRenderObject.Create;
      Obj.Data:= AData;

      {if ( Assigned( Obj.Data )) then
        WriteLn( 'Creating RenderObject with data ' + Obj.Data.Name )
      else
        WriteLn( 'Creating RenderObject without data.' );}

      mat4decompose( AMatrix, loc, rot, scale );
      q:= quat( rot );
      Obj.Rotations.Add( q );
      Obj.Positions.Add( loc );
      Obj.Scalings.Add( scale );
      Obj.Matrices.Add( AMatrix );
      Obj.Material:= AMaterial;
      Obj.InstanceCount:= 1;
      SetLength( Obj.Modifiers, ModifierStack.Count );
      for i:= 0 to ModifierStack.Count - 1 do
        if ( Assigned( ModifierStack[ i ])) then
          begin
            Obj.Modifiers[ i ]:= ModifierStack[ i ];
            Obj.Modifiers[ i ].AddObject( Obj )
          end;
      Result:= inherited Add( Obj );
    end;
end;

constructor TP3DCustomRenderList.Create;
begin
  inherited Create;
  ModifierStack:= TP3DRenderListModifierList.Create;
end;

destructor TP3DCustomRenderList.Destroy;
begin
  FreeAndNil( FModifierStack );
  inherited Destroy;
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

procedure P3DGraphicsFinish;
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
  P3DGraphicsFinish;

end.

