// ##################################
// # <----------------------------- #
// #       PASCAL 3D ENGINE         #
// # -----------------------------> #
// ##################################
// #                                #
// # P3D Interface                  #
// ##################################
{$mode objfpc}

{.$DEFINE DEBUG_GUI}

unit p3dgui;

interface
  uses
    Classes,
    SysUtils,
    Contnrs, DOM,
    math,
    clipbrd,
    strutils,
    SDL2,
    LazFileUtils,
    dglOpenGL,
    p3dMath,
    pascal3d.utils,
    pascal3d.events,
    pascal3d.core,
    typinfo;

  type
    TP3DGraphicControl = class;
    TP3DControlList = class;

    TP3DGUIDraw       = procedure( Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Single ) of Object;
    TP3DGUIMouseClick = procedure( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer ) of Object;
    TP3DGUIMouseEvent = procedure( Sender: TP3DGraphicControl; X, Y: Integer ) of Object;
    TP3DGUIDragDrop   = procedure( Sender: TP3DGraphicControl; Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean ) of Object;


    TP3DControlAlign = ( alNone, alLeft, alRight, alClient, alTop, alBottom );
    TP3DPageMode = ( pmScroll, pmStretch, pmExpand );


  {$DEFINE INTERFACE}
  {$INCLUDE p3dgui_graphiccontrol.inc}
  {$INCLUDE p3dgui_controllist.inc}
  {$INCLUDE p3dgui_manager.inc}

  {$INCLUDE p3dgui_colors.inc}

  {$INCLUDE p3dgui_buttons.inc}
  {$INCLUDE p3dgui_menus.inc}
  {$INCLUDE p3dgui_stdctrls.inc}
  {$INCLUDE p3dgui_commonctrls.inc}
  {$INCLUDE p3dgui_sceneviewer.inc}
  {$INCLUDE p3dgui_advanced.inc}
  {$INCLUDE p3dgui_forms.inc}
  {$INCLUDE p3dgui_objectinspector.inc}

  {$UNDEF INTERFACE}


var
  P3DGUIManager: TP3DGUIManager;
  DragDropSrc: TP3DGraphicControl;

procedure P3DGUIInit;
procedure P3DGUIFinish;

implementation

uses
  Types, XMLRead, jsonparser, fpjson;

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgui_colors.inc}
{$INCLUDE p3dgui_manager.inc}
{$INCLUDE p3dgui_controllist.inc}
{$INCLUDE p3dgui_graphiccontrol.inc}

{$INCLUDE p3dgui_buttons.inc}
{$INCLUDE p3dgui_stdctrls.inc}
{$INCLUDE p3dgui_commonctrls.inc}
{$INCLUDE p3dgui_sceneviewer.inc}
{$INCLUDE p3dgui_menus.inc}
{$INCLUDE p3dgui_advanced.inc}
{$INCLUDE p3dgui_forms.inc}
{$INCLUDE p3dgui_objectinspector.inc}


procedure P3DGUIInit;
begin
  P3DGUIInitColors;
  if ( not Assigned( P3DGUIManager )) then
    P3DGUIManager:= TP3DGUIManager.Create;
end;

procedure P3DGUIFinish;
begin
  if ( Assigned( P3DGUIManager )) then
    begin
      P3DGUIManager.Free;
      P3DGUIManager:= nil;
    end;
end;

finalization
  P3DUtilsFinish;



end.

