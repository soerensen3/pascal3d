// ##################################
// # <----------------------------- #
// #       Revelation Engine        #
// # -----------------------------> #
// ##################################
// #                                #
// # P3D Interface                  #
// ##################################

unit p3dgui;

interface
  uses
    Classes,
    SysUtils,
    Contnrs,
    math,
    clipbrd,
    SDL2,
    p3dgenerics,
    p3dwindow,
    p3dobjects,
    p3dinput,
    p3dtext,
    p3dMath,
    p3dviewport,
    p3dcanvas;

  type
    TP3DGraphicControl = class;
    TP3DControlList = class;

    TP3DGUIDraw       = procedure( Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Single ) of Object;
    TP3DGUIMouseClick = procedure( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer ) of Object;
    TP3DGUIMouseEvent = procedure( Sender: TP3DGraphicControl; X, Y: Integer ) of Object;
    TP3DGUIDragDrop   = procedure( Sender: TP3DGraphicControl; Source: TP3DGraphicControl; X, Y: Integer; var Accept: Boolean ) of Object;


    TP3DControlAlign = ( alNone, alLeft, alRight, alClient, alTop, alBottom );


  {$DEFINE INTERFACE}
  {$INCLUDE p3dgui_manager.inc}
  {$INCLUDE p3dgui_controllist.inc}
  {$INCLUDE p3dgui_graphiccontrol.inc}

  {$UNDEF INTERFACE}

var
  GUIManager: TP3DGUIManager;
  DragDropSrc: TP3DGraphicControl;

implementation

uses
  Types;

var
  LastMouseOverCtrl: TP3DGraphicControl;
  LastMouseDownCtrl: array[ 0..2 ] of TP3DGraphicControl;

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3dgui_manager.inc}
{$INCLUDE p3dgui_controllist.inc}
{$INCLUDE p3dgui_graphiccontrol.inc}


initialization
  GUIManager:= TP3DGUIManager.Create;
  LastMouseOverCtrl:= nil;
  LastMouseDownCtrl[ 0 ]:= nil;
  LastMouseDownCtrl[ 1 ]:= nil;
  LastMouseDownCtrl[ 2 ]:= nil;


finalization
  GUIManager.Free;


end.

