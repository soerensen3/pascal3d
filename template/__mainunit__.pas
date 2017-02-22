unit __mainunit__;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  SDL2, //EXTERNAL LIBS SDL2 AND OPENGL - These are only needed if we access OpenGL or SDL directly
  dglOpenGL,

  p3devents,
  p3dgraphics,
  {$IFDEF GUI}
  p3dgui,  //USE GUI. YOU NEED TO ADD PACKAGE p3d_gui TO THE DEPENDENCIES
  {$ENDIF}
  p3dMath, //P3DMath unit that provides vector and matrix types and functions
  Math;

type

  { TP3D__PROJNAME__Application }

  __appclassname__ = class ( TP3DApplication )
    private
      FArrowRenderList: TP3DRenderList;
      FArrowScene: TP3DScene;
      FCatchMouse: Boolean;
      FCamObj: TP3DActor;
      FClearColor: TVec4;
      FMainScene: TP3DScene;
      FRenderList: TP3DRenderList;
      FSkyCamObj: TP3DActor;
      FSkyRenderList: TP3DRenderList;
      FSkyScene: TP3DScene;
      procedure SetCatchMouse(AValue: Boolean);
      procedure SetClearColor(AValue: TVec4);

    protected
      procedure Events; override;
      procedure Render; override;
      procedure InitGL;
      procedure MouseMotion(Event: TSDL_MouseMotionEvent); override;
      procedure Keyboard(Event: TSDL_KeyboardEvent); override;

    public
      procedure Initialize; override;
      procedure Finalize; override;

      property MainScene: TP3DScene read FMainScene write FMainScene;
      property SkyScene: TP3DScene read FSkyScene write FSkyScene;
      property ArrowScene: TP3DScene read FArrowScene write FArrowScene;
      property RenderList: TP3DRenderList read FRenderList write FRenderList;
      property SkyRenderList: TP3DRenderList read FSkyRenderList write FSkyRenderList;
      property ArrowRenderList: TP3DRenderList read FArrowRenderList write FArrowRenderList;

      property CamObj: TP3DActor read FCamObj write FCamObj;
      property SkyCamObj: TP3DActor read FSkyCamObj write FSkyCamObj;
      property CatchMouse: Boolean read FCatchMouse write SetCatchMouse;
      property ClearColor: TVec4 read FClearColor write SetClearColor;
    {$IFDEF GUI}
    private
      procedure TreeViewSelectionChange( var NewSelection: TP3DTreeNode );
      FOI: TP3DObjectInspector;

    public
      property OI: TP3DObjectInspector read FOI write FOI;
    {$ENDIF}
  end;

{$DEFINE INTERFACE}
{$INCLUDE init.inc}
{$INCLUDE events.inc}
{$INCLUDE render.inc}
{$UNDEF INTERFACE}


implementation

{$DEFINE IMPLEMENTATION}
{$INCLUDE init.inc}
{$INCLUDE events.inc}
{$INCLUDE render.inc}
{$UNDEF IMPLEMENTATION}



end.

