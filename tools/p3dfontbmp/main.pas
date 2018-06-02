unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  SDL2, //EXTERNAL LIBS SDL2 AND OPENGL - These are only needed if we access OpenGL or SDL directly
  dglOpenGL,

  p3d.events,
  p3d.core,
  p3d.utils,
  p3d.ui,
  p3d.Math, //P3DMath unit that provides vector and matrix types and functions
  Math;

type

  { TP3DFontBitmap }

  TP3DFontBitmap = class ( TP3DApplication )
    private
      FActiveFont: TP3DFontBmp;
      FCanvas: TP3DCanvas2D;
      FOffSet: TVec2;
      FZoom: Single;
      procedure SetOffSet(AValue: TVec2);
      procedure SetZoom(AValue: Single);

    protected
      procedure Events; override;
      procedure InitGL;
      procedure Keyboard(Event: TSDL_KeyboardEvent); override;
      procedure MouseWheel(Event: TSDL_MouseWheelEvent); override;
      procedure MouseMotion(Event: TSDL_MouseMotionEvent); override;
      procedure Render; override;

    public
      procedure Initialize; override;
      procedure Finalize; override;

      property ActiveFont: TP3DFontBmp read FActiveFont write FActiveFont;
      property Canvas: TP3DCanvas2D read FCanvas write FCanvas;
      property Zoom: Single read FZoom write SetZoom;
      property OffSet: TVec2 read FOffSet write SetOffSet;
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

