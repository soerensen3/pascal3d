unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  SDL2, //EXTERNAL LIBS SDL2 AND OPENGL - These are only needed if we access OpenGL or SDL directly
  dglOpenGL,

  pascal3d.events,
  pascal3d.core,
  pascal3d.utils,
  p3dMath, //P3DMath unit that provides vector and matrix types and functions
  Math;

type

  { TP3Dp3dfontbmpApplication }

  { TP3DFontBitmap }

  TP3DFontBitmap = class ( TP3DApplication )
    private
      FActiveFont: TP3DFontBmp;
      FCanvas: TP3DCanvas2D;

    protected
      procedure Events; override;
      procedure InitGL;
      procedure Keyboard(Event: TSDL_KeyboardEvent); override;
      procedure Render; override;

    public
      procedure Initialize; override;
      procedure Finalize; override;

      property ActiveFont: TP3DFontBmp read FActiveFont write FActiveFont;
      property Canvas: TP3DCanvas2D read FCanvas write FCanvas;
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

