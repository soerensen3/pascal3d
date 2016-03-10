unit __MAINUNIT__;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,

  SDL2, //EXTERNAL LIBS SDL2 AND OPENGL
  dglOpenGL,

  p3dwindow,
  p3dSDLApplication,
  //USE GUI. YOU NEED TO ADD PACKAGE p3d_gui TO THE DEPENDENCIES
  //p3dgui,
  Math,
  p3dMath,

  p3dmodel,
  p3dscene,
  p3dinput,
  p3dviewport,
  p3dlogging;

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

