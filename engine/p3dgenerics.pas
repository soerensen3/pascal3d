unit p3dgenerics;

{$mode objfpc}{$H+}

interface
  uses
    Classes, sysutils;

  type
    {$MACRO ON}
    {$DEFINE INTERFACE}

    {$DEFINE OBJECTLIST}
    {$DEFINE TCustomList:= TP3DCustomObjectList}
    {$INCLUDE p3dgenerics_customlist.inc}
    {$UNDEF OBJECTLIST}

    {.$DEFINE TCustomList:= TP3DCustomList}
    {.$INCLUDE p3dgenerics_customlist.inc}

    {$UNDEF INTERFACE}

implementation

{$DEFINE IMPLEMENTATION}

{.$DEFINE TCustomList:= TP3DCustomList}
{.$INCLUDE p3dgenerics_customlist.inc}

{$DEFINE OBJECTLIST}
{$DEFINE TCustomList:= TP3DCustomObjectList}
{$INCLUDE p3dgenerics_customlist.inc}
{$UNDEF OBJECTLIST}

{$UNDEF IMPLEMENTATION}

end.

