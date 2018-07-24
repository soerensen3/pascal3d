// ##################################
// # <----------------------------- #
// #       PASCAL 3D ENGINE         #
// # -----------------------------> #
// ##################################
// #                                #
// # P3D Interface                  #
// ##################################
{$mode objfpc}{$H+}

{.$DEFINE DEBUG_GUI}

unit p3d.ui;

interface
  uses
    Classes,
    SysUtils,
    Contnrs,
    math,
    clipbrd,
    strutils,
    typinfo,
    jsonparser,
    fpjson,
    SDL2,
    LazFileUtils,
    dglOpenGL,
    p3d.math,
    p3d.utils,
    p3d.events,
    p3d.core;

  {$DEFINE INTERFACE}
  {$INCLUDE p3d.ui_colors.inc}

type
  TP3DGraphicControl = class;
  TP3DGraphicControlContainerList = class;

  TP3DGUIDraw       = procedure( Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Float ) of Object;
  TP3DGUIMouseClick = procedure( Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Float ) of Object;
  TP3DGUIMouseEvent = procedure( Sender: TP3DGraphicControl; X, Y: Float ) of Object;
  TP3DGUIDragDrop   = procedure( Sender: TP3DGraphicControl; Source: TP3DGraphicControl; X, Y: Float; var Accept: Boolean ) of Object;


  TP3DControlAlign = ( alNone, alLeft, alRight, alClient, alTop, alBottom );
  TP3DPageMode = ( pmScroll, pmStretch, pmExpand );

  {$INCLUDE p3d.ui_lib.inc}
  {$UNDEF INTERFACE}


  procedure RenderButton(Canvas: TP3DCanvas2D; Rect: TP3DRect; Focused: Boolean; C1, C2, C3, C4, CE1, CE2, CE3, CE4: TP3DColorRGBA);
  function RenderButtonText( Canvas: TP3DCanvas2D; Caption: String; var CacheObject: TP3DTextBmp;  R: TP3DRect; HAlignment: TP3DHorizontalAlignment; VAlignment: TP3DVerticalAlignment; C: TP3DColorRGBA ): TVec2;
  function P3DAlignInRect( R:TP3DRect; AlignWidthHeight: TVec2; HAlignment: TP3DHorizontalAlignment; VAlignment: TP3DVerticalAlignment ): TP3DRect;

  //IMPLEMENT UI-Master
  {
    UIMaster for each Viewer
      Handlers
        SetPosition
        SetWidthHeight
        SetAlign
        SetPageMode
        AddChildren
        RemoveChildren
      AppliedTransformation

    Each Control has a pointer to a UIMaster
    Can be composed to render list
  }


var
  P3DUI: TP3DUI;
  P3DUIViewerActive: TP3DUIViewerBase;

procedure P3DUIInit;
procedure P3DUIFinish;

implementation

{uses
  Types, ;}

procedure cle( Sender: TObject; const AddMsg: String = ''  ); inline;
begin
  P3DCheckLastError( Sender, AddMsg );
end;

{$DEFINE IMPLEMENTATION}
{$INCLUDE p3d.ui_colors.inc}
{$INCLUDE p3d.ui_lib.inc}


procedure P3DUIInit;
begin
  P3DGUIInitColors;
  if ( not Assigned( P3DUI )) then
    P3DUI:= TP3DUI.Create;
  P3DUIViewerActive:= nil;
  P3DClassFactory.AddArray([ TP3DGraphicControl, TP3DButton, TP3DCheckBox, TP3DColorRGBAComboBox, TP3DColorRGBAPicker,
                             TP3DColorRGBAValue, TP3DColorRGBAWheel, TP3DComboBox, TP3DComboBoxDataBlock, TP3DDialogFile,
                             TP3DDialogStreamableClass, TP3DEdit, TP3DEventValueEdit, TP3DForm, TP3DGroupBox, TP3DGroupButton,
                             TP3DImage, TP3DLabel, TP3DListView, TP3DListViewFile, TP3DModalWindow, TP3DScrollBar, TP3DSplitter,
                             TP3DTreeView, TP3DTreeViewClassFactory, TP3DTreeViewDirectory, TP3DTreeViewObjects,
                             TP3DTreeViewProperties, TP3DValueEdit, TP3DVectorEdit ]);
end;

procedure P3DUIFinish;
begin
  if ( Assigned( P3DUI )) then
    begin
      P3DUI.Free;
      P3DUI:= nil;
    end;
end;

procedure RenderButton(Canvas: TP3DCanvas2D; Rect: TP3DRect; Focused: Boolean; C1, C2, C3, C4, CE1, CE2, CE3, CE4: TP3DColorRGBA);
var
  clf: TVec4;
begin
  Canvas.RenderRect( Rect.TopLeft, Rect.BottomRight +  - 1, C1, C2, C3, C4 );
  Canvas.RenderLineRect( Rect.TopLeft, Rect.BottomRight - 1, CE1, CE2, CE3, CE4 );

  if ( Focused ) then
    begin
      clf:= vec4( 0.5, 0.5, 0.5, 0.5 );
      Canvas.RenderLineRect( Rect.TopLeft + 4, Rect.BottomRight - 4, clf, clf, clf, clf );
    end;
end;

function RenderButtonText( Canvas: TP3DCanvas2D; Caption: String; var CacheObject: TP3DTextBmp;  R: TP3DRect; HAlignment: TP3DHorizontalAlignment; VAlignment: TP3DVerticalAlignment; C: TP3DColorRGBA ): TVec2;
var
  P: TVec2;
begin
  if ( Caption > '' ) then
    begin
      if ( not Assigned( CacheObject )) then
        CacheObject:= TP3DTextBmp.CreateSimple( Caption, P3DFontManagerBmp.Fonts[ Canvas.Font.Name ], Canvas.Font.Size );

      P:= Canvas.CalcAlignedTextRect( CacheObject, R, HAlignment, VAlignment );

      Canvas.Font.Color:= C;
      Canvas.RenderText( Caption, P, CacheObject );
      Result:= P;
    end
  else
    Result:= vec2( 0 );
end;

function P3DAlignInRect(R: TP3DRect; AlignWidthHeight: TVec2; HAlignment: TP3DHorizontalAlignment;
  VAlignment: TP3DVerticalAlignment): TP3DRect;
begin
  case HAlignment of
    haLeft: Result.Left:= R.Left;
    haCenter: Result.Left:= R.Left + ( R.Width - AlignWidthHeight.X ) / 2;
    haRight: Result.Left:= R.Left + R.Width - AlignWidthHeight.X;
  end;
  case VAlignment of
    vaTop: Result.Top:= R.Top;
    vaCenter: Result.Top:= R.Top + ( R.Height - AlignWidthHeight.Y ) / 2;
    vaBottom: Result.Top:= R.Top + R.Height - AlignWidthHeight.Y;
  end;
  Result.WidthHeight:= AlignWidthHeight;
end;


finalization
  P3DUtilsFinish;

end.

