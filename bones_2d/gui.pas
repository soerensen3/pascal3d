unit gui;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  zgl_mouse,
  zgl_primitives_2d,
  zgl_collision_2d,
  zgl_font,
  zgl_text;

type

  { TGraphicControl }

  TGraphicControl = class( TPersistent )
    private
      FClicked: Boolean;
      Fmx, Fmy: Integer;
      FDown: Boolean;
      FHeight: Integer;
      FLeft: Integer;
      FTop: Integer;
      FWidth: Integer;

    public
//      constructor Create;
//      destructor Destroy;

      procedure Paint; virtual;
      procedure Input; virtual;

    published
      property Left: Integer read FLeft write FLeft;
      property Top: Integer read FTop write FTop;
      property Width: Integer read FWidth write FWidth;
      property Height: Integer read FHeight write FHeight;

      property Down: Boolean read FDown write FDown;
      property Clicked: Boolean read FClicked write FClicked;
  end;

  { TButton }

  TButton = class( TGraphicControl )
    private
      FCaption: String;
      FSelected: Boolean;

    public
      procedure Paint; override;

    published
      property Caption: String read FCaption write FCaption;
      property Selected: Boolean read FSelected write FSelected;
  end;

  { TListBox }

  TListBox = class( TGraphicControl )
    private
      FCaption: String;
      FSelection: Integer;

    public
      procedure Paint; override;
      procedure Input; override;

    published
      property Caption: String read FCaption write FCaption;
      property Selection: Integer read FSelection write FSelection;
  end;

var
  CtrlOnClickHandler: procedure( Sender: TGraphicControl; x,y: Integer );
  ListBoxGetCount: function( LB: TListBox ): Integer;
  ListBoxGetItem: function( LB: TListBox; Index: Integer ): String;

implementation

uses vars;

{ TListBox }

procedure TListBox.Paint;
var
  n, i: Integer;
begin
  pr2d_Rect( Transform( Left, True ), Transform( Top + 10, False ), Transform( Width, True, True ), Transform( Height, False, True ), $336699, 255, PR2D_FILL );
  pr2d_Rect( Transform( Left + 5, True ), Transform( Top, False ), Transform( Width - 10, True, True ), Transform( 30, False, True ), $447AB0, 255, PR2D_FILL );
  text_DrawEx( fntMain, Transform( Left + 8, True ), Transform( Top + 6, False ), 0.4 * zoom, 0, Caption, 255, $FFFFFF );

  if ( Assigned( ListBoxGetCount ) AND Assigned( ListBoxGetItem )) then
    begin
      n:= ListBoxGetCount( Self );
      for i:= 0 to n - 1 do
        begin
          if ( Selection = i ) then
            pr2d_Rect( Transform( Left + 5, True ), Transform( Top + ( i + 1 ) * 30, False ), Transform( Width - 10, True, True ), Transform( 30, False, True ), $A1B9D3, 255, PR2D_FILL );
          text_DrawEx( fntMain, Transform( Left + 8, True ), Transform( Top + 36 + i * 30, False ), 0.4 * zoom, 0, ListBoxGetItem( Self, i ), 255, $FFFFFF );
        end;
    end;
end;

procedure TListBox.Input;
begin
  inherited Input;
  if ( Clicked ) then
    begin
      Selection:= Fmy div 30 - 1;
    end;
end;

{ TButton }

procedure TButton.Paint;
begin
  inherited Paint;
  if ( Down ) then
    pr2d_Rect( Transform( Left, True ), Transform( Top, False ), Transform( Width, True, True ), Transform( Height, False, True ), $395B7E, 255, PR2D_FILL )
  else
    pr2d_Rect( Transform( Left, True ), Transform( Top, False ), Transform( Width, True, True ), Transform( Height, False, True ), $336699, 255, PR2D_FILL );

  if ( Selected ) then
    pr2d_Rect( Transform( Left, True ), Transform( Top, False ), Transform( Width, True, True ), Transform( Height, False, True ), $FFFFFF, 255 )
  else
    pr2d_Rect( Transform( Left, True ), Transform( Top, False ), Transform( Width, True, True ), Transform( Height, False, True ), $68A1DB, 255 );
  text_DrawEx( fntMain, Transform( Left + 3, True ), Transform( Top + 6, False ), 0.4 * zoom, 0, Caption, 255, $FFFFFF );
end;

{ TGraphicControl }

procedure TGraphicControl.Paint;
begin

end;

procedure TGraphicControl.Input;
begin
  if ( mouseClick[ 0 ]) then
    if ( col2d_PointInRect( mouseX, mouseY, RectTransform( Rect( Left, Top, Width, Height )))) then
      if ( Assigned( CtrlOnClickHandler )) then
        begin
          Fmx:= Round(( mouseX - offx ) / Zoom - Left );
          Fmy:= Round(( mouseY - offy ) / Zoom - Top );
          CtrlOnClickHandler( Self, Fmx, Fmy );
          FClicked:= True;
        end;
  if (( mouseDown[ 0 ]) AND
    ( col2d_PointInRect( mouseX, mouseY, RectTransform( Rect( Left, Top, Width, Height ))))) then
      FDown:= True
    else
      FDown:= False;
end;

end.
