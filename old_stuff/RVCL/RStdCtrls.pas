unit RStdCtrls;

interface
  uses
    Core,
    Classes,
    Timers,
    RScrollbars,
    RVCL,
    Types,
    RUtils,
    Input,
    ClipBrd,
    Math,
    Windows,
    RTypes,
//    GlobalVars,
    RMenus;

  type
    TRevEdit = class( TRevFocusControl )
      private
        FText: String;
        FColor: Cardinal;
        FSel1: Integer;
        FSel2: Integer;
        FTextColor: Cardinal;
        FSelColor: Cardinal;
        FCVis: Boolean;
        Timer: TTimer;
        LastKey: Integer;
        _start: Integer;
        FNoShift: Boolean;
        FOnSetText: TNotifyEvent;
        FOnAutoComplete: TNotifyEvent;
        FPopupMenu: TRevPopupMenu;

        function CoordToCursorPos( Coord: Integer ): Integer;
        procedure Blink( Sender: TObject );
        procedure SetSel1( const Value: Integer );
        procedure SetSel2( const Value: Integer );
        procedure SetText( const Value: String );
        procedure ReplaceSel( S: String );

        procedure MCut( Sender: TObject );
        procedure MCopy( Sender: TObject );
        procedure MPaste( Sender: TObject );
        procedure MDelete( Sender: TObject );
        procedure MSelectAll( Sender: TObject );

      public
        procedure Draw; override;
        procedure KeyDown( Loop: Boolean ); override;
        procedure MouseMove( IsOver: Boolean; X, Y: Integer ); override;
        procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
        procedure MouseUp( mb1, mb2, mb3: Boolean; X, Y: Integer); override;
        procedure MouseDblClick( mb1, mb2, mb3: Boolean; X, Y: Integer ); override;
        procedure Autocomplete( S: TStringList );
        function GetSelText: String;
        procedure SelectAll;
        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
        destructor Destroy; override;

      published
        property Text: String read FText write SetText;
        property Color: Cardinal read FColor write FColor;
        property TextColor: Cardinal read FTextColor write FTextColor;
        property SelColor: Cardinal read FSelColor write FSelColor;
        property Sel1: Integer read FSel1 write SetSel1;
        property Sel2: Integer read FSel2 write SetSel2;
        property OnSetText: TNotifyEvent read FOnSetText write FOnSetText;
        property OnAutoComplete: TNotifyEvent read FOnAutoComplete write FOnAutoComplete;
    end;

    TRevListBox = class( TRevFocusControl )
      private
        FItems: TStringList;
        FColorBound: Cardinal;
        FColor: Cardinal;
        FColorText: Cardinal;
        FColorSel: Cardinal;
        FScrollBarH: TRevScrollBar;
        FScrollBarV: TRevScrollBar;
        FScrollBlock1: TRevGraphicControl;
        FScrollBlock2: TRevGraphicControl;
        FScroll: TPoint;
        FCanSelect: Boolean;
        FItemHeight: Integer;
        FSelection: Integer;
        FColorSelText: Cardinal;
        FListCtrl: TRevGraphicControl;

        function GetTextWidth: Integer;
        procedure Scroll( Sender: TObject );
        function GetSelection: Integer;
        procedure SetSelection( const Value: Integer );

      public
        constructor Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
        destructor Destroy; override;

        procedure Draw; override;
        procedure MouseDown( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
        procedure DrawList( Sender: TRevGraphicControl; OffSetX, OffSetY, _Width, _Height: Single );
        procedure KeyDown(Loop: Boolean); override;
        procedure FocusSelection;

        property Items: TStringList read FItems write FItems;
        property Color: Cardinal read FColor write FColor;
        property ColorBound: Cardinal read FColorBound write FColorBound;
        property ColorText: Cardinal read FColorText write FColorText;
        property ColorSel: Cardinal read FColorSel write FColorSel;
        property ColorSelText: Cardinal read FColorSelText write FColorSelText;
        property ScrollBarH: TRevScrollBar read FScrollBarH;
        property ScrollBarV: TRevScrollBar read FScrollBarV;
        property ItemHeight: Integer read FItemHeight write FItemHeight;
        property CanSelect: Boolean read FCanSelect write FCanSelect;
        property Selection: Integer read GetSelection write SetSelection;
    end;

implementation

{ TEdit }

procedure TRevEdit.Blink( Sender: TObject );
begin
  FCVis:= not FCVis;
end;

constructor TRevEdit.Create( AOwner: TBaseObject; AEngine: TEngine; _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
begin
  inherited;
  Color:= $7F00003F;
  TextColor:= $AFFFFFFF;
  SelColor:= $3FFF0000;
  Text:= '';
  Timer:= TTimer.Create( Self, AEngine );
  Timer.Interval:= 500;
  Timer.OnTimer:= Blink;
  FPopupMenu:= TRevPopupMenu.Create( Self, AEngine, Manager );
  FPopupMenu.Menus[ FPopupMenu.Menus.Add( 'Cut' )].OnClick:= MCut;
  FPopupMenu.Menus[ FPopupMenu.Menus.Add( 'Copy' )].OnClick:= MCopy;
  FPopupMenu.Menus[ FPopupMenu.Menus.Add( 'Paste' )].OnClick:= MPaste;
  FPopupMenu.Menus[ FPopupMenu.Menus.Add( 'Delete' )].OnClick:= MDelete;
  FPopupMenu.Menus[ FPopupMenu.Menus.Add( 'Select All' )].OnClick:= MSelectAll;  
end;

destructor TRevEdit.Destroy;
begin
//  Timer.Free;
  inherited;
end;

procedure TRevEdit.Draw;
var
  cl: Cardinal;
  r: TRect;
begin
  inherited;
//  Canvas.RenderRect( 0, 0, Width - 1, Height - 1, Color, Color, Brightness( Color, 64 ), Brightness( Color, 64 ) );
  Canvas.RenderRect( 0, 0, Width, Height div 2,
    Brightness( Color, -32 ), Brightness( Color, -32 ), Brightness( Color, -64 ), Brightness( Color, -64 ));
  Canvas.RenderRect( 0, Height div 2, Width, Height,
    Brightness( Color, 0 ), Brightness( Color, 0 ), Brightness( Color, 64 ), Brightness( Color, 64 ));

  cl:= $FFFFFFFF * Ord( HasFocus );
  Canvas.RenderLineRect( 0, 0, Width - 1, Height - 1, cl, cl, cl, cl );
  Canvas.Font.Size:= Round( Height - 2 );
  Canvas.Font.Color:= TextColor;
  Canvas.RenderText( Text, Point( 5 - FScrollX, 0 ));
  if ( HasFocus and FCVis ) then
    Canvas.RenderText( '|', Point( 5 + Manager.DefaultFont.TextWidth( Copy( Text, 0, Sel1 ),
    Round( Height - 2 )), 0 ));

  if ( Sel1 > Sel2 ) then
    begin
      r.Left:= - FScrollX + Manager.DefaultFont.TextWidth( Copy( Text, 1, Sel2 ), Round( Height ));
      r.Top:= 1;
      r.Right:= r.Left + Manager.DefaultFont.TextWidth( GetSelText, Round( Height - 2 ));
      r.Bottom:= Round( Height ) - 1;
    end
  else if ( Sel1 < Sel2 ) then
    begin
      r.Left:= - FScrollX + Manager.DefaultFont.TextWidth( Copy( Text, 1, Sel1 ), Round( Height ));
      r.Top:= 1;
      r.Right:= r.Left + Manager.DefaultFont.TextWidth( GetSelText, Round( Height - 2 ));
      r.Bottom:= Round( Height ) - 1;
    end;

  if ( Sel1 <> Sel2 ) then
    if ( HasFocus ) then
      Canvas.RenderRect( 5 + r.Left, r.Top, 5 + r.Right, r.Bottom,
        SelColor, SelColor, Brightness( SelColor, 64 ), Brightness( SelColor, 64 ))
    else
      Canvas.RenderRect( 5 + r.Left, r.Top, 5 + r.Right, r.Bottom,
        ColorMultiplyW( $80FFFFFF, SelColor ), ColorMultiplyW( $80FFFFFF, SelColor ),
        Brightness( ColorMultiplyW( $80FFFFFF, SelColor ), 64 ),
        Brightness( ColorMultiplyW( $80FFFFFF, SelColor ), 64 ))
end;

procedure TRevEdit.KeyDown( Loop: Boolean );
var
  I: Integer;
  S: String;
  ClpBrdStr: PChar;
  ClpBrdString: String;
  TestKey: array [ 0..255 ] of Boolean;
begin
  inherited;
  if ( WasDown ) then
    exit;
  For I:= 0 to 255 do
    if ( Manager.GameInput.Keys[ I ]) then
      begin
        if ( not Loop ) then
          if ( not Manager.GameInput.KeysChanged[ I ]) then
            continue;

        S:= IKtoASC( I, Manager.GameInput.Keys[ IK_LSHIFT ] or Manager.GameInput.Keys[ IK_RSHIFT ]);
        if ( Manager.GameInput.Keys[ IK_LCONTROL ] or Manager.GameInput.Keys[ IK_RCONTROL ]) then
          begin
            if ( Clipboard.HasFormat( CF_TEXT )) then
              if ( S = 'v' ) then
                begin
                  ReplaceSel( Clipboard.AsText );
                  continue;
                end;
            if ( S = 'c' ) then
              begin
                Clipboard.AsText:= GetSelText;
                continue;
              end
            else
              if ( S = 'x' ) then
                begin
                  Clipboard.AsText:= GetSelText;
                  ReplaceSel( '' );
                  continue;
                end;
          end;


        if ( Length( S ) > 0 ) then
          begin
            FNoShift:= True;
            ReplaceSel( S );
            if ( Assigned( FOnAutoComplete )) then
              FOnAutoComplete( Self );
            continue;
          end;

        if ( I = IK_LEFT ) then
          if ( Manager.GameInput.Keys[ IK_LCONTROL ] or Manager.GameInput.Keys[ IK_RCONTROL ]) then
            begin
              Sel1:= GetWordAtCursor( FText, Sel1 - 1, True ).x;
              Sel2:= Sel1;
            end
          else
            begin
              Sel1:= Sel1 - 1;
              Sel2:= Sel1;
            end;

        if ( I = IK_RIGHT ) then
          if ( Manager.GameInput.Keys[ IK_LCONTROL ] or Manager.GameInput.Keys[ IK_RCONTROL ]) then
            begin
              Sel1:= GetWordAtCursor( FText, Sel1 + 1, True ).y;
              Sel2:= Sel1;
            end
          else
            begin
              Sel1:= Sel1 + 1;
              Sel2:= Sel1;
            end;


        if ( I = IK_BACK ) then
          begin
            FNoShift:= True;
            if ( Sel1 - Sel2 <> 0 ) then
              ReplaceSel( '' )
            else
              begin
                Delete( FText, Sel1, 1 );
                Sel1:= Sel1 - 1;
                Sel2:= Sel1;
                Text:= FText;
                FNoShift:= False;
              end;
          end;
        if ( I = IK_DELETE ) then
          begin
            if ( Sel2 <> Sel1 ) then
              begin
                if ( Sel1 < Sel2 ) then
                  Delete( FText, Sel1 + 1, Sel2 - Sel1 )
                else
                  Delete( FText, Sel2 + 1, Sel1 - Sel2 );
                Sel2:= Sel1;
                Text:= FText;
              end
            else
              begin
                Delete( FText, Sel1 + 1, 1 );
                Text:= FText;
              end;
          end;
        if ( I = IK_HOME ) then
          begin
            Sel1:= 0;
            Sel2:= 0;
          end;
        if ( I = IK_END ) then
          begin
            Sel1:= Length( Text );
            Sel2:= Length( Text );
          end;

        break;
      end;
end;

procedure TRevEdit.MouseMove( IsOver: Boolean; X, Y: Integer );
begin
  inherited;
  if ( WasDown ) then
    Sel1:= CoordToCursorPos( X );
end;

procedure TRevEdit.MouseDown( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  inherited;
  if ( mb1 ) then
    begin
      Sel1:= CoordToCursorPos( X );
      Sel2:= Sel1;
    end;
end;

procedure TRevEdit.SetSel2( const Value: Integer );
var
  Val: Integer;
begin
  if ( not FNoShift ) then
    if (( Manager.GameInput.Keys[ IK_LSHIFT ]) or ( Manager.GameInput.Keys[ IK_RSHIFT ])) then
      exit;
  FSel2:= Max( 0, Min( Length( Text ), Value ));
  FCVis:= True;
//  Timer.Reset;
end;

procedure TRevEdit.SetSel1( const Value: Integer );
begin
  FSel1:= Max( 0, Min( Length( Text ), Value ));
  FCVis:= True;
//  Timer.Reset;
end;

procedure TRevEdit.MouseDblClick( mb1, mb2, mb3: Boolean; X, Y: Integer );
var
  p: TPoint;
begin
  inherited;
  if ( not mb1 ) then
    exit;
  p:= GetWordAtCursor( FText, CoordToCursorPos( X ), False );
  Sel1:= p.y;
  Sel2:= p.x;
end;

procedure TRevEdit.SetText( const Value: String );
begin
  FText:= Value;
  if ( Sel1 > Length( FText )) then
    Sel1:= Length( FText );
  Sel2:= Sel1;
  if ( Assigned( FOnSetText )) then
    FOnSetText( Self );
end;

procedure TRevEdit.ReplaceSel( S: String );
begin
  if ( Sel1 <> Sel2 ) then
    begin
      if ( Sel1 > Sel2 ) then
        begin
          Delete( FText, Sel2 + 1, Sel1 - Sel2 );
          Sel1:= Sel2;
        end
      else
        begin
          Delete( FText, Sel1 + 1, Sel2 - Sel1 );
          Sel2:= Sel1;
        end;
    end;
  if ( Length( S ) > 0 ) then
    begin
      Insert( S, FText, Sel1 + 1 );
      Sel1:= Sel1 + Length( S );
      Sel2:= Sel1;
    end;
  FNoShift:= False;
end;

function TRevEdit.GetSelText: String;
begin
  Result:= '';
  if ( Sel1 <> Sel2 ) then
    if ( Sel1 > Sel2 ) then
      Result:= Copy( FText, Sel2 + 1, Sel1 - Sel2 )
    else
      Result:= Copy( FText, Sel1 + 1, Sel2 - Sel1 );
end;

procedure TRevEdit.Autocomplete( S: TStringList );
var
  I, n: Integer;
begin
  for I:= 0 to S.Count - 1 do
    if (( Length( Text ) < Length( S[ I ])) and ( Pos( Text, S[ I ]) = 1 )) then
      begin
        n:= Length( Text );
        Text:= S[ I ];
        Sel1:= Length( Text );
        Sel2:= n;
        break;
      end;
end;

function TRevEdit.CoordToCursorPos( Coord: Integer ): Integer;
var
  X: Integer;
begin
  X:= 0;
  Result:= -1;
  while ( X < Coord ) do
    begin
      if ( Result + 2 > Length( Text )) then
        begin
          Result:= Length( Text );
          break;
        end;
      Inc( X, Manager.DefaultFont.TextWidth( Text[ Result + 2 ], Round( Height )));
      Inc( Result );
    end;
end;

procedure TRevEdit.MouseUp( mb1, mb2, mb3: Boolean; X, Y: Integer );
begin
  inherited;
  if ( mb2 ) then
    FPopupMenu.PopUp( X + FScreenLeft, Y + FScreenTop );
end;

procedure TRevEdit.MCopy( Sender: TObject );
begin
  Clipboard.AsText:= GetSelText;
end;

procedure TRevEdit.MCut( Sender: TObject );
begin
  Clipboard.AsText:= GetSelText;
  ReplaceSel( '' );  
end;

procedure TRevEdit.MDelete( Sender: TObject );
begin
  ReplaceSel( '' );
end;

procedure TRevEdit.MPaste( Sender: TObject );
begin
  ReplaceSel( Clipboard.AsText );
end;

procedure TRevEdit.SelectAll;
begin
  FSel1:= Length( FText );
  FSel2:= 0;
end;

procedure TRevEdit.MSelectAll( Sender: TObject );
begin
  SelectAll;
end;

{ TListBox }

constructor TRevListBox.Create( AOwner: TBaseObject; AEngine: TEngine;
  _Manager: TVCLManager; const Parent: TRevGraphicControl = nil );
begin
  inherited;
  FItems:= TStringList.Create;
  FColor:= $3F003088;
  FColorBound:= $3F00003F;
  FColorText:= $9FFFFFFF;
  FColorSel:= $FFFFFFFF;
  FColorSelText:= $FF000000;

  FScrollBlock1:= TRevGraphicControl.Create( Self, AEngine, Manager, Self );
  FScrollBlock1.Height:= 20;
  FScrollBlock1.Align:= alBottom;
  FScrollBlock2:= TRevGraphicControl.Create( Self, AEngine, Manager, FScrollBlock1 );
  FScrollBlock2.Width:= 20;
  FScrollBlock2.Align:= alRight;

  FScrollBarH:= TRevScrollBar.Create( Self, AEngine, Manager, FScrollBlock1 );
  FScrollBarV:= TRevScrollBar.Create( Self, AEngine, Manager, Self );

  FScrollBarH.Height:= 20;
  FScrollBarH.Kind:= sbHorizontal;
  FScrollBarH.OnChange:= Scroll;

  FScrollBarV.Width:= 20;
  FScrollBarV.Kind:= sbVertical;
  FScrollBarV.OnChange:= Scroll;

  FListCtrl:= TRevGraphicControl.Create( Self, AEngine, Manager, Self );
  FListCtrl.OnDraw:= DrawList;
  FListCtrl.Align:= alClient;
  FListCtrl.OnMouseDown:= MouseDown;

  FCanSelect:= True;
  FItemHeight:= 16;
  BoundLeft:= 3;
  BoundTop:= 3;
  BoundRight:= 3;
  BoundBottom:= 3;
end;

destructor TRevListBox.Destroy;
begin
  if ( FItems <> Engine.Log.ConsoleLog ) then
    FItems.Free;
{  FScrollBarH.Free;
  FScrollBarV.Free;
  FScrollBlock2.Free;
  FScrollBlock1.Free;}
  inherited;
end;

procedure TRevListBox.Draw;
begin
  Canvas.Render3DFrame( 0, 0, Width - 1, Height - 1, Color, Color, Color, Color, -50 );
  Canvas.RenderRect( 0, 0, Width - 1, Height - 1, Color, Color, Color, Color );
end;


procedure TRevListBox.DrawList( Sender: TRevGraphicControl; OffSetX, OffSetY,
  _Width, _Height: Single );
var
  I: Integer;
  P: TPoint;
//  V: TViewport;
  Pos: TPoint;
begin
  inherited;
  Canvas.RenderRect( 0, 0, FListCtrl.Width - 1, FListCtrl.Height - 1, Brightness( ColorBound, -64 ), ColorBound, ColorBound, Brightness( ColorBound, 64 ));
  Canvas.Render3DFrame( 0, 0, FListCtrl.Width - 1, FListCtrl.Height - 1, Brightness( ColorBound, -64 ), ColorBound, ColorBound, Brightness( ColorBound, 64 ), 64 );
  Canvas.Render3DFrame( 3, 3, FListCtrl.Width - 4, FListCtrl.Height - 4, Brightness( Color, -64 ), Color, Color, Brightness( Color, 64 ), 64 );
  Canvas.RenderRect( 3, 3, FListCtrl.Width - 4, FListCtrl.Height - 4, Brightness( Color, -64 ), Color, Color, Brightness( Color, 64 ));

//  Render3DFrame( 0, 0, Width - 1, Height - 1, Color, Color, Color, Color, -50 );
//  RenderRect( 0, 0, Width - 1, Height - 1, Color, Color, Color, Color );

{  V.X:= Max( 0, FScreenLeft + 3 );
  V.Y:= Max( 0, FScreenTop + 3 );
  V.Width:= Min( FScreenWidth, Round( Width - 27 ) - V.X + FScreenLeft + 3 );
  V.Height:= Min( FScreenHeight, Round( Height - 27 ) - V.Y + FScreenTop + 3 );

  Engine.DisplayDriver.SetViewport( V );}

  if ( Items.Count > 0 ) then
    begin
      FScrollBarH.PageSize:= FListCtrl.Width / GetTextWidth;
      FScrollBarV.PageSize:= FListCtrl.Height / ItemHeight / Items.Count;
      Pos.x:= Round(( Max( 0, GetTextWidth - FListCtrl.Width ) * FScrollBarH.Percentage ));
      Pos.y:= Round(( Items.Count * ItemHeight - FListCtrl.Height + 6 ) * FScrollBarV.Percentage );
    end
  else
    begin
      FScrollBarH.PageSize:= 1;
      FScrollBarV.PageSize:= 1;
      Pos.y:= 0;
      Pos.x:= 0;
    end;

  Canvas.Font.Size:= ItemHeight;
  for I:= 0 to Items.Count - 1 do
    begin
      P:= Point( 3 - Pos.x, 3 - Pos.y + I * ItemHeight );
      if (( P.Y < FListCtrl.Height ) and ( P.Y + ItemHeight >= 0 )) then
        begin
          if ( Selection = I ) then
            begin
              Canvas.RenderRect( 0, P.Y, FListCtrl.Width - 1, P.Y + ItemHeight, ColorSel, ColorSel, ColorSel, ColorSel );
              Canvas.Font.Color:= ColorSelText
            end
          else
            Canvas.Font.Color:= ColorText;
          Canvas.RenderTextAdv( Items.Strings[ I ], P );
        end;
    end;
end;

procedure TRevListBox.FocusSelection;
var
  PosY, Py: Integer;
begin
  if ( Selection > -1 ) then
    begin
      FScrollBarV.PageSize:= ( FListCtrl.Height ) / ItemHeight / Items.Count;
      PosY:= Round(( Items.Count * ItemHeight - FListCtrl.Height + 6 ) * FScrollBarV.Percentage );
      Py:= 3 - PosY + Selection * ItemHeight;
      if ( Py < 0 ) then
        FScrollBarV.Percentage:= -(Py+PosY )/(-Items.Count*ItemHeight+FListCtrl.Height-6);
      if ( Py + ItemHeight > FListCtrl.Height ) then
        FScrollBarV.Percentage:= -(Py+PosY + ItemHeight - FListCtrl.Height)/(-Items.Count*ItemHeight+FListCtrl.Height-6);
    end;
end;

function TRevListBox.GetSelection: Integer;
begin
  if ( CanSelect ) then
    Result:= FSelection
  else
    Result:= -1;
end;

function TRevListBox.GetTextWidth: Integer;
var
  I, tmp: Integer;
begin
  Result:= 0;
  For I:= 0 to Items.Count - 1 do
    begin
      tmp:= Manager.DefaultFont.TextWidthAdv( Items[ I ], ItemHeight );
      Result:= Max( tmp, Result );
    end;
end;

procedure TRevListBox.KeyDown(Loop: Boolean);
begin
  inherited;
  if ( Manager.GameInput.Keys[ IK_DOWN ]) then
    Selection:= Selection + 1;
  if ( Manager.GameInput.Keys[ IK_UP ]) then
    Selection:= Selection - 1;
  if ( Manager.GameInput.Keys[ IK_RIGHT ]) then
    ScrollBarH.Percentage:= ScrollBarH.Percentage + 0.1;
//  if ( Manager.GameInput.Keys[ IK_LEFT ]) then
//    ScrollBarH.Percentage:= ScrollBarH.Percentage - 0.1;
  if ( Manager.GameInput.Keys[ IK_PGDN ]) then
    begin
      Selection:= Selection + Round( ScrollBarV.PageSize * Items.Count );
//      ScrollBarV.Percentage:= ScrollBarV.Percentage + ScrollBarV.PageSize;
    end;
end;

procedure TRevListBox.MouseDown( Sender: TRevGraphicControl; mb1, mb2, mb3: Boolean; X, Y: Integer );
var
  Py: Integer;
begin
  Py:= Round(( Items.Count * ItemHeight - FListCtrl.Height + 6 ) * FScrollBarV.Percentage );
  Selection:= ( Y + Py ) div ItemHeight;
  SetFocus( True );
end;

procedure TRevListBox.Scroll( Sender: TObject );
begin
  //TODO: FIX ERROR WHEN SCROLLING
  FScroll.x:= Round(( Max( 0, GetTextWidth - ( Width - FScrollBarV.Width )) * FScrollBarH.Percentage ));
  FScroll.y:= Round( Items.Count * FScrollBarV.Percentage );
end;

procedure TRevListBox.SetSelection( const Value: Integer );
begin
  if ( CanSelect ) then
    if (( Value < Items.Count ) and ( Value >= -1 )) then
      begin
        FSelection:= Value;
        FocusSelection;
      end;
end;

initialization
  Rev_RegisterClasses([ TRevEdit, TRevListBox ]);

end.
