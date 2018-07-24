//p3dgui_forms.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

type

  { TP3DForm }

  TP3DForm = class( TP3DGroupBox )
    private
      FMouseDownPos: TVec2;

      procedure MoveWindowTo( APos: TVec2 ); virtual;

    public
      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      procedure Draw(); override;

      procedure MouseDown( mb1, mb2, mb3: Boolean; X, Y: Float ); override;
      procedure MouseMove(X, Y: Float); override;
  end;

  TP3DModalResult = ( mrUndefined, mrCancel, mrOk );

  TP3DModalWindow = class;

  { TP3DModalWindow }
  TP3DModalCallback = procedure ( Sender: TP3DModalWindow; Result: TP3DModalResult ) of object;

  TP3DModalWindow = class( TP3DForm )
    protected
      FModalResult: TP3DModalResult;
      FOverlay: TP3DStreamablePointer;
      FCallBack: TP3DModalCallback;

      function GetOverlay: TP3DGraphicControl;
      procedure SetModalResult(AValue: TP3DModalResult);
      procedure SetOverlay(AValue: TP3DGraphicControl);
      procedure Close; override;

    public
      procedure ShowModal( CallBack: TP3DModalCallback );

      constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
      destructor Destroy; override;
      procedure DrawOverlay( Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Single );

      property Overlay: TP3DGraphicControl read GetOverlay write SetOverlay;
      property ModalResult: TP3DModalResult read FModalResult write SetModalResult;
  end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DForm }

procedure TP3DForm.MoveWindowTo(APos: TVec2);
begin
  TopLeft:= APos;
end;

constructor TP3DForm.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create(AOwner);
  FCanClose:= True;
  Font.Color:= White;
end;

procedure TP3DForm.Draw();
var
  hw: Extended;
  pt1: TVec2;
  pt2: TVec2;
  r: TP3DRect;
begin
  if ( Borderless ) then
    exit;
  Canvas.RenderRect( vec2( 0, 0 ), WidthHeight, Color[ cp300 ]);
  Canvas.RenderLine( vec2( 0, 1 ), vec2( ClippedRect.Width, 1 ), Color[ cp200 ]);
  Canvas.RenderLine( vec2( 0, ClippedRect.Height ), ClippedRect.WidthHeight, Color[ cp400 ]);

  Canvas.RenderRect( vec2( 0, 3 ), vec2( ClippedRect.Width, FCaptionTxt.Height + 3 ), Color[ cp800 ]);
  Canvas.GetValidText( Caption, FCaptionTxt,
    P3DFontManager[ Canvas.Font.Name, Canvas.Font.Size ]);
  if ( not Assigned( FCaptionTxt  )) then
    hw:= 0
  else
    hw:= FCaptionTxt.Height / 2;
  Canvas.Font.Assign( Font );
  Canvas.RenderTextShadow( FCaptionTxt, vec2( hw * 2, 2 ), 5, Black );
  Canvas.RenderText( FCaptionTxt, vec2( hw * 2, 2 ));
  //Canvas.RenderLineRect( vec2( hw ), vec2( Width, Height ) - hw, BorderColor );
  {pt1:= vec2( hw );
  pt2:= WidthHeight - hw;
  Canvas.RenderLine( pt1, vec2( hw * 2, pt1.y ), BorderColor );
  if ( Assigned( FCaptionTxt )) then
    Canvas.RenderLine( vec2( FCaptionTxt.Width + hw * 2, pt1.y ), vec2( pt2.x, pt1.y ), BorderColor );
  Canvas.RenderLine( vec2( pt1.x, pt2.y ), pt2, BorderColor );
  Canvas.RenderLine( pt1, vec2( pt1.x, pt2.y ), BorderColor );
  Canvas.RenderLine( vec2( pt2.x, pt1.y ), pt2, BorderColor );}

  //Canvas.RenderRect( vec2( hw * 2, 0 ), vec2( FCaptionTxt.Width, FCaptionTxt.Height ) + hw * 2, Color );

  if ( CanCollapse ) then
    begin
      Canvas.GetValidText( FCollapseSymbol, FCollapseSymbolTxt, P3DFontManager[ P3DSymbolFontName, Canvas.Font.Size ]);
      if ( P3DRect( 3, 3, FCollapseSymbolTxt.Width, FCollapseSymbolTxt.Height ).PtInRect( vec2( UIViewer.MouseX, UIViewer.MouseY ) - Canvas.TopLeft )) then
        Canvas.Font.Color:= P3DThemeDefault.Accent[ cp500 ]
      else
        Canvas.Font.Color:= P3DThemeDefault.Primary[ cp500 ];
      Canvas.RenderText(
        FCollapseSymbolTxt, vec2( 3 ));
    end;
  if ( CanClose ) then
    begin
      Canvas.GetValidText( FCloseSymbol, FCloseSymbolTxt,
                P3DFontManager[ P3DSymbolFontName, Canvas.Font.Size ]);
      r:= P3DRect( vec2( Width - FCloseSymbolTxt.Width - 3, 3 ), vec2( FCloseSymbolTxt.Width, FCloseSymbolTxt.Height ));
      if ( r.PtInRect( vec2( UIViewer.MouseX, UIViewer.MouseY ) - ClippedRect.TopLeft )) then
        Canvas.Font.Color:= P3DThemeDefault.Accent[ cp500 ]
      else
        Canvas.Font.Color:= P3DThemeDefault.Primary[ cp500 ];
      //Canvas.RenderRectShadow( r.TopLeft, r.BottomRight, 5, Color );
      Canvas.RenderTextShadow( FCloseSymbolTxt, vec2( Width - FCloseSymbolTxt.Width - 2, 4 ), 1, Black.fade( 0.5 ));
      Canvas.RenderText( FCloseSymbolTxt, vec2( Width - FCloseSymbolTxt.Width - 3, 3 ));
    end;
end;

procedure TP3DForm.MouseDown(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  if ( mb1 and ( gcisMouseOver in InputState )) then
    begin
      BringToFront;
      FMouseDownPos:= UIViewer.Mouse - TopLeft;
    end;
end;

procedure TP3DForm.MouseMove(X, Y: Float);
begin
  inherited MouseMove( X, Y );

  P3DApplication.MainWindow.Title:= vec2( X, Y ).ToString() + ' (' + FMouseDownPos.ToString() + ') / ' + UIViewer.Mouse.ToString() + ' - [' + UIViewer.ViewRect.ToString + ']';
  if ( gcisMouseBtn1Down in InputState ) then begin
    MoveWindowTo( {vec2( X, Y ) - FMouseDownPos }UIViewer.Mouse - FMouseDownPos{ - UIViewer.Position} );//}{ - P3DUIViewerActive.Position });
    //P3DApplication.MainWindow.Title:= APos.ToString();
  end;
end;

{ TP3DModalWindow }

procedure TP3DModalWindow.SetModalResult(AValue: TP3DModalResult);
begin
  if FModalResult=AValue then Exit;
  FModalResult:=AValue;
  Visible:= False;
  Overlay.Visible:= False;
  if ( Assigned( FCallBack )) then
    FCallBack( Self, AValue );
end;

function TP3DModalWindow.GetOverlay: TP3DGraphicControl;
begin
  Result:= TP3DGraphicControl( FOverlay.AsValue );
end;

procedure TP3DModalWindow.SetOverlay(AValue: TP3DGraphicControl);
begin
  FOverlay.AsValue:= AValue;
end;

procedure TP3DModalWindow.Close;
begin
  inherited Close;
  ModalResult:= mrCancel;
end;

procedure TP3DModalWindow.ShowModal(CallBack: TP3DModalCallback);
begin
  FModalResult:= mrUndefined;
  Overlay.Visible:= True;
  Overlay.BringToFront;
  Visible:= True;
  BringToFront;
  FCallBack:= CallBack;
  TopLeft:= ( UIViewer.ScreenCanvas.WidthHeight - WidthHeight ) / 2;
end;

constructor TP3DModalWindow.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FOverlay:= TP3DStreamablePointer.Create( 'Overlay', TP3DGraphicControl );
  Overlay:= TP3DGraphicControl.Create();
  Overlay.OnDraw:= @DrawOverlay;
  Overlay.Visible:= False;
  Overlay.Align:= alClient;
  Visible:= False;
  Properties.Add( FOverlay );
end;

destructor TP3DModalWindow.Destroy;
begin
  Overlay.Free;
  inherited Destroy;
end;

procedure TP3DModalWindow.DrawOverlay(Sender: TP3DGraphicControl; OffSetX, OffSetY, _Width, _Height: Single);
begin
  Sender.Canvas.RenderRect( vec2( 0 ), Sender.WidthHeight, vec4( P3DThemeDefault.Primary[ cp900 ].XYZ, 0.6 ));
end;


{$ENDIF}

