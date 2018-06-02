//p3dgui_menus.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

  type
    TP3DMenuItemList = class;

    TP3DPopupMenu = class;

    { TP3DMenuItem }

    TP3DMenuItem = class( TP3DStreamable )
      private
        FCaption: String;
        FColor: TP3DColorRGBA;
        FEnabled: Boolean;
        FFont: TP3DCanvasFont;
        FIsSelectable: Boolean;
        FLeft: Float;
        FParent: TP3DStreamablePointer;
        FTop: Float;
        FUserData: TObject;
        FCaptionTxt: TP3DTextBmp;

        function GetHeight: Float;
        function GetParent: TP3DPopupMenu;
        function GetWidth: Float;
        procedure SetCaption(AValue: String);
        procedure SetParent(AValue: TP3DPopupMenu);
        procedure UpdateCaption;

      public
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
        destructor Destroy; override;

        property UserData: TObject read FUserData write FUserData;
        property IsSelectable: Boolean read FIsSelectable write FIsSelectable;
        property Caption: String read FCaption write SetCaption;
        property Font: TP3DCanvasFont read FFont write FFont;
        property Color: TP3DColorRGBA read FColor write FColor;
        property Parent: TP3DPopupMenu read GetParent write SetParent;
        property Width: Float read GetWidth;
        property Height: Float read GetHeight;
        property Left: Float read FLeft write FTop;
        property Top: Float read FTop write FTop;
        property Enabled: Boolean read FEnabled write FEnabled;
    end;

    TP3DPopupItemClick = procedure ( Sender: TP3DMenuItem ) of object;

    { TP3DMenuItemList }

    TP3DMenuItemList = class( specialize gP3DStreamableContainerList < TP3DMenuItem > )
      public
        function Add( ACaption: String ): Integer; overload;
    end;


    { TP3DCustomPopupMenu }

    TP3DCustomPopupMenu = class ( TP3DGraphicControl )
      protected
        FPresetNormal: TP3DButtonPreset;
        FPresetSelection: TP3DButtonPreset;

      public
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
        destructor Destroy; override;

        class function IsFocusControl: Boolean; override;
        class function HasShadow: Boolean; override;

        procedure Paint(); override;
        procedure Draw; override;

        procedure PopUp( X, Y: Float ); virtual;
        procedure Hide;

        property PresetNormal: TP3DButtonPreset read FPresetNormal write FPresetNormal;
        property PresetSelection: TP3DButtonPreset read FPresetSelection write FPresetSelection;
    end;

    { TP3DPopupMenu }

    TP3DPopupMenu = class ( TP3DCustomPopupMenu )
      private
        FAlignment: TP3DHorizontalAlignment;
        FAutoWidth: Boolean;
        FItems: TP3DMenuItemList;
        FOnItemClick: TP3DPopupItemClick;
        FSelection: TP3DStreamablePointer;

        function GetSelection: TP3DMenuItem;
        procedure ItemsChange( Sender: TObject; Item: TP3DStreamable; Action: TP3DListOnChangeAction );
        procedure SetAlignment(AValue: TP3DHorizontalAlignment);
        procedure SetAutoWidth(AValue: Boolean);
        procedure SetSelection(AValue: TP3DMenuItem);

      public
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

        procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Float); override;
        procedure MouseMove(X, Y: Float); override;
        procedure Update; override;
        procedure Draw; override;

      published
        property Items: TP3DMenuItemList read FItems write FItems;
        property OnItemClick: TP3DPopupItemClick read FOnItemClick write FOnItemClick;
        property Selection: TP3DMenuItem read GetSelection write SetSelection;
        property AutoWidth: Boolean read FAutoWidth write SetAutoWidth;
        property Alignment: TP3DHorizontalAlignment read FAlignment write SetAlignment;
    end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DMenuItemList }

function TP3DMenuItemList.Add(ACaption: String): Integer;
var
  Itm: TP3DMenuItem;
begin
  Itm:= TP3DMenuItem.Create( Self );
  Itm.Caption:= ACaption;
  Result:= Add( Itm );
end;

{ TP3DMenuItem }

procedure TP3DMenuItem.SetCaption(AValue: String);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  UpdateCaption;
end;

procedure TP3DMenuItem.SetParent(AValue: TP3DPopupMenu);
begin
  FParent.AsValue:= AValue;
end;

function TP3DMenuItem.GetHeight: Float;
begin
  if ( not Assigned( FCaptionTxt )) then
    UpdateCaption;
  Result:= FCaptionTxt.Height;
end;

function TP3DMenuItem.GetParent: TP3DPopupMenu;
begin
  Result:= TP3DPopupMenu( FParent.AsValue );
end;

function TP3DMenuItem.GetWidth: Float;
begin
  if ( not Assigned( FCaptionTxt )) then
    UpdateCaption;
  Result:= FCaptionTxt.Width;
end;

procedure TP3DMenuItem.UpdateCaption;
begin
  Parent.Canvas.GetValidText( Caption, FCaptionTxt, P3DFontManager[ Font.Name, Font.Size ]);
end;

constructor TP3DMenuItem.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FParent:= TP3DStreamablePointer.Create( 'Parent', TP3DPopupMenu);
  Properties.Add( FParent );
  Parent:= ( AOwner.GetContainer.Context.GetInstance as TP3DPopupMenu );
  Font:= TP3DCanvasFont.Create;
  Font.Assign( Parent.Canvas.Font );
  Enabled:= True;
end;

destructor TP3DMenuItem.Destroy;
begin
  Font.Free;
  FCaptionTxt.Free;
  inherited Destroy;
end;


{ TP3DPopupMenu }

procedure TP3DPopupMenu.ItemsChange(Sender: TObject; Item: TP3DStreamable; Action: TP3DListOnChangeAction);
begin
  if ( Action <> actClear ) then
    Update;
end;

procedure TP3DPopupMenu.SetAlignment(AValue: TP3DHorizontalAlignment);
begin
  if FAlignment=AValue then Exit;
  FAlignment:=AValue;
  Update;
end;

procedure TP3DPopupMenu.SetAutoWidth(AValue: Boolean);
begin
  if FAutoWidth=AValue then Exit;
  FAutoWidth:=AValue;
  Update;
end;

function TP3DPopupMenu.GetSelection: TP3DMenuItem;
begin
  Result:= TP3DMenuItem( FSelection.AsValue );
end;

procedure TP3DPopupMenu.SetSelection(AValue: TP3DMenuItem);
begin
  FSelection.AsValue:= AValue;
end;

procedure TP3DPopupMenu.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Float);
begin
  inherited MouseClick( mb1, mb2, mb3, X, Y );
  if ( Assigned( OnItemClick ) and Assigned( Selection ) and Selection.Enabled ) then
    OnItemClick( Selection );
end;

procedure TP3DPopupMenu.MouseMove(X, Y: Float);
var
  Itm: TP3DMenuItem;
begin
  inherited MouseMove(X, Y);

//  if ( Assigned( Selection ) and P3DRect( Selection.Left, Selection.Top, Selection.Width, Selection.Height ).PtInRect( vec2( X, Y ))) then
  if ( Assigned( Selection ) and P3DRect( 0, Selection.Top, WidthHeight.x, Selection.Height ).PtInRect( vec2( X, Y ))) then
    exit;

  Selection:= nil;

  for Itm in Items do
//    if ( P3DRect( Itm.Left, Itm.Top, Itm.Width, Itm.Height ).PtInRect( vec2( X, Y ))) then
    if ( P3DRect( 0, Itm.Top, WidthHeight.x, Itm.Height ).PtInRect( vec2( X, Y ))) then
      begin
        Selection:= Itm;
        break;
      end;
end;

constructor TP3DPopupMenu.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  FSelection:= TP3DStreamablePointer.Create( 'Selection', TP3DMenuItem );
  Properties.Add( FSelection );
  FItems:= TP3DMenuItemList.Create( 'Items' );
  FItems.OnChange:= @ItemsChange;
  FAutoWidth:= True;
  Properties.Add( FItems );
end;

procedure TP3DPopupMenu.Update;
var
  Itm: TP3DMenuItem;
  MinSize: Integer;
  Line: Float;
begin
  MinSize:= 50;
  Line:= 0;
  for Itm in Items do
    begin
      //Ctrl.OnMouseClick:= @ItemMouseClick;
      Itm.Top:= Line;
      Line+= Itm.Height * 1.2;
      MinSize:= Max( MinSize, Round( Itm.Width ));
    end;
  if ( Items.Count > 0 ) then
    Height:= Line;
  if ( AutoWidth ) then
    Width:= Min( Width, MinSize );
  inherited Update;
  FNeedsUpdate:= True;
end;

procedure TP3DPopupMenu.Draw;
  procedure RenderButton( Itm: TP3DMenuItem; C1, C2, C3, C4, CE1, CE2, CE3, CE4: TVec4);
  var
    r: TP3DRect;
  begin
    r:= P3DRect( 0, Itm.Top, CanvasRect.Width, Itm.Height );
    Canvas.RenderRect( r.TopLeft, r.BottomRight, C1, C2, C3, C4 );
    Canvas.RenderLineRect( r.TopLeft, r.BottomRight, CE1, CE2, CE3, CE4 );
  end;
var
  Itm: TP3DMenuItem;
begin
  inherited Draw;
  Update;
  for Itm in Items do
    begin
      if (( Itm.Enabled ) and ( Itm = Selection )) then
        with ( PresetSelection ) do
          begin
            RenderButton( Itm, Color[ cp900 ], Color[ cp400 ], Color[ cp600 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ], Color[ cp900 ]);
            Canvas.Font.Color:= FontColor;
          end
      else
        Canvas.Font.Color:= PresetNormal.FontColor;
      if ( not Itm.Enabled ) then
        Canvas.Font.Color:= vec4( Canvas.Font.Color.xyz, 0.4 );

      case Alignment of
        haLeft: Canvas.RenderText( Itm.FCaptionTxt, vec2( Itm.Left, Itm.Top ));
        haCenter: Canvas.RenderText( Itm.FCaptionTxt, vec2(( CanvasRect.Width - Itm.Width ) / 2, Itm.Top ));
        haRight: Canvas.RenderText( Itm.FCaptionTxt, vec2( CanvasRect.Width - Itm.Width, Itm.Top ));
      end;
    end;
end;

procedure TP3DCustomPopupMenu.Paint();
begin
  if (( not Focused ) and ( not Controls.ChildOf( P3DUI.FocusedControl ))) then
    Visible:= False;

  inherited Paint();
end;


constructor TP3DCustomPopupMenu.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Visible:= False;
  PresetNormal:= TP3DButtonPreset.Create;
  PresetNormal.Color:= P3DThemeDefault.Primary;//vec4( vec3( 0.93 ), 1 );
  PresetNormal.FontColor:= P3DThemeDefault.Accent[ cp900 ];//vec4( vec3( 0 ), 1 );
  PresetSelection:= TP3DButtonPreset.Create;
  PresetSelection.Color:= P3DThemeDefault.Accent;//vec4( vec3( 0.93 ), 1 );
  PresetSelection.FontColor:= P3DThemeDefault.Primary[ cp50 ];//vec4( vec3( 0 ), 1 );
end;

destructor TP3DCustomPopupMenu.Destroy;
begin
  PresetNormal.Free;
  PresetSelection.Free;
  inherited Destroy;
end;

class function TP3DCustomPopupMenu.IsFocusControl: Boolean;
begin
  Result:= True;
end;

class function TP3DCustomPopupMenu.HasShadow: Boolean;
begin
  Result:= True;
end;

procedure TP3DCustomPopupMenu.Draw;
begin
  inherited Draw;
  Canvas.RenderRect( vec2( 0 ), CanvasRect.WidthHeight, vec4( PresetNormal.Color[ cp100 ].xyz, 0.9 ));
end;

procedure TP3DCustomPopupMenu.PopUp(X, Y: Float);
begin
  BringToFront;
  TopLeft:= vec2( X, Y );
  Visible:= True;
  Focused:= True;
end;

procedure TP3DCustomPopupMenu.Hide;
begin
  Visible:= False;
end;


{$ENDIF}

