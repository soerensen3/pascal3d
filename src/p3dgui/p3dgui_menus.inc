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
        FParent: TP3DPopupMenu;
        FTop: Float;
        FUserData: TObject;
        FCaptionTxt: TP3DTextBmp;

        function GetHeight: Float;
        function GetWidth: Float;
        procedure SetCaption(AValue: String);
        procedure UpdateCaption;

      public
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;
        destructor Destroy; override;

        property UserData: TObject read FUserData write FUserData;
        property IsSelectable: Boolean read FIsSelectable write FIsSelectable;
        property Caption: String read FCaption write SetCaption;
        property Font: TP3DCanvasFont read FFont write FFont;
        property Color: TP3DColorRGBA read FColor write FColor;
        property Parent: TP3DPopupMenu read FParent write FParent;
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
      public
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

        class function IsFocusControl: Boolean; override;

        procedure Paint(); override;
        procedure Draw; override;

        procedure PopUp( X, Y: Integer ); virtual;
        procedure Hide;
    end;

    { TP3DPopupMenu }

    TP3DPopupMenu = class ( TP3DCustomPopupMenu )
      private
        FItems: TP3DMenuItemList;
        FOnItemClick: TP3DPopupItemClick;
        FSelectedPreset: TP3DButtonPreset;
        FSelection: TP3DMenuItem;

        procedure ItemsChange( Sender: TObject; Item: TP3DStreamable; Action: TP3DListOnChangeAction );

      public
        constructor Create(const AOwner: IP3DStreamableContainer=nil); override;

        class function HasShadow: Boolean; override;

        procedure MouseClick( mb1, mb2, mb3: Boolean; X, Y: Integer); override;
        procedure MouseMove(X, Y: Integer); override;
        procedure Update;
        procedure Draw; override;

      published
        property Items: TP3DMenuItemList read FItems write FItems;
        property OnItemClick: TP3DPopupItemClick read FOnItemClick write FOnItemClick;
        property SelectedPreset: TP3DButtonPreset read FSelectedPreset write FSelectedPreset;
        property Selection: TP3DMenuItem read FSelection write FSelection;
    end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DMenuItem }


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

function TP3DMenuItem.GetHeight: Float;
begin
  if ( not Assigned( FCaptionTxt )) then
    UpdateCaption;
  Result:= FCaptionTxt.Height;
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

class function TP3DPopupMenu.HasShadow: Boolean;
begin
  Result:= True
end;

procedure TP3DPopupMenu.MouseClick(mb1, mb2, mb3: Boolean; X, Y: Integer);
begin
  inherited MouseClick( mb1, mb2, mb3, X, Y );
  if ( Assigned( OnItemClick ) and Assigned( Selection ) and Selection.Enabled ) then
    OnItemClick( Selection );
end;

procedure TP3DPopupMenu.MouseMove(X, Y: Integer);
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
  FItems:= TP3DMenuItemList.Create( 'Items' );
  FItems.OnChange:= @ItemsChange;
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
  if ( Items.Count > 0 ) then
    begin
      for Itm in Items do
        begin
          //Ctrl.OnMouseClick:= @ItemMouseClick;
          Itm.Top:= Line;
          Line+= Itm.Height;
          MinSize:= Max( MinSize, Round( Itm.Width ));
        end;
      Height:= Line;
    end;
  Width:= MinSize;
  FNeedsUpdate:= True;
end;

procedure TP3DPopupMenu.Draw;
  procedure RenderButton( Itm: TP3DMenuItem; C1, C2, C3, C4, CE1, CE2, CE3, CE4: TVec4);
  var
    r: TP3DRect;
  begin
    r:= P3DRect( 0, Itm.Top, Canvas.Width, Itm.Height );
    Canvas.RenderRect( r.TopLeft, r.BottomRight, C1, C2, C3, C4 );
    Canvas.RenderLineRect( r.TopLeft, r.BottomRight, CE1, CE2, CE3, CE4 );
  end;
var
  Itm: TP3DMenuItem;
begin
  inherited Draw;
  for Itm in Items do
    begin
      if (( Itm.Enabled ) and ( Itm = Selection )) then
        begin
          RenderButton( Itm, Accent900, Accent400, Accent600, Accent900, Accent900, Accent900, Accent900, Accent900 );
          Canvas.Font.Color:= Accent50;
        end
      else
        Canvas.Font.Color:= Accent900;
      if ( not Itm.Enabled ) then
        Canvas.Font.Color:= vec4( Canvas.Font.Color.xyz, 0.4 );

      Canvas.RenderText( Itm.FCaptionTxt, vec2( Itm.Left, Itm.Top ));
    end;
end;

procedure TP3DCustomPopupMenu.Paint();
begin
  if (( not Focused ) and ( not Controls.ChildOf( P3DGUIManager.FocusedControl ))) then
    Visible:= False;

  inherited Paint();
end;


constructor TP3DCustomPopupMenu.Create(const AOwner: IP3DStreamableContainer);
begin
  inherited Create( AOwner );
  Visible:= False;
end;

class function TP3DCustomPopupMenu.IsFocusControl: Boolean;
begin
  Result:= True;
end;

procedure TP3DCustomPopupMenu.Draw;
begin
  inherited Draw;
  Canvas.RenderRect( vec2( 0 ), Canvas.WidthHeight, vec4( Primary100.xyz, 0.8 ));
end;

procedure TP3DCustomPopupMenu.PopUp(X, Y: Integer);
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

