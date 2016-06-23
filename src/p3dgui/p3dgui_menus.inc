//p3dgui_menus.inc
//part of p3dgui.pas

{$IFDEF INTERFACE}

  type
    TP3DMenuItem = class;

    TP3DCustomMenuItemList = specialize gP3DCustomObjectList < TP3DMenuItem >;

    TP3DPopupItemClick = procedure ( Sender: TP3DMenuItem ) of object;

    { TP3DMenuItemList }

    TP3DMenuItemList = class( TP3DCustomMenuItemList )
      private
        FParent: TP3DGraphicControl;

      public
        function Add( ACaption: String ): Integer; overload;

        constructor Create( AParent: TP3DGraphicControl );
        property Parent: TP3DGraphicControl read FParent write FParent;
    end;

    { TP3DMenuItem }

    TP3DMenuItem = class( TP3DButton )
      private
        FUserData: TObject;

      public
        constructor Create(const AOwner: TP3DObjectList = nil; const AParent: TP3DGraphicControl=nil);

        property UserData: TObject read FUserData write FUserData;
    end;

    { TP3DCustomPopupMenu }

    TP3DCustomPopupMenu = class ( TP3DGraphicControl )
      public
        constructor Create(const AOwner: TP3DObjectList = nil; const AParent: TP3DGraphicControl=nil);

        class function IsFocusControl: Boolean; override;

        procedure Render(BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect); override;
        procedure Draw; override;

        procedure PopUp( X, Y: Integer ); virtual;
        procedure Hide;
    end;

    { TP3DPopupMenu }

    TP3DPopupMenu = class ( TP3DCustomPopupMenu )
      private
        FItems: TP3DMenuItemList;
        FOnItemClick: TP3DPopupItemClick;

        procedure ItemsChange( Sender: TObject );
        procedure ItemMouseClick(Sender: TP3DGraphicControl; mb1, mb2, mb3: Boolean; X,
          Y: Integer);

      public
        constructor Create(const AOwner: TP3DObjectList = nil; const AParent: TP3DGraphicControl=nil);
        destructor Destroy; override;

        procedure Update;

      published
        property Items: TP3DMenuItemList read FItems write FItems;
        property OnItemClick: TP3DPopupItemClick read FOnItemClick write FOnItemClick;
    end;

{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TP3DMenuItem }

constructor TP3DMenuItem.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited Create( AOwner, AParent );
  HAlignment:= haLeft;
end;

{ TP3DMenuItemList }

function TP3DMenuItemList.Add(ACaption: String): Integer;
var
  Ctrl: TP3DMenuItem;
begin
  Ctrl:= TP3DMenuItem.Create( Parent.ParentList, Parent );
  Ctrl.Caption:= ACaption;
  Ctrl.Align:= alTop;
  Result:= Add( Ctrl );
end;

constructor TP3DMenuItemList.Create(AParent: TP3DGraphicControl);
begin
  inherited Create;
  Parent:= AParent;
end;

{ TP3DPopupMenu }

procedure TP3DPopupMenu.ItemsChange(Sender: TObject);
begin
  Update;
end;

procedure TP3DPopupMenu.ItemMouseClick(Sender: TP3DGraphicControl; mb1, mb2,
  mb3: Boolean; X, Y: Integer);
begin
  if ( Sender is TP3DMenuItem and Assigned( OnItemClick )) then
    OnItemClick( TP3DMenuItem( Sender ));
end;

constructor TP3DPopupMenu.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  FItems:= TP3DMenuItemList.Create( Self );
  FItems.OnChange:= @ItemsChange;
end;

destructor TP3DPopupMenu.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TP3DPopupMenu.Update;
var
  Ctrl: TP3DMenuItem;
  MinSize: Integer;
begin
  MinSize:= 50;
  if ( Items.Count > 0 ) then
    begin
      for Ctrl in Items do
        begin
          Ctrl.OnMouseClick:= @ItemMouseClick;
          Ctrl.Height:= Round( Ctrl.TextHeight );
          MinSize:= Max( MinSize, Round( Ctrl.TextWidth ));
        end;
      Height:= Ctrl.Top + Ctrl.Height;
    end;
  Width:= MinSize;
end;

procedure TP3DCustomPopupMenu.Render(BaseColor: TVec4; ScrollAcc: TVec2; ClipRect: TP3DRect);
var
  p1: TVec2;
begin
  if (( not Focused ) and ( not Controls.ChildOf( P3DGUIManager.FocusedControl ))) then
    Visible:= False;

  p1:= vec2( Canvas.Left + 2, Canvas.Top + 2 );
  P3DGUIManager.ScreenCanvas.Lock;
  P3DGUIManager.ScreenCanvas.RenderRectShadow( p1, p1 + vec2( Canvas.Width, Canvas.Height ), 5, vec4( 0, 0, 0, 0.1 ));
  P3DGUIManager.ScreenCanvas.Unlock();

  inherited Render(BaseColor, ScrollAcc, ClipRect);
end;


constructor TP3DCustomPopupMenu.Create(const AOwner: TP3DObjectList;
  const AParent: TP3DGraphicControl);
begin
  inherited;
  Visible:= False;
end;

class function TP3DCustomPopupMenu.IsFocusControl: Boolean;
begin
  Result:= True;
end;

procedure TP3DCustomPopupMenu.Draw;
begin
  inherited Draw;
  Canvas.RenderRect( vec2( 0 ), vec2( ClientRect.Right, ClientRect.Bottom ), vec4( 0.8, 0.8, 0.8, 1 ));
end;

procedure TP3DCustomPopupMenu.PopUp(X, Y: Integer);
begin
  BringToFront;
  Left:= X;
  Top:= Y;
  Visible:= True;
  Focused:= True;
end;

procedure TP3DCustomPopupMenu.Hide;
begin
  Visible:= False;
end;


{$ENDIF}

