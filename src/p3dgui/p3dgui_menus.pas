unit p3dgui_menus;

{$mode objfpc}{$H+}

interface

  uses
    Classes, SysUtils, Math, p3dMath, p3dgui, p3dgraphics, p3dgui_buttons, p3dutils;

  type
    TP3DMenuItem = class;

    { TP3DMenuItemList }

    TP3DCustomMenuItemList = specialize gP3DCustomObjectList < TP3DMenuItem >;
    TP3DMenuItemList = class( TP3DCustomMenuItemList )
      private
        FParent: TP3DGraphicControl;

      public
        function Add( ACaption: String ): Integer; overload;

        constructor Create( AParent: TP3DGraphicControl );
        property Parent: TP3DGraphicControl read FParent write FParent;
    end;

    TP3DMenuItem = class( TP3DButton )
      private
        FUserData: TObject;

      public
        constructor Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
          const AParent: TP3DGraphicControl=nil);
        property UserData: TObject read FUserData write FUserData;
    end;

    { TP3DPopupMenu }

    { TP3DCustomPopupMenu }

    TP3DCustomPopupMenu = class ( TP3DGraphicControl )
      public
        constructor Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
          const AParent: TP3DGraphicControl=nil);

        class function IsFocusControl: Boolean; override;

        procedure Render( BaseColor: TVec4; ScrollAcc: TVec2); override;
        procedure Draw; override;

        procedure PopUp( X, Y: Integer ); virtual;

    end;

    TP3DPopupMenu = class ( TP3DCustomPopupMenu )
      private
        FItems: TP3DMenuItemList;

        procedure ItemsChange( Sender: TObject );

      public
        constructor Create(AOwner: TP3DObjectList; AManager: TP3DGUIManager;
          const AParent: TP3DGraphicControl=nil);
        destructor Destroy; override;

        procedure Update;

      published
        property Items: TP3DMenuItemList read FItems write FItems;
    end;

implementation

{ TP3DMenuItem }

constructor TP3DMenuItem.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; const AParent: TP3DGraphicControl);
begin
  inherited;
  HAlignment:= haLeft;
end;

{ TP3DMenuItemList }

function TP3DMenuItemList.Add(ACaption: String): Integer;
var
  Ctrl: TP3DMenuItem;
begin
  Ctrl:= TP3DMenuItem.Create( Parent.ParentList, Parent.Manager, Parent );
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

constructor TP3DPopupMenu.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; const AParent: TP3DGraphicControl);
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
          Ctrl.Height:= Round( Ctrl.TextHeight );
          MinSize:= Max( MinSize, Round( Ctrl.TextWidth ));
        end;
      Height:= Ctrl.Top + Ctrl.Height;
    end;
  Width:= MinSize;
end;

procedure TP3DCustomPopupMenu.Render(BaseColor: TVec4; ScrollAcc: TVec2);
var
  p1: TVec2;
begin
  if (( not Focused ) and ( not Controls.ChildOf( Manager.FocusedControl ))) then
    Visible:= False;

  p1:= vec2( Canvas.Left + 2, Canvas.Top + 2 );
  Manager.ScreenCanvas.Lock;
  Manager.ScreenCanvas.RenderRectShadow( p1, p1 + vec2( Canvas.Width, Canvas.Height ), 5, vec4( 0, 0, 0, 0.1 ));
  Manager.ScreenCanvas.Unlock();

  inherited Render(BaseColor, ScrollAcc);
end;


constructor TP3DCustomPopupMenu.Create(AOwner: TP3DObjectList;
  AManager: TP3DGUIManager; const AParent: TP3DGraphicControl);
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


end.

